' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Diagnostics.CodeAnalysis
Imports System.Runtime.CompilerServices
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.FindSymbols
Imports Microsoft.CodeAnalysis.Rename

Namespace CSharpToVBConverter
    <ExcludeFromCodeCoverage>
    Friend Module SymbolRenamer

        Private Iterator Function FollowProperty(Of TIn As TOut, TOut As Class)(start As TIn, getProperty As Func(Of TOut, TOut)) As IEnumerable(Of TOut)
            Dim current As TOut = start
            While current IsNot Nothing
                Yield current
                current = getProperty(current)
            End While
        End Function

        <Extension>
        Private Function FollowProperty(Of TOut As Class)(start As TOut, getProperty As Func(Of TOut, TOut)) As IEnumerable(Of TOut)
            Return FollowProperty(Of TOut, TOut)(start, getProperty)
        End Function

        Private Function GetBaseForNewName(declaration As ISymbol) As String
            Dim name As String = GetName(declaration)
            Dim baseForNewName As String
            Select Case declaration.Kind
                Case = SymbolKind.Method
                    baseForNewName = name & "Method"
                Case = SymbolKind.Property
                    baseForNewName = name & "Prop"
                Case = SymbolKind.NamedType
                    baseForNewName = name & "Type"
                Case = SymbolKind.Field
                    baseForNewName = name & "Field"
                Case Else
#Disable Warning CA1308 ' Normalize strings to uppercase
                    baseForNewName = $"{declaration.Kind.ToString().ToLowerInvariant()(0)}{Char.ToUpperInvariant(name.Chars(index:=0))}{name.Substring(startIndex:=1)}"
#Enable Warning CA1308 ' Normalize strings to uppercase
            End Select

            Return baseForNewName
        End Function

        Private Function GetMethodSymbolsWithNewNames(methodSymbols As IMethodSymbol(),
        names As HashSet(Of String),
        specialSymbolUsingName As Boolean, caseSensitive As Boolean) As (Original As ISymbol, NewName As String)()
            Dim stringComparer As StringComparer = If(caseSensitive, StringComparer.Ordinal, StringComparer.OrdinalIgnoreCase)
            Dim methodsBySignature() As (original As ISymbol, NewName As String) = methodSymbols _
            .ToLookup(Function(m) m.GetUnqualifiedMethodSignature(caseSensitive)) _
            .Where(Function(g) g.Count() > 1) _
            .SelectMany(Function(clashingMethodGroup) As (Original As ISymbol, NewName As String)()
                            Dim thisMethodGroupNames As HashSet(Of String) = New HashSet(Of String)(stringComparer)
                            Dim symbolsWithNewNames As (Original As ISymbol, NewNam As String)() = GetSymbolsWithNewNames(clashingMethodGroup,
                                Function(n) Not names.Contains(n) AndAlso thisMethodGroupNames.Add(n),
                                Not specialSymbolUsingName).ToArray()
                            Return symbolsWithNewNames
                        End Function).ToArray()

            For Each newMethodNames As String In methodsBySignature.Select(Function(m) m.NewName)
                names.Add(newMethodNames)
            Next

            Return methodsBySignature
        End Function

        <Extension>
        Private Function GetParameterSignature(methodSymbol As IMethodSymbol) As String
            Return String.Join(" ", methodSymbol.Parameters.Select(Function(p) p.Type))
        End Function

        <Extension>
        Private Function GetUnqualifiedMethodSignature(methodSymbol As IMethodSymbol, caseSensitiveName As Boolean) As (Name As String, TypeParameterCount As Integer, ParameterTypes As String)
#Disable Warning CA1308 ' Normalize strings to uppercase
            Return (If(caseSensitiveName, methodSymbol.Name, methodSymbol.Name.ToLowerInvariant()), methodSymbol.TypeParameters.Length, GetParameterSignature(methodSymbol))
#Enable Warning CA1308 ' Normalize strings to uppercase
        End Function

        Friend Function GetName(m As ISymbol) As String
            If m.CanBeReferencedByName Then
                Return m.Name
            End If
            If m.ExplicitInterfaceImplementations().Any() Then
                Return m.Name.Split("."c).Last()
            End If

            Return m.Name
        End Function

        Friend Function GetNamespacesAndTypesInAssembly(_project As Project, _compilation As Compilation) As IEnumerable(Of INamespaceOrTypeSymbol)
            Return _compilation.GlobalNamespace.FollowProperty(Function(n As INamespaceOrTypeSymbol) CType(n.GetMembers.OfType(Of INamespaceOrTypeSymbol)() _
                                                                                                                   .Where(Function(s) s.IsDefinedInSource() AndAlso s?.ContainingAssembly?.Name = _project.AssemblyName), INamespaceSymbol))
        End Function

        Friend Function GetSymbolsWithNewNames(toRename As IEnumerable(Of ISymbol),
                                                               canUse As Func(Of String, Boolean),
                                           canKeepOne As Boolean) As IEnumerable(Of (Original As ISymbol, NewName As String))
            Dim symbolsWithNewNames As IEnumerable(Of (Original As ISymbol, NewName As String)) =
                toRename.
                OrderByDescending(Function(x) x.DeclaredAccessibility).
                ThenByDescending(Function(x) x.Kind = SymbolKind.Parameter OrElse x.Kind = SymbolKind.Property).
                Skip(If(canKeepOne, 1, 0)).
                Select(Function(tr) As (Original As ISymbol, NewName As String)
                           Dim newName As String = GenerateUniqueName(GetBaseForNewName(tr), canUse)
                           Return (Original:=tr, newName)
                       End Function)
            Return symbolsWithNewNames
        End Function

        Friend Function GetSymbolsWithNewNames(symbolGroup As IReadOnlyCollection(Of ISymbol), names As HashSet(Of String), caseSensitive As Boolean) As IEnumerable(Of (Original As ISymbol, NewName As String))
            Dim canRename As IReadOnlyCollection(Of ISymbol) = symbolGroup.Where(Function(s) s.IsDefinedInSource() AndAlso s.CanBeReferencedByName).ToArray()
            Dim specialSymbolUsingName As Boolean = canRename.Count < symbolGroup.Count
            Dim methodSymbols As IMethodSymbol() = canRename.OfType(Of IMethodSymbol)().ToArray()
            Dim canKeepOneNormalMemberName As Boolean = Not specialSymbolUsingName AndAlso Not methodSymbols.Any()
            symbolGroup = canRename.Except(methodSymbols).ToArray()
            Dim methodsWithNewNames As (Original As ISymbol, NewName As String)() = GetMethodSymbolsWithNewNames(methodSymbols.ToArray(), names, specialSymbolUsingName, caseSensitive)
            Return GetSymbolsWithNewNames(symbolGroup, AddressOf names.Add, canKeepOneNormalMemberName).Concat(methodsWithNewNames)
        End Function

        Friend Async Function PerformRenamesAsync(prgt As Project, symbolsWithNewNames As IReadOnlyCollection(Of (Original As ISymbol, NewName As String))) As Task(Of Project)
            Dim projectSolution As Solution = prgt.Solution
            For Each sym As (Original As ISymbol, NewName As String) In symbolsWithNewNames
                prgt = projectSolution.GetProject(prgt.Id)
                Dim comp As Compilation = Await prgt.GetCompilationAsync().ConfigureAwait(False)
                Dim currentDeclaration As ISymbol = SymbolFinder.FindSimilarSymbols(sym.Original, comp).FirstOrDefault()
                If currentDeclaration Is Nothing Then Continue For ' Must have already renamed this symbol for a different reason
                projectSolution = Await Renamer.RenameSymbolAsync(projectSolution, currentDeclaration, sym.NewName, projectSolution.Workspace.Options).ConfigureAwait(False)
            Next

            Return projectSolution.GetProject(prgt.Id)
        End Function

    End Module
End Namespace
