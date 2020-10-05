' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Diagnostics.CodeAnalysis
Imports System.Runtime.CompilerServices
Imports Microsoft.CodeAnalysis
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax

Namespace CSharpToVBConverter

    <ExcludeFromCodeCoverage>
    Friend Module ClashingMemberRenamer

        Private Function DeclarationWhereManyNotNull(Of TNode, TResult)(symbol As ISymbol, selectManyWhereNotNull As Func(Of TNode, IEnumerable(Of TResult))) As IEnumerable(Of TResult)
            Return symbol.DeclaringSyntaxReferences.Select(Function(d) d.GetSyntax()).OfType(Of TNode)().SelectMany(selectManyWhereNotNull).Where(Function(x) x IsNot Nothing)
        End Function

        Private Function DeclarationWhereNotNull(Of TNode, TResult)(symbol As ISymbol, selectWhereNotNull As Func(Of TNode, TResult)) As IEnumerable(Of TResult)
            Return symbol.DeclaringSyntaxReferences.Select(Function(d) d.GetSyntax()).OfType(Of TNode)().Select(selectWhereNotNull).Where(Function(x) x IsNot Nothing)
        End Function

        ''' <remarks>
        ''' In VB there's a special extra local defined with the same name as the method name, so the method symbol should be included in any conflict analysis
        ''' </remarks>
        Private Function GetCsLocalSymbolsPerScope(_semanticModel As SemanticModel, symbol As ISymbol) As IEnumerable(Of IEnumerable(Of ISymbol))
            Select Case True
                Case TypeOf symbol Is IMethodSymbol
                    Dim methodSymbol As IMethodSymbol = CType(symbol, IMethodSymbol)
                    Return GetCsSymbolsDeclaredByMethod(_semanticModel, methodSymbol, Function(n As CSS.BaseMethodDeclarationSyntax) If(CType(n.ExpressionBody, CS.CSharpSyntaxNode), n.Body), New SymbolKind() {SymbolKind.Local, SymbolKind.Parameter, SymbolKind.TypeParameter})
                Case TypeOf symbol Is IPropertySymbol
                    Dim propertySymbol As IPropertySymbol = CType(symbol, IPropertySymbol)
                    Return GetCsSymbolsDeclaredByProperty(_semanticModel, propertySymbol)
                Case TypeOf symbol Is IEventSymbol
                    Dim eventSymbol As IEventSymbol = CType(symbol, IEventSymbol)
                    Return GetCsSymbolsDeclaredByEvent(_semanticModel, eventSymbol)
                Case TypeOf symbol Is IFieldSymbol
                    Dim fieldSymbol As IFieldSymbol = CType(symbol, IFieldSymbol)
                    Return GetCsSymbolsDeclaredByField(_semanticModel, fieldSymbol).Yield()
                Case Else
                    Return Array.Empty(Of ISymbol).Yield
            End Select
        End Function

        Private Function GetCsSymbolsDeclaredByEvent(_semanticModel As SemanticModel, propertySymbol As IEventSymbol) As IEnumerable(Of IEnumerable(Of ISymbol))
            Dim kinds As SymbolKind() = New SymbolKind() {SymbolKind.Local, SymbolKind.TypeParameter}
            Dim getAccessorBody As Func(Of CSS.AccessorDeclarationSyntax, CS.CSharpSyntaxNode) = Function(n) If(CType(n.ExpressionBody, CS.CSharpSyntaxNode), n.Body)
            Return GetCsSymbolsDeclaredByMethod(_semanticModel, propertySymbol.AddMethod, getAccessorBody, kinds) _
            .Concat(GetCsSymbolsDeclaredByMethod(_semanticModel, propertySymbol.RemoveMethod, getAccessorBody, kinds))
        End Function

        Private Function GetCsSymbolsDeclaredByField(_semanticModel As SemanticModel, fieldSymbol As IFieldSymbol) As IEnumerable(Of ISymbol)
            Return DeclarationWhereManyNotNull(fieldSymbol,
            Function(f As CSS.BaseFieldDeclarationSyntax) f.Declaration.Variables.Select(Function(v) v.Initializer?.Value)) _
            .SelectMany(Function(i) _semanticModel.LookupSymbols(i.SpanStart, fieldSymbol.ContainingType))
        End Function

        Private Function GetCsSymbolsDeclaredByMethod(Of TNode)(_semanticModel As SemanticModel, methodSymbol As IMethodSymbol, selectWhereNotNull As Func(Of TNode, CS.CSharpSyntaxNode), kinds As SymbolKind()) As IEnumerable(Of IEnumerable(Of ISymbol))
            If methodSymbol Is Nothing Then
                Return Enumerable.Empty(Of IEnumerable(Of ISymbol))()
            End If
            Dim bodies As IEnumerable(Of CS.CSharpSyntaxNode) = DeclarationWhereNotNull(methodSymbol, selectWhereNotNull).Where(Function(x) x.SyntaxTree Is _semanticModel.SyntaxTree)
            Return bodies.SelectMany(AddressOf GetDeepestBlocks).Select(Function(block) _semanticModel.LookupSymbols(block.SpanStart).Where(Function(x As ISymbol) x.MatchesKind(kinds)))
        End Function

        Private Function GetCsSymbolsDeclaredByProperty(_semanticModel As SemanticModel, propertySymbol As IPropertySymbol) As IEnumerable(Of IEnumerable(Of ISymbol))
            Dim getAccessorBody As Func(Of CSS.AccessorDeclarationSyntax, CS.CSharpSyntaxNode) = Function(n) If(CType(n.ExpressionBody, CS.CSharpSyntaxNode), n.Body)
            Return GetCsSymbolsDeclaredByMethod(_semanticModel, propertySymbol.GetMethod, getAccessorBody, New SymbolKind() {SymbolKind.Local, SymbolKind.Parameter, SymbolKind.TypeParameter}) _
            .Concat(GetCsSymbolsDeclaredByMethod(_semanticModel, propertySymbol.SetMethod, getAccessorBody, New SymbolKind() {SymbolKind.Local, SymbolKind.TypeParameter}))
        End Function

#Disable Warning IDE0051 ' Remove unused private members
        <Extension>
        Private Function GetCsSymbolsPerScope(_semanticModel As SemanticModel, symbol As ISymbol) As IEnumerable(Of IEnumerable(Of ISymbol))
#Enable Warning IDE0051 ' Remove unused private members
            Return GetCsLocalSymbolsPerScope(_semanticModel, symbol).Select(Function(y) y.Union(symbol.Yield()))
        End Function

        Private Function GetDeepestBlocks(_body As CS.CSharpSyntaxNode) As IEnumerable(Of CSS.BlockSyntax)
            Return _body.DescendantNodesAndSelf().OfType(Of CSS.BlockSyntax)().Where(Function(x) Not x.DescendantNodes().OfType(Of CSS.BlockSyntax)().Any())
        End Function

        Private Function GetLocalSymbolSets(containerSymbol As INamespaceOrTypeSymbol, compilation As Compilation, members As IReadOnlyCollection(Of ISymbol)) As IEnumerable(Of IEnumerable(Of ISymbol))
            If Not (TypeOf containerSymbol Is ITypeSymbol) Then
                Return Enumerable.Empty(Of IEnumerable(Of ISymbol))()
            End If

            Dim semanticModels As IEnumerable(Of SemanticModel) = containerSymbol.Locations.Select(Function(loc) loc.SourceTree).Distinct() _
                .Where(AddressOf compilation.ContainsSyntaxTree) _
                .Select(Function(_sourceTree) compilation.GetSemanticModel(_sourceTree, ignoreAccessibility:=True))
            Return semanticModels.SelectMany(Function(semanticModel) members.SelectMany(AddressOf semanticModel.GetCsSymbolsPerScope))
        End Function

        Private Function GetSymbolsWithNewNames(containerSymbol As INamespaceOrTypeSymbol, compilation As Compilation) As IEnumerable(Of (Original As ISymbol, NewName As String))
            Dim members As ISymbol() = containerSymbol.GetMembers() _
                .Where(Function(m) m.Locations.Any(Function(loc) loc.SourceTree IsNot Nothing AndAlso compilation.ContainsSyntaxTree(loc.SourceTree))).ToArray()
            Dim symbolSets As IEnumerable(Of IEnumerable(Of ISymbol)) = GetLocalSymbolSets(containerSymbol, compilation, members).Concat(members.AsEnumerable().Yield())
            Return symbolSets.SelectMany(AddressOf GetUniqueNamesForSymbolSet)
        End Function

        Private Function GetUniqueNamesForSymbolSet(symbols As IEnumerable(Of ISymbol)) As IEnumerable(Of (Original As ISymbol, NewName As String))
            Dim membersByCaseInsensitiveName As ILookup(Of String, ISymbol) = symbols.ToLookup(AddressOf GetName, Function(m) m, StringComparer.OrdinalIgnoreCase)
            Dim names As HashSet(Of String) = New HashSet(Of String)(membersByCaseInsensitiveName.Select(Function(ms) ms.Key),
                StringComparer.OrdinalIgnoreCase)
            Dim symbolsWithNewNames As IEnumerable(Of (Original As ISymbol, NewName As String)) = membersByCaseInsensitiveName.Where(Function(ms) ms.Count() > 1) _
                .SelectMany(Function(symbolGroup) SymbolRenamer.GetSymbolsWithNewNames(symbolGroup.ToArray(), names, False))
            Return symbolsWithNewNames
        End Function

        ''' <summary>
        ''' Renames symbols in a CSharp project so that they don't clash on case within the same named scope, attempting to rename the least public ones first.
        ''' This is because C# is case sensitive but VB is case insensitive.
        ''' </summary>
        ''' <remarks>
        ''' Cases in different named scopes should be dealt with by <seealso cref= "DocumentExtensions.ExpandAsync"/>.
        ''' For names scoped within a type member, see <seealso cref= "SemanticModelExtensions.GetCsLocalSymbolsPerScope"/>.
        ''' </remarks>
        Public Async Function RenameClashingSymbolsAsync(_project As Project) As Task(Of Project)
            Dim compilation As Compilation = Await _project.GetCompilationAsync().ConfigureAwait(False)
            Dim memberRenames As IEnumerable(Of (Original As ISymbol, NewName As String)) = GetNamespacesAndTypesInAssembly(_project, compilation) _
                .SelectMany(Function(x) GetSymbolsWithNewNames(x, compilation))
            Return Await PerformRenamesAsync(_project, memberRenames.ToList()).ConfigureAwait(False)
        End Function

    End Module
End Namespace
