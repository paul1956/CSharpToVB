' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports Microsoft.CodeAnalysis
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax

Namespace CSharpToVBConverter.CSharpToVBVisitors

    Partial Public Class CSharpConverter

        Partial Friend Class NodesVisitor

            ''' <summary>
            ''' Ensures that any 'names' is unique and does not collide with any other name.  Names that
            ''' are marked as IsFixed can not be touched.  This does mean that if there are two names
            ''' that are the same, and both are fixed that you will end up with non-unique names at the
            ''' end.
            ''' </summary>
            Private Shared Function EnsureUniqueness(names As IList(Of String), isFixed As IList(Of Boolean), Optional canUse As Func(Of String, Boolean) = Nothing) As IList(Of String)
                Dim copy As List(Of String) = names.ToList()
                EnsureUniquenessInPlace(copy, isFixed, canUse)
                Return copy
            End Function

            ''' <summary>
            ''' Transforms baseName into a name that does not conflict with any name in 'reservedNames'
            ''' </summary>
            Private Shared Function EnsureUniqueness(baseName As String, usedIdentifiers As Dictionary(Of String, SymbolTableEntry), reservedNames As IEnumerable(Of String)) As String
                Dim names As New List(Of String) From {baseName}
                Dim isFixed As New List(Of Boolean) From {False}
                For Each s As SymbolTableEntry In usedIdentifiers.Values
                    names.Add(s.Name)
                Next
                For Each s As String In reservedNames.Distinct
                    If names.Contains(s) Then Continue For
                    names.Add(s)
                Next
                isFixed.AddRange(Enumerable.Repeat(True, names.Count - 1))

                Return EnsureUniqueness(names, isFixed).First()
            End Function

            Private Shared Sub EnsureUniquenessInPlace(names As IList(Of String), isFixed As IList(Of Boolean), canUse As Func(Of String, Boolean))
                canUse = If(canUse, Function(s As String) True)

                ' Don't enumerate as we will be modifying the collection in place.
                Dim i As Integer = 0
                Do While i < names.Count
                    Dim name As String = names(i)
                    Dim collisionIndices As List(Of Integer) = GetCollisionIndices(names, name, isCaseSensitive:=False)

                    If canUse(name) AndAlso collisionIndices.Count < 2 Then
                        ' no problems with this parameter name, move onto the next one.
                        i += 1
                        Continue Do
                    End If

                    HandleCollisions(isFixed, names, name, collisionIndices, canUse, isCaseSensitive:=False)
                    i += 1
                Loop
            End Sub

            Private Shared Function GenerateUniqueName(baseName As String, extension As String, canUse As Func(Of String, Boolean)) As String
                If Not String.IsNullOrEmpty(extension) AndAlso Not extension.StartsWith(".", StringComparison.OrdinalIgnoreCase) Then
                    extension = "." & extension
                End If

                Dim name As String = baseName & extension
                Dim index As Integer = 1

                ' Check for collisions
                Do While Not canUse(name)
                    name = baseName & index & extension
                    index += 1
                Loop

                Return name
            End Function

            Private Shared Function GetCollisionIndices(names As IList(Of String), name As String, Optional isCaseSensitive As Boolean = True) As List(Of Integer)
                Dim comparer As StringComparer = If(isCaseSensitive, StringComparer.Ordinal, StringComparer.OrdinalIgnoreCase)
                Dim collisionIndices As List(Of Integer) = names.Select(Function(currentName As String, index As Integer) New With {Key currentName, Key index}).Where(Function(t) comparer.Equals(t.currentName, name)).Select(Function(t) t.index).ToList()
                Return collisionIndices
            End Function

            Private Shared Sub HandleCollisions(isFixed As IList(Of Boolean), names As IList(Of String), name As String, collisionIndices As List(Of Integer), canUse As Func(Of String, Boolean), Optional isCaseSensitive As Boolean = True)
                Dim suffix As Integer = 1
                Dim comparer As StringComparer = If(isCaseSensitive, StringComparer.Ordinal, StringComparer.OrdinalIgnoreCase)
                For Each e As IndexClass(Of Integer) In collisionIndices.WithIndex
                    If isFixed(e.Value) Then
                        ' can't do anything about this name.
                        Continue For
                    End If

                    Do
                        Dim newName As String = name & suffix
                        suffix += 1
                        If Not names.Contains(newName, comparer) AndAlso canUse(newName) Then
                            ' Found a name that doesn't conflict with anything else.
                            names(e.Value) = newName
                            Exit Do
                        End If
                    Loop
                Next
            End Sub

            Friend Shared Function GenerateUniqueName(baseName As String, canUse As Func(Of String, Boolean)) As String
                Return GenerateUniqueName(baseName, String.Empty, canUse)
            End Function

            Friend Function GetUniqueVariableNameInScope(node As CS.CSharpSyntaxNode, variableNameBase As String, usedIdentifiers As Dictionary(Of String, SymbolTableEntry)) As String
                Dim isField As Boolean = node.AncestorsAndSelf().OfType(Of CSS.FieldDeclarationSyntax).Any
                Dim reservedNames As New List(Of String) From {
                        "_"
                        }
                reservedNames.AddRange(node.DescendantNodesAndSelf().SelectMany(Function(lSyntaxNode As SyntaxNode) _semanticModel.LookupSymbols(lSyntaxNode.SpanStart).Select(Function(s As ISymbol) s.Name)).Distinct)
                Dim uniqueVariableName As String = EnsureUniqueness(variableNameBase, usedIdentifiers, reservedNames)
                usedIdentifiers.Add(uniqueVariableName,
                                    New SymbolTableEntry(uniqueVariableName,
                                                         isType:=False,
                                                         isField
                                                         )
                                    )
                Return uniqueVariableName
            End Function

        End Class

    End Class

End Namespace
