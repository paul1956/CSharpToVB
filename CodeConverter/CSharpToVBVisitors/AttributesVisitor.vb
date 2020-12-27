' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBConverter.ToVisualBasic

    Partial Public Class CSharpConverter

        Partial Friend Class NodesVisitor
            Inherits CS.CSharpSyntaxVisitor(Of VB.VisualBasicSyntaxNode)

            Public Overrides Function VisitAttribute(node As CSS.AttributeSyntax) As VB.VisualBasicSyntaxNode
                Return Factory.Attribute(DirectCast(DirectCast(node.Parent, CSS.AttributeListSyntax).Target?.Accept(Me), VBS.AttributeTargetSyntax),
                                               DirectCast(node.Name.Accept(Me).WithConvertedTriviaFrom(node.Name), VBS.TypeSyntax),
                                               DirectCast(node.ArgumentList?.Accept(Me), VBS.ArgumentListSyntax))
            End Function

            Public Overrides Function VisitAttributeArgument(node As CSS.AttributeArgumentSyntax) As VB.VisualBasicSyntaxNode
                Dim name As VBS.NameColonEqualsSyntax = Nothing
                If node.NameColon IsNot Nothing Then
                    name = Factory.NameColonEquals(DirectCast(node.NameColon.Name.Accept(Me), VBS.IdentifierNameSyntax))
                    ' HACK for VB Error
                    If name.ToString = "[error]:=" Then
                        name = Nothing
                    End If
                ElseIf node.NameEquals IsNot Nothing Then
                    name = Factory.NameColonEquals(DirectCast(node.NameEquals.Name.Accept(Me), VBS.IdentifierNameSyntax))
                End If

                Dim value As VBS.ExpressionSyntax = DirectCast(node.Expression.Accept(Me), VBS.ExpressionSyntax)
                Return Factory.SimpleArgument(name, value).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitAttributeArgumentList(node As CSS.AttributeArgumentListSyntax) As VB.VisualBasicSyntaxNode
                Dim vbArguments As New List(Of VBS.ArgumentSyntax)
                Dim newTrailingTrivia As New SyntaxTriviaList
                Dim nameRequiredIndex As Integer = 0
                For Each e As IndexClass(Of CSS.AttributeArgumentSyntax) In node.Arguments.WithIndex
                    Dim localLeadingTrivia As New SyntaxTriviaList
                    Dim localTrailingTrivia As New SyntaxTriviaList
                    Dim item As VBS.ArgumentSyntax = DirectCast(e.Value.Accept(Me), VBS.ArgumentSyntax)
                    Dim name As VBS.IdentifierNameSyntax = Nothing
                    If item.IsNamed Then
                        If nameRequiredIndex = 0 Then
                            nameRequiredIndex = 1
                        End If
                        name = CType(item, VBS.SimpleArgumentSyntax).NameColonEquals.Name
                    End If
                    If nameRequiredIndex > 0 AndAlso Not item.IsNamed Then
                        If TypeOf node.Parent Is CSS.AttributeSyntax Then
                            Dim possibleMethodInfo As SymbolInfo = _semanticModel.GetSymbolInfo(CType(node.Parent, CSS.AttributeSyntax))
                            If possibleMethodInfo.CandidateSymbols.Length = 1 Then
                                Dim method As IMethodSymbol = CType(possibleMethodInfo.CandidateSymbols(0), IMethodSymbol)
                                name = Factory.IdentifierName(method.Parameters(e.index).Name)
                            End If
                        End If
                        If name Is Nothing Then
                            name = Factory.IdentifierName($"TODO_VBRequiresNameHere{nameRequiredIndex}")
                            nameRequiredIndex += 1
                        End If
                        Dim nameColonEquals As VBS.NameColonEqualsSyntax = Factory.NameColonEquals(name)
                        item = Factory.SimpleArgument(nameColonEquals, item.GetExpression)
                    End If
                    If item.HasLeadingTrivia Then
                        Dim initialTriviaList As SyntaxTriviaList = item.GetLeadingTrivia
                        For i1 As Integer = 0 To initialTriviaList.Count - 1
                            Dim trivia As SyntaxTrivia = initialTriviaList(i1)
                            Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(initialTriviaList, i1, LookaheadCount:=1)
                            If trivia.IsComment Then
                                newTrailingTrivia = newTrailingTrivia.Add(trivia)
                                If nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                    i1 += 1
                                End If
                            Else
                                If trivia.IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                                    Select Case nextTrivia.RawKind
                                        Case VB.SyntaxKind.EndOfLineTrivia
                                            i1 += 2
                                        Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.None
                                        Case Else
                                            Stop
                                            localLeadingTrivia = localLeadingTrivia.Add(trivia)
                                    End Select
                                Else
                                    localLeadingTrivia = localLeadingTrivia.Add(trivia)
                                End If
                            End If
                        Next
                        item = item.WithLeadingTrivia(localLeadingTrivia)
                    End If
                    If item.HasTrailingTrivia Then
                        For Each t As SyntaxTrivia In item.GetTrailingTrivia
                            If t.IsComment OrElse t.IsEndOfLine Then
                                newTrailingTrivia = newTrailingTrivia.Add(t)
                            Else
                                localTrailingTrivia = localTrailingTrivia.Add(t)
                            End If
                        Next
                    End If
                    item = item.WithTrailingTrivia(localTrailingTrivia)
                    vbArguments.Add(item)
                Next
                Return Factory.ArgumentList(openParenToken, Factory.SeparatedList(vbArguments), CloseParenToken).WithConvertedLeadingTriviaFrom(node).WithTrailingTrivia(newTrailingTrivia).WithAppendedTrailingTrivia(node.GetTrailingTrivia.ConvertTriviaList())
            End Function

            Public Overrides Function VisitAttributeList(node As CSS.AttributeListSyntax) As VB.VisualBasicSyntaxNode
                Dim csSeparators As IEnumerable(Of SyntaxToken) = node.Attributes.GetSeparators
                Dim lessThanTokenWithTrivia As SyntaxToken = LessThanToken.WithConvertedTriviaFrom(node.OpenBracketToken)
                Dim greaterThenTokenWithTrivia As SyntaxToken = GreaterThanToken.WithTrailingTrivia(node.CloseBracketToken.TrailingTrivia.ConvertTriviaList()).AdjustTokenTrailingTrivia(RemoveTrailingLineContinuation:=True)
                Dim attributeList As New List(Of VBS.AttributeSyntax)
                Dim separators As New List(Of SyntaxToken)
                Dim separatorCount As Integer = node.Attributes.Count - 1
                For index As Integer = 0 To separatorCount
                    Dim e As CSS.AttributeSyntax = node.Attributes(index)
                    attributeList.Add(DirectCast(e.Accept(Me), VBS.AttributeSyntax).RemoveExtraLeadingEOL)
                    If separatorCount > index Then
                        separators.Add(CommaToken.WithConvertedTrailingTriviaFrom(csSeparators(index)))
                    End If
                Next
                RestructureNodesAndSeparators(lessThanTokenWithTrivia, attributeList, separators, greaterThenTokenWithTrivia)
                Dim attributes As SeparatedSyntaxList(Of VBS.AttributeSyntax) = Factory.SeparatedList(attributeList, separators)
                If attributes.Last.HasTrailingTrivia Then
                    attributes.Replace(attributes.Last, attributes.Last.WithoutTrailingTrivia)
                End If
                Return Factory.AttributeList(lessThanTokenWithTrivia, attributes, greaterThenTokenWithTrivia)
            End Function

            Public Overrides Function VisitAttributeTargetSpecifier(node As CSS.AttributeTargetSpecifierSyntax) As VB.VisualBasicSyntaxNode
                Dim id As SyntaxToken
                Select Case CS.CSharpExtensions.Kind(node.Identifier)
                    Case CS.SyntaxKind.AssemblyKeyword
                        id = AssemblyKeyword
                    Case CS.SyntaxKind.ModuleKeyword
                        id = ModuleKeyword
                    Case CS.SyntaxKind.ParamKeyword, CS.SyntaxKind.TypeKeyword
                        Return Nothing
                    Case CS.SyntaxKind.ReturnKeyword
                        ' Not necessary, return attributes are moved by ConvertAndSplitAttributes.
                        Return Nothing
                    Case Else
                        Return Nothing
                End Select
                Return Factory.AttributeTarget(id).WithConvertedTriviaFrom(node)
            End Function

        End Class

    End Class

End Namespace
