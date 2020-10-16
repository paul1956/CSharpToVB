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
                Dim list As CSS.AttributeListSyntax = DirectCast(node.Parent, CSS.AttributeListSyntax)
                Return Factory.Attribute(DirectCast(list.Target?.Accept(Me), VBS.AttributeTargetSyntax),
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
                    Dim Item As VBS.ArgumentSyntax = DirectCast(e.Value.Accept(Me), VBS.ArgumentSyntax)
                    Dim name As VBS.IdentifierNameSyntax = Nothing
                    If Item.IsNamed Then
                        If nameRequiredIndex = 0 Then
                            nameRequiredIndex = 1
                        End If
                        name = CType(Item, VBS.SimpleArgumentSyntax).NameColonEquals.Name
                    End If
                    If nameRequiredIndex > 0 AndAlso Not Item.IsNamed Then
                        If TypeOf node.Parent Is CSS.AttributeSyntax Then
                            Dim possibleMethodInfo As SymbolInfo = _mSemanticModel.GetSymbolInfo(CType(node.Parent, CSS.AttributeSyntax))
                            If possibleMethodInfo.CandidateSymbols.Length = 1 Then
                                Dim method As IMethodSymbol = CType(possibleMethodInfo.CandidateSymbols(0), IMethodSymbol)
                                name = Factory.IdentifierName(method.Parameters(e.Index).Name)
                            End If
                        End If
                        If name Is Nothing Then
                            name = Factory.IdentifierName($"TODO_VBRequiresNameHere{nameRequiredIndex}")
                            nameRequiredIndex += 1
                        End If
                        Dim nameColonEquals As VBS.NameColonEqualsSyntax = Factory.NameColonEquals(name)
                        Item = Factory.SimpleArgument(nameColonEquals, Item.GetExpression)
                    End If
                    If Item.HasLeadingTrivia Then
                        Dim initialTriviaList As SyntaxTriviaList = Item.GetLeadingTrivia
                        For i1 As Integer = 0 To initialTriviaList.Count - 1
                            Dim Trivia As SyntaxTrivia = initialTriviaList(i1)
                            Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(initialTriviaList, i1, LookaheadCount:=1)
                            If Trivia.IsComment Then
                                newTrailingTrivia = newTrailingTrivia.Add(Trivia)
                                If nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                    i1 += 1
                                End If
                            Else
                                If Trivia.IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                                    Select Case nextTrivia.RawKind
                                        Case VB.SyntaxKind.EndOfLineTrivia
                                            i1 += 2
                                        Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.None
                                        Case Else
                                            Stop
                                            localLeadingTrivia = localLeadingTrivia.Add(Trivia)
                                    End Select
                                Else
                                    localLeadingTrivia = localLeadingTrivia.Add(Trivia)
                                End If
                            End If
                        Next
                        Item = Item.WithLeadingTrivia(localLeadingTrivia)
                    End If
                    If Item.HasTrailingTrivia Then
                        For Each t As SyntaxTrivia In Item.GetTrailingTrivia
                            If t.IsComment OrElse t.IsEndOfLine Then
                                newTrailingTrivia = newTrailingTrivia.Add(t)
                            Else
                                localTrailingTrivia = localTrailingTrivia.Add(t)
                            End If
                        Next
                    End If
                    Item = Item.WithTrailingTrivia(localTrailingTrivia)
                    vbArguments.Add(Item)
                Next
                Return Factory.ArgumentList(OpenParenToken, Factory.SeparatedList(vbArguments), CloseParenToken).WithConvertedLeadingTriviaFrom(node).WithTrailingTrivia(newTrailingTrivia).WithAppendedTrailingTrivia(node.GetTrailingTrivia.ConvertTriviaList())
            End Function

            Public Overrides Function VisitAttributeList(node As CSS.AttributeListSyntax) As VB.VisualBasicSyntaxNode
                Dim csSeparators As IEnumerable(Of SyntaxToken) = node.Attributes.GetSeparators
                Dim LessThanTokenWithTrivia As SyntaxToken = LessThanToken.WithConvertedTriviaFrom(node.OpenBracketToken)
                Dim GreaterThenTokenWithTrivia As SyntaxToken = GreaterThanToken.WithTrailingTrivia(node.CloseBracketToken.TrailingTrivia.ConvertTriviaList()).AdjustTokenTrailingTrivia(RemoveTrailingLineContinuation:=True)
                Dim AttributeList As New List(Of VBS.AttributeSyntax)
                Dim Separators As New List(Of SyntaxToken)
                Dim SeparatorCount As Integer = node.Attributes.Count - 1
                For index As Integer = 0 To SeparatorCount
                    Dim e As CSS.AttributeSyntax = node.Attributes(index)
                    AttributeList.Add(DirectCast(e.Accept(Me), VBS.AttributeSyntax).RemoveExtraLeadingEOL)
                    If SeparatorCount > index Then
                        Separators.Add(CommaToken.WithConvertedTrailingTriviaFrom(csSeparators(index)))
                    End If
                Next
                RestructureNodesAndSeparators(LessThanTokenWithTrivia, AttributeList, Separators, GreaterThenTokenWithTrivia)
                Dim Attributes1 As SeparatedSyntaxList(Of VBS.AttributeSyntax) = Factory.SeparatedList(AttributeList, Separators)
                If Attributes1.Last.HasTrailingTrivia Then
                    Attributes1.Replace(Attributes1.Last, Attributes1.Last.WithoutTrailingTrivia)
                End If
                Return Factory.AttributeList(LessThanTokenWithTrivia, Attributes1, GreaterThenTokenWithTrivia)
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
