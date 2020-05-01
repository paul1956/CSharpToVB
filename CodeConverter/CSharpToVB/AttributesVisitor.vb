' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports CSharpToVBCodeConverter.Util

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBCodeConverter.DestVisualBasic

    Partial Public Class CSharpConverter

        Partial Friend Class NodesVisitor
            Inherits CS.CSharpSyntaxVisitor(Of VB.VisualBasicSyntaxNode)

            Public Overrides Function VisitAttribute(node As CSS.AttributeSyntax) As VB.VisualBasicSyntaxNode
                Dim list As CSS.AttributeListSyntax = DirectCast(node.Parent, CSS.AttributeListSyntax)
                Return VBFactory.Attribute(DirectCast(list.Target?.Accept(Me), VBS.AttributeTargetSyntax),
                                               DirectCast(node.Name.Accept(Me).WithConvertedTriviaFrom(node.Name), VBS.TypeSyntax),
                                               DirectCast(node.ArgumentList?.Accept(Me), VBS.ArgumentListSyntax))
            End Function

            Public Overrides Function VisitAttributeArgument(node As CSS.AttributeArgumentSyntax) As VB.VisualBasicSyntaxNode
                Dim name As VBS.NameColonEqualsSyntax = Nothing
                If node.NameColon IsNot Nothing Then
                    name = VBFactory.NameColonEquals(DirectCast(node.NameColon.Name.Accept(Me), VBS.IdentifierNameSyntax))
                    ' HACK for VB Error
                    If name.ToString = "[error]:=" Then
                        name = Nothing
                    End If
                ElseIf node.NameEquals IsNot Nothing Then
                    name = VBFactory.NameColonEquals(DirectCast(node.NameEquals.Name.Accept(Me), VBS.IdentifierNameSyntax))
                End If

                Dim value As VBS.ExpressionSyntax = DirectCast(node.Expression.Accept(Me), VBS.ExpressionSyntax)
                Return VBFactory.SimpleArgument(name, value).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitAttributeArgumentList(node As CSS.AttributeArgumentListSyntax) As VB.VisualBasicSyntaxNode
                Dim vbArguments As New List(Of VBS.ArgumentSyntax)
                Dim trailingTriviaList As New List(Of SyntaxTrivia)
                For Each e As IndexClass(Of CSS.AttributeArgumentSyntax) In node.Arguments.WithIndex
                    Dim localLeadingTrivia As New List(Of SyntaxTrivia)
                    Dim localTrailingTrivia As New List(Of SyntaxTrivia)
                    Dim Item As VBS.ArgumentSyntax = DirectCast(e.Value.Accept(Me), VBS.ArgumentSyntax)
                    If Item.HasLeadingTrivia Then
                        Dim triviaList As SyntaxTriviaList = Item.GetLeadingTrivia
                        For i1 As Integer = 0 To triviaList.Count - 1
                            Dim Trivia As SyntaxTrivia = triviaList(i1)
                            Dim NextTrivia As SyntaxTrivia = If(i1 < triviaList.Count - 1, triviaList(i1 + 1), Nothing)
                            If Trivia.IsComment Then
                                trailingTriviaList.Add(Trivia)
                                If NextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                    i1 += 1
                                End If
                            Else
                                If Trivia.IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                                    Select Case NextTrivia.RawKind
                                        Case VB.SyntaxKind.EndOfLineTrivia
                                            i1 += 2
                                        Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.None
                                        Case Else
                                            Stop
                                            localLeadingTrivia.Add(Trivia)
                                    End Select
                                Else
                                    localLeadingTrivia.Add(Trivia)
                                End If
                            End If
                        Next
                        Item = Item.WithLeadingTrivia(localLeadingTrivia)
                    End If
                    If Item.HasTrailingTrivia Then
                        For Each t As SyntaxTrivia In Item.GetTrailingTrivia
                            If t.IsComment OrElse t.IsEndOfLine Then
                                trailingTriviaList.Add(t)
                            Else
                                localTrailingTrivia.Add(t)
                            End If
                        Next
                    End If
                    Item = Item.WithTrailingTrivia(localTrailingTrivia)
                    vbArguments.Add(Item)
                Next
                Return VBFactory.ArgumentList(OpenParenToken, VBFactory.SeparatedList(vbArguments), CloseParenToken).WithConvertedLeadingTriviaFrom(node).WithTrailingTrivia(trailingTriviaList).WithAppendedTrailingTrivia(ConvertTrivia(node.GetTrailingTrivia))
            End Function

            Public Overrides Function VisitAttributeList(node As CSS.AttributeListSyntax) As VB.VisualBasicSyntaxNode
                Dim csSeparators As IEnumerable(Of SyntaxToken) = node.Attributes.GetSeparators
                Dim LessThanTokenWithTrivia As SyntaxToken = LessThanToken.WithConvertedTriviaFrom(node.OpenBracketToken)
                Dim finalTrailingTriviaList As New List(Of SyntaxTrivia)
                Dim FirstComment As Boolean = True
                Dim NeedWhiteSpace As Boolean = True
                Dim needLineContinuation As Boolean = False
                For Each T As SyntaxTrivia In node.CloseBracketToken.TrailingTrivia
                    Dim VBSyntaxTrivia As SyntaxTrivia = ConvertTrivia(T)
                    Select Case VBSyntaxTrivia.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            finalTrailingTriviaList.Add(VBSyntaxTrivia)
                            NeedWhiteSpace = False
                        Case VB.SyntaxKind.CommentTrivia
                            If FirstComment Then
                                FirstComment = False
                                If NeedWhiteSpace Then
                                    NeedWhiteSpace = False
                                    finalTrailingTriviaList.Add(SpaceTrivia)
                                End If
                                finalTrailingTriviaList.Add(LineContinuation)
                                needLineContinuation = False
                            End If
                            finalTrailingTriviaList.Add(VBSyntaxTrivia)
                        Case VB.SyntaxKind.EndOfLineTrivia
                            If NeedWhiteSpace Then
                                NeedWhiteSpace = False
                                finalTrailingTriviaList.Add(SpaceTrivia)
                            End If
                            If needLineContinuation Then
                                finalTrailingTriviaList.Add(LineContinuation)
                                needLineContinuation = False
                                NeedWhiteSpace = True
                            End If
                            finalTrailingTriviaList.Add(VBSyntaxTrivia)
                        Case Else
                            Stop
                            NeedWhiteSpace = True
                    End Select
                Next
                Dim GreaterThenTokenWithTrivia As SyntaxToken = GreaterThanToken.WithTrailingTrivia(finalTrailingTriviaList)

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
                Dim Attributes1 As SeparatedSyntaxList(Of VBS.AttributeSyntax) = VBFactory.SeparatedList(AttributeList, Separators)
                If Attributes1.Last.HasTrailingTrivia Then
                    Attributes1.Replace(Attributes1.Last, Attributes1.Last.WithoutTrailingTrivia)
                End If
                Return VBFactory.AttributeList(LessThanTokenWithTrivia, Attributes1, GreaterThenTokenWithTrivia)
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
                Return VBFactory.AttributeTarget(id).WithConvertedTriviaFrom(node)
            End Function

        End Class

    End Class

End Namespace
