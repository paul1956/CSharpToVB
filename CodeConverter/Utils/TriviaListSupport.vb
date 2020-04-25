' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports CSharpToVBCodeConverter.Util

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory

Namespace CSharpToVBCodeConverter.DestVisualBasic

    Public Module TriviaListSupport

        Friend Sub RelocateAttributeDirectiveDisabledTrivia(TriviaList As SyntaxTriviaList, FoundDirective As Boolean, IsTheory As Boolean, ByRef StatementLeadingTrivia As List(Of SyntaxTrivia), ByRef StatementTrailingTrivia As List(Of SyntaxTrivia))
            If IsTheory Then
                Return
            End If
            For Each t As SyntaxTrivia In TriviaList
                If Not (t.IsDirective OrElse t.MatchesKind(VB.SyntaxKind.DisabledTextTrivia,
                                                           VB.SyntaxKind.RegionDirectiveTrivia,
                                                           VB.SyntaxKind.EndRegionDirectiveTrivia)) Then
                    Continue For
                End If
                If FoundDirective Then
                    If StatementTrailingTrivia.Count = 0 Then
                        StatementTrailingTrivia.Add(VBEOLTrivia)
                    End If
                    StatementTrailingTrivia.Add(t)
                Else
                    If t.IsDirective Then
                        StatementLeadingTrivia.Add(t)
                        FoundDirective = True
                    Else
                        StatementTrailingTrivia.Add(t)
                    End If
                End If
            Next
        End Sub

        Friend Function RelocateDirectiveDisabledTrivia(TriviaList As SyntaxTriviaList, ByRef StatementTrivia As List(Of SyntaxTrivia), RemoveEOL As Boolean) As SyntaxTriviaList
            Dim NewTrivia As New SyntaxTriviaList
            Dim FoundDirective As Boolean = False
            For Each t As SyntaxTrivia In TriviaList
                If Not (FoundDirective OrElse t.IsDirective OrElse t.IsKind(VB.SyntaxKind.DisabledTextTrivia)) Then
                    If t.IsEndOfLine AndAlso RemoveEOL Then
                        NewTrivia = NewTrivia.Add(SpaceTrivia)
                    Else
                        NewTrivia = NewTrivia.Add(t)
                    End If
                Else
                    If t.IsDirective Then
                        StatementTrivia.Add(VBEOLTrivia)
                        StatementTrivia.Add(t)
                        FoundDirective = True
                    Else
                        StatementTrivia.Add(t)
                    End If
                End If
            Next
            Return NewTrivia
        End Function

        Friend Function RelocateLeadingCommentTrivia(TriviaList As SyntaxTriviaList, ByRef StatementLeadingTrivia As List(Of SyntaxTrivia)) As SyntaxTriviaList
            Dim NewLeadingTrivia As New SyntaxTriviaList
            Dim FoundComment As Boolean = False
            For Each t As SyntaxTrivia In TriviaList
                If Not (FoundComment OrElse t.IsComment) Then
                    If t.IsEndOfLine Then
                        NewLeadingTrivia = NewLeadingTrivia.Add(SpaceTrivia)
                    Else
                        NewLeadingTrivia = NewLeadingTrivia.Add(t)
                    End If
                Else
                    If t.IsComment Then
                        StatementLeadingTrivia.Add(VBEOLTrivia)
                        StatementLeadingTrivia.Add(t)
                        FoundComment = True
                    Else
                        StatementLeadingTrivia.Add(t)
                    End If
                End If
            Next
            Return NewLeadingTrivia
        End Function

        Friend Function TranslateTokenList(ChildTokens As IEnumerable(Of SyntaxToken)) As SyntaxTokenList
            Dim newTokenList As New SyntaxTokenList
            For Each token As SyntaxToken In ChildTokens
                Dim newLeadingTriviaList As New SyntaxTriviaList
                Dim newTrailingTriviaList As New SyntaxTriviaList

                Dim tokenText As String = token.Text
                Dim valueText As String = token.ValueText
                If token.HasLeadingTrivia Then
                    For Each t As SyntaxTrivia In token.LeadingTrivia
                        If t.IsKind(CS.SyntaxKind.DocumentationCommentExteriorTrivia) Then
                            newLeadingTriviaList = newLeadingTriviaList.Add(VBFactory.DocumentationCommentExteriorTrivia(token.LeadingTrivia(0).
                                                                                                                         ToString().
                                                                                                                         Replace("///", "'''", StringComparison.Ordinal)))
                            If Not tokenText.StartsWith(" ", StringComparison.Ordinal) Then
                                tokenText = " " & tokenText
                                valueText = " " & valueText
                            End If
                        Else
                            Stop
                        End If
                    Next
                End If
                Select Case token.RawKind
                    Case CS.SyntaxKind.XmlTextLiteralToken
                        newTokenList = newTokenList.Add(VBFactory.XmlTextLiteralToken(newLeadingTriviaList, tokenText, valueText, newTrailingTriviaList))
                    Case CS.SyntaxKind.XmlTextLiteralNewLineToken
                        newTokenList = newTokenList.Add(VBFactory.XmlTextNewLine(text:=vbCrLf, value:=vbCrLf, newLeadingTriviaList, newTrailingTriviaList))
                    Case CS.SyntaxKind.XmlEntityLiteralToken
                        newTokenList = newTokenList.Add(VBFactory.XmlEntityLiteralToken(newLeadingTriviaList, tokenText, valueText, newTrailingTriviaList))
                    Case Else
                        Stop
                End Select
            Next
            Return newTokenList
        End Function

        Friend Function TriviaIsIdentical(LeadingTrivia As SyntaxTriviaList, NodeLeadingTrivia As List(Of SyntaxTrivia)) As Boolean
            If LeadingTrivia.Count <> NodeLeadingTrivia.Count Then
                Return False
            End If
            If Not (LeadingTrivia.ContainsCommentOrDirectiveTrivia OrElse NodeLeadingTrivia.ContainsCommentOrDirectiveTrivia) Then
                Return False
            End If

            For index As Integer = 0 To LeadingTrivia.Count - 1
                If LeadingTrivia(index).ToFullString.Trim <> NodeLeadingTrivia(index).ToFullString.Trim Then
                    Return False
                End If
            Next
            Return True
        End Function

    End Module

End Namespace
