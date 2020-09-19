' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports Microsoft.CodeAnalysis
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic

Namespace CSharpToVBConverter.ToVisualBasic

    Public Module TriviaListSupport

        ''' <summary>
        ''' Compare 2 Trivia Lists and see if they both end with the same SyntaxKinds
        ''' </summary>
        ''' <param name="NodeLeadingTrivia"></param>
        ''' <param name="LeadingTrivia"></param>
        ''' <returns>True if LeadingTrivia ends with the same trivia kinds as NodeLeadingTrivia</returns>
        Friend Function EndsWithSimilarTrivia(NodeLeadingTrivia As SyntaxTriviaList, LeadingTrivia As SyntaxTriviaList) As Boolean
            If NodeLeadingTrivia.Count > LeadingTrivia.Count Then
                Return False
            End If
            If (Not LeadingTrivia.ContainsCommentOrDirectiveTrivia) OrElse (Not NodeLeadingTrivia.ContainsCommentOrDirectiveTrivia) Then
                Return False
            End If

            For index As Integer = 1 To NodeLeadingTrivia.Count
                If LeadingTrivia(LeadingTrivia.Count - index).RawKind <> NodeLeadingTrivia(NodeLeadingTrivia.Count - index).RawKind Then
                    Return False
                End If
            Next
            Return True
        End Function

        Friend Sub RelocateAttributeDirectiveDisabledTrivia(TriviaList As SyntaxTriviaList, FoundDirective As Boolean, IsTheory As Boolean, ByRef StatementLeadingTrivia As SyntaxTriviaList, ByRef StatementTrailingTrivia As SyntaxTriviaList)
            If IsTheory Then
                Exit Sub
            End If
            For Each t As SyntaxTrivia In TriviaList
                If Not (t.IsDirective OrElse t.IsKind(VB.SyntaxKind.DisabledTextTrivia,
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
                        StatementLeadingTrivia = StatementLeadingTrivia.Add(t)
                        FoundDirective = True
                    Else
                        StatementTrailingTrivia = StatementTrailingTrivia.Add(t)
                    End If
                End If
            Next
        End Sub

        Friend Function RelocateDirectiveDisabledTrivia(TriviaList As SyntaxTriviaList, ByRef StatementTrivia As SyntaxTriviaList, RemoveEOL As Boolean) As SyntaxTriviaList
            Dim NewTrivia As New SyntaxTriviaList
            Dim FoundDirective As Boolean = False
            For Each t As SyntaxTrivia In TriviaList
                If Not (FoundDirective OrElse t.IsDirective OrElse t.IsSkippedOrDisabledTrivia) Then
                    If t.IsEndOfLine AndAlso RemoveEOL Then
                        NewTrivia = NewTrivia.Add(VBSpaceTrivia)
                    Else
                        NewTrivia = NewTrivia.Add(t)
                    End If
                Else
                    If t.IsDirective Then
                        StatementTrivia = StatementTrivia.Add(VBEOLTrivia)
                        StatementTrivia = StatementTrivia.Add(t)
                        FoundDirective = True
                    Else
                        StatementTrivia = StatementTrivia.Add(t)
                    End If
                End If
            Next
            Return NewTrivia
        End Function

        Friend Function RelocateLeadingCommentTrivia(TriviaList As SyntaxTriviaList, ByRef StatementLeadingTrivia As SyntaxTriviaList) As SyntaxTriviaList
            Dim newLeadingTrivia As New SyntaxTriviaList
            Dim FoundComment As Boolean = False
            For Each t As SyntaxTrivia In TriviaList
                If Not (FoundComment OrElse t.IsComment) Then
                    If t.IsEndOfLine Then
                        newLeadingTrivia = newLeadingTrivia.Add(VBSpaceTrivia)
                    Else
                        newLeadingTrivia = newLeadingTrivia.Add(t)
                    End If
                Else
                    If t.IsComment Then
                        StatementLeadingTrivia = StatementLeadingTrivia.Add(VBEOLTrivia)
                        StatementLeadingTrivia = StatementLeadingTrivia.Add(t)
                        FoundComment = True
                    Else
                        StatementLeadingTrivia = StatementLeadingTrivia.Add(t)
                    End If
                End If
            Next
            Return newLeadingTrivia
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
                            newLeadingTriviaList = newLeadingTriviaList.Add(Factory.DocumentationCommentExteriorTrivia(token.LeadingTrivia(0).
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
                        newTokenList = newTokenList.Add(Factory.XmlTextLiteralToken(newLeadingTriviaList, tokenText, valueText, newTrailingTriviaList))
                    Case CS.SyntaxKind.XmlTextLiteralNewLineToken
                        newTokenList = newTokenList.Add(Factory.XmlTextNewLine(text:=vbCrLf, value:=vbCrLf, newLeadingTriviaList, newTrailingTriviaList))
                    Case CS.SyntaxKind.XmlEntityLiteralToken
                        newTokenList = newTokenList.Add(Factory.XmlEntityLiteralToken(newLeadingTriviaList, tokenText, valueText, newTrailingTriviaList))
                    Case Else
                        Stop
                End Select
            Next
            Return newTokenList
        End Function

    End Module

End Namespace
