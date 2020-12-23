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
        ''' Compare 2 trivia Lists and see if they both end with the same SyntaxKinds
        ''' </summary>
        ''' <param name="NodeLeadingTrivia"></param>
        ''' <param name="leadingTrivia"></param>
        ''' <returns>True if leadingTrivia ends with the same trivia kinds as NodeLeadingTrivia</returns>
        Friend Function EndsWithSimilarTrivia(NodeLeadingTrivia As SyntaxTriviaList, leadingTrivia As SyntaxTriviaList) As Boolean
            If NodeLeadingTrivia.Count > leadingTrivia.Count Then
                Return False
            End If
            If (Not leadingTrivia.ContainsCommentOrDirectiveTrivia) OrElse (Not NodeLeadingTrivia.ContainsCommentOrDirectiveTrivia) Then
                Return False
            End If

            For index As Integer = 1 To NodeLeadingTrivia.Count
                If leadingTrivia(leadingTrivia.Count - index).RawKind <> NodeLeadingTrivia(NodeLeadingTrivia.Count - index).RawKind Then
                    Return False
                End If
            Next
            Return True
        End Function

        Friend Sub RelocateAttributeDirectiveDisabledTrivia(TriviaList As SyntaxTriviaList, FoundDirective As Boolean, IsTheory As Boolean, ByRef statementLeadingTrivia As SyntaxTriviaList, ByRef statementTrailingTrivia As SyntaxTriviaList)
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
                    If statementTrailingTrivia.Count = 0 Then
                        statementTrailingTrivia.Add(VBEOLTrivia)
                    End If
                    statementTrailingTrivia.Add(t)
                Else
                    If t.IsDirective Then
                        statementLeadingTrivia = statementLeadingTrivia.Add(t)
                        FoundDirective = True
                    Else
                        statementTrailingTrivia = statementTrailingTrivia.Add(t)
                    End If
                End If
            Next
        End Sub

        Friend Function RelocateDirectiveDisabledTrivia(TriviaList As SyntaxTriviaList, ByRef StatementTrivia As SyntaxTriviaList, RemoveEOL As Boolean) As SyntaxTriviaList
            Dim newTrivia As New SyntaxTriviaList
            Dim foundDirective As Boolean = False
            For Each t As SyntaxTrivia In TriviaList
                If Not (foundDirective OrElse t.IsDirective OrElse t.IsSkippedOrDisabledTrivia) Then
                    If t.IsEndOfLine AndAlso RemoveEOL Then
                        newTrivia = newTrivia.Add(Factory.Space)
                    Else
                        newTrivia = newTrivia.Add(t)
                    End If
                Else
                    If t.IsDirective Then
                        StatementTrivia = StatementTrivia.Add(VBEOLTrivia)
                        StatementTrivia = StatementTrivia.Add(t)
                        foundDirective = True
                    Else
                        StatementTrivia = StatementTrivia.Add(t)
                    End If
                End If
            Next
            Return newTrivia
        End Function

        Friend Function RelocateLeadingCommentTrivia(TriviaList As SyntaxTriviaList, ByRef statementLeadingTrivia As SyntaxTriviaList) As SyntaxTriviaList
            Dim newLeadingTrivia As New SyntaxTriviaList
            Dim foundComment As Boolean = False
            For Each t As SyntaxTrivia In TriviaList
                If Not (foundComment OrElse t.IsComment) Then
                    If t.IsEndOfLine Then
                        newLeadingTrivia = newLeadingTrivia.Add(Factory.Space)
                    Else
                        newLeadingTrivia = newLeadingTrivia.Add(t)
                    End If
                Else
                    If t.IsComment Then
                        statementLeadingTrivia = statementLeadingTrivia.Add(VBEOLTrivia)
                        statementLeadingTrivia = statementLeadingTrivia.Add(t)
                        foundComment = True
                    Else
                        statementLeadingTrivia = statementLeadingTrivia.Add(t)
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
                    For Each t As SyntaxTrivia In token.leadingTrivia
                        If t.IsKind(CS.SyntaxKind.DocumentationCommentExteriorTrivia) Then
                            newLeadingTriviaList = newLeadingTriviaList.Add(Factory.DocumentationCommentExteriorTrivia(token.leadingTrivia(0).
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
