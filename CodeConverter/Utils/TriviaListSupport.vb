Option Explicit On
Option Infer Off
Option Strict On

Imports IVisualBasicCode.CodeConverter.Util
Imports IVisualBasicCode.CodeConverter.Visual_Basic.CSharpConverter

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports VB = Microsoft.CodeAnalysis.VisualBasic

Namespace IVisualBasicCode.CodeConverter.Visual_Basic

    Public Module TriviaListSupport

        Private Function ConvertDocumentCommentExternalTrivia(t As SyntaxTrivia) As SyntaxTrivia
            If t.IsKind(CS.SyntaxKind.DocumentationCommentExteriorTrivia) Then
                Return VB.SyntaxFactory.DocumentationCommentExteriorTrivia(t.ToString.Replace("///", "'''"))
            End If

            Throw UnreachableException()
        End Function

        Public Function CreateVBDocumentCommentFromCSharpComment(singleLineDocumentationComment As CS.Syntax.DocumentationCommentTriviaSyntax) As VB.Syntax.DocumentationCommentTriviaSyntax
            Dim walker As New XMLVisitor()
            walker.Visit(singleLineDocumentationComment)

            Dim xmlNodes As New List(Of VB.Syntax.XmlNodeSyntax)
            For Each node As CS.Syntax.XmlNodeSyntax In singleLineDocumentationComment.Content
                Try
                    Dim Item As VB.Syntax.XmlNodeSyntax = DirectCast(node.Accept(walker), VB.Syntax.XmlNodeSyntax)
                    xmlNodes.Add(Item)
                Catch ex As Exception
                    Stop
                    Throw
                End Try
            Next
            Return VB.SyntaxFactory.DocumentationComment(xmlNodes.ToArray)
        End Function

        ''' <summary>
        ''' Replace EOL with Space Trivia
        ''' </summary>
        ''' <param name="m"></param>
        ''' <returns>New Trailing Trivia for Token</returns>
        Public Function GetTrailingTriviaWithoutEOL(m As SyntaxTriviaList) As List(Of SyntaxTrivia)
            Dim NewTrailingTrivia As New List(Of SyntaxTrivia)
            For Each t As SyntaxTrivia In m
                If t.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                    NewTrailingTrivia.Add(SpaceTrivia)
                ElseIf t.IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                    NewTrailingTrivia.Add(t)
                Else
                    Stop
                    NewTrailingTrivia.Add(t)
                End If
            Next
            Return NewTrailingTrivia
        End Function

        Public Function ProcessVBToken(CurrentToken As SyntaxToken, StatementLeadingTrivia As List(Of SyntaxTrivia), StatementTrailingTrivia As List(Of SyntaxTrivia), Optional RemoveEOL As Boolean = False) As SyntaxToken
            Dim CurrentTokenLeadingTrivia As New List(Of SyntaxTrivia)
            Dim CurrentTokenTrailingTrivia As New List(Of SyntaxTrivia)

            For i As Integer = 0 To CurrentToken.LeadingTrivia.Count - 1
                Dim t As SyntaxTrivia = CurrentToken.LeadingTrivia(i)
                Select Case t.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        If i + 1 < CurrentToken.LeadingTrivia.Count Then
                            If CurrentToken.LeadingTrivia(i + 1).IsComment Then
                                StatementLeadingTrivia.Add(t)
                            Else
                                CurrentTokenLeadingTrivia.Add(SpaceTrivia)
                            End If
                        Else
                            CurrentTokenLeadingTrivia.Add(t)
                        End If
                    Case VB.SyntaxKind.EndOfLineTrivia
                        If RemoveEOL Then
                            Continue For
                        End If
                        If i > 0 Then
                            If CurrentToken.LeadingTrivia(i - 1).IsComment Then
                                StatementLeadingTrivia.Add(t)
                            Else
                                CurrentTokenLeadingTrivia.Add(t)
                            End If
                        Else
                            CurrentTokenLeadingTrivia.Add(t)
                        End If
                    Case VB.SyntaxKind.CommentTrivia
                        StatementLeadingTrivia.Add(t)
                    Case VB.SyntaxKind.LineContinuationTrivia
                        Stop
                    Case VB.SyntaxKind.DisabledTextTrivia, VB.SyntaxKind.ConstDirectiveTrivia, VB.SyntaxKind.IfDirectiveTrivia,
                         VB.SyntaxKind.ElseIfDirectiveTrivia, VB.SyntaxKind.ElseDirectiveTrivia, VB.SyntaxKind.EndIfDirectiveTrivia,
                        VB.SyntaxKind.RegionDirectiveTrivia, VB.SyntaxKind.EndRegionDirectiveTrivia,
                         VB.SyntaxKind.DisableWarningDirectiveTrivia, VB.SyntaxKind.EnableWarningDirectiveTrivia
                        StatementLeadingTrivia.Add(t)
                    Case Else
                        Debug.WriteLine($"Unexpected Kind ={t.RawKind.ToString} ")
                        Stop
                End Select
            Next
            For i As Integer = 0 To CurrentToken.TrailingTrivia.Count - 1
                Dim t As SyntaxTrivia = CurrentToken.TrailingTrivia(i)
                Select Case t.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        If i + 1 < CurrentToken.TrailingTrivia.Count Then
                            If CurrentToken.TrailingTrivia(i + 1).IsComment Then
                                StatementTrailingTrivia.Add(t)
                            Else
                                CurrentTokenTrailingTrivia.Add(SpaceTrivia)
                            End If
                        Else
                            CurrentTokenTrailingTrivia.Add(t)
                        End If
                    Case VB.SyntaxKind.EndOfLineTrivia
                        If RemoveEOL Then
                            Continue For
                        End If
                        If i > 0 Then
                            If CurrentToken.TrailingTrivia(i - 1).IsComment Then
                                StatementTrailingTrivia.Add(t)
                            Else
                                CurrentTokenTrailingTrivia.Add(t)
                            End If
                        Else
                            CurrentTokenTrailingTrivia.Add(t)
                        End If
                    Case VB.SyntaxKind.CommentTrivia
                        StatementTrailingTrivia.Add(t)
                    Case VB.SyntaxKind.SkippedTokensTrivia
                        CurrentToken = VB.SyntaxFactory.ParseToken($"[{CurrentToken.TrailingTrivia(0)}]")
                    Case VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.DisabledTextTrivia, VB.SyntaxKind.ElseDirectiveTrivia
                        StatementLeadingTrivia.Add(t)
                    Case VB.SyntaxKind.EndIfDirectiveTrivia
                        StatementTrailingTrivia.Add(t)
                    Case VB.SyntaxKind.LineContinuationTrivia
                        CurrentTokenTrailingTrivia.Add(t)
                    Case Else
                        Debug.WriteLine(t.RawKind.ToString)
                        Stop
                End Select
            Next
            Return CurrentToken.With(CurrentTokenLeadingTrivia, CurrentTokenTrailingTrivia)
        End Function

        Public Sub RelocateAttributeDirectiveDisabledTrivia(TriviaList As SyntaxTriviaList, FoundDirective As Boolean, IsTheory As Boolean, ByRef StatementLeadingTrivia As List(Of SyntaxTrivia), ByRef StatementTrailingTrivia As List(Of SyntaxTrivia))
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
                        StatementTrailingTrivia.Add(VB_EOLTrivia)
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

        Public Function RelocateDirectiveDisabledTrivia(TriviaList As SyntaxTriviaList, ByRef StatementTrivia As List(Of SyntaxTrivia), RemoveEOL As Boolean) As SyntaxTriviaList
            Dim NewTrivia As New SyntaxTriviaList
            Dim FoundDirective As Boolean = False
            For Each t As SyntaxTrivia In TriviaList
                If Not (FoundDirective OrElse t.IsDirective OrElse t.MatchesKind(VB.SyntaxKind.DisabledTextTrivia)) Then
                    If t.IsEndOfLine AndAlso RemoveEOL Then
                        NewTrivia = NewTrivia.Add(SpaceTrivia)
                    Else
                        NewTrivia = NewTrivia.Add(t)
                    End If
                Else
                    If t.IsDirective Then
                        StatementTrivia.Add(VB_EOLTrivia)
                        StatementTrivia.Add(t)
                        FoundDirective = True
                    Else
                        StatementTrivia.Add(t)
                    End If
                End If
            Next
            Return NewTrivia
        End Function

        Public Function RelocateLeadingCommentTrivia(TriviaList As SyntaxTriviaList, ByRef StatementLeadingTrivia As List(Of SyntaxTrivia)) As SyntaxTriviaList
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
                        StatementLeadingTrivia.Add(VB_EOLTrivia)
                        StatementLeadingTrivia.Add(t)
                        FoundComment = True
                    Else
                        StatementLeadingTrivia.Add(t)
                    End If
                End If
            Next
            Return NewLeadingTrivia
        End Function

        Public Function TranslateTokenList(ChildTokens As IEnumerable(Of SyntaxToken)) As SyntaxTokenList
            Dim NewTokenList As New SyntaxTokenList
            For Each token As SyntaxToken In ChildTokens
                Dim NewLeadingTriviaList As New SyntaxTriviaList
                Dim NewTrailingTriviaList As New SyntaxTriviaList
                If token.HasLeadingTrivia Then
                    For Each t As SyntaxTrivia In token.LeadingTrivia
                        Dim FullString As String = t.ToFullString
                        If FullString.Length > 3 AndAlso FullString.StartsWith("///") Then
                            Stop
                        End If
                        NewLeadingTriviaList = NewLeadingTriviaList.Add(ConvertDocumentCommentExternalTrivia(t))
                    Next
                End If
                If token.HasTrailingTrivia Then
                    Throw UnexpectedValue($"XMLToken '{token.ToFullString}' should not have trailing Trivia")
                End If
                Select Case token.RawKind
                    Case CS.SyntaxKind.XmlTextLiteralToken
                        NewTokenList = NewTokenList.Add(VB.SyntaxFactory.XmlTextLiteralToken(leadingTrivia:=NewLeadingTriviaList, text:=token.Text, value:=token.ValueText, trailingTrivia:=NewTrailingTriviaList))
                    Case CS.SyntaxKind.XmlTextLiteralNewLineToken
                        NewTokenList = NewTokenList.Add(VB.SyntaxFactory.XmlTextNewLine(text:=vbCrLf, value:=vbCrLf, leading:=NewLeadingTriviaList, trailing:=NewTrailingTriviaList))
                    Case CS.SyntaxKind.XmlEntityLiteralToken
                        NewTokenList = NewTokenList.Add(VB.SyntaxFactory.XmlEntityLiteralToken(leadingTrivia:=NewLeadingTriviaList, text:=token.Text, value:=token.Text.ToString, trailingTrivia:=NewTrailingTriviaList))
                    Case Else
                        Stop
                End Select
            Next
            Return NewTokenList
        End Function

        Public Function TriviaIsIdentical(LeadingTrivia As SyntaxTriviaList, NodeLeadingTrivia As List(Of SyntaxTrivia)) As Boolean
            If LeadingTrivia.Count <> NodeLeadingTrivia.Count Then Return False
            For i As Integer = 0 To LeadingTrivia.Count - 1
                If LeadingTrivia(i).ToFullString.Trim <> NodeLeadingTrivia(i).ToFullString.Trim Then
                    Return False
                End If
            Next
            Return True
        End Function

    End Module

End Namespace