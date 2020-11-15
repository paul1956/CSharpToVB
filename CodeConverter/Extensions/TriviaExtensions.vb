' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports Microsoft.CodeAnalysis
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSFactory = Microsoft.CodeAnalysis.CSharp.SyntaxFactory
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic

Namespace CSharpToVBConverter
    Public Module TriviaExtensions

        <Extension>
        Friend Function AdjustWhitespace(trivia As SyntaxTrivia, nextTrivia As SyntaxTrivia, afterLineContinue As Boolean) As SyntaxTrivia
            If trivia.Span.Length = nextTrivia.Span.Length Then
                Return trivia
            End If
            Dim lineContinueOffset As Integer = If(afterLineContinue, 2, 0)
            If trivia.Span.Length > nextTrivia.Span.Length Then
                Return Factory.Whitespace(New String(" "c, Math.Max(trivia.FullWidth - lineContinueOffset, 1)))
            End If
            Return Factory.Whitespace(New String(" "c, Math.Max(nextTrivia.FullWidth - lineContinueOffset, 1)))
        End Function

        <Extension>
        Friend Function AppendWhitespace(FirstTrivia As SyntaxTrivia, Width As Integer) As SyntaxTrivia
            If FirstTrivia.Language = "C#" Then
                Return CSFactory.Whitespace(StrDup(Math.Max(FirstTrivia.FullWidth + Width, 1), " "c))
            Else
                Return Factory.Whitespace(StrDup(Math.Max(FirstTrivia.FullWidth + Width, 1), " "c))
            End If
        End Function

        <Extension>
        Friend Function DirectiveNotAllowedHere(trivia As SyntaxTrivia) As SyntaxTriviaList
            Dim newTriviaList As New SyntaxTriviaList
            Dim leadingTriviaList As New SyntaxTriviaList
            leadingTriviaList = leadingTriviaList.AddRange({Factory.Space,
                                                        LineContinuation,
                                                        Factory.Space})

            Dim triviaAsString As String = ""

            If trivia.IsKind(VB.SyntaxKind.DisabledTextTrivia) Then
                newTriviaList = newTriviaList.AddRange(leadingTriviaList)
                newTriviaList = newTriviaList.Add(Factory.CommentTrivia($" ' TODO VB does not allow Disabled Text here, original text:"))
                newTriviaList = newTriviaList.Add(VBEOLTrivia)
                For Each triviaAsString In trivia.ToFullString.SplitLines()
                    newTriviaList = newTriviaList.AddRange(leadingTriviaList)
                    newTriviaList = newTriviaList.Add(Factory.CommentTrivia($" ' {triviaAsString}".Replace("  ", " ", StringComparison.Ordinal).TrimEnd))
                    newTriviaList = newTriviaList.Add(VBEOLTrivia)
                Next
                If newTriviaList.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                    newTriviaList = newTriviaList.RemoveAt(newTriviaList.Count - 1)
                End If
                Return newTriviaList
            End If

            Select Case trivia.RawKind
                Case VB.SyntaxKind.IfDirectiveTrivia
                    triviaAsString = $"#If {trivia.ToFullString.Substring("#if".Length).Trim.WithoutNewLines(" "c)}"
                Case VB.SyntaxKind.ElseDirectiveTrivia
                    triviaAsString = $"#Else {trivia.ToFullString.Substring("#Else".Length).Trim.WithoutNewLines(" "c)}"
                Case VB.SyntaxKind.ElseIfDirectiveTrivia
                    triviaAsString = $"#ElseIf {trivia.ToFullString.Substring("#Else If".Length).Trim.WithoutNewLines(" "c)}"
                Case VB.SyntaxKind.EndIfDirectiveTrivia
                    triviaAsString = $"#EndIf {trivia.ToFullString.Substring("#End if".Length).Trim.WithoutNewLines(" "c)}"
                Case VB.SyntaxKind.DisableWarningDirectiveTrivia
                    triviaAsString = $"#Disable Warning Directive {trivia.ToFullString.Substring("#Disable Warning".Length).Trim.WithoutNewLines(" "c)}"
                Case VB.SyntaxKind.EnableWarningDirectiveTrivia
                    triviaAsString = $"#Enable Warning Directive {trivia.ToFullString.Substring("#Enable Warning".Length).Trim.WithoutNewLines(" "c)}"
                Case Else
                    Stop
            End Select
            Const msg As String = " ' TODO VB does not allow directives here, original directive: "
            newTriviaList = New SyntaxTriviaList
            newTriviaList = newTriviaList.AddRange({
            Factory.Space,
            LineContinuation,
            Factory.Space,
            Factory.CommentTrivia($"{msg}{triviaAsString}".Replace("  ", " ", StringComparison.Ordinal).TrimEnd)
            })
            Return newTriviaList
        End Function

        <Extension>
        Friend Function FullWidth(trivia As SyntaxTrivia) As Integer
            If trivia.Language = "C#" AndAlso trivia.IsKind(CS.SyntaxKind.WhitespaceTrivia) Then
                Return 0
            End If
            Return trivia.Span.Length
        End Function

        <Extension>
        Friend Function IsComment(trivia As SyntaxTrivia) As Boolean
            Return trivia.IsSingleLineComment OrElse
            trivia.IsMultiLineComment OrElse
            trivia.IsDocComment
        End Function

        <Extension>
        Friend Function IsCommentOrDirectiveTrivia(t As SyntaxTrivia) As Boolean
            Return t.IsComment OrElse t.IsDirective
        End Function

        <Extension>
        Friend Function IsDocComment(trivia As SyntaxTrivia) As Boolean
            Return trivia.IsKind(CS.SyntaxKind.DocumentationCommentExteriorTrivia) OrElse
               trivia.IsKind(CS.SyntaxKind.MultiLineDocumentationCommentTrivia) OrElse
               trivia.IsKind(CS.SyntaxKind.SingleLineDocumentationCommentTrivia) OrElse
               trivia.IsKind(VB.SyntaxKind.DocumentationCommentExteriorTrivia) OrElse
               trivia.IsKind(VB.SyntaxKind.DocumentationCommentTrivia)
        End Function

        <Extension>
        Friend Function IsEndOfLine(trivia As SyntaxTrivia) As Boolean
            Return trivia.IsKind(CS.SyntaxKind.EndOfLineTrivia) OrElse
            trivia.IsKind(VB.SyntaxKind.EndOfLineTrivia)
        End Function

        <Extension>
        Friend Function IsKind(trivia As SyntaxTrivia, ParamArray kinds() As VB.SyntaxKind) As Boolean
            Return kinds.Contains(CType(trivia.RawKind, VB.SyntaxKind))
        End Function

        <Extension>
        Friend Function IsMultiLineComment(trivia As SyntaxTrivia) As Boolean
            Return trivia.IsKind(CS.SyntaxKind.MultiLineCommentTrivia) OrElse
            trivia.IsKind(CS.SyntaxKind.MultiLineDocumentationCommentTrivia)
        End Function

        <Extension>
        Friend Function IsNone(trivia As SyntaxTrivia) As Boolean
            Return trivia.RawKind = 0
        End Function

        <Extension>
        Friend Function IsRegularOrDocComment(trivia As SyntaxTrivia) As Boolean
            Return trivia.IsSingleLineComment OrElse
            trivia.IsMultiLineComment() OrElse
            trivia.IsDocComment()
        End Function

        <Extension>
        Friend Function IsSingleLineComment(trivia As SyntaxTrivia) As Boolean
            Return trivia.RawKind = CS.SyntaxKind.SingleLineCommentTrivia OrElse
            trivia.RawKind = CS.SyntaxKind.SingleLineDocumentationCommentTrivia OrElse
            trivia.RawKind = VB.SyntaxKind.CommentTrivia OrElse
            trivia.RawKind = VB.SyntaxKind.DocumentationCommentExteriorTrivia
        End Function

        <Extension>
        Friend Function IsSkippedOrDisabledTrivia(t As SyntaxTrivia) As Boolean
            If t.RawKind = VB.SyntaxKind.DisabledTextTrivia Then
                Return True
            End If
            If t.RawKind = CS.SyntaxKind.DisabledTextTrivia Then
                Return True
            End If
            If t.RawKind = VB.SyntaxKind.SkippedTokensTrivia Then
                Return True
            End If
            If t.RawKind = CS.SyntaxKind.SkippedTokensTrivia Then
                Return True
            End If

            Return False
        End Function

        <Extension>
        Friend Function IsWhitespace(trivia As SyntaxTrivia) As Boolean
            Return trivia.RawKind = CS.SyntaxKind.WhitespaceTrivia OrElse
            trivia.RawKind = VB.SyntaxKind.WhitespaceTrivia
        End Function

        <Extension>
        Friend Function IsWhitespaceOrEndOfLine(trivia As SyntaxTrivia) As Boolean
            Return trivia.IsWhitespace OrElse
             trivia.IsEndOfLine
        End Function

    End Module
End Namespace
