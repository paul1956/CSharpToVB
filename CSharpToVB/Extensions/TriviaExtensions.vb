' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports VB = Microsoft.CodeAnalysis.VisualBasic

Friend Module TriviaExtensions

    <Extension>
    Public Function IsComment(trivia As SyntaxTrivia) As Boolean
        Return trivia.IsSingleLineComment OrElse trivia.IsMultiLineComment
    End Function

    <Extension>
    Public Function IsDocComment(trivia As SyntaxTrivia) As Boolean
        Return trivia.IsSingleLineDocComment() OrElse trivia.IsMultiLineDocComment()
    End Function

    <Extension>
    Public Function IsMultiLineComment(trivia As SyntaxTrivia) As Boolean
        Return trivia.IsKind(CS.SyntaxKind.MultiLineCommentTrivia) OrElse
                trivia.IsKind(CS.SyntaxKind.DocumentationCommentExteriorTrivia) OrElse
                trivia.IsKind(CS.SyntaxKind.MultiLineDocumentationCommentTrivia)
    End Function

    <Extension>
    Public Function IsMultiLineDocComment(trivia As SyntaxTrivia) As Boolean
        Return trivia.IsKind(CS.SyntaxKind.MultiLineDocumentationCommentTrivia)
    End Function

    <Extension>
    Public Function IsRegularOrDocComment(trivia As SyntaxTrivia) As Boolean
        Return trivia.IsSingleLineComment() OrElse trivia.IsMultiLineComment() OrElse trivia.IsDocComment()
    End Function

    <Extension>
    Public Function IsSingleLineComment(trivia As SyntaxTrivia) As Boolean
        Return trivia.IsKind(CS.SyntaxKind.SingleLineCommentTrivia) OrElse
                trivia.IsKind(CS.SyntaxKind.SingleLineDocumentationCommentTrivia) OrElse
                trivia.IsKind(VB.SyntaxKind.CommentTrivia)
    End Function

    <Extension>
    Public Function IsSingleLineDocComment(trivia As SyntaxTrivia) As Boolean
        Return trivia.IsKind(CS.SyntaxKind.SingleLineDocumentationCommentTrivia)
    End Function

End Module
