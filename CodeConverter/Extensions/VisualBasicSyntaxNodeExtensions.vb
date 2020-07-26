' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices

Imports Microsoft.CodeAnalysis
Imports VB = Microsoft.CodeAnalysis.VisualBasic

Namespace CSharpToVBCodeConverter.ToVisualBasic

    Module VisualBasicSyntaxNodeExtensions

        ''' <summary>
        '''
        ''' </summary>
        ''' <param name="node"></param>
        ''' <returns>True if any Trivia is a Comment or a Directive</returns>
        <Extension>
        Friend Function ContainsCommentOrDirectiveTrivia(node As VB.VisualBasicSyntaxNode) As Boolean
            Dim CurrentToken As SyntaxToken = node.GetFirstToken
            While CurrentToken <> Nothing
                If CurrentToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia OrElse CurrentToken.TrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                    Return True
                End If
                CurrentToken = CurrentToken.GetNextToken
            End While

            Return False
        End Function

        <Extension>
        Friend Function ContainsEOLTrivia(node As VB.VisualBasicSyntaxNode) As Boolean
            If Not node.HasTrailingTrivia Then
                Return False
            End If
            Dim TriviaList As SyntaxTriviaList = node.GetTrailingTrivia
            For Each t As SyntaxTrivia In TriviaList
                If t.IsEndOfLine Then
                    Return True
                End If
            Next
            Return False
        End Function

    End Module
End Namespace
