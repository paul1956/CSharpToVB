' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports Microsoft.CodeAnalysis
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBConverter
    Friend Module ArgumentSyntaxExtensions

        ''' <summary>
        ''' Remove directive trivia
        ''' </summary>
        ''' <param name="node"></param>
        ''' <returns></returns>
        <Extension>
        Friend Function ConvertDirectiveToComment(node As VBS.ArgumentSyntax, ByRef foundEOL As Boolean) As VBS.ArgumentSyntax
            Dim newLeadingTrivia As SyntaxTriviaList
            Dim newTrailingTrivia As SyntaxTriviaList
            For Each trivia As SyntaxTrivia In node.GetLeadingTrivia
                Select Case trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia, VB.SyntaxKind.CommentTrivia
                        newLeadingTrivia = newLeadingTrivia.Add(trivia)
                        foundEOL = False
                    Case VB.SyntaxKind.EndOfLineTrivia
                        If Not foundEOL Then
                            newLeadingTrivia = newLeadingTrivia.Add(trivia)
                        End If
                        foundEOL = True
                    Case VB.SyntaxKind.DisabledTextTrivia,
                         VB.SyntaxKind.IfDirectiveTrivia,
                         VB.SyntaxKind.ElseDirectiveTrivia,
                         VB.SyntaxKind.ElseIfDirectiveTrivia,
                         VB.SyntaxKind.EndIfDirectiveTrivia
                        newLeadingTrivia = newLeadingTrivia.Add(Factory.CommentTrivia($" ' TODO VB does not allow directives here, original statement {trivia.ToFullString.WithoutNewLines(" "c)}"))
                        foundEOL = False
                    Case Else
                        Stop
                End Select
            Next
            foundEOL = False
            For Each trivia As SyntaxTrivia In node.GetTrailingTrivia
                Select Case trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia, VB.SyntaxKind.CommentTrivia
                        newTrailingTrivia = newTrailingTrivia.Add(trivia)
                        foundEOL = False
                    Case VB.SyntaxKind.EndOfLineTrivia
                        If Not foundEOL Then
                            newTrailingTrivia = newTrailingTrivia.Add(trivia)
                            foundEOL = True
                        End If
                    Case VB.SyntaxKind.DisableWarningDirectiveTrivia,
                         VB.SyntaxKind.IfDirectiveTrivia,
                         VB.SyntaxKind.ElseDirectiveTrivia,
                         VB.SyntaxKind.ElseIfDirectiveTrivia,
                         VB.SyntaxKind.EndIfDirectiveTrivia
                        newTrailingTrivia = newTrailingTrivia.Add(Factory.CommentTrivia($" ' TODO VB does not allow directives here, original statement {trivia.ToFullString.WithoutNewLines(" "c)}"))
                        foundEOL = False
                    Case Else
                        Stop
                End Select
            Next

            Return node.With(newLeadingTrivia, newTrailingTrivia)
        End Function

    End Module
End Namespace
