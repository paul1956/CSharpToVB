' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports Microsoft.CodeAnalysis
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBConverter
    Module ArgumentSyntaxExtensions

        ''' <summary>
        ''' Remove directive trivia
        ''' </summary>
        ''' <param name="node"></param>
        ''' <returns></returns>
        <Extension>
        Friend Function RemoveDirectiveTrivia(node As VBS.ArgumentSyntax, ByRef FoundEOL As Boolean) As VBS.ArgumentSyntax
            Dim newLeadingTrivia As SyntaxTriviaList
            Dim NewTrailingTrivia As SyntaxTriviaList
            For Each trivia As SyntaxTrivia In node.GetLeadingTrivia
                Select Case trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia, VB.SyntaxKind.CommentTrivia
                        newLeadingTrivia = newLeadingTrivia.Add(trivia)
                        FoundEOL = False
                    Case VB.SyntaxKind.EndOfLineTrivia
                        If Not FoundEOL Then
                            newLeadingTrivia = newLeadingTrivia.Add(trivia)
                        End If
                        FoundEOL = True
                    Case VB.SyntaxKind.DisabledTextTrivia,
                         VB.SyntaxKind.IfDirectiveTrivia,
                         VB.SyntaxKind.ElseDirectiveTrivia,
                         VB.SyntaxKind.ElseIfDirectiveTrivia,
                         VB.SyntaxKind.EndIfDirectiveTrivia
                        ' skip
                    Case Else
                        Stop
                End Select
            Next
            FoundEOL = False
            For Each trivia As SyntaxTrivia In node.GetTrailingTrivia
                Select Case trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia, VB.SyntaxKind.CommentTrivia
                        NewTrailingTrivia = NewTrailingTrivia.Add(trivia)
                        FoundEOL = False
                    Case VB.SyntaxKind.EndOfLineTrivia
                        If Not FoundEOL Then
                            NewTrailingTrivia = NewTrailingTrivia.Add(trivia)
                            FoundEOL = True
                        End If
                    Case VB.SyntaxKind.DisableWarningDirectiveTrivia,
                         VB.SyntaxKind.IfDirectiveTrivia,
                         VB.SyntaxKind.ElseDirectiveTrivia,
                         VB.SyntaxKind.ElseIfDirectiveTrivia,
                         VB.SyntaxKind.EndIfDirectiveTrivia
                        ' skip
                    Case Else
                        Stop
                End Select
            Next

            Return node.With(newLeadingTrivia, NewTrailingTrivia)
        End Function

    End Module
End Namespace
