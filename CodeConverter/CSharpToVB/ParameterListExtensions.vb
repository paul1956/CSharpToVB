' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports Microsoft.CodeAnalysis

Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBCodeConverter.ToVisualBasic
    Module ParameterListExtensions

        <Extension>
        Friend Function RelocateDirectivesInTrailingTrivia(ParameterList As VBS.ParameterListSyntax, StatementTrailingTrivia As List(Of SyntaxTrivia)) As VBS.ParameterListSyntax
            If ParameterList IsNot Nothing AndAlso ParameterList.HasTrailingTrivia AndAlso ParameterList.GetTrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                Dim ParameterListTrailingTrivia As New List(Of SyntaxTrivia)
                Dim TrailingTrivialist As SyntaxTriviaList = ParameterList.GetTrailingTrivia
                Dim FoundEndIf As Boolean = False
                For Each e As IndexClass(Of SyntaxTrivia) In TrailingTrivialist.WithIndex
                    Dim NextTrivia As SyntaxTrivia = If(Not e.IsLast, TrailingTrivialist(e.Index + 1), Nothing)
                    Select Case e.Value.RawKind
                        Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                            ParameterListTrailingTrivia.Add(e.Value)
                        Case VB.SyntaxKind.EndOfLineTrivia
                            If FoundEndIf Then
                                StatementTrailingTrivia.Add(e.Value)
                            Else
                                ParameterListTrailingTrivia.Add(e.Value)
                            End If
                        Case VB.SyntaxKind.WhitespaceTrivia
                            ParameterListTrailingTrivia.Add(e.Value)
                        Case VB.SyntaxKind.EndIfDirectiveTrivia
                            FoundEndIf = True
                            If Not StatementTrailingTrivia.Any Then
                                StatementTrailingTrivia.Add(VBEOLTrivia)
                            End If
                            StatementTrailingTrivia.Add(e.Value)
                        Case Else
                            Stop
                    End Select
                Next
                ParameterList = ParameterList.WithTrailingTrivia(ParameterListTrailingTrivia)
            End If

            Return ParameterList
        End Function

    End Module
End Namespace
