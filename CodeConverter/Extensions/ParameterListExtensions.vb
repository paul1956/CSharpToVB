' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports Microsoft.CodeAnalysis

Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBConverter.ToVisualBasic
    Module ParameterListExtensions

        <Extension>
        Friend Function RelocateDirectivesInTrailingTrivia(ParameterList As VBS.ParameterListSyntax, statementTrailingTrivia As SyntaxTriviaList) As VBS.ParameterListSyntax
            If ParameterList IsNot Nothing AndAlso ParameterList.HasTrailingTrivia AndAlso ParameterList.GetTrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                Dim parameterListTrailingTrivia As New SyntaxTriviaList
                Dim initialTriviaList As SyntaxTriviaList = ParameterList.GetTrailingTrivia
                Dim FoundEndIf As Boolean = False
                For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                    Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(initialTriviaList, e.index, LookaheadCount:=1)
                    Select Case e.Value.RawKind
                        Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                            parameterListTrailingTrivia = parameterListTrailingTrivia.Add(e.Value)
                        Case VB.SyntaxKind.EndOfLineTrivia
                            If FoundEndIf Then
                                statementTrailingTrivia = statementTrailingTrivia.Add(e.Value)
                            Else
                                parameterListTrailingTrivia = parameterListTrailingTrivia.Add(e.Value)
                            End If
                        Case VB.SyntaxKind.WhitespaceTrivia
                            parameterListTrailingTrivia = parameterListTrailingTrivia.Add(e.Value)
                        Case VB.SyntaxKind.EndIfDirectiveTrivia
                            FoundEndIf = True
                            If Not statementTrailingTrivia.Any Then
                                statementTrailingTrivia = statementTrailingTrivia.Add(VBEOLTrivia)
                            End If
                            statementTrailingTrivia = statementTrailingTrivia.Add(e.Value)
                        Case Else
                            Stop
                    End Select
                Next
                ParameterList = ParameterList.WithTrailingTrivia(parameterListTrailingTrivia)
            End If

            Return ParameterList
        End Function

    End Module
End Namespace
