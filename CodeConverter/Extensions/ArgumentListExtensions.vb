' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax

Namespace CSharpToVBConverter.ToVisualBasic

    Public Module ArgumentListExtensions

        <Extension>
        Friend Function ContainsConditionalDirective(argumentList As CSS.ArgumentListSyntax) As Boolean
            If argumentList.Arguments.Count = 0 Then
                Return False
            End If
            For Each e As IndexClass(Of CSS.ArgumentSyntax) In argumentList.Arguments.WithIndex
                For Each t As SyntaxTrivia In e.Value.GetLeadingTrivia
                    Select Case t.RawKind
                        Case CS.SyntaxKind.DisabledTextTrivia,
                             CS.SyntaxKind.ElifDirectiveTrivia,
                             CS.SyntaxKind.ElseDirectiveTrivia,
                             CS.SyntaxKind.EndIfDirectiveTrivia,
                             CS.SyntaxKind.IfDirectiveTrivia
                            Return True
                        Case CS.SyntaxKind.NullableDirectiveTrivia
                            Return False
                        Case CS.SyntaxKind.EndOfLineTrivia,
                             CS.SyntaxKind.SingleLineCommentTrivia,
                             CS.SyntaxKind.WhitespaceTrivia
                            ' ignore
                        Case Else
                            Debug.WriteLine($"Unknown TriviaKind {CS.CSharpExtensions.Kind(t)} in ContainsConditionalDirective")
                            Stop
                    End Select
                Next
            Next
            Return False
        End Function

    End Module
End Namespace
