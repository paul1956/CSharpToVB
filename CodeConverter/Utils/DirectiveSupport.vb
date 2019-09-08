' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.Runtime.CompilerServices

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax

Namespace CSharpToVBCodeConverter.Visual_Basic

    Public Module DirectiveSupport

        <Flags>
        Public Enum DirectiveStates
            None = 0
            IfFound = 1
            ElseFound = 2
            ElIf = 4
            DisabledText = 8
            EndIfFound = 16
            Region = 32
            EndRegion = 64
            PragmaWarning = 128
        End Enum

        <Extension>
        Friend Function ContainsConditionalDirective(ArgumentList As CSS.ArgumentListSyntax) As Boolean
            If ArgumentList.Arguments.Count = 0 Then
                Return False
            End If
            For i As Integer = 0 To ArgumentList.Arguments.Count - 1
                For Each t As SyntaxTrivia In ArgumentList.Arguments(i).GetLeadingTrivia
                    Select Case t.RawKind
                        Case CS.SyntaxKind.IfDirectiveTrivia
                            Return True
                        Case CS.SyntaxKind.DisabledTextTrivia
                            Return True
                        Case CS.SyntaxKind.ElseDirectiveTrivia
                            Return True
                        Case CS.SyntaxKind.ElifDirectiveTrivia
                            Return True
                        Case CS.SyntaxKind.EndIfDirectiveTrivia
                            Return True
                        Case CS.SyntaxKind.NullableDirectiveTrivia
                            Return False
                        Case CS.SyntaxKind.WhitespaceTrivia
                            ' ignore
                        Case CS.SyntaxKind.SingleLineCommentTrivia
                            ' ignore
                        Case CS.SyntaxKind.EndOfLineTrivia
                            ' Ignore
                        Case Else
                            Debug.WriteLine($"Unknown TriviaKind {CType(t.RawKind, CS.SyntaxKind).ToString} in ContainsConditionalDirective")
                            Stop
                    End Select
                Next
            Next
            Return False
        End Function

    End Module
End Namespace
