' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.Runtime.CompilerServices

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax

Namespace CSharpToVBCodeConverter.DestVisualBasic

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
            For Each e As IndexClass(Of CSS.ArgumentSyntax) In ArgumentList.Arguments.WithIndex
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
                            Debug.WriteLine($"Unknown TriviaKind {CType(t.RawKind, CS.SyntaxKind)} in ContainsConditionalDirective")
                            Stop
                    End Select
                Next
            Next
            Return False
        End Function

    End Module
End Namespace
