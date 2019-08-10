' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.Runtime.CompilerServices

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax

Namespace IVisualBasicCode.CodeConverter.Visual_Basic

    Public Module DirectiveSupport

        <Flags>
        Public Enum DirectiveState
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
        Public Function ContainsConditionalDirective(ArgumentList As CSS.ArgumentListSyntax) As DirectiveState
            Dim FoundDirectives As DirectiveState = DirectiveState.None
            If ArgumentList.Arguments.Count = 0 Then
                Return FoundDirectives
            End If
            For i As Integer = 0 To ArgumentList.Arguments.Count - 1
                For Each t As SyntaxTrivia In ArgumentList.Arguments(i).GetLeadingTrivia
                    Select Case t.RawKind
                        Case CS.SyntaxKind.IfDirectiveTrivia
                            FoundDirectives = FoundDirectives.SetFlags(DirectiveState.IfFound)
                        Case CS.SyntaxKind.DisabledTextTrivia
                            FoundDirectives = FoundDirectives.SetFlags(DirectiveState.DisabledText)
                        Case CS.SyntaxKind.ElseDirectiveTrivia
                            FoundDirectives = FoundDirectives.SetFlags(DirectiveState.ElseFound)
                        Case CS.SyntaxKind.ElifDirectiveTrivia
                            FoundDirectives = FoundDirectives.SetFlags(DirectiveState.ElIf)
                        Case CS.SyntaxKind.EndIfDirectiveTrivia
                            FoundDirectives = FoundDirectives.SetFlags(DirectiveState.EndIfFound)
                        Case CS.SyntaxKind.WhitespaceTrivia
                            ' ignore
                        Case CS.SyntaxKind.SingleLineCommentTrivia
                            ' ignore
                        Case CS.SyntaxKind.EndOfLineTrivia
                            ' Ignore
                        Case Else
                            Debug.WriteLine($"Unknown TriviaKind {CType(t.RawKind, CS.SyntaxKind).ToString}")
                            Stop
                    End Select
                Next
            Next
            Return FoundDirectives
        End Function

    End Module
End Namespace