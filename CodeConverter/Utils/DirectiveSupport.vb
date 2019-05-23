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
        Public Function ContainsConditionalDirective(Tokens As SyntaxTokenList) As DirectiveState
            Dim FoundDirectives As DirectiveState = DirectiveState.None
            If Tokens.Count = 0 Then
                Return FoundDirectives
            End If
            For i As Integer = 0 To Tokens.Count - 1
                For Each t As SyntaxTrivia In Tokens(i).LeadingTrivia
                    Select Case t.RawKind
                        Case CS.SyntaxKind.IfDirectiveTrivia
                            FoundDirectives.ClearFlags
                            FoundDirectives = FoundDirectives.SetFlags(DirectiveState.IfFound)
                        Case CS.SyntaxKind.DisabledTextTrivia
                            FoundDirectives = FoundDirectives.SetFlags(DirectiveState.DisabledText)
                        Case CS.SyntaxKind.ElseDirectiveTrivia
                            FoundDirectives = FoundDirectives.SetFlags(DirectiveState.ElseFound)
                        Case CS.SyntaxKind.ElifDirectiveTrivia
                            FoundDirectives = FoundDirectives.SetFlags(DirectiveState.ElIf)
                        Case CS.SyntaxKind.EndIfDirectiveTrivia
                            FoundDirectives = FoundDirectives.SetFlags(DirectiveState.EndIfFound)
                        Case Else
                            Stop
                    End Select
                Next
            Next
            Return FoundDirectives
        End Function

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

        <Extension>
        Public Function GetConditionalDirectives(AttributeList As CSS.AttributeListSyntax) As DirectiveState
            Dim FoundDirectives As DirectiveState = DirectiveState.None
            If AttributeList.Attributes.Count = 0 Then
                Return FoundDirectives
            End If
            For i As Integer = 0 To AttributeList.Attributes.Count - 1
                For Each t As SyntaxTrivia In AttributeList.GetLeadingTrivia
                    FoundDirectives = ContainsDirectiveOrDisabledText(FoundDirectives, t)
                Next
                For Each t As SyntaxTrivia In AttributeList.GetTrailingTrivia
                    FoundDirectives = ContainsDirectiveOrDisabledText(FoundDirectives, t)
                Next
            Next
            Return FoundDirectives
        End Function

        Private Function ContainsDirectiveOrDisabledText(FoundDirectives As DirectiveState, t As SyntaxTrivia) As DirectiveState
            Select Case t.RawKind
                Case CS.SyntaxKind.EndOfLineTrivia, CS.SyntaxKind.WhitespaceTrivia
                    ' ignore
                Case CS.SyntaxKind.SingleLineDocumentationCommentTrivia, CS.SyntaxKind.SingleLineCommentTrivia
                    ' Ignore
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
                Case CS.SyntaxKind.RegionDirectiveTrivia
                    FoundDirectives = FoundDirectives.SetFlags(DirectiveState.Region)
                Case CS.SyntaxKind.EndRegionDirectiveTrivia
                    FoundDirectives = FoundDirectives.SetFlags(DirectiveState.EndRegion)
                Case CS.SyntaxKind.PragmaWarningDirectiveTrivia
                    FoundDirectives = FoundDirectives.SetFlags(DirectiveState.PragmaWarning)
                Case CS.SyntaxKind.None, CS.SyntaxKind.MultiLineCommentTrivia, CS.SyntaxKind.MultiLineDocumentationCommentTrivia,
                     CS.SyntaxKind.SingleLineCommentTrivia, CS.SyntaxKind.SingleLineDocumentationCommentTrivia
                    ' Ignore
                Case Else
                    Debug.WriteLine($"Unknown TriviaKind {t.RawKind.ToString}")
                    Stop
            End Select

            Return FoundDirectives
        End Function

    End Module
End Namespace