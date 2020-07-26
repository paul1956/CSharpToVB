' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports CSharpToVBCodeConverter.ToVisualBasic
Imports Microsoft.CodeAnalysis
Imports VB = Microsoft.CodeAnalysis.VisualBasic

Module SyntaxTokenListExtensions

    <Extension>
    Private Function RestructureModifierLeadingTrivia(Modifier As SyntaxToken, i As Integer, ByRef LeadingTriviaNotHandled As Boolean, ByRef StatementLeadingTrivia As List(Of SyntaxTrivia), ByRef StatementTrailingTrivia As List(Of SyntaxTrivia)) As SyntaxTriviaList
        Dim NewModifierLeadingTrivia As New SyntaxTriviaList
        If LeadingTriviaNotHandled Then
            If i = 0 Then
                StatementLeadingTrivia.AddRange(Modifier.LeadingTrivia)
            Else
                NewModifierLeadingTrivia = RelocateDirectiveDisabledTrivia(Modifier.LeadingTrivia, StatementLeadingTrivia, RemoveEOL:=True)
            End If
        Else
            If i = 0 Then
                If Modifier.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    StatementTrailingTrivia.Add(VBEOLTrivia)
                End If
                StatementTrailingTrivia.AddRange(Modifier.LeadingTrivia)
            Else
                NewModifierLeadingTrivia = RelocateDirectiveDisabledTrivia(Modifier.LeadingTrivia, StatementTrailingTrivia, RemoveEOL:=True)
            End If

        End If
        LeadingTriviaNotHandled = StatementLeadingTrivia.Count = 0
        Return NewModifierLeadingTrivia
    End Function

    <Extension>
    Friend Function Contains(Tokens As SyntaxTokenList, Kind As CSharp.SyntaxKind) As Boolean
        Return Tokens.Contains(Function(m As SyntaxToken) m.IsKind(Kind))
    End Function

    <Extension>
    Friend Function Contains(Tokens As IEnumerable(Of SyntaxToken), ParamArray Kind() As VB.SyntaxKind) As Boolean
        Return Tokens.Contains(Function(m As SyntaxToken) m.IsKind(Kind))
    End Function

    <Extension>
    Friend Function IndexOf(Tokens As IEnumerable(Of SyntaxToken), Kind As VB.SyntaxKind) As Integer
        For i As Integer = 0 To Tokens.Count - 1
            If Tokens(i).IsKind(Kind) Then
                Return i
            End If
        Next
        Return -1
    End Function

    <Extension>
    Friend Function RestructureModifier(NodeModifier As SyntaxToken, i As Integer, ByRef AttributesNotFound As Boolean, ByRef StatementLeadingTrivia As List(Of SyntaxTrivia), ByRef StatementTrailingTrivia As List(Of SyntaxTrivia)) As SyntaxToken
        If (Not AttributesNotFound) AndAlso NodeModifier.RawKind = VB.SyntaxKind.EmptyToken AndAlso NodeModifier.HasLeadingTrivia AndAlso NodeModifier.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
            StatementTrailingTrivia.Add(VBEOLTrivia)
            StatementTrailingTrivia.AddRange(NodeModifier.LeadingTrivia)
            NodeModifier = NodeModifier.WithLeadingTrivia(SpaceTrivia)
        Else
            NodeModifier = NodeModifier.WithLeadingTrivia(NodeModifier.RestructureModifierLeadingTrivia(i, AttributesNotFound, StatementLeadingTrivia, StatementTrailingTrivia))
        End If
        If NodeModifier.TrailingTrivia.ContainsCommentOrDirectiveTrivia Then
            StatementLeadingTrivia.AddRange(RelocateDirectiveDisabledTrivia(NodeModifier.TrailingTrivia, StatementTrailingTrivia, RemoveEOL:=False))
            If StatementLeadingTrivia.Any AndAlso StatementLeadingTrivia.Last.RawKind <> VB.SyntaxKind.EndOfLineTrivia Then
                StatementLeadingTrivia.Add(VBEOLTrivia)
            End If
            Return NodeModifier.WithTrailingTrivia(SpaceTrivia)
        End If
        Return NodeModifier.WithTrailingTrivia(RelocateDirectiveDisabledTrivia(NodeModifier.TrailingTrivia, StatementTrailingTrivia, RemoveEOL:=True))
    End Function

    <Extension>
    Public Function [With](token As SyntaxToken, leading As IEnumerable(Of SyntaxTrivia), trailing As IEnumerable(Of SyntaxTrivia)) As SyntaxToken
        Return token.WithLeadingTrivia(leading).WithTrailingTrivia(trailing)
    End Function

End Module
