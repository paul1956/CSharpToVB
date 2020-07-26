' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports CSharpToVBCodeConverter.ToVisualBasic
Imports Microsoft.CodeAnalysis
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Module ArgumentSyntaxExtensions

    ''' <summary>
    ''' Remove directive trivia
    ''' </summary>
    ''' <param name="node"></param>
    ''' <returns></returns>
    <Extension>
    Friend Function RemoveDirectiveTrivia(node As VBS.ArgumentSyntax, ByRef FoundEOL As Boolean) As VBS.ArgumentSyntax
        Dim NewLeadingTrivia As New List(Of SyntaxTrivia)
        Dim NewTrailingTrivia As New List(Of SyntaxTrivia)
        For Each trivia As SyntaxTrivia In node.GetLeadingTrivia
            Select Case trivia.RawKind
                Case VB.SyntaxKind.WhitespaceTrivia, VB.SyntaxKind.CommentTrivia
                    NewLeadingTrivia.Add(trivia)
                    FoundEOL = False
                Case VB.SyntaxKind.EndOfLineTrivia
                    If Not FoundEOL Then
                        NewLeadingTrivia.Add(trivia)
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
                    NewTrailingTrivia.Add(trivia)
                    FoundEOL = False
                Case VB.SyntaxKind.EndOfLineTrivia
                    If Not FoundEOL Then
                        NewTrailingTrivia.Add(trivia)
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

        Return node.With(NewLeadingTrivia, NewTrailingTrivia)
    End Function

    <Extension>
    Friend Function RestructureArguments(VB_Node As VBS.StatementSyntax, csArgumentList As CSS.ArgumentListSyntax) As VBS.StatementSyntax
        If Not csArgumentList.ContainsConditionalDirective Then
            Return VB_Node
        End If

        Dim StatementLeadingTrivia As New List(Of SyntaxTrivia)
        Dim StatementTrailingTrivia As New List(Of SyntaxTrivia)
        If TypeOf VB_Node Is VBS.ExpressionStatementSyntax Then
            Dim ExpressionStatement As VBS.ExpressionStatementSyntax = DirectCast(VB_Node, VBS.ExpressionStatementSyntax)
            Dim Expr As VBS.InvocationExpressionSyntax = DirectCast(ExpressionStatement.Expression, VBS.InvocationExpressionSyntax)
            If Expr.ArgumentList.Arguments.Count = 0 Then
                Return VB_Node
            End If
            For Each e As IndexClass(Of CSS.ArgumentSyntax) In csArgumentList.Arguments.WithIndex
                Dim newArgumentLeadingTrivia As SyntaxTriviaList = RelocateDirectiveDisabledTrivia(VBFactory.TriviaList(ConvertTrivia(e.Value.GetLeadingTrivia)), StatementLeadingTrivia, RemoveEOL:=True)
                Dim newArgumentTrailingTrivia As SyntaxTriviaList = RelocateDirectiveDisabledTrivia(VBFactory.TriviaList(ConvertTrivia(e.Value.GetTrailingTrivia)), StatementTrailingTrivia, RemoveEOL:=False)
                ExpressionStatement = ExpressionStatement.ReplaceNode(
                                            Expr.ArgumentList.Arguments(e.Index),
                                            Expr.ArgumentList.Arguments(e.Index).With(newArgumentLeadingTrivia, newArgumentTrailingTrivia)
                                            )
            Next
            StatementTrailingTrivia.AddRange(ConvertTrivia(csArgumentList.CloseParenToken.LeadingTrivia))
            StatementTrailingTrivia.AddRange(ConvertTrivia(csArgumentList.CloseParenToken.TrailingTrivia))
            If StatementTrailingTrivia.Any AndAlso StatementTrailingTrivia(0).IsDirective Then
                StatementTrailingTrivia.Insert(0, VBEOLTrivia)
            End If
            Return ExpressionStatement.WithPrependedLeadingTrivia(StatementLeadingTrivia).
                                            WithMergedTrailingTrivia(StatementTrailingTrivia)
        Else
            Stop
            Return VB_Node
        End If
    End Function

End Module
