Imports System.Runtime.CompilerServices

Imports IVisualBasicCode.CodeConverter.Util

Imports Microsoft.CodeAnalysis

Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory

Namespace IVisualBasicCode.CodeConverter.Visual_Basic

    Public Module ArgumentSupport

        <Extension>
        Public Function RestructureArguments(VB_Node As VBS.StatementSyntax, CS_ArgumentList As CSS.ArgumentListSyntax) As VBS.StatementSyntax
            If CS_ArgumentList.ContainsConditionalDirective = DirectiveState.None Then
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
                For i As Integer = 0 To CS_ArgumentList.Arguments.Count - 1
                    Dim ArgumentLeadingTrivia As SyntaxTriviaList = VBFactory.TriviaList(ConvertTrivia(CS_ArgumentList.Arguments(i).GetLeadingTrivia))
                    Dim NewArgumentLeadingTrivia As SyntaxTriviaList = RelocateDirectiveDisabledTrivia(ArgumentLeadingTrivia, StatementLeadingTrivia, RemoveEOL:=True)

                    Dim ArgumentTrailingTrivia As SyntaxTriviaList = VBFactory.TriviaList(ConvertTrivia(CS_ArgumentList.Arguments(i).GetTrailingTrivia))
                    Dim NewArgumentTrailingTrivia As SyntaxTriviaList = RelocateDirectiveDisabledTrivia(ArgumentTrailingTrivia, StatementTrailingTrivia, RemoveEOL:=False)

                    Dim m As VBS.ArgumentSyntax = Expr.ArgumentList.Arguments(i)
                    ExpressionStatement = ExpressionStatement.ReplaceNode(m,
                                                    m.With(NewArgumentLeadingTrivia, NewArgumentTrailingTrivia))
                Next
                StatementTrailingTrivia.AddRange(ConvertTrivia(CS_ArgumentList.CloseParenToken.LeadingTrivia))
                StatementTrailingTrivia.AddRange(ConvertTrivia(CS_ArgumentList.CloseParenToken.TrailingTrivia))
                If StatementTrailingTrivia.Count > 0 AndAlso StatementTrailingTrivia(0).IsDirective Then
                    StatementTrailingTrivia.Insert(0, VB_EOLTrivia)
                End If
                Return ExpressionStatement.WithPrependedLeadingTrivia(StatementLeadingTrivia).
                                            WithMergedTrailingTrivia(StatementTrailingTrivia)
            Else
                Stop
                Return VB_Node
            End If
        End Function

    End Module

End Namespace