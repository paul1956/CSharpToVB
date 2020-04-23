' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports CSharpToVBCodeConverter.Util

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBCodeConverter.DestVisualBasic

    Partial Public Class CSharpConverter

        Partial Friend Class NodesVisitor
            Inherits CS.CSharpSyntaxVisitor(Of VB.VisualBasicSyntaxNode)

            Private Iterator Function ConvertQueryBody(body As CSS.QueryBodySyntax) As IEnumerable(Of VBS.QueryClauseSyntax)
                If TypeOf body.SelectOrGroup Is CSS.SelectClauseSyntax Then
                    Yield DirectCast(body.SelectOrGroup.Accept(Me), VBS.QueryClauseSyntax)
                Else
                    Dim group As CSS.GroupClauseSyntax = DirectCast(body.SelectOrGroup, CSS.GroupClauseSyntax)
                    Dim items As VBS.ExpressionRangeVariableSyntax = VBFactory.ExpressionRangeVariable(DirectCast(group.GroupExpression.Accept(Me), VBS.ExpressionSyntax))
                    Dim nameEquals As VBS.VariableNameEqualsSyntax = VBFactory.VariableNameEquals(VBFactory.ModifiedIdentifier(GeneratePlaceholder("groupByKey")))
                    Dim expression As VBS.ExpressionSyntax = DirectCast(group.ByExpression.Accept(Me), VBS.ExpressionSyntax)
                    Dim keys As VBS.ExpressionRangeVariableSyntax
                    Dim functionName As SyntaxToken
                    If body.Continuation Is Nothing Then
                        keys = VBFactory.ExpressionRangeVariable(expression)
                        functionName = VBFactory.Identifier("Group")
                    Else
                        keys = VBFactory.ExpressionRangeVariable(nameEquals, expression)
                        functionName = GenerateSafeVBToken(body.Continuation.Identifier)
                    End If
                    Dim aggrationRange As VBS.AggregationRangeVariableSyntax = VBFactory.AggregationRangeVariable(VBFactory.FunctionAggregation(functionName))
                    Yield VBFactory.GroupByClause(VBFactory.SingletonSeparatedList(items), VBFactory.SingletonSeparatedList(keys), VBFactory.SingletonSeparatedList(aggrationRange))
                    If body.Continuation?.Body IsNot Nothing Then
                        For Each clause As VBS.QueryClauseSyntax In ConvertQueryBody(body.Continuation.Body)
                            Yield clause
                        Next
                    End If
                End If
            End Function

            Private Function GeneratePlaceholder(v As String) As String
                Return $"__{v}{Math.Min(Threading.Interlocked.Increment(_placeholder), _placeholder - 1)}__"
            End Function

            Public Overrides Function VisitFromClause(node As CSS.FromClauseSyntax) As VB.VisualBasicSyntaxNode
                Dim expression As VBS.ExpressionSyntax = DirectCast(node.Expression.Accept(Me), VBS.ExpressionSyntax).WithConvertedTriviaFrom(node.Expression)
                If expression Is Nothing Then
                    Return Nothing
                End If
                Dim identifier As VBS.ModifiedIdentifierSyntax = VBFactory.ModifiedIdentifier(GenerateSafeVBToken(node.Identifier))
                Dim collectionRangevariable As VBS.CollectionRangeVariableSyntax = VBFactory.CollectionRangeVariable(identifier, expression)
                Return VBFactory.FromClause(collectionRangevariable).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitJoinClause(node As CSS.JoinClauseSyntax) As VB.VisualBasicSyntaxNode
                If node.Into IsNot Nothing Then
                    Return VBFactory.GroupJoinClause(
                        VBFactory.SingletonSeparatedList(
                            VBFactory.CollectionRangeVariable(
                                                            VBFactory.ModifiedIdentifier(GenerateSafeVBToken(node.Identifier)
                                                            ), If(node.Type Is Nothing, Nothing, VBFactory.SimpleAsClause(DirectCast(node.Type.Accept(Me), VBS.TypeSyntax))),
                                                               DirectCast(node.InExpression.Accept(Me), VBS.ExpressionSyntax))),
                        VBFactory.SingletonSeparatedList(
                                        VBFactory.JoinCondition(
                                                                DirectCast(node.LeftExpression.Accept(Me), VBS.ExpressionSyntax),
                                                                DirectCast(node.RightExpression.Accept(Me), VBS.ExpressionSyntax)
                                                                )
                                                        ),
                        VBFactory.SingletonSeparatedList(
                                                        VBFactory.AggregationRangeVariable(
                                                                                            VBFactory.VariableNameEquals(VBFactory.ModifiedIdentifier(GenerateSafeVBToken(node.Into.Identifier))
                                                                                            ),
                        VBFactory.GroupAggregation()))).WithConvertedTriviaFrom(node)
                Else
                    Return VBFactory.SimpleJoinClause(
                        VBFactory.SingletonSeparatedList(VBFactory.CollectionRangeVariable(VBFactory.ModifiedIdentifier(GenerateSafeVBToken(node.Identifier)), If(node.Type Is Nothing, Nothing, VBFactory.SimpleAsClause(DirectCast(node.Type.Accept(Me), VBS.TypeSyntax))), DirectCast(node.InExpression.Accept(Me), VBS.ExpressionSyntax))), VBFactory.SingletonSeparatedList(VBFactory.JoinCondition(DirectCast(node.LeftExpression.Accept(Me), VBS.ExpressionSyntax), DirectCast(node.RightExpression.Accept(Me), VBS.ExpressionSyntax)))).WithConvertedTriviaFrom(node)
                End If
            End Function

            Public Overrides Function VisitLetClause(node As CSS.LetClauseSyntax) As VB.VisualBasicSyntaxNode
                Dim nameEquals As VBS.VariableNameEqualsSyntax = VBFactory.VariableNameEquals(VBFactory.ModifiedIdentifier(GenerateSafeVBToken(node.Identifier)))
                Dim expression As VBS.ExpressionSyntax = DirectCast(node.Expression.Accept(Me), VBS.ExpressionSyntax)
                Dim expressionRangeVariable As VBS.ExpressionRangeVariableSyntax = VBFactory.ExpressionRangeVariable(nameEquals, expression)
                Return VBFactory.LetClause(VBFactory.SingletonSeparatedList(expressionRangeVariable)).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitOrderByClause(node As CSS.OrderByClauseSyntax) As VB.VisualBasicSyntaxNode
                Return VBFactory.OrderByClause(VBFactory.SeparatedList(node.Orderings.Select(Function(o As CSS.OrderingSyntax) DirectCast(o.Accept(Me), VBS.OrderingSyntax)))).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitOrdering(node As CSS.OrderingSyntax) As VB.VisualBasicSyntaxNode
                If node.IsKind(CS.SyntaxKind.DescendingOrdering) Then
                    Return VBFactory.Ordering(VB.SyntaxKind.DescendingOrdering, DirectCast(node.Expression.Accept(Me), VBS.ExpressionSyntax)).WithConvertedTriviaFrom(node)
                Else
                    Return VBFactory.Ordering(VB.SyntaxKind.AscendingOrdering, DirectCast(node.Expression.Accept(Me), VBS.ExpressionSyntax)).WithConvertedTriviaFrom(node)
                End If
            End Function

            Public Overrides Function VisitQueryExpression(node As CSS.QueryExpressionSyntax) As VB.VisualBasicSyntaxNode
                ' From trivia handled in VisitFromClause
                Dim fromClause As VBS.QueryClauseSyntax = DirectCast(node.FromClause.Accept(Me), VBS.QueryClauseSyntax)
                Dim bodyClauses As IEnumerable(Of VBS.QueryClauseSyntax) = node.Body.Clauses.Select(Function(c As CSS.QueryClauseSyntax) DirectCast(c.Accept(Me).WithConvertedTriviaFrom(c), VBS.QueryClauseSyntax))
                Dim body As IEnumerable(Of VBS.QueryClauseSyntax) = ConvertQueryBody(node.Body)
                Return VBFactory.QueryExpression(VBFactory.SingletonList(fromClause).AddRange(bodyClauses).AddRange(body)).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitSelectClause(node As CSS.SelectClauseSyntax) As VB.VisualBasicSyntaxNode
                Dim expression As VBS.ExpressionSyntax = DirectCast(node.Expression.Accept(Me), VBS.ExpressionSyntax).NormalizeWhitespace
                Return VBFactory.SelectClause(VBFactory.ExpressionRangeVariable(expression)).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitWhereClause(node As CSS.WhereClauseSyntax) As VB.VisualBasicSyntaxNode
                Dim condition As VBS.ExpressionSyntax = DirectCast(node.Condition.Accept(Me), VBS.ExpressionSyntax)
                Return VBFactory.WhereClause(condition).WithConvertedTriviaFrom(node)
            End Function

        End Class

    End Class

End Namespace
