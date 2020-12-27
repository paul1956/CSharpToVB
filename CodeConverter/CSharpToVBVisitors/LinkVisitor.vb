' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBConverter.ToVisualBasic

    Partial Public Class CSharpConverter

        Partial Friend Class NodesVisitor
            Inherits CS.CSharpSyntaxVisitor(Of VB.VisualBasicSyntaxNode)

            Private Iterator Function ConvertQueryBody(Node As CS.CSharpSyntaxNode, body As CSS.QueryBodySyntax) As IEnumerable(Of VBS.QueryClauseSyntax)
                If TypeOf body.SelectOrGroup Is CSS.SelectClauseSyntax Then
                    Yield DirectCast(body.SelectOrGroup.Accept(Me), VBS.QueryClauseSyntax)
                Else
                    Dim group As CSS.GroupClauseSyntax = DirectCast(body.SelectOrGroup, CSS.GroupClauseSyntax)
                    Dim items As VBS.ExpressionRangeVariableSyntax = Factory.ExpressionRangeVariable(DirectCast(group.GroupExpression.Accept(Me), VBS.ExpressionSyntax))
                    Dim nameEquals As VBS.VariableNameEqualsSyntax = Factory.VariableNameEquals(Factory.ModifiedIdentifier(Me.GeneratePlaceholder("groupByKey")))
                    Dim expression As VBS.ExpressionSyntax = DirectCast(group.ByExpression.Accept(Me), VBS.ExpressionSyntax)
                    Dim keys As VBS.ExpressionRangeVariableSyntax
                    Dim functionNameToken As SyntaxToken
                    If body.Continuation Is Nothing Then
                        keys = Factory.ExpressionRangeVariable(expression)
                        functionNameToken = Factory.Identifier("Group")
                    Else
                        keys = Factory.ExpressionRangeVariable(nameEquals, expression)
                        functionNameToken = GenerateSafeVBToken(body.Continuation.Identifier, Node, _usedIdentifiers, _semanticModel)
                    End If
                    Dim aggrationRange As VBS.AggregationRangeVariableSyntax = Factory.AggregationRangeVariable(Factory.FunctionAggregation(functionNameToken))
                    Yield Factory.GroupByClause(Factory.SingletonSeparatedList(items), Factory.SingletonSeparatedList(keys), Factory.SingletonSeparatedList(aggrationRange))
                    If body.Continuation?.Body IsNot Nothing Then
                        For Each clause As VBS.QueryClauseSyntax In Me.ConvertQueryBody(Node, body.Continuation.Body)
                            Yield clause
                        Next
                    End If
                End If
            End Function

            Private Function GeneratePlaceholder(v As String) As String
                Return $"__{v}{Math.Min(Threading.Interlocked.Increment(_placeholder), _placeholder - 1)}__"
            End Function

            Public Overrides Function VisitFromClause(node As CSS.FromClauseSyntax) As VB.VisualBasicSyntaxNode
                Dim expression As VBS.ExpressionSyntax = DirectCast(node.Expression.Accept(Me), VBS.ExpressionSyntax)
                If expression Is Nothing Then
                    Return Nothing
                End If
                Dim identifier As VBS.ModifiedIdentifierSyntax = Factory.ModifiedIdentifier(GenerateSafeVBToken(node.Identifier, node, _usedIdentifiers, _semanticModel))
                Dim rangeVariableToken As VBS.CollectionRangeVariableSyntax = Factory.CollectionRangeVariable(identifier, expression)
                Return Factory.FromClause(rangeVariableToken).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitJoinClause(node As CSS.JoinClauseSyntax) As VB.VisualBasicSyntaxNode
                If node.Into IsNot Nothing Then
                    Return Factory.GroupJoinClause(
                        Factory.SingletonSeparatedList(
                            Factory.CollectionRangeVariable(
                                                            Factory.ModifiedIdentifier(GenerateSafeVBToken(node.Identifier, node, _usedIdentifiers, _semanticModel)
                                                            ), If(node.Type Is Nothing, Nothing, Factory.SimpleAsClause(DirectCast(node.Type.Accept(Me), VBS.TypeSyntax))),
                                                               DirectCast(node.InExpression.Accept(Me), VBS.ExpressionSyntax))),
                        Factory.SingletonSeparatedList(
                                        Factory.JoinCondition(
                                                                DirectCast(node.LeftExpression.Accept(Me), VBS.ExpressionSyntax),
                                                                DirectCast(node.RightExpression.Accept(Me), VBS.ExpressionSyntax)
                                                                )
                                                        ),
                        Factory.SingletonSeparatedList(
                                                        Factory.AggregationRangeVariable(
                                                                                            Factory.VariableNameEquals(Factory.ModifiedIdentifier(GenerateSafeVBToken(node.Into.Identifier, node, _usedIdentifiers, _semanticModel))
                                                                                            ),
                        Factory.GroupAggregation()))).WithConvertedTriviaFrom(node)
                Else
                    Return Factory.SimpleJoinClause(
                        Factory.SingletonSeparatedList(Factory.CollectionRangeVariable(Factory.ModifiedIdentifier(GenerateSafeVBToken(node.Identifier,
                            node,
                            _usedIdentifiers, _semanticModel)), If(node.Type Is Nothing, Nothing, Factory.SimpleAsClause(DirectCast(node.Type.Accept(Me), VBS.TypeSyntax))), DirectCast(node.InExpression.Accept(Me), VBS.ExpressionSyntax))), Factory.SingletonSeparatedList(Factory.JoinCondition(DirectCast(node.LeftExpression.Accept(Me), VBS.ExpressionSyntax), DirectCast(node.RightExpression.Accept(Me), VBS.ExpressionSyntax)))).WithConvertedTriviaFrom(node)
                End If
            End Function

            Public Overrides Function VisitLetClause(node As CSS.LetClauseSyntax) As VB.VisualBasicSyntaxNode
                Dim nameEquals As VBS.VariableNameEqualsSyntax = Factory.VariableNameEquals(Factory.ModifiedIdentifier(GenerateSafeVBToken(node.Identifier, node, _usedIdentifiers, _semanticModel)))
                Dim expression As VBS.ExpressionSyntax = DirectCast(node.Expression.Accept(Me), VBS.ExpressionSyntax)
                Dim expressionRangeVariable As VBS.ExpressionRangeVariableSyntax = Factory.ExpressionRangeVariable(nameEquals, expression)
                Return Factory.LetClause(Factory.SingletonSeparatedList(expressionRangeVariable)).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitOrderByClause(node As CSS.OrderByClauseSyntax) As VB.VisualBasicSyntaxNode
                Return Factory.OrderByClause(Factory.SeparatedList(node.Orderings.Select(Function(o As CSS.OrderingSyntax) DirectCast(o.Accept(Me), VBS.OrderingSyntax)))).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitOrdering(node As CSS.OrderingSyntax) As VB.VisualBasicSyntaxNode
                If node.IsKind(CS.SyntaxKind.DescendingOrdering) Then
                    Return Factory.Ordering(VB.SyntaxKind.DescendingOrdering, DirectCast(node.Expression.Accept(Me), VBS.ExpressionSyntax)).WithConvertedLeadingTriviaFrom(node)
                Else
                    Return Factory.Ordering(VB.SyntaxKind.AscendingOrdering, DirectCast(node.Expression.Accept(Me), VBS.ExpressionSyntax)).WithConvertedLeadingTriviaFrom(node)
                End If
            End Function

            Public Overrides Function VisitQueryExpression(node As CSS.QueryExpressionSyntax) As VB.VisualBasicSyntaxNode
                ' From trivia handled in VisitFromClause
                Dim fromClause As VBS.QueryClauseSyntax = DirectCast(node.FromClause.Accept(Me), VBS.QueryClauseSyntax)
                Dim bodyClauses As IEnumerable(Of VBS.QueryClauseSyntax) = node.Body.Clauses.Select(Function(c As CSS.QueryClauseSyntax) DirectCast(c.Accept(Me).WithConvertedTriviaFrom(c), VBS.QueryClauseSyntax))
                Dim body As IEnumerable(Of VBS.QueryClauseSyntax) = Me.ConvertQueryBody(node, node.Body)
                Return Factory.QueryExpression(Factory.SingletonList(fromClause).AddRange(bodyClauses).AddRange(body)).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitSelectClause(node As CSS.SelectClauseSyntax) As VB.VisualBasicSyntaxNode
                Dim expression As VBS.ExpressionSyntax = DirectCast(node.Expression.Accept(Me), VBS.ExpressionSyntax).NormalizeWhitespace
                Return Factory.SelectClause(Factory.ExpressionRangeVariable(expression)).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitWhereClause(node As CSS.WhereClauseSyntax) As VB.VisualBasicSyntaxNode
                Dim condition As VBS.ExpressionSyntax = DirectCast(node.Condition.Accept(Me), VBS.ExpressionSyntax)
                Return Factory.WhereClause(condition).WithConvertedTriviaFrom(node)
            End Function

        End Class

    End Class

End Namespace
