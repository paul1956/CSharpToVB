Option Explicit On
Option Infer Off
Option Strict On

Imports IVisualBasicCode.CodeConverter.Util
Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory

Namespace IVisualBasicCode.CodeConverter.Visual_Basic

    Partial Public Class CSharpConverter

        Partial Protected Friend Class NodesVisitor
            Inherits CS.CSharpSyntaxVisitor(Of VB.VisualBasicSyntaxNode)
            Private Iterator Function ConvertQueryBody(body As CSS.QueryBodySyntax) As IEnumerable(Of VBS.QueryClauseSyntax)
                'If TypeOf body.SelectOrGroup Is CSS.GroupClauseSyntax AndAlso body.Continuation Is Nothing Then
                '    Throw New NotSupportedException("group by clause without into Not supported In VB")
                'End If
                If TypeOf body.SelectOrGroup Is CSS.SelectClauseSyntax Then
                    Yield DirectCast(body.SelectOrGroup.Accept(Me), VBS.QueryClauseSyntax)
                Else
                    Dim group As CSS.GroupClauseSyntax = DirectCast(body.SelectOrGroup, CSS.GroupClauseSyntax)
                    Dim Items As VBS.ExpressionRangeVariableSyntax = VBFactory.ExpressionRangeVariable(DirectCast(group.GroupExpression.Accept(Me), VBS.ExpressionSyntax))
                    Dim Identifier As VBS.ModifiedIdentifierSyntax = VBFactory.ModifiedIdentifier(Me.GeneratePlaceholder("groupByKey"))
                    Dim NameEquals As VBS.VariableNameEqualsSyntax = VBFactory.VariableNameEquals(Identifier)
                    Dim Expression As VBS.ExpressionSyntax = DirectCast(group.ByExpression.Accept(Me), VBS.ExpressionSyntax)
                    Dim Keys As VBS.ExpressionRangeVariableSyntax
                    Dim FunctionName As SyntaxToken
                    If body.Continuation Is Nothing Then
                        Keys = VBFactory.ExpressionRangeVariable(Expression)
                        FunctionName = VBFactory.Identifier("Group")
                    Else
                        Keys = VBFactory.ExpressionRangeVariable(NameEquals, Expression)
                        FunctionName = GenerateSafeVBToken(body.Continuation.Identifier, False)
                    End If
                    Dim Aggregation As VBS.FunctionAggregationSyntax = VBFactory.FunctionAggregation(FunctionName)
                    Dim AggrationRange As VBS.AggregationRangeVariableSyntax = VBFactory.AggregationRangeVariable(Aggregation)
                    Yield VBFactory.GroupByClause(VBFactory.SingletonSeparatedList(Items), VBFactory.SingletonSeparatedList(Keys), VBFactory.SingletonSeparatedList(AggrationRange))
                    If body.Continuation?.Body IsNot Nothing Then
                        For Each clause As VBS.QueryClauseSyntax In Me.ConvertQueryBody(body.Continuation.Body)
                            Yield clause
                        Next
                    End If
                End If
            End Function
            Private Function GeneratePlaceholder(v As String) As String
                Return $"__{v}{Math.Min(Threading.Interlocked.Increment(Me.placeholder), Me.placeholder - 1)}__"
            End Function
            Public Overrides Function VisitFromClause(node As CSS.FromClauseSyntax) As VB.VisualBasicSyntaxNode
                Dim expression As VBS.ExpressionSyntax = DirectCast(node.Expression.Accept(Me), VBS.ExpressionSyntax).WithConvertedTriviaFrom(node.Expression)
                If expression Is Nothing Then
                    Return Nothing
                End If
                Dim identifier As VBS.ModifiedIdentifierSyntax = VBFactory.ModifiedIdentifier(GenerateSafeVBToken(node.Identifier, False))
                Dim CollectionRangevariable As VBS.CollectionRangeVariableSyntax = VBFactory.CollectionRangeVariable(identifier, expression)
                Return VBFactory.FromClause(CollectionRangevariable).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitJoinClause(node As CSS.JoinClauseSyntax) As VB.VisualBasicSyntaxNode
                If node.Into IsNot Nothing Then
                    Return VBFactory.GroupJoinClause(
                        VBFactory.SingletonSeparatedList(
                            VBFactory.CollectionRangeVariable(
                                                            VBFactory.ModifiedIdentifier(GenerateSafeVBToken(node.Identifier, False)
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
                                                                                            VBFactory.VariableNameEquals(VBFactory.ModifiedIdentifier(GenerateSafeVBToken(node.Into.Identifier, False))
                                                                                            ),
                        VBFactory.GroupAggregation()))).WithConvertedTriviaFrom(node)
                Else
                    Return VBFactory.SimpleJoinClause(
                        VBFactory.SingletonSeparatedList(VBFactory.CollectionRangeVariable(VBFactory.ModifiedIdentifier(GenerateSafeVBToken(node.Identifier, False)), If(node.Type Is Nothing, Nothing, VBFactory.SimpleAsClause(DirectCast(node.Type.Accept(Me), VBS.TypeSyntax))), DirectCast(node.InExpression.Accept(Me), VBS.ExpressionSyntax))), VBFactory.SingletonSeparatedList(VBFactory.JoinCondition(DirectCast(node.LeftExpression.Accept(Me), VBS.ExpressionSyntax), DirectCast(node.RightExpression.Accept(Me), VBS.ExpressionSyntax)))).WithConvertedTriviaFrom(node)
                End If
            End Function

            Public Overrides Function VisitLetClause(node As CSS.LetClauseSyntax) As VB.VisualBasicSyntaxNode
                Dim Identifier As SyntaxToken = GenerateSafeVBToken(node.Identifier, False)
                Dim ModifiedIdentifier As VBS.ModifiedIdentifierSyntax = VBFactory.ModifiedIdentifier(Identifier)
                Dim NameEquals As VBS.VariableNameEqualsSyntax = VBFactory.VariableNameEquals(ModifiedIdentifier)
                Dim Expression As VBS.ExpressionSyntax = DirectCast(node.Expression.Accept(Me), VBS.ExpressionSyntax)
                Dim ExpressionRangeVariable As VBS.ExpressionRangeVariableSyntax = VBFactory.ExpressionRangeVariable(NameEquals, Expression)
                Return VBFactory.LetClause(VBFactory.SingletonSeparatedList(ExpressionRangeVariable)).WithConvertedTriviaFrom(node)
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
                Dim FromClause As VBS.QueryClauseSyntax = DirectCast(node.FromClause.Accept(Me), VBS.QueryClauseSyntax)
                Dim BodyClauses As IEnumerable(Of VBS.QueryClauseSyntax) = node.Body.Clauses.Select(Function(c As CSS.QueryClauseSyntax) DirectCast(c.Accept(Me).WithConvertedTriviaFrom(c), VBS.QueryClauseSyntax))
                Dim Body As IEnumerable(Of VBS.QueryClauseSyntax) = Me.ConvertQueryBody(node.Body)
                Return VBFactory.QueryExpression(VBFactory.SingletonList(FromClause).AddRange(BodyClauses).AddRange(Body)).WithConvertedTriviaFrom(node)
            End Function
            Public Overrides Function VisitSelectClause(node As CSS.SelectClauseSyntax) As VB.VisualBasicSyntaxNode
                Dim expression As VBS.ExpressionSyntax = DirectCast(node.Expression.Accept(Me), VBS.ExpressionSyntax)
                Return VBFactory.SelectClause(VBFactory.ExpressionRangeVariable(expression)).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitWhereClause(node As CSS.WhereClauseSyntax) As VB.VisualBasicSyntaxNode
                Dim condition As VBS.ExpressionSyntax = DirectCast(node.Condition.Accept(Me), VBS.ExpressionSyntax)
                Return VBFactory.WhereClause(condition).WithConvertedTriviaFrom(node)
            End Function
        End Class
    End Class
End Namespace