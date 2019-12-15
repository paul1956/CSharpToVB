' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

Imports CSharpToVBCodeConverter.Util

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBCodeConverter.Visual_Basic

    Partial Public Class CSharpConverter

        Partial Friend Class NodesVisitor
            Inherits CS.CSharpSyntaxVisitor(Of VB.VisualBasicSyntaxNode)

            Private Iterator Function ConvertQueryBody(body As CSS.QueryBodySyntax) As IEnumerable(Of VBS.QueryClauseSyntax)
                If TypeOf body.SelectOrGroup Is CSS.SelectClauseSyntax Then
                    Yield DirectCast(body.SelectOrGroup.Accept(Me), VBS.QueryClauseSyntax)
                Else
                    Dim group As CSS.GroupClauseSyntax = DirectCast(body.SelectOrGroup, CSS.GroupClauseSyntax)
                    Dim Items As VBS.ExpressionRangeVariableSyntax = VBFactory.ExpressionRangeVariable(DirectCast(group.GroupExpression.Accept(Me), VBS.ExpressionSyntax))
                    Dim Identifier As VBS.ModifiedIdentifierSyntax = VBFactory.ModifiedIdentifier(GeneratePlaceholder("groupByKey"))
                    Dim NameEquals As VBS.VariableNameEqualsSyntax = VBFactory.VariableNameEquals(Identifier)
                    Dim Expression As VBS.ExpressionSyntax = DirectCast(group.ByExpression.Accept(Me), VBS.ExpressionSyntax)
                    Dim Keys As VBS.ExpressionRangeVariableSyntax
                    Dim FunctionName As SyntaxToken
                    If body.Continuation Is Nothing Then
                        Keys = VBFactory.ExpressionRangeVariable(Expression)
                        FunctionName = VBFactory.Identifier("Group")
                    Else
                        Keys = VBFactory.ExpressionRangeVariable(NameEquals, Expression)
                        FunctionName = GenerateSafeVBToken(body.Continuation.Identifier, IsQualifiedName:=False, IsTypeName:=False)
                    End If
                    Dim Aggregation As VBS.FunctionAggregationSyntax = VBFactory.FunctionAggregation(FunctionName)
                    Dim AggrationRange As VBS.AggregationRangeVariableSyntax = VBFactory.AggregationRangeVariable(Aggregation)
                    Yield VBFactory.GroupByClause(VBFactory.SingletonSeparatedList(Items), VBFactory.SingletonSeparatedList(Keys), VBFactory.SingletonSeparatedList(AggrationRange))
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
                Dim identifier As VBS.ModifiedIdentifierSyntax = VBFactory.ModifiedIdentifier(GenerateSafeVBToken(node.Identifier, IsQualifiedName:=False, IsTypeName:=False))
                Dim CollectionRangevariable As VBS.CollectionRangeVariableSyntax = VBFactory.CollectionRangeVariable(identifier, expression)
                Return VBFactory.FromClause(CollectionRangevariable).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitJoinClause(node As CSS.JoinClauseSyntax) As VB.VisualBasicSyntaxNode
                If node.Into IsNot Nothing Then
                    Return VBFactory.GroupJoinClause(
                        VBFactory.SingletonSeparatedList(
                            VBFactory.CollectionRangeVariable(
                                                            VBFactory.ModifiedIdentifier(GenerateSafeVBToken(node.Identifier, IsQualifiedName:=False, IsTypeName:=False)
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
                                                                                            VBFactory.VariableNameEquals(VBFactory.ModifiedIdentifier(GenerateSafeVBToken(node.Into.Identifier, IsQualifiedName:=False, IsTypeName:=False))
                                                                                            ),
                        VBFactory.GroupAggregation()))).WithConvertedTriviaFrom(node)
                Else
                    Return VBFactory.SimpleJoinClause(
                        VBFactory.SingletonSeparatedList(VBFactory.CollectionRangeVariable(VBFactory.ModifiedIdentifier(GenerateSafeVBToken(node.Identifier, IsQualifiedName:=False, IsTypeName:=False)), If(node.Type Is Nothing, Nothing, VBFactory.SimpleAsClause(DirectCast(node.Type.Accept(Me), VBS.TypeSyntax))), DirectCast(node.InExpression.Accept(Me), VBS.ExpressionSyntax))), VBFactory.SingletonSeparatedList(VBFactory.JoinCondition(DirectCast(node.LeftExpression.Accept(Me), VBS.ExpressionSyntax), DirectCast(node.RightExpression.Accept(Me), VBS.ExpressionSyntax)))).WithConvertedTriviaFrom(node)
                End If
            End Function

            Public Overrides Function VisitLetClause(node As CSS.LetClauseSyntax) As VB.VisualBasicSyntaxNode
                Dim Identifier As SyntaxToken = GenerateSafeVBToken(node.Identifier, IsQualifiedName:=False, IsTypeName:=False)
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
                Dim Body As IEnumerable(Of VBS.QueryClauseSyntax) = ConvertQueryBody(node.Body)
                Return VBFactory.QueryExpression(VBFactory.SingletonList(FromClause).AddRange(BodyClauses).AddRange(Body)).WithConvertedTriviaFrom(node)
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
