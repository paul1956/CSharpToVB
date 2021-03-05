' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Diagnostics.CodeAnalysis
Imports System.Runtime.InteropServices
Imports System.Text
Imports Extensions
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax
Imports Utilities

Imports CS = Microsoft.CodeAnalysis.CSharp

Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory

Imports VB = Microsoft.CodeAnalysis.VisualBasic

Namespace CSharpToVBConverter.CSharpToVBVisitors

    Partial Public NotInheritable Class CSharpConverter

        <Guid("5AF92C9F-AE9B-4D2B-9BCE-6FD041654FC7")>
        Friend Class MethodBodyVisitor
            Inherits CS.CSharpSyntaxVisitor(Of SyntaxList(Of StatementSyntax))

            Private ReadOnly _blockInfo As Stack(Of BlockInfo) = New Stack(Of BlockInfo)()
            Private ReadOnly _nodesVisitor As NodesVisitor
            Private ReadOnly _semanticModel As SemanticModel

            ' currently only works with switch blocks
            Private _switchCount As Integer

            Friend Sub New(semanticModel As SemanticModel, nodesVisitor As NodesVisitor)
                _semanticModel = semanticModel
                _nodesVisitor = nodesVisitor
            End Sub

            Public Property IsIterator As Boolean

            Private Shared Function TrimParenthesis(node As CSS.IfStatementSyntax) As CSS.ExpressionSyntax
                Dim condition As CSS.ExpressionSyntax = node.Condition
                Do While TypeOf condition Is CSS.ParenthesizedExpressionSyntax
                    Dim pExp As CSS.ParenthesizedExpressionSyntax = CType(condition, CSS.ParenthesizedExpressionSyntax)
                    condition = pExp.Expression
                Loop
                Return condition
            End Function

            Private Shared Function TryGetBinaryExpression(node As CSS.IfStatementSyntax, ByRef binaryExpressionSyntax As CSS.BinaryExpressionSyntax, notEqualsExpression As CS.SyntaxKind, operand As CS.SyntaxKind) As Boolean
                binaryExpressionSyntax = TryCast(TrimParenthesis(node), CSS.BinaryExpressionSyntax)
                Return binaryExpressionSyntax IsNot Nothing AndAlso binaryExpressionSyntax.IsKind(notEqualsExpression) AndAlso (binaryExpressionSyntax.Left.IsKind(operand) OrElse binaryExpressionSyntax.Right.IsKind(operand))
            End Function

            Private Iterator Function AddLabels(blocks As CaseBlockSyntax(), gotoLabels As List(Of VB.VisualBasicSyntaxNode)) As IEnumerable(Of CaseBlockSyntax)
                For Each block As CaseBlockSyntax In blocks
                    Dim caseBlock As CaseBlockSyntax = block
                    For Each caseClause As CaseClauseSyntax In caseBlock.CaseStatement.Cases
                        Dim expression As VB.VisualBasicSyntaxNode = If(TypeOf caseClause Is ElseCaseClauseSyntax, DirectCast(caseClause, VB.VisualBasicSyntaxNode), DirectCast(caseClause, SimpleCaseClauseSyntax).Value)
                        If gotoLabels.Any(Function(label As VB.VisualBasicSyntaxNode) label.IsEquivalentTo(expression.WithoutTrivia)) Then
                            caseBlock = caseBlock.WithStatements(caseBlock.Statements.Insert(0, Factory.LabelStatement(Me.MakeGotoSwitchLabel(expression))))
                        End If
                    Next

                    Yield caseBlock
                Next
            End Function

            Private Sub CollectElseBlocks(node As CSS.IfStatementSyntax, elseIfBlocks As List(Of ElseIfBlockSyntax), ByRef elseBlock As ElseBlockSyntax, ByRef openBraceLeadingTrivia As SyntaxTriviaList, ByRef closeBraceTrailingTrivia As SyntaxTriviaList)
                If node.Else Is Nothing Then
                    Exit Sub
                End If
                Dim savedNeedEndUsingCount As Integer = _nodesVisitor.NeededEndUsingCount
                Try
                    _nodesVisitor.NeededEndUsingCount = 0
                    If TypeOf node.Else.Statement Is CSS.IfStatementSyntax Then
                        Dim [elseIf] As CSS.IfStatementSyntax = DirectCast(node.Else.Statement, CSS.IfStatementSyntax)
                        Dim elseIfKeywordWithTrivia As SyntaxToken = ElseIfKeyword.WithLeadingTrivia(node.Else.Statement.GetLeadingTrivia.ConvertTriviaList()).WithPrependedLeadingTrivia(node.Else.GetLeadingTrivia.ConvertTriviaList())
                        Dim newThenTrailingTrivia As New SyntaxTriviaList
                        Dim condition As ExpressionSyntax = DirectCast([elseIf].Condition.Accept(_nodesVisitor), ExpressionSyntax)
                        newThenTrailingTrivia = newThenTrailingTrivia.AddRange(condition.GetTrailingTrivia)
                        If node.CloseParenToken.HasLeadingTrivia AndAlso node.CloseParenToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                            newThenTrailingTrivia = newThenTrailingTrivia.AddRange(node.CloseParenToken.LeadingTrivia.ConvertTriviaList())
                        End If
                        Dim thenKeywordWithTrivia As SyntaxToken = ThenKeyword.WithTrailingTrivia(newThenTrailingTrivia)
                        Dim elseIfStatement As ElseIfStatementSyntax = Factory.ElseIfStatement(elseIfKeywordWithTrivia, condition.WithTrailingTrivia(SpaceTrivia), thenKeywordWithTrivia)
                        Dim elseIfBlock As ElseIfBlockSyntax = Factory.ElseIfBlock(elseIfStatement.WithTrailingEol, Me.ConvertBlock([elseIf].Statement, openBraceLeadingTrivia, closeBraceTrailingTrivia))
                        elseIfBlocks.Add(elseIfBlock)
                        Me.CollectElseBlocks([elseIf], elseIfBlocks, elseBlock, openBraceLeadingTrivia, closeBraceTrailingTrivia)
                    Else
                        Dim statements As SyntaxList(Of StatementSyntax) = Me.ConvertBlock(node.Else.Statement, openBraceLeadingTrivia, closeBraceTrailingTrivia)
                        statements = _nodesVisitor.AdjustUsingIfNeeded(statements)

                        Dim newTrailingTrivia As New SyntaxTriviaList
                        If node.Else.Statement.GetBraces.Item1.HasTrailingTrivia Then
                            newTrailingTrivia = newTrailingTrivia.AddRange(node.Else.Statement.GetBraces.Item1.TrailingTrivia.ConvertTriviaList())
                        End If
                        Dim elseStatement As ElseStatementSyntax = Factory.ElseStatement(ElseKeyword.WithConvertedLeadingTriviaFrom(node.Else.ElseKeyword)).WithTrailingTrivia(newTrailingTrivia)

                        If statements.Any AndAlso statements.LastOrDefault.GetTrailingTrivia.LastOrDefault.IsEndOfLine AndAlso openBraceLeadingTrivia.FirstOrDefault.IsEndOfLine Then
                            openBraceLeadingTrivia = openBraceLeadingTrivia.RemoveAt(0)
                        End If
                        elseBlock = Factory.ElseBlock(elseStatement, statements).WithPrependedLeadingTrivia(openBraceLeadingTrivia).WithAppendedTrailingTrivia(closeBraceTrailingTrivia)
                        openBraceLeadingTrivia = Nothing
                        closeBraceTrailingTrivia = Nothing
                    End If
                Finally
                    _nodesVisitor.NeededEndUsingCount = savedNeedEndUsingCount
                End Try
            End Sub

            Private Function ConvertBlock(node As CSS.StatementSyntax, ByRef openBraceLeadingTrivia As SyntaxTriviaList, ByRef closeBraceTrailingTrivia As SyntaxTriviaList) As SyntaxList(Of StatementSyntax)
                Dim csBraces As (openBrace As SyntaxToken, closeBrace As SyntaxToken)
                Dim csOpenBrace As SyntaxToken
                Dim csCloseBrace As SyntaxToken

                csBraces = node.GetBraces
                csOpenBrace = If(csBraces.openBrace = Nothing, New SyntaxToken, csBraces.openBrace)
                csCloseBrace = If(csBraces.closeBrace = Nothing, New SyntaxToken, csBraces.closeBrace)

                If csOpenBrace.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    openBraceLeadingTrivia = csOpenBrace.LeadingTrivia.ConvertTriviaList()
                End If
                Dim openBraceTrailingTrivia As New SyntaxTriviaList
                If csOpenBrace.TrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                    openBraceTrailingTrivia = csOpenBrace.TrailingTrivia.ConvertTriviaList()
                End If

                Dim closeBraceLeadingTrivia As New SyntaxTriviaList
                If csCloseBrace.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    closeBraceLeadingTrivia = csCloseBrace.LeadingTrivia.ConvertTriviaList()
                End If
                If csCloseBrace.TrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                    closeBraceTrailingTrivia = csCloseBrace.TrailingTrivia.ConvertTriviaList()
                End If
                Dim savedNeedEndUsingCount As Integer = _nodesVisitor.NeededEndUsingCount
                Try
                    _nodesVisitor.NeededEndUsingCount = 0
                    Select Case True
                        Case TypeOf node Is CSS.BlockSyntax
                            Dim nodeBlock As CSS.BlockSyntax = DirectCast(node, CSS.BlockSyntax)
                            Dim vbBlock As New List(Of StatementSyntax)
                            For Each e As IndexClass(Of CSS.StatementSyntax) In nodeBlock.Statements.WithIndex
                                Dim statements As List(Of StatementSyntax) = e.Value.Accept(Me).ToList
                                If e.IsFirst AndAlso statements.Any Then
                                    statements(0) = statements(0).WithPrependedLeadingTrivia(openBraceTrailingTrivia)
                                End If
                                vbBlock.AddRange(statements)
                            Next

                            If vbBlock.Count = 0 Then
                                vbBlock.Add(Factory.EmptyStatement.WithConvertedTriviaFrom(nodeBlock))
                            Else
                                If Not (vbBlock.First.IsKind(VB.SyntaxKind.EmptyStatement) OrElse
                                        (vbBlock.First.IsKind(VB.SyntaxKind.TryBlock) AndAlso vbBlock.First.GetLeadingTrivia.Count = 1)
                                       ) Then
                                    vbBlock.Item(0) = vbBlock(0).WithLeadingTrivia(nodeBlock.Statements(0).GetLeadingTrivia.ConvertTriviaList()).WithPrependedLeadingTrivia(node.GetLeadingTrivia.ConvertTriviaList()).RemoveExtraLeadingEol
                                End If
                                If vbBlock.Item(vbBlock.Count - 1).IsKind(VB.SyntaxKind.EmptyStatement) Then
                                    vbBlock.Item(vbBlock.Count - 1) = vbBlock.Last.WithoutTrailingTrivia().WithTrailingEol
                                Else
                                    vbBlock.Item(vbBlock.Count - 1) = vbBlock.Last.WithTrailingTrivia(nodeBlock.Statements.Last.GetTrailingTrivia.ConvertTriviaList()).WithTrailingEol
                                End If
                                If closeBraceLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                    vbBlock.Add(Factory.EmptyStatement.WithLeadingTrivia(closeBraceLeadingTrivia))
                                End If
                            End If
                            Return _nodesVisitor.AdjustUsingIfNeeded(Factory.List(vbBlock))
                        Case TypeOf node Is CSS.EmptyStatementSyntax
                            Return Factory.List(Of StatementSyntax)()
                    End Select
                Catch ex As Exception
                    Throw
                Finally
                    _nodesVisitor.NeededEndUsingCount = savedNeedEndUsingCount
                End Try
                If TypeOf node IsNot CSS.LocalFunctionStatementSyntax Then
                    Return node.Accept(Me)
                End If
                Return Factory.SingletonList(Of StatementSyntax)(Factory.EmptyStatement)
            End Function

            Private Function ConvertCatchClause(index As Integer, catchClause As CSS.CatchClauseSyntax) As CatchBlockSyntax
                Dim openBraceLeadingTrivia As New SyntaxTriviaList
                Dim closingBraceTrailingTrivia As New SyntaxTriviaList
                Dim vbStatements As SyntaxList(Of StatementSyntax) = Me.ConvertBlock(catchClause.Block, openBraceLeadingTrivia, closingBraceTrailingTrivia)

                If catchClause.Declaration Is Nothing Then
                    Return Factory.CatchBlock(Factory.CatchStatement().WithTrailingTrivia(VbEolTrivia).WithAppendedTrailingTrivia(closingBraceTrailingTrivia), vbStatements)
                End If
                If openBraceLeadingTrivia.ContainsCommentOrDirectiveTrivia OrElse closingBraceTrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                    If vbStatements.Any Then
                        vbStatements = vbStatements.Replace(vbStatements(0), vbStatements(0).WithPrependedLeadingTrivia(openBraceLeadingTrivia))
                        Dim lastStatement As Integer = vbStatements.Count - 1
                        vbStatements = vbStatements.Replace(vbStatements(lastStatement), vbStatements(lastStatement).WithAppendedTrailingTrivia(closingBraceTrailingTrivia).WithTrailingEol)
                    Else
                        vbStatements.Add(Factory.EmptyStatement.With(openBraceLeadingTrivia, closingBraceTrailingTrivia).WithTrailingEol)
                    End If
                End If
                Dim type As TypeSyntax = DirectCast(catchClause.Declaration.Type.Accept(_nodesVisitor), TypeSyntax)
                Dim simpleTypeName As String
                simpleTypeName = If(TypeOf type Is QualifiedNameSyntax, DirectCast(type, QualifiedNameSyntax).Right.ToString(), type.ToString())
                Dim identifier As SyntaxToken = If(catchClause.Declaration.Identifier.IsKind(CS.SyntaxKind.None),
                                                        Factory.Identifier($"__unused{simpleTypeName}{index + 1}__"),
                                                        GenerateSafeVbToken(catchClause.Declaration.Identifier, catchClause, _semanticModel, _nodesVisitor._usedIdentifiers))
                Dim whenClause As CatchFilterClauseSyntax = If(catchClause.Filter Is Nothing, Nothing, Factory.CatchFilterClause(filter:=DirectCast(catchClause.Filter.FilterExpression.Accept(_nodesVisitor), ExpressionSyntax)))
                Dim catchStatement As CatchStatementSyntax = Factory.CatchStatement(
                                                                    Factory.IdentifierName(identifier),
                                                                    Factory.SimpleAsClause(type),
                                                                    whenClause).
                                                                    WithConvertedLeadingTriviaFrom(catchClause)
                If Not catchStatement.HasTrailingTrivia Then
                    catchStatement = catchStatement.WithTrailingTrivia(VbEolTrivia)
                ElseIf catchStatement.GetTrailingTrivia.Last <> VbEolTrivia Then
                    catchStatement = catchStatement.WithTrailingTrivia(VbEolTrivia)
                End If
                Return Factory.CatchBlock(catchStatement, vbStatements)
            End Function

            Private Function ConvertForToSimpleForNext(node As CSS.ForStatementSyntax, <Out> ByRef block As StatementSyntax, ByRef hasVariable As Boolean) As Boolean
                block = Nothing
                hasVariable = node.Declaration IsNot Nothing AndAlso node.Declaration.Variables.Count = 1
                If Not (hasVariable OrElse node.Initializers.Count = 1) Then
                    Return False
                End If
                If node.Incrementors.Count <> 1 Then
                    Return False
                End If
                Dim iterator As AssignmentStatementSyntax
                If node.Incrementors.Any Then
                    iterator = TryCast(node.Incrementors.First.Accept(_nodesVisitor), AssignmentStatementSyntax)
                    If iterator Is Nothing OrElse Not iterator.IsKind(VB.SyntaxKind.AddAssignmentStatement, VB.SyntaxKind.SubtractAssignmentStatement) Then
                        Return False
                    End If
                Else
                    Return False
                End If
                Dim iteratorIdentifier As IdentifierNameSyntax = TryCast(iterator.Left, IdentifierNameSyntax)
                If iteratorIdentifier Is Nothing Then
                    Return False
                End If
                Dim stepExpr As LiteralExpressionSyntax = TryCast(iterator.Right, LiteralExpressionSyntax)
                If stepExpr Is Nothing OrElse Not (TypeOf stepExpr.Token.Value Is Integer) Then Return False
                Dim [step] As Integer = CInt(stepExpr.Token.Value)
                If iterator.OperatorToken.IsKind(VB.SyntaxKind.MinusToken, VB.SyntaxKind.MinusEqualsToken) Then
                    [step] = -[step]
                End If
                Dim condition As CSS.BinaryExpressionSyntax = TryCast(node.Condition, CSS.BinaryExpressionSyntax)
                If condition Is Nothing OrElse Not (TypeOf condition.Left Is CSS.IdentifierNameSyntax) Then
                    Return False
                End If
                If DirectCast(condition.Left, CSS.IdentifierNameSyntax).Identifier.IsEquivalentTo(iteratorIdentifier.Identifier) Then
                    Return False
                End If
                Dim toValue As ExpressionSyntax
                If iterator.IsKind(VB.SyntaxKind.SubtractAssignmentStatement) Then
                    If condition.IsKind(CS.SyntaxKind.GreaterThanOrEqualExpression) OrElse condition.IsKind(CS.SyntaxKind.NotEqualsExpression) Then
                        toValue = DirectCast(condition.Right.Accept(_nodesVisitor), ExpressionSyntax)
                    ElseIf condition.IsKind(CS.SyntaxKind.GreaterThanExpression) Then
                        toValue = Factory.BinaryExpression(VB.SyntaxKind.AddExpression,
                                                                 DirectCast(condition.Right.Accept(_nodesVisitor), ExpressionSyntax),
                                                                 PlusToken,
                                                                 OneExpression)
                    Else
                        Return False
                    End If
                Else
                    If condition.IsKind(CS.SyntaxKind.LessThanOrEqualExpression) OrElse condition.IsKind(CS.SyntaxKind.NotEqualsExpression) Then
                        toValue = DirectCast(condition.Right.Accept(_nodesVisitor), ExpressionSyntax)
                    ElseIf condition.IsKind(CS.SyntaxKind.LessThanExpression) Then
                        toValue = Factory.BinaryExpression(VB.SyntaxKind.SubtractExpression,
                                                                 DirectCast(condition.Right.Accept(_nodesVisitor), ExpressionSyntax),
                                                                 MinusToken,
                                                                 OneExpression)
                    Else
                        Return False
                    End If
                End If

                Dim controlVariable As VB.VisualBasicSyntaxNode
                Dim fromValue As ExpressionSyntax
                If hasVariable Then
                    Dim v As CSS.VariableDeclaratorSyntax = node.Declaration.Variables(0)
                    fromValue = DirectCast(v.Initializer?.Value.Accept(_nodesVisitor), ExpressionSyntax)
                    If fromValue Is Nothing Then
                        Return False
                    End If
                    Dim forVariableToken As SyntaxToken = GenerateSafeVbToken(v.Identifier, node, _semanticModel, _nodesVisitor._usedIdentifiers)
                    Dim names As SeparatedSyntaxList(Of ModifiedIdentifierSyntax) =
                        Factory.SingletonSeparatedList(Factory.ModifiedIdentifier(forVariableToken))
                    controlVariable = Factory.VariableDeclarator(names,
                                                                 asClause:=If(node.Declaration.Type.IsVar, Nothing, Factory.SimpleAsClause(DirectCast(node.Declaration.Type.Accept(_nodesVisitor), TypeSyntax))),
                                                                 initializer:=Nothing
                                                                )
                Else
                    Dim initializer As CSS.AssignmentExpressionSyntax = TryCast(node.Initializers.FirstOrDefault(), CSS.AssignmentExpressionSyntax)
                    If initializer Is Nothing OrElse Not initializer.IsKind(CS.SyntaxKind.SimpleAssignmentExpression) Then
                        Return False
                    End If
                    If TypeOf initializer.Left IsNot CSS.IdentifierNameSyntax Then
                        Return False
                    End If
                    If DirectCast(initializer.Left, CSS.IdentifierNameSyntax).Identifier.IsEquivalentTo(iteratorIdentifier.Identifier) Then
                        Return False
                    End If
                    controlVariable = initializer.Left.Accept(_nodesVisitor)
                    fromValue = DirectCast(initializer.Right.Accept(_nodesVisitor), ExpressionSyntax)
                End If

                Dim stmtFirstToken As SyntaxToken = node.Statement.GetFirstToken
                Dim openBraceLeadingTrivia As New SyntaxTriviaList
                Dim closingBraceTrailingTrivia As New SyntaxTriviaList

                Dim statements As SyntaxList(Of StatementSyntax) = Me.ConvertBlock(node.Statement, openBraceLeadingTrivia, closingBraceTrailingTrivia)

                Dim forStatementTrailingTrivia As SyntaxTriviaList = node.CloseParenToken.CollectConvertedTokenTrivia(getLeading:=True, getTrailing:=True)
                If stmtFirstToken.IsKind(CS.SyntaxKind.OpenBraceToken) Then
                    If stmtFirstToken.TrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                        forStatementTrailingTrivia = forStatementTrailingTrivia.AddRange(stmtFirstToken.TrailingTrivia.ConvertTriviaList())
                    End If
                End If

                Dim stepClause As ForStepClauseSyntax = If([step] = 1,
                                                           Nothing,
                                                           Factory.ForStepClause(Factory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression,
                                                                                                           Factory.Literal([step]))
                                                          ))
                Dim forStmt As ForStatementSyntax = Factory.ForStatement(ForKeyword.WithConvertedLeadingTriviaFrom(node.ForKeyword),
                                                                          controlVariable,
                                                                          EqualsToken,
                                                                          fromValue,
                                                                          ToKeyword,
                                                                          toValue,
                                                                          stepClause
                                                                         ).WithTrailingTrivia(forStatementTrailingTrivia).WithTrailingEol
                block = Factory.ForBlock(forStmt,
                                         statements,
                                         Factory.NextStatement().WithTrailingTrivia(closingBraceTrailingTrivia).WithTrailingEol
                                        )
                Return True
            End Function

            Private Function ConvertSingleBlock(node As CSS.ExpressionSyntax) As StatementSyntax
                Dim exprNode As VB.VisualBasicSyntaxNode = Nothing
                Dim newLeadingTrivia As New SyntaxTriviaList

                If TypeOf node Is CSS.AssignmentExpressionSyntax Then
                    Dim csAssignment As CSS.AssignmentExpressionSyntax = DirectCast(node, CSS.AssignmentExpressionSyntax)
                    If csAssignment.Left.IsKind(CS.SyntaxKind.ParenthesizedExpression) Then
                        Dim csLeft As CSS.ParenthesizedExpressionSyntax = DirectCast(csAssignment.Left, CSS.ParenthesizedExpressionSyntax)
                        Dim leftExpr As ExpressionSyntax = CType(csLeft.Expression.Accept(_nodesVisitor), ExpressionSyntax)
                        Dim rightExpr As ExpressionSyntax = DirectCast(csAssignment.Right.Accept(_nodesVisitor), ExpressionSyntax)
                        If csAssignment.IsKind(CS.SyntaxKind.SimpleAssignmentExpression) Then
                            exprNode = Factory.SimpleAssignmentStatement(leftExpr, rightExpr).
                                                         WithConvertedTriviaFrom(node)
                            newLeadingTrivia = newLeadingTrivia.AddRange(node.CheckCorrectnessLeadingTrivia(attemptToPortMade:=False, "Parenthesized Expression Assignment"))
                        End If
                    End If
                ElseIf TypeOf node Is CSS.PostfixUnaryExpressionSyntax Then
                    Dim csPostFixUnaryExpression As CSS.PostfixUnaryExpressionSyntax = DirectCast(node, CSS.PostfixUnaryExpressionSyntax)
                    Dim csOperand As CSS.ParenthesizedExpressionSyntax = TryCast(csPostFixUnaryExpression.Operand, CSS.ParenthesizedExpressionSyntax)
                    If csOperand IsNot Nothing Then
                        Dim kind As VB.SyntaxKind = CS.CSharpExtensions.Kind(node).GetExpressionKind()
                        Dim operandExpr As ExpressionSyntax = DirectCast(csOperand.Expression.Accept(_nodesVisitor), ExpressionSyntax)
                        exprNode = Factory.AssignmentStatement(CS.CSharpExtensions.Kind(node).GetExpressionKind(),
                                                               operandExpr,
                                                               kind.GetOperatorToken(isReferenceType:=False),
                                                               OneExpression)
                        newLeadingTrivia = newLeadingTrivia.AddRange(node.CheckCorrectnessLeadingTrivia(attemptToPortMade:=False, "Parenthesized Expression Assignment"))
                    End If
                End If
                If exprNode Is Nothing Then
                    exprNode = node.Accept(_nodesVisitor)
                    If exprNode.IsKind(VB.SyntaxKind.TryBlock) Then
                        Dim tmpTrivia As SyntaxTriviaList = exprNode.GetLeadingTrivia
                        If tmpTrivia.Count = 2 AndAlso tmpTrivia(0).IsComment AndAlso tmpTrivia(1).IsEndOfLine Then
                            newLeadingTrivia = newLeadingTrivia.AddRange(tmpTrivia)
                        Else
                            newLeadingTrivia = newLeadingTrivia.AddRange(node.GetLeadingTrivia.ConvertTriviaList())
                        End If
                    Else
                        newLeadingTrivia = newLeadingTrivia.AddRange(node.GetLeadingTrivia.ConvertTriviaList())
                    End If
                End If
                Dim newTrailingTrivia As SyntaxTriviaList = exprNode.GetTrailingTrivia
                exprNode = exprNode.WithoutTrivia
                If TypeOf exprNode IsNot StatementSyntax Then
                    Select Case True
                        Case TypeOf exprNode Is ObjectCreationExpressionSyntax
                            exprNode = FactoryDimStatement(node.GetUniqueVariableNameInScope("tempVar", _nodesVisitor._usedIdentifiers, _semanticModel),
                                                          Factory.AsNewClause(DirectCast(exprNode, NewExpressionSyntax)),
                                                          initializer:=Nothing)
                        Case TypeOf exprNode Is InvocationExpressionSyntax
                            exprNode = If(exprNode.GetFirstToken.IsKind(VB.SyntaxKind.NewKeyword), Factory.CallStatement(DirectCast(exprNode, ExpressionSyntax).WithLeadingTrivia(SpaceTrivia)), DirectCast(Factory.ExpressionStatement(DirectCast(exprNode, ExpressionSyntax)), VB.VisualBasicSyntaxNode))
                        Case Else
                            exprNode = Factory.ExpressionStatement(DirectCast(exprNode, ExpressionSyntax))
                    End Select
                End If
                Return DirectCast(exprNode, StatementSyntax).WithLeadingTrivia(newLeadingTrivia).WithTrailingTrivia(newTrailingTrivia).WithTrailingEol
            End Function

            Private Function ConvertSingleExpression(node As CSS.ExpressionSyntax, leadingTrivia As SyntaxTriviaList, trailingTrivia As SyntaxTriviaList) As List(Of StatementSyntax)
                Dim statementList As New List(Of StatementSyntax)
                Dim oneStatement As VB.VisualBasicSyntaxNode = Nothing
                Dim newLeadingTrivia As New SyntaxTriviaList

                If leadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    Dim convertedLeadingTrivia As SyntaxTriviaList = leadingTrivia.ConvertTriviaList()
                    newLeadingTrivia = newLeadingTrivia.AddRange(convertedLeadingTrivia)
                End If

                Dim rightExpr As ExpressionSyntax
                If TypeOf node Is CSS.AssignmentExpressionSyntax Then
                    Dim csAssignment As CSS.AssignmentExpressionSyntax = DirectCast(node, CSS.AssignmentExpressionSyntax)
                    rightExpr = DirectCast(csAssignment.Right.Accept(_nodesVisitor), ExpressionSyntax)
                    If csAssignment.Left.IsKind(CS.SyntaxKind.ParenthesizedExpression) Then
                        Dim csLeft As CSS.ParenthesizedExpressionSyntax = DirectCast(csAssignment.Left, CSS.ParenthesizedExpressionSyntax)
                        Dim leftExpression As ExpressionSyntax = CType(csLeft.Expression.Accept(_nodesVisitor), ExpressionSyntax)
                        If csAssignment.IsKind(CS.SyntaxKind.SimpleAssignmentExpression) Then
                            oneStatement = Factory.SimpleAssignmentStatement(leftExpression, rightExpr).
                                                         WithConvertedTriviaFrom(node)
                            newLeadingTrivia = newLeadingTrivia.AddRange(node.CheckCorrectnessLeadingTrivia(attemptToPortMade:=False, "Parenthesized Expression Assignment"))
                        End If
                    End If
                    ' Handle New Invocation Expression = something
                    'Dim x As IO.FileInfo = New IO.FileInfo("")
                    'x.IsReadOnly = True
                    If node.IsKind(CS.SyntaxKind.SimpleAssignmentExpression) Then
                        Dim csMemberAccessExpression As CSS.MemberAccessExpressionSyntax = TryCast(csAssignment.Left, CSS.MemberAccessExpressionSyntax)
                        If csMemberAccessExpression IsNot Nothing Then
                            Dim csObjectCreationExpression As CSS.ObjectCreationExpressionSyntax = TryCast(csMemberAccessExpression.Expression, CSS.ObjectCreationExpressionSyntax)
                            If csObjectCreationExpression IsNot Nothing Then
                                Dim newExpression As NewExpressionSyntax = DirectCast(csObjectCreationExpression.Accept(_nodesVisitor), NewExpressionSyntax)
                                Dim nameToken As SyntaxToken = Factory.Identifier(node.GetUniqueVariableNameInScope("tempVar", _nodesVisitor._usedIdentifiers, _semanticModel))
                                statementList.Add(FactoryDimStatement(nameToken,
                                                                      Factory.AsNewClause(newExpression.WithLeadingTrivia(SpaceTrivia)),
                                                                      initializer:=Nothing
                                                                      ).WithLeadingTrivia(newExpression.GetLeadingTrivia)
                                                  )
                                Dim kind As VB.SyntaxKind = CS.CSharpExtensions.Kind(node).GetExpressionKind()
                                Dim operatorToken As SyntaxToken = kind.GetOperatorToken(isReferenceType:=False)
                                statementList.Add(Factory.AssignmentStatement(kind,
                                                                              Factory.SimpleMemberAccessExpression(Factory.IdentifierName(nameToken),
                                                                                                                   DotToken,
                                                                                                                   CType(csMemberAccessExpression.Name.Accept(_nodesVisitor), SimpleNameSyntax)),
                                                                              operatorToken,
                                                                              rightExpr).WithTrailingEol)
                                Return statementList
                            End If
                        End If
                    ElseIf csAssignment.Left.IsKind(CS.SyntaxKind.SimpleMemberAccessExpression) AndAlso rightExpr.IsKind(VB.SyntaxKind.AddressOfExpression) Then
                        Dim vbMemberAccessExpression As MemberAccessExpressionSyntax = CType(csAssignment.Left.Accept(_nodesVisitor), MemberAccessExpressionSyntax)
                        Dim statementLeadingTrivia As New SyntaxTriviaList
                        If vbMemberAccessExpression.ContainsCommentOrDirectiveTrivia Then
                            statementLeadingTrivia = statementLeadingTrivia.AddRange(vbMemberAccessExpression.GetLeadingTrivia)
                            vbMemberAccessExpression = vbMemberAccessExpression.WithLeadingTrivia(SpaceTrivia)
                        Else
                            vbMemberAccessExpression = vbMemberAccessExpression.AdjustExpressionTrivia(adjustLeading:=True, directiveNotAllowed:=False)
                        End If
                        Dim handlerStatement As AddRemoveHandlerStatementSyntax
                        If node.IsKind(CS.SyntaxKind.AddAssignmentExpression) Then
                            handlerStatement = Factory.AddHandlerStatement(vbMemberAccessExpression, rightExpr)
                        Else
                            handlerStatement = Factory.RemoveHandlerStatement(vbMemberAccessExpression, rightExpr)
                        End If
                        statementList.Add(handlerStatement.WithLeadingTrivia(statementLeadingTrivia).WithTrailingEol)
                        Return statementList
                    End If
                ElseIf TypeOf node Is CSS.PostfixUnaryExpressionSyntax Then
                    Dim csPostFixUnaryExpression As CSS.PostfixUnaryExpressionSyntax = DirectCast(node, CSS.PostfixUnaryExpressionSyntax)
                    Dim csOperand As CSS.ParenthesizedExpressionSyntax = TryCast(csPostFixUnaryExpression.Operand, CSS.ParenthesizedExpressionSyntax)
                    If csOperand IsNot Nothing Then
                        Dim kind As VB.SyntaxKind = CS.CSharpExtensions.Kind(node).GetExpressionKind()
                        Dim operandExpression As ExpressionSyntax = DirectCast(csOperand.Expression.Accept(_nodesVisitor), ExpressionSyntax)
                        oneStatement = Factory.AssignmentStatement(CS.CSharpExtensions.Kind(node).GetExpressionKind(),
                                                                   operandExpression,
                                                                   kind.GetOperatorToken(isReferenceType:=False),
                                                                   OneExpression)
                        newLeadingTrivia = newLeadingTrivia.AddRange(node.CheckCorrectnessLeadingTrivia(attemptToPortMade:=False, "Parenthesized Expression Assignment"))
                    End If
                End If
                Dim newTrailingTrivia As SyntaxTriviaList
                If trailingTrivia.ContainsCommentOrDirectiveTrivia Then
                    newTrailingTrivia = newTrailingTrivia.AddRange(trailingTrivia.ConvertTriviaList())
                End If
                If oneStatement Is Nothing Then
                    oneStatement = node.Accept(_nodesVisitor)
                    If Not EndsWithSimilarTrivia(node.GetLeadingTrivia.ConvertTriviaList(), oneStatement.GetLeadingTrivia) Then
                        newLeadingTrivia = newLeadingTrivia.AddRange(oneStatement.GetLeadingTrivia)
                    End If

                    If Not EndsWithSimilarTrivia(node.GetTrailingTrivia.ConvertTriviaList(), oneStatement.GetTrailingTrivia) Then
                        newTrailingTrivia = newTrailingTrivia.AddRange(oneStatement.GetTrailingTrivia)
                    End If
                    If oneStatement.IsKind(VB.SyntaxKind.TryBlock) Then
                        Dim tryLeadingTrivia As SyntaxTriviaList = oneStatement.GetLeadingTrivia
                        If tryLeadingTrivia.Any Then
                            If tryLeadingTrivia(0).IsComment AndAlso tryLeadingTrivia(0).ToFullString = "' TODO: This Try Block can be removed" Then
                                statementList.AddRange(DirectCast(oneStatement, TryBlockSyntax).Statements)
                                Dim i As Integer
                                For i = 0 To newLeadingTrivia.Count - 1
                                    Dim t As SyntaxTrivia = newLeadingTrivia(i)
                                    If t.IsComment AndAlso t.ToFullString = "' TODO: This Try Block can be removed" Then
                                        Exit For
                                    End If
                                Next
                                newLeadingTrivia = newLeadingTrivia.RemoveAt(i)
                                While newLeadingTrivia.Count - 1 > i AndAlso newLeadingTrivia(i).IsEndOfLine
                                    Dim nextTrivia As SyntaxTrivia = newLeadingTrivia.GetForwardTriviaOrDefault(i, 1)
                                    If nextTrivia.IsWhitespace AndAlso i + 1 < newLeadingTrivia.Count - 1 Then
                                        newLeadingTrivia = newLeadingTrivia.RemoveAt(i + 1)
                                    End If
                                    newLeadingTrivia = newLeadingTrivia.RemoveAt(i)
                                End While
                                If newLeadingTrivia.Last.IsEndOfLine AndAlso newLeadingTrivia.GetForwardTriviaOrDefault(newLeadingTrivia.Count - 2, 0).IsWhitespace Then
                                    newLeadingTrivia = newLeadingTrivia.RemoveAt(newLeadingTrivia.Count - 1)
                                End If
                                statementList(0) = statementList(0).WithLeadingTrivia(newLeadingTrivia)
                                Dim endIndex As Integer = statementList.Count - 1
                                statementList(endIndex) = statementList(endIndex).WithTrailingTrivia(newTrailingTrivia).WithTrailingEol
                                Return statementList
                            End If
                        End If
                    End If
                End If
                newLeadingTrivia = New SyntaxTriviaList
                If leadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    newLeadingTrivia = newLeadingTrivia.AddRange(leadingTrivia.ConvertTriviaList())
                End If
                If Not EndsWithSimilarTrivia(node.GetLeadingTrivia, leadingTrivia) Then
                    newLeadingTrivia = newLeadingTrivia.AddRange(node.GetLeadingTrivia.ConvertTriviaList())
                End If
                oneStatement = oneStatement.WithoutTrivia
                If TypeOf oneStatement IsNot StatementSyntax Then
                    Select Case True
                        Case TypeOf oneStatement Is ObjectCreationExpressionSyntax
                            oneStatement = FactoryDimStatement(node.GetUniqueVariableNameInScope("tempVar", _nodesVisitor._usedIdentifiers, _semanticModel),
                                                              Factory.AsNewClause(DirectCast(oneStatement, NewExpressionSyntax)),
                                                              initializer:=Nothing)
                        Case TypeOf oneStatement Is AwaitExpressionSyntax
                            oneStatement = Factory.ExpressionStatement(CType(oneStatement, AwaitExpressionSyntax))

                        Case TypeOf oneStatement Is InvocationExpressionSyntax
                            oneStatement = If(oneStatement.GetFirstToken.IsKind(VB.SyntaxKind.NewKeyword), Factory.CallStatement(DirectCast(oneStatement, ExpressionSyntax).WithLeadingTrivia(SpaceTrivia)), DirectCast(Factory.ExpressionStatement(DirectCast(oneStatement, ExpressionSyntax)), VB.VisualBasicSyntaxNode))
                        Case Else
                            oneStatement = Factory.ExpressionStatement(DirectCast(oneStatement, ExpressionSyntax))
                    End Select
                End If
                statementList.AddRange(ReplaceOneStatementWithMarkedStatements(node, DirectCast(oneStatement, StatementSyntax).WithLeadingTrivia(newLeadingTrivia).WithTrailingTrivia(newTrailingTrivia).WithTrailingEol))
                Return statementList
            End Function

            Private Function ConvertSwitchSection(section As CSS.SwitchSectionSyntax) As CaseBlockSyntax
                Dim leadingStatements As New List(Of StatementSyntax)
                If section.Labels.OfType(Of CSS.DefaultSwitchLabelSyntax)().Any() Then
                    Return Factory.CaseElseBlock(Factory.CaseElseStatement(Factory.ElseCaseClause()), Me.ConvertSwitchSectionBlock(section, leadingStatements))
                End If
                Dim labelList As New List(Of CaseClauseSyntax)
                Dim vbLabelLeadingTrivia As SyntaxTriviaList = section.GetLeadingTrivia.ConvertTriviaList()
                Dim csLabelTrailingTrivia As New SyntaxTriviaList
                ' Find Case leading space
                For Each caseLabel As CSS.SwitchLabelSyntax In section.Labels
                    Dim caseLabelExpr As ExpressionSyntax
                    Dim caseLabelWhenExpr As ExpressionSyntax
                    Select Case True
                        Case TypeOf caseLabel Is CSS.CaseSwitchLabelSyntax
                            caseLabelExpr = DirectCast(CType(caseLabel, CSS.CaseSwitchLabelSyntax).Value.Accept(_nodesVisitor), ExpressionSyntax)
                            caseLabelWhenExpr = Nothing
                        Case TypeOf caseLabel Is CSS.CasePatternSwitchLabelSyntax
                            Dim patternLabel As CSS.CasePatternSwitchLabelSyntax = DirectCast(caseLabel, CSS.CasePatternSwitchLabelSyntax)
                            Dim variableNameToken As SyntaxToken
                            caseLabelWhenExpr = CType(patternLabel.WhenClause?.Accept(_nodesVisitor), ExpressionSyntax)
                            If TypeOf patternLabel.Pattern Is CSS.ConstantPatternSyntax Then
                                Dim constantPattern As CSS.ConstantPatternSyntax = DirectCast(patternLabel.Pattern, CSS.ConstantPatternSyntax)
                                caseLabelExpr = DirectCast(constantPattern.Expression.Accept(_nodesVisitor), ExpressionSyntax).WithConvertedLeadingTriviaFrom(patternLabel)
                                csLabelTrailingTrivia = csLabelTrailingTrivia.AddRange(caseLabel.GetTrailingTrivia)
                            ElseIf TypeOf patternLabel.Pattern Is CSS.DeclarationPatternSyntax Then
                                Dim pattern As CSS.DeclarationPatternSyntax = DirectCast(patternLabel.Pattern, CSS.DeclarationPatternSyntax)
                                Dim type As TypeSyntax = DirectCast(pattern.Type.Accept(_nodesVisitor), TypeSyntax)
                                If TypeOf pattern.Designation Is CSS.SingleVariableDesignationSyntax Then
                                    variableNameToken = GenerateSafeVbToken(DirectCast(pattern.Designation, CSS.SingleVariableDesignationSyntax).Identifier,
                                        section,
                                        _semanticModel, _nodesVisitor._usedIdentifiers)
                                ElseIf TypeOf pattern.Designation Is CSS.DiscardDesignationSyntax Then
                                Else
                                    Stop
                                End If

                                Dim switchExpr As ExpressionSyntax = DirectCast(DirectCast(section.Parent, CSS.SwitchStatementSyntax).Expression.Accept(_nodesVisitor), ExpressionSyntax)
                                If TypeOf pattern.Designation Is CSS.DiscardDesignationSyntax Then
                                    caseLabelExpr = Factory.TypeOfIsExpression(switchExpr, CType(pattern.Type.Accept(_nodesVisitor), TypeSyntax))
                                Else
                                    leadingStatements.Add(FactoryDimStatement(variableNameToken,
                                                                                Factory.SimpleAsClause(type),
                                                                                Factory.EqualsValue(Factory.CTypeExpression(switchExpr, type))
                                                                                ))
                                    caseLabelExpr = DirectCast(pattern.Designation.Accept(_nodesVisitor), ExpressionSyntax)
                                End If
                            ElseIf TypeOf patternLabel.Pattern Is CSS.VarPatternSyntax Then
                                Dim switchExpression1 As ExpressionSyntax = DirectCast(DirectCast(section.Parent, CSS.SwitchStatementSyntax).Expression.Accept(_nodesVisitor), ExpressionSyntax)
                                caseLabelExpr = Nothing

                                Dim varPattern As CSS.VarPatternSyntax = CType(patternLabel.Pattern, CSS.VarPatternSyntax)
                                If varPattern.Designation.IsKind(CS.SyntaxKind.DiscardDesignation) Then
                                    If patternLabel.WhenClause IsNot Nothing Then
                                        caseLabelWhenExpr = CType(patternLabel.WhenClause.Accept(_nodesVisitor), ExpressionSyntax)
                                    Else
                                        caseLabelExpr = Factory.IdentifierName("Else")
                                    End If
                                ElseIf varPattern.Designation.IsKind(CS.SyntaxKind.SingleVariableDesignation) Then
                                    variableNameToken = GenerateSafeVbToken(DirectCast(varPattern.Designation, CSS.SingleVariableDesignationSyntax).Identifier,
                                        section,
                                        _semanticModel, _nodesVisitor._usedIdentifiers)
                                    leadingStatements.Add(FactoryDimStatement(variableNameToken,
                                                                                Factory.SimpleAsClause(Factory.PredefinedType(ObjectKeyword)),
                                                                                Factory.EqualsValue(switchExpression1)).WithTrailingEol)
                                    caseLabelExpr = Factory.IdentifierName("Else")
                                Else
                                    caseLabelExpr = Nothing
                                End If
                            ElseIf TypeOf patternLabel.Pattern Is CSS.RecursivePatternSyntax Then
                                caseLabelExpr = NothingExpression
                                vbLabelLeadingTrivia = vbLabelLeadingTrivia.AddRange(section.CheckCorrectnessLeadingTrivia(attemptToPortMade:=False, $"VB has no equivalent to the C# 'Recursive Pattern({patternLabel.Pattern}) in 'case' statements"))
                            Else
                                caseLabelExpr = Nothing
                            End If
                        Case Else
                            caseLabelExpr = Nothing
                            caseLabelWhenExpr = Nothing
                    End Select
                    If caseLabelExpr Is Nothing Then
                        labelList.Add(Factory.SimpleCaseClause(caseLabelWhenExpr))
                        csLabelTrailingTrivia = csLabelTrailingTrivia.AddRange(section.GetTrailingTrivia)
                    Else
                        If caseLabelWhenExpr Is Nothing Then
                            labelList.Add(Factory.SimpleCaseClause(caseLabelExpr.WithoutLeadingTrivia))
                        Else
                            ' TODO use line continuation instead of space
                            labelList.Add(Factory.SimpleCaseClause(caseLabelExpr.With({SpaceTrivia}, {SpaceTrivia})))
                            leadingStatements.Add(Factory.SingleLineIfStatement(caseLabelWhenExpr.With({SpaceTrivia}, {SpaceTrivia}),
                                                                                     Factory.SingletonList(Of StatementSyntax)(Factory.ExitSelectStatement),
                                                                                     elseClause:=Nothing
                                                                                     ).WithLeadingTrivia(caseLabelExpr.GetLeadingTrivia).
                                                                                       WithTrailingEol
                                                    )
                        End If
                    End If
                Next
                Dim commentString As New StringBuilder
                csLabelTrailingTrivia = csLabelTrailingTrivia.AddRange(section.GetTrailingTrivia)
                For Each t As SyntaxTrivia In csLabelTrailingTrivia.ConvertTriviaList()
                    Select Case t.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            commentString.Append(t.ToString)
                        Case VB.SyntaxKind.EndOfLineTrivia
                            'ignore
                        Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                            commentString.Append(t.ToString.Trim.TrimStart("'"c).Trim)
                        Case Else
                            Stop
                    End Select
                Next
                Dim trailingTrivia As New SyntaxTriviaList
                If commentString.Length > 0 Then
                    trailingTrivia = trailingTrivia.AddRange({Factory.CommentTrivia($"'{commentString.ToString.Trim}"), VbEolTrivia})
                End If
                Return Factory.CaseBlock(Factory.CaseStatement(Factory.SeparatedList(labelList)) _
                                                .With(vbLabelLeadingTrivia, trailingTrivia) _
                                                .WithTrailingEol(),
                                         Me.ConvertSwitchSectionBlock(section, leadingStatements))
            End Function

            Private Function ConvertSwitchSectionBlock(section As CSS.SwitchSectionSyntax, statements As List(Of StatementSyntax)) As SyntaxList(Of StatementSyntax)
                Dim lastStatement As CSS.StatementSyntax = section.Statements.LastOrDefault()
                Dim openBraceLeadingTrivia As New SyntaxTriviaList
                Dim closingBraceTrailingTrivia As New SyntaxTriviaList
                For Each s As CSS.StatementSyntax In section.Statements
                    If s Is lastStatement AndAlso TypeOf s Is CSS.BreakStatementSyntax Then
                        If lastStatement.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia OrElse
                            lastStatement.GetTrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                            statements.Add(Factory.EmptyStatement.WithConvertedTriviaFrom(lastStatement))
                        End If
                        Continue For
                    End If
                    statements.AddRange(Me.ConvertBlock(s, openBraceLeadingTrivia, closingBraceTrailingTrivia))
                    If openBraceLeadingTrivia.Any Then
                        statements(0) = statements.First.WithPrependedLeadingTrivia(openBraceLeadingTrivia)
                    End If
                    If closingBraceTrailingTrivia.Any Then
                        statements(statements.Count - 1) = statements.Last.WithAppendedTrailingTrivia(closingBraceTrailingTrivia)
                    End If
                Next
                Return Factory.List(statements)
            End Function

            Private Function MakeGotoSwitchLabel(expression As VB.VisualBasicSyntaxNode) As String
                If TypeOf expression Is ElseCaseClauseSyntax Then
                    Return $"_Select{_switchCount}_CaseDefault"
                End If
                Dim expressionString As String = expression.ToString

                Return expressionString.GetSafeVbName()
            End Function

            Private Function TryConvertIfNotNullRaiseEvent(node As CSS.IfStatementSyntax, ByRef raiseEventStatement As StatementSyntax) As Boolean
                raiseEventStatement = Nothing
                Dim comparisonExpression As CSS.BinaryExpressionSyntax = Nothing
                Return TryGetBinaryExpression(node, comparisonExpression, CS.SyntaxKind.NotEqualsExpression, CS.SyntaxKind.NullLiteralExpression) AndAlso
                    Me.TryConvertRaiseEvent(node.Statement, raiseEventStatement)
            End Function

            Private Function TryConvertRaiseEvent(resultStatement As CSS.StatementSyntax, ByRef raiseEventStatement As StatementSyntax) As Boolean
                Dim singleStatement As CSS.ExpressionStatementSyntax
                Dim isBlock As Boolean = TypeOf resultStatement Is CSS.BlockSyntax
                Dim block As CSS.BlockSyntax = If(isBlock, CType(resultStatement, CSS.BlockSyntax), Nothing)
                If isBlock Then
                    If block.Statements.Count <> 1 Then
                        Return False
                    End If
                    singleStatement = TryCast(block.Statements(0), CSS.ExpressionStatementSyntax)
                Else
                    singleStatement = TryCast(resultStatement, CSS.ExpressionStatementSyntax)
                End If

                Dim isInvocationExpression As Boolean = TypeOf singleStatement?.Expression Is CSS.InvocationExpressionSyntax
                Dim singleInvocationExpression As CSS.InvocationExpressionSyntax = If(isInvocationExpression, CType(singleStatement?.Expression, CSS.InvocationExpressionSyntax), Nothing)
                If Not isInvocationExpression Then
                    Return False
                End If

                raiseEventStatement = TryCast(singleInvocationExpression.Accept(_nodesVisitor), RaiseEventStatementSyntax)
                If raiseEventStatement Is Nothing Then
                    Return False
                End If
                raiseEventStatement = raiseEventStatement.WithTrailingEol
                Return True
            End Function

            Private Function WillConvertToFor(node As CSS.ForStatementSyntax) As Boolean
                Dim hasVariable As Boolean = node.Declaration IsNot Nothing AndAlso node.Declaration.Variables.Count = 1
                If Not (hasVariable OrElse node.Initializers.Count = 1) Then
                    Return False
                End If
                If node.Incrementors.Count <> 1 Then
                    Return False
                End If
                Dim incrementors As VB.VisualBasicSyntaxNode = node.Incrementors.First?.Accept(_nodesVisitor)
                Dim iterator As AssignmentStatementSyntax = TryCast(incrementors, AssignmentStatementSyntax)
                If iterator Is Nothing OrElse Not iterator.IsKind(VB.SyntaxKind.AddAssignmentStatement, VB.SyntaxKind.SubtractAssignmentStatement) Then
                    Return False
                End If
                Dim iteratorIdentifier As IdentifierNameSyntax = TryCast(iterator.Left, IdentifierNameSyntax)
                If iteratorIdentifier Is Nothing Then
                    Return False
                End If
                Dim stepExpression As LiteralExpressionSyntax = TryCast(iterator.Right, LiteralExpressionSyntax)
                If stepExpression Is Nothing OrElse Not (TypeOf stepExpression.Token.Value Is Integer) Then
                    Return False
                End If
                Dim condition As CSS.BinaryExpressionSyntax = TryCast(node.Condition, CSS.BinaryExpressionSyntax)
                If condition Is Nothing OrElse Not (TypeOf condition.Left Is CSS.IdentifierNameSyntax) Then
                    Return False
                End If
                If DirectCast(condition.Left, CSS.IdentifierNameSyntax).Identifier.IsEquivalentTo(iteratorIdentifier.Identifier) Then
                    Return False
                End If
                If iterator.IsKind(VB.SyntaxKind.SubtractAssignmentStatement) Then
                    If condition.IsKind(CS.SyntaxKind.GreaterThanOrEqualExpression) OrElse condition.IsKind(CS.SyntaxKind.NotEqualsExpression) Then
                    ElseIf condition.IsKind(CS.SyntaxKind.GreaterThanExpression) Then
                    Else
                        Return False
                    End If
                Else
                    If condition.IsKind(CS.SyntaxKind.LessThanOrEqualExpression) OrElse condition.IsKind(CS.SyntaxKind.NotEqualsExpression) Then
                    ElseIf condition.IsKind(CS.SyntaxKind.LessThanExpression) Then
                    Else
                        Return False
                    End If
                End If

                Dim start As ExpressionSyntax
                If hasVariable Then
                    Dim v As CSS.VariableDeclaratorSyntax = node.Declaration.Variables(0)
                    start = DirectCast(v.Initializer?.Value.Accept(_nodesVisitor), ExpressionSyntax)
                    If start Is Nothing Then
                        Return False
                    End If
                Else
                    Dim initializer As CSS.AssignmentExpressionSyntax = TryCast(node.Initializers.FirstOrDefault(), CSS.AssignmentExpressionSyntax)
                    If initializer Is Nothing OrElse Not initializer.IsKind(CS.SyntaxKind.SimpleAssignmentExpression) Then
                        Return False
                    End If
                    If TypeOf initializer.Left IsNot CSS.IdentifierNameSyntax Then
                        Return False
                    End If
                    If DirectCast(initializer.Left, CSS.IdentifierNameSyntax).Identifier.IsEquivalentTo(iteratorIdentifier.Identifier) Then
                        Return False
                    End If
                End If
                Return True
            End Function

            <ExcludeFromCodeCoverage>
            Public Overrides Function DefaultVisit(node As SyntaxNode) As SyntaxList(Of StatementSyntax)
                Throw New NotImplementedException(node.GetType().ToString & " not implemented!")
            End Function

            Public Overrides Function VisitBlock(node As CSS.BlockSyntax) As SyntaxList(Of StatementSyntax)
                Dim savedNeedEndUsingCount As Integer = _nodesVisitor.NeededEndUsingCount
                Try
                    _nodesVisitor.NeededEndUsingCount = 0
                    Dim stmtList As SyntaxList(Of StatementSyntax) = Factory.List(node.Statements.Where(Function(s As CSS.StatementSyntax) Not (TypeOf s Is CSS.EmptyStatementSyntax)).SelectMany(Function(s As CSS.StatementSyntax) s.Accept(Me)))
                    Dim ifStatement As IfStatementSyntax = Nothing
                    Dim isSubBlock As Boolean = node.Parent.IsKind(CS.SyntaxKind.Block)
                    If isSubBlock AndAlso stmtList.Any Then
                        ifStatement = Factory.IfStatement(IfKeyword, Factory.TrueLiteralExpression(TrueKeyword), ThenKeyword)
                    End If
                    If node.OpenBraceToken.HasLeadingTrivia OrElse node.OpenBraceToken.HasTrailingTrivia Then
                        If stmtList.Any Then
                            If isSubBlock Then
                                ifStatement = ifStatement.WithPrependedLeadingTrivia(node.OpenBraceToken.TrailingTrivia.ConvertTriviaList()).WithLeadingTrivia(node.OpenBraceToken.LeadingTrivia.ConvertTriviaList())
                            Else
                                stmtList = stmtList.Replace(stmtList(0), stmtList(0).WithPrependedLeadingTrivia(node.OpenBraceToken.TrailingTrivia.ConvertTriviaList()).WithPrependedLeadingTrivia(node.OpenBraceToken.LeadingTrivia.ConvertTriviaList()))
                            End If
                        Else
                            stmtList = stmtList.Add(Factory.EmptyStatement.WithConvertedTriviaFrom(node.OpenBraceToken))
                        End If
                    End If
                    If node.CloseBraceToken.HasLeadingTrivia OrElse node.OpenBraceToken.HasTrailingTrivia Then
                        If stmtList.Any Then
                            If isSubBlock AndAlso ifStatement IsNot Nothing Then
                                ifStatement = ifStatement.WithTrailingTrivia(node.OpenBraceToken.LeadingTrivia.ConvertTriviaList()).WithTrailingTrivia(node.OpenBraceToken.TrailingTrivia.ConvertTriviaList())
                            Else
                                stmtList = stmtList.Replace(stmtList.Last, stmtList.Last.WithAppendedTrailingTrivia(node.OpenBraceToken.LeadingTrivia.ConvertTriviaList()).WithAppendedTrailingTrivia(node.OpenBraceToken.TrailingTrivia.ConvertTriviaList()))
                            End If
                        Else
                            stmtList = stmtList.Add(Factory.EmptyStatement.WithConvertedTriviaFrom(node.CloseBraceToken))
                        End If
                    End If
                    stmtList = _nodesVisitor.AdjustUsingIfNeeded(stmtList)
                    If Not isSubBlock OrElse ifStatement Is Nothing Then
                        Return stmtList
                    Else
                        Dim newStmtList As New SyntaxList(Of StatementSyntax)
                        Return newStmtList.Add(Factory.MultiLineIfBlock(ifStatement, stmtList, Nothing, Nothing))
                    End If
                Finally
                    _nodesVisitor.NeededEndUsingCount = savedNeedEndUsingCount
                End Try
            End Function

            Public Overrides Function VisitBreakStatement(node As CSS.BreakStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim stmtKind As VB.SyntaxKind = VB.SyntaxKind.None
                Dim blockKeyword As SyntaxToken
                For Each stmt As CSS.StatementSyntax In node.GetAncestors(Of CSS.StatementSyntax)()
                    If TypeOf stmt Is CSS.DoStatementSyntax Then
                        stmtKind = VB.SyntaxKind.ExitDoStatement
                        blockKeyword = DoKeyword
                        Exit For
                    ElseIf TypeOf stmt Is CSS.WhileStatementSyntax Then
                        stmtKind = VB.SyntaxKind.ExitWhileStatement
                        blockKeyword = WhileKeyword
                        Exit For
                    ElseIf TypeOf stmt Is CSS.ForStatementSyntax Then
                        If Me.WillConvertToFor(DirectCast(stmt, CSS.ForStatementSyntax)) Then
                            stmtKind = VB.SyntaxKind.ExitForStatement
                            blockKeyword = ForKeyword
                            Exit For
                        Else
                            stmtKind = VB.SyntaxKind.ExitWhileStatement
                            blockKeyword = WhileKeyword
                            Exit For
                        End If
                    ElseIf TypeOf stmt Is CSS.ForEachStatementSyntax Then
                        stmtKind = VB.SyntaxKind.ExitForStatement
                        blockKeyword = ForKeyword
                        Exit For
                    ElseIf TypeOf stmt Is CSS.SwitchStatementSyntax Then
                        stmtKind = VB.SyntaxKind.ExitSelectStatement
                        blockKeyword = SelectKeyword
                        Exit For
                    End If
                Next
                blockKeyword = blockKeyword.WithAppendedTrailingTrivia(node.SemicolonToken.CollectConvertedTokenTrivia(getLeading:=True, getTrailing:=True))
                Return Factory.SingletonList(Of StatementSyntax)(Factory.ExitStatement(stmtKind,
                                                                                       blockKeyword).
                                                                                           WithConvertedLeadingTriviaFrom(node).
                                                                                           WithTrailingEol
                                                                )
            End Function

            Public Overrides Function VisitCheckedStatement(node As CSS.CheckedStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim openBraceLeadingTrivia As New SyntaxTriviaList
                Dim closingBraceTrailingTrivia As New SyntaxTriviaList
                If node.Keyword.IsKind(CS.SyntaxKind.CheckedKeyword) Then
                    Return WrapInComment(Me.ConvertBlock(node.Block, openBraceLeadingTrivia, closingBraceTrailingTrivia), node, "Visual Basic Default Is checked math, check that this works for you!")
                End If
                Return WrapInComment(Me.ConvertBlock(node.Block, openBraceLeadingTrivia, closingBraceTrailingTrivia), node, "Visual Basic does Not support unchecked statements!")
            End Function

            Public Overrides Function VisitContinueStatement(node As CSS.ContinueStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim statementKind As VB.SyntaxKind = VB.SyntaxKind.None
                Dim blockKeyword As SyntaxToken
                For Each stmt As CSS.StatementSyntax In node.GetAncestors(Of CSS.StatementSyntax)()
                    If TypeOf stmt Is CSS.DoStatementSyntax Then
                        statementKind = VB.SyntaxKind.ContinueDoStatement
                        blockKeyword = DoKeyword
                        Exit For
                    End If

                    If TypeOf stmt Is CSS.WhileStatementSyntax Then
                        statementKind = VB.SyntaxKind.ContinueWhileStatement
                        blockKeyword = WhileKeyword
                        Exit For
                    End If

                    If TypeOf stmt Is CSS.ForEachStatementSyntax OrElse TypeOf stmt Is CSS.ForEachVariableStatementSyntax Then
                        statementKind = VB.SyntaxKind.ContinueForStatement
                        blockKeyword = ForKeyword
                    End If

                    Dim forStatement As CSS.ForStatementSyntax = TryCast(stmt, CSS.ForStatementSyntax)
                    If forStatement IsNot Nothing Then
                        Dim isFor As Boolean = Me.WillConvertToFor(forStatement)
                        statementKind = If(isFor, VB.SyntaxKind.ContinueForStatement, VB.SyntaxKind.ContinueWhileStatement)
                        blockKeyword = If(isFor, ForKeyword, WhileKeyword)
                        Exit For
                    End If
                Next

                Return Factory.SingletonList(Of StatementSyntax)(
                    Factory.ContinueStatement(statementKind,
                                              blockKeyword
                                              ).WithConvertedLeadingTriviaFrom(node) _
                                               .WithTrailingTrivia(node.SemicolonToken.CollectConvertedTokenTrivia(
                                                                                               getLeading:=True,
                                                                                               getTrailing:=True)
                                                                                               ) _
                                               .WithTrailingEol)
            End Function

            Public Overrides Function VisitDeclarationExpression(node As CSS.DeclarationExpressionSyntax) As SyntaxList(Of StatementSyntax)
                Return MyBase.VisitDeclarationExpression(node)
            End Function

            Public Overrides Function VisitDoStatement(node As CSS.DoStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim condition As ExpressionSyntax = DirectCast(node.Condition.Accept(_nodesVisitor), ExpressionSyntax)
                Dim openBraceLeadingTrivia As New SyntaxTriviaList
                Dim closingBraceTrailingTrivia As New SyntaxTriviaList
                Dim stmt As SyntaxList(Of StatementSyntax) = Me.ConvertBlock(node.Statement, openBraceLeadingTrivia, closingBraceTrailingTrivia)
                If openBraceLeadingTrivia.Any OrElse closingBraceTrailingTrivia.Any Then
                    Stop
                End If
                Dim doStmt As DoStatementSyntax = Factory.DoStatement(VB.SyntaxKind.SimpleDoStatement)
                Dim loopStmt As LoopStatementSyntax = Factory.LoopStatement(VB.SyntaxKind.LoopWhileStatement, Factory.WhileClause(condition).WithTrailingEol)
                Dim block As DoLoopBlockSyntax = Factory.DoLoopWhileBlock(doStmt, stmt, loopStmt)
                Return ReplaceOneStatementWithMarkedStatements(node, block)
            End Function

            Public Overrides Function VisitEmptyStatement(node As CSS.EmptyStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim leadingTrivia As SyntaxTriviaList = node.GetLeadingTrivia.ConvertTriviaList()
                Dim trailingTrivia As SyntaxTriviaList = node.GetTrailingTrivia.ConvertTriviaList()
                If Not leadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    leadingTrivia = New SyntaxTriviaList
                End If
                If Not trailingTrivia.ContainsCommentOrDirectiveTrivia Then
                    trailingTrivia = New SyntaxTriviaList
                End If
                Return Factory.SingletonList(Of StatementSyntax)(Factory.EmptyStatement().With(leadingTrivia, trailingTrivia))
            End Function

            Public Overrides Function VisitExpressionStatement(node As CSS.ExpressionStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim statementList As List(Of StatementSyntax) = Me.ConvertSingleExpression(node.Expression,
                                                                                           node.GetLeadingTrivia,
                                                                                           node.GetTrailingTrivia)
                Return ReplaceStatementsWithMarkedStatements(node, statementList)
            End Function

            Public Overrides Function VisitFixedStatement(node As CSS.FixedStatementSyntax) As SyntaxList(Of StatementSyntax)
                Return Factory.SingletonList(Of StatementSyntax)(FlagUnsupportedStatements(node, "C# Fixed is not support by VB", commentOutOriginalStatements:=True))
            End Function

            Public Overrides Function VisitForEachStatement(node As CSS.ForEachStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim variableDeclarator As VariableDeclaratorSyntax
                Dim asClause As SimpleAsClauseSyntax = Nothing
                Dim forEachVariableToken As SyntaxToken = GenerateSafeVbToken(node.Identifier, node, _semanticModel, _nodesVisitor._usedIdentifiers)
                If node.Type.IsVar Then
                    Dim variableITypeSymbol As (_Error As Boolean, _ITypeSymbol As ITypeSymbol) = node.Expression.DetermineType(_semanticModel)
                    If variableITypeSymbol._Error = False Then
                        Dim type As TypeSyntax = variableITypeSymbol._ITypeSymbol.ConvertITypeSymbolToType
                        Select Case True
                            Case TypeOf type Is ArrayTypeSyntax
                                type = CType(type, ArrayTypeSyntax).ElementType
                            Case TypeOf type Is GenericNameSyntax
                                Dim genericName As GenericNameSyntax = CType(type, GenericNameSyntax)
                                Select Case genericName.TypeArgumentList.Arguments.Count
                                    Case 1
                                        type = genericName.TypeArgumentList.Arguments(0)
                                    Case 2
                                        type = Factory.ParseTypeName($"KeyValuePair(Of {genericName.TypeArgumentList.Arguments(0)},{genericName.TypeArgumentList.Arguments(1)})")
                                    Case Else
                                        Stop
                                End Select
                        End Select

                        asClause = If(type IsNot Nothing, Factory.SimpleAsClause(type), Nothing)
                    End If
                Else
                    Dim vbType As TypeSyntax
                    If node.Type.IsKind(CS.SyntaxKind.IdentifierName) Then
                        vbType = Factory.IdentifierName(GenerateSafeVbToken(DirectCast(node.Type, CSS.IdentifierNameSyntax).Identifier,
                                                                            node,
                                                                            _semanticModel,
                                                                            _nodesVisitor._usedIdentifiers,
                                                                            isQualifiedName:=False,
                                                                            isTypeName:=True)
                                                                           )
                    Else
                        vbType = DirectCast(node.Type.Accept(_nodesVisitor), TypeSyntax)
                    End If
                    asClause = Factory.SimpleAsClause(vbType)
                End If
                variableDeclarator = Factory.VariableDeclarator(Factory.SingletonSeparatedList(Factory.ModifiedIdentifier(forEachVariableToken).WithTrailingTrivia(SpaceTrivia)),
                                                                  asClause,
                                                                  initializer:=Nothing)

                Dim expression As ExpressionSyntax = DirectCast(node.Expression.Accept(_nodesVisitor), ExpressionSyntax).WithConvertedTrailingTriviaFrom(node.CloseParenToken)
                Dim openBraceLeadingTrivia As New SyntaxTriviaList
                Dim closingBraceTrailingTrivia As New SyntaxTriviaList
                Dim innerStatements As SyntaxList(Of StatementSyntax) = Me.ConvertBlock(node.Statement, openBraceLeadingTrivia, closingBraceTrailingTrivia)
                If openBraceLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    Stop
                End If
                If node.AwaitKeyword.IsKind(CS.SyntaxKind.None) Then

                    Dim forEachStatement As ForEachStatementSyntax = Factory.ForEachStatement(variableDeclarator, expression).WithTrailingEol
                    Dim block As ForEachBlockSyntax = Factory.ForEachBlock(forEachStatement.WithConvertedLeadingTriviaFrom(node.ForEachKeyword),
                                                                           innerStatements,
                                                                           Factory.NextStatement().WithLeadingTrivia(closingBraceTrailingTrivia).WithTrailingEol())
                    Return ReplaceOneStatementWithMarkedStatements(node, block)
                End If
                Dim messageEnumerator As SyntaxToken = Factory.Identifier(node.GetUniqueVariableNameInScope("messageEnumerator", _nodesVisitor._usedIdentifiers, _semanticModel))
                Dim cancelExpression As ExpressionSyntax = Factory.ParseExpression("Threading.CancellationToken.None")
                Dim methodStatement As CSS.MethodDeclarationSyntax = node.Parent.GetAncestor(Of CSS.MethodDeclarationSyntax)
                If methodStatement IsNot Nothing Then
                    Dim methodAttributes As SyntaxList(Of CSS.AttributeListSyntax) = methodStatement.AttributeLists
                    If methodAttributes.Any Then
                        Dim needCancelToken As Boolean = False
                        For Each methodAttribute As CSS.AttributeListSyntax In methodAttributes
                            If methodAttribute.Attributes.Count > 0 Then
                                If TypeOf methodAttribute.Attributes(0).Name Is CSS.QualifiedNameSyntax Then
                                    Dim qualifiedName As CSS.QualifiedNameSyntax = CType(methodAttribute.Attributes(0).Name, CSS.QualifiedNameSyntax)
                                    If qualifiedName.Right.Identifier.ValueText.EndsWith("EnumeratorCancellationAttribute", StringComparison.OrdinalIgnoreCase) Then
                                        needCancelToken = True
                                        Exit For
                                    End If
                                ElseIf TypeOf methodAttribute.Attributes(0).Name Is CSS.IdentifierNameSyntax Then
                                    Dim identifierName As CSS.IdentifierNameSyntax = CType(methodAttribute.Attributes(0).Name, CSS.IdentifierNameSyntax)
                                    If identifierName.Identifier.ValueText.EndsWith("EnumeratorCancellationAttribute", StringComparison.OrdinalIgnoreCase) Then
                                        needCancelToken = True
                                        Exit For
                                    End If
                                End If
                            End If
                        Next
                        If needCancelToken Then
                            For Each parameter As CSS.ParameterSyntax In methodStatement.ParameterList.Parameters
                                If parameter.Type.ToString.EndsWith("CancellationToken", StringComparison.Ordinal) Then
                                    cancelExpression = Factory.ParseExpression(parameter.Identifier.ValueText)
                                    Exit For
                                End If
                            Next
                        End If
                    End If
                End If
                Dim memberAccessExpression As ExpressionSyntax = Factory.MemberAccessExpression(VB.SyntaxKind.SimpleMemberAccessExpression,
                                                                                                  expression.WithoutTrailingTrivia,
                                                                                                  DotToken,
                                                                                                  Factory.IdentifierName("GetAsyncEnumerator"))
                Dim argumentList As ArgumentListSyntax = Factory.ArgumentList(Factory.SingletonSeparatedList(Of ArgumentSyntax)(Factory.SimpleArgument(cancelExpression)))
                Dim invocationExpression As InvocationExpressionSyntax = Factory.InvocationExpression(memberAccessExpression, argumentList)
                Dim initializer As EqualsValueSyntax = Factory.EqualsValue(invocationExpression)
                Dim asyncBlock As New List(Of StatementSyntax) From {
                    FactoryDimStatement(messageEnumerator, asClause:=Nothing, initializer)
                }
                '    Try
                '    {
                Dim tryStatement As TryStatementSyntax = Factory.TryStatement()
                '        While (await messageEnumerator.MoveNextAsync())
                '        {
                Dim moveNextExpression As ExpressionSyntax = Factory.QualifiedName(Factory.IdentifierName(messageEnumerator),
                                                                                     DotToken,
                                                                                     Factory.IdentifierName("MoveNextAsync"))
                Dim whileStatement As WhileStatementSyntax = Factory.WhileStatement(Factory.AwaitExpression(moveNextExpression))
                '            var message = messageEnumerator.Current;
                Dim whileStatements As New List(Of StatementSyntax) From {
                    FactoryDimStatement(forEachVariableToken,
                                   asClause:=Nothing,
                                   Factory.EqualsValue(Factory.QualifiedName(Factory.IdentifierName(messageEnumerator),
                                                       DotToken,
                                                       Factory.IdentifierName("Current"))
                                                       )
                                  )
                }
                whileStatements.AddRange(innerStatements)

                '    Finally
                '    {
                '        If (messageEnumerator!= null) Then
                Dim condition As ExpressionSyntax = Factory.IsNotExpression(Factory.IdentifierName(messageEnumerator),
                                                                              NothingExpression)
                Dim ifStatement As IfStatementSyntax = Factory.IfStatement(IfKeyword, condition, ThenKeyword)
                '                                {
                '            await messageEnumerator.DisposeAsync();
                Dim disposeAsyncExpression As ExpressionSyntax = Factory.QualifiedName(Factory.IdentifierName(messageEnumerator),
                                                                                         DotToken,
                                                                                         Factory.IdentifierName("DisposeAsync"))
                Dim awaitStatement As StatementSyntax = Factory.ExpressionStatement(Factory.AwaitExpression(disposeAsyncExpression))
                '        }
                '    }
                Dim finallyStatements As SyntaxList(Of StatementSyntax) =
                    Factory.SingletonList(Of StatementSyntax)(Factory.MultiLineIfBlock(ifStatement,
                                                                                           Factory.SingletonList(awaitStatement),
                                                                                           elseBlock:=Nothing,
                                                                                           elseIfBlocks:=Nothing))
                Dim finallyBlock As FinallyBlockSyntax = Factory.FinallyBlock(finallyStatements)
                Dim whileBlock As WhileBlockSyntax = Factory.WhileBlock(whileStatement, Factory.List(whileStatements))
                Dim tryBlock As TryBlockSyntax = Factory.TryBlock(tryStatement,
                                                                    Factory.SingletonList(Of StatementSyntax)(whileBlock),
                                                                    catchBlocks:=Nothing,
                                                                    finallyBlock,
                                                                    Factory.EndTryStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), TryKeyword)).WithTrailingEol
                asyncBlock.Add(tryBlock)
                Return ReplaceStatementsWithMarkedStatements(node,
                                                             Factory.List(asyncBlock))
            End Function

            Public Overrides Function VisitForEachVariableStatement(node As CSS.ForEachVariableStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim tupleExpression As CSS.TupleExpressionSyntax = TryCast(node.Variable, CSS.TupleExpressionSyntax)
                If tupleExpression IsNot Nothing Then
                    Dim forEachVariableToken As SyntaxToken = Factory.Identifier("tempTuple")
                    Dim variableIdentifier As IdentifierNameSyntax = Factory.IdentifierName(forEachVariableToken)

                    Dim nodes As New List(Of TupleElementSyntax)
                    Dim declarators As SeparatedSyntaxList(Of VariableDeclaratorSyntax)
                    Dim identifier As IdentifierNameSyntax
                    For Each e As IndexClass(Of CSS.ArgumentSyntax) In tupleExpression.Arguments.WithIndex
                        Dim arg As CSS.ArgumentSyntax = e.Value
                        If TypeOf arg.Expression Is CSS.DeclarationExpressionSyntax Then
                            Dim declarationExpression As CSS.DeclarationExpressionSyntax = CType(arg.Expression, CSS.DeclarationExpressionSyntax)
                            Dim typeName As TypeSyntax = CType(declarationExpression.Type.Accept(_nodesVisitor), TypeSyntax)
                            identifier = CType(declarationExpression.Designation.Accept(_nodesVisitor), IdentifierNameSyntax)
                            nodes.Add(Factory.NamedTupleElement(identifier.Identifier, Factory.SimpleAsClause(typeName)))
                            Dim value As ExpressionSyntax = Factory.SimpleMemberAccessExpression(variableIdentifier, DotToken, identifier)
                            Dim initializer As EqualsValueSyntax = Factory.EqualsValue(value)
                            Dim elementDeclarator As VariableDeclaratorSyntax = Factory.VariableDeclarator(Factory.SingletonSeparatedList(Factory.ModifiedIdentifier(identifier.Identifier)),
                                                                                                           Factory.SimpleAsClause(typeName),
                                                                                                           initializer)
                            declarators = declarators.Add(elementDeclarator)
                        Else
                            identifier = Factory.IdentifierName($"_{e.index + 1}")
                            nodes.Add(Factory.NamedTupleElement(identifier.Identifier, Factory.SimpleAsClause(PredefinedTypeObject)))
                        End If
                    Next
                    Dim nodeStatementList As List(Of (Integer, StatementSyntax)) = node.GetStatementsForNode(VB.SyntaxKind.LocalDeclarationStatement)
                    For Each nodeStatement As (Integer, StatementSyntax) In nodeStatementList
                        RemoveMarkedStatement(node, nodeStatement.Item2)
                    Next

                    Dim forEachVariable As TupleTypeSyntax = Factory.TupleType(Factory.SeparatedList(nodes))
                    Dim variableDeclarator As VariableDeclaratorSyntax = Factory.VariableDeclarator(Factory.SingletonSeparatedList(Factory.ModifiedIdentifier(forEachVariableToken).WithTrailingTrivia(SpaceTrivia)),
                                                                                                    Factory.SimpleAsClause(forEachVariable),
                                                                                                    initializer:=Nothing)
                    Dim forEachStatement As ForEachStatementSyntax = Factory.ForEachStatement(variableDeclarator,
                                                                                              CType(node.Expression.Accept(_nodesVisitor), ExpressionSyntax)).WithTrailingEol
                    Dim openBraceLeadingTrivia As New SyntaxTriviaList
                    Dim closingBraceTrailingTrivia As New SyntaxTriviaList
                    Dim innerStatements As SyntaxList(Of StatementSyntax) = Me.ConvertBlock(node.Statement, openBraceLeadingTrivia, closingBraceTrailingTrivia)

                    Dim tupleDimStatement As StatementSyntax = Factory.LocalDeclarationStatement(DimModifier, declarators).WithTrailingEol
                    innerStatements = innerStatements.Insert(0, tupleDimStatement)
                    Dim block As ForEachBlockSyntax = Factory.ForEachBlock(forEachStatement.WithConvertedLeadingTriviaFrom(node.ForEachKeyword),
                                                                           innerStatements,
                                                                           Factory.NextStatement().WithLeadingTrivia(closingBraceTrailingTrivia).WithTrailingEol())
                    Return ReplaceOneStatementWithMarkedStatements(node, block)
                End If

                Return Factory.SingletonList(Of StatementSyntax)(FlagUnsupportedStatements(node,
                                                                                                 "For Each Variable statement",
                                                                                                 commentOutOriginalStatements:=True))
            End Function

            Public Overrides Function VisitForStatement(node As CSS.ForStatementSyntax) As SyntaxList(Of StatementSyntax)
                '   ForStatement -> ForNextStatement when for-loop is simple

                ' only the following forms of the for-statement are allowed:
                ' for (TypeReference name = start; name < oneAfterEnd; name += step)
                ' for (name = start; name < oneAfterEnd; name += step)
                ' for (TypeReference name = start; name <= end; name += step)
                ' for (name = start; name <= end; name += step)
                ' for (TypeReference name = start; name > oneAfterEnd; name -= step)
                ' for (name = start; name > oneAfterEnd; name -= step)
                ' for (TypeReference name = start; name >= end; name -= step)
                ' for (name = start; name >= end; name -= step)
                Dim block As StatementSyntax = Nothing

                ' check if the form Is valid And collect TypeReference, name, start, end And step
                Dim hasVariable As Boolean = False
                If Me.ConvertForToSimpleForNext(node, block, hasVariable) Then
                    Return ReplaceOneStatementWithMarkedStatements(node, block)
                Else
                    Dim openBraceLeadingTrivia As New SyntaxTriviaList
                    Dim closingBraceTrailingTrivia As New SyntaxTriviaList
                    Dim statements As SyntaxList(Of StatementSyntax) = Me.ConvertBlock(node.Statement, openBraceLeadingTrivia, closingBraceTrailingTrivia).AddRange(node.Incrementors.Select(AddressOf Me.ConvertSingleBlock))

                    Dim trailingTrivia As SyntaxTriviaList = node.SecondSemicolonToken.CollectConvertedTokenTrivia(getLeading:=True, getTrailing:=True)
                    trailingTrivia = trailingTrivia.AddRange(node.CloseParenToken.CollectConvertedTokenTrivia(getLeading:=True, getTrailing:=True))
                    trailingTrivia = trailingTrivia.AddRange(node.GetBraces.Item1.CollectConvertedTokenTrivia(getLeading:=True, getTrailing:=True))
                    Dim condition As ExpressionSyntax = If(node.Condition Is Nothing, Factory.TrueLiteralExpression(TrueKeyword).WithTrailingTrivia(trailingTrivia), DirectCast(node.Condition.Accept(_nodesVisitor), ExpressionSyntax))

                    Dim whileStmt As WhileStatementSyntax = Factory.WhileStatement(WhileKeyword.WithConvertedLeadingTriviaFrom(node.ForKeyword),
                                                                                                condition
                                                                                                ).WithTrailingEol
                    whileStmt = CType(PrependStatementWithMarkedStatementTrivia(node, whileStmt), WhileStatementSyntax)
                    Dim endWhileStmt As EndBlockStatementSyntax = Factory.EndWhileStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), WhileKeyword).WithLeadingTrivia(closingBraceTrailingTrivia).WithConvertedTrailingTriviaFrom(node.GetBraces.Item2).WithTrailingEol
                    block = Factory.WhileBlock(whileStmt, statements, endWhileStmt)
                    Dim stmtList As SyntaxList(Of StatementSyntax) = Factory.List(node.Initializers.Select(AddressOf Me.ConvertSingleBlock)).Add(block)
                    If hasVariable Then
                        stmtList = stmtList.Insert(0, node.Declaration.Accept(Me).First.WithConvertedTrailingTriviaFrom(node.FirstSemicolonToken).WithTrailingEol)
                    End If
                    Return ReplaceStatementsWithMarkedStatements(node, stmtList)
                End If

            End Function

            Public Overrides Function VisitGotoStatement(node As CSS.GotoStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim labelNameToken As LabelSyntax
                If node.IsKind(CS.SyntaxKind.GotoCaseStatement, CS.SyntaxKind.GotoDefaultStatement) Then
                    If _blockInfo.Count = 0 Then Throw New InvalidOperationException("GoTo Case/GoTo Default outside switch Is illegal!")
                    Dim labelExpression As VB.VisualBasicSyntaxNode = If(node.Expression?.Accept(_nodesVisitor), Factory.ElseCaseClause())
                    _blockInfo.Peek().s_gotoCaseExpressions.Add(labelExpression)
                    labelNameToken = Factory.Label(VB.SyntaxKind.IdentifierLabel, Me.MakeGotoSwitchLabel(labelExpression))
                Else
                    labelNameToken = Factory.Label(VB.SyntaxKind.IdentifierLabel, GenerateSafeVbToken(DirectCast(node.Expression, CSS.IdentifierNameSyntax).Identifier, node, _semanticModel, _nodesVisitor._usedIdentifiers))
                End If

                Return Factory.SingletonList(Of StatementSyntax)(Factory.GoToStatement(labelNameToken).WithConvertedTriviaFrom(node).WithTrailingEol)
            End Function

            Public Overrides Function VisitIfStatement(node As CSS.IfStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim stmt As StatementSyntax = Nothing
                Dim savedNeedEndUsingCount As Integer = _nodesVisitor.NeededEndUsingCount
                Try
                    _nodesVisitor.NeededEndUsingCount = 0
                    If node.Else Is Nothing AndAlso Me.TryConvertIfNotNullRaiseEvent(node, stmt) Then
                        Return Factory.SingletonList(stmt)
                    End If

                    Dim listOfElseIfBlocks As New List(Of ElseIfBlockSyntax)()
                    Dim elseBlock As ElseBlockSyntax = Nothing
                    Dim openBraceLeadingTrivia As New SyntaxTriviaList
                    Dim closeBraceTrailingTrivia As New SyntaxTriviaList
                    Me.CollectElseBlocks(node, listOfElseIfBlocks, elseBlock, openBraceLeadingTrivia, closeBraceTrailingTrivia)

                    Dim openParenToken As SyntaxToken = node.OpenParenToken

                    Dim ifKeywordWithTrivia As SyntaxToken = IfKeyword.WithTrailingTrivia(SpaceTrivia).
                                                WithConvertedLeadingTriviaFrom(node.IfKeyword).
                                                WithAppendedTrailingTrivia(openParenToken.LeadingTrivia.ConvertTriviaList()).
                                                WithAppendedTrailingTrivia(openParenToken.TrailingTrivia.ConvertTriviaList())
                    ifKeywordWithTrivia = ifKeywordWithTrivia.WithModifiedTokenTrivia(leadingToken:=True, afterEol:=False, requireTrailingSpace:=True, finalLeadingDirectiveNotAllowed:=False)

                    Dim statementTrailingTrivia As SyntaxTriviaList
                    statementTrailingTrivia = node.CloseParenToken.CollectConvertedTokenTrivia(getLeading:=True, getTrailing:=True)
                    If statementTrailingTrivia.Any AndAlso Not statementTrailingTrivia(0).IsEndOfLine Then
                        statementTrailingTrivia = statementTrailingTrivia.Insert(0, VbEolTrivia)
                    End If
                    Dim conditionWithTrivia As ExpressionSyntax = DirectCast(node.Condition.Accept(_nodesVisitor), ExpressionSyntax).AdjustNodeTrivia(SeparatorFollows:=True)

                    If node.Statement.IsKind(CS.SyntaxKind.EmptyStatement) Then
                        statementTrailingTrivia = statementTrailingTrivia.InsertRange(0, DirectCast(node.Statement, CSS.EmptyStatementSyntax).SemicolonToken.TrailingTrivia.ConvertTriviaList())
                    End If
                    Dim ifStatement As IfStatementSyntax = Factory.IfStatement(ifKeywordWithTrivia.WithLeadingTrivia(),
                                                                                conditionWithTrivia,
                                                                                ThenKeyword
                                                                               ).With(ifKeywordWithTrivia.LeadingTrivia, statementTrailingTrivia).
                                                                               WithTrailingEol

                    Dim braces As (openBrace As SyntaxToken, closeBrace As SyntaxToken) = node.Statement.GetBraces
                    Dim closeBrace As SyntaxToken = braces.closeBrace
                    Dim endIfStatement As EndBlockStatementSyntax = Factory.EndIfStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), IfKeyword).WithConvertedTriviaFrom(closeBrace).WithTrailingEol
                    Dim elseIfBlocks As SyntaxList(Of ElseIfBlockSyntax) = Factory.List(listOfElseIfBlocks)
                    If elseBlock IsNot Nothing AndAlso elseBlock.Statements.Any AndAlso elseBlock.Statements(0).IsKind(VB.SyntaxKind.EmptyStatement) Then
                        endIfStatement = endIfStatement.WithLeadingTrivia(elseBlock.GetTrailingTrivia)
                        elseBlock = Factory.ElseBlock(Factory.ElseStatement(), statements:=Nothing)
                    End If
                    Dim statements As SyntaxList(Of StatementSyntax) = Me.ConvertBlock(node.Statement, openBraceLeadingTrivia, closeBraceTrailingTrivia)
                    If closeBraceTrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                        endIfStatement = endIfStatement.WithLeadingTrivia(closeBraceTrailingTrivia)
                    End If
                    If statements.LastOrDefault.IsKind(VB.SyntaxKind.EmptyStatement) Then
                        If EndsWithSimilarTrivia(endIfStatement.GetLeadingTrivia, statements.LastOrDefault.GetLeadingTrivia) Then
                            statements = statements.RemoveAt(statements.Count - 1)
                        End If
                    End If
                    If TypeOf node.Statement Is CSS.BlockSyntax Then
                        statements = _nodesVisitor.AdjustUsingIfNeeded(statements)
                        stmt = Factory.MultiLineIfBlock(ifStatement,
                                                        statements,
                                                        elseIfBlocks,
                                                        elseBlock,
                                                        endIfStatement.WithTrailingEol
                                                       )
                    Else
                        Dim isInvocationExpression As Boolean = False
                        If node.Statement.IsKind(CS.SyntaxKind.ExpressionStatement) Then
                            Dim exprStmt As CSS.ExpressionStatementSyntax = DirectCast(node.Statement, CSS.ExpressionStatementSyntax)
                            If exprStmt.Expression.IsKind(CS.SyntaxKind.InvocationExpression) Then
                                isInvocationExpression = exprStmt.Expression.DescendantNodes().OfType(Of CSS.ConditionalExpressionSyntax).Any
                            End If
                        End If
                        statements = _nodesVisitor.AdjustUsingIfNeeded(statements)
                        If listOfElseIfBlocks.Any() OrElse isInvocationExpression OrElse Not node.Statement.IsSimpleStatement Then
                            stmt = Factory.MultiLineIfBlock(ifStatement,
                                                            statements,
                                                            elseIfBlocks,
                                                            elseBlock,
                                                            endIfStatement.WithTrailingEol
                                                            )
                        Else
                            If ifStatement.GetTrailingTrivia.ContainsEolTrivia Then
                                Dim ifBlockStatements As SyntaxList(Of StatementSyntax) = statements
                                stmt = Factory.MultiLineIfBlock(ifStatement,
                                                                ifBlockStatements,
                                                                elseIfBlocks,
                                                                elseBlock,
                                                                endIfStatement.WithTrailingEol
                                                                )
                            Else
                                If elseBlock IsNot Nothing OrElse (statements.Count = 1 AndAlso TypeOf statements(0) Is EmptyStatementSyntax) Then
                                    stmt = Factory.MultiLineIfBlock(ifStatement,
                                                                    statements,
                                                                    elseIfBlocks:=Nothing,
                                                                    elseBlock,
                                                                    endIfStatement.WithTrailingEol
                                                                    )
                                Else
                                    stmt = Factory.SingleLineIfStatement(ifKeywordWithTrivia,
                                                                         conditionWithTrivia,
                                                                         ThenKeyword,
                                                                         statements,
                                                                         elseClause:=Nothing
                                                                         )
                                End If
                            End If
                        End If
                    End If
                Catch ex As Exception
                    Throw
                Finally
                    _nodesVisitor.NeededEndUsingCount = savedNeedEndUsingCount
                End Try

                Return ReplaceOneStatementWithMarkedStatements(node, stmt)
            End Function

            Public Overrides Function VisitLabeledStatement(node As CSS.LabeledStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim openBraceLeadingTrivia As New SyntaxTriviaList
                Dim closingBraceTrailingTrivia As New SyntaxTriviaList
                Dim statements As SyntaxList(Of StatementSyntax) = Me.ConvertBlock(node.Statement, openBraceLeadingTrivia, closingBraceTrailingTrivia)
                If openBraceLeadingTrivia.Any OrElse closingBraceTrailingTrivia.Any Then
                    Stop
                End If
                Return Factory.SingletonList(Of StatementSyntax)(Factory.LabelStatement(GenerateSafeVbToken(node.Identifier, node, _semanticModel, _nodesVisitor._usedIdentifiers))).AddRange(statements)
            End Function

            Public Overrides Function VisitLocalDeclarationStatement(node As CSS.LocalDeclarationStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, _nodesVisitor.IsModule, TokenContext.Local).ToList
                If modifiers.Count = 0 Then
                    modifiers.Add(DimKeyword.WithTrailingTrivia(SpaceTrivia))
                End If
                Dim leadingTrivia As New SyntaxTriviaList
                Dim declarators As SeparatedSyntaxList(Of VariableDeclaratorSyntax) = node.Declaration.RemodelVariableDeclaration(_nodesVisitor, _semanticModel, isFieldDeclaration:=False, leadingTrivia)
                Dim localDeclStmt As StatementSyntax
                If node.UsingKeyword.IsKind(CS.SyntaxKind.UsingKeyword) Then
                    localDeclStmt = Factory.UsingStatement(Nothing, declarators).WithLeadingTrivia(leadingTrivia).WithTrailingEol
                    _nodesVisitor.NeededEndUsingCount += 1
                Else
                    localDeclStmt = Factory.LocalDeclarationStatement(Factory.TokenList(modifiers),
                                                                      declarators
                                                                     ).WithLeadingTrivia(leadingTrivia).
                                                                      WithAppendedTrailingTrivia(node.SemicolonToken.TrailingTrivia.ConvertTriviaList()).WithTrailingEol ' this picks up end of line comments
                End If
                ' Don't repeat leading comments
                If Not localDeclStmt.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    localDeclStmt = localDeclStmt.WithConvertedLeadingTriviaFrom(node)
                End If

                Dim stmtList As New List(Of StatementSyntax) From {
                    localDeclStmt
                }
                If node.SemicolonToken.HasLeadingTrivia And node.SemicolonToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    stmtList.Add(Factory.EmptyStatement.WithConvertedLeadingTriviaFrom(node.SemicolonToken))
                End If

                Return ReplaceStatementsWithMarkedStatements(node, stmtList)
            End Function

            Public Overrides Function VisitLocalFunctionStatement(node As CSS.LocalFunctionStatementSyntax) As SyntaxList(Of StatementSyntax)
                If node.AncestorsAndSelf().OfType(Of CSS.LocalFunctionStatementSyntax).Count > 1 Then
                    Return Factory.SingletonList(Of StatementSyntax)(Factory.EmptyStatement)
                End If
                Dim localFunctionSymbol As IMethodSymbol = CType(_semanticModel.GetDeclaredSymbol(node), IMethodSymbol)
                Dim indexOfFirstReferencingStatement As Integer
                Dim stmtWithIssues As CSS.StatementSyntax = Nothing
                If TypeOf node.Parent Is CSS.BlockSyntax Then
                    Dim parent As CSS.BlockSyntax = CType(node.Parent, CSS.BlockSyntax)
                    indexOfFirstReferencingStatement = parent.Statements.TakeWhile(Function(s As CSS.StatementSyntax) Not ContainsLocalFunctionReference(s, localFunctionSymbol, _semanticModel)).Count()
                    If indexOfFirstReferencingStatement = parent.Statements.Count Then
                        indexOfFirstReferencingStatement = 0
                    Else
                        For Each e As IndexClass(Of CSS.StatementSyntax) In parent.Statements.WithIndex
                            If TypeOf e.Value Is CSS.ReturnStatementSyntax Then
                                If indexOfFirstReferencingStatement > e.index Then
                                    indexOfFirstReferencingStatement = e.index
                                End If
                                Exit For
                            End If
                        Next
                    End If
                    stmtWithIssues = parent.Statements(indexOfFirstReferencingStatement)
                ElseIf TypeOf node.Parent Is CSS.SwitchSectionSyntax Then
                    Dim parent As CSS.SwitchSectionSyntax = CType(node.Parent, CSS.SwitchSectionSyntax)
                    indexOfFirstReferencingStatement = parent.Statements.TakeWhile(Function(s As CSS.StatementSyntax) Not ContainsLocalFunctionReference(s, localFunctionSymbol, _semanticModel)).Count()
                    If indexOfFirstReferencingStatement >= parent.Statements.Count Then
                        stmtWithIssues = CType(parent.Parent, CSS.StatementSyntax)
                    Else
                        stmtWithIssues = parent.Statements(indexOfFirstReferencingStatement)
                    End If
                ElseIf TypeOf node.Parent Is CSS.GlobalStatementSyntax Then
                    stmtWithIssues = node
                End If

                Dim parameters As SeparatedSyntaxList(Of CSS.ParameterSyntax) = node.ParameterList.Parameters
                Dim vbParameters As New SeparatedSyntaxList(Of ParameterSyntax)
                If parameters.Any Then
                    For index As Integer = 0 To parameters.Count - 1
                        vbParameters = vbParameters.Add(DirectCast(parameters(index).Accept(_nodesVisitor), ParameterSyntax))
                    Next
                End If
                Dim parameterList As ParameterListSyntax = Factory.ParameterList(openParenToken, vbParameters, CloseParenToken)
                Dim iSReturnVoid As Boolean = node.ReturnType Is Nothing OrElse node.ReturnType.ToString = "void"
                Dim lambdaHeader As LambdaHeaderSyntax
                Dim kind As VB.SyntaxKind
                Dim endBlock As EndBlockStatementSyntax
                Dim csBraces As (openBrace As SyntaxToken, closeBrace As SyntaxToken) = node.Body.GetBraces
                Dim asClause As SimpleAsClauseSyntax

                Dim typeList As New List(Of TypeSyntax)
                For Each parameter As ParameterSyntax In parameterList.Parameters
                    typeList.Add(parameter.AsClause.Type)
                Next
                Dim returnType As TypeSyntax = Nothing
                Dim modifiers As SyntaxTokenList = Factory.TokenList(ConvertModifiers(node.Modifiers, _nodesVisitor.IsModule, TokenContext.LocalFunction))
                If node.DescendantNodes().OfType(Of CSS.YieldStatementSyntax).Any Then
                    modifiers = modifiers.Add(IteratorKeyword)
                End If

                If iSReturnVoid Then
                    kind = VB.SyntaxKind.MultiLineSubLambdaExpression
                    lambdaHeader = Factory.SubLambdaHeader(attributeLists:=Nothing, modifiers, parameterList, asClause:=Nothing)
                    endBlock = Factory.EndSubStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), SubKeyword).WithConvertedTriviaFrom(csBraces.closeBrace).WithTrailingEol
                Else
                    returnType = DirectCast(node.ReturnType.Accept(_nodesVisitor), TypeSyntax).WithLeadingTrivia(SpaceTrivia)
                    typeList.Add(returnType)
                    kind = VB.SyntaxKind.MultiLineSubLambdaExpression
                    lambdaHeader = Factory.FunctionLambdaHeader(attributeLists:=Nothing, modifiers, parameterList, Factory.SimpleAsClause(returnType))
                    endBlock = Factory.EndFunctionStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), FunctionKeyword).WithConvertedTriviaFrom(csBraces.closeBrace)
                End If
                Dim nameToken As SyntaxToken = GenerateSafeVbToken(node.Identifier, node, _semanticModel, _nodesVisitor._usedIdentifiers)
                Dim body As New SyntaxList(Of StatementSyntax)
                If node.Body IsNot Nothing Then
                    body = ReplaceStatementsWithMarkedStatements(node, node.Body.Accept(Me))
                ElseIf node.ExpressionBody IsNot Nothing Then
                    If node.ExpressionBody.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        body = node.ExpressionBody.WithoutLeadingTrivia.GetExpressionBodyStatements(iSReturnVoid, _nodesVisitor)
                    Else
                        body = node.ExpressionBody.GetExpressionBodyStatements(iSReturnVoid, _nodesVisitor)
                    End If
                ElseIf node.AttributeLists.Count > 0 Then
                    Dim returnAttributes As New SyntaxList(Of AttributeListSyntax)
                    Dim functionStmtTrailingTrivia As SyntaxTriviaList
                    Dim attributes As List(Of AttributeListSyntax) = _nodesVisitor.ConvertAndSplitAttributes(node.AttributeLists, returnAttributes, functionStmtTrailingTrivia)
                    If TypeOf node Is CSS.LocalFunctionStatementSyntax Then
                        Dim dllMethodStatement As MethodStatementSyntax = Factory.FunctionStatement(Factory.List(attributes), modifiers, nameToken, Nothing, parameterList, Factory.SimpleAsClause(returnType), Nothing, Nothing)
                        Dim dllDecl As MethodBlockSyntax = Factory.MethodBlock(VB.SyntaxKind.FunctionBlock, dllMethodStatement, Factory.EndFunctionStatement.WithAppendedEol)
                        Dim classWithIssues As CSS.ClassDeclarationSyntax = node.GetAncestor(Of CSS.ClassDeclarationSyntax)()

                        classWithIssues.AddMarker(dllDecl, StatementHandlingOption.AddMethod, True)
                        Return Factory.SingletonList(Of StatementSyntax)(Factory.EmptyStatement())
                    Else
                        Throw UnreachableException()
                    End If
                End If
                If TypeOf node.Parent Is CSS.GlobalStatementSyntax Then
                    Return body
                End If
                If typeList.Any Then
                    Dim typeArguments As TypeArgumentListSyntax = FactoryTypeArgumentList(typeList)
                    Dim genericName As TypeSyntax = Factory.GenericName(Factory.Identifier(If(iSReturnVoid, "Action", "Func")), typeArguments)
                    asClause = Factory.SimpleAsClause(genericName)
                Else
                    asClause = Factory.SimpleAsClause(Factory.IdentifierName("Action"))
                End If
                If node.ExpressionBody?.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    body = body.Replace(body(0), body(0).WithConvertedLeadingTriviaFrom(node.ExpressionBody))
                    Dim block As CSS.BlockSyntax = TryCast(node.Parent, CSS.BlockSyntax)
                    If block IsNot Nothing Then
                        If block.CloseBraceToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                            body = body.Add(Factory.EmptyStatement.WithConvertedLeadingTriviaFrom(block.CloseBraceToken))
                        End If
                    End If
                End If
                body = _nodesVisitor.AdjustUsingIfNeeded(body)
                Dim lambdaExpression As MultiLineLambdaExpressionSyntax = Factory.MultiLineLambdaExpression(
                                                            kind,
                                                            lambdaHeader.WithoutLeadingTrivia.WithTrailingEol,
                                                            body,
                                                            endBlock)
                Dim initializer As EqualsValueSyntax = Factory.EqualsValue(lambdaExpression)
                Dim dimStatement As LocalDeclarationStatementSyntax = FactoryDimStatement(nameToken, asClause, initializer).WithConvertedTrailingTriviaFrom(node).
                                        WithPrependedLeadingTrivia(Factory.CommentTrivia($"' TODO Check: Local function was replaced with Lambda"))
                If stmtWithIssues.Equals(node) Then
                    Return Factory.SingletonList(Of StatementSyntax)(dimStatement.WithConvertedTriviaFrom(node))
                End If
                Return Factory.SingletonList(Of StatementSyntax)(dimStatement)
            End Function

            Public Overrides Function VisitLockStatement(node As CSS.LockStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim lockStmt As SyncLockStatementSyntax = Factory.SyncLockStatement(DirectCast(node.Expression?.Accept(_nodesVisitor), ExpressionSyntax)).WithConvertedLeadingTriviaFrom(node)
                Dim csBraces As (openBrace As SyntaxToken, closeBrace As SyntaxToken)
                Dim openBraceLeadingTrivia As SyntaxTriviaList = csBraces.openBrace.CollectConvertedTokenTrivia(getLeading:=True, getTrailing:=True)
                Dim closingBraceTrailingTrivia As SyntaxTriviaList = csBraces.closeBrace.CollectConvertedTokenTrivia(getLeading:=True, getTrailing:=True)

                Dim statements As SyntaxList(Of StatementSyntax) = Me.ConvertBlock(node.Statement, openBraceLeadingTrivia, closingBraceTrailingTrivia)
                Dim endSyncLockStmt As EndBlockStatementSyntax = Factory.EndSyncLockStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), SyncLockKeyword).
                                                                                    WithLeadingTrivia(closingBraceTrailingTrivia).
                                                                                    WithAppendedTrailingTrivia(node.GetTrailingTrivia.ConvertTriviaList()).
                                                                                    WithTrailingEol
                Dim lockBlock As SyncLockBlockSyntax = Factory.SyncLockBlock(lockStmt.WithTrailingEol,
                                                                             statements,
                                                                             endSyncLockStmt)
                Return ReplaceOneStatementWithMarkedStatements(node, lockBlock)
            End Function

            Public Overrides Function VisitReturnStatement(node As CSS.ReturnStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim stmt As StatementSyntax
                Dim movedLeadingTrivia As New SyntaxTriviaList
                Dim expression As ExpressionSyntax
                If node.Expression Is Nothing Then
                    stmt = Factory.ReturnStatement.
                                        WithConvertedTriviaFrom(node).
                                        WithTrailingEol
                Else
                    expression = DirectCast(node.Expression.Accept(_nodesVisitor), ExpressionSyntax)
                    ' TODO Handle ref expressions
                    If expression IsNot Nothing Then
                        movedLeadingTrivia = movedLeadingTrivia.AddRange(node.GetLeadingTrivia.ConvertTriviaList())
                        If expression.HasLeadingTrivia AndAlso Not expression.GetLeadingTrivia.ContainsEndIfTrivia Then
                            movedLeadingTrivia = movedLeadingTrivia.AddRange(expression.GetLeadingTrivia)
                        Else
                            node.AddMarker(Factory.EmptyStatement.WithLeadingTrivia(expression.GetLeadingTrivia), StatementHandlingOption.AppendEmptyStatement, allowDuplicates:=False)
                        End If
                        expression = expression?.WithLeadingTrivia(SpaceTrivia)
                    End If
                    stmt = Factory.ReturnStatement(expression?.WithLeadingTrivia(SpaceTrivia)).
                                            WithLeadingTrivia(movedLeadingTrivia).
                                            WithTrailingTrivia(node.SemicolonToken.TrailingTrivia.ConvertTriviaList()).
                                            WithTrailingEol
                End If
                Return ReplaceOneStatementWithMarkedStatements(node, stmt)
            End Function

            Public Overrides Function VisitSwitchStatement(node As CSS.SwitchStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim stmt As StatementSyntax
                _blockInfo.Push(New BlockInfo())
                Try
                    Dim blocks As List(Of CaseBlockSyntax) = node.Sections.Select(AddressOf Me.ConvertSwitchSection).ToList
                    Dim orderedBlocks As New List(Of CaseBlockSyntax)
                    Dim caseElseIndex As Integer = -1
                    For blockIndex As Integer = 0 To blocks.Count - 1
                        If blocks(blockIndex).IsKind(VB.SyntaxKind.CaseElseBlock) Then
                            caseElseIndex = blockIndex
                        Else
                            orderedBlocks.Add(blocks(blockIndex))
                        End If
                    Next
                    If caseElseIndex >= 0 Then
                        orderedBlocks.Add(blocks(caseElseIndex))
                    End If
                    Dim expression As ExpressionSyntax = Nothing
                    Dim patternSwitch As CSS.CasePatternSwitchLabelSyntax = TryCast(node.Sections(0).Labels(0), CSS.CasePatternSwitchLabelSyntax)
                    If patternSwitch IsNot Nothing Then
                        If TypeOf patternSwitch.Pattern Is CSS.DeclarationPatternSyntax Then
                            expression = Factory.TrueLiteralExpression(TrueKeyword)
                        ElseIf TypeOf patternSwitch.Pattern Is CSS.VarPatternSyntax Then
                            ' TODO Handle
                        ElseIf TypeOf patternSwitch.Pattern Is CSS.ConstantPatternSyntax Then
                            expression = CType(patternSwitch.Pattern.Accept(_nodesVisitor), ExpressionSyntax)
                        ElseIf TypeOf patternSwitch.Pattern Is CSS.RecursivePatternSyntax Then
                            ' TODO Handle
                        Else
                            Stop
                        End If
                    End If
                    Dim endSelectStmt As EndBlockStatementSyntax = Factory.EndBlockStatement(
                                                                    VB.SyntaxKind.EndSelectStatement,
                                                                    EndKeyword.WithTrailingTrivia(SpaceTrivia),
                                                                    SelectKeyword).
                                                                        WithConvertedTriviaFrom(node.CloseBraceToken).
                                                                        WithTrailingEol
                    stmt = Factory.SelectBlock(
                                Factory.SelectStatement(SelectKeyword,
                                                            CaseKeyword,
                                                            If(expression, DirectCast(node.Expression.Accept(_nodesVisitor), ExpressionSyntax))
                                                            ).WithTrailingEol,
                                Factory.List(nodes:=Me.AddLabels(blocks:=orderedBlocks.ToArray,
                                               gotoLabels:=_blockInfo.Peek().s_gotoCaseExpressions)
                                               ),
                                endSelectStmt
                                )
                    _switchCount += 1
                Finally
                    _blockInfo.Pop()
                End Try
                Return ReplaceOneStatementWithMarkedStatements(node, stmt.WithConvertedLeadingTriviaFrom(node))
            End Function

            Public Overrides Function VisitThrowStatement(node As CSS.ThrowStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim stmt As StatementSyntax = If(node.Expression Is Nothing,
                            Factory.ThrowStatement().WithTrailingEol,
                            Factory.ThrowStatement(DirectCast(node.Expression.Accept(_nodesVisitor), ExpressionSyntax)))
                Return ReplaceStatementsWithMarkedStatements(node, Factory.SingletonList(stmt.WithConvertedTriviaFrom(node).WithTrailingEol))
            End Function

            Public Overrides Function VisitTryStatement(node As CSS.TryStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim openBraceLeadingTrivia As New SyntaxTriviaList
                Dim closingBraceTrailingTrivia As New SyntaxTriviaList
                Dim tryStmt As TryStatementSyntax = Factory.TryStatement.WithTrailingEol
                Dim catchBlocks As SyntaxList(Of CatchBlockSyntax) = Factory.List(node.Catches.IndexedSelect(AddressOf Me.ConvertCatchClause))
                Dim newTriviaList As New SyntaxTriviaList
                For blockIndex As Integer = 0 To catchBlocks.Count - 1
                    Dim catchBlock As CatchBlockSyntax = catchBlocks(blockIndex)
                    If catchBlock.Statements.Any AndAlso catchBlock.Statements(0).IsKind(VB.SyntaxKind.EmptyStatement) Then
                        Dim tempTriviaList As SyntaxTriviaList = catchBlock.Statements(0).GetTrailingTrivia
                        newTriviaList = newTriviaList.AddRange(catchBlock.GetBraces().Item2.LeadingTrivia)
                        catchBlocks.Replace(catchBlocks(blockIndex), Factory.CatchBlock(catchBlock.CatchStatement.WithLeadingTrivia(newTriviaList)))
                        newTriviaList = tempTriviaList
                    Else
                        catchBlocks = catchBlocks.Replace(catchBlock, catchBlock.WithLeadingTrivia(newTriviaList))
                        newTriviaList = New SyntaxTriviaList
                    End If
                Next
                If catchBlocks.Count - 1 >= 0 Then
                    catchBlocks = catchBlocks.Replace(catchBlocks(0), catchBlocks(0).WithConvertedTriviaFrom(node.Block.CloseBraceToken).WithTrailingEol)
                End If
                Dim finallyBlock As FinallyBlockSyntax = Nothing
                If node.Finally IsNot Nothing Then
                    Dim finallyStatements As SyntaxList(Of StatementSyntax) =
                        Me.ConvertBlock(node.Finally.Block,
                            openBraceLeadingTrivia,
                            closingBraceTrailingTrivia
                                        )
                    finallyBlock = Factory.FinallyBlock(Factory.FinallyStatement.WithLeadingTrivia(newTriviaList).WithTrailingEol, finallyStatements)
                    newTriviaList = New SyntaxTriviaList
                    If finallyBlock.Statements(0).IsKind(VB.SyntaxKind.EmptyStatement) Then
                        newTriviaList = newTriviaList.AddRange(finallyBlock.Statements(0).GetTrailingTrivia)
                        finallyBlock = finallyBlock.WithTrailingTrivia(VbEolTrivia)
                    End If
                End If
                Dim endTryStmt As EndBlockStatementSyntax = Factory.EndTryStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), TryKeyword).WithTrailingEol
                If node.Catches.Any Then
                    endTryStmt = endTryStmt.WithConvertedTriviaFrom(node.Catches.Last.Block.GetBraces.Item2)
                Else
                    endTryStmt = endTryStmt.WithLeadingTrivia(closingBraceTrailingTrivia)
                End If
                If newTriviaList.Any Then
                    endTryStmt = endTryStmt.WithLeadingTrivia(newTriviaList)
                End If
                Dim tryBlockStatements As SyntaxList(Of StatementSyntax) = Me.ConvertBlock(node.Block, openBraceLeadingTrivia, closingBraceTrailingTrivia)
                If closingBraceTrailingTrivia.Any Then
                    If finallyBlock IsNot Nothing Then
                        finallyBlock = finallyBlock.WithPrependedLeadingTrivia(closingBraceTrailingTrivia)
                    End If
                End If

                If catchBlocks.Any AndAlso EndsWithSimilarTrivia(tryBlockStatements.LastOrDefault.GetLeadingTrivia, catchBlocks.FirstOrDefault.GetLeadingTrivia) Then
                    If tryBlockStatements.LastOrDefault.IsKind(VB.SyntaxKind.EmptyStatement) Then
                        tryBlockStatements = tryBlockStatements.RemoveAt(tryBlockStatements.Count - 1)
                    End If
                End If

                Dim block As TryBlockSyntax = Factory.TryBlock(tryStmt,
                                                                tryBlockStatements,
                                                                catchBlocks,
                                                                finallyBlock,
                                                                endTryStmt
                                                                )
                Return ReplaceOneStatementWithMarkedStatements(node, block.WithConvertedTriviaFrom(node))
            End Function

            Public Overrides Function VisitUnsafeStatement(node As CSS.UnsafeStatementSyntax) As SyntaxList(Of StatementSyntax)
                Return Factory.SingletonList(Of StatementSyntax)(FlagUnsupportedStatements(node, "Unsafe statement", commentOutOriginalStatements:=True))
            End Function

            Public Overrides Function VisitUsingStatement(node As CSS.UsingStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim usingStmt As UsingStatementSyntax
                Dim openBraceLeadingTrivia As New SyntaxTriviaList
                Dim closingBraceTrailingTrivia As New SyntaxTriviaList
                Dim leadingTrivia As New SyntaxTriviaList
                If node.Declaration Is Nothing Then
                    Dim usingBlock As UsingBlockSyntax
                    leadingTrivia = leadingTrivia.AddRange(node.GetLeadingTrivia.ConvertTriviaList())
                    If node.Expression IsNot Nothing AndAlso node.Expression.IsKind(CS.SyntaxKind.ConditionalAccessExpression) Then
                        Dim csConditionalAccessExpr As CSS.ConditionalAccessExpressionSyntax = DirectCast(node.Expression, CSS.ConditionalAccessExpressionSyntax)
                        Dim vbConditionalAccessExpr As VB.VisualBasicSyntaxNode = csConditionalAccessExpr.Expression.Accept(_nodesVisitor)
                        Dim condition As BinaryExpressionSyntax = Factory.IsNotExpression(left:=CType(vbConditionalAccessExpr, ExpressionSyntax),
                                                                                                right:=NothingExpression)
                        Dim ifStatement As IfStatementSyntax = Factory.IfStatement(condition)
                        usingStmt = Factory.UsingStatement(Factory.ParseExpression($"{vbConditionalAccessExpr}{csConditionalAccessExpr.WhenNotNull.Accept(_nodesVisitor)}"), Factory.SeparatedList(Of VariableDeclaratorSyntax)())
                        usingBlock = Factory.UsingBlock(usingStmt.WithTrailingEol, Me.ConvertBlock(node.Statement, openBraceLeadingTrivia, closingBraceTrailingTrivia)).WithLeadingTrivia(leadingTrivia)
                        Dim ifStmtBlock As MultiLineIfBlockSyntax = Factory.MultiLineIfBlock(ifStatement, Factory.SingletonList(Of StatementSyntax)(usingBlock), elseIfBlocks:=Nothing, elseBlock:=Nothing).WithLeadingTrivia(leadingTrivia)
                        Return ReplaceOneStatementWithMarkedStatements(node, ifStmtBlock)
                    Else
                        usingStmt = Factory.UsingStatement(DirectCast(node.Expression?.Accept(_nodesVisitor), ExpressionSyntax), Factory.SeparatedList(Of VariableDeclaratorSyntax)())
                    End If
                Else
                    usingStmt = Factory.UsingStatement(expression:=Nothing, node.Declaration.RemodelVariableDeclaration(_nodesVisitor, _semanticModel, isFieldDeclaration:=False, leadingTrivia))
                End If

                Return ReplaceOneStatementWithMarkedStatements(node,
                                                               Factory.UsingBlock(usingStmt.WithTrailingEol,
                                                                                  Me.ConvertBlock(node.Statement,
                                                                                      openBraceLeadingTrivia,
                                                                                      closingBraceTrailingTrivia),
                                                                                  Factory.EndUsingStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia),
                                                                                                            UsingKeyword).WithConvertedTriviaFrom(node.Statement.GetBraces.Item2).
                                                                                                                                                WithTrailingEol()).WithLeadingTrivia(leadingTrivia))
            End Function

            Public Overrides Function VisitVariableDeclaration(node As CSS.VariableDeclarationSyntax) As SyntaxList(Of StatementSyntax)
                Dim leadingTrivia As New SyntaxTriviaList
                Dim vbType As TypeSyntax = DirectCast(node.Type.Accept(_nodesVisitor), TypeSyntax)
                If vbType.HasLeadingTrivia Then
                    leadingTrivia = leadingTrivia.AddRange(vbType.GetLeadingTrivia)
                    vbType = vbType.WithLeadingTrivia(SpaceTrivia)
                End If
                Dim collectedCommentTrivia As SyntaxTriviaList
                Dim declaratorsWithoutInitializers As New List(Of CSS.VariableDeclaratorSyntax)()
                Dim vbDeclarators As New SeparatedSyntaxList(Of VariableDeclaratorSyntax)
                For Each e As IndexClass(Of CSS.VariableDeclaratorSyntax) In node.Variables.WithIndex
                    Dim v As CSS.VariableDeclaratorSyntax = e.Value
                    If v.Initializer Is Nothing Then
                        declaratorsWithoutInitializers.Add(v.WithTrailingTrivia(collectedCommentTrivia))
                        Continue For
                    Else
                        Dim asClause As SimpleAsClauseSyntax = If(node.Type.IsVar OrElse node.Type.IsKind(CS.SyntaxKind.RefType), Nothing, Factory.SimpleAsClause(vbType))
                        Dim value As ExpressionSyntax = DirectCast(v.Initializer.Value.Accept(_nodesVisitor), ExpressionSyntax)
                        If value Is Nothing Then
                            value = Factory.IdentifierName("HandleRefExpression").WithConvertedTriviaFrom(v.Initializer.Value)
                        End If
                        If value.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                            leadingTrivia = leadingTrivia.AddRange(value.GetLeadingTrivia)
                        End If
                        Dim initializer As EqualsValueSyntax = Factory.EqualsValue(value.WithLeadingTrivia(SpaceTrivia))
                        Dim names As SeparatedSyntaxList(Of ModifiedIdentifierSyntax) = Factory.SingletonSeparatedList(CType(v.Accept(_nodesVisitor), ModifiedIdentifierSyntax))
                        Dim declarator As VariableDeclaratorSyntax = Factory.VariableDeclarator(names,
                                                                                              asClause,
                                                                                              initializer
                                                                                             )
                        If declarator.HasTrailingTrivia Then
                            Dim foundEol As Boolean = False
                            Dim nonCommentTrailingTrivia As New SyntaxTriviaList
                            For Each t As SyntaxTrivia In declarator.GetTrailingTrivia
                                Select Case t.RawKind
                                    Case VB.SyntaxKind.EndOfLineTrivia
                                        foundEol = True
                                    Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                                        collectedCommentTrivia = collectedCommentTrivia.Add(t)
                                    Case VB.SyntaxKind.WhitespaceTrivia
                                        collectedCommentTrivia = collectedCommentTrivia.Add(t)
                                        nonCommentTrailingTrivia = nonCommentTrailingTrivia.Add(t)
                                        'Case Else
                                        ' Directives are ignored but the results are converted. Disabled Text is deleted
                                End Select
                            Next
                            If foundEol Then
                                collectedCommentTrivia = collectedCommentTrivia.Add(VbEolTrivia)
                                declarator = declarator.WithTrailingTrivia(collectedCommentTrivia)
                                collectedCommentTrivia = New SyntaxTriviaList
                            Else
                                declarator = declarator.WithTrailingTrivia(nonCommentTrailingTrivia)
                            End If
                            If e.IsLast Then
                                If Not (declarator.HasTrailingTrivia _
                                        AndAlso declarator.GetTrailingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia)) Then
                                    declarator = declarator.WithAppendedEol
                                End If
                            End If
                        End If
                        vbDeclarators = vbDeclarators.Add(declarator)
                    End If
                Next
                If declaratorsWithoutInitializers.Any Then
                    Stop
                End If
                Dim localDeclStmt As LocalDeclarationStatementSyntax =
                    FactoryDimStatement(vbDeclarators).WithoutTrivia.WithLeadingTrivia(leadingTrivia).WithAppendedTrailingTrivia(node.GetTrailingTrivia.ConvertTriviaList()) ' this picks up end of line comments
                ' Don't repeat leading comments
                If Not localDeclStmt.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    localDeclStmt = localDeclStmt.WithConvertedLeadingTriviaFrom(node)
                End If
                Return Factory.SingletonList(Of StatementSyntax)(localDeclStmt)
            End Function

            Public Overrides Function VisitWhileStatement(node As CSS.WhileStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim condition As ExpressionSyntax = DirectCast(node.Condition.Accept(_nodesVisitor), ExpressionSyntax)
                Dim openBraceLeadingTrivia As New SyntaxTriviaList
                Dim closingBraceTrailingTrivia As New SyntaxTriviaList
                Dim whileStatements As SyntaxList(Of StatementSyntax) = Me.ConvertBlock(node.Statement, openBraceLeadingTrivia, closingBraceTrailingTrivia)
                If openBraceLeadingTrivia.Any Then
                    whileStatements = whileStatements.Replace(whileStatements.First, whileStatements.First.WithPrependedLeadingTrivia(openBraceLeadingTrivia))
                End If
                Dim endWhileStmt As EndBlockStatementSyntax = Factory.EndWhileStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), WhileKeyword).WithLeadingTrivia(closingBraceTrailingTrivia).WithTrailingEol
                Dim block As WhileBlockSyntax = Factory.WhileBlock(Factory.WhileStatement(condition).WithConvertedLeadingTriviaFrom(node.WhileKeyword).WithTrailingEol, whileStatements, endWhileStmt)
                Return ReplaceOneStatementWithMarkedStatements(node, block)
            End Function

            Public Overrides Function VisitYieldStatement(node As CSS.YieldStatementSyntax) As SyntaxList(Of StatementSyntax)
                Me.IsIterator = True
                Dim stmt As StatementSyntax
                If node.Expression Is Nothing Then
                    stmt = Factory.ReturnStatement.WithTrailingEol
                Else
                    stmt = Factory.YieldStatement(DirectCast(node.Expression.Accept(_nodesVisitor), ExpressionSyntax)).WithTrailingEol
                End If
                Return ReplaceOneStatementWithMarkedStatements(node, stmt.WithConvertedTriviaFrom(node))
            End Function

            Private Class BlockInfo
                Public ReadOnly s_gotoCaseExpressions As New List(Of VB.VisualBasicSyntaxNode)()
            End Class

        End Class

    End Class

End Namespace
