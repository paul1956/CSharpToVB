' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Diagnostics.CodeAnalysis
Imports System.Runtime.InteropServices
Imports System.Text
Imports CSharpToVBConverter.ToVisualBasic
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CSharp
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

Imports CS = Microsoft.CodeAnalysis.CSharp

Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory

Imports VB = Microsoft.CodeAnalysis.VisualBasic

Namespace CSharpToVBConverter.ToVisualBasic

    Partial Public NotInheritable Class CSharpConverter

        <Guid("5AF92C9F-AE9B-4D2B-9BCE-6FD041654FC7")>
        Friend Class MethodBodyVisitor
            Inherits CSharpSyntaxVisitor(Of SyntaxList(Of StatementSyntax))

            Private ReadOnly _blockInfo As Stack(Of BlockInfo) = New Stack(Of BlockInfo)()
            Private ReadOnly _literalExpression As ExpressionSyntax = Factory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, Factory.Literal(1))
            Private ReadOnly _nodesVisitor As NodesVisitor
            Private ReadOnly _semanticModel As SemanticModel

            ' currently only works with switch blocks
            Private _switchCount As Integer

            Friend Sub New(semanticModel As SemanticModel, nodesVisitor As NodesVisitor)
                _semanticModel = semanticModel
                _nodesVisitor = nodesVisitor
            End Sub

            Public Property IsInterator As Boolean

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

            Private Iterator Function AddLabels(blocks As CaseBlockSyntax(), gotoLabels As List(Of VisualBasicSyntaxNode)) As IEnumerable(Of CaseBlockSyntax)
                For Each _block As CaseBlockSyntax In blocks
                    Dim block As CaseBlockSyntax = _block
                    For Each caseClause As CaseClauseSyntax In block.CaseStatement.Cases
                        Dim expression As VisualBasicSyntaxNode = If(TypeOf caseClause Is ElseCaseClauseSyntax, DirectCast(caseClause, VisualBasicSyntaxNode), DirectCast(caseClause, SimpleCaseClauseSyntax).Value)
                        If gotoLabels.Any(Function(label As VisualBasicSyntaxNode) label.IsEquivalentTo(expression.WithoutTrivia)) Then
                            block = block.WithStatements(block.Statements.Insert(0, Factory.LabelStatement(Me.MakeGotoSwitchLabel(expression))))
                        End If
                    Next

                    Yield block
                Next
            End Function

            Private Sub CollectElseBlocks(node As CSS.IfStatementSyntax, elseIfBlocks As List(Of ElseIfBlockSyntax), ByRef elseBlock As ElseBlockSyntax, ByRef OpenBraceLeadingTrivia As SyntaxTriviaList, ByRef CloseBraceTrailingTrivia As SyntaxTriviaList)
                If node.Else Is Nothing Then
                    Exit Sub
                End If

                If TypeOf node.Else.Statement Is CSS.IfStatementSyntax Then
                    Dim [elseIf] As CSS.IfStatementSyntax = DirectCast(node.Else.Statement, CSS.IfStatementSyntax)
                    Dim elseIFKeywordWithTrivia As SyntaxToken = ElseIfKeyword.WithLeadingTrivia(ConvertTriviaList(node.Else.Statement.GetLeadingTrivia)).WithPrependedLeadingTrivia(ConvertTriviaList(node.Else.GetLeadingTrivia))
                    Dim newThenTrailingTrivia As New SyntaxTriviaList
                    Dim condition As ExpressionSyntax = DirectCast([elseIf].Condition.Accept(_nodesVisitor), ExpressionSyntax)
                    newThenTrailingTrivia = newThenTrailingTrivia.AddRange(condition.GetTrailingTrivia)
                    If node.CloseParenToken.HasLeadingTrivia AndAlso node.CloseParenToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        newThenTrailingTrivia = newThenTrailingTrivia.AddRange(node.CloseParenToken.LeadingTrivia.ConvertTriviaList())
                    End If
                    Dim thenKeywordWithTrivia As SyntaxToken = ThenKeyword.WithTrailingTrivia(newThenTrailingTrivia)
                    Dim elseIfStatement As ElseIfStatementSyntax = Factory.ElseIfStatement(elseIFKeywordWithTrivia, condition.WithTrailingTrivia(VBSpaceTrivia), thenKeywordWithTrivia)
                    Dim elseIfBlock As ElseIfBlockSyntax = Factory.ElseIfBlock(elseIfStatement.WithTrailingEOL(RemoveLastLineContinuation:=True), Me.ConvertBlock([elseIf].Statement, OpenBraceLeadingTrivia, CloseBraceTrailingTrivia))
                    elseIfBlocks.Add(elseIfBlock)
                    Me.CollectElseBlocks([elseIf], elseIfBlocks, elseBlock, OpenBraceLeadingTrivia, CloseBraceTrailingTrivia)
                Else
                    Dim statements As SyntaxList(Of StatementSyntax) = Me.ConvertBlock(node.Else.Statement, OpenBraceLeadingTrivia, CloseBraceTrailingTrivia)
                    Dim newTrailingTrivia As New SyntaxTriviaList
                    If node.Else.Statement.GetBraces.Item1.HasTrailingTrivia Then
                        newTrailingTrivia = newTrailingTrivia.AddRange(ConvertTriviaList(node.Else.Statement.GetBraces.Item1.TrailingTrivia))
                    End If
                    Dim elseStatement As ElseStatementSyntax = Factory.ElseStatement(ElseKeyword.WithConvertedLeadingTriviaFrom(node.Else.ElseKeyword)).WithTrailingTrivia(newTrailingTrivia)
                    If statements.Any AndAlso statements.LastOrDefault.GetTrailingTrivia.LastOrDefault.IsEndOfLine AndAlso OpenBraceLeadingTrivia.FirstOrDefault.IsEndOfLine Then
                        OpenBraceLeadingTrivia = OpenBraceLeadingTrivia.RemoveAt(0)
                    End If
                    elseBlock = Factory.ElseBlock(elseStatement, statements).WithPrependedLeadingTrivia(OpenBraceLeadingTrivia).WithAppendedTrailingTrivia(CloseBraceTrailingTrivia)
                    OpenBraceLeadingTrivia = Nothing
                    CloseBraceTrailingTrivia = Nothing
                End If
            End Sub

            Private Function ConvertBlock(node As CSS.StatementSyntax, ByRef openBraceLeadingTrivia As SyntaxTriviaList, ByRef closeBraceTrailingTrivia As SyntaxTriviaList) As SyntaxList(Of StatementSyntax)
                Dim csBraces As (OpenBrace As SyntaxToken, CloseBrace As SyntaxToken)
                Dim csOpenBrace As SyntaxToken
                Dim csCloseBrace As SyntaxToken

                csBraces = node.GetBraces
                csOpenBrace = If(csBraces.OpenBrace = Nothing, New SyntaxToken, csBraces.OpenBrace)
                csCloseBrace = If(csBraces.CloseBrace = Nothing, New SyntaxToken, csBraces.CloseBrace)

                If csOpenBrace.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    openBraceLeadingTrivia = ConvertTriviaList(csOpenBrace.LeadingTrivia)
                End If
                Dim openBraceTrailingTrivia As New SyntaxTriviaList
                If csOpenBrace.TrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                    openBraceTrailingTrivia = ConvertTriviaList(csOpenBrace.TrailingTrivia)
                End If

                Dim closeBraceLeadingTrivia As New SyntaxTriviaList
                If csCloseBrace.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    closeBraceLeadingTrivia = ConvertTriviaList(csCloseBrace.LeadingTrivia)
                End If
                If csCloseBrace.TrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                    closeBraceTrailingTrivia = ConvertTriviaList(csCloseBrace.TrailingTrivia)
                End If
                Try
                    Select Case True
                        Case TypeOf node Is CSS.BlockSyntax
                            Dim nodeBlock As CSS.BlockSyntax = DirectCast(node, CSS.BlockSyntax)
                            Dim vbBlock As New List(Of StatementSyntax)
                            For Each e As IndexClass(Of CSS.StatementSyntax) In nodeBlock.Statements.WithIndex
                                Dim Statements As List(Of StatementSyntax) = e.Value.Accept(Me).ToList
                                If e.IsFirst AndAlso Statements.Any Then
                                    Statements(0) = Statements(0).WithPrependedLeadingTrivia(openBraceTrailingTrivia)
                                End If
                                vbBlock.AddRange(Statements)
                            Next

                            If vbBlock.Count = 0 Then
                                vbBlock.Add(Factory.EmptyStatement.WithConvertedTriviaFrom(nodeBlock))
                            Else
                                If Not (vbBlock.First.IsKind(VB.SyntaxKind.EmptyStatement) OrElse
                                       (vbBlock.First.IsKind(VB.SyntaxKind.TryBlock) AndAlso vbBlock.First.GetLeadingTrivia.Count = 1)
                                        ) Then
                                    vbBlock.Item(0) = vbBlock(0).WithLeadingTrivia(ConvertTriviaList(nodeBlock.Statements(0).GetLeadingTrivia)).WithPrependedLeadingTrivia(ConvertTriviaList(node.GetLeadingTrivia)).RemoveExtraLeadingEOL
                                End If
                                If vbBlock.Item(vbBlock.Count - 1).IsKind(VB.SyntaxKind.EmptyStatement) Then
                                    vbBlock.Item(vbBlock.Count - 1) = vbBlock.Last.WithoutTrailingTrivia().WithTrailingEOL(RemoveLastLineContinuation:=True)
                                Else
                                    vbBlock.Item(vbBlock.Count - 1) = vbBlock.Last.WithTrailingTrivia(ConvertTriviaList(nodeBlock.Statements.Last.GetTrailingTrivia)).WithTrailingEOL(RemoveLastLineContinuation:=True)
                                End If
                                If closeBraceLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                    vbBlock.Add(Factory.EmptyStatement.WithLeadingTrivia(closeBraceLeadingTrivia))
                                End If
                            End If
                            Return Factory.List(vbBlock)
                        Case TypeOf node Is CSS.EmptyStatementSyntax
                            Return Factory.List(Of StatementSyntax)()
                    End Select
                Catch ex As Exception
                    Stop
                    Throw
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
                    Return Factory.CatchBlock(Factory.CatchStatement().WithTrailingTrivia(VBEOLTrivia).WithAppendedTrailingTrivia(closingBraceTrailingTrivia), vbStatements)
                End If
                If openBraceLeadingTrivia.ContainsCommentOrDirectiveTrivia OrElse closingBraceTrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                    If vbStatements.Any Then
                        vbStatements = vbStatements.Replace(vbStatements(0), vbStatements(0).WithPrependedLeadingTrivia(openBraceLeadingTrivia))
                        Dim laststatement As Integer = vbStatements.Count - 1
                        vbStatements = vbStatements.Replace(vbStatements(laststatement), vbStatements(laststatement).WithAppendedTrailingTrivia(closingBraceTrailingTrivia).WithTrailingEOL(RemoveLastLineContinuation:=True))
                    Else
                        vbStatements.Add(Factory.EmptyStatement.With(openBraceLeadingTrivia, closingBraceTrailingTrivia).WithTrailingEOL(RemoveLastLineContinuation:=True))
                    End If
                End If
                Dim type As TypeSyntax = DirectCast(catchClause.Declaration.Type.Accept(_nodesVisitor), TypeSyntax)
                Dim simpleTypeName As String
                simpleTypeName = If(TypeOf type Is QualifiedNameSyntax, DirectCast(type, QualifiedNameSyntax).Right.ToString(), type.ToString())
                Dim identifier As SyntaxToken = If(catchClause.Declaration.Identifier.IsKind(CS.SyntaxKind.None),
                                                        Factory.Identifier($"__unused{simpleTypeName}{index + 1}__"),
                                                        GenerateSafeVBToken(catchClause.Declaration.Identifier, catchClause, _semanticModel))
                Dim whenClause As CatchFilterClauseSyntax = If(catchClause.Filter Is Nothing, Nothing, Factory.CatchFilterClause(filter:=DirectCast(catchClause.Filter.FilterExpression.Accept(_nodesVisitor), ExpressionSyntax)))
                Dim catchStatement As CatchStatementSyntax = Factory.CatchStatement(
                                                                    Factory.IdentifierName(identifier),
                                                                    Factory.SimpleAsClause(type),
                                                                    whenClause).
                                                                    WithConvertedLeadingTriviaFrom(catchClause)
                If Not catchStatement.HasTrailingTrivia Then
                    catchStatement = catchStatement.WithTrailingTrivia(VBEOLTrivia)
                ElseIf catchStatement.GetTrailingTrivia.Last <> VBEOLTrivia Then
                    catchStatement = catchStatement.WithTrailingTrivia(VBEOLTrivia)
                End If
                Return Factory.CatchBlock(catchStatement, vbStatements)
            End Function

            Private Function ConvertForToSimpleForNext(node As CSS.ForStatementSyntax, <Out> ByRef block As StatementSyntax, ByRef HasVariable As Boolean) As Boolean
                block = Nothing
                HasVariable = node.Declaration IsNot Nothing AndAlso node.Declaration.Variables.Count = 1
                If Not HasVariable AndAlso node.Initializers.Count <> 1 Then
                    Return False
                End If
                If node.Incrementors.Count <> 1 Then
                    Return False
                End If
                Dim Incrementors As VisualBasicSyntaxNode = node.Incrementors.FirstOrDefault()?.Accept(_nodesVisitor)
                Dim iterator As AssignmentStatementSyntax = TryCast(Incrementors, AssignmentStatementSyntax)
                If iterator Is Nothing OrElse Not iterator.IsKind(VB.SyntaxKind.AddAssignmentStatement, VB.SyntaxKind.SubtractAssignmentStatement) Then
                    Return False
                End If
                Dim iteratorIdentifier As IdentifierNameSyntax = TryCast(iterator.Left, IdentifierNameSyntax)
                If iteratorIdentifier Is Nothing Then
                    Return False
                End If
                Dim StepExpression As LiteralExpressionSyntax = TryCast(iterator.Right, LiteralExpressionSyntax)
                If StepExpression Is Nothing OrElse Not (TypeOf StepExpression.Token.Value Is Integer) Then Return False
                Dim [step] As Integer = CInt(StepExpression.Token.Value)
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
                Dim ToValue As ExpressionSyntax
                If iterator.IsKind(VB.SyntaxKind.SubtractAssignmentStatement) Then
                    If condition.IsKind(CS.SyntaxKind.GreaterThanOrEqualExpression) OrElse condition.IsKind(CS.SyntaxKind.NotEqualsExpression) Then
                        ToValue = DirectCast(condition.Right.Accept(_nodesVisitor), ExpressionSyntax)
                    ElseIf condition.IsKind(CS.SyntaxKind.GreaterThanExpression) Then
                        ToValue = Factory.BinaryExpression(VB.SyntaxKind.AddExpression,
                                                                 DirectCast(condition.Right.Accept(_nodesVisitor), ExpressionSyntax),
                                                                 PlusToken,
                                                                 _literalExpression)
                    Else
                        Return False
                    End If
                Else
                    If condition.IsKind(CS.SyntaxKind.LessThanOrEqualExpression) OrElse condition.IsKind(CS.SyntaxKind.NotEqualsExpression) Then
                        ToValue = DirectCast(condition.Right.Accept(_nodesVisitor), ExpressionSyntax)
                    ElseIf condition.IsKind(CS.SyntaxKind.LessThanExpression) Then
                        ToValue = Factory.BinaryExpression(VB.SyntaxKind.SubtractExpression,
                                                                 DirectCast(condition.Right.Accept(_nodesVisitor), ExpressionSyntax),
                                                                 MinusToken,
                                                                 _literalExpression)
                    Else
                        Return False
                    End If
                End If

                Dim ControlVariable As VisualBasicSyntaxNode
                Dim FromValue As ExpressionSyntax
                If HasVariable Then
                    Dim v As CSS.VariableDeclaratorSyntax = node.Declaration.Variables(0)
                    FromValue = DirectCast(v.Initializer?.Value.Accept(_nodesVisitor), ExpressionSyntax)
                    If FromValue Is Nothing Then
                        Return False
                    End If
                    Dim forVariableToken As SyntaxToken = GenerateSafeVBToken(v.Identifier, node, _semanticModel)
                    Dim names As SeparatedSyntaxList(Of ModifiedIdentifierSyntax) =
                        Factory.SingletonSeparatedList(Factory.ModifiedIdentifier(forVariableToken))
                    ControlVariable = Factory.VariableDeclarator(names,
                                                                asClause:=If(node.Declaration.Type.IsVar, Nothing, Factory.SimpleAsClause(DirectCast(node.Declaration.Type.Accept(_nodesVisitor), TypeSyntax))),
                                                                initializer:=Nothing)
                Else
                    Dim initializer As CSS.AssignmentExpressionSyntax = TryCast(node.Initializers.FirstOrDefault(), CSS.AssignmentExpressionSyntax)
                    If initializer Is Nothing OrElse Not initializer.IsKind(CS.SyntaxKind.SimpleAssignmentExpression) Then
                        Return False
                    End If
                    If Not (TypeOf initializer.Left Is CSS.IdentifierNameSyntax) Then
                        Return False
                    End If
                    If DirectCast(initializer.Left, CSS.IdentifierNameSyntax).Identifier.IsEquivalentTo(iteratorIdentifier.Identifier) Then
                        Return False
                    End If
                    ControlVariable = initializer.Left.Accept(_nodesVisitor)
                    FromValue = DirectCast(initializer.Right.Accept(_nodesVisitor), ExpressionSyntax)
                End If

                Dim StatementFirstToken As SyntaxToken = node.Statement.GetFirstToken
                Dim OpenBraceLeadingTrivia As New SyntaxTriviaList
                Dim ClosingBraceTrailingTrivia As New SyntaxTriviaList

                Dim Statements As SyntaxList(Of StatementSyntax) = Me.ConvertBlock(node.Statement, OpenBraceLeadingTrivia, ClosingBraceTrailingTrivia)

                Dim ForStatementTrailingTrivia As SyntaxTriviaList = CollectConvertedTokenTrivia(node.CloseParenToken, GetLeading:=True, GetTrailing:=True)
                If StatementFirstToken.IsKind(CS.SyntaxKind.OpenBraceToken) Then
                    If StatementFirstToken.TrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                        ForStatementTrailingTrivia = ForStatementTrailingTrivia.AddRange(ConvertTriviaList(StatementFirstToken.TrailingTrivia))
                    End If
                End If

                Dim StepClause As ForStepClauseSyntax = If([step] = 1,
                                                            Nothing,
                                                            Factory.ForStepClause(Factory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression,
                                                                                                            Factory.Literal([step]))
                                                          ))
                Dim ForStatement As ForStatementSyntax = Factory.ForStatement(ForKeyword.WithConvertedLeadingTriviaFrom(node.ForKeyword),
                                                                              ControlVariable,
                                                                              EqualsToken,
                                                                              FromValue,
                                                                              ToKeyword,
                                                                              ToValue,
                                                                              StepClause
                                                                             ).WithTrailingTrivia(ForStatementTrailingTrivia).WithTrailingEOL(RemoveLastLineContinuation:=True)
                block = Factory.ForBlock(ForStatement,
                                            Statements,
                                            Factory.NextStatement().WithTrailingTrivia(ClosingBraceTrailingTrivia).WithTrailingEOL(RemoveLastLineContinuation:=True)
                                            )
                Return True
            End Function

            Private Function ConvertSingleBlock(node As CSS.ExpressionSyntax) As StatementSyntax
                Dim exprNode As VisualBasicSyntaxNode = Nothing
                Dim newLeadingTrivia As New SyntaxTriviaList

                If TypeOf node Is CSS.AssignmentExpressionSyntax Then
                    Dim csAssignment As CSS.AssignmentExpressionSyntax = DirectCast(node, CSS.AssignmentExpressionSyntax)
                    If csAssignment.Left.IsKind(CS.SyntaxKind.ParenthesizedExpression) Then
                        Dim csLeft As CSS.ParenthesizedExpressionSyntax = DirectCast(csAssignment.Left, CSS.ParenthesizedExpressionSyntax)
                        Dim LeftExpression As ExpressionSyntax = CType(csLeft.Expression.Accept(_nodesVisitor), ExpressionSyntax)
                        Dim RightExpression As ExpressionSyntax = DirectCast(csAssignment.Right.Accept(_nodesVisitor), ExpressionSyntax)
                        If csAssignment.IsKind(CS.SyntaxKind.SimpleAssignmentExpression) Then
                            exprNode = Factory.SimpleAssignmentStatement(LeftExpression, RightExpression).
                                                         WithConvertedTriviaFrom(node)
                            newLeadingTrivia = newLeadingTrivia.AddRange(node.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=False, "Parenthesized Expression Assignment"))
                        End If
                    End If
                ElseIf TypeOf node Is CSS.PostfixUnaryExpressionSyntax Then
                    Dim CSPostFixUnaryExpression As CSS.PostfixUnaryExpressionSyntax = DirectCast(node, CSS.PostfixUnaryExpressionSyntax)
                    If TypeOf CSPostFixUnaryExpression.Operand Is CSS.ParenthesizedExpressionSyntax Then
                        Dim csOperand As CSS.ParenthesizedExpressionSyntax = DirectCast(CSPostFixUnaryExpression.Operand, CSS.ParenthesizedExpressionSyntax)
                        Dim kind As VB.SyntaxKind = ConvertCSExpressionsKindToVBKind(CS.CSharpExtensions.Kind(node))
                        Dim OperandExpression As ExpressionSyntax = DirectCast(csOperand.Expression.Accept(_nodesVisitor), ExpressionSyntax)
                        exprNode = Factory.AssignmentStatement(ConvertCSExpressionsKindToVBKind(CS.CSharpExtensions.Kind(node)),
                                                                OperandExpression,
                                                                ExpressionKindToOperatorToken(kind, IsReferenceType:=False),
                                                                _literalExpression)
                        newLeadingTrivia = newLeadingTrivia.AddRange(node.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=False, "Parenthesized Expression Assignment"))
                    End If
                End If
                If exprNode Is Nothing Then
                    exprNode = node.Accept(_nodesVisitor)
                    If exprNode.IsKind(VB.SyntaxKind.TryBlock) Then
                        Dim TmpTrivia As SyntaxTriviaList = exprNode.GetLeadingTrivia
                        If TmpTrivia.Count = 2 AndAlso TmpTrivia(0).IsComment AndAlso TmpTrivia(1).IsEndOfLine Then
                            newLeadingTrivia = newLeadingTrivia.AddRange(TmpTrivia)
                        Else
                            newLeadingTrivia = newLeadingTrivia.AddRange(ConvertTriviaList(node.GetLeadingTrivia))
                        End If
                    Else
                        newLeadingTrivia = newLeadingTrivia.AddRange(ConvertTriviaList(node.GetLeadingTrivia))
                    End If
                End If
                Dim NewTrailingTrivia As SyntaxTriviaList = exprNode.GetTrailingTrivia
                exprNode = exprNode.WithoutTrivia
                If Not (TypeOf exprNode Is StatementSyntax) Then
                    Select Case True
                        Case TypeOf exprNode Is ObjectCreationExpressionSyntax
                            exprNode = FactoryDimStatement(GetUniqueVariableNameInScope(node, "tempVar", _semanticModel),
                                                          Factory.AsNewClause(DirectCast(exprNode, NewExpressionSyntax)),
                                                          initializer:=Nothing)
                        Case TypeOf exprNode Is InvocationExpressionSyntax
                            exprNode = If(exprNode.GetFirstToken.IsKind(VB.SyntaxKind.NewKeyword), Factory.CallStatement(DirectCast(exprNode, ExpressionSyntax).WithLeadingTrivia(VBSpaceTrivia)), DirectCast(Factory.ExpressionStatement(DirectCast(exprNode, ExpressionSyntax)), VisualBasicSyntaxNode))
                        Case Else
                            exprNode = Factory.ExpressionStatement(DirectCast(exprNode, ExpressionSyntax))
                    End Select
                End If
                Return DirectCast(exprNode, StatementSyntax).WithLeadingTrivia(newLeadingTrivia).WithTrailingTrivia(NewTrailingTrivia).WithTrailingEOL(RemoveLastLineContinuation:=True)
            End Function

            Private Function ConvertSingleExpression(node As CSS.ExpressionSyntax, LeadingTrivia As SyntaxTriviaList, TrailingTrivia As SyntaxTriviaList) As List(Of StatementSyntax)
                Dim StatementList As New List(Of StatementSyntax)
                Dim OneStatement As VisualBasicSyntaxNode = Nothing
                Dim newLeadingTrivia As New SyntaxTriviaList

                If LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    Dim convertedLeadingTrivia As SyntaxTriviaList = ConvertTriviaList(LeadingTrivia)
                    newLeadingTrivia = newLeadingTrivia.AddRange(convertedLeadingTrivia)
                End If

                Dim RightExpression As ExpressionSyntax
                If TypeOf node Is CSS.AssignmentExpressionSyntax Then
                    Dim csAssignment As CSS.AssignmentExpressionSyntax = DirectCast(node, CSS.AssignmentExpressionSyntax)
                    RightExpression = DirectCast(csAssignment.Right.Accept(_nodesVisitor), ExpressionSyntax)
                    If csAssignment.Left.IsKind(CS.SyntaxKind.ParenthesizedExpression) Then
                        Dim csLeft As CSS.ParenthesizedExpressionSyntax = DirectCast(csAssignment.Left, CSS.ParenthesizedExpressionSyntax)
                        Dim LeftExpression As ExpressionSyntax = CType(csLeft.Expression.Accept(_nodesVisitor), ExpressionSyntax)
                        If csAssignment.IsKind(CS.SyntaxKind.SimpleAssignmentExpression) Then
                            OneStatement = Factory.SimpleAssignmentStatement(LeftExpression, RightExpression).
                                                         WithConvertedTriviaFrom(node)
                            newLeadingTrivia = newLeadingTrivia.AddRange(node.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=False, "Parenthesized Expression Assignment"))
                        End If
                    End If
                    ' Handle New Invocation Expression = something
                    'Dim x As IO.FileInfo = New IO.FileInfo("")
                    'x.IsReadOnly = True
                    If node.IsKind(CS.SyntaxKind.SimpleAssignmentExpression) Then
                        If TypeOf csAssignment.Left Is CSS.MemberAccessExpressionSyntax Then
                            Dim csMemberAccessExpression As CSS.MemberAccessExpressionSyntax = CType(csAssignment.Left, CSS.MemberAccessExpressionSyntax)
                            If TypeOf csMemberAccessExpression.Expression Is CSS.ObjectCreationExpressionSyntax Then
                                Dim csObjectCreationExpression As CSS.ObjectCreationExpressionSyntax = CType(csMemberAccessExpression.Expression, CSS.ObjectCreationExpressionSyntax)
                                Dim newExpression As NewExpressionSyntax = DirectCast(csObjectCreationExpression.Accept(_nodesVisitor), NewExpressionSyntax)
                                Dim nameToken As SyntaxToken = Factory.Identifier(GetUniqueVariableNameInScope(node, "tempVar", _semanticModel))
                                StatementList.Add(FactoryDimStatement(nameToken,
                                                                      Factory.AsNewClause(newExpression),
                                                                      initializer:=Nothing
                                                                     )
                                                 )
                                Dim kind As VB.SyntaxKind = ConvertCSExpressionsKindToVBKind(CS.CSharpExtensions.Kind(node))
                                Dim OperatorToken As SyntaxToken = ExpressionKindToOperatorToken(kind, IsReferenceType:=False)
                                StatementList.Add(Factory.AssignmentStatement(kind,
                                                                              Factory.SimpleMemberAccessExpression(Factory.IdentifierName(nameToken),
                                                                                                                   DotToken,
                                                                                                                   CType(csMemberAccessExpression.Name.Accept(_nodesVisitor), SimpleNameSyntax)),
                                                                              OperatorToken,
                                                                              RightExpression).WithTrailingEOL(RemoveLastLineContinuation:=True))
                                Return StatementList
                                Stop
                            End If
                        End If
                    End If
                ElseIf TypeOf node Is CSS.PostfixUnaryExpressionSyntax Then
                    Dim CSPostFixUnaryExpression As CSS.PostfixUnaryExpressionSyntax = DirectCast(node, CSS.PostfixUnaryExpressionSyntax)
                    If TypeOf CSPostFixUnaryExpression.Operand Is CSS.ParenthesizedExpressionSyntax Then
                        Dim csOperand As CSS.ParenthesizedExpressionSyntax = DirectCast(CSPostFixUnaryExpression.Operand, CSS.ParenthesizedExpressionSyntax)
                        Dim kind As VB.SyntaxKind = ConvertCSExpressionsKindToVBKind(CS.CSharpExtensions.Kind(node))
                        Dim OperandExpression As ExpressionSyntax = DirectCast(csOperand.Expression.Accept(_nodesVisitor), ExpressionSyntax)
                        OneStatement = Factory.AssignmentStatement(ConvertCSExpressionsKindToVBKind(CS.CSharpExtensions.Kind(node)),
                                                                OperandExpression,
                                                                ExpressionKindToOperatorToken(kind, IsReferenceType:=False),
                                                                _literalExpression)
                        newLeadingTrivia = newLeadingTrivia.AddRange(node.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=False, "Parenthesized Expression Assignment"))
                    End If
                End If
                Dim NewTrailingTrivia As SyntaxTriviaList
                If TrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                    NewTrailingTrivia = NewTrailingTrivia.AddRange(ConvertTriviaList(TrailingTrivia))
                End If
                If OneStatement Is Nothing Then
                    OneStatement = node.Accept(_nodesVisitor)
                    If Not EndsWithSimilarTrivia(node.GetLeadingTrivia.ConvertTriviaList(), OneStatement.GetLeadingTrivia) Then
                        newLeadingTrivia = newLeadingTrivia.AddRange(OneStatement.GetLeadingTrivia)
                    End If

                    If Not EndsWithSimilarTrivia(node.GetTrailingTrivia.ConvertTriviaList(), OneStatement.GetTrailingTrivia) Then
                        NewTrailingTrivia = NewTrailingTrivia.AddRange(OneStatement.GetTrailingTrivia)
                    End If
                    If OneStatement.IsKind(VB.SyntaxKind.TryBlock) Then
                        Dim tryLeadingTrivia As SyntaxTriviaList = OneStatement.GetLeadingTrivia
                        If tryLeadingTrivia.Any Then
                            If tryLeadingTrivia(0).IsComment AndAlso tryLeadingTrivia(0).ToFullString = "' TODO: This Try Block can be removed" Then
                                StatementList.AddRange(DirectCast(OneStatement, TryBlockSyntax).Statements)
                                newLeadingTrivia = newLeadingTrivia.RemoveRange(tryLeadingTrivia)
                                StatementList(0) = StatementList(0).WithLeadingTrivia(newLeadingTrivia)
                                Dim Last As Integer = StatementList.Count - 1
                                StatementList(Last) = StatementList(Last).WithTrailingTrivia(NewTrailingTrivia).WithTrailingEOL(RemoveLastLineContinuation:=True)
                                Return StatementList
                            End If
                        End If
                    End If
                End If
                newLeadingTrivia = New SyntaxTriviaList
                If LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    newLeadingTrivia = newLeadingTrivia.AddRange(ConvertTriviaList(LeadingTrivia))
                End If
                If Not EndsWithSimilarTrivia(node.GetLeadingTrivia, LeadingTrivia) Then
                    newLeadingTrivia = newLeadingTrivia.AddRange(ConvertTriviaList(node.GetLeadingTrivia))
                End If
                OneStatement = OneStatement.WithoutTrivia
                If Not TypeOf OneStatement Is StatementSyntax Then
                    Select Case True
                        Case TypeOf OneStatement Is ObjectCreationExpressionSyntax
                            OneStatement = FactoryDimStatement(GetUniqueVariableNameInScope(node, "tempVar", _semanticModel),
                                                              Factory.AsNewClause(DirectCast(OneStatement, NewExpressionSyntax)),
                                                              initializer:=Nothing)
                        Case TypeOf OneStatement Is AwaitExpressionSyntax
                            OneStatement = FactoryDimStatement(GetUniqueVariableNameInScope(node, "tempVar", _semanticModel),
                                                               asClause:=Nothing,
                                                               Factory.EqualsValue(CType(OneStatement, AwaitExpressionSyntax)))

                        Case TypeOf OneStatement Is InvocationExpressionSyntax
                            OneStatement = If(OneStatement.GetFirstToken.IsKind(VB.SyntaxKind.NewKeyword), Factory.CallStatement(DirectCast(OneStatement, ExpressionSyntax).WithLeadingTrivia(VBSpaceTrivia)), DirectCast(Factory.ExpressionStatement(DirectCast(OneStatement, ExpressionSyntax)), VisualBasicSyntaxNode))
                        Case Else
                            OneStatement = Factory.ExpressionStatement(DirectCast(OneStatement, ExpressionSyntax))
                    End Select
                End If
                StatementList.AddRange(ReplaceOneStatementWithMarkedStatements(node, DirectCast(OneStatement, StatementSyntax).WithLeadingTrivia(newLeadingTrivia).WithTrailingTrivia(NewTrailingTrivia).WithTrailingEOL(RemoveLastLineContinuation:=True)))
                Return StatementList
            End Function

            Private Function ConvertSwitchSection(section As CSS.SwitchSectionSyntax) As CaseBlockSyntax
                Dim NewLeadingStatements As New List(Of StatementSyntax)
                If section.Labels.OfType(Of CSS.DefaultSwitchLabelSyntax)().Any() Then
                    Return Factory.CaseElseBlock(Factory.CaseElseStatement(Factory.ElseCaseClause()), Me.ConvertSwitchSectionBlock(section, NewLeadingStatements))
                End If
                Dim LabelList As New List(Of CaseClauseSyntax)
                Dim vbLabelLeadingTrivia As SyntaxTriviaList = ConvertTriviaList(section.GetLeadingTrivia)
                Dim csLabelTrailingTrivia As New SyntaxTriviaList
                ' Find Case leading space
                For Each CaseLabel As CSS.SwitchLabelSyntax In section.Labels
                    Dim CaseLabelExpression As ExpressionSyntax
                    Dim CaseLabelWhenExpression As ExpressionSyntax
                    Select Case True
                        Case TypeOf CaseLabel Is CSS.CaseSwitchLabelSyntax
                            CaseLabelExpression = DirectCast(CType(CaseLabel, CSS.CaseSwitchLabelSyntax).Value.Accept(_nodesVisitor), ExpressionSyntax)
                            CaseLabelWhenExpression = Nothing
                            'If Not TriviaIsIdentical(Factory.TriviaList(vbLabelLeadingTrivia), ConvertTriviaList(CaseLabel.GetLeadingTrivia).ToList) Then
                            '    vbLabelLeadingTrivia=vbLabelLeadingTrivia.AddRange(ConvertTriviaList(CaseLabel.GetLeadingTrivia))
                            'End If
                        Case TypeOf CaseLabel Is CSS.CasePatternSwitchLabelSyntax
                            Dim PatternLabel As CSS.CasePatternSwitchLabelSyntax = DirectCast(CaseLabel, CSS.CasePatternSwitchLabelSyntax)
                            Dim variableNameToken As SyntaxToken
                            CaseLabelWhenExpression = CType(PatternLabel.WhenClause?.Accept(_nodesVisitor), ExpressionSyntax)
                            If TypeOf PatternLabel.Pattern Is CSS.ConstantPatternSyntax Then
                                Dim ConstantPattern As CSS.ConstantPatternSyntax = DirectCast(PatternLabel.Pattern, CSS.ConstantPatternSyntax)
                                CaseLabelExpression = DirectCast(ConstantPattern.Expression.Accept(_nodesVisitor), ExpressionSyntax).WithConvertedLeadingTriviaFrom(PatternLabel)
                                csLabelTrailingTrivia = csLabelTrailingTrivia.AddRange(CaseLabel.GetTrailingTrivia)
                            ElseIf TypeOf PatternLabel.Pattern Is CSS.DeclarationPatternSyntax Then
                                Dim Pattern As CSS.DeclarationPatternSyntax = DirectCast(PatternLabel.Pattern, CSS.DeclarationPatternSyntax)
                                Dim Type As TypeSyntax = DirectCast(Pattern.Type.Accept(_nodesVisitor), TypeSyntax)
                                If TypeOf Pattern.Designation Is CSS.SingleVariableDesignationSyntax Then
                                    variableNameToken = GenerateSafeVBToken(DirectCast(Pattern.Designation, CSS.SingleVariableDesignationSyntax).Identifier,
                                                                     section,
                                                                     _semanticModel)
                                ElseIf TypeOf Pattern.Designation Is CSS.DiscardDesignationSyntax Then
                                Else
                                    Stop
                                End If

                                Dim SwitchExpression1 As ExpressionSyntax = DirectCast(DirectCast(section.Parent, CSS.SwitchStatementSyntax).Expression.Accept(_nodesVisitor), ExpressionSyntax)
                                If TypeOf Pattern.Designation Is CSS.DiscardDesignationSyntax Then
                                    CaseLabelExpression = Factory.TypeOfIsExpression(SwitchExpression1, CType(Pattern.Type.Accept(_nodesVisitor), TypeSyntax))
                                Else
                                    NewLeadingStatements.Add(FactoryDimStatement(variableNameToken,
                                                                                Factory.SimpleAsClause(Type),
                                                                                Factory.EqualsValue(Factory.CTypeExpression(SwitchExpression1, Type))
                                                                                ))
                                    CaseLabelExpression = DirectCast(Pattern.Designation.Accept(_nodesVisitor), ExpressionSyntax)
                                End If
                            ElseIf TypeOf PatternLabel.Pattern Is CSS.VarPatternSyntax Then
                                Dim VarPattern As CSS.VarPatternSyntax = CType(PatternLabel.Pattern, CSS.VarPatternSyntax)
                                Dim SwitchExpression1 As ExpressionSyntax = DirectCast(DirectCast(section.Parent, CSS.SwitchStatementSyntax).Expression.Accept(_nodesVisitor), ExpressionSyntax)

                                CaseLabelExpression = Nothing
                                If VarPattern.Designation.IsKind(CS.SyntaxKind.DiscardDesignation) Then
                                    If PatternLabel.WhenClause IsNot Nothing Then
                                        CaseLabelWhenExpression = CType(PatternLabel.WhenClause.Accept(_nodesVisitor), ExpressionSyntax)
                                    Else
                                        CaseLabelExpression = Factory.IdentifierName("Else")
                                    End If
                                ElseIf VarPattern.Designation.IsKind(CS.SyntaxKind.SingleVariableDesignation) Then
                                    variableNameToken = GenerateSafeVBToken(DirectCast(VarPattern.Designation, CSS.SingleVariableDesignationSyntax).Identifier,
                                                                     section,
                                                                     _semanticModel)
                                    NewLeadingStatements.Add(FactoryDimStatement(variableNameToken,
                                                                                Factory.SimpleAsClause(Factory.PredefinedType(ObjectKeyword)),
                                                                                Factory.EqualsValue(SwitchExpression1)).WithTrailingEOL(RemoveLastLineContinuation:=True))
                                    CaseLabelExpression = Factory.IdentifierName("Else")
                                Else
                                    CaseLabelExpression = Nothing
                                End If
                            ElseIf TypeOf PatternLabel.Pattern Is CSS.RecursivePatternSyntax Then
                                CaseLabelExpression = NothingExpression
                                vbLabelLeadingTrivia = vbLabelLeadingTrivia.AddRange(section.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=False, $"VB has no equivalent to the C# 'Recursive Pattern({PatternLabel.Pattern}) in 'case' statements"))
                            Else
                                CaseLabelExpression = Nothing
                                Stop
                            End If
                        Case Else
                            CaseLabelExpression = Nothing
                            CaseLabelWhenExpression = Nothing
                            Stop
                    End Select
                    If CaseLabelExpression Is Nothing Then
                        LabelList.Add(Factory.SimpleCaseClause(CaseLabelWhenExpression))
                        csLabelTrailingTrivia = csLabelTrailingTrivia.AddRange(section.GetTrailingTrivia)
                    Else
                        If CaseLabelWhenExpression Is Nothing Then
                            LabelList.Add(Factory.SimpleCaseClause(CaseLabelExpression.WithoutLeadingTrivia))
                        Else
                            ' TODO use line continuation instead of space
                            LabelList.Add(Factory.SimpleCaseClause(CaseLabelExpression.With({VBSpaceTrivia}, {VBSpaceTrivia})))
                            NewLeadingStatements.Add(Factory.SingleLineIfStatement(CaseLabelWhenExpression.With({VBSpaceTrivia}, {VBSpaceTrivia}),
                                                                                     Factory.SingletonList(Of StatementSyntax)(Factory.ExitSelectStatement),
                                                                                     elseClause:=Nothing
                                                                                     ).WithLeadingTrivia(CaseLabelExpression.GetLeadingTrivia).
                                                                                       WithTrailingEOL(RemoveLastLineContinuation:=True)
                                                    )
                        End If
                    End If
                Next
                Dim CommentString As New StringBuilder
                csLabelTrailingTrivia = csLabelTrailingTrivia.AddRange(section.GetTrailingTrivia)
                For Each t As SyntaxTrivia In ConvertTriviaList(csLabelTrailingTrivia)
                    Select Case t.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            CommentString.Append(t.ToString)
                        Case VB.SyntaxKind.EndOfLineTrivia
                            'ignore
                        Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                            CommentString.Append(t.ToString.Trim.TrimStart("'"c).Trim)
                        Case Else
                            Stop
                    End Select
                Next
                Dim TrailingTrivia As New SyntaxTriviaList
                If CommentString.Length > 0 Then
                    TrailingTrivia = TrailingTrivia.AddRange({Factory.CommentTrivia($"'{CommentString.ToString.Trim}"), VBEOLTrivia})
                End If
                Dim CaseStatement As CaseStatementSyntax = Factory.CaseStatement(Factory.SeparatedList(LabelList)).With(vbLabelLeadingTrivia, TrailingTrivia).WithTrailingEOL(RemoveLastLineContinuation:=True)
                Dim switchBlockStatements As SyntaxList(Of StatementSyntax) = Me.ConvertSwitchSectionBlock(section, NewLeadingStatements)
                Return Factory.CaseBlock(CaseStatement, switchBlockStatements)
            End Function

            Private Function ConvertSwitchSectionBlock(section As CSS.SwitchSectionSyntax, Statements As List(Of StatementSyntax)) As SyntaxList(Of StatementSyntax)
                Dim lastStatement As CSS.StatementSyntax = section.Statements.LastOrDefault()
                Dim OpenBraceLeadingTrivia As New SyntaxTriviaList
                Dim ClosingBraceTrailingTrivia As New SyntaxTriviaList
                For Each s As CSS.StatementSyntax In section.Statements
                    If s Is lastStatement AndAlso TypeOf s Is CSS.BreakStatementSyntax Then
                        If lastStatement.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia OrElse
                            lastStatement.GetTrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                            Statements.Add(Factory.EmptyStatement.WithConvertedTriviaFrom(lastStatement))
                        End If
                        Continue For
                    End If
                    Statements.AddRange(Me.ConvertBlock(s, OpenBraceLeadingTrivia, ClosingBraceTrailingTrivia))
                    If OpenBraceLeadingTrivia.Any Then
                        Statements(0) = Statements.First.WithPrependedLeadingTrivia(OpenBraceLeadingTrivia)
                    End If
                    If ClosingBraceTrailingTrivia.Any Then
                        Statements(Statements.Count - 1) = Statements.Last.WithAppendedTrailingTrivia(ClosingBraceTrailingTrivia)
                    End If
                Next
                Return Factory.List(Statements)
            End Function

            Private Function MakeGotoSwitchLabel(Expression As VisualBasicSyntaxNode) As String
                If TypeOf Expression Is ElseCaseClauseSyntax Then
                    Return $"_Select{_switchCount}_CaseDefault"
                End If
                Dim ExpressionBuilder As New StringBuilder
                For Each e As IndexClass(Of Char) In Expression.ToString.
                                                    Replace(".", "Dot", StringComparison.Ordinal).
                                                    Replace("""", "Quote", StringComparison.Ordinal).
                                                    Replace("[", "OpenBracket", StringComparison.Ordinal).
                                                    Replace("]", "CloseBracket", StringComparison.Ordinal).
                                                    Replace(" ", "_", StringComparison.Ordinal).WithIndex
                    If e.IsFirst AndAlso Not VB.SyntaxFacts.IsIdentifierStartCharacter(e.Value) Then
                        ExpressionBuilder.Append($"_")
                    End If
                    If VB.SyntaxFacts.IsIdentifierPartCharacter(e.Value) Then
                        ExpressionBuilder.Append(e.Value)
                    Else
                        ExpressionBuilder.Append("_"c)
                    End If
                Next
                Return ExpressionBuilder.ToString
            End Function

            Private Function TryConvertIfNotNullRaiseEvent(node As CSS.IfStatementSyntax, ByRef raiseEventStatement As StatementSyntax) As Boolean
                raiseEventStatement = Nothing
                Dim comparisonExpression As CSS.BinaryExpressionSyntax = Nothing
                Return TryGetBinaryExpression(node, comparisonExpression, CS.SyntaxKind.NotEqualsExpression, CS.SyntaxKind.NullLiteralExpression) AndAlso
                    Me.TryConvertRaiseEvent(node.Statement, comparisonExpression, raiseEventStatement)
            End Function

            Private Function TryConvertRaiseEvent(resultStatement As CSS.StatementSyntax, _1 As CSS.BinaryExpressionSyntax, ByRef raiseEventStatement As StatementSyntax) As Boolean
                Dim singleStatement As CSS.ExpressionStatementSyntax
                Dim tempVar As Boolean = TypeOf resultStatement Is CSS.BlockSyntax
                Dim block As CSS.BlockSyntax = If(tempVar, CType(resultStatement, CSS.BlockSyntax), Nothing)
                If tempVar Then
                    If block.Statements.Count <> 1 Then
                        Return False
                    End If
                    singleStatement = TryCast(block.Statements(0), CSS.ExpressionStatementSyntax)
                Else
                    singleStatement = TryCast(resultStatement, CSS.ExpressionStatementSyntax)
                End If

                Dim isInvovationExpression As Boolean = TypeOf singleStatement?.Expression Is CSS.InvocationExpressionSyntax
                Dim singleInvocationExpression As CSS.InvocationExpressionSyntax = If(isInvovationExpression, CType(singleStatement?.Expression, CSS.InvocationExpressionSyntax), Nothing)
                If Not isInvovationExpression Then
                    Return False
                End If

                raiseEventStatement = TryCast(singleInvocationExpression.Accept(_nodesVisitor), RaiseEventStatementSyntax)
                If raiseEventStatement Is Nothing Then
                    Return False
                End If
                raiseEventStatement = raiseEventStatement.WithTrailingEOL(RemoveLastLineContinuation:=True)
                Return True
            End Function

            Private Function WillConvertToFor(node As CSS.ForStatementSyntax) As Boolean
                Dim hasVariable As Boolean = node.Declaration IsNot Nothing AndAlso node.Declaration.Variables.Count = 1
                If Not hasVariable AndAlso node.Initializers.Count <> 1 Then
                    Return False
                End If
                If node.Incrementors.Count <> 1 Then
                    Return False
                End If
                Dim Incrementors As VisualBasicSyntaxNode = node.Incrementors.FirstOrDefault()?.Accept(_nodesVisitor)
                Dim iterator As AssignmentStatementSyntax = TryCast(Incrementors, AssignmentStatementSyntax)
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
                    If Not (TypeOf initializer.Left Is CSS.IdentifierNameSyntax) Then
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
                Dim ListOfStatements As SyntaxList(Of StatementSyntax) = Factory.List(node.Statements.Where(Function(s As CSS.StatementSyntax) Not (TypeOf s Is CSS.EmptyStatementSyntax)).SelectMany(Function(s As CSS.StatementSyntax) s.Accept(Me)))
                If node.OpenBraceToken.HasLeadingTrivia OrElse node.OpenBraceToken.HasTrailingTrivia Then
                    If ListOfStatements.Any Then
                        ListOfStatements = ListOfStatements.Replace(ListOfStatements(0), ListOfStatements(0).WithPrependedLeadingTrivia(ConvertTriviaList(node.OpenBraceToken.TrailingTrivia)).WithPrependedLeadingTrivia(ConvertTriviaList(node.OpenBraceToken.LeadingTrivia)))
                    Else
                        ListOfStatements = ListOfStatements.Add(Factory.EmptyStatement.WithConvertedTriviaFrom(node.OpenBraceToken))
                    End If
                End If
                If node.CloseBraceToken.HasLeadingTrivia OrElse node.OpenBraceToken.HasTrailingTrivia Then
                    If ListOfStatements.Any Then
                        ListOfStatements = ListOfStatements.Replace(ListOfStatements.Last, ListOfStatements.Last.WithAppendedTrailingTrivia(ConvertTriviaList(node.OpenBraceToken.LeadingTrivia)).WithAppendedTrailingTrivia(ConvertTriviaList(node.OpenBraceToken.TrailingTrivia)))
                    Else
                        ListOfStatements = ListOfStatements.Add(Factory.EmptyStatement.WithConvertedTriviaFrom(node.CloseBraceToken))
                    End If
                End If
                Return ListOfStatements
            End Function

            Public Overrides Function VisitBreakStatement(node As CSS.BreakStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim StatementKind As VB.SyntaxKind = VB.SyntaxKind.None
                Dim BlockKeyword As SyntaxToken
                For Each stmt As CSS.StatementSyntax In node.GetAncestors(Of CSS.StatementSyntax)()
                    If TypeOf stmt Is CSS.DoStatementSyntax Then
                        StatementKind = VB.SyntaxKind.ExitDoStatement
                        BlockKeyword = DoKeyword
                        Exit For
                    ElseIf TypeOf stmt Is CSS.WhileStatementSyntax Then
                        StatementKind = VB.SyntaxKind.ExitWhileStatement
                        BlockKeyword = WhileKeyword
                        Exit For
                    ElseIf TypeOf stmt Is CSS.ForStatementSyntax Then
                        If Me.WillConvertToFor(DirectCast(stmt, CSS.ForStatementSyntax)) Then
                            StatementKind = VB.SyntaxKind.ExitForStatement
                            BlockKeyword = ForKeyword
                            Exit For
                        Else
                            StatementKind = VB.SyntaxKind.ExitWhileStatement
                            BlockKeyword = WhileKeyword
                            Exit For
                        End If
                    ElseIf TypeOf stmt Is CSS.ForEachStatementSyntax Then
                        StatementKind = VB.SyntaxKind.ExitForStatement
                        BlockKeyword = ForKeyword
                        Exit For
                    ElseIf TypeOf stmt Is CSS.SwitchStatementSyntax Then
                        StatementKind = VB.SyntaxKind.ExitSelectStatement
                        BlockKeyword = SelectKeyword
                        Exit For
                    End If
                Next
                BlockKeyword = BlockKeyword.WithAppendedTrailingTrivia(CollectConvertedTokenTrivia(node.SemicolonToken, GetLeading:=True, GetTrailing:=True))
                Return Factory.SingletonList(Of StatementSyntax)(Factory.ExitStatement(StatementKind,
                                                                                       BlockKeyword).
                                                                                           WithConvertedLeadingTriviaFrom(node).
                                                                                           WithTrailingEOL(RemoveLastLineContinuation:=True)
                                                                )
            End Function

            Public Overrides Function VisitCheckedStatement(node As CSS.CheckedStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim OpenBraceLeadingTrivia As New SyntaxTriviaList
                Dim ClosingBraceTrailingTrivia As New SyntaxTriviaList
                If node.Keyword.IsKind(CS.SyntaxKind.CheckedKeyword) Then
                    Return WrapInComment(Me.ConvertBlock(node.Block, OpenBraceLeadingTrivia, ClosingBraceTrailingTrivia), node, "Visual Basic Default Is checked math, check that this works for you!")
                End If
                Return WrapInComment(Me.ConvertBlock(node.Block, OpenBraceLeadingTrivia, ClosingBraceTrailingTrivia), node, "Visual Basic does Not support unchecked statements!")
            End Function

            Public Overrides Function VisitContinueStatement(node As CSS.ContinueStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim statementKind As VB.SyntaxKind = VB.SyntaxKind.None
                Dim BlockKeyword As SyntaxToken
                For Each stmt As CSS.StatementSyntax In node.GetAncestors(Of CSS.StatementSyntax)()
                    If TypeOf stmt Is CSS.DoStatementSyntax Then
                        statementKind = VB.SyntaxKind.ContinueDoStatement
                        BlockKeyword = DoKeyword
                        Exit For
                    End If

                    If TypeOf stmt Is CSS.WhileStatementSyntax Then
                        statementKind = VB.SyntaxKind.ContinueWhileStatement
                        BlockKeyword = WhileKeyword
                        Exit For
                    End If

                    If TypeOf stmt Is CSS.ForEachStatementSyntax Then
                        statementKind = VB.SyntaxKind.ContinueForStatement
                        BlockKeyword = ForKeyword
                    End If

                    If TypeOf stmt Is CSS.ForStatementSyntax Then
                        Dim isFor As Boolean = Me.WillConvertToFor(DirectCast(stmt, CSS.ForStatementSyntax))
                        statementKind = If(isFor, VB.SyntaxKind.ContinueForStatement, VB.SyntaxKind.ContinueWhileStatement)
                        BlockKeyword = If(isFor, ForKeyword, WhileKeyword)
                        Exit For
                    End If
                Next

                Return Factory.SingletonList(Of StatementSyntax)(
                    Factory.ContinueStatement(statementKind,
                                              BlockKeyword
                                              ).WithConvertedLeadingTriviaFrom(node) _
                                               .WithTrailingTrivia(CollectConvertedTokenTrivia(node.SemicolonToken,
                                                                                               GetLeading:=True,
                                                                                               GetTrailing:=True)
                                                                                               ) _
                                               .WithTrailingEOL(RemoveLastLineContinuation:=True))
            End Function

            Public Overrides Function VisitDeclarationExpression(node As CSS.DeclarationExpressionSyntax) As SyntaxList(Of StatementSyntax)
                Return MyBase.VisitDeclarationExpression(node)
            End Function

            Public Overrides Function VisitDoStatement(node As CSS.DoStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim condition As ExpressionSyntax = DirectCast(node.Condition.Accept(_nodesVisitor), ExpressionSyntax)
                Dim OpenBraceLeadingTrivia As New SyntaxTriviaList
                Dim ClosingBraceTrailingTrivia As New SyntaxTriviaList
                Dim stmt As SyntaxList(Of StatementSyntax) = Me.ConvertBlock(node.Statement, OpenBraceLeadingTrivia, ClosingBraceTrailingTrivia)
                If OpenBraceLeadingTrivia.Any OrElse ClosingBraceTrailingTrivia.Any Then
                    Stop
                End If
                Dim DoStatement As DoStatementSyntax = Factory.DoStatement(VB.SyntaxKind.SimpleDoStatement)
                Dim LoopStatement As LoopStatementSyntax = Factory.LoopStatement(VB.SyntaxKind.LoopWhileStatement, Factory.WhileClause(condition).WithTrailingEOL(RemoveLastLineContinuation:=True))
                Dim block As DoLoopBlockSyntax = Factory.DoLoopWhileBlock(DoStatement, stmt, LoopStatement)
                Return ReplaceOneStatementWithMarkedStatements(node, block)
            End Function

            Public Overrides Function VisitEmptyStatement(node As CSS.EmptyStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim leadingTrivia As SyntaxTriviaList = node.GetLeadingTrivia.ConvertTriviaList
                Dim trailingTrivia As SyntaxTriviaList = node.GetTrailingTrivia.ConvertTriviaList
                If Not leadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    leadingTrivia = New SyntaxTriviaList
                End If
                If Not trailingTrivia.ContainsCommentOrDirectiveTrivia Then
                    trailingTrivia = New SyntaxTriviaList
                End If
                Return Factory.SingletonList(Of StatementSyntax)(Factory.EmptyStatement().With(leadingTrivia, trailingTrivia))
            End Function

            Public Overrides Function VisitExpressionStatement(node As CSS.ExpressionStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim StatementList As List(Of StatementSyntax) = Me.ConvertSingleExpression(node.Expression,
                                                                                           node.GetLeadingTrivia,
                                                                                           node.GetTrailingTrivia)
                Return ReplaceStatementsWithMarkedStatements(node, StatementList)
            End Function

            Public Overrides Function VisitFixedStatement(node As CSS.FixedStatementSyntax) As SyntaxList(Of StatementSyntax)
                Return Factory.SingletonList(Of StatementSyntax)(FlagUnsupportedStatements(node, "C# Fixed is not support by VB", CommentOutOriginalStatements:=True))
            End Function

            Public Overrides Function VisitForEachStatement(node As CSS.ForEachStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim variableDeclarator As VariableDeclaratorSyntax
                Dim asClause As SimpleAsClauseSyntax = Nothing
                Dim forEachVariableToken As SyntaxToken = GenerateSafeVBToken(node.Identifier, node, _semanticModel)
                Dim variableIdentifier As IdentifierNameSyntax = Factory.IdentifierName(forEachVariableToken)
                If node.Type.IsVar Then
                    Dim variableITypeSymbol As (_Error As Boolean, _ITypeSymbol As ITypeSymbol) = node.Expression.DetermineType(_semanticModel)
                    If variableITypeSymbol._Error = False Then
                        Dim type As TypeSyntax = variableITypeSymbol._ITypeSymbol.GetElementType
                        asClause = If(type IsNot Nothing, Factory.SimpleAsClause(type), Nothing)
                    End If
                Else
                    Dim VBType As TypeSyntax
                    If node.Type.IsKind(CS.SyntaxKind.IdentifierName) Then
                        VBType = Factory.IdentifierName(GenerateSafeVBToken(DirectCast(node.Type, CSS.IdentifierNameSyntax).Identifier,
                                                                              node,
                                                                              _semanticModel,
                                                                              IsQualifiedName:=False,
                                                                              IsTypeName:=True))
                    Else
                        VBType = DirectCast(node.Type.Accept(_nodesVisitor), TypeSyntax)
                    End If
                    asClause = Factory.SimpleAsClause(VBType)
                End If
                variableDeclarator = Factory.VariableDeclarator(Factory.SingletonSeparatedList(Factory.ModifiedIdentifier(forEachVariableToken).WithTrailingTrivia(VBSpaceTrivia)),
                                                                  asClause,
                                                                  initializer:=Nothing)

                Dim expression As ExpressionSyntax = DirectCast(node.Expression.Accept(_nodesVisitor), ExpressionSyntax).WithConvertedTrailingTriviaFrom(node.CloseParenToken)
                Dim OpenBraceLeadingTrivia As New SyntaxTriviaList
                Dim ClosingBraceTrailingTrivia As New SyntaxTriviaList
                Dim innerStmts As SyntaxList(Of StatementSyntax) = Me.ConvertBlock(node.Statement, OpenBraceLeadingTrivia, ClosingBraceTrailingTrivia)
                If OpenBraceLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    Stop
                End If
                Dim nextStatement As NextStatementSyntax = Factory.NextStatement().WithLeadingTrivia(ClosingBraceTrailingTrivia).WithTrailingEOL(RemoveLastLineContinuation:=True)
                If node.AwaitKeyword.IsKind(CS.SyntaxKind.None) Then

                    Dim ForEachStatement As ForEachStatementSyntax = Factory.ForEachStatement(variableDeclarator, expression).WithTrailingEOL(RemoveLastLineContinuation:=True)
                    Dim block As ForEachBlockSyntax = Factory.ForEachBlock(ForEachStatement.WithConvertedLeadingTriviaFrom(node.ForEachKeyword),
                                                                             innerStmts,
                                                                             nextStatement)
                    Return ReplaceOneStatementWithMarkedStatements(node, block)
                End If
                Dim messageEnumerator As SyntaxToken = Factory.Identifier(GetUniqueVariableNameInScope(node, "messageEnumerator", _semanticModel))
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
                            For Each parm As CSS.ParameterSyntax In methodStatement.ParameterList.Parameters
                                If parm.Type.ToString.EndsWith("CancellationToken", StringComparison.Ordinal) Then
                                    cancelExpression = Factory.ParseExpression(parm.Identifier.ValueText)
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
                Dim asyncblock As New List(Of StatementSyntax) From {
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
                '            _logger.LogInformation($"Incrementing count by {message.Count}");

                '            _counter.Increment(message.Count);
                '        }
                '    }
                whileStatements.AddRange(innerStmts)

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
                                                                    Factory.EndTryStatement)
                asyncblock.Add(tryBlock)
                Return ReplaceStatementsWithMarkedStatements(node,
                                                             Factory.List(asyncblock))
            End Function

            Public Overrides Function VisitForEachVariableStatement(node As CSS.ForEachVariableStatementSyntax) As SyntaxList(Of StatementSyntax)
                Return Factory.SingletonList(Of StatementSyntax)(FlagUnsupportedStatements(node,
                                                                                                 "For Each Variable Statement",
                                                                                                 CommentOutOriginalStatements:=True))
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
                Dim HasVariable As Boolean = False
                If Me.ConvertForToSimpleForNext(node, block, HasVariable) Then
                    Return ReplaceOneStatementWithMarkedStatements(node, block)
                Else
                    Dim OpenBraceleadingTrivia As New SyntaxTriviaList
                    Dim ClosingBraceTrailingTrivia As New SyntaxTriviaList
                    Dim stmts As SyntaxList(Of StatementSyntax) = Me.ConvertBlock(node.Statement, OpenBraceleadingTrivia, ClosingBraceTrailingTrivia).AddRange(node.Incrementors.Select(AddressOf Me.ConvertSingleBlock))

                    Dim trailingtrivia As SyntaxTriviaList = node.SecondSemicolonToken.CollectConvertedTokenTrivia(GetLeading:=True, GetTrailing:=True).ToSyntaxTriviaList
                    trailingtrivia = trailingtrivia.AddRange(node.CloseParenToken.CollectConvertedTokenTrivia(GetLeading:=True, GetTrailing:=True))
                    trailingtrivia = trailingtrivia.AddRange(node.GetBraces.Item1.CollectConvertedTokenTrivia(GetLeading:=True, GetTrailing:=True))
                    Dim condition As ExpressionSyntax = If(node.Condition Is Nothing, Factory.TrueLiteralExpression(TrueKeyword).WithTrailingTrivia(trailingtrivia), DirectCast(node.Condition.Accept(_nodesVisitor), ExpressionSyntax))

                    Dim WhileStatement As WhileStatementSyntax = Factory.WhileStatement(WhileKeyword.WithConvertedLeadingTriviaFrom(node.ForKeyword),
                                                                                                condition
                                                                                                ).WithTrailingEOL(RemoveLastLineContinuation:=True)
                    WhileStatement = CType(PrependStatementWithMarkedStatementTrivia(node, WhileStatement), WhileStatementSyntax)
                    Dim EndWhileStatement As EndBlockStatementSyntax = Factory.EndWhileStatement.WithLeadingTrivia(ClosingBraceTrailingTrivia).WithConvertedTrailingTriviaFrom(node.GetBraces.Item2)
                    block = Factory.WhileBlock(WhileStatement, stmts, EndWhileStatement)
                    Dim Statements As SyntaxList(Of StatementSyntax) = Factory.List(node.Initializers.Select(AddressOf Me.ConvertSingleBlock)).Add(block)
                    If HasVariable Then
                        Statements = Statements.Insert(0, node.Declaration.Accept(Me).First.WithConvertedTrailingTriviaFrom(node.FirstSemicolonToken).WithTrailingEOL(RemoveLastLineContinuation:=True))
                    End If
                    Return ReplaceStatementsWithMarkedStatements(node, Statements)
                End If

            End Function

            Public Overrides Function VisitGotoStatement(node As CSS.GotoStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim labelNameToken As LabelSyntax
                If node.IsKind(CS.SyntaxKind.GotoCaseStatement, CS.SyntaxKind.GotoDefaultStatement) Then
                    If _blockInfo.Count = 0 Then Throw New InvalidOperationException("GoTo Case/GoTo Default outside switch Is illegal!")
                    Dim labelExpression As VisualBasicSyntaxNode = If(node.Expression?.Accept(_nodesVisitor), Factory.ElseCaseClause())
                    _blockInfo.Peek().GotoCaseExpressions.Add(labelExpression)
                    labelNameToken = Factory.Label(VB.SyntaxKind.IdentifierLabel, Me.MakeGotoSwitchLabel(labelExpression))
                Else
                    labelNameToken = Factory.Label(VB.SyntaxKind.IdentifierLabel, GenerateSafeVBToken(DirectCast(node.Expression, CSS.IdentifierNameSyntax).Identifier, node, _semanticModel))
                End If

                Return Factory.SingletonList(Of StatementSyntax)(Factory.GoToStatement(labelNameToken).WithConvertedTriviaFrom(node).WithTrailingEOL(RemoveLastLineContinuation:=True))
            End Function

            Public Overrides Function VisitIfStatement(node As CSS.IfStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim stmt As StatementSyntax = Nothing
                Try

                    If node.Else Is Nothing AndAlso Me.TryConvertIfNotNullRaiseEvent(node, stmt) Then
                        Return Factory.SingletonList(stmt)
                    End If

                    Dim name As IdentifierNameSyntax = Nothing
                    Dim arguments As New List(Of ArgumentSyntax)()
                    Dim ListOfElseIfBlocks As New List(Of ElseIfBlockSyntax)()
                    Dim ElseBlock As ElseBlockSyntax = Nothing
                    Dim openBraceLeadingTrivia As New SyntaxTriviaList
                    Dim closeBraceTrailingTrivia As New SyntaxTriviaList
                    Me.CollectElseBlocks(node, ListOfElseIfBlocks, ElseBlock, openBraceLeadingTrivia, closeBraceTrailingTrivia)

                    Dim OpenParenToken As SyntaxToken = node.OpenParenToken

                    Dim IfKeywordWithTrivia As SyntaxToken = IfKeyword.
                                                WithConvertedTriviaFrom(node.IfKeyword).
                                                WithAppendedTrailingTrivia(ConvertTriviaList(OpenParenToken.LeadingTrivia)).
                                                WithAppendedTrailingTrivia(ConvertTriviaList(OpenParenToken.TrailingTrivia))
                    IfKeywordWithTrivia = IfKeywordWithTrivia.WithModifiedTokenTrivia(LeadingToken:=True, AfterEOL:=False)

                    Dim StatementTrailingTrivia As SyntaxTriviaList
                    StatementTrailingTrivia = CollectConvertedTokenTrivia(node.CloseParenToken, GetLeading:=True, GetTrailing:=True)
                    If StatementTrailingTrivia.Any AndAlso Not StatementTrailingTrivia(0).IsEndOfLine Then
                        StatementTrailingTrivia = StatementTrailingTrivia.Insert(0, VBEOLTrivia)
                    End If
                    Dim ConditionWithTrivia As ExpressionSyntax = DirectCast(node.Condition.Accept(_nodesVisitor), ExpressionSyntax).AdjustNodeTrivia(SeparatorFollows:=True)

                    If node.Statement.IsKind(CS.SyntaxKind.EmptyStatement) Then
                        StatementTrailingTrivia = StatementTrailingTrivia.InsertRange(0, ConvertTriviaList(DirectCast(node.Statement, CSS.EmptyStatementSyntax).SemicolonToken.TrailingTrivia))
                    End If
                    Dim IfStatement As IfStatementSyntax = Factory.IfStatement(IfKeywordWithTrivia,
                                                                                ConditionWithTrivia,
                                                                                ThenKeyword
                                                                               ).WithTrailingTrivia(StatementTrailingTrivia).
                                                                               WithTrailingEOL(RemoveLastLineContinuation:=True)

                    Dim Braces As (OpenBrace As SyntaxToken, CloseBrace As SyntaxToken) = node.Statement.GetBraces
                    Dim OpenBrace As SyntaxToken = Braces.OpenBrace
                    Dim CloseBrace As SyntaxToken = Braces.CloseBrace
                    Dim EndIfStatement As EndBlockStatementSyntax = Factory.EndIfStatement(EndKeyword, IfKeyword).WithConvertedTriviaFrom(CloseBrace)
                    Dim ElseIfBlocks As SyntaxList(Of ElseIfBlockSyntax) = Factory.List(ListOfElseIfBlocks)
                    If ElseBlock IsNot Nothing AndAlso ElseBlock.Statements.Any AndAlso ElseBlock.Statements(0).IsKind(VB.SyntaxKind.EmptyStatement) Then
                        EndIfStatement = EndIfStatement.WithLeadingTrivia(ElseBlock.GetTrailingTrivia)
                        ElseBlock = Factory.ElseBlock(Factory.ElseStatement(), statements:=Nothing)
                    End If
                    Dim Statements As SyntaxList(Of StatementSyntax) = Me.ConvertBlock(node.Statement, openBraceLeadingTrivia, closeBraceTrailingTrivia)
                    If closeBraceTrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                        EndIfStatement = EndIfStatement.WithLeadingTrivia(closeBraceTrailingTrivia)
                    End If
                    If Statements.LastOrDefault.IsKind(VB.SyntaxKind.EmptyStatement) Then
                        If EndsWithSimilarTrivia(EndIfStatement.GetLeadingTrivia, Statements.LastOrDefault.GetLeadingTrivia) Then
                            Statements = Statements.RemoveAt(Statements.Count - 1)
                        End If
                    End If
                    If TypeOf node.Statement Is CSS.BlockSyntax Then
                        stmt = Factory.MultiLineIfBlock(IfStatement,
                                                          Statements,
                                                          ElseIfBlocks,
                                                          ElseBlock,
                                                          EndIfStatement.WithTrailingEOL(RemoveLastLineContinuation:=True)
                                                         )
                    Else
                        Dim IsInvocationExpression As Boolean = False
                        If node.Statement.IsKind(CS.SyntaxKind.ExpressionStatement) Then
                            Dim ExpressionStatement As CSS.ExpressionStatementSyntax = DirectCast(node.Statement, CSS.ExpressionStatementSyntax)
                            If ExpressionStatement.Expression.IsKind(CS.SyntaxKind.InvocationExpression) Then
                                IsInvocationExpression = ExpressionStatement.Expression.DescendantNodes().OfType(Of CSS.ConditionalExpressionSyntax).Any
                            End If
                        End If
                        If ListOfElseIfBlocks.Any() OrElse IsInvocationExpression OrElse Not node.Statement.IsSimpleStatement Then
                            stmt = Factory.MultiLineIfBlock(IfStatement,
                                                            Statements,
                                                            ElseIfBlocks,
                                                            ElseBlock,
                                                            EndIfStatement.WithTrailingEOL(RemoveLastLineContinuation:=True)
                                                            )
                        Else
                            If IfStatement.GetTrailingTrivia.ContainsEOLTrivia Then
                                Dim IFBlockStatements As SyntaxList(Of StatementSyntax) = Statements
                                stmt = Factory.MultiLineIfBlock(IfStatement,
                                                                IFBlockStatements,
                                                                ElseIfBlocks,
                                                                ElseBlock,
                                                                EndIfStatement.WithTrailingEOL(RemoveLastLineContinuation:=True)
                                                                )
                            Else
                                If ElseBlock IsNot Nothing OrElse (Statements.Count = 1 AndAlso TypeOf Statements(0) Is EmptyStatementSyntax) Then
                                    stmt = Factory.MultiLineIfBlock(IfStatement,
                                                                    Statements,
                                                                    elseIfBlocks:=Nothing,
                                                                    ElseBlock,
                                                                    EndIfStatement.WithTrailingEOL(RemoveLastLineContinuation:=True)
                                                                    )
                                Else
                                    stmt = Factory.SingleLineIfStatement(IfKeywordWithTrivia,
                                                                         ConditionWithTrivia,
                                                                         ThenKeyword,
                                                                         Statements,
                                                                         elseClause:=Nothing
                                                                         )
                                End If
                            End If
                        End If
                    End If
                Catch ex As Exception
                    Stop
                    Throw
                End Try

                Return ReplaceOneStatementWithMarkedStatements(node, stmt)
            End Function

            Public Overrides Function VisitLabeledStatement(node As CSS.LabeledStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim OpenBraceLeadingTrivia As New SyntaxTriviaList
                Dim ClosingBraceTrailingTrivia As New SyntaxTriviaList
                Dim Statements As SyntaxList(Of StatementSyntax) = Me.ConvertBlock(node.Statement, OpenBraceLeadingTrivia, ClosingBraceTrailingTrivia)
                If OpenBraceLeadingTrivia.Any OrElse ClosingBraceTrailingTrivia.Any Then
                    Stop
                End If
                Return Factory.SingletonList(Of StatementSyntax)(Factory.LabelStatement(GenerateSafeVBToken(node.Identifier, node, _semanticModel))).AddRange(Statements)
            End Function

            Public Overrides Function VisitLocalDeclarationStatement(node As CSS.LocalDeclarationStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, _nodesVisitor.IsModule, TokenContext.Local)
                If modifiers.Count = 0 Then
                    modifiers.Add(DimKeyword)
                End If
                Dim LeadingTrivia As New SyntaxTriviaList
                Dim declarators As SeparatedSyntaxList(Of VariableDeclaratorSyntax) = node.Declaration.RemodelVariableDeclaration(_nodesVisitor, _semanticModel, IsFieldDeclaration:=False, LeadingTrivia)
                Dim localDeclarationStatement As LocalDeclarationStatementSyntax = Factory.LocalDeclarationStatement(
                                                        Factory.TokenList(modifiers),
                                                        declarators).
                                                    WithLeadingTrivia(LeadingTrivia).
                                                    WithAppendedTrailingTrivia(ConvertTriviaList(node.SemicolonToken.TrailingTrivia)).WithTrailingEOL(RemoveLastLineContinuation:=True) ' this picks up end of line comments
                ' Don't repeat leading comments
                If Not localDeclarationStatement.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    localDeclarationStatement = localDeclarationStatement.WithConvertedLeadingTriviaFrom(node)
                End If

                Dim StmtList As New List(Of StatementSyntax) From {
                    localDeclarationStatement
                }
                If node.SemicolonToken.HasLeadingTrivia And node.SemicolonToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    StmtList.Add(Factory.EmptyStatement.WithConvertedLeadingTriviaFrom(node.SemicolonToken))
                End If

                Return ReplaceStatementsWithMarkedStatements(node, StmtList)
            End Function

            Public Overrides Function VisitLocalFunctionStatement(node As CSS.LocalFunctionStatementSyntax) As SyntaxList(Of StatementSyntax)
                If node.AncestorsAndSelf().OfType(Of CSS.LocalFunctionStatementSyntax).Count > 1 Then
                    Return Factory.SingletonList(Of StatementSyntax)(Factory.EmptyStatement)
                End If
                Dim localFunctionSymbol As IMethodSymbol = CType(_semanticModel.GetDeclaredSymbol(node), IMethodSymbol)
                Dim indexOfFirstReferencingStatement As Integer = -1
                Dim StatementWithIssues As CSS.StatementSyntax = Nothing
                If TypeOf node.Parent Is CSS.BlockSyntax Then
                    Dim _parent As CSS.BlockSyntax = CType(node.Parent, CSS.BlockSyntax)
                    indexOfFirstReferencingStatement = _parent.Statements.TakeWhile(Function(s As CSS.StatementSyntax) Not ContainsLocalFunctionReference(s, localFunctionSymbol, _semanticModel)).Count()
                    If indexOfFirstReferencingStatement = _parent.Statements.Count Then
                        indexOfFirstReferencingStatement = 0
                    Else
                        For Each e As IndexClass(Of CSS.StatementSyntax) In _parent.Statements.WithIndex
                            If TypeOf e.Value Is CSS.ReturnStatementSyntax Then
                                If indexOfFirstReferencingStatement > e.Index Then
                                    indexOfFirstReferencingStatement = e.Index
                                End If
                                Exit For
                            End If
                        Next
                    End If
                    StatementWithIssues = _parent.Statements(indexOfFirstReferencingStatement)
                ElseIf TypeOf node.Parent Is CSS.SwitchSectionSyntax Then
                    Dim _parent As CSS.SwitchSectionSyntax = CType(node.Parent, CSS.SwitchSectionSyntax)
                    indexOfFirstReferencingStatement = _parent.Statements.TakeWhile(Function(s As CSS.StatementSyntax) Not ContainsLocalFunctionReference(s, localFunctionSymbol, _semanticModel)).Count()
                    If indexOfFirstReferencingStatement >= _parent.Statements.Count Then
                        StatementWithIssues = CType(_parent.Parent, CSS.StatementSyntax)
                    Else
                        StatementWithIssues = _parent.Statements(indexOfFirstReferencingStatement)
                    End If
                ElseIf TypeOf node.Parent Is CSS.GlobalStatementSyntax Then
                    StatementWithIssues = node
                Else
                    Stop
                End If

                Dim parameters As SeparatedSyntaxList(Of CSS.ParameterSyntax) = node.ParameterList.Parameters
                Dim vbParameters As New SeparatedSyntaxList(Of ParameterSyntax)
                If parameters.Any Then
                    For index As Integer = 0 To parameters.Count - 1
                        vbParameters = vbParameters.Add(DirectCast(parameters(index).Accept(_nodesVisitor), ParameterSyntax))
                    Next
                End If
                Dim parameterList As ParameterListSyntax = Factory.ParameterList(OpenParenToken, vbParameters, CloseParenToken)
                Dim returnsVoid As Boolean = node.ReturnType Is Nothing OrElse node.ReturnType.ToString = "void"
                Dim lambdaHeader As LambdaHeaderSyntax
                Dim Kind As VB.SyntaxKind
                Dim endblock As EndBlockStatementSyntax
                Dim csBraces As (OpenBrace As SyntaxToken, CloseBrace As SyntaxToken) = node.Body.GetBraces

                Dim TypeList As New List(Of TypeSyntax)
                For Each parameter As ParameterSyntax In parameterList.Parameters
                    TypeList.Add(parameter.AsClause.Type)
                Next
                Dim returnType As TypeSyntax
                Dim modifiers As SyntaxTokenList = Factory.TokenList(ConvertModifiers(node.Modifiers, _nodesVisitor.IsModule, TokenContext.LocalFunction))
                If node.DescendantNodes().OfType(Of CSS.YieldStatementSyntax).Any Then
                    modifiers = modifiers.Add(IteratorKeyword)
                End If

                If returnsVoid Then
                    Kind = VB.SyntaxKind.MultiLineSubLambdaExpression
                    lambdaHeader = Factory.SubLambdaHeader(attributeLists:=Nothing, modifiers, parameterList, asClause:=Nothing)
                    endblock = Factory.EndSubStatement().WithConvertedTriviaFrom(csBraces.CloseBrace)
                Else
                    returnType = DirectCast(node.ReturnType.Accept(_nodesVisitor), TypeSyntax).WithLeadingTrivia(VBSpaceTrivia)
                    TypeList.Add(returnType)
                    Kind = VB.SyntaxKind.MultiLineSubLambdaExpression
                    lambdaHeader = Factory.FunctionLambdaHeader(attributeLists:=Nothing, modifiers, parameterList, Factory.SimpleAsClause(returnType))
                    endblock = Factory.EndFunctionStatement().WithConvertedTriviaFrom(csBraces.CloseBrace)
                End If
                Dim body As New SyntaxList(Of StatementSyntax)
                If node.Body IsNot Nothing Then
                    body = ReplaceStatementsWithMarkedStatements(node, node.Body.Accept(Me))
                Else
                    If node.ExpressionBody?.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        body = node.ExpressionBody.WithoutLeadingTrivia.GetExpressionBodyStatements(_nodesVisitor)
                    Else
                        body = node.ExpressionBody.GetExpressionBodyStatements(_nodesVisitor)
                    End If
                End If
                If TypeOf node.Parent Is CSS.GlobalStatementSyntax Then
                    Return body
                End If
                Dim nameToken As SyntaxToken = GenerateSafeVBToken(node.Identifier, node, _semanticModel)
                Dim asClause As SimpleAsClauseSyntax = Nothing
                If TypeList.Any Then
                    Dim typeArguments As TypeArgumentListSyntax = Factory.TypeArgumentList(Factory.SeparatedList(TypeList)).NormalizeWhitespace
                    Dim genericName As TypeSyntax = Factory.GenericName(Factory.Identifier(If(returnsVoid, "Action", "Func")), typeArguments)
                    asClause = Factory.SimpleAsClause(genericName)
                Else
                    asClause = Factory.SimpleAsClause(Factory.IdentifierName("Action"))
                End If
                Dim lambdaExpression As MultiLineLambdaExpressionSyntax = Factory.MultiLineLambdaExpression(
                                                            Kind,
                                                            lambdaHeader.WithoutLeadingTrivia.WithTrailingEOL(RemoveLastLineContinuation:=True),
                                                            body,
                                                            endblock)
                Dim initializer As EqualsValueSyntax = Factory.EqualsValue(lambdaExpression)
                Dim dimStatement As LocalDeclarationStatementSyntax = FactoryDimStatement(nameToken, asClause, initializer).WithConvertedTrailingTriviaFrom(node).
                                        WithPrependedLeadingTrivia(Factory.CommentTrivia($"' TODO Check: Local function was replaced with Lambda"))
                If StatementWithIssues.Equals(node) Then
                    Return Factory.SingletonList(Of StatementSyntax)(dimStatement.WithConvertedTriviaFrom(node))
                End If
                Return Factory.SingletonList(Of StatementSyntax)(dimStatement)
            End Function

            Public Overrides Function VisitLockStatement(node As CSS.LockStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim LockStatement As SyncLockStatementSyntax = Factory.SyncLockStatement(DirectCast(node.Expression?.Accept(_nodesVisitor), ExpressionSyntax)).
                                                                                            WithConvertedLeadingTriviaFrom(node)
                Dim csBraces As (OpenBrace As SyntaxToken, CloseBrace As SyntaxToken)
                Dim OpenBraceLeadingTrivia As SyntaxTriviaList = CollectConvertedTokenTrivia(csBraces.OpenBrace, GetLeading:=True, GetTrailing:=True)
                Dim ClosingBraceTrailingTrivia As SyntaxTriviaList = CollectConvertedTokenTrivia(csBraces.CloseBrace, GetLeading:=True, GetTrailing:=True)

                Dim Statements As SyntaxList(Of StatementSyntax) = Me.ConvertBlock(node.Statement, OpenBraceLeadingTrivia, ClosingBraceTrailingTrivia)
                Dim EndSyncLockStatement As EndBlockStatementSyntax = Factory.EndSyncLockStatement.WithLeadingTrivia(ClosingBraceTrailingTrivia).WithAppendedTrailingTrivia(ConvertTriviaList(node.GetTrailingTrivia))
                Dim LockBlock As SyncLockBlockSyntax = Factory.SyncLockBlock(LockStatement.WithTrailingEOL(RemoveLastLineContinuation:=True), Statements, EndSyncLockStatement)
                Return ReplaceOneStatementWithMarkedStatements(node, LockBlock)
            End Function

            Public Overrides Function VisitReturnStatement(node As CSS.ReturnStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim stmt As StatementSyntax
                Dim MovedLeadingTrivia As New SyntaxTriviaList
                Dim Expression As ExpressionSyntax
                If node.Expression Is Nothing Then
                    stmt = Factory.ReturnStatement.
                                        WithConvertedTriviaFrom(node).
                                        WithTrailingEOL(RemoveLastLineContinuation:=True)
                Else
                    Expression = DirectCast(node.Expression.Accept(_nodesVisitor), ExpressionSyntax)
                    ' TODO Handle ref expressions
                    If Expression IsNot Nothing Then
                        MovedLeadingTrivia = MovedLeadingTrivia.AddRange(ConvertTriviaList(node.GetLeadingTrivia))
                        If Expression.HasLeadingTrivia AndAlso Not Expression.GetLeadingTrivia.ContainsEndIfTrivia Then
                            MovedLeadingTrivia = MovedLeadingTrivia.AddRange(Expression.GetLeadingTrivia)
                        Else
                            node.AddMarker(Factory.EmptyStatement.WithLeadingTrivia(Expression.GetLeadingTrivia), StatementHandlingOption.AppendEmptyStatement, AllowDuplicates:=False)
                        End If
                        Expression = Expression?.WithLeadingTrivia(VBSpaceTrivia)
                    End If
                    stmt = Factory.ReturnStatement(Expression?.WithLeadingTrivia(VBSpaceTrivia)).
                                            WithLeadingTrivia(MovedLeadingTrivia).
                                            WithTrailingTrivia(ConvertTriviaList(node.SemicolonToken.TrailingTrivia)).
                                            WithTrailingEOL(RemoveLastLineContinuation:=True)
                End If
                Return ReplaceOneStatementWithMarkedStatements(node, stmt)
            End Function

            Public Overrides Function VisitSwitchStatement(node As CSS.SwitchStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim stmt As StatementSyntax
                _blockInfo.Push(New BlockInfo())
                Try
                    Dim blocks As List(Of CaseBlockSyntax) = node.Sections.Select(AddressOf Me.ConvertSwitchSection).ToList
                    Dim OrderedBlocks As New List(Of CaseBlockSyntax)
                    Dim CaseElseIndex As Integer = -1
                    For blockIndex As Integer = 0 To blocks.Count - 1
                        If blocks(blockIndex).IsKind(VB.SyntaxKind.CaseElseBlock) Then
                            CaseElseIndex = blockIndex
                        Else
                            OrderedBlocks.Add(blocks(blockIndex))
                        End If
                    Next
                    If CaseElseIndex >= 0 Then
                        OrderedBlocks.Add(blocks(CaseElseIndex))
                    End If
                    Dim Expression As ExpressionSyntax = Nothing
                    If TypeOf node.Sections(0).Labels(0) Is CSS.CasePatternSwitchLabelSyntax Then
                        Dim PatternSwitch As CSS.CasePatternSwitchLabelSyntax = DirectCast(node.Sections(0).Labels(0), CSS.CasePatternSwitchLabelSyntax)
                        If TypeOf PatternSwitch.Pattern Is CSS.DeclarationPatternSyntax Then
                            Expression = Factory.TrueLiteralExpression(TrueKeyword)
                        ElseIf TypeOf PatternSwitch.Pattern Is CSS.VarPatternSyntax Then
                            ' TODO Handle
                        ElseIf TypeOf PatternSwitch.Pattern Is CSS.ConstantPatternSyntax Then
                            ' TODO Handle
                        ElseIf TypeOf PatternSwitch.Pattern Is CSS.RecursivePatternSyntax Then
                            ' TODO Handle
                        Else
                            Stop
                        End If
                    End If
                    Dim EndSelectStatement As EndBlockStatementSyntax = Factory.EndBlockStatement(
                                                                    VB.SyntaxKind.EndSelectStatement,
                                                                    EndKeyword,
                                                                    SelectKeyword).
                                                                        WithConvertedTriviaFrom(node.CloseBraceToken)
                    stmt = Factory.SelectBlock(
                                Factory.SelectStatement(SelectKeyword,
                                                            CaseKeyword,
                                                            If(Expression, DirectCast(node.Expression.Accept(_nodesVisitor), ExpressionSyntax))
                                                            ).WithTrailingEOL(RemoveLastLineContinuation:=True),
                                Factory.List(nodes:=Me.AddLabels(blocks:=OrderedBlocks.ToArray,
                                               gotoLabels:=_blockInfo.Peek().GotoCaseExpressions)
                                               ),
                                EndSelectStatement
                                )
                    _switchCount += 1
                Finally
                    _blockInfo.Pop()
                End Try
                Return ReplaceOneStatementWithMarkedStatements(node, stmt.WithConvertedLeadingTriviaFrom(node))
            End Function

            Public Overrides Function VisitThrowStatement(node As CSS.ThrowStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim stmt As StatementSyntax = If(node.Expression Is Nothing,
                            Factory.ThrowStatement().WithTrailingEOL(RemoveLastLineContinuation:=True),
                            Factory.ThrowStatement(DirectCast(node.Expression.Accept(_nodesVisitor), ExpressionSyntax)).WithTrailingEOL(RemoveLastLineContinuation:=True))
                Return ReplaceStatementsWithMarkedStatements(node, Factory.SingletonList(stmt.WithConvertedTriviaFrom(node)))
            End Function

            Public Overrides Function VisitTryStatement(node As CSS.TryStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim OpenBraceLeadingTrivia As New SyntaxTriviaList
                Dim ClosingBraceTrailingTrivia As New SyntaxTriviaList
                Dim TryStatement As TryStatementSyntax = Factory.TryStatement()
                Dim CatchBlocks As SyntaxList(Of CatchBlockSyntax) = Factory.List(node.Catches.IndexedSelect(AddressOf Me.ConvertCatchClause))
                Dim newTriviaList As New SyntaxTriviaList
                For blockIndex As Integer = 0 To CatchBlocks.Count - 1
                    Dim CatchBlock As CatchBlockSyntax = CatchBlocks(blockIndex)
                    If CatchBlock.Statements.Any AndAlso CatchBlock.Statements(0).IsKind(VB.SyntaxKind.EmptyStatement) Then
                        Dim TempTriviaList As SyntaxTriviaList = CatchBlock.Statements(0).GetTrailingTrivia
                        newTriviaList = newTriviaList.AddRange(CatchBlock.GetBraces().Item2.LeadingTrivia)
                        CatchBlocks.Replace(CatchBlocks(blockIndex), Factory.CatchBlock(CatchBlock.CatchStatement.WithLeadingTrivia(newTriviaList)))
                        newTriviaList = TempTriviaList
                    Else
                        CatchBlocks = CatchBlocks.Replace(CatchBlock, CatchBlock.WithLeadingTrivia(newTriviaList))
                        newTriviaList = New SyntaxTriviaList
                    End If
                Next
                If CatchBlocks.Count - 1 >= 0 Then
                    CatchBlocks = CatchBlocks.Replace(CatchBlocks(0), CatchBlocks(0).WithConvertedTriviaFrom(node.Block.CloseBraceToken).WithTrailingEOL(RemoveLastLineContinuation:=True))
                End If
                Dim FinallyBlock As FinallyBlockSyntax = Nothing
                If node.Finally IsNot Nothing Then
                    Dim FinallyStatements As SyntaxList(Of StatementSyntax) =
                        Me.ConvertBlock(node.Finally.Block,
                                        OpenBraceLeadingTrivia,
                                        ClosingBraceTrailingTrivia
                                        )
                    FinallyBlock = Factory.FinallyBlock(FinallyStatements).WithPrependedLeadingTrivia(newTriviaList)
                    newTriviaList = New SyntaxTriviaList
                    If FinallyBlock.Statements(0).IsKind(VB.SyntaxKind.EmptyStatement) Then
                        newTriviaList = newTriviaList.AddRange(FinallyBlock.Statements(0).GetTrailingTrivia)
                        FinallyBlock = FinallyBlock.WithTrailingTrivia(VBEOLTrivia)
                    End If
                End If
                Dim EndTryStatement As EndBlockStatementSyntax = Factory.EndTryStatement()
                If node.Catches.Any Then
                    EndTryStatement = EndTryStatement.WithConvertedTriviaFrom(node.Catches.Last.Block.GetBraces.Item2)
                Else
                    EndTryStatement = EndTryStatement.WithLeadingTrivia(ClosingBraceTrailingTrivia)
                End If
                If newTriviaList.Any Then
                    EndTryStatement = EndTryStatement.WithLeadingTrivia(newTriviaList)
                End If
                Dim TryBlockStatements As SyntaxList(Of StatementSyntax) = Me.ConvertBlock(node.Block, OpenBraceLeadingTrivia, ClosingBraceTrailingTrivia)
                If ClosingBraceTrailingTrivia.Any Then
                    If FinallyBlock IsNot Nothing Then
                        FinallyBlock = FinallyBlock.WithPrependedLeadingTrivia(ClosingBraceTrailingTrivia)
                    End If
                End If

                If CatchBlocks.Any AndAlso EndsWithSimilarTrivia(TryBlockStatements.LastOrDefault.GetLeadingTrivia, CatchBlocks.FirstOrDefault.GetLeadingTrivia) Then
                    If TryBlockStatements.LastOrDefault.IsKind(VB.SyntaxKind.EmptyStatement) Then
                        TryBlockStatements = TryBlockStatements.RemoveAt(TryBlockStatements.Count - 1)
                    End If
                End If

                Dim block As TryBlockSyntax = Factory.TryBlock(TryStatement,
                                                                TryBlockStatements,
                                                                CatchBlocks,
                                                                FinallyBlock,
                                                                EndTryStatement
                                                                )
                Return Factory.SingletonList(Of StatementSyntax)(block.WithConvertedTriviaFrom(node))
            End Function

            Public Overrides Function VisitUnsafeStatement(node As CSS.UnsafeStatementSyntax) As SyntaxList(Of StatementSyntax)
                Return Factory.SingletonList(Of StatementSyntax)(FlagUnsupportedStatements(node, "Unsafe statement", CommentOutOriginalStatements:=True))
            End Function

            Public Overrides Function VisitUsingStatement(node As CSS.UsingStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim UsingStatement As UsingStatementSyntax
                Dim OpenBraceLeadingTrivia As New SyntaxTriviaList
                Dim ClosingBraceTrailingTrivia As New SyntaxTriviaList
                Dim LeadingTrivia As New SyntaxTriviaList
                If node.Declaration Is Nothing Then
                    Dim UsingBlock As UsingBlockSyntax
                    LeadingTrivia = LeadingTrivia.AddRange(ConvertTriviaList(node.GetLeadingTrivia))
                    If node.Expression IsNot Nothing AndAlso node.Expression.IsKind(CS.SyntaxKind.ConditionalAccessExpression) Then
                        Dim csConditionalAccessExpression As CSS.ConditionalAccessExpressionSyntax = DirectCast(node.Expression, CSS.ConditionalAccessExpressionSyntax)
                        Dim VB_ConditionalAccessExpression As VisualBasicSyntaxNode = csConditionalAccessExpression.Expression.Accept(_nodesVisitor)
                        Dim Condition As BinaryExpressionSyntax = Factory.IsNotExpression(left:=CType(VB_ConditionalAccessExpression, ExpressionSyntax),
                                                                                                right:=NothingExpression)
                        Dim IfStatement As IfStatementSyntax = Factory.IfStatement(Condition)
                        UsingStatement = Factory.UsingStatement(Factory.ParseExpression($"{VB_ConditionalAccessExpression}{csConditionalAccessExpression.WhenNotNull.Accept(_nodesVisitor)}"), Factory.SeparatedList(Of VariableDeclaratorSyntax)())
                        UsingBlock = Factory.UsingBlock(UsingStatement.WithTrailingEOL(RemoveLastLineContinuation:=True), Me.ConvertBlock(node.Statement, OpenBraceLeadingTrivia, ClosingBraceTrailingTrivia)).WithLeadingTrivia(LeadingTrivia)
                        Dim IfStatementBlock As MultiLineIfBlockSyntax = Factory.MultiLineIfBlock(IfStatement, Factory.SingletonList(Of StatementSyntax)(UsingBlock), elseIfBlocks:=Nothing, elseBlock:=Nothing).WithLeadingTrivia(LeadingTrivia)
                        Return ReplaceOneStatementWithMarkedStatements(node, IfStatementBlock)
                    Else
                        UsingStatement = Factory.UsingStatement(DirectCast(node.Expression?.Accept(_nodesVisitor), ExpressionSyntax), Factory.SeparatedList(Of VariableDeclaratorSyntax)())
                    End If
                Else
                    UsingStatement = Factory.UsingStatement(expression:=Nothing, node.Declaration.RemodelVariableDeclaration(_nodesVisitor, _semanticModel, IsFieldDeclaration:=False, LeadingTrivia))
                End If

                Dim EndUsing As EndBlockStatementSyntax = Factory.EndUsingStatement.WithConvertedTriviaFrom(node.Statement.GetBraces.Item2)
                Return ReplaceOneStatementWithMarkedStatements(node,
                                                               Factory.UsingBlock(UsingStatement.WithTrailingEOL(RemoveLastLineContinuation:=True),
                                                                                  Me.ConvertBlock(node.Statement,
                                                                                                  OpenBraceLeadingTrivia,
                                                                                                  ClosingBraceTrailingTrivia),
                                                                                  EndUsing).WithLeadingTrivia(LeadingTrivia))
            End Function

            Public Overrides Function VisitVariableDeclaration(node As CSS.VariableDeclarationSyntax) As SyntaxList(Of StatementSyntax)
                Dim leadingTrivia As New SyntaxTriviaList
                Dim vbType As TypeSyntax = DirectCast(node.Type.Accept(_nodesVisitor), TypeSyntax)
                If vbType.HasLeadingTrivia Then
                    leadingTrivia = leadingTrivia.AddRange(vbType.GetLeadingTrivia)
                    vbType = vbType.WithLeadingTrivia(VBSpaceTrivia)
                End If
                Dim collectedCommentTrivia As SyntaxTriviaList
                Dim declaratorsWithoutInitializers As New List(Of CSS.VariableDeclaratorSyntax)()
                Dim vbDeclarators As New List(Of VariableDeclaratorSyntax)
                For Each e As IndexClass(Of CSS.VariableDeclaratorSyntax) In node.Variables.WithIndex
                    Dim v As CSS.VariableDeclaratorSyntax = e.Value
                    If v.Initializer Is Nothing Then
                        declaratorsWithoutInitializers.Add(v.WithTrailingTrivia(collectedCommentTrivia))
                        Continue For
                    Else
                        Dim AsClause As SimpleAsClauseSyntax = If(node.Type.IsVar OrElse node.Type.IsKind(CS.SyntaxKind.RefType), Nothing, Factory.SimpleAsClause(vbType))
                        Dim Value As ExpressionSyntax = DirectCast(v.Initializer.Value.Accept(_nodesVisitor), ExpressionSyntax)
                        If Value Is Nothing Then
                            Value = Factory.IdentifierName("HandleRefExpression").WithConvertedTriviaFrom(v.Initializer.Value)
                        End If
                        If Value.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                            leadingTrivia = leadingTrivia.AddRange(Value.GetLeadingTrivia)
                        End If
                        Dim Initializer As EqualsValueSyntax = Factory.EqualsValue(Value.WithLeadingTrivia(VBSpaceTrivia))
                        Dim Names As SeparatedSyntaxList(Of ModifiedIdentifierSyntax) = Factory.SingletonSeparatedList(CType(v.Accept(_nodesVisitor), ModifiedIdentifierSyntax))
                        Dim Declator As VariableDeclaratorSyntax = Factory.VariableDeclarator(
                                                                                                Names,
                                                                                                AsClause,
                                                                                                Initializer
                                                                                                )
                        If Declator.HasTrailingTrivia Then
                            Dim FoundEOL As Boolean = False
                            Dim NonCommentTrailingTrivia As New SyntaxTriviaList
                            For Each t As SyntaxTrivia In Declator.GetTrailingTrivia
                                Select Case t.RawKind
                                    Case VB.SyntaxKind.EndOfLineTrivia
                                        FoundEOL = True
                                    Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                                        collectedCommentTrivia = collectedCommentTrivia.Add(t)
                                    Case VB.SyntaxKind.WhitespaceTrivia
                                        collectedCommentTrivia = collectedCommentTrivia.Add(t)
                                        NonCommentTrailingTrivia = NonCommentTrailingTrivia.Add(t)
                                    Case Else
                                        ' Directives are ignored but the results are converted. Disabled Text is deleted
                                        'Stop
                                End Select
                            Next
                            If FoundEOL Then
                                collectedCommentTrivia = collectedCommentTrivia.Add(VBEOLTrivia)
                                Declator = Declator.WithTrailingTrivia(collectedCommentTrivia)
                                collectedCommentTrivia = New SyntaxTriviaList
                            Else
                                Declator = Declator.WithTrailingTrivia(NonCommentTrailingTrivia)
                            End If
                            If e.IsLast Then
                                If Not Declator.HasTrailingTrivia OrElse Not Declator.GetTrailingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                    Declator = Declator.WithAppendedEOL
                                End If
                            End If
                        End If
                        vbDeclarators.Add(Declator)
                    End If
                Next
                If declaratorsWithoutInitializers.Any Then
                    Stop
                End If
                Dim LocalDeclarationStatement As LocalDeclarationStatementSyntax = Factory.LocalDeclarationStatement(
                    Factory.TokenList(DimModifier),
                    Factory.SeparatedList(vbDeclarators)).WithoutTrivia.WithLeadingTrivia(leadingTrivia).WithAppendedTrailingTrivia(ConvertTriviaList(node.GetTrailingTrivia)) ' this picks up end of line comments
                ' Don't repeat leading comments
                If Not LocalDeclarationStatement.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    LocalDeclarationStatement = LocalDeclarationStatement.WithConvertedLeadingTriviaFrom(node)
                End If
                Return Factory.SingletonList(Of StatementSyntax)(LocalDeclarationStatement)
            End Function

            Public Overrides Function VisitWhileStatement(node As CSS.WhileStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim condition As ExpressionSyntax = DirectCast(node.Condition.Accept(_nodesVisitor), ExpressionSyntax)
                Dim OpenBraceLeadingTrivia As New SyntaxTriviaList
                Dim ClosingBraceTrailingTrivia As New SyntaxTriviaList
                Dim WhileStatements As SyntaxList(Of StatementSyntax) = Me.ConvertBlock(node.Statement, OpenBraceLeadingTrivia, ClosingBraceTrailingTrivia)
                If OpenBraceLeadingTrivia.Any Then
                    WhileStatements = WhileStatements.Replace(WhileStatements.First, WhileStatements.First.WithPrependedLeadingTrivia(OpenBraceLeadingTrivia))
                    OpenBraceLeadingTrivia = Nothing
                End If
                Dim EndWhileStatement As EndBlockStatementSyntax = Factory.EndWhileStatement().WithLeadingTrivia(ClosingBraceTrailingTrivia)
                Dim block As WhileBlockSyntax = Factory.WhileBlock(Factory.WhileStatement(condition).WithConvertedLeadingTriviaFrom(node.WhileKeyword).WithTrailingEOL(RemoveLastLineContinuation:=True), WhileStatements, EndWhileStatement)
                Return ReplaceOneStatementWithMarkedStatements(node, block)
            End Function

            Public Overrides Function VisitYieldStatement(node As CSS.YieldStatementSyntax) As SyntaxList(Of StatementSyntax)
                IsInterator = True
                Dim stmt As StatementSyntax
                If node.Expression Is Nothing Then
                    stmt = Factory.ReturnStatement.WithTrailingEOL(RemoveLastLineContinuation:=True)
                Else
                    stmt = Factory.YieldStatement(DirectCast(node.Expression.Accept(_nodesVisitor), ExpressionSyntax)).WithTrailingEOL(RemoveLastLineContinuation:=True)
                End If
                Return ReplaceOneStatementWithMarkedStatements(node, stmt.WithConvertedTriviaFrom(node))
            End Function

            Private Class BlockInfo
                Public ReadOnly GotoCaseExpressions As New List(Of VisualBasicSyntaxNode)()
            End Class

        End Class

    End Class

End Namespace
