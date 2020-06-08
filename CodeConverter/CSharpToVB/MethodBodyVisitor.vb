' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.Diagnostics.CodeAnalysis
Imports System.Runtime.InteropServices
Imports System.Text

Imports CSharpToVBCodeConverter.Utilities

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CSharp
Imports Microsoft.CodeAnalysis.Simplification
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

Imports CS = Microsoft.CodeAnalysis.CSharp

Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax

Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory

Namespace CSharpToVBCodeConverter.ToVisualBasic

    Partial Public NotInheritable Class CSharpConverter

        Friend Class MethodBodyVisitor
            Inherits CSharpSyntaxVisitor(Of SyntaxList(Of StatementSyntax))

            Private ReadOnly _blockInfo As Stack(Of BlockInfo) = New Stack(Of BlockInfo)()
            Private ReadOnly _literalExpression As ExpressionSyntax = VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal(1))
            Private ReadOnly _nodesVisitor As NodesVisitor
            Private ReadOnly _semanticModel As SemanticModel
            ' currently only works with switch blocks

            Private _switchCount As Integer = 0

            Public Sub New(semanticModel As SemanticModel, nodesVisitor As NodesVisitor)
                _semanticModel = semanticModel
                _nodesVisitor = nodesVisitor
            End Sub

            Public Property IsInterator As Boolean

            Private Shared Function ContainsLocalFunctionReference(syntax As SyntaxNode, localFunctionSymbol As IMethodSymbol, _semanticModel As SemanticModel) As Boolean
                Return syntax.DescendantNodes().
                                OfType(Of CSS.SimpleNameSyntax)().
                                Any(Function(name As CSS.SimpleNameSyntax) name.Identifier.ValueText = localFunctionSymbol.Name AndAlso
                                SymbolEqualityComparer.[Default].Equals(_semanticModel.GetSymbolInfo(name).Symbol, localFunctionSymbol))
            End Function

            Private Shared Function GetPossibleEventName(expression As CSS.ExpressionSyntax) As String
                Dim ident As CSS.IdentifierNameSyntax = TryCast(expression, CSS.IdentifierNameSyntax)
                If ident IsNot Nothing Then Return ident.Identifier.Text

                If TypeOf expression Is CSS.ParenthesizedExpressionSyntax Then
                    expression = DirectCast(expression, CSS.ParenthesizedExpressionSyntax).Expression
                Else
                    Return Nothing
                End If
                Dim fre As CSS.MemberAccessExpressionSyntax = TryCast(expression, CSS.MemberAccessExpressionSyntax)
                If fre IsNot Nothing AndAlso fre.Expression.IsKind(CS.SyntaxKind.ThisExpression) Then Return fre.Name.Identifier.Text
                Return Nothing
            End Function

            Private Shared Function IsSimpleStatement(statement As CSS.StatementSyntax) As Boolean
                Return TypeOf statement Is CSS.ExpressionStatementSyntax OrElse
                    TypeOf statement Is CSS.BreakStatementSyntax OrElse
                    TypeOf statement Is CSS.ContinueStatementSyntax OrElse
                    TypeOf statement Is CSS.ReturnStatementSyntax OrElse
                    TypeOf statement Is CSS.YieldStatementSyntax OrElse
                    TypeOf statement Is CSS.ThrowStatementSyntax
            End Function

            Private Iterator Function AddLabels(blocks As CaseBlockSyntax(), gotoLabels As List(Of VisualBasicSyntaxNode)) As IEnumerable(Of CaseBlockSyntax)
                For Each _block As CaseBlockSyntax In blocks
                    Dim block As CaseBlockSyntax = _block
                    For Each caseClause As CaseClauseSyntax In block.CaseStatement.Cases
                        Dim expression As VisualBasicSyntaxNode = If(TypeOf caseClause Is ElseCaseClauseSyntax, DirectCast(caseClause, VisualBasicSyntaxNode), (DirectCast(caseClause, SimpleCaseClauseSyntax)).Value)
                        If gotoLabels.Any(Function(label As VisualBasicSyntaxNode) label.IsEquivalentTo(expression.WithoutTrivia)) Then
                            block = block.WithStatements(block.Statements.Insert(0, VBFactory.LabelStatement(MakeGotoSwitchLabel(expression))))
                        End If
                    Next

                    Yield block
                Next
            End Function

            Private Sub CollectElseBlocks(node As CSS.IfStatementSyntax, elseIfBlocks As List(Of ElseIfBlockSyntax), ByRef elseBlock As ElseBlockSyntax, ByRef OpenBraceTrailingTrivia As List(Of SyntaxTrivia), ByRef CloseBraceLeadingTrivia As List(Of SyntaxTrivia))
                If node.[Else] Is Nothing Then
                    Exit Sub
                End If

                If TypeOf node.[Else].Statement Is CSS.IfStatementSyntax Then
                    Dim [elseIf] As CSS.IfStatementSyntax = DirectCast(node.[Else].Statement, CSS.IfStatementSyntax)
                    Dim elseIFKeywordWithTrivia As SyntaxToken = ElseIfKeyword.WithLeadingTrivia(ConvertTrivia(node.[Else].Statement.GetLeadingTrivia)).WithPrependedLeadingTrivia(ConvertTrivia(node.[Else].GetLeadingTrivia))
                    Dim newThenTrailingTrivia As New List(Of SyntaxTrivia)
                    Dim condition As ExpressionSyntax = DirectCast([elseIf].Condition.Accept(_nodesVisitor), ExpressionSyntax)
                    newThenTrailingTrivia.AddRange(condition.GetTrailingTrivia)
                    If node.CloseParenToken.HasLeadingTrivia AndAlso node.CloseParenToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        newThenTrailingTrivia.AddRange(ConvertTrivia(node.CloseParenToken.LeadingTrivia))
                    End If
                    Dim thenKeywordWithTrivia As SyntaxToken = ThenKeyword.WithTrailingTrivia(newThenTrailingTrivia)
                    Dim elseIfStatement As ElseIfStatementSyntax = VBFactory.ElseIfStatement(elseIFKeywordWithTrivia, condition.WithTrailingTrivia(SpaceTrivia), thenKeywordWithTrivia)
                    Dim elseIfBlock As ElseIfBlockSyntax = VBFactory.ElseIfBlock(elseIfStatement.WithTrailingEOL, ConvertBlock([elseIf].Statement, OpenBraceTrailingTrivia, CloseBraceLeadingTrivia))
                    If Not OpenBraceTrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                        OpenBraceTrailingTrivia.Clear()
                    End If
                    If Not CloseBraceLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        CloseBraceLeadingTrivia.Clear()
                    End If
                    elseIfBlocks.Add(elseIfBlock)
                    CollectElseBlocks([elseIf], elseIfBlocks, elseBlock, OpenBraceTrailingTrivia, CloseBraceLeadingTrivia)
                Else
                    Dim statements As SyntaxList(Of StatementSyntax) = ConvertBlock(node.[Else].Statement, OpenBraceTrailingTrivia, CloseBraceLeadingTrivia)
                    Dim trailingTriviaList As New List(Of SyntaxTrivia)
                    If node.Else.Statement.GetBraces.Item1.HasTrailingTrivia Then
                        trailingTriviaList.AddRange(ConvertTrivia(node.Else.Statement.GetBraces.Item1.TrailingTrivia))
                    End If
                    Dim elseStatement As ElseStatementSyntax = VBFactory.ElseStatement(ElseKeyword.WithConvertedLeadingTriviaFrom(node.Else.ElseKeyword)).WithTrailingTrivia(trailingTriviaList)

                    elseBlock = VBFactory.ElseBlock(elseStatement, statements).WithPrependedLeadingTrivia(OpenBraceTrailingTrivia).WithAppendedTrailingTrivia(CloseBraceLeadingTrivia)
                    OpenBraceTrailingTrivia.Clear()
                    CloseBraceLeadingTrivia.Clear()
                End If
            End Sub

            Private Function ConvertBlock(node As CSS.StatementSyntax, ByRef openBraceTrailiningTrivia As List(Of SyntaxTrivia), ByRef closeBraceLeadingTrivia As List(Of SyntaxTrivia)) As SyntaxList(Of StatementSyntax)
                Dim csBraces As (OpenBrace As SyntaxToken, CloseBrace As SyntaxToken) = node.GetBraces
                Dim csOpenBrace As SyntaxToken = If(csBraces.OpenBrace = Nothing, New SyntaxToken, csBraces.OpenBrace)
                Dim openBraceLeadingTrivia As New List(Of SyntaxTrivia)
                openBraceLeadingTrivia.AddRange(ConvertTrivia(csOpenBrace.LeadingTrivia))
                openBraceTrailiningTrivia.AddRange(ConvertTrivia(csOpenBrace.TrailingTrivia))
                Dim csCloseBrace As SyntaxToken = If(csBraces.CloseBrace = Nothing, New SyntaxToken, csBraces.CloseBrace)
                closeBraceLeadingTrivia.AddRange(ConvertTrivia(csCloseBrace.LeadingTrivia))
                Select Case True
                    Case TypeOf node Is CSS.BlockSyntax
                        Dim nodeBlock As CSS.BlockSyntax = DirectCast(node, CSS.BlockSyntax)
                        Dim vbBlock As New List(Of StatementSyntax)
                        For Each e As IndexClass(Of CSS.StatementSyntax) In nodeBlock.Statements.WithIndex
                            Dim Statements As List(Of StatementSyntax) = e.Value.Accept(Me).ToList
                            If e.IsFirst AndAlso Statements.Any Then
                                Statements(0) = Statements(0).WithPrependedLeadingTrivia(openBraceLeadingTrivia)
                            End If
                            vbBlock.AddRange(Statements)
                        Next

                        If vbBlock.Count = 0 Then
                            vbBlock.Add(VBFactory.EmptyStatement.WithConvertedTriviaFrom(nodeBlock))
                        Else
                            If Not (vbBlock.First.IsKind(VB.SyntaxKind.EmptyStatement) OrElse
                                   (vbBlock.First.IsKind(VB.SyntaxKind.TryBlock) AndAlso vbBlock.First.GetLeadingTrivia.Count = 1)
                                    ) Then
                                vbBlock.Item(0) = vbBlock(0).WithLeadingTrivia(ConvertTrivia(nodeBlock.Statements(0).GetLeadingTrivia)).WithPrependedLeadingTrivia(ConvertTrivia(node.GetLeadingTrivia))
                            End If
                            vbBlock.Item(vbBlock.Count - 1) = vbBlock.Last.WithTrailingTrivia(ConvertTrivia(nodeBlock.Statements.Last.GetTrailingTrivia)).WithTrailingEOL
                        End If
                        Return VBFactory.List(vbBlock)
                    Case TypeOf node Is CSS.EmptyStatementSyntax
                        Return VBFactory.List(Of StatementSyntax)()
                End Select
                If TypeOf node IsNot CSS.LocalFunctionStatementSyntax Then
                    Return node.Accept(Me)
                End If
                Return VBFactory.SingletonList(Of StatementSyntax)(VBFactory.EmptyStatement)
            End Function

            Private Function ConvertCatchClause(index As Integer, catchClause As CSS.CatchClauseSyntax) As CatchBlockSyntax
                Dim openBraceTrailingTrivia As New List(Of SyntaxTrivia)
                Dim closingBraceLeadingTrivia As New List(Of SyntaxTrivia)
                Dim vbStatements As SyntaxList(Of StatementSyntax) = ConvertBlock(catchClause.Block, openBraceTrailingTrivia, closingBraceLeadingTrivia)

                If catchClause.Declaration Is Nothing Then
                    Return VBFactory.CatchBlock(VBFactory.CatchStatement().WithTrailingTrivia(VBEOLTrivia).WithAppendedTrailingTrivia(closingBraceLeadingTrivia), vbStatements)
                End If
                If openBraceTrailingTrivia.Any OrElse closingBraceLeadingTrivia.Any Then
                    vbStatements = vbStatements.Replace(vbStatements(0), vbStatements(0).WithPrependedLeadingTrivia(openBraceTrailingTrivia))
                    Dim laststatement As Integer = vbStatements.Count - 1
                    vbStatements = vbStatements.Replace(vbStatements(laststatement), vbStatements(laststatement).WithAppendedTrailingTrivia(closingBraceLeadingTrivia).WithTrailingEOL)
                End If
                Dim type As TypeSyntax = DirectCast(catchClause.Declaration.Type.Accept(_nodesVisitor), TypeSyntax)
                Dim simpleTypeName As String
                simpleTypeName = If(TypeOf type Is QualifiedNameSyntax, DirectCast(type, QualifiedNameSyntax).Right.ToString(), type.ToString())
                Dim identifier As SyntaxToken = If(catchClause.Declaration.Identifier.IsKind(CS.SyntaxKind.None),
                                                        VBFactory.Identifier($"__unused{simpleTypeName}{index + 1}__"),
                                                        GenerateSafeVBToken(catchClause.Declaration.Identifier))
                Dim whenClause As CatchFilterClauseSyntax = If(catchClause.Filter Is Nothing, Nothing, VBFactory.CatchFilterClause(filter:=DirectCast(catchClause.Filter.FilterExpression.Accept(_nodesVisitor), ExpressionSyntax)))
                Dim catchStatement As CatchStatementSyntax = VBFactory.CatchStatement(
                                                                    VBFactory.IdentifierName(identifier),
                                                                    VBFactory.SimpleAsClause(type),
                                                                    whenClause).
                                                                    WithConvertedLeadingTriviaFrom(catchClause)
                If Not catchStatement.HasTrailingTrivia Then
                    catchStatement = catchStatement.WithTrailingTrivia(VBEOLTrivia)
                ElseIf catchStatement.GetTrailingTrivia.Last <> VBEOLTrivia Then
                    catchStatement = catchStatement.WithTrailingTrivia(VBEOLTrivia)
                End If
                Return VBFactory.CatchBlock(catchStatement, vbStatements)
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
                If iterator.OperatorToken.IsKind(VB.SyntaxKind.MinusToken) Then
                    [step] = -[step]
                End If
                Dim condition As CSS.BinaryExpressionSyntax = TryCast(node.Condition, CSS.BinaryExpressionSyntax)
                If condition Is Nothing OrElse Not (TypeOf condition.Left Is CSS.IdentifierNameSyntax) Then
                    Return False
                End If
                If (DirectCast(condition.Left, CSS.IdentifierNameSyntax)).Identifier.IsEquivalentTo(iteratorIdentifier.Identifier) Then
                    Return False
                End If
                Dim ToValue As ExpressionSyntax
                If iterator.IsKind(VB.SyntaxKind.SubtractAssignmentStatement) Then
                    If condition.IsKind(CS.SyntaxKind.GreaterThanOrEqualExpression) OrElse condition.IsKind(CS.SyntaxKind.NotEqualsExpression) Then
                        ToValue = DirectCast(condition.Right.Accept(_nodesVisitor), ExpressionSyntax)
                    ElseIf condition.IsKind(CS.SyntaxKind.GreaterThanExpression) Then
                        ToValue = VBFactory.BinaryExpression(VB.SyntaxKind.AddExpression,
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
                        ToValue = VBFactory.BinaryExpression(VB.SyntaxKind.SubtractExpression,
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
                    Dim names As SeparatedSyntaxList(Of ModifiedIdentifierSyntax) = VBFactory.SingletonSeparatedList(
                        VBFactory.ModifiedIdentifier(
                            GenerateSafeVBToken(v.Identifier,
                                                IsQualifiedName:=False,
                                                IsTypeName:=False)
                                                        )
                                                    )
                    ControlVariable = VBFactory.VariableDeclarator(names,
                                                                asClause:=If(node.Declaration.Type.IsVar, Nothing, VBFactory.SimpleAsClause(DirectCast(node.Declaration.Type.Accept(_nodesVisitor), TypeSyntax))),
                                                                initializer:=Nothing)
                Else
                    Dim initializer As CSS.AssignmentExpressionSyntax = TryCast(node.Initializers.FirstOrDefault(), CSS.AssignmentExpressionSyntax)
                    If initializer Is Nothing OrElse Not initializer.IsKind(CS.SyntaxKind.SimpleAssignmentExpression) Then
                        Return False
                    End If
                    If Not (TypeOf initializer.Left Is CSS.IdentifierNameSyntax) Then
                        Return False
                    End If
                    If (DirectCast(initializer.Left, CSS.IdentifierNameSyntax)).Identifier.IsEquivalentTo(iteratorIdentifier.Identifier) Then
                        Return False
                    End If
                    ControlVariable = initializer.Left.Accept(_nodesVisitor)
                    FromValue = DirectCast(initializer.Right.Accept(_nodesVisitor), ExpressionSyntax)
                End If

                Dim ForStatementBlockTrailingTrivia As New List(Of SyntaxTrivia)
                If node.CloseParenToken.HasLeadingTrivia Then
                    ForStatementBlockTrailingTrivia.AddRange(ConvertTrivia(node.CloseParenToken.LeadingTrivia))
                End If
                If node.CloseParenToken.HasTrailingTrivia Then
                    ForStatementBlockTrailingTrivia.AddRange(ConvertTrivia(node.CloseParenToken.TrailingTrivia))
                End If

                Dim StatementFirstToken As SyntaxToken = node.Statement.GetFirstToken
                Dim OpenBraceTrailingTrivia As New List(Of SyntaxTrivia)

                Dim ClosingBraceLeadingTrivia As New List(Of SyntaxTrivia)
                Dim Statements As SyntaxList(Of StatementSyntax) = ConvertBlock(node.Statement, OpenBraceTrailingTrivia, ClosingBraceLeadingTrivia)

                Dim ForStatementTrailingTrivia As New List(Of SyntaxTrivia)
                If StatementFirstToken.IsKind(CS.SyntaxKind.OpenBraceToken) Then
                    If StatementFirstToken.TrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                        ForStatementTrailingTrivia.AddRange(ConvertTrivia(StatementFirstToken.TrailingTrivia))
                    End If
                End If

                If ForStatementTrailingTrivia.Any AndAlso ForStatementTrailingTrivia.ContainsCommentOrDirectiveTrivia AndAlso Not ForStatementTrailingTrivia(0).IsEndOfLine Then
                    ForStatementTrailingTrivia.Insert(0, VBEOLTrivia)
                End If

                Dim StepClause As ForStepClauseSyntax = If([step] = 1,
                                                                Nothing,
                                                                VBFactory.ForStepClause(
                                                                            VBFactory.LiteralExpression(
                                                                                            VB.SyntaxKind.NumericLiteralExpression,
                                                                                            VBFactory.Literal([step])
                                                                                            )
                                                                            )
                                                                )
                Dim ForStatement As ForStatementSyntax = VBFactory.ForStatement(ForKeyword.WithConvertedLeadingTriviaFrom(node.ForKeyword),
                                                                           ControlVariable,
                                                                           EqualsToken,
                                                                           FromValue,
                                                                           ToKeyword,
                                                                           ToValue,
                                                                           StepClause
                                                                           ).WithTrailingTrivia(ForStatementTrailingTrivia).WithTrailingEOL
                block = VBFactory.ForBlock(ForStatement,
                                            Statements,
                                            VBFactory.NextStatement().WithTrailingTrivia(ClosingBraceLeadingTrivia).WithTrailingEOL
                                            )
                Return True
            End Function

            Private Function ConvertSingleBlock(node As CSS.ExpressionSyntax) As StatementSyntax
                Dim exprNode As VisualBasicSyntaxNode = Nothing
                Dim NewLeadingTrivia As New List(Of SyntaxTrivia)

                If TypeOf node Is CSS.AssignmentExpressionSyntax Then
                    Dim csAssignment As CSS.AssignmentExpressionSyntax = DirectCast(node, CSS.AssignmentExpressionSyntax)
                    If csAssignment.Left.IsKind(CS.SyntaxKind.ParenthesizedExpression) Then
                        Dim csLeft As CSS.ParenthesizedExpressionSyntax = DirectCast(csAssignment.Left, CSS.ParenthesizedExpressionSyntax)
                        Dim LeftExpression As ExpressionSyntax = CType(csLeft.Expression.Accept(_nodesVisitor), ExpressionSyntax)
                        Dim RightExpression As ExpressionSyntax = DirectCast(csAssignment.Right.Accept(_nodesVisitor), ExpressionSyntax)
                        If csAssignment.IsKind(CS.SyntaxKind.SimpleAssignmentExpression) Then
                            exprNode = VBFactory.SimpleAssignmentStatement(LeftExpression, RightExpression).
                                                         WithConvertedTriviaFrom(node)
                            NewLeadingTrivia.AddRange(node.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=False, "Parenthesized Expression Assignment"))
                        End If
                    End If
                ElseIf TypeOf node Is CSS.PostfixUnaryExpressionSyntax Then
                    Dim CSPostFixUnaryExpression As CSS.PostfixUnaryExpressionSyntax = DirectCast(node, CSS.PostfixUnaryExpressionSyntax)
                    If TypeOf CSPostFixUnaryExpression.Operand Is CSS.ParenthesizedExpressionSyntax Then
                        Dim csOperand As CSS.ParenthesizedExpressionSyntax = DirectCast(CSPostFixUnaryExpression.Operand, CSS.ParenthesizedExpressionSyntax)
                        Dim kind As VB.SyntaxKind = ConvertCSExpressionsKindToVBKind(CS.CSharpExtensions.Kind(node))
                        Dim OperandExpression As ExpressionSyntax = DirectCast(csOperand.Expression.Accept(_nodesVisitor), ExpressionSyntax)
                        exprNode = VBFactory.AssignmentStatement(ConvertCSExpressionsKindToVBKind(CS.CSharpExtensions.Kind(node)),
                                                                OperandExpression,
                                                                ExpressionKindToOperatorToken(kind),
                                                                _literalExpression)
                        NewLeadingTrivia.AddRange(node.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=False, "Parenthesized Expression Assignment"))
                    End If
                End If
                If exprNode Is Nothing Then
                    exprNode = node.Accept(_nodesVisitor)
                    If exprNode.IsKind(VB.SyntaxKind.TryBlock) Then
                        Dim TmpTrivia As New List(Of SyntaxTrivia)
                        TmpTrivia.AddRange(exprNode.GetLeadingTrivia)
                        If TmpTrivia.Count = 2 AndAlso TmpTrivia(0).IsComment AndAlso TmpTrivia(1).IsEndOfLine Then
                            NewLeadingTrivia.AddRange(TmpTrivia)
                        Else
                            NewLeadingTrivia.AddRange(ConvertTrivia(node.GetLeadingTrivia))
                        End If
                    Else
                        NewLeadingTrivia.AddRange(ConvertTrivia(node.GetLeadingTrivia))
                    End If
                End If
                Dim NewTrailingTrivia As New List(Of SyntaxTrivia)
                NewTrailingTrivia.AddRange(exprNode.GetTrailingTrivia)
                exprNode = exprNode.WithoutTrivia
                If Not (TypeOf exprNode Is StatementSyntax) Then
                    Select Case True
                        Case TypeOf exprNode Is ObjectCreationExpressionSyntax
                            exprNode = FactoryDimStatement(GetUniqueVariableNameInScope(node, "tempVar", _semanticModel),
                                                          VBFactory.AsNewClause(DirectCast(exprNode, NewExpressionSyntax)),
                                                          initializer:=Nothing)
                        Case TypeOf exprNode Is InvocationExpressionSyntax
                            exprNode = If(exprNode.GetFirstToken.IsKind(VB.SyntaxKind.NewKeyword), VBFactory.CallStatement(DirectCast(exprNode, ExpressionSyntax)), DirectCast(VBFactory.ExpressionStatement(DirectCast(exprNode, ExpressionSyntax)), VisualBasicSyntaxNode))
                        Case Else
                            exprNode = VBFactory.ExpressionStatement(DirectCast(exprNode, ExpressionSyntax))
                    End Select
                End If
                Return DirectCast(exprNode, StatementSyntax).WithLeadingTrivia(NewLeadingTrivia).WithTrailingTrivia(NewTrailingTrivia).WithTrailingEOL
            End Function

            Private Function ConvertSingleExpression(node As CSS.ExpressionSyntax) As List(Of StatementSyntax)
                Dim StatementList As New List(Of StatementSyntax)
                Dim OneStatement As VisualBasicSyntaxNode = Nothing
                Dim NewLeadingTrivia As New List(Of SyntaxTrivia)

                If TypeOf node Is CSS.AssignmentExpressionSyntax Then
                    Dim csAssignment As CSS.AssignmentExpressionSyntax = DirectCast(node, CSS.AssignmentExpressionSyntax)
                    If csAssignment.Left.IsKind(CS.SyntaxKind.ParenthesizedExpression) Then
                        Dim csLeft As CSS.ParenthesizedExpressionSyntax = DirectCast(csAssignment.Left, CSS.ParenthesizedExpressionSyntax)
                        Dim LeftExpression As ExpressionSyntax = CType(csLeft.Expression.Accept(_nodesVisitor), ExpressionSyntax)
                        Dim RightExpression As ExpressionSyntax = DirectCast(csAssignment.Right.Accept(_nodesVisitor), ExpressionSyntax)
                        If csAssignment.IsKind(CS.SyntaxKind.SimpleAssignmentExpression) Then
                            OneStatement = VBFactory.SimpleAssignmentStatement(LeftExpression, RightExpression).
                                                         WithConvertedTriviaFrom(node)
                            NewLeadingTrivia.AddRange(node.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=False, "Parenthesized Expression Assignment"))
                        End If
                    End If
                ElseIf TypeOf node Is CSS.PostfixUnaryExpressionSyntax Then
                    Dim CSPostFixUnaryExpression As CSS.PostfixUnaryExpressionSyntax = DirectCast(node, CSS.PostfixUnaryExpressionSyntax)
                    If TypeOf CSPostFixUnaryExpression.Operand Is CSS.ParenthesizedExpressionSyntax Then
                        Dim csOperand As CSS.ParenthesizedExpressionSyntax = DirectCast(CSPostFixUnaryExpression.Operand, CSS.ParenthesizedExpressionSyntax)
                        Dim kind As VB.SyntaxKind = ConvertCSExpressionsKindToVBKind(CS.CSharpExtensions.Kind(node))
                        Dim OperandExpression As ExpressionSyntax = DirectCast(csOperand.Expression.Accept(_nodesVisitor), ExpressionSyntax)
                        OneStatement = VBFactory.AssignmentStatement(ConvertCSExpressionsKindToVBKind(CS.CSharpExtensions.Kind(node)),
                                                                OperandExpression,
                                                                ExpressionKindToOperatorToken(kind),
                                                                _literalExpression)
                        NewLeadingTrivia.AddRange(node.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=False, "Parenthesized Expression Assignment"))
                    End If
                End If
                Dim NewTrailingTrivia As New List(Of SyntaxTrivia)
                If OneStatement Is Nothing Then
                    NewLeadingTrivia.AddRange(ConvertTrivia(node.GetLeadingTrivia))
                    OneStatement = node.Accept(_nodesVisitor)
                    NewTrailingTrivia.AddRange(OneStatement.GetTrailingTrivia)
                    If OneStatement.IsKind(VB.SyntaxKind.TryBlock) Then
                        Dim tryLeadingTrivia As SyntaxTriviaList = OneStatement.GetLeadingTrivia
                        If tryLeadingTrivia.Any Then
                            If tryLeadingTrivia(0).IsKind(VB.SyntaxKind.CommentTrivia) AndAlso tryLeadingTrivia(0).ToFullString = "' TODO: This Try Block can be removed" Then
                                StatementList.AddRange(DirectCast(OneStatement, TryBlockSyntax).Statements)
                                StatementList(0) = StatementList(0).WithLeadingTrivia(NewLeadingTrivia)
                                Dim Last As Integer = StatementList.Count - 1
                                StatementList(Last) = StatementList(Last).WithTrailingTrivia(NewTrailingTrivia).WithTrailingEOL
                                Return StatementList
                            End If
                        End If
                    End If
                End If
                NewLeadingTrivia.Clear()
                NewLeadingTrivia.AddRange(ConvertTrivia(node.GetLeadingTrivia))
                OneStatement = OneStatement.WithoutTrivia
                If Not TypeOf OneStatement Is StatementSyntax Then
                    Select Case True
                        Case TypeOf OneStatement Is ObjectCreationExpressionSyntax
                            OneStatement = FactoryDimStatement(GetUniqueVariableNameInScope(node, "tempVar", _semanticModel),
                                                              VBFactory.AsNewClause(DirectCast(OneStatement, NewExpressionSyntax)),
                                                              initializer:=Nothing)
                        Case TypeOf OneStatement Is InvocationExpressionSyntax
                            OneStatement = If(OneStatement.GetFirstToken.IsKind(VB.SyntaxKind.NewKeyword), VBFactory.CallStatement(DirectCast(OneStatement, ExpressionSyntax)), DirectCast(VBFactory.ExpressionStatement(DirectCast(OneStatement, ExpressionSyntax)), VisualBasicSyntaxNode))
                        Case Else
                            OneStatement = VBFactory.ExpressionStatement(DirectCast(OneStatement, ExpressionSyntax))
                    End Select
                End If
                StatementList.AddRange(ReplaceOneStatementWithMarkedStatements(node, DirectCast(OneStatement, StatementSyntax).WithLeadingTrivia(NewLeadingTrivia).WithTrailingTrivia(NewTrailingTrivia).WithTrailingEOL))
                Return StatementList
            End Function

            Private Function ConvertSwitchSection(section As CSS.SwitchSectionSyntax) As CaseBlockSyntax
                Dim NewLeadingStatements As New List(Of StatementSyntax)
                If section.Labels.OfType(Of CSS.DefaultSwitchLabelSyntax)().Any() Then
                    Return VBFactory.CaseElseBlock(VBFactory.CaseElseStatement(VBFactory.ElseCaseClause()), ConvertSwitchSectionBlock(section, NewLeadingStatements))
                End If
                Dim LabelList As New List(Of CaseClauseSyntax)
                Dim vbLabelLeadingTrivia As New List(Of SyntaxTrivia)
                vbLabelLeadingTrivia.AddRange(ConvertTrivia(section.GetLeadingTrivia))
                Dim csLabelTrailingTrivia As New List(Of SyntaxTrivia)
                ' Find Case leading space
                For Each CaseLabel As CSS.SwitchLabelSyntax In section.Labels
                    Dim CaseLabelExpression As ExpressionSyntax
                    Dim CaseLabelWhenExpression As ExpressionSyntax
                    Select Case True
                        Case TypeOf CaseLabel Is CSS.CaseSwitchLabelSyntax
                            CaseLabelExpression = DirectCast(CType(CaseLabel, CSS.CaseSwitchLabelSyntax).Value.Accept(_nodesVisitor), ExpressionSyntax)
                            CaseLabelWhenExpression = Nothing
                            'If Not TriviaIsIdentical(VBFactory.TriviaList(vbLabelLeadingTrivia), ConvertTrivia(CaseLabel.GetLeadingTrivia).ToList) Then
                            '    vbLabelLeadingTrivia.AddRange(ConvertTrivia(CaseLabel.GetLeadingTrivia))
                            'End If
                            csLabelTrailingTrivia.AddRange(CaseLabel.GetTrailingTrivia)
                        Case TypeOf CaseLabel Is CSS.CasePatternSwitchLabelSyntax
                            Dim PatternLabel As CSS.CasePatternSwitchLabelSyntax = DirectCast(CaseLabel, CSS.CasePatternSwitchLabelSyntax)
                            Dim Identifier As SyntaxToken
                            CaseLabelWhenExpression = CType(PatternLabel.WhenClause?.Accept(_nodesVisitor), ExpressionSyntax)
                            If TypeOf PatternLabel.Pattern Is CSS.ConstantPatternSyntax Then
                                Dim ConstantPattern As CSS.ConstantPatternSyntax = DirectCast(PatternLabel.Pattern, CSS.ConstantPatternSyntax)
                                CaseLabelExpression = DirectCast(ConstantPattern.Expression.Accept(_nodesVisitor), ExpressionSyntax).WithConvertedLeadingTriviaFrom(PatternLabel)
                                csLabelTrailingTrivia.AddRange(CaseLabel.GetTrailingTrivia)
                            ElseIf TypeOf PatternLabel.Pattern Is CSS.DeclarationPatternSyntax Then
                                Dim Pattern As CSS.DeclarationPatternSyntax = DirectCast(PatternLabel.Pattern, CSS.DeclarationPatternSyntax)
                                Dim Type As TypeSyntax = DirectCast(Pattern.Type.Accept(_nodesVisitor), TypeSyntax)
                                If TypeOf Pattern.Designation Is CSS.SingleVariableDesignationSyntax Then
                                    Identifier = GenerateSafeVBToken(DirectCast(Pattern.Designation, CSS.SingleVariableDesignationSyntax).Identifier,
                                                                     IsQualifiedName:=False,
                                                                     IsTypeName:=False)
                                ElseIf TypeOf Pattern.Designation Is CSS.DiscardDesignationSyntax Then
                                Else
                                    Stop
                                End If

                                Dim SwitchExpression1 As ExpressionSyntax = DirectCast(DirectCast(section.Parent, CSS.SwitchStatementSyntax).Expression.Accept(_nodesVisitor), ExpressionSyntax)
                                If TypeOf Pattern.Designation Is CSS.DiscardDesignationSyntax Then
                                    CaseLabelExpression = VBFactory.TypeOfIsExpression(SwitchExpression1, CType(Pattern.Type.Accept(_nodesVisitor), TypeSyntax))
                                Else
                                    NewLeadingStatements.Add(FactoryDimStatement(Identifier,
                                                                                VBFactory.SimpleAsClause(Type),
                                                                                VBFactory.EqualsValue(VBFactory.CTypeExpression(SwitchExpression1, Type))
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
                                        CaseLabelExpression = VBFactory.IdentifierName("Else")
                                    End If
                                ElseIf VarPattern.Designation.IsKind(CS.SyntaxKind.SingleVariableDesignation) Then
                                    Identifier = GenerateSafeVBToken(DirectCast(VarPattern.Designation, CSS.SingleVariableDesignationSyntax).Identifier,
                                                                    IsQualifiedName:=False,
                                                                    IsTypeName:=False)
                                    NewLeadingStatements.Add(FactoryDimStatement(Identifier,
                                                                                VBFactory.SimpleAsClause(VBFactory.PredefinedType(ObjectKeyword)),
                                                                                VBFactory.EqualsValue(SwitchExpression1)).WithTrailingEOL)
                                    CaseLabelExpression = VBFactory.IdentifierName("Else")
                                Else
                                    CaseLabelExpression = Nothing
                                End If
                            ElseIf TypeOf PatternLabel.Pattern Is CSS.RecursivePatternSyntax Then
                                CaseLabelExpression = NothingExpression
                                vbLabelLeadingTrivia.AddRange(section.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=False, $"VB has no equivalent to the C# 'Recursive Pattern({PatternLabel.Pattern}) in 'case' statements"))
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
                        LabelList.Add(VBFactory.SimpleCaseClause(CaseLabelWhenExpression))
                    Else
                        If CaseLabelWhenExpression Is Nothing Then
                            LabelList.Add(VBFactory.SimpleCaseClause(CaseLabelExpression.WithoutLeadingTrivia))
                        Else
                            ' TODO use line continuation instead of space
                            LabelList.Add(VBFactory.SimpleCaseClause(CaseLabelExpression.With({SpaceTrivia}, {SpaceTrivia})))
                            NewLeadingStatements.Add(VBFactory.SingleLineIfStatement(VBFactory.UnaryExpression(VB.SyntaxKind.NotExpression, NotKeyword, CaseLabelExpression.With({SpaceTrivia}, {SpaceTrivia})),
                                                                                     VBFactory.SingletonList(Of StatementSyntax)(VBFactory.ExitSelectStatement),
                                                                                     elseClause:=Nothing
                                                                                     ).WithLeadingTrivia(CaseLabelExpression.GetLeadingTrivia).
                                                                                       WithTrailingEOL
                                                    )
                        End If
                    End If
                Next
                Dim CommentString As New StringBuilder
                For Each t As SyntaxTrivia In ConvertTrivia(csLabelTrailingTrivia)
                    Select Case t.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            CommentString.Append(t.ToString)
                        Case VB.SyntaxKind.EndOfLineTrivia
                            'ignore
                        Case VB.SyntaxKind.CommentTrivia
                            CommentString.Append(t.ToString.Trim.TrimStart("'"c).Trim)
                        Case Else
                            Stop
                    End Select
                Next
                Dim TrailingTrivia As New List(Of SyntaxTrivia)
                If CommentString.Length > 0 Then
                    TrailingTrivia.Add(VBFactory.CommentTrivia($" ' {CommentString}"))
                    TrailingTrivia.Add(VBEOLTrivia)
                End If
                Dim CaseStatement As CaseStatementSyntax = VBFactory.CaseStatement(VBFactory.SeparatedList(LabelList)).With(vbLabelLeadingTrivia, TrailingTrivia).WithTrailingEOL
                Return VBFactory.CaseBlock(CaseStatement, ConvertSwitchSectionBlock(section, NewLeadingStatements))
            End Function

            Private Function ConvertSwitchSectionBlock(section As CSS.SwitchSectionSyntax, Statements As List(Of StatementSyntax)) As SyntaxList(Of StatementSyntax)
                Dim lastStatement As CSS.StatementSyntax = section.Statements.LastOrDefault()
                Dim OpenBraceTrailingTrivia As New List(Of SyntaxTrivia)
                Dim ClosingBraceLeadingTrivia As New List(Of SyntaxTrivia)
                For Each s As CSS.StatementSyntax In section.Statements
                    If s Is lastStatement AndAlso TypeOf s Is CSS.BreakStatementSyntax Then
                        If lastStatement.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                            Statements.Add(VBFactory.EmptyStatement.WithConvertedTriviaFrom(lastStatement))
                        End If
                        Continue For
                    End If
                    Statements.AddRange(ConvertBlock(s, OpenBraceTrailingTrivia, ClosingBraceLeadingTrivia))
                    If OpenBraceTrailingTrivia.Any Then
                        Statements(0) = Statements.First.WithPrependedLeadingTrivia(OpenBraceTrailingTrivia)
                    End If
                    If ClosingBraceLeadingTrivia.Any Then
                        Statements(Statements.Count - 1) = Statements.Last.WithAppendedTrailingTrivia(ClosingBraceLeadingTrivia)
                    End If
                Next
                Return VBFactory.List(Statements)
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
                        ExpressionBuilder.Append("_")
                    End If
                Next
                Return ExpressionBuilder.ToString
            End Function

            Private Function TryConvertRaiseEvent(node As CSS.IfStatementSyntax, <Out> ByRef name As IdentifierNameSyntax, arguments As List(Of ArgumentSyntax)) As Boolean
                name = Nothing
                Dim condition As CSS.ExpressionSyntax = node.Condition
                While TypeOf condition Is CSS.ParenthesizedExpressionSyntax
                    condition = (DirectCast(condition, CSS.ParenthesizedExpressionSyntax)).Expression
                End While

                If Not (TypeOf condition Is CSS.BinaryExpressionSyntax) Then Return False
                Dim be As CSS.BinaryExpressionSyntax = DirectCast(condition, CSS.BinaryExpressionSyntax)
                If Not be.IsKind(CS.SyntaxKind.NotEqualsExpression) OrElse
                    (Not be.Left.IsKind(CS.SyntaxKind.NullLiteralExpression) AndAlso
                    Not be.Right.IsKind(CS.SyntaxKind.NullLiteralExpression)) Then
                    Return False
                End If
                Dim singleStatement As CSS.ExpressionStatementSyntax
                If TypeOf node.Statement Is CSS.BlockSyntax Then
                    Dim block As CSS.BlockSyntax = (DirectCast(node.Statement, CSS.BlockSyntax))
                    If block.Statements.Count <> 1 Then Return False
                    singleStatement = TryCast(block.Statements(0), CSS.ExpressionStatementSyntax)
                Else
                    singleStatement = TryCast(node.Statement, CSS.ExpressionStatementSyntax)
                End If

                If singleStatement Is Nothing OrElse Not (TypeOf singleStatement.Expression Is CSS.InvocationExpressionSyntax) Then Return False
                Dim possibleEventName As String = If(GetPossibleEventName(be.Left), GetPossibleEventName(be.Right))
                If possibleEventName Is Nothing Then Return False
                Dim invocation As CSS.InvocationExpressionSyntax = DirectCast(singleStatement.Expression, CSS.InvocationExpressionSyntax)
                Dim invocationName As String = GetPossibleEventName(invocation.Expression)
                If possibleEventName <> invocationName Then Return False
                name = VBFactory.IdentifierName(possibleEventName)
                arguments.AddRange(invocation.ArgumentList.Arguments.Select(Function(a As CSS.ArgumentSyntax) DirectCast(a.Accept(_nodesVisitor), ArgumentSyntax)))
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
                If (DirectCast(condition.Left, CSS.IdentifierNameSyntax)).Identifier.IsEquivalentTo(iteratorIdentifier.Identifier) Then
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
                    If (DirectCast(initializer.Left, CSS.IdentifierNameSyntax)).Identifier.IsEquivalentTo(iteratorIdentifier.Identifier) Then
                        Return False
                    End If
                End If
                Return True
            End Function

            Public Shared Function GetUniqueVariableNameInScope(node As CSharpSyntaxNode, variableNameBase As String, lSemanticModel As SemanticModel) As String
                Dim reservedNames As New List(Of String) From {
                    "_"
                }
                reservedNames.AddRange(node.DescendantNodesAndSelf().SelectMany(Function(lSyntaxNode As SyntaxNode) lSemanticModel.LookupSymbols(lSyntaxNode.SpanStart).Select(Function(s As ISymbol) s.Name)).Distinct)
                Dim UniqueVariableName As String = EnsureUniqueness(variableNameBase, reservedNames)
                s_usedIdentifiers.Add(UniqueVariableName,
                                    New SymbolTableEntry(UniqueVariableName,
                                                         IsType:=False
                                                         )
                                    )
                Return UniqueVariableName
            End Function

            <ExcludeFromCodeCoverage>
            Public Overrides Function DefaultVisit(node As SyntaxNode) As SyntaxList(Of StatementSyntax)
                Throw New NotImplementedException(node.[GetType]().ToString & " not implemented!")
            End Function

            Public Overrides Function VisitBlock(node As CSS.BlockSyntax) As SyntaxList(Of StatementSyntax)
                Dim ListOfStatements As SyntaxList(Of StatementSyntax) = VBFactory.List(node.Statements.Where(Function(s As CSS.StatementSyntax) Not (TypeOf s Is CSS.EmptyStatementSyntax)).SelectMany(Function(s As CSS.StatementSyntax) s.Accept(Me)))
                If node.OpenBraceToken.HasLeadingTrivia OrElse node.OpenBraceToken.HasTrailingTrivia Then
                    ListOfStatements = ListOfStatements.Insert(0, VBFactory.EmptyStatement.WithConvertedTriviaFrom(node.OpenBraceToken))
                End If
                If node.CloseBraceToken.HasLeadingTrivia OrElse node.OpenBraceToken.HasTrailingTrivia Then
                    ListOfStatements = ListOfStatements.Add(VBFactory.EmptyStatement.WithConvertedTriviaFrom(node.CloseBraceToken))
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
                    End If

                    If TypeOf stmt Is CSS.WhileStatementSyntax Then
                        StatementKind = VB.SyntaxKind.ExitWhileStatement
                        BlockKeyword = WhileKeyword
                        Exit For
                    End If

                    If TypeOf stmt Is CSS.ForStatementSyntax Then
                        If WillConvertToFor(DirectCast(stmt, CSS.ForStatementSyntax)) Then
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
                    End If

                    If TypeOf stmt Is CSS.SwitchStatementSyntax Then
                        StatementKind = VB.SyntaxKind.ExitSelectStatement
                        BlockKeyword = SelectKeyword
                        Exit For
                    End If
                Next

                Return VBFactory.SingletonList(Of StatementSyntax)(VBFactory.ExitStatement(StatementKind,
                                                                                                   BlockKeyword
                                                                                                   ).WithConvertedTriviaFrom(node))
            End Function

            Public Overrides Function VisitCheckedStatement(node As CSS.CheckedStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim OpenBraceTrailingTrivia As New List(Of SyntaxTrivia)
                Dim ClosingBraceLeadingTrivia As New List(Of SyntaxTrivia)
                If node.Keyword.IsKind(CS.SyntaxKind.CheckedKeyword) Then
                    Return WrapInComment(ConvertBlock(node.Block, OpenBraceTrailingTrivia, ClosingBraceLeadingTrivia), node, "Visual Basic Default Is checked math, check that this works for you!")
                End If
                Return WrapInComment(ConvertBlock(node.Block, OpenBraceTrailingTrivia, ClosingBraceLeadingTrivia), node, "Visual Basic does Not support unchecked statements!")
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

                    If TypeOf stmt Is CSS.ForStatementSyntax OrElse TypeOf stmt Is CSS.ForEachStatementSyntax Then
                        statementKind = VB.SyntaxKind.ContinueForStatement
                        BlockKeyword = ForKeyword
                        Exit For
                    End If
                Next

                Return VBFactory.SingletonList(Of StatementSyntax)(VBFactory.ContinueStatement(statementKind,
                                                                                                       BlockKeyword
                                                                                                       ).WithConvertedTriviaFrom(node))
            End Function

            Public Overrides Function VisitDeclarationExpression(node As CSS.DeclarationExpressionSyntax) As SyntaxList(Of StatementSyntax)
                Return MyBase.VisitDeclarationExpression(node)
            End Function

            Public Overrides Function VisitDoStatement(node As CSS.DoStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim condition As ExpressionSyntax = DirectCast(node.Condition.Accept(_nodesVisitor), ExpressionSyntax)
                Dim OpenBraceTrailingTrivia As New List(Of SyntaxTrivia)
                Dim ClosingBraceLeadingTrivia As New List(Of SyntaxTrivia)
                Dim stmt As SyntaxList(Of StatementSyntax) = ConvertBlock(node.Statement, OpenBraceTrailingTrivia, ClosingBraceLeadingTrivia)
                'If OpenBraceTrailingTrivia.Any OrElse ClosingBraceLeadingTrivia.Any Then
                '    Stop
                'End If
                Dim DoStatement As DoStatementSyntax = VBFactory.DoStatement(VB.SyntaxKind.SimpleDoStatement)
                Dim LoopStatement As LoopStatementSyntax = VBFactory.LoopStatement(VB.SyntaxKind.LoopWhileStatement, VBFactory.WhileClause(condition).WithTrailingEOL)
                Dim block As DoLoopBlockSyntax = VBFactory.DoLoopWhileBlock(DoStatement, stmt, LoopStatement)
                Return ReplaceOneStatementWithMarkedStatements(node, block)
            End Function

            Public Overrides Function VisitEmptyStatement(node As CSS.EmptyStatementSyntax) As SyntaxList(Of StatementSyntax)
                Return VBFactory.SingletonList(Of StatementSyntax)(VBFactory.EmptyStatement().WithConvertedTriviaFrom(node))
            End Function

            Public Overrides Function VisitExpressionStatement(node As CSS.ExpressionStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim Initializer As EqualsValueSyntax
                Dim csExpression As CSS.ExpressionSyntax = node.Expression
                Dim Declarator As VariableDeclaratorSyntax = Nothing
                Dim Statement As StatementSyntax = Nothing
                Dim TrailingTrivia As New List(Of SyntaxTrivia)
                Dim Names As SeparatedSyntaxList(Of ModifiedIdentifierSyntax)

                If node.GetFirstToken.IsKind(CS.SyntaxKind.NewKeyword) AndAlso csExpression.IsKind(CS.SyntaxKind.SimpleAssignmentExpression) Then
                    ' Handle this case
                    'Dim x As New IO.FileInfo("C:\temp") With {.IsReadOnly = True}
                    Dim csAssignmentExpression As CSS.AssignmentExpressionSyntax = CType(csExpression, CSS.AssignmentExpressionSyntax)

                    If TypeOf csAssignmentExpression.Left Is CSS.MemberAccessExpressionSyntax Then
                        Initializer = VBFactory.EqualsValue(CType(csAssignmentExpression.Right.Accept(_nodesVisitor), ExpressionSyntax))
                        Dim csMemberAccessExpression As CSS.MemberAccessExpressionSyntax = CType(csAssignmentExpression.Left, CSS.MemberAccessExpressionSyntax)
                        Dim csObjectCreationExpression As CSS.ObjectCreationExpressionSyntax = CType(csMemberAccessExpression.Expression, CSS.ObjectCreationExpressionSyntax)

                        Dim ObjectInitializer As ObjectCreationInitializerSyntax
                        Dim NewObject As ObjectCreationExpressionSyntax
                        If TypeOf csObjectCreationExpression.Type IsNot CSS.IdentifierNameSyntax Then
                            Dim AssignmentStatement As AssignmentStatementSyntax = DirectCast(csAssignmentExpression.Accept(_nodesVisitor), AssignmentStatementSyntax)
                            Dim MemberAccessExpression As MemberAccessExpressionSyntax = DirectCast(AssignmentStatement.Left, MemberAccessExpressionSyntax)
                            Dim FieldInitializer As FieldInitializerSyntax = VBFactory.NamedFieldInitializer(name:=DirectCast(MemberAccessExpression.Name, IdentifierNameSyntax),
                                                                                                                 expression:=Initializer.Value
                                                                                                                 )
                            ObjectInitializer = VBFactory.ObjectMemberInitializer(FieldInitializer)
                            Dim ObjectCreateExpression As ObjectCreationExpressionSyntax = DirectCast(MemberAccessExpression.Expression, ObjectCreationExpressionSyntax)
                            NewObject = VBFactory.ObjectCreationExpression(
                                                    NewKeyword,
                                                    ObjectCreateExpression.AttributeLists,
                                                    ObjectCreateExpression.Type,
                                                    ObjectCreateExpression.ArgumentList,
                                                    ObjectInitializer)
                        ElseIf TypeOf csObjectCreationExpression.Type Is CSS.IdentifierNameSyntax Then
                            Dim FieldInitializer As FieldInitializerSyntax = VBFactory.NamedFieldInitializer(name:=DirectCast(csMemberAccessExpression.Name.Accept(_nodesVisitor), IdentifierNameSyntax),
                                                                                                                 expression:=Initializer.Value
                                                                                                                 )
                            ObjectInitializer = VBFactory.ObjectMemberInitializer(FieldInitializer)

                            NewObject = VBFactory.ObjectCreationExpression(NewKeyword,
                                                                           Nothing,
                                                                           CType(csObjectCreationExpression.Type.Accept(_nodesVisitor), IdentifierNameSyntax),
                                                                           CType(csObjectCreationExpression.ArgumentList.Accept(_nodesVisitor), ArgumentListSyntax),
                                                                           ObjectInitializer)
                        Else
                            NewObject = Nothing
                            Stop
                        End If
                        Dim identifierString As String = GetUniqueVariableNameInScope(node, "TempVar", _semanticModel)
                        Names = VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(identifierString))
                        Statement = FactoryDimStatement(identifierString,
                                                       VBFactory.AsNewClause(NewObject.WithLeadingTrivia(SpaceTrivia)),
                                                       initializer:=Nothing)
                    End If
                ElseIf node.ToString.StartsWith("(", StringComparison.Ordinal) AndAlso csExpression.IsKind(CS.SyntaxKind.InvocationExpression) Then
                    Dim InvovationExpression As CSS.InvocationExpressionSyntax = DirectCast(csExpression, CSS.InvocationExpressionSyntax)
                    Dim csInvocationExpression As CSS.InvocationExpressionSyntax = InvovationExpression
                    Dim exprNode As InvocationExpressionSyntax
                    Dim AccessExpression As MemberAccessExpressionSyntax = Nothing
                    If InvovationExpression.Expression.IsKind(CS.SyntaxKind.SimpleMemberAccessExpression) Then
                        exprNode = DirectCast(csExpression.Accept(_nodesVisitor), InvocationExpressionSyntax)
                        AccessExpression = DirectCast(exprNode.Expression, MemberAccessExpressionSyntax)
                    ElseIf InvovationExpression.Expression.IsKind(CS.SyntaxKind.ParenthesizedExpression) Then
                        Dim ParenthesizedExpression As CSS.ExpressionSyntax = CType(InvovationExpression.Expression, CSS.ParenthesizedExpressionSyntax).Expression
                        csExpression = CType(InvovationExpression.Expression, CSS.ParenthesizedExpressionSyntax)
                        If ParenthesizedExpression.IsKind(CS.SyntaxKind.SimpleMemberAccessExpression) Then
                            csExpression = ParenthesizedExpression
                            AccessExpression = DirectCast(csExpression.Accept(_nodesVisitor), MemberAccessExpressionSyntax)
                        ElseIf ParenthesizedExpression.IsKind(CS.SyntaxKind.CastExpression) Then
                            Dim CTypeExpression As CastExpressionSyntax = DirectCast(ParenthesizedExpression.Accept(_nodesVisitor), CTypeExpressionSyntax)
                            Dim Type As TypeSyntax = CTypeExpression.Type
                            Initializer = VBFactory.EqualsValue(CTypeExpression.WithLeadingTrivia(SpaceTrivia))
                            Dim AsClause As AsClauseSyntax = VBFactory.SimpleAsClause(Type)
                            Names = VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(GetUniqueVariableNameInScope(node, "TempVar", _semanticModel)))
                            Declarator = VBFactory.VariableDeclarator(Names, AsClause, Initializer)
                        Else
                            Stop
                        End If
                    ElseIf InvovationExpression.Expression.IsKind(CS.SyntaxKind.PointerMemberAccessExpression) Then
                        AccessExpression = CType(InvovationExpression.Expression.Accept(_nodesVisitor), MemberAccessExpressionSyntax)
                    Else
                        Stop
                    End If
                    Dim StmtList As New SyntaxList(Of StatementSyntax)
                    Dim IdentifierString As String = Nothing
                    If Declarator Is Nothing Then
                        If Names.Count = 0 Then
                            IdentifierString = GetUniqueVariableNameInScope(node, "TempVar", _semanticModel)
                            Names = VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(IdentifierString))
                        End If
                        ' Handle this case
                        ' (new Exception("RequestServiceAsync Timeout")).ReportServiceHubNFW("RequestServiceAsync Timeout");
                        Select Case AccessExpression.Expression.Kind
                            Case VB.SyntaxKind.IdentifierName
                                Declarator = Nothing
                                IdentifierString = AccessExpression.Expression.ToString
                            Case VB.SyntaxKind.CTypeExpression, VB.SyntaxKind.TryCastExpression
                                Dim AsClause As AsClauseSyntax
                                If AccessExpression.Expression.IsKind(VB.SyntaxKind.CTypeExpression) Then
                                    Dim CTypeExpression As CTypeExpressionSyntax = CType(AccessExpression.Expression, CTypeExpressionSyntax)
                                    AsClause = VBFactory.SimpleAsClause(CTypeExpression.Type.WithLeadingTrivia(SpaceTrivia))
                                    Initializer = VBFactory.EqualsValue(CTypeExpression.WithLeadingTrivia(SpaceTrivia))
                                Else
                                    Dim TryCastExpression As TryCastExpressionSyntax = CType(AccessExpression.Expression, TryCastExpressionSyntax)
                                    AsClause = VBFactory.SimpleAsClause(TryCastExpression.Type.WithLeadingTrivia(SpaceTrivia))
                                    Initializer = VBFactory.EqualsValue(TryCastExpression.WithLeadingTrivia(SpaceTrivia))
                                End If
                                Declarator = VBFactory.VariableDeclarator(Names, AsClause, Initializer)
                            Case VB.SyntaxKind.ParenthesizedExpression
                                Dim ParenthesizedExpression As ParenthesizedExpressionSyntax = CType(AccessExpression.Expression, ParenthesizedExpressionSyntax)
                                Initializer = VBFactory.EqualsValue(ParenthesizedExpression.Expression.WithLeadingTrivia(SpaceTrivia))
                                Select Case ParenthesizedExpression.Expression.Kind
                                    Case VB.SyntaxKind.ObjectCreationExpression
                                        Dim NewObject As ObjectCreationExpressionSyntax = CType(ParenthesizedExpression.Expression, ObjectCreationExpressionSyntax)
                                        Dim AsNewClause As AsNewClauseSyntax = VBFactory.AsNewClause(NewObject.WithLeadingTrivia(SpaceTrivia))
                                        Declarator = VBFactory.VariableDeclarator(Names, AsNewClause, initializer:=Nothing)
                                    Case VB.SyntaxKind.PredefinedCastExpression
                                        Dim InvocationExpression As CSS.InvocationExpressionSyntax = CType(csExpression, CSS.InvocationExpressionSyntax)
                                        Dim MemberAccessExpression As CSS.MemberAccessExpressionSyntax = CType(InvocationExpression.Expression, CSS.MemberAccessExpressionSyntax)
                                        Dim csParenthesizedExpression As CSS.ParenthesizedExpressionSyntax = CType(MemberAccessExpression.Expression, CSS.ParenthesizedExpressionSyntax)
                                        Dim CastExpression As CSS.CastExpressionSyntax = CType(csParenthesizedExpression.Expression, CSS.CastExpressionSyntax)
                                        Dim Type As TypeSyntax = CType(CastExpression.Type.Accept(_nodesVisitor), TypeSyntax)
                                        Dim AsClause As AsClauseSyntax = VBFactory.SimpleAsClause(Type)
                                        Declarator = VBFactory.VariableDeclarator(Names, AsClause, Initializer)
                                    Case VB.SyntaxKind.AwaitExpression
                                        Dim InvocationExpression As CSS.InvocationExpressionSyntax = CType(csExpression, CSS.InvocationExpressionSyntax)
                                        Dim MemberAccessExpression As CSS.MemberAccessExpressionSyntax = CType(InvocationExpression.Expression, CSS.MemberAccessExpressionSyntax)
                                        Dim csParenthesizedExpression As CSS.ParenthesizedExpressionSyntax = CType(MemberAccessExpression.Expression, CSS.ParenthesizedExpressionSyntax)
                                        Dim AwaitExpression As CSS.AwaitExpressionSyntax = CType(csParenthesizedExpression.Expression, CSS.AwaitExpressionSyntax)
                                        Dim csAwaitExpressionInfo As CS.AwaitExpressionInfo = _semanticModel.GetAwaitExpressionInfo(AwaitExpression)
                                        Dim Type As TypeSyntax
                                        If csAwaitExpressionInfo.GetResultMethod Is Nothing Then
                                            Type = VBFactory.ParseTypeName($"Object")
                                        Else
                                            Type = VBFactory.ParseTypeName($" {csAwaitExpressionInfo.GetResultMethod.ReturnType}")
                                        End If
                                        Dim AsClause As AsClauseSyntax = VBFactory.SimpleAsClause(Type)
                                        Declarator = VBFactory.VariableDeclarator(Names, AsClause, Initializer)
                                    Case Else
                                        Declarator = Nothing
                                        Stop
                                End Select
                            Case VB.SyntaxKind.InvocationExpression
                                Dim MemberAccessExpression As InvocationExpressionSyntax = CType(AccessExpression.Expression, InvocationExpressionSyntax)
                                Initializer = VBFactory.EqualsValue(MemberAccessExpression.WithLeadingTrivia(SpaceTrivia))
                                Declarator = VBFactory.VariableDeclarator(Names, asClause:=Nothing, Initializer)
                            Case VB.SyntaxKind.SimpleMemberAccessExpression
                                Dim MemberAccessExpression As MemberAccessExpressionSyntax = CType(AccessExpression.Expression, MemberAccessExpressionSyntax)
                                Initializer = VBFactory.EqualsValue(MemberAccessExpression.WithLeadingTrivia(SpaceTrivia))
                                Declarator = VBFactory.VariableDeclarator(Names, asClause:=Nothing, Initializer)
                            Case Else
                                Declarator = Nothing
                                Stop
                        End Select
                    End If
                    If Declarator IsNot Nothing Then
                        Dim declarators As SeparatedSyntaxList(Of VariableDeclaratorSyntax) = VBFactory.SingletonSeparatedList(Declarator.WithTrailingEOL)
                        StmtList = StmtList.Add(VBFactory.LocalDeclarationStatement(DimModifier, declarators).WithConvertedLeadingTriviaFrom(node))
                    End If
                    If AccessExpression IsNot Nothing Then
                        With AccessExpression
                            If String.IsNullOrWhiteSpace(IdentifierString) Then
                                IdentifierString = GetUniqueVariableNameInScope(node, "TempVar", _semanticModel)
                            End If
                            Dim Identifier As ExpressionSyntax = VBFactory.IdentifierName(IdentifierString)
                            Dim AgrumentList As ArgumentListSyntax = DirectCast(csInvocationExpression.ArgumentList.Accept(_nodesVisitor), ArgumentListSyntax)
                            Dim InvocationExpression As InvocationExpressionSyntax = VBFactory.InvocationExpression(
                                                                                                                    VBFactory.MemberAccessExpression(.Kind,
                                                                                                                                                    expression:=Identifier,
                                                                                                                                                    .OperatorToken,
                                                                                                                                                    .Name),
                                                                                                                    AgrumentList
                                                                                                                    ).WithConvertedTriviaFrom(node)
                            StmtList = StmtList.Add(VBFactory.ExpressionStatement(InvocationExpression))
                        End With
                    End If
                    Return ReplaceStatementsWithMarkedStatements(node, StmtList)
                End If
                Dim StatementList As New List(Of StatementSyntax)
                If Statement Is Nothing Then
                    StatementList = ConvertSingleExpression(csExpression)
                Else
                    StatementList.Add(Statement.WithAppendedTrailingTrivia(ConvertTrivia(node.GetTrailingTrivia)).WithTrailingEOL)
                End If
                StatementList(StatementList.Count - 1) = StatementList.Last.WithAppendedTrailingTrivia(ConvertTrivia(node.GetTrailingTrivia)).WithTrailingEOL

                Return ReplaceStatementsWithMarkedStatements(node, VBFactory.List(StatementList))
            End Function

            Public Overrides Function VisitFixedStatement(node As CSS.FixedStatementSyntax) As SyntaxList(Of StatementSyntax)
                Return VBFactory.SingletonList(Of StatementSyntax)(FlagUnsupportedStatements(node, "C# Fixed is not support by VB", CommentOutOriginalStatements:=True))
            End Function

            Public Overrides Function VisitForEachStatement(node As CSS.ForEachStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim variableDeclarator As VariableDeclaratorSyntax
                Dim asClause As SimpleAsClauseSyntax = Nothing
                Dim variableToken As SyntaxToken = GenerateSafeVBToken(node.Identifier,
                                                                        IsQualifiedName:=False,
                                                                        IsTypeName:=False)
                Dim variableIdentifier As IdentifierNameSyntax = VBFactory.IdentifierName(variableToken)
                If node.Type.IsVar Then
                    Dim variableITypeSymbol As (_Error As Boolean, _ITypeSymbol As ITypeSymbol) = node.Expression.DetermineType(_semanticModel)
                    If variableITypeSymbol._Error = False Then
                        Dim _ITypeSymbol As ITypeSymbol = variableITypeSymbol._ITypeSymbol
                        Dim type As TypeSyntax = NodesVisitor.GetElementType(_ITypeSymbol)
                        asClause = If(type IsNot Nothing, VBFactory.SimpleAsClause(type), Nothing)
                    End If
                Else
                    Dim VBType As TypeSyntax
                    If node.Type.IsKind(CS.SyntaxKind.IdentifierName) Then
                        VBType = VBFactory.IdentifierName(GenerateSafeVBToken(DirectCast(node.Type, CSS.IdentifierNameSyntax).Identifier,
                                                                              IsQualifiedName:=False,
                                                                              IsTypeName:=True))
                    Else
                        VBType = DirectCast(node.Type.Accept(_nodesVisitor), TypeSyntax)
                    End If
                    asClause = VBFactory.SimpleAsClause(VBType)
                End If
                variableDeclarator = VBFactory.VariableDeclarator(VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(variableToken).WithTrailingTrivia(SpaceTrivia)),
                                                                  asClause,
                                                                  initializer:=Nothing)

                Dim expression As ExpressionSyntax = DirectCast(node.Expression.Accept(_nodesVisitor), ExpressionSyntax).WithConvertedTrailingTriviaFrom(node.CloseParenToken)
                Dim openBraceTrailingTrivia As New List(Of SyntaxTrivia)
                Dim closingBraceLeadingTrivia As New List(Of SyntaxTrivia)
                Dim innerStmts As SyntaxList(Of StatementSyntax) = ConvertBlock(node.Statement, openBraceTrailingTrivia, closingBraceLeadingTrivia)
                Dim nextStatement As NextStatementSyntax = VBFactory.NextStatement().WithLeadingTrivia(closingBraceLeadingTrivia)
                If node.AwaitKeyword.IsKind(CS.SyntaxKind.None) Then

                    Dim ForEachStatement As ForEachStatementSyntax = VBFactory.ForEachStatement(variableDeclarator, expression).WithTrailingEOL
                    Dim block As ForEachBlockSyntax = VBFactory.ForEachBlock(ForEachStatement.WithConvertedLeadingTriviaFrom(node.ForEachKeyword),
                                                                             innerStmts,
                                                                             nextStatement).WithAdditionalAnnotations(Simplifier.Annotation)
                    Return ReplaceOneStatementWithMarkedStatements(node,
                                                                   VBFactory.ForEachBlock(ForEachStatement.WithConvertedLeadingTriviaFrom(node.ForEachKeyword),
                                                                             innerStmts,
                                                                             nextStatement
                                                                   ).WithAdditionalAnnotations(Simplifier.Annotation))
                End If
                ' public override async Task<CounterReply> AccumulateCount(IAsyncStreamReader<CounterRequest> requestStream, ServerCallContext context)
                '{
                '    // Instead of passing CancellationToken.None to GetAsyncEnumerator, pass the CancellationToken parameter from the containing method
                '    // if And only if it Is marked with System.Runtime.CompilerServices.EnumeratorCancellationAttribute.

                '    var messageEnumerator = requestStream.ReadAllAsync().GetAsyncEnumerator(CancellationToken.None);
                Dim messageEnumerator As SyntaxToken = VBFactory.Identifier(GetUniqueVariableNameInScope(node, "messageEnumerator", _semanticModel))
                Dim cancelExpression As ExpressionSyntax = VBFactory.ParseExpression("Threading.CancellationToken.None")
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
                                    cancelExpression = VBFactory.ParseExpression(parm.Identifier.ValueText)
                                    Exit For
                                End If
                            Next
                        End If
                    End If
                End If
                Dim memberAccessExpression As ExpressionSyntax = VBFactory.MemberAccessExpression(VB.SyntaxKind.SimpleMemberAccessExpression,
                                                                                                  expression.WithoutTrailingTrivia,
                                                                                                  DotToken,
                                                                                                  VBFactory.IdentifierName("GetAsyncEnumerator"))
                Dim argumentList As ArgumentListSyntax = VBFactory.ArgumentList(VBFactory.SingletonSeparatedList(Of ArgumentSyntax)(VBFactory.SimpleArgument(cancelExpression)))
                Dim invocationExpression As InvocationExpressionSyntax = VBFactory.InvocationExpression(memberAccessExpression, argumentList)
                Dim initializer As EqualsValueSyntax = VBFactory.EqualsValue(invocationExpression)
                Dim asyncblock As New List(Of StatementSyntax) From {
                    FactoryDimStatement(messageEnumerator, asClause:=Nothing, initializer)
                }
                '    Try
                '    {
                Dim tryStatement As TryStatementSyntax = VBFactory.TryStatement()
                '        While (await messageEnumerator.MoveNextAsync())
                '        {
                Dim moveNextExpression As ExpressionSyntax = VBFactory.QualifiedName(VBFactory.IdentifierName(messageEnumerator),
                                                                                     DotToken,
                                                                                     VBFactory.IdentifierName("MoveNextAsync"))
                Dim whileStatement As WhileStatementSyntax = VBFactory.WhileStatement(VBFactory.AwaitExpression(moveNextExpression))
                '            var message = messageEnumerator.Current;
                Dim whileStatements As New List(Of StatementSyntax) From {
                    FactoryDimStatement(variableToken,
                                       asClause:=Nothing,
                                       VBFactory.EqualsValue(VBFactory.QualifiedName(VBFactory.IdentifierName(messageEnumerator),
                                                             DotToken,
                                                             VBFactory.IdentifierName("Current"))
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
                Dim condition As ExpressionSyntax = VBFactory.IsNotExpression(VBFactory.IdentifierName(messageEnumerator),
                                                                              NothingExpression)
                Dim ifStatement As IfStatementSyntax = VBFactory.IfStatement(IfKeyword, condition, ThenKeyword)
                '                                {
                '            await messageEnumerator.DisposeAsync();
                Dim disposeAsyncExpression As ExpressionSyntax = VBFactory.QualifiedName(VBFactory.IdentifierName(messageEnumerator),
                                                                                         DotToken,
                                                                                         VBFactory.IdentifierName("DisposeAsync"))
                Dim awaitStatement As StatementSyntax = VBFactory.ExpressionStatement(VBFactory.AwaitExpression(disposeAsyncExpression))
                '        }
                '    }
                Dim finallyStatements As SyntaxList(Of StatementSyntax) =
                    VBFactory.SingletonList(Of StatementSyntax)(VBFactory.MultiLineIfBlock(ifStatement,
                                                                                           VBFactory.SingletonList(awaitStatement),
                                                                                           elseBlock:=Nothing,
                                                                                           elseIfBlocks:=Nothing))
                Dim finallyBlock As FinallyBlockSyntax = VBFactory.FinallyBlock(finallyStatements)
                Dim whileBlock As WhileBlockSyntax = VBFactory.WhileBlock(whileStatement, VBFactory.List(whileStatements))
                Dim tryBlock As TryBlockSyntax = VBFactory.TryBlock(tryStatement,
                                                                    VBFactory.SingletonList(Of StatementSyntax)(whileBlock),
                                                                    catchBlocks:=Nothing,
                                                                    finallyBlock,
                                                                    VBFactory.EndTryStatement)
                asyncblock.Add(tryBlock)
                Return ReplaceStatementsWithMarkedStatements(node,
                                                             VBFactory.List(asyncblock))
            End Function

            Public Overrides Function VisitForEachVariableStatement(node As CSS.ForEachVariableStatementSyntax) As SyntaxList(Of StatementSyntax)
                Return VBFactory.SingletonList(Of StatementSyntax)(FlagUnsupportedStatements(node,
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
                If ConvertForToSimpleForNext(node, block, HasVariable) Then
                    Return ReplaceOneStatementWithMarkedStatements(node, block)
                Else
                    Dim OpenBraceTrailingTrivia As New List(Of SyntaxTrivia)
                    Dim ClosingBraceLeadingTrivia As New List(Of SyntaxTrivia)
                    Dim stmts As SyntaxList(Of StatementSyntax) = ConvertBlock(node.Statement, OpenBraceTrailingTrivia, ClosingBraceLeadingTrivia).AddRange(node.Incrementors.Select(AddressOf ConvertSingleBlock))
                    Dim condition As ExpressionSyntax = If(node.Condition Is Nothing, VBFactory.TrueLiteralExpression(TrueKeyword), DirectCast(node.Condition.Accept(_nodesVisitor), ExpressionSyntax))
                    Dim WhileStatement As WhileStatementSyntax = VBFactory.WhileStatement(WhileKeyword.WithConvertedLeadingTriviaFrom(node.ForKeyword),
                                                                                                condition.WithAppendedTrailingTrivia(ConvertTrivia(node.SecondSemicolonToken.TrailingTrivia))
                                                                                                ).WithAppendedTrailingTrivia(ConvertTrivia(node.GetBraces.Item1.TrailingTrivia)).WithPrependedLeadingTrivia(node.GetBraces.Item1.LeadingTrivia).WithTrailingEOL
                    WhileStatement = CType(PrependStatementWithMarkedStatementTrivia(node, WhileStatement), WhileStatementSyntax)
                    Dim EndWhileStatement As EndBlockStatementSyntax = VBFactory.EndWhileStatement.WithLeadingTrivia(ClosingBraceLeadingTrivia).WithConvertedTrailingTriviaFrom(node.GetBraces.Item2)
                    block = VBFactory.WhileBlock(WhileStatement, stmts, EndWhileStatement)
                    Dim Statements As SyntaxList(Of StatementSyntax) = VBFactory.List(node.Initializers.Select(AddressOf ConvertSingleBlock)).Add(block)
                    If HasVariable Then
                        Statements = Statements.Insert(0, node.Declaration.Accept(Me).First.WithConvertedTrailingTriviaFrom(node.FirstSemicolonToken).WithTrailingEOL)
                    End If
                    Return ReplaceStatementsWithMarkedStatements(node, Statements)
                End If

            End Function

            Public Overrides Function VisitGotoStatement(node As CSS.GotoStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim label As LabelSyntax
                If node.IsKind(CS.SyntaxKind.GotoCaseStatement, CS.SyntaxKind.GotoDefaultStatement) Then
                    If _blockInfo.Count = 0 Then Throw New InvalidOperationException("GoTo Case/GoTo Default outside switch Is illegal!")
                    Dim labelExpression As VisualBasicSyntaxNode = If(node.Expression?.Accept(_nodesVisitor), VBFactory.ElseCaseClause())
                    _blockInfo.Peek().GotoCaseExpressions.Add(labelExpression)
                    label = VBFactory.Label(VB.SyntaxKind.IdentifierLabel, MakeGotoSwitchLabel(labelExpression))
                Else
                    label = VBFactory.Label(VB.SyntaxKind.IdentifierLabel, GenerateSafeVBToken(DirectCast(node.Expression, CSS.IdentifierNameSyntax).Identifier,
                                                                                               IsQualifiedName:=False,
                                                                                               IsTypeName:=False))
                End If

                Return VBFactory.SingletonList(Of StatementSyntax)(VBFactory.GoToStatement(label).WithConvertedTriviaFrom(node).WithTrailingEOL)
            End Function

            Public Overrides Function VisitIfStatement(node As CSS.IfStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim name As IdentifierNameSyntax = Nothing
                Dim arguments As New List(Of ArgumentSyntax)()
                Dim stmt As StatementSyntax
                If node.[Else] Is Nothing AndAlso TryConvertRaiseEvent(node, name, arguments) Then
                    stmt = VBFactory.RaiseEventStatement(name, VBFactory.ArgumentList(VBFactory.SeparatedList(arguments)))
                    Return VBFactory.SingletonList(stmt)
                End If

                Dim ListOfElseIfBlocks As New List(Of ElseIfBlockSyntax)()
                Dim ElseBlock As ElseBlockSyntax = Nothing
                Dim OpenBraceTrailingTrivia As New List(Of SyntaxTrivia)
                Dim ClosingBraceLeadingTrivia As New List(Of SyntaxTrivia)
                CollectElseBlocks(node, ListOfElseIfBlocks, ElseBlock, OpenBraceTrailingTrivia, ClosingBraceLeadingTrivia)

                Dim StatementTrailingTrivia As New List(Of SyntaxTrivia)

                Dim OpenParenToken As SyntaxToken = node.OpenParenToken

                Dim IfKeywordWithTrivia As SyntaxToken = IfKeyword.
                                            WithConvertedTriviaFrom(node.IfKeyword).
                                            WithAppendedTrailingTrivia(ConvertTrivia(OpenParenToken.LeadingTrivia)).
                                            WithAppendedTrailingTrivia(ConvertTrivia(OpenParenToken.TrailingTrivia))
                IfKeywordWithTrivia = IfKeywordWithTrivia.WithModifiedTokenTrivia(LeadingToken:=True, AfterEOL:=False)

                StatementTrailingTrivia.AddRange(ConvertTrivia(node.CloseParenToken.LeadingTrivia))
                StatementTrailingTrivia.AddRange(ConvertTrivia(node.CloseParenToken.TrailingTrivia))
                If StatementTrailingTrivia.Any AndAlso Not StatementTrailingTrivia(0).IsEndOfLine Then
                    StatementTrailingTrivia.Insert(0, VBEOLTrivia)
                End If
                Dim ConditionWithTrivia As ExpressionSyntax = DirectCast(node.Condition.Accept(_nodesVisitor).WithAppendedTrailingTrivia(ConvertTrivia(node.Condition.GetTrailingTrivia)).WithModifiedNodeTrivia(SeparatorFollows:=True), ExpressionSyntax)

                If node.Statement.IsKind(CS.SyntaxKind.EmptyStatement) Then
                    StatementTrailingTrivia.InsertRange(0, ConvertTrivia(DirectCast(node.Statement, CSS.EmptyStatementSyntax).SemicolonToken.TrailingTrivia))
                End If
                Dim IfStatement As IfStatementSyntax = VBFactory.IfStatement(
                                                                                 IfKeywordWithTrivia,
                                                                                 ConditionWithTrivia,
                                                                                 ThenKeyword
                                                                                ).
                                                                                WithTrailingTrivia(StatementTrailingTrivia).WithTrailingEOL

                Dim Braces As (SyntaxToken, SyntaxToken) = node.Statement.GetBraces
                Dim OpenBraces As SyntaxToken = Braces.Item1
                Dim CloseBraces As SyntaxToken = Braces.Item2
                Dim EndIfStatement As EndBlockStatementSyntax = VBFactory.EndIfStatement(EndKeyword,
                                                                                             IfKeyword
                                                                                            ).
                                                                                            WithConvertedTriviaFrom(CloseBraces)
                Dim ElseIfBlocks As SyntaxList(Of ElseIfBlockSyntax) = VBFactory.List(ListOfElseIfBlocks)
                If ElseBlock IsNot Nothing AndAlso ElseBlock.Statements(0).IsKind(VB.SyntaxKind.EmptyStatement) Then
                    EndIfStatement = EndIfStatement.WithLeadingTrivia(ElseBlock.GetTrailingTrivia)
                    ElseBlock = VBFactory.ElseBlock(VBFactory.ElseStatement(), statements:=Nothing)
                End If
                Dim Statements As SyntaxList(Of StatementSyntax) = ConvertBlock(node.Statement, OpenBraceTrailingTrivia, ClosingBraceLeadingTrivia)
                If ClosingBraceLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    EndIfStatement = EndIfStatement.WithLeadingTrivia(ClosingBraceLeadingTrivia)
                End If
                If TypeOf node.Statement Is CSS.BlockSyntax Then
                    stmt = VBFactory.MultiLineIfBlock(IfStatement,
                                                      Statements,
                                                      ElseIfBlocks,
                                                      ElseBlock,
                                                      EndIfStatement.WithTrailingEOL
                                                     )
                Else
                    Dim IsInvocationExpression As Boolean = False
                    If node.Statement.IsKind(CS.SyntaxKind.ExpressionStatement) Then
                        Dim ExpressionStatement As CSS.ExpressionStatementSyntax = DirectCast(node.Statement, CSS.ExpressionStatementSyntax)
                        If ExpressionStatement.Expression.IsKind(CS.SyntaxKind.InvocationExpression) Then
                            IsInvocationExpression = ExpressionStatement.Expression.DescendantNodes().OfType(Of CSS.ConditionalExpressionSyntax).Any
                        End If
                    End If
                    If ListOfElseIfBlocks.Any() OrElse Not IsSimpleStatement(node.Statement) OrElse IsInvocationExpression Then
                        stmt = VBFactory.MultiLineIfBlock(IfStatement,
                                                          Statements,
                                                          ElseIfBlocks,
                                                          ElseBlock,
                                                          EndIfStatement.WithTrailingEOL
                                                          )
                    Else
                        If IfStatement.GetTrailingTrivia.ContainsEOLTrivia Then
                            Dim IFBlockStatements As SyntaxList(Of StatementSyntax) = Statements
                            stmt = VBFactory.MultiLineIfBlock(
                                                              IfStatement,
                                                              IFBlockStatements,
                                                              ElseIfBlocks,
                                                              ElseBlock,
                                                              EndIfStatement.WithTrailingEOL
                                                             )
                        Else
                            If ElseBlock IsNot Nothing OrElse (Statements.Count = 1 AndAlso TypeOf Statements(0) Is EmptyStatementSyntax) Then
                                stmt = VBFactory.MultiLineIfBlock(IfStatement,
                                                                  Statements,
                                                                  elseIfBlocks:=Nothing,
                                                                  ElseBlock,
                                                                  EndIfStatement.WithTrailingEOL
                                                                 )
                            Else
                                stmt = VBFactory.SingleLineIfStatement(IfKeywordWithTrivia,
                                                                       ConditionWithTrivia,
                                                                       ThenKeyword,
                                                                       Statements,
                                                                       elseClause:=Nothing
                                                                      )
                            End If
                        End If
                    End If
                End If

                Return ReplaceOneStatementWithMarkedStatements(node, stmt)
            End Function

            Public Overrides Function VisitLabeledStatement(node As CSS.LabeledStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim OpenBraceTrailingTrivia As New List(Of SyntaxTrivia)
                Dim ClosingBraceLeadingTrivia As New List(Of SyntaxTrivia)
                Dim Statements As SyntaxList(Of StatementSyntax) = ConvertBlock(node.Statement, OpenBraceTrailingTrivia, ClosingBraceLeadingTrivia)
                If OpenBraceTrailingTrivia.Any OrElse ClosingBraceLeadingTrivia.Any Then
                    'Stop
                End If
                Return VBFactory.SingletonList(Of StatementSyntax)(VBFactory.LabelStatement(GenerateSafeVBToken(node.Identifier,
                                                                                                                    IsQualifiedName:=False,
                                                                                                                    IsTypeName:=False)
                                                                                                                    )).AddRange(Statements)
            End Function

            Public Overrides Function VisitLocalDeclarationStatement(node As CSS.LocalDeclarationStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, _nodesVisitor.IsModule, TokenContext.Local)
                If modifiers.Count = 0 Then
                    modifiers.Add(DimKeyword)
                End If
                Dim LeadingTrivia As New List(Of SyntaxTrivia)
                Dim declarators As SeparatedSyntaxList(Of VariableDeclaratorSyntax) = RemodelVariableDeclaration(node.Declaration, _nodesVisitor, _semanticModel, IsFieldDeclaration:=False, LeadingTrivia)
                Dim localDeclarationStatement As LocalDeclarationStatementSyntax = VBFactory.LocalDeclarationStatement(
                                                        VBFactory.TokenList(modifiers),
                                                        declarators).
                                                    WithLeadingTrivia(LeadingTrivia).
                                                    WithAppendedTrailingTrivia(ConvertTrivia(node.SemicolonToken.TrailingTrivia)).WithTrailingEOL ' this picks up end of line comments
                ' Don't repeat leading comments
                If Not localDeclarationStatement.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    localDeclarationStatement = localDeclarationStatement.WithConvertedLeadingTriviaFrom(node)
                End If

                Dim StmtList As New List(Of StatementSyntax) From {
                    localDeclarationStatement
                }
                If node.SemicolonToken.HasLeadingTrivia And node.SemicolonToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    StmtList.Add(VBFactory.EmptyStatement.WithConvertedLeadingTriviaFrom(node.SemicolonToken))
                End If

                Return ReplaceStatementsWithMarkedStatements(node, VBFactory.List(StmtList))
            End Function

            Public Overrides Function VisitLocalFunctionStatement(node As CSS.LocalFunctionStatementSyntax) As SyntaxList(Of StatementSyntax)
                If node.AncestorsAndSelf().OfType(Of CSS.LocalFunctionStatementSyntax).Count > 1 Then
                    Return VBFactory.SingletonList(Of StatementSyntax)(VBFactory.EmptyStatement)
                End If
                Dim localFunctionSymbol As IMethodSymbol = CType(_semanticModel.GetDeclaredSymbol(node), IMethodSymbol)
                Dim indexOfFirstReferencingStatement As Integer = -1
                Dim StatementWithIssues As CSS.StatementSyntax = Nothing
                If TypeOf node.Parent Is CSS.BlockSyntax Then
                    Dim _parent As CSS.BlockSyntax = CType(node.Parent, CSS.BlockSyntax)
                    If Not _parent.Parent.IsKind(CS.SyntaxKind.MethodDeclaration, CS.SyntaxKind.ConstructorDeclaration, CS.SyntaxKind.GetAccessorDeclaration) Then
                        Return VBFactory.SingletonList(Of StatementSyntax)(VBFactory.EmptyStatement.WithLeadingTrivia(node.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=False, "Local Functions are Not support by VB")).WithPrependedLeadingTrivia(ConvertTrivia(node.GetLeadingTrivia)).WithConvertedTrailingTriviaFrom(node))
                    End If
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
                Dim parameterList As ParameterListSyntax = VBFactory.ParameterList(OpenParenToken, vbParameters, CloseParenToken)
                Dim returnsVoid As Boolean = node.ReturnType Is Nothing OrElse node.ReturnType.ToString = "void"
                Dim lambdaHeader As LambdaHeaderSyntax
                Dim Kind As VB.SyntaxKind
                Dim endblock As EndBlockStatementSyntax
                Dim csBraces As (LeftBrace As SyntaxToken, RightBrace As SyntaxToken) = node.Body.GetBraces

                Dim TypeList As New List(Of TypeSyntax)
                For Each parameter As ParameterSyntax In parameterList.Parameters
                    TypeList.Add(parameter.AsClause.Type)
                Next
                Dim returnType As TypeSyntax
                Dim modifiers As SyntaxTokenList = VBFactory.TokenList(ConvertModifiers(node.Modifiers, _nodesVisitor.IsModule, TokenContext.LocalFunction))
                If node.DescendantNodes().OfType(Of CSS.YieldStatementSyntax).Any Then
                    modifiers = modifiers.Add(IteratorKeyword)
                End If

                If returnsVoid Then
                    Kind = VB.SyntaxKind.MultiLineSubLambdaExpression
                    lambdaHeader = VBFactory.SubLambdaHeader(attributeLists:=Nothing, modifiers, parameterList, asClause:=Nothing)
                    endblock = VBFactory.EndSubStatement().WithConvertedTriviaFrom(csBraces.RightBrace)
                Else
                    returnType = DirectCast(node.ReturnType.Accept(_nodesVisitor), TypeSyntax).WithLeadingTrivia(SpaceTrivia)
                    TypeList.Add(returnType)
                    Kind = VB.SyntaxKind.MultiLineSubLambdaExpression
                    lambdaHeader = VBFactory.FunctionLambdaHeader(attributeLists:=Nothing, modifiers, parameterList, VBFactory.SimpleAsClause(returnType))
                    endblock = VBFactory.EndFunctionStatement().WithConvertedTriviaFrom(csBraces.RightBrace)
                End If
                Dim body As New SyntaxList(Of StatementSyntax)
                If node.Body IsNot Nothing Then
                    body = ReplaceStatementsWithMarkedStatements(node, node.Body.Accept(Me))
                Else
                    If node.ExpressionBody.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        body = _nodesVisitor.GetExpressionBodyStatements(node.ExpressionBody.WithoutLeadingTrivia)
                    Else
                        body = _nodesVisitor.GetExpressionBodyStatements(node.ExpressionBody)
                    End If
                End If
                Dim nameToken As SyntaxToken = GenerateSafeVBToken(node.Identifier,
                                                                    IsQualifiedName:=False,
                                                                    IsTypeName:=False)
                Dim asClause As SimpleAsClauseSyntax = Nothing
                If TypeList.Any Then
                    Dim typeArguments As TypeArgumentListSyntax = VBFactory.TypeArgumentList(VBFactory.SeparatedList(TypeList))
                    Dim genericName As TypeSyntax = VBFactory.GenericName(VBFactory.Identifier(If(returnsVoid, "Action", "Func")), typeArguments)
                    asClause = VBFactory.SimpleAsClause(genericName)
                Else
                    asClause = VBFactory.SimpleAsClause(VBFactory.IdentifierName("Action"))
                End If
                Dim lambdaExpression As MultiLineLambdaExpressionSyntax = VBFactory.MultiLineLambdaExpression(
                                                            Kind,
                                                            lambdaHeader.WithoutLeadingTrivia.WithTrailingEOL,
                                                            body,
                                                            endblock)
                Dim initializer As EqualsValueSyntax = VBFactory.EqualsValue(lambdaExpression)
                Dim dimStatement As LocalDeclarationStatementSyntax = FactoryDimStatement(nameToken, asClause, initializer).WithConvertedTrailingTriviaFrom(node).
                                        WithPrependedLeadingTrivia(VBFactory.CommentTrivia($"' TODO Check: Local function was replaced with Lambda"))
                If StatementWithIssues.Equals(node) Then
                    Return VBFactory.SingletonList(Of StatementSyntax)(dimStatement.WithConvertedTriviaFrom(node))
                End If
                'StatementWithIssues.AddMarker(dimStatement, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                Return VBFactory.SingletonList(Of StatementSyntax)(dimStatement)
            End Function

            Public Overrides Function VisitLockStatement(node As CSS.LockStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim LockStatement As SyncLockStatementSyntax = VBFactory.SyncLockStatement(DirectCast(node.Expression?.Accept(_nodesVisitor), ExpressionSyntax)).
                                                                                            WithConvertedLeadingTriviaFrom(node)
                Dim OpenBrace As SyntaxToken = node.GetBraces.Item1
                Dim OpenBraceTrivia As New List(Of SyntaxTrivia)
                OpenBraceTrivia.AddRange(ConvertTrivia(OpenBrace.LeadingTrivia))
                OpenBraceTrivia.AddRange(ConvertTrivia(OpenBrace.TrailingTrivia))
                Dim ClosingBrace As SyntaxToken = node.GetBraces.Item2
                Dim ClosingBraceTrivia As New List(Of SyntaxTrivia)
                ClosingBraceTrivia.AddRange(ConvertTrivia(ClosingBrace.LeadingTrivia))
                ClosingBraceTrivia.AddRange(ConvertTrivia(ClosingBrace.TrailingTrivia))

                Dim Statements As SyntaxList(Of StatementSyntax) = ConvertBlock(node.Statement, OpenBraceTrivia, ClosingBraceTrivia)
                Dim EndSyncLockStatement As EndBlockStatementSyntax = VBFactory.EndSyncLockStatement.WithLeadingTrivia(ClosingBraceTrivia).WithAppendedTrailingTrivia(ConvertTrivia(node.GetTrailingTrivia))
                Dim LockBlock As SyncLockBlockSyntax = VBFactory.SyncLockBlock(LockStatement.WithTrailingEOL, Statements, EndSyncLockStatement)
                Return ReplaceOneStatementWithMarkedStatements(node, LockBlock)
            End Function

            Public Overrides Function VisitReturnStatement(node As CSS.ReturnStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim stmt As StatementSyntax
                Dim MovedLeadingTrivia As New List(Of SyntaxTrivia)
                Dim Expression As ExpressionSyntax
                If node.Expression Is Nothing Then
                    stmt = VBFactory.ReturnStatement.
                                        WithConvertedTriviaFrom(node).
                                        WithTrailingEOL
                Else
                    Expression = DirectCast(node.Expression.Accept(_nodesVisitor), ExpressionSyntax)
                    ' TODO Handle ref expressions
                    If Expression IsNot Nothing Then
                        MovedLeadingTrivia.AddRange(ConvertTrivia(node.GetLeadingTrivia))
                        If Expression.HasLeadingTrivia AndAlso Not Expression.GetLeadingTrivia.ContainsEndIfTrivia Then
                            MovedLeadingTrivia.AddRange(Expression.GetLeadingTrivia)
                        Else
                            node.AddMarker(VBFactory.EmptyStatement.WithLeadingTrivia(Expression.GetLeadingTrivia), StatementHandlingOption.AppendEmptyStatement, AllowDuplicates:=False)
                        End If
                        Expression = Expression?.WithLeadingTrivia(SpaceTrivia)
                    End If
                    stmt = VBFactory.ReturnStatement(Expression?.WithLeadingTrivia(SpaceTrivia)).
                                            WithLeadingTrivia(MovedLeadingTrivia).
                                            WithTrailingTrivia(ConvertTrivia(node.SemicolonToken.TrailingTrivia)).
                                            WithTrailingEOL
                End If
                Return ReplaceOneStatementWithMarkedStatements(node, stmt)
            End Function

            Public Overrides Function VisitSwitchStatement(node As CSS.SwitchStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim stmt As StatementSyntax
                _blockInfo.Push(New BlockInfo())
                Try
                    Dim blocks As List(Of CaseBlockSyntax) = node.Sections.Select(AddressOf ConvertSwitchSection).ToList
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
                            Expression = VBFactory.TrueLiteralExpression(TrueKeyword)
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
                    Dim EndSelectStatement As EndBlockStatementSyntax = VBFactory.EndBlockStatement(
                                                                    VB.SyntaxKind.EndSelectStatement,
                                                                    EndKeyword,
                                                                    SelectKeyword).
                                                                        WithConvertedTriviaFrom(node.CloseBraceToken)
                    stmt = VBFactory.SelectBlock(
                                VBFactory.SelectStatement(SelectKeyword,
                                                            CaseKeyword,
                                                            If(Expression, DirectCast(node.Expression.Accept(_nodesVisitor), ExpressionSyntax))
                                                            ).WithTrailingEOL,
                                VBFactory.List(nodes:=AddLabels(blocks:=OrderedBlocks.ToArray,
                                gotoLabels:=_blockInfo.Peek().GotoCaseExpressions)),
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
                            VBFactory.ThrowStatement().WithTrailingEOL,
                            VBFactory.ThrowStatement(DirectCast(node.Expression.Accept(_nodesVisitor), ExpressionSyntax)).WithTrailingEOL)
                Return ReplaceStatementsWithMarkedStatements(node, VBFactory.SingletonList(stmt.WithConvertedTriviaFrom(node)))
            End Function

            Public Overrides Function VisitTryStatement(node As CSS.TryStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim OpenBraceTrailingTrivia As New List(Of SyntaxTrivia)
                Dim ClosingBraceLeadingTrivia As New List(Of SyntaxTrivia)
                Dim TryStatement As TryStatementSyntax = VBFactory.TryStatement()
                Dim CatchBlocks As SyntaxList(Of CatchBlockSyntax) = VBFactory.List(node.Catches.IndexedSelect(AddressOf ConvertCatchClause))
                Dim TriviaList As New List(Of SyntaxTrivia)
                For blockIndex As Integer = 0 To CatchBlocks.Count - 1
                    Dim CatchBlock As CatchBlockSyntax = CatchBlocks(blockIndex)
                    If CatchBlock.Statements(0).IsKind(VB.SyntaxKind.EmptyStatement) Then
                        Dim TempTriviaList As New List(Of SyntaxTrivia)
                        TempTriviaList.AddRange(CatchBlock.Statements(0).GetTrailingTrivia)
                        TriviaList.AddRange(CatchBlock.GetBraces().Item2.LeadingTrivia)
                        CatchBlocks.Replace(CatchBlocks(blockIndex), VBFactory.CatchBlock(CatchBlock.CatchStatement.WithLeadingTrivia(TriviaList)))
                        TriviaList = TempTriviaList
                    Else
                        CatchBlocks = CatchBlocks.Replace(CatchBlock, CatchBlock.WithLeadingTrivia(TriviaList))
                        TriviaList.Clear()
                    End If
                Next
                If CatchBlocks.Count - 1 >= 0 Then
                    CatchBlocks = CatchBlocks.Replace(CatchBlocks(0), CatchBlocks(0).WithConvertedTriviaFrom(node.Block.CloseBraceToken).WithTrailingEOL)
                End If
                Dim FinallyBlock As FinallyBlockSyntax = Nothing
                If node.Finally IsNot Nothing Then
                    Dim FinallyStatements As SyntaxList(Of StatementSyntax) = ConvertBlock(node.[Finally].Block, OpenBraceTrailingTrivia, ClosingBraceLeadingTrivia)
                    FinallyBlock = VBFactory.FinallyBlock(FinallyStatements).WithPrependedLeadingTrivia(TriviaList)
                    TriviaList.Clear()
                    If FinallyBlock.Statements(0).IsKind(VB.SyntaxKind.EmptyStatement) Then
                        TriviaList.AddRange(FinallyBlock.Statements(0).GetTrailingTrivia)
                        FinallyBlock = FinallyBlock.WithTrailingTrivia(VBEOLTrivia)
                    End If
                End If
                Dim EndTryStatement As EndBlockStatementSyntax = VBFactory.EndTryStatement()
                If node.Catches.Any Then
                    EndTryStatement = EndTryStatement.WithConvertedTriviaFrom(node.Catches.Last.Block.GetBraces.Item2)
                Else
                    EndTryStatement = EndTryStatement.WithLeadingTrivia(ClosingBraceLeadingTrivia)
                End If
                If TriviaList.Any Then
                    EndTryStatement = EndTryStatement.WithLeadingTrivia(TriviaList)
                End If
                Dim TryBlockStatements As SyntaxList(Of StatementSyntax) = ConvertBlock(node.Block, OpenBraceTrailingTrivia, ClosingBraceLeadingTrivia)
                If ClosingBraceLeadingTrivia.Any Then
                    If FinallyBlock IsNot Nothing Then
                        FinallyBlock = FinallyBlock.WithPrependedLeadingTrivia(ClosingBraceLeadingTrivia)
                    End If
                End If
                Dim block As TryBlockSyntax = VBFactory.TryBlock(TryStatement,
                                                                    TryBlockStatements,
                                                                    CatchBlocks,
                                                                    FinallyBlock,
                                                                    EndTryStatement
                                                                    )
                Return VBFactory.SingletonList(Of StatementSyntax)(block.WithConvertedTriviaFrom(node))
            End Function

            Public Overrides Function VisitUnsafeStatement(node As CSS.UnsafeStatementSyntax) As SyntaxList(Of StatementSyntax)
                Return VBFactory.SingletonList(Of StatementSyntax)(FlagUnsupportedStatements(node, "Unsafe statement", CommentOutOriginalStatements:=True))
            End Function

            Public Overrides Function VisitUsingStatement(node As CSS.UsingStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim UsingStatement As UsingStatementSyntax
                Dim OpenBraceTrailingTrivia As New List(Of SyntaxTrivia)
                Dim ClosingBraceLeadingTrivia As New List(Of SyntaxTrivia)
                Dim LeadingTrivia As New List(Of SyntaxTrivia)
                If node.Declaration Is Nothing Then
                    Dim UsingBlock As UsingBlockSyntax
                    LeadingTrivia.AddRange(ConvertTrivia(node.GetLeadingTrivia))
                    If node.Expression IsNot Nothing AndAlso node.Expression.IsKind(CS.SyntaxKind.ConditionalAccessExpression) Then
                        Dim csConditionalAccessExpression As CSS.ConditionalAccessExpressionSyntax = DirectCast(node.Expression, CSS.ConditionalAccessExpressionSyntax)
                        Dim VB_ConditionalAccessExpression As VisualBasicSyntaxNode = csConditionalAccessExpression.Expression.Accept(_nodesVisitor)
                        Dim Condition As BinaryExpressionSyntax = VBFactory.IsNotExpression(left:=CType(VB_ConditionalAccessExpression, ExpressionSyntax),
                                                                                                right:=NothingExpression)
                        Dim IfStatement As IfStatementSyntax = VBFactory.IfStatement(Condition)
                        UsingStatement = VBFactory.UsingStatement(VBFactory.ParseExpression($"{VB_ConditionalAccessExpression}{csConditionalAccessExpression.WhenNotNull.Accept(_nodesVisitor)}"), VBFactory.SeparatedList(Of VariableDeclaratorSyntax)())
                        UsingBlock = VBFactory.UsingBlock(UsingStatement.WithTrailingEOL, ConvertBlock(node.Statement, OpenBraceTrailingTrivia, ClosingBraceLeadingTrivia)).WithLeadingTrivia(LeadingTrivia)
                        Dim IfStatementBlock As MultiLineIfBlockSyntax = VBFactory.MultiLineIfBlock(IfStatement, VBFactory.SingletonList(Of StatementSyntax)(UsingBlock), elseIfBlocks:=Nothing, elseBlock:=Nothing).WithLeadingTrivia(LeadingTrivia)
                        Return ReplaceOneStatementWithMarkedStatements(node, IfStatementBlock)
                    Else
                        UsingStatement = VBFactory.UsingStatement(DirectCast(node.Expression?.Accept(_nodesVisitor), ExpressionSyntax), VBFactory.SeparatedList(Of VariableDeclaratorSyntax)())
                    End If
                Else
                    UsingStatement = VBFactory.UsingStatement(expression:=Nothing, RemodelVariableDeclaration(node.Declaration, _nodesVisitor, _semanticModel, IsFieldDeclaration:=False, LeadingTrivia))
                End If

                Dim EndUsing As EndBlockStatementSyntax = VBFactory.EndUsingStatement.WithConvertedTriviaFrom(node.Statement.GetBraces.Item2)
                Return ReplaceOneStatementWithMarkedStatements(node, VBFactory.UsingBlock(UsingStatement.WithTrailingEOL,
                                                                                       ConvertBlock(node.Statement, OpenBraceTrailingTrivia, ClosingBraceLeadingTrivia),
                                                                                       EndUsing
                                                                                       ).WithLeadingTrivia(LeadingTrivia))
            End Function

            Public Overrides Function VisitVariableDeclaration(node As CSS.VariableDeclarationSyntax) As SyntaxList(Of StatementSyntax)
                Dim leadingTrivia As New List(Of SyntaxTrivia)
                Dim vbType As TypeSyntax = DirectCast(node.Type.Accept(_nodesVisitor), TypeSyntax)
                If vbType.HasLeadingTrivia Then
                    leadingTrivia.AddRange(vbType.GetLeadingTrivia)
                    vbType = vbType.WithLeadingTrivia(SpaceTrivia)
                End If
                Dim collectedCommentTrivia As New List(Of SyntaxTrivia)
                Dim declaratorsWithoutInitializers As New List(Of CSS.VariableDeclaratorSyntax)()
                Dim vbDeclarators As New List(Of VariableDeclaratorSyntax)
                For Each e As IndexClass(Of CSS.VariableDeclaratorSyntax) In node.Variables.WithIndex
                    Dim v As CSS.VariableDeclaratorSyntax = e.Value
                    If v.Initializer Is Nothing Then
                        declaratorsWithoutInitializers.Add(v.WithTrailingTrivia(collectedCommentTrivia))
                        Continue For
                    Else
                        Dim AsClause As SimpleAsClauseSyntax = If(node.Type.IsVar OrElse node.Type.IsKind(CS.SyntaxKind.RefType), Nothing, VBFactory.SimpleAsClause(vbType))
                        Dim Value As ExpressionSyntax = DirectCast(v.Initializer.Value.Accept(_nodesVisitor), ExpressionSyntax)
                        If Value Is Nothing Then
                            Value = VBFactory.IdentifierName("HandleRefExpression").WithConvertedTriviaFrom(v.Initializer.Value)
                        End If
                        If Value.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                            leadingTrivia.AddRange(Value.GetLeadingTrivia)
                        End If
                        Dim Initializer As EqualsValueSyntax = VBFactory.EqualsValue(Value.WithLeadingTrivia(SpaceTrivia))
                        ' Get the names last to lead with var jsonWriter = new JsonWriter(stringWriter)
                        ' Which should be Dim jsonWriter_Renamed = new JsonWriter(stringWriter)
                        Dim Names As SeparatedSyntaxList(Of ModifiedIdentifierSyntax) = VBFactory.SingletonSeparatedList(CType(v.Accept(_nodesVisitor), ModifiedIdentifierSyntax))
                        Dim Declator As VariableDeclaratorSyntax = VBFactory.VariableDeclarator(
                                                                                                Names,
                                                                                                AsClause,
                                                                                                Initializer
                                                                                                )
                        If Declator.HasTrailingTrivia Then
                            Dim FoundEOL As Boolean = False
                            Dim NonCommentTrailingTrivia As New List(Of SyntaxTrivia)
                            For Each t As SyntaxTrivia In Declator.GetTrailingTrivia
                                Select Case t.RawKind
                                    Case VB.SyntaxKind.EndOfLineTrivia
                                        FoundEOL = True
                                    Case VB.SyntaxKind.CommentTrivia
                                        collectedCommentTrivia.Add(t)
                                    Case VB.SyntaxKind.WhitespaceTrivia
                                        collectedCommentTrivia.Add(t)
                                        NonCommentTrailingTrivia.Add(t)
                                    Case Else
                                        ' Directives are ignored but the results are converted. Disabled Text is deleted
                                        'Stop
                                End Select
                            Next
                            If FoundEOL Then
                                collectedCommentTrivia.Add(VBEOLTrivia)
                                Declator = Declator.WithTrailingTrivia(collectedCommentTrivia)
                                collectedCommentTrivia.Clear()
                            Else
                                Declator = Declator.WithTrailingTrivia(NonCommentTrailingTrivia)
                            End If
                            If e.IsLast Then
                                If Not Declator.HasTrailingTrivia OrElse Not Declator.GetTrailingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                    Declator = Declator.WithAppendedTrailingTrivia(VBEOLTrivia)
                                End If
                            End If
                        End If
                        vbDeclarators.Add(Declator)
                    End If
                Next
                If declaratorsWithoutInitializers.Any Then
                    Stop
                End If
                Dim LocalDeclarationStatement As LocalDeclarationStatementSyntax = VBFactory.LocalDeclarationStatement(
                    VBFactory.TokenList(DimModifier),
                    VBFactory.SeparatedList(vbDeclarators)).WithoutTrivia.WithLeadingTrivia(leadingTrivia).WithAppendedTrailingTrivia(ConvertTrivia(node.GetTrailingTrivia)) ' this picks up end of line comments
                ' Don't repeat leading comments
                If Not LocalDeclarationStatement.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    LocalDeclarationStatement = LocalDeclarationStatement.WithConvertedLeadingTriviaFrom(node)
                End If
                Return VBFactory.SingletonList(Of StatementSyntax)(LocalDeclarationStatement)
            End Function

            Public Overrides Function VisitWhileStatement(node As CSS.WhileStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim condition As ExpressionSyntax = DirectCast(node.Condition.Accept(_nodesVisitor), ExpressionSyntax)
                Dim OpenBraceTrailingTrivia As New List(Of SyntaxTrivia)
                Dim ClosingBraceLeadingTrivia As New List(Of SyntaxTrivia)
                Dim WhileStatements As SyntaxList(Of StatementSyntax) = ConvertBlock(node.Statement, OpenBraceTrailingTrivia, ClosingBraceLeadingTrivia)
                If OpenBraceTrailingTrivia.Any Then
                    WhileStatements = WhileStatements.Replace(WhileStatements.First, WhileStatements.First.WithPrependedLeadingTrivia(OpenBraceTrailingTrivia))
                    OpenBraceTrailingTrivia.Clear()
                End If
                Dim EndWhileStatement As EndBlockStatementSyntax = VBFactory.EndWhileStatement().WithLeadingTrivia(ClosingBraceLeadingTrivia)
                Dim block As WhileBlockSyntax = VBFactory.WhileBlock(VBFactory.WhileStatement(condition).WithConvertedLeadingTriviaFrom(node.WhileKeyword).WithTrailingEOL, WhileStatements, EndWhileStatement)
                Return ReplaceOneStatementWithMarkedStatements(node, block)
            End Function

            Public Overrides Function VisitYieldStatement(node As CSS.YieldStatementSyntax) As SyntaxList(Of StatementSyntax)
                IsInterator = True
                Dim stmt As StatementSyntax
                If node.Expression Is Nothing Then
                    stmt = VBFactory.ReturnStatement.WithTrailingEOL
                Else
                    stmt = VBFactory.YieldStatement(DirectCast(node.Expression.Accept(_nodesVisitor), ExpressionSyntax)).WithTrailingEOL
                End If
                Return ReplaceOneStatementWithMarkedStatements(node, stmt.WithConvertedTriviaFrom(node))
            End Function

            Private Class BlockInfo
                Public ReadOnly GotoCaseExpressions As New List(Of VisualBasicSyntaxNode)()
            End Class

        End Class

    End Class

End Namespace
