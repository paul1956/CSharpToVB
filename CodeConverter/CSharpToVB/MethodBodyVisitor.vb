' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

Imports System.Diagnostics.CodeAnalysis
Imports System.Runtime.InteropServices
Imports System.Text

Imports IVisualBasicCode.CodeConverter.Util

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CSharp
Imports Microsoft.CodeAnalysis.Simplification
Imports Microsoft.CodeAnalysis.VisualBasic

Imports CS = Microsoft.CodeAnalysis.CSharp

Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax

Imports VB = Microsoft.CodeAnalysis.VisualBasic

Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace IVisualBasicCode.CodeConverter.Visual_Basic

    Partial Public NotInheritable Class CSharpConverter

        Friend Class MethodBodyVisitor
            Inherits CS.CSharpSyntaxVisitor(Of SyntaxList(Of VBS.StatementSyntax))

            Private ReadOnly LiteralExpression_1 As VBS.ExpressionSyntax = VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal(1))
            Private ReadOnly mBlockInfo As Stack(Of BlockInfo) = New Stack(Of BlockInfo)()
            Private ReadOnly mNodesVisitor As NodesVisitor
            Private ReadOnly mSemanticModel As SemanticModel
            ' currently only works with switch blocks

            Private switchCount As Integer = 0

            Public Sub New(semanticModel As SemanticModel, nodesVisitor As NodesVisitor)
                Me.mSemanticModel = semanticModel
                Me.mNodesVisitor = nodesVisitor
            End Sub

            Public Property IsInterator As Boolean

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

            Private Iterator Function AddLabels(blocks As VBS.CaseBlockSyntax(), gotoLabels As List(Of VB.VisualBasicSyntaxNode)) As IEnumerable(Of VBS.CaseBlockSyntax)
                For Each _block As VBS.CaseBlockSyntax In blocks
                    Dim block As VBS.CaseBlockSyntax = _block
                    For Each caseClause As VBS.CaseClauseSyntax In block.CaseStatement.Cases
                        Dim expression As VB.VisualBasicSyntaxNode = If(TypeOf caseClause Is VBS.ElseCaseClauseSyntax, DirectCast(caseClause, VB.VisualBasicSyntaxNode), (DirectCast(caseClause, VBS.SimpleCaseClauseSyntax)).Value)
                        If gotoLabels.Any(Function(label As VB.VisualBasicSyntaxNode) label.IsEquivalentTo(expression.WithoutTrivia)) Then
                            block = block.WithStatements(block.Statements.Insert(0, VBFactory.LabelStatement(Me.MakeGotoSwitchLabel(expression))))
                        End If
                    Next

                    Yield block
                Next
            End Function

            Private Sub CollectElseBlocks(node As CSS.IfStatementSyntax, elseIfBlocks As List(Of VBS.ElseIfBlockSyntax), ByRef elseBlock As VBS.ElseBlockSyntax, ByRef OpenBraceTrailingTrivia As List(Of SyntaxTrivia), ByRef CloseBraceLeadingTrivia As List(Of SyntaxTrivia))
                If node.[Else] Is Nothing Then
                    Return
                End If

                If TypeOf node.[Else].Statement Is CSS.IfStatementSyntax Then
                    Dim [elseIf] As CSS.IfStatementSyntax = DirectCast(node.[Else].Statement, CSS.IfStatementSyntax)
                    Dim ElseIFKeywordWithTrivia As SyntaxToken = ElseIfKeyword.WithLeadingTrivia(ConvertTrivia(node.[Else].Statement.GetLeadingTrivia)).WithPrependedLeadingTrivia(ConvertTrivia(node.[Else].GetLeadingTrivia))
                    Dim NewThenTrailingTrivia As New List(Of SyntaxTrivia)
                    Dim Condition As VBS.ExpressionSyntax = DirectCast([elseIf].Condition.Accept(Me.mNodesVisitor), VBS.ExpressionSyntax)
                    NewThenTrailingTrivia.AddRange(Condition.GetTrailingTrivia)
                    If node.CloseParenToken.HasLeadingTrivia AndAlso node.CloseParenToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        NewThenTrailingTrivia.AddRange(ConvertTrivia(node.CloseParenToken.LeadingTrivia))
                    End If
                    'NewThenTrailingTrivia.AddRange(ConvertTrivia([elseIf].GetLeadingTrivia))
                    Dim ThenKeywordWIthTrivia As SyntaxToken = ThenKeyword.WithTrailingTrivia(NewThenTrailingTrivia)
                    Dim ElseIfStatement As VBS.ElseIfStatementSyntax = VBFactory.ElseIfStatement(ElseIFKeywordWithTrivia, Condition.WithTrailingTrivia(SpaceTrivia), ThenKeywordWIthTrivia)
                    Dim ElseIfBlock As VBS.ElseIfBlockSyntax = VBFactory.ElseIfBlock(ElseIfStatement.WithTrailingEOL, Me.ConvertBlock([elseIf].Statement, OpenBraceTrailingTrivia, CloseBraceLeadingTrivia))
                    elseIfBlocks.Add(ElseIfBlock)
                    Me.CollectElseBlocks([elseIf], elseIfBlocks, elseBlock, OpenBraceTrailingTrivia, CloseBraceLeadingTrivia)
                Else
                    Dim Statements As SyntaxList(Of VBS.StatementSyntax) = Me.ConvertBlock(node.[Else].Statement, OpenBraceTrailingTrivia, CloseBraceLeadingTrivia)
                    Dim TrailingTriviaList As New List(Of SyntaxTrivia)
                    If node.Else.Statement.GetBraces.Item1.HasTrailingTrivia Then
                        TrailingTriviaList.AddRange(ConvertTrivia(node.Else.Statement.GetBraces.Item1.TrailingTrivia))
                    End If
                    Dim ElseStatement As VBS.ElseStatementSyntax = VBFactory.ElseStatement(ElseKeyword.WithConvertedLeadingTriviaFrom(node.Else.ElseKeyword)).WithTrailingTrivia(TrailingTriviaList)

                    elseBlock = VBFactory.ElseBlock(ElseStatement, Statements).WithPrependedLeadingTrivia(OpenBraceTrailingTrivia).WithAppendedTrailingTrivia(CloseBraceLeadingTrivia)
                    OpenBraceTrailingTrivia.Clear()
                    CloseBraceLeadingTrivia.Clear()
                End If
            End Sub

            Private Function ConvertBlock(node As CSS.StatementSyntax, ByRef OpenBraceTrailiningTrivia As List(Of SyntaxTrivia), ByRef CloseBraceLeadingTrivia As List(Of SyntaxTrivia)) As SyntaxList(Of VBS.StatementSyntax)
                Dim Braces As (OpenBrace As SyntaxToken, CloseBrace As SyntaxToken) = node.GetBraces
                Dim OpenBrace As SyntaxToken = If(Braces.OpenBrace = Nothing, New SyntaxToken, Braces.OpenBrace)
                Dim OpenBraceLeadingTrivia As New List(Of SyntaxTrivia)
                OpenBraceLeadingTrivia.AddRange(ConvertTrivia(OpenBrace.LeadingTrivia))
                OpenBraceTrailiningTrivia.AddRange(ConvertTrivia(OpenBrace.TrailingTrivia))
                Dim CloseBrace As SyntaxToken = If(Braces.CloseBrace = Nothing, New SyntaxToken, Braces.CloseBrace)
                CloseBraceLeadingTrivia.AddRange(ConvertTrivia(CloseBrace.LeadingTrivia))
                Select Case True
                    Case TypeOf node Is CSS.BlockSyntax
                        Dim NodeBlock As CSS.BlockSyntax = DirectCast(node, CSS.BlockSyntax)
                        Dim StatementList As New List(Of VBS.StatementSyntax)
                        For i As Integer = 0 To NodeBlock.Statements.Count - 1
                            Dim Statements As List(Of VBS.StatementSyntax) = NodeBlock.Statements(i).Accept(Me).ToList
                            If i = 0 AndAlso Statements.Count > 0 Then
                                Statements(0) = Statements(0).WithPrependedLeadingTrivia(OpenBraceLeadingTrivia)
                            End If
                            StatementList.AddRange(Statements)
                        Next

                        If StatementList.Count = 0 Then
                            StatementList.Add(VBFactory.EmptyStatement.WithConvertedTriviaFrom(NodeBlock))
                        Else
                            If Not (StatementList.First.IsKind(VB.SyntaxKind.EmptyStatement) OrElse
                                   (StatementList.First.IsKind(VB.SyntaxKind.TryBlock) AndAlso StatementList.First.GetLeadingTrivia.Count = 1)
                                    ) Then
                                StatementList.Item(0) = StatementList(0).WithLeadingTrivia(ConvertTrivia(NodeBlock.Statements(0).GetLeadingTrivia)).WithPrependedLeadingTrivia(ConvertTrivia(node.GetLeadingTrivia))
                            End If
                            StatementList.Item(StatementList.Count - 1) = StatementList.Last.WithTrailingTrivia(ConvertTrivia(NodeBlock.Statements.Last.GetTrailingTrivia)).WithTrailingEOL
                        End If
                        Dim OpenBraceToken As SyntaxToken = node.GetBraces.Item1
                        Dim CloseBraceToken As SyntaxToken = node.GetBraces.Item2
                        Return VBFactory.List(StatementList)
                    Case TypeOf node Is CSS.EmptyStatementSyntax
                        Return VBFactory.List(Of VBS.StatementSyntax)()
                End Select

                Return node.Accept(Me)
            End Function

            Private Function ConvertCatchClause(index As Integer, catchClause As CSS.CatchClauseSyntax) As VBS.CatchBlockSyntax
                Dim OpenBraceTrailingTrivia As New List(Of SyntaxTrivia)
                Dim ClosingBraceLeadingTrivia As New List(Of SyntaxTrivia)
                Dim statements As SyntaxList(Of VBS.StatementSyntax) = Me.ConvertBlock(catchClause.Block, OpenBraceTrailingTrivia, ClosingBraceLeadingTrivia)

                If catchClause.Declaration Is Nothing Then
                    Return VBFactory.CatchBlock(VBFactory.CatchStatement().WithTrailingTrivia(VB_EOLTrivia).WithAppendedTrailingTrivia(ClosingBraceLeadingTrivia), statements)
                End If
                If OpenBraceTrailingTrivia.Count > 0 OrElse ClosingBraceLeadingTrivia.Count > 0 Then
                    statements = statements.Replace(statements(0), statements(0).WithPrependedLeadingTrivia(OpenBraceTrailingTrivia))
                    Dim laststatement As Integer = statements.Count - 1
                    statements = statements.Replace(statements(laststatement), statements(laststatement).WithAppendedTrailingTrivia(ClosingBraceLeadingTrivia))
                End If
                Dim type As VBS.TypeSyntax = DirectCast(catchClause.Declaration.Type.Accept(Me.mNodesVisitor), VBS.TypeSyntax)
                Dim simpleTypeName As String
                simpleTypeName = If(TypeOf type Is VBS.QualifiedNameSyntax, (DirectCast(type, VBS.QualifiedNameSyntax)).Right.ToString(), type.ToString())
                Dim identifier As SyntaxToken = If(catchClause.Declaration.Identifier.IsKind(CS.SyntaxKind.None),
                                                        VBFactory.Identifier($"__unused{simpleTypeName}{index + 1}__"),
                                                        GenerateSafeVBToken(catchClause.Declaration.Identifier, IsQualifiedName:=False, IsTypeName:=False))
                Dim WhenClause As VBS.CatchFilterClauseSyntax = If(catchClause.Filter Is Nothing, Nothing, VBFactory.CatchFilterClause(filter:=DirectCast(catchClause.Filter.FilterExpression.Accept(Me.mNodesVisitor), VBS.ExpressionSyntax)))
                Dim CatchStatement As VBS.CatchStatementSyntax = VBFactory.CatchStatement(
                                                        identifierName:=VBFactory.IdentifierName(identifier),
                                                        asClause:=VBFactory.SimpleAsClause(type),
                                                        whenClause:=WhenClause).WithConvertedLeadingTriviaFrom(catchClause)
                If Not CatchStatement.HasTrailingTrivia Then
                    CatchStatement = CatchStatement.WithTrailingTrivia(VB_EOLTrivia)
                ElseIf CatchStatement.GetTrailingTrivia.Last <> VB_EOLTrivia Then
                    CatchStatement = CatchStatement.WithTrailingTrivia(VB_EOLTrivia)
                End If
                Return VBFactory.CatchBlock(CatchStatement, statements)
            End Function

            Private Function ConvertForToSimpleForNext(node As CSS.ForStatementSyntax, <Out> ByRef block As VBS.StatementSyntax, ByRef HasVariable As Boolean) As Boolean
                block = Nothing
                HasVariable = node.Declaration IsNot Nothing AndAlso node.Declaration.Variables.Count = 1
                If Not HasVariable AndAlso node.Initializers.Count <> 1 Then
                    Return False
                End If
                If node.Incrementors.Count <> 1 Then
                    Return False
                End If
                Dim Incrementors As VB.VisualBasicSyntaxNode = node.Incrementors.FirstOrDefault()?.Accept(Me.mNodesVisitor)
                Dim iterator As VBS.AssignmentStatementSyntax = TryCast(Incrementors, VBS.AssignmentStatementSyntax)
                If iterator Is Nothing OrElse Not iterator.IsKind(VB.SyntaxKind.AddAssignmentStatement, VB.SyntaxKind.SubtractAssignmentStatement) Then
                    Return False
                End If
                Dim iteratorIdentifier As VBS.IdentifierNameSyntax = TryCast(iterator.Left, VBS.IdentifierNameSyntax)
                If iteratorIdentifier Is Nothing Then
                    Return False
                End If
                Dim StepExpression As VBS.LiteralExpressionSyntax = TryCast(iterator.Right, VBS.LiteralExpressionSyntax)
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
                Dim ToValue As VBS.ExpressionSyntax
                If iterator.IsKind(VB.SyntaxKind.SubtractAssignmentStatement) Then
                    If condition.IsKind(CS.SyntaxKind.GreaterThanOrEqualExpression) OrElse condition.IsKind(CS.SyntaxKind.NotEqualsExpression) Then
                        ToValue = DirectCast(condition.Right.Accept(Me.mNodesVisitor), VBS.ExpressionSyntax)
                    ElseIf condition.IsKind(CS.SyntaxKind.GreaterThanExpression) Then
                        ToValue = VBFactory.BinaryExpression(VB.SyntaxKind.AddExpression,
                                                                 DirectCast(condition.Right.Accept(Me.mNodesVisitor), VBS.ExpressionSyntax),
                                                                 PlusToken,
                                                                 Me.LiteralExpression_1)
                    Else
                        Return False
                    End If
                Else
                    If condition.IsKind(CS.SyntaxKind.LessThanOrEqualExpression) OrElse condition.IsKind(CS.SyntaxKind.NotEqualsExpression) Then
                        ToValue = DirectCast(condition.Right.Accept(Me.mNodesVisitor), VBS.ExpressionSyntax)
                    ElseIf condition.IsKind(CS.SyntaxKind.LessThanExpression) Then
                        ToValue = VBFactory.BinaryExpression(VB.SyntaxKind.SubtractExpression,
                                                                 DirectCast(condition.Right.Accept(Me.mNodesVisitor), VBS.ExpressionSyntax),
                                                                 MinusToken,
                                                                 Me.LiteralExpression_1)
                    Else
                        Return False
                    End If
                End If

                Dim ControlVariable As VB.VisualBasicSyntaxNode
                Dim FromValue As VBS.ExpressionSyntax
                If HasVariable Then
                    Dim v As CSS.VariableDeclaratorSyntax = node.Declaration.Variables(0)
                    FromValue = DirectCast(v.Initializer?.Value.Accept(Me.mNodesVisitor), VBS.ExpressionSyntax)
                    If FromValue Is Nothing Then
                        Return False
                    End If
                    ControlVariable = VBFactory.VariableDeclarator(VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(GenerateSafeVBToken(id:=v.Identifier, IsQualifiedName:=False, IsTypeName:=False))), asClause:=If(node.Declaration.Type.IsVar, Nothing, VBFactory.SimpleAsClause(type:=DirectCast(node.Declaration.Type.Accept(Me.mNodesVisitor), VBS.TypeSyntax))), initializer:=Nothing)
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
                    ControlVariable = initializer.Left.Accept(Me.mNodesVisitor)
                    FromValue = DirectCast(initializer.Right.Accept(Me.mNodesVisitor), VBS.ExpressionSyntax)
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
                Dim Statements As SyntaxList(Of VBS.StatementSyntax) = Me.ConvertBlock(node.Statement, OpenBraceTrailingTrivia, ClosingBraceLeadingTrivia)

                Dim ForStatementTrailingTrivia As New List(Of SyntaxTrivia)
                If StatementFirstToken.IsKind(CS.SyntaxKind.OpenBraceToken) Then
                    If StatementFirstToken.TrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                        ForStatementTrailingTrivia.AddRange(ConvertTrivia(StatementFirstToken.TrailingTrivia))
                    End If
                End If

                If ForStatementTrailingTrivia.Count > 0 AndAlso ForStatementTrailingTrivia.ContainsCommentOrDirectiveTrivia AndAlso Not ForStatementTrailingTrivia(0).IsEndOfLine Then
                    ForStatementTrailingTrivia.Insert(0, VB_EOLTrivia)
                End If

                Dim StepClause As VBS.ForStepClauseSyntax = If([step] = 1,
                                                                Nothing,
                                                                VBFactory.ForStepClause(
                                                                            VBFactory.LiteralExpression(
                                                                                            VB.SyntaxKind.NumericLiteralExpression,
                                                                                            VBFactory.Literal([step])
                                                                                            )
                                                                            )
                                                                )
                Dim ForStatement As VBS.ForStatementSyntax = VBFactory.ForStatement(ForKeyword.WithConvertedLeadingTriviaFrom(node.ForKeyword),
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

            Private Function ConvertSingleExpression(node As CSS.ExpressionSyntax) As VBS.StatementSyntax
                Dim exprNode As VB.VisualBasicSyntaxNode = Nothing
                Dim NewLeadingTrivia As New List(Of SyntaxTrivia)

                If TypeOf node Is CSS.AssignmentExpressionSyntax Then
                    Dim CS_Assignment As CSS.AssignmentExpressionSyntax = DirectCast(node, CSS.AssignmentExpressionSyntax)
                    If CS_Assignment.Left.IsKind(CS.SyntaxKind.ParenthesizedExpression) Then
                        Dim CS_Left As CSS.ParenthesizedExpressionSyntax = DirectCast(CS_Assignment.Left, CSS.ParenthesizedExpressionSyntax)
                        Dim LeftExpression As VBS.ExpressionSyntax = CType(CS_Left.Expression.Accept(Me.mNodesVisitor), VBS.ExpressionSyntax)
                        Dim RightExpression As VBS.ExpressionSyntax = DirectCast(CS_Assignment.Right.Accept(Me.mNodesVisitor), VBS.ExpressionSyntax)
                        If CS_Assignment.IsKind(CS.SyntaxKind.SimpleAssignmentExpression) Then
                            exprNode = VBFactory.SimpleAssignmentStatement(LeftExpression, RightExpression).
                                                         WithConvertedTriviaFrom(node)
                            NewLeadingTrivia.AddRange(CheckCorrectnessLeadingTrivia(node, "Parenthesized Expression Assignment"))
                        End If
                    End If
                ElseIf TypeOf node Is CSS.PostfixUnaryExpressionSyntax Then
                    Dim CSPostFixUnaryExpression As CSS.PostfixUnaryExpressionSyntax = DirectCast(node, CSS.PostfixUnaryExpressionSyntax)
                    If TypeOf CSPostFixUnaryExpression.Operand Is CSS.ParenthesizedExpressionSyntax Then
                        Dim CS_Operand As CSS.ParenthesizedExpressionSyntax = DirectCast(CSPostFixUnaryExpression.Operand, CSS.ParenthesizedExpressionSyntax)
                        Dim kind As VB.SyntaxKind = ConvertCSExpressionsKindToVBKind(CS.CSharpExtensions.Kind(node))
                        Dim OperandExpression As VBS.ExpressionSyntax = DirectCast(CS_Operand.Expression.Accept(Me.mNodesVisitor), VBS.ExpressionSyntax)
                        exprNode = VBFactory.AssignmentStatement(ConvertCSExpressionsKindToVBKind(CS.CSharpExtensions.Kind(node)),
                                                                OperandExpression,
                                                                ExpressionKindToOperatorToken(kind),
                                                                Me.LiteralExpression_1)
                        NewLeadingTrivia.AddRange(CheckCorrectnessLeadingTrivia(node, "Parenthesized Expression Assignment"))
                    End If
                End If
                If exprNode Is Nothing Then
                    exprNode = node.Accept(Me.mNodesVisitor)
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
                If Not (TypeOf exprNode Is VBS.StatementSyntax) Then
                    Select Case True
                        Case TypeOf exprNode Is VBS.ObjectCreationExpressionSyntax
                            Dim variable As VBS.VariableDeclaratorSyntax = VBFactory.VariableDeclarator(VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(VBFactory.Identifier(GetUniqueVariableNameInScope(node, "tempVar", Me.mSemanticModel)))), VBFactory.AsNewClause(DirectCast(exprNode, VBS.NewExpressionSyntax)), initializer:=Nothing)
                            Dim SeparatedListOfvariableDeclarations As SeparatedSyntaxList(Of VBS.VariableDeclaratorSyntax) = VBFactory.SingletonSeparatedList(variable)
                            exprNode = VBFactory.LocalDeclarationStatement(DimModifier, SeparatedListOfvariableDeclarations)
                        Case TypeOf exprNode Is VBS.InvocationExpressionSyntax
                            exprNode = If(exprNode.GetFirstToken.IsKind(VB.SyntaxKind.NewKeyword), VBFactory.CallStatement(DirectCast(exprNode, VBS.ExpressionSyntax)), DirectCast(VBFactory.ExpressionStatement(DirectCast(exprNode, VBS.ExpressionSyntax)), VB.VisualBasicSyntaxNode))
                        Case Else
                            exprNode = VBFactory.ExpressionStatement(DirectCast(exprNode, VBS.ExpressionSyntax))
                    End Select
                End If
                Return DirectCast(exprNode, VBS.StatementSyntax).WithLeadingTrivia(NewLeadingTrivia).WithTrailingTrivia(NewTrailingTrivia).WithTrailingEOL
            End Function

            Private Function ConvertSwitchSection(section As CSS.SwitchSectionSyntax) As VBS.CaseBlockSyntax
                Dim NewDimStatements As New List(Of VBS.StatementSyntax)
                If section.Labels.OfType(Of CSS.DefaultSwitchLabelSyntax)().Any() Then
                    Return VBFactory.CaseElseBlock(VBFactory.CaseElseStatement(VBFactory.ElseCaseClause()), Me.ConvertSwitchSectionBlock(section, NewDimStatements))
                End If
                Dim LabelList As New List(Of VBS.CaseClauseSyntax)
                Dim CS_LabelTrivia As New List(Of SyntaxTrivia)
                Dim NewLeadingTrivia As New List(Of SyntaxTrivia)
                Dim NeedWarningMessage As Boolean = False
                NewLeadingTrivia.AddRange(ConvertTrivia(section.GetLeadingTrivia))
                ' Find Case leading space
                For Each CaseLabel As CSS.SwitchLabelSyntax In section.Labels
                    Dim CaseLabelExpression As VBS.ExpressionSyntax
                    Dim CaseLabelWhenExpression As VBS.ExpressionSyntax
                    Select Case True
                        Case TypeOf CaseLabel Is CSS.CaseSwitchLabelSyntax
                            CaseLabelExpression = DirectCast(CType(CaseLabel, CSS.CaseSwitchLabelSyntax).Value.Accept(Me.mNodesVisitor), VBS.ExpressionSyntax)
                            CaseLabelWhenExpression = Nothing
                            CS_LabelTrivia.AddRange(CaseLabel.GetTrailingTrivia)
                        Case TypeOf CaseLabel Is CSS.CasePatternSwitchLabelSyntax
                            Dim PatternLabel As CSS.CasePatternSwitchLabelSyntax = DirectCast(CaseLabel, CSS.CasePatternSwitchLabelSyntax)
                            Dim Identifier As SyntaxToken
                            CaseLabelWhenExpression = CType(PatternLabel.WhenClause?.Accept(Me.mNodesVisitor), VBS.ExpressionSyntax)
                            If TypeOf PatternLabel.Pattern Is CSS.ConstantPatternSyntax Then
                                Dim ConstantPattern As CSS.ConstantPatternSyntax = DirectCast(PatternLabel.Pattern, CSS.ConstantPatternSyntax).WithLeadingTrivia()
                                CaseLabelExpression = DirectCast(ConstantPattern.Expression.Accept(Me.mNodesVisitor), VBS.ExpressionSyntax)
                                CS_LabelTrivia.AddRange(CaseLabel.GetTrailingTrivia)
                            ElseIf TypeOf PatternLabel.Pattern Is CSS.DeclarationPatternSyntax Then
                                Dim Pattern As CSS.DeclarationPatternSyntax = DirectCast(PatternLabel.Pattern, CSS.DeclarationPatternSyntax)
                                Dim Type As VBS.TypeSyntax = DirectCast(Pattern.Type.Accept(Me.mNodesVisitor), VBS.TypeSyntax)
                                If TypeOf Pattern.Designation Is CSS.SingleVariableDesignationSyntax Then
                                    Identifier = GenerateSafeVBToken(DirectCast(Pattern.Designation, CSS.SingleVariableDesignationSyntax).Identifier, IsQualifiedName:=False, IsTypeName:=False)
                                ElseIf TypeOf Pattern.Designation Is CSS.DiscardDesignationSyntax Then
                                Else
                                    Stop
                                End If

                                Dim SwitchExpression1 As VBS.ExpressionSyntax = DirectCast(DirectCast(section.Parent, CSS.SwitchStatementSyntax).Expression.Accept(Me.mNodesVisitor), VBS.ExpressionSyntax)
                                If TypeOf Pattern.Designation Is CSS.DiscardDesignationSyntax Then
                                    CaseLabelExpression = VBFactory.TypeOfIsExpression(SwitchExpression1, CType(Pattern.Type.Accept(Me.mNodesVisitor), VBS.TypeSyntax))
                                Else
                                    Dim SeparatedSyntaxList As SeparatedSyntaxList(Of VBS.ModifiedIdentifierSyntax) = VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(Identifier.WithTrailingTrivia(SpaceTrivia)))
                                    Dim Initializer As VBS.EqualsValueSyntax = VBFactory.EqualsValue(VBFactory.CTypeExpression(SwitchExpression1, Type))
                                    Dim variable As VBS.VariableDeclaratorSyntax = VBFactory.VariableDeclarator(SeparatedSyntaxList, VBFactory.SimpleAsClause(Type), Initializer)
                                    Dim Declarators As SeparatedSyntaxList(Of VBS.VariableDeclaratorSyntax) = VBFactory.SingletonSeparatedList(variable)

                                    NewDimStatements.Add(VBFactory.LocalDeclarationStatement(DimModifier, Declarators))
                                    CaseLabelExpression = DirectCast(Pattern.Designation.Accept(Me.mNodesVisitor), VBS.ExpressionSyntax)
                                End If
                            ElseIf TypeOf PatternLabel.Pattern Is CSS.VarPatternSyntax Then
                                Dim VarPattern As CSS.VarPatternSyntax = CType(PatternLabel.Pattern, CSS.VarPatternSyntax)
                                Dim SwitchExpression1 As VBS.ExpressionSyntax = DirectCast(DirectCast(section.Parent, CSS.SwitchStatementSyntax).Expression.Accept(Me.mNodesVisitor), VBS.ExpressionSyntax)

                                CaseLabelExpression = Nothing
                                If VarPattern.Designation.IsKind(CS.SyntaxKind.DiscardDesignation) Then
                                    If PatternLabel.WhenClause IsNot Nothing Then
                                        CaseLabelWhenExpression = CType(PatternLabel.WhenClause.Accept(Me.mNodesVisitor), VBS.ExpressionSyntax)
                                    Else
                                        CaseLabelExpression = VBFactory.IdentifierName("Else")
                                    End If
                                ElseIf VarPattern.Designation.IsKind(CS.SyntaxKind.SingleVariableDesignation) Then
                                    Identifier = GenerateSafeVBToken(DirectCast(VarPattern.Designation, CSS.SingleVariableDesignationSyntax).Identifier, IsQualifiedName:=False, IsTypeName:=False)
                                    Dim AsClause As VBS.AsClauseSyntax = VBFactory.SimpleAsClause(VBFactory.PredefinedType(ObjectKeyword))
                                    Dim Initializer As VBS.EqualsValueSyntax = VBFactory.EqualsValue(SwitchExpression1)
                                    Dim ModifiedIdentifier As VBS.ModifiedIdentifierSyntax = VBFactory.ModifiedIdentifier(Identifier.WithTrailingTrivia(SpaceTrivia))
                                    Dim Variable As VBS.VariableDeclaratorSyntax = VBFactory.VariableDeclarator(VBFactory.SingletonSeparatedList(ModifiedIdentifier), AsClause, Initializer)
                                    Dim Declarators As SeparatedSyntaxList(Of VBS.VariableDeclaratorSyntax) = VBFactory.SingletonSeparatedList(Variable)
                                    NewDimStatements.Add(VBFactory.LocalDeclarationStatement(DimModifier, Declarators).WithTrailingEOL)
                                    CaseLabelExpression = VBFactory.IdentifierName("Else")
                                Else
                                    CaseLabelExpression = Nothing
                                    Stop
                                End If
                            ElseIf TypeOf PatternLabel.Pattern Is CSS.RecursivePatternSyntax Then
                                CaseLabelExpression = NothingExpression
                                NewLeadingTrivia.AddRange(section.CheckCorrectnessLeadingTrivia($"VB has no equivalent to the C# 'Recursive Pattern({PatternLabel.Pattern.ToString}) in 'case' statements"))
                            Else
                                CaseLabelExpression = Nothing
                                Stop
                            End If
                            If PatternLabel.WhenClause IsNot Nothing Then
                                NeedWarningMessage = True
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
                            LabelList.Add(VBFactory.SimpleCaseClause(CaseLabelExpression))
                        Else
                            ' TODO use line continuation instead of space
                            LabelList.Add(VBFactory.SimpleCaseClause(VBFactory.AndAlsoExpression(CaseLabelExpression.WithTrailingTrivia(SpaceTrivia), CaseLabelWhenExpression).WithLeadingTrivia(SpaceTrivia)))
                        End If
                    End If
                Next
                If NeedWarningMessage Then
                    NewLeadingTrivia.AddRange(section.CheckCorrectnessLeadingTrivia("VB has no equivalent to the C# 'when' clause in 'case' statements"))
                End If
                Dim CommentString As New StringBuilder
                For Each t As SyntaxTrivia In ConvertTrivia(CS_LabelTrivia)
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
                    TrailingTrivia.Add(VBFactory.CommentTrivia($" ' {CommentString.ToString}"))
                    TrailingTrivia.Add(VB_EOLTrivia)
                End If
                Dim CaseStatement As VBS.CaseStatementSyntax = VBFactory.CaseStatement(VBFactory.SeparatedList(LabelList)).With(NewLeadingTrivia, TrailingTrivia).WithTrailingEOL
                Return VBFactory.CaseBlock(CaseStatement, Me.ConvertSwitchSectionBlock(section, NewDimStatements))
            End Function

            Private Function ConvertSwitchSectionBlock(section As CSS.SwitchSectionSyntax, Statements As List(Of VBS.StatementSyntax)) As SyntaxList(Of VBS.StatementSyntax)
                Dim lastStatement As CSS.StatementSyntax = section.Statements.LastOrDefault()
                Dim OpenBraceTrailingTrivia As New List(Of SyntaxTrivia)
                Dim ClosingBraceLeadingTrivia As New List(Of SyntaxTrivia)
                For Each s As CSS.StatementSyntax In section.Statements
                    If s Is lastStatement AndAlso TypeOf s Is CSS.BreakStatementSyntax Then
                        Statements.Add(VBFactory.EmptyStatement.WithConvertedTriviaFrom(lastStatement))
                        Continue For
                    End If
                    Statements.AddRange(Me.ConvertBlock(s, OpenBraceTrailingTrivia, ClosingBraceLeadingTrivia))
                    If OpenBraceTrailingTrivia.Count > 0 Then
                        Statements(0) = Statements.First.WithPrependedLeadingTrivia(OpenBraceTrailingTrivia)
                    End If
                    If ClosingBraceLeadingTrivia.Count > 0 Then
                        Statements(Statements.Count - 1) = Statements.Last.WithAppendedTrailingTrivia(ClosingBraceLeadingTrivia)
                    End If
                Next
                Return VBFactory.List(Statements)
            End Function

            Private Function MakeGotoSwitchLabel(Expression As VB.VisualBasicSyntaxNode) As String
                If TypeOf Expression Is VBS.ElseCaseClauseSyntax Then
                    Return $"_Select{Me.switchCount}_CaseDefault"
                End If
                Dim ExpressionBuilder As New StringBuilder
                Dim ExpressionText As String = Expression.ToString.Replace(".", "Dot").Replace("""", "Quote").Replace("[", "OpenBracket").Replace("]", "CloseBracket").Replace(" ", "_")
                Dim c As Char = ExpressionText(0)
                If VisualBasic.SyntaxFacts.IsIdentifierStartCharacter(c) Then
                    ExpressionBuilder.Append("_")
                Else
                    If IsNumeric(c) Then
                        ExpressionBuilder.Append($"_{c}")
                    Else
                        ExpressionBuilder.Append(c)
                    End If
                End If
                For i As Integer = 1 To ExpressionText.Length - 1
                    c = ExpressionText(i)
                    If VisualBasic.SyntaxFacts.IsIdentifierPartCharacter(c) Then
                        ExpressionBuilder.Append(c)
                    Else
                        ExpressionBuilder.Append("_")
                    End If
                Next
                Return ExpressionBuilder.ToString
            End Function

            Private Function TryConvertRaiseEvent(node As CSS.IfStatementSyntax, <Out> ByRef name As VBS.IdentifierNameSyntax, arguments As List(Of VBS.ArgumentSyntax)) As Boolean
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
                arguments.AddRange(invocation.ArgumentList.Arguments.Select(Function(a As CSS.ArgumentSyntax) DirectCast(a.Accept(Me.mNodesVisitor), VBS.ArgumentSyntax)))
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
                Dim Incrementors As VB.VisualBasicSyntaxNode = node.Incrementors.FirstOrDefault()?.Accept(Me.mNodesVisitor)
                Dim iterator As VBS.AssignmentStatementSyntax = TryCast(Incrementors, VBS.AssignmentStatementSyntax)
                If iterator Is Nothing OrElse Not iterator.IsKind(VB.SyntaxKind.AddAssignmentStatement, VB.SyntaxKind.SubtractAssignmentStatement) Then
                    Return False
                End If
                Dim iteratorIdentifier As VBS.IdentifierNameSyntax = TryCast(iterator.Left, VBS.IdentifierNameSyntax)
                If iteratorIdentifier Is Nothing Then
                    Return False
                End If
                Dim stepExpression As VBS.LiteralExpressionSyntax = TryCast(iterator.Right, VBS.LiteralExpressionSyntax)
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

                Dim start As VBS.ExpressionSyntax
                If hasVariable Then
                    Dim v As CSS.VariableDeclaratorSyntax = node.Declaration.Variables(0)
                    start = DirectCast(v.Initializer?.Value.Accept(Me.mNodesVisitor), VBS.ExpressionSyntax)
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

            Public Shared Function GetUniqueVariableNameInScope(node As CS.CSharpSyntaxNode, variableNameBase As String, lSemanticModel As SemanticModel) As String
                Dim mWithBlockTempVariableNames As New List(Of String)
                Dim reservedNames As IEnumerable(Of String) = mWithBlockTempVariableNames.Concat(node.DescendantNodesAndSelf().SelectMany(Function(lSyntaxNode As SyntaxNode) lSemanticModel.LookupSymbols(lSyntaxNode.SpanStart).Select(Function(s As ISymbol) s.Name))).Distinct
                Dim UniqueVariableName As String = EnsureUniqueness(variableNameBase, reservedNames, isCaseSensitive:=False)
                UsedIdentifiers.Add(UniqueVariableName, value:=New SymbolTableEntry(_Name:=UniqueVariableName, _IsType:=False))
                Return UniqueVariableName
            End Function

            <ExcludeFromCodeCoverage>
            Public Overrides Function DefaultVisit(node As SyntaxNode) As SyntaxList(Of VBS.StatementSyntax)
                Throw New NotImplementedException(node.[GetType]().ToString & " not implemented!")
            End Function

            Public Overrides Function VisitBlock(node As CSS.BlockSyntax) As SyntaxList(Of VBS.StatementSyntax)
                Dim ListOfStatements As SyntaxList(Of VBS.StatementSyntax) = VBFactory.List(node.Statements.Where(Function(s As CSS.StatementSyntax) Not (TypeOf s Is CSS.EmptyStatementSyntax)).SelectMany(Function(s As CSS.StatementSyntax) s.Accept(Me)))
                If node.OpenBraceToken.HasLeadingTrivia OrElse node.OpenBraceToken.HasTrailingTrivia Then
                    ListOfStatements = ListOfStatements.Insert(0, VBFactory.EmptyStatement.WithConvertedTriviaFrom(node.OpenBraceToken))
                End If
                If node.CloseBraceToken.HasLeadingTrivia OrElse node.OpenBraceToken.HasTrailingTrivia Then
                    ListOfStatements = ListOfStatements.Add(VBFactory.EmptyStatement.WithConvertedTriviaFrom(node.CloseBraceToken))
                End If
                Return ListOfStatements
            End Function

            Public Overrides Function VisitBreakStatement(node As CSS.BreakStatementSyntax) As SyntaxList(Of VBS.StatementSyntax)
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
                    End If

                    If TypeOf stmt Is CSS.SwitchStatementSyntax Then
                        StatementKind = VB.SyntaxKind.ExitSelectStatement
                        BlockKeyword = SelectKeyword
                        Exit For
                    End If
                Next

                Return VBFactory.SingletonList(Of VBS.StatementSyntax)(VBFactory.ExitStatement(StatementKind,
                                                                                                   BlockKeyword
                                                                                                   ).WithConvertedTriviaFrom(node))
            End Function

            Public Overrides Function VisitCheckedStatement(node As CSS.CheckedStatementSyntax) As SyntaxList(Of VBS.StatementSyntax)
                Dim OpenBraceTrailingTrivia As New List(Of SyntaxTrivia)
                Dim ClosingBraceLeadingTrivia As New List(Of SyntaxTrivia)
                If node.Keyword.ValueText = "checked" Then
                    Return WrapInComment(Me.ConvertBlock(node.Block, OpenBraceTrailingTrivia, ClosingBraceLeadingTrivia), node, "Visual Basic Default Is checked math, check that this works for you!")
                End If
                Return WrapInComment(Me.ConvertBlock(node.Block, OpenBraceTrailingTrivia, ClosingBraceLeadingTrivia), node, "Visual Basic does Not support unchecked statements!")
            End Function

            Public Overrides Function VisitContinueStatement(node As CSS.ContinueStatementSyntax) As SyntaxList(Of VBS.StatementSyntax)
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

                Return VBFactory.SingletonList(Of VBS.StatementSyntax)(VBFactory.ContinueStatement(statementKind,
                                                                                                       BlockKeyword
                                                                                                       ).WithConvertedTriviaFrom(node))
            End Function

            Public Overrides Function VisitDeclarationExpression(node As CSS.DeclarationExpressionSyntax) As SyntaxList(Of VBS.StatementSyntax)
                Return MyBase.VisitDeclarationExpression(node)
            End Function

            Public Overrides Function VisitDoStatement(node As CSS.DoStatementSyntax) As SyntaxList(Of VBS.StatementSyntax)
                Dim condition As VBS.ExpressionSyntax = DirectCast(node.Condition.Accept(Me.mNodesVisitor), VBS.ExpressionSyntax)
                Dim OpenBraceTrailingTrivia As New List(Of SyntaxTrivia)
                Dim ClosingBraceLeadingTrivia As New List(Of SyntaxTrivia)
                Dim stmt As SyntaxList(Of VBS.StatementSyntax) = Me.ConvertBlock(node.Statement, OpenBraceTrailingTrivia, ClosingBraceLeadingTrivia)
                'If OpenBraceTrailingTrivia.Count > 0 OrElse ClosingBraceLeadingTrivia.Count > 0 Then
                '    Stop
                'End If
                Dim DoStatement As VBS.DoStatementSyntax = VBFactory.DoStatement(VB.SyntaxKind.SimpleDoStatement)
                Dim LoopStatement As VBS.LoopStatementSyntax = VBFactory.LoopStatement(VB.SyntaxKind.LoopWhileStatement, VBFactory.WhileClause(condition).WithTrailingEOL)
                Dim block As VBS.DoLoopBlockSyntax = VBFactory.DoLoopWhileBlock(DoStatement, stmt, LoopStatement)
                Dim syntaxList1 As SyntaxList(Of VBS.StatementSyntax) = VBFactory.SingletonList(Of VBS.StatementSyntax)(block)
                Return ReplaceStatementsWithMarkedStatements(node, syntaxList1)
            End Function

            Public Overrides Function VisitEmptyStatement(node As CSS.EmptyStatementSyntax) As SyntaxList(Of VBS.StatementSyntax)
                Return VBFactory.SingletonList(Of VBS.StatementSyntax)(VBFactory.EmptyStatement().WithConvertedTriviaFrom(node))
            End Function

            Public Overrides Function VisitExpressionStatement(node As CSS.ExpressionStatementSyntax) As SyntaxList(Of VBS.StatementSyntax)
                Dim Initializer As VBS.EqualsValueSyntax
                Dim CS_Expression As CSS.ExpressionSyntax = node.Expression
                Dim Declarator As VBS.VariableDeclaratorSyntax = Nothing
                Dim Statement As VBS.StatementSyntax = Nothing
                Dim TrailingTrivia As New List(Of SyntaxTrivia)

                If node.GetFirstToken.IsKind(CS.SyntaxKind.NewKeyword) AndAlso CS_Expression.IsKind(CS.SyntaxKind.SimpleAssignmentExpression) Then
                    ' Handle this case
                    'Dim x As New IO.FileInfo("C:\temp") With {.IsReadOnly = True}
                    Dim CS_AssignmentExpression As CSS.AssignmentExpressionSyntax = CType(CS_Expression, CSS.AssignmentExpressionSyntax)

                    If TypeOf CS_AssignmentExpression.Left Is CSS.MemberAccessExpressionSyntax Then
                        Initializer = VBFactory.EqualsValue(CType(CS_AssignmentExpression.Right.Accept(Me.mNodesVisitor), VBS.ExpressionSyntax))
                        Dim CS_MemberAccessExpression As CSS.MemberAccessExpressionSyntax = CType(CS_AssignmentExpression.Left, CSS.MemberAccessExpressionSyntax)
                        Dim CS_ObjectCreationExpression As CSS.ObjectCreationExpressionSyntax = CType(CS_MemberAccessExpression.Expression, CSS.ObjectCreationExpressionSyntax)

                        Dim ObjectInitializer As VBS.ObjectCreationInitializerSyntax
                        Dim NewObject As VBS.ObjectCreationExpressionSyntax
                        If TypeOf CS_ObjectCreationExpression.Type IsNot CSS.IdentifierNameSyntax Then
                            Dim AssignmentStatement As VBS.AssignmentStatementSyntax = DirectCast(CS_AssignmentExpression.Accept(Me.mNodesVisitor), VBS.AssignmentStatementSyntax)
                            Dim MemberAccessExpression As VBS.MemberAccessExpressionSyntax = DirectCast(AssignmentStatement.Left, VBS.MemberAccessExpressionSyntax)
                            Dim FieldInitializer As VBS.FieldInitializerSyntax = VBFactory.NamedFieldInitializer(name:=DirectCast(MemberAccessExpression.Name, VBS.IdentifierNameSyntax), expression:=Initializer.Value)
                            ObjectInitializer = VBFactory.ObjectMemberInitializer(FieldInitializer)
                            Dim ObjectCreateExpression As VBS.ObjectCreationExpressionSyntax = DirectCast(MemberAccessExpression.Expression, VBS.ObjectCreationExpressionSyntax)
                            NewObject = VBFactory.ObjectCreationExpression(
                                                    NewKeyword,
                                                    ObjectCreateExpression.AttributeLists,
                                                    ObjectCreateExpression.Type,
                                                    ObjectCreateExpression.ArgumentList,
                                                    ObjectInitializer)
                        ElseIf TypeOf CS_ObjectCreationExpression.Type Is CSS.IdentifierNameSyntax Then
                            Dim FieldInitializer As VBS.FieldInitializerSyntax = VBFactory.NamedFieldInitializer(name:=DirectCast(CS_MemberAccessExpression.Name.Accept(Me.mNodesVisitor), VBS.IdentifierNameSyntax), expression:=Initializer.Value)
                            ObjectInitializer = VBFactory.ObjectMemberInitializer(FieldInitializer)

                            NewObject = VBFactory.ObjectCreationExpression(NewKeyword,
                                                                           Nothing,
                                                                           CType(CS_ObjectCreationExpression.Type.Accept(Me.mNodesVisitor), VBS.IdentifierNameSyntax),
                                                                           CType(CS_ObjectCreationExpression.ArgumentList.Accept(Me.mNodesVisitor), VBS.ArgumentListSyntax),
                                                                           ObjectInitializer)
                        Else
                            NewObject = Nothing
                            Stop
                        End If
                        Dim AsNewClause As VBS.AsNewClauseSyntax = VBFactory.AsNewClause(newExpression:=NewObject.WithLeadingTrivia(SpaceTrivia))
                        Dim Names As SeparatedSyntaxList(Of VBS.ModifiedIdentifierSyntax) = VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(GetUniqueVariableNameInScope(node, "TempVar", Me.mSemanticModel)))
                        Dim Declarators As SeparatedSyntaxList(Of VBS.VariableDeclaratorSyntax) = VBFactory.SingletonSeparatedList(VBFactory.VariableDeclarator(Names, AsNewClause, initializer:=Nothing))
                        Statement = VBFactory.LocalDeclarationStatement(DimModifier, Declarators)
                    End If
                ElseIf node.ToString.StartsWith("(") AndAlso CS_Expression.IsKind(CS.SyntaxKind.InvocationExpression) Then
                    Dim InvovationExpression As CSS.InvocationExpressionSyntax = DirectCast(CS_Expression, CSS.InvocationExpressionSyntax)
                    Dim CS_InvocationExpression As CSS.InvocationExpressionSyntax = InvovationExpression
                    Dim exprNode As VBS.InvocationExpressionSyntax
                    Dim AccessExpression As VBS.MemberAccessExpressionSyntax = Nothing
                    If InvovationExpression.Expression.IsKind(CS.SyntaxKind.SimpleMemberAccessExpression) Then
                        exprNode = DirectCast(CS_Expression.Accept(Me.mNodesVisitor), VBS.InvocationExpressionSyntax)
                        AccessExpression = DirectCast(exprNode.Expression, VBS.MemberAccessExpressionSyntax)
                    ElseIf InvovationExpression.Expression.IsKind(CS.SyntaxKind.ParenthesizedExpression) Then
                        Dim ParenthesizedExpression As CSS.ExpressionSyntax = CType(InvovationExpression.Expression, CSS.ParenthesizedExpressionSyntax).Expression
                        CS_Expression = CType(InvovationExpression.Expression, CSS.ParenthesizedExpressionSyntax)
                        If ParenthesizedExpression.IsKind(CS.SyntaxKind.SimpleMemberAccessExpression) Then
                            CS_Expression = ParenthesizedExpression
                            AccessExpression = DirectCast(CS_Expression.Accept(Me.mNodesVisitor), VBS.MemberAccessExpressionSyntax)
                        ElseIf ParenthesizedExpression.IsKind(CS.SyntaxKind.CastExpression) Then
                            Dim CTypeExpression As VBS.CastExpressionSyntax = DirectCast(ParenthesizedExpression.Accept(Me.mNodesVisitor), VBS.CTypeExpressionSyntax)
                            Dim Type As VBS.TypeSyntax = CTypeExpression.Type
                            Initializer = VBFactory.EqualsValue(CTypeExpression.WithLeadingTrivia(SpaceTrivia))
                            Dim AsClause As VBS.AsClauseSyntax = VBFactory.SimpleAsClause(Type)
                            Dim Names As SeparatedSyntaxList(Of VBS.ModifiedIdentifierSyntax) = VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(GetUniqueVariableNameInScope(node, "TempVar", Me.mSemanticModel)))
                            Declarator = VBFactory.VariableDeclarator(Names, AsClause, Initializer)
                        Else
                            Stop
                        End If
                    ElseIf InvovationExpression.Expression.IsKind(CS.SyntaxKind.PointerMemberAccessExpression) Then
                        AccessExpression = CType(InvovationExpression.Expression.Accept(Me.mNodesVisitor), VBS.MemberAccessExpressionSyntax)
                    Else
                        Stop
                    End If
                    Dim StmtList As New SyntaxList(Of VBS.StatementSyntax)
                    Dim IdentifierString As String = ""
                    If Declarator Is Nothing Then
                        ' Handle this case
                        ' (new Exception("RequestServiceAsync Timeout")).ReportServiceHubNFW("RequestServiceAsync Timeout");
                        Select Case AccessExpression.Expression.Kind
                            Case VB.SyntaxKind.IdentifierName
                                Declarator = Nothing
                                IdentifierString = AccessExpression.Expression.ToString
                            Case VB.SyntaxKind.CTypeExpression, VB.SyntaxKind.TryCastExpression
                                Dim AsClause As VBS.AsClauseSyntax
                                If AccessExpression.Expression.IsKind(VB.SyntaxKind.CTypeExpression) Then
                                    Dim CTypeExpression As VBS.CTypeExpressionSyntax = CType(AccessExpression.Expression, VBS.CTypeExpressionSyntax)
                                    AsClause = VBFactory.SimpleAsClause(CTypeExpression.Type.WithLeadingTrivia(SpaceTrivia))
                                    Initializer = VBFactory.EqualsValue(CTypeExpression.WithLeadingTrivia(SpaceTrivia))
                                Else
                                    Dim TryCastExpression As VBS.TryCastExpressionSyntax = CType(AccessExpression.Expression, VBS.TryCastExpressionSyntax)
                                    AsClause = VBFactory.SimpleAsClause(TryCastExpression.Type.WithLeadingTrivia(SpaceTrivia))
                                    Initializer = VBFactory.EqualsValue(TryCastExpression.WithLeadingTrivia(SpaceTrivia))
                                End If
                                Dim Names As SeparatedSyntaxList(Of VBS.ModifiedIdentifierSyntax) = VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(GetUniqueVariableNameInScope(node, "TempVar", Me.mSemanticModel)))
                                Declarator = VBFactory.VariableDeclarator(Names, asClause:=AsClause, initializer:=Initializer)
                            Case VB.SyntaxKind.ParenthesizedExpression
                                Dim ParenthesizedExpression As VBS.ParenthesizedExpressionSyntax = CType(AccessExpression.Expression, VBS.ParenthesizedExpressionSyntax)
                                Initializer = VBFactory.EqualsValue(ParenthesizedExpression.Expression.WithLeadingTrivia(SpaceTrivia))
                                Dim Names As SeparatedSyntaxList(Of VBS.ModifiedIdentifierSyntax) = VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(GetUniqueVariableNameInScope(node, "TempVar", Me.mSemanticModel)))
                                Select Case ParenthesizedExpression.Expression.Kind
                                    Case VB.SyntaxKind.ObjectCreationExpression
                                        Dim NewObject As VBS.ObjectCreationExpressionSyntax = CType(ParenthesizedExpression.Expression, VBS.ObjectCreationExpressionSyntax)
                                        Dim AsNewClause As VBS.AsNewClauseSyntax = VBFactory.AsNewClause(NewObject.WithLeadingTrivia(SpaceTrivia))
                                        Declarator = VBFactory.VariableDeclarator(Names, AsNewClause, initializer:=Nothing)
                                    Case VB.SyntaxKind.PredefinedCastExpression
                                        Dim InvocationExpression As CSS.InvocationExpressionSyntax = CType(CS_Expression, CSS.InvocationExpressionSyntax)
                                        Dim MemberAccessExpression As CSS.MemberAccessExpressionSyntax = CType(InvocationExpression.Expression, CSS.MemberAccessExpressionSyntax)
                                        Dim CS_ParenthesizedExpression As CSS.ParenthesizedExpressionSyntax = CType(MemberAccessExpression.Expression, CSS.ParenthesizedExpressionSyntax)
                                        Dim CastExpression As CSS.CastExpressionSyntax = CType(CS_ParenthesizedExpression.Expression, CSS.CastExpressionSyntax)
                                        Dim Type As VBS.TypeSyntax = CType(CastExpression.Type.Accept(Me.mNodesVisitor), VBS.TypeSyntax)
                                        Dim AsClause As VBS.AsClauseSyntax = VBFactory.SimpleAsClause(Type)
                                        Declarator = VBFactory.VariableDeclarator(Names, AsClause, Initializer)
                                    Case VB.SyntaxKind.AwaitExpression
                                        Dim InvocationExpression As CSS.InvocationExpressionSyntax = CType(CS_Expression, CSS.InvocationExpressionSyntax)
                                        Dim MemberAccessExpression As CSS.MemberAccessExpressionSyntax = CType(InvocationExpression.Expression, CSS.MemberAccessExpressionSyntax)
                                        Dim CS_ParenthesizedExpression As CSS.ParenthesizedExpressionSyntax = CType(MemberAccessExpression.Expression, CSS.ParenthesizedExpressionSyntax)
                                        Dim AwaitExpression As CSS.AwaitExpressionSyntax = CType(CS_ParenthesizedExpression.Expression, CSS.AwaitExpressionSyntax)
                                        Dim CS_AwaitExpressionInfo As CS.AwaitExpressionInfo = Me.mSemanticModel.GetAwaitExpressionInfo(AwaitExpression)
                                        Dim Type As VBS.TypeSyntax
                                        If CS_AwaitExpressionInfo.GetResultMethod Is Nothing Then
                                            Type = VBFactory.ParseTypeName($"Object")
                                        Else
                                            Type = VBFactory.ParseTypeName($" {CS_AwaitExpressionInfo.GetResultMethod.ReturnType.ToString}")
                                        End If
                                        Dim AsClause As VBS.AsClauseSyntax = VBFactory.SimpleAsClause(Type)
                                        Declarator = VBFactory.VariableDeclarator(Names, AsClause, Initializer)
                                    Case Else
                                        Declarator = Nothing
                                        Stop
                                End Select
                            Case VB.SyntaxKind.InvocationExpression
                                Dim MemberAccessExpression As VBS.InvocationExpressionSyntax = CType(AccessExpression.Expression, VBS.InvocationExpressionSyntax)
                                Initializer = VBFactory.EqualsValue(MemberAccessExpression.WithLeadingTrivia(SpaceTrivia))
                                Dim Names As SeparatedSyntaxList(Of VBS.ModifiedIdentifierSyntax) = VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(GetUniqueVariableNameInScope(node, "TempVar", Me.mSemanticModel)))
                                Declarator = VBFactory.VariableDeclarator(Names, asClause:=Nothing, Initializer)
                            Case VB.SyntaxKind.SimpleMemberAccessExpression
                                Dim MemberAccessExpression As VBS.MemberAccessExpressionSyntax = CType(AccessExpression.Expression, VBS.MemberAccessExpressionSyntax)
                                Initializer = VBFactory.EqualsValue(MemberAccessExpression.WithLeadingTrivia(SpaceTrivia))
                                Dim Names As SeparatedSyntaxList(Of VBS.ModifiedIdentifierSyntax) = VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(GetUniqueVariableNameInScope(node, "TempVar", Me.mSemanticModel)))
                                Declarator = VBFactory.VariableDeclarator(Names, asClause:=Nothing, Initializer)
                            Case Else
                                Declarator = Nothing
                                Stop
                        End Select
                    End If
                    If Declarator IsNot Nothing Then
                        Dim Declarators As SeparatedSyntaxList(Of VBS.VariableDeclaratorSyntax) = VBFactory.SingletonSeparatedList(Declarator)
                        StmtList = StmtList.Add(VBFactory.LocalDeclarationStatement(DimModifier, Declarators).WithConvertedLeadingTriviaFrom(node))
                    End If
                    If AccessExpression IsNot Nothing Then
                        With AccessExpression
                            Dim Identifier As VBS.ExpressionSyntax = VBFactory.IdentifierName(IdentifierString)
                            Dim MemberAccessExpression As VBS.ExpressionSyntax = VBFactory.MemberAccessExpression(kind:= .Kind, expression:=Identifier, operatorToken:= .OperatorToken, name:= .Name)
                            Dim AgrumentList As VBS.ArgumentListSyntax = DirectCast(CS_InvocationExpression.ArgumentList.Accept(Me.mNodesVisitor), VBS.ArgumentListSyntax)
                            Dim InvocationExpression As VBS.InvocationExpressionSyntax = VBFactory.InvocationExpression(MemberAccessExpression, AgrumentList).WithConvertedTriviaFrom(node)
                            StmtList = StmtList.Add(VBFactory.ExpressionStatement(InvocationExpression))
                        End With
                    End If
                    Return ReplaceStatementsWithMarkedStatements(node, StmtList)
                End If
                If Statement Is Nothing Then
                    Statement = Me.ConvertSingleExpression(CS_Expression)
                End If

                Dim syntaxList1 As SyntaxList(Of VBS.StatementSyntax) = VBFactory.SingletonList(Statement.WithAppendedTrailingTrivia(ConvertTrivia(node.GetTrailingTrivia)).WithTrailingEOL)
                Return ReplaceStatementsWithMarkedStatements(node, syntaxList1)
            End Function

            Public Overrides Function VisitFixedStatement(node As CSS.FixedStatementSyntax) As SyntaxList(Of VBS.StatementSyntax)
                Return VBFactory.SingletonList(Of VBS.StatementSyntax)(FlagUnsupportedStatements(node, "C# Fixed is not support by VB", CommentOutOriginalStatements:=True))
            End Function

            Public Overrides Function VisitForEachStatement(node As CSS.ForEachStatementSyntax) As SyntaxList(Of VBS.StatementSyntax)
                Dim variable As VB.VisualBasicSyntaxNode
                If node.Type.IsVar Then
                    variable = VBFactory.IdentifierName(GenerateSafeVBToken(node.Identifier, IsQualifiedName:=False, IsTypeName:=False))

                    Dim variableITypeSymbol As (_ITypeSymbol As ITypeSymbol, _Error As Boolean) = node.Expression.DetermineType(Me.mSemanticModel)
                    If variableITypeSymbol._Error = False Then
                        Dim _ITypeSymbol As ITypeSymbol = variableITypeSymbol._ITypeSymbol
                        variable = VBFactory.VariableDeclarator(VBFactory.SingletonSeparatedList(
                                  VBFactory.ModifiedIdentifier(GenerateSafeVBToken(node.Identifier, IsQualifiedName:=False, IsTypeName:=False)).WithTrailingTrivia(SpaceTrivia)),
                                                                asClause:=VBFactory.SimpleAsClause(NodesVisitor.GetElementType(_ITypeSymbol)),
                                                                initializer:=Nothing)

                    End If
                Else
                    Dim VBType As VBS.TypeSyntax
                    If node.Type.IsKind(CS.SyntaxKind.IdentifierName) Then
                        VBType = VBFactory.IdentifierName(GenerateSafeVBToken(DirectCast(node.Type, CSS.IdentifierNameSyntax).Identifier, IsQualifiedName:=False, IsTypeName:=True))
                    Else
                        VBType = DirectCast(node.Type.Accept(Me.mNodesVisitor), VBS.TypeSyntax)
                    End If
                    variable = VBFactory.VariableDeclarator(VBFactory.SingletonSeparatedList(
                                      VBFactory.ModifiedIdentifier(GenerateSafeVBToken(node.Identifier, IsQualifiedName:=False, IsTypeName:=False)).WithTrailingTrivia(SpaceTrivia)),
                                                                    asClause:=VBFactory.SimpleAsClause(VBType),
                                                                    initializer:=Nothing)
                End If

                    Dim expression As VBS.ExpressionSyntax = DirectCast(node.Expression.Accept(Me.mNodesVisitor), VBS.ExpressionSyntax).WithConvertedTrailingTriviaFrom(node.CloseParenToken)
                Dim OpenBraceTrailingTrivia As New List(Of SyntaxTrivia)
                Dim ClosingBraceLeadingTrivia As New List(Of SyntaxTrivia)
                Dim stmt As SyntaxList(Of VBS.StatementSyntax) = Me.ConvertBlock(node.Statement, OpenBraceTrailingTrivia, ClosingBraceLeadingTrivia)
                Dim NextStatement As VBS.NextStatementSyntax = VBFactory.NextStatement().WithLeadingTrivia(ClosingBraceLeadingTrivia)
                Dim ForEachStatementSyntax As VBS.ForEachStatementSyntax = VBFactory.ForEachStatement(variable, expression).WithTrailingEOL
                Dim block As VBS.ForEachBlockSyntax = VBFactory.ForEachBlock(ForEachStatementSyntax.WithConvertedLeadingTriviaFrom(node.ForEachKeyword),
                                                                             stmt,
                                                                             NextStatement).WithAdditionalAnnotations(Simplifier.Annotation)
                Return ReplaceStatementsWithMarkedStatements(node, VBFactory.SingletonList(Of VBS.StatementSyntax)(block))
            End Function

            Public Overrides Function VisitForEachVariableStatement(node As CSS.ForEachVariableStatementSyntax) As SyntaxList(Of VBS.StatementSyntax)
                Return VBFactory.SingletonList(Of VBS.StatementSyntax)(FlagUnsupportedStatements(node, "For Each Variable Statement", CommentOutOriginalStatements:=True))
            End Function

            Public Overrides Function VisitForStatement(node As CSS.ForStatementSyntax) As SyntaxList(Of VBS.StatementSyntax)
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
                Dim block As VBS.StatementSyntax = Nothing

                ' check if the form Is valid And collect TypeReference, name, start, end And step
                Dim HasVariable As Boolean = False
                If Me.ConvertForToSimpleForNext(node, block, HasVariable) Then
                    Return ReplaceStatementsWithMarkedStatements(node, VBFactory.SingletonList(block))
                Else
                    Dim OpenBraceTrailingTrivia As New List(Of SyntaxTrivia)
                    Dim ClosingBraceLeadingTrivia As New List(Of SyntaxTrivia)
                    Dim stmts As SyntaxList(Of VBS.StatementSyntax) = Me.ConvertBlock(node.Statement, OpenBraceTrailingTrivia, ClosingBraceLeadingTrivia).AddRange(node.Incrementors.Select(AddressOf Me.ConvertSingleExpression))
                    Dim condition As VBS.ExpressionSyntax = If(node.Condition Is Nothing, VBFactory.TrueLiteralExpression(TrueKeyword), DirectCast(node.Condition.Accept(Me.mNodesVisitor), VBS.ExpressionSyntax))
                    Dim WhileStatement As VBS.WhileStatementSyntax = VBFactory.WhileStatement(WhileKeyword.WithConvertedLeadingTriviaFrom(node.ForKeyword),
                                                                                                condition.WithAppendedTrailingTrivia(ConvertTrivia(node.SecondSemicolonToken.TrailingTrivia))
                                                                                                ).WithAppendedTrailingTrivia(ConvertTrivia(node.GetBraces.Item1.TrailingTrivia)).WithPrependedLeadingTrivia(node.GetBraces.Item1.LeadingTrivia).WithTrailingEOL
                    WhileStatement = CType(PrependStatementWithMarkedStatementTrivia(node, WhileStatement), VBS.WhileStatementSyntax)
                    Dim EndWhileStatement As VBS.EndBlockStatementSyntax = VBFactory.EndWhileStatement.WithLeadingTrivia(ClosingBraceLeadingTrivia).WithConvertedTrailingTriviaFrom(node.GetBraces.Item2)
                    block = VBFactory.WhileBlock(WhileStatement, stmts, EndWhileStatement)
                    Dim Statements As SyntaxList(Of VBS.StatementSyntax) = VBFactory.List(node.Initializers.Select(AddressOf Me.ConvertSingleExpression)).Add(block)
                    If HasVariable Then
                        Statements = Statements.Insert(0, node.Declaration.Accept(Me).First.WithConvertedTrailingTriviaFrom(node.FirstSemicolonToken).WithTrailingEOL)
                    End If
                    Return ReplaceStatementsWithMarkedStatements(node, Statements)
                End If

            End Function

            Public Overrides Function VisitGotoStatement(node As CSS.GotoStatementSyntax) As SyntaxList(Of VBS.StatementSyntax)
                Dim label As VBS.LabelSyntax
                If node.IsKind(CS.SyntaxKind.GotoCaseStatement, CS.SyntaxKind.GotoDefaultStatement) Then
                    If Me.mBlockInfo.Count = 0 Then Throw New InvalidOperationException("GoTo Case/GoTo Default outside switch Is illegal!")
                    Dim labelExpression As VB.VisualBasicSyntaxNode = If(node.Expression?.Accept(Me.mNodesVisitor), VBFactory.ElseCaseClause())
                    Me.mBlockInfo.Peek().GotoCaseExpressions.Add(labelExpression)
                    label = VBFactory.Label(VB.SyntaxKind.IdentifierLabel, Me.MakeGotoSwitchLabel(labelExpression))
                Else
                    label = VBFactory.Label(VB.SyntaxKind.IdentifierLabel, GenerateSafeVBToken((DirectCast(node.Expression, CSS.IdentifierNameSyntax)).Identifier, IsQualifiedName:=False, IsTypeName:=False))
                End If

                Return VBFactory.SingletonList(Of VBS.StatementSyntax)(VBFactory.GoToStatement(label).WithConvertedTriviaFrom(node).WithTrailingEOL)
            End Function

            Public Overrides Function VisitIfStatement(node As CSS.IfStatementSyntax) As SyntaxList(Of VBS.StatementSyntax)
                Dim name As VBS.IdentifierNameSyntax = Nothing
                Dim arguments As New List(Of VBS.ArgumentSyntax)()
                Dim stmt As VBS.StatementSyntax
                If node.[Else] Is Nothing AndAlso Me.TryConvertRaiseEvent(node, name, arguments) Then
                    stmt = VBFactory.RaiseEventStatement(name, VBFactory.ArgumentList(VBFactory.SeparatedList(arguments)))
                    Return VBFactory.SingletonList(stmt)
                End If

                Dim ListOfElseIfBlocks As New List(Of VBS.ElseIfBlockSyntax)()
                Dim ElseBlock As VBS.ElseBlockSyntax = Nothing
                Dim OpenBraceTrailingTrivia As New List(Of SyntaxTrivia)
                Dim ClosingBraceLeadingTrivia As New List(Of SyntaxTrivia)
                Me.CollectElseBlocks(node, ListOfElseIfBlocks, ElseBlock, OpenBraceTrailingTrivia, ClosingBraceLeadingTrivia)

                Dim StatementTrailingTrivia As New List(Of SyntaxTrivia)

                Dim OpenParenToken As SyntaxToken = node.OpenParenToken

                Dim IfKeywordWithTrivia As SyntaxToken = IfKeyword.
                                            WithConvertedTriviaFrom(node.IfKeyword).
                                            WithAppendedTrailingTrivia(ConvertTrivia(OpenParenToken.LeadingTrivia)).
                                            WithAppendedTrailingTrivia(ConvertTrivia(OpenParenToken.TrailingTrivia))
                IfKeywordWithTrivia = IfKeywordWithTrivia.WithModifiedTokenTrivia(LeadingToken:=True, AfterEOL:=False)

                StatementTrailingTrivia.AddRange(ConvertTrivia(node.CloseParenToken.LeadingTrivia))
                StatementTrailingTrivia.AddRange(ConvertTrivia(node.CloseParenToken.TrailingTrivia))
                If StatementTrailingTrivia.Count > 0 AndAlso Not StatementTrailingTrivia(0).IsEndOfLine Then
                    StatementTrailingTrivia.Insert(0, VB_EOLTrivia)
                End If
                Dim ConditionWithTrivia As VBS.ExpressionSyntax = DirectCast(node.Condition.Accept(Me.mNodesVisitor).WithAppendedTrailingTrivia(ConvertTrivia(node.Condition.GetTrailingTrivia)).WithModifiedNodeTrivia(SeparatorFollows:=True), VBS.ExpressionSyntax)

                If node.Statement.IsKind(CS.SyntaxKind.EmptyStatement) Then
                    StatementTrailingTrivia.InsertRange(0, ConvertTrivia(DirectCast(node.Statement, CSS.EmptyStatementSyntax).SemicolonToken.TrailingTrivia))
                End If
                Dim IfStatement As VBS.IfStatementSyntax = VBFactory.IfStatement(
                                                                                IfKeywordWithTrivia,
                                                                                ConditionWithTrivia,
                                                                                ThenKeyword
                                                                                ).
                                                                                WithTrailingTrivia(StatementTrailingTrivia).WithTrailingEOL

                Dim Braces As (SyntaxToken, SyntaxToken) = node.Statement.GetBraces
                Dim OpenBraces As SyntaxToken = Braces.Item1
                Dim CloseBraces As SyntaxToken = Braces.Item2
                Dim EndIfStatement As VBS.EndBlockStatementSyntax = VBFactory.EndIfStatement(
                                                                                EndKeyword,
                                                                                IfKeyword
                                                                                ).
                                                                                WithConvertedTriviaFrom(CloseBraces)
                Dim ElseIfBlocks As SyntaxList(Of VBS.ElseIfBlockSyntax) = VBFactory.List(ListOfElseIfBlocks)
                If ElseBlock IsNot Nothing AndAlso ElseBlock.Statements(0).IsKind(VB.SyntaxKind.EmptyStatement) Then
                    EndIfStatement = EndIfStatement.WithLeadingTrivia(ElseBlock.GetTrailingTrivia)
                    ElseBlock = VBFactory.ElseBlock(VBFactory.ElseStatement(), statements:=Nothing)
                End If
                Dim Statements As SyntaxList(Of VBS.StatementSyntax) = Me.ConvertBlock(node.Statement, OpenBraceTrailingTrivia, ClosingBraceLeadingTrivia)
                If ClosingBraceLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    EndIfStatement = EndIfStatement.WithLeadingTrivia(ClosingBraceLeadingTrivia)
                End If
                If TypeOf node.Statement Is CSS.BlockSyntax Then
                    stmt = VBFactory.MultiLineIfBlock(IfStatement,
                                                          Statements,
                                                          ElseIfBlocks,
                                                          ElseBlock,
                                                          EndIfStatement
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
                                                                EndIfStatement
                                                             )
                    Else
                        If IfStatement.GetTrailingTrivia.ContainsEOLTrivia Then
                            Dim IFBlockStatements As SyntaxList(Of VBS.StatementSyntax) = Statements
                            stmt = VBFactory.MultiLineIfBlock(IfStatement,
                                                                   statements:=IFBlockStatements,
                                                                   ElseIfBlocks,
                                                                   ElseBlock,
                                                                   EndIfStatement
                                                                   )
                        Else
                            If ElseBlock IsNot Nothing OrElse (Statements.Count = 1 AndAlso TypeOf Statements(0) Is VBS.EmptyStatementSyntax) Then
                                stmt = VBFactory.MultiLineIfBlock(IfStatement,
                                                                     Statements,
                                                                     elseIfBlocks:=Nothing,
                                                                     ElseBlock,
                                                                     EndIfStatement)
                            Else
                                stmt = VBFactory.SingleLineIfStatement(IfKeywordWithTrivia, ConditionWithTrivia, ThenKeyword,
                                                                           Statements,
                                                                           elseClause:=Nothing)
                            End If
                        End If
                    End If
                End If

                Return ReplaceStatementsWithMarkedStatements(node, VBFactory.SingletonList(stmt))
            End Function

            Public Overrides Function VisitLabeledStatement(node As CSS.LabeledStatementSyntax) As SyntaxList(Of VBS.StatementSyntax)
                Dim OpenBraceTrailingTrivia As New List(Of SyntaxTrivia)
                Dim ClosingBraceLeadingTrivia As New List(Of SyntaxTrivia)
                Dim Statements As SyntaxList(Of VBS.StatementSyntax) = Me.ConvertBlock(node.Statement, OpenBraceTrailingTrivia, ClosingBraceLeadingTrivia)
                If OpenBraceTrailingTrivia.Count > 0 OrElse ClosingBraceLeadingTrivia.Count > 0 Then
                    'Stop
                End If
                Return VBFactory.SingletonList(Of VBS.StatementSyntax)(VBFactory.LabelStatement(GenerateSafeVBToken(node.Identifier, IsQualifiedName:=False, IsTypeName:=False))).AddRange(Statements)
            End Function

            Public Overrides Function VisitLocalDeclarationStatement(node As CSS.LocalDeclarationStatementSyntax) As SyntaxList(Of VBS.StatementSyntax)
                Dim modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, Me.mNodesVisitor.IsModule, TokenContext.Local)
                If modifiers.Count = 0 Then
                    modifiers.Add(DimKeyword)
                End If
                Dim LeadingTrivia As New List(Of SyntaxTrivia)
                Dim declarators As SeparatedSyntaxList(Of VBS.VariableDeclaratorSyntax) = RemodelVariableDeclaration(node.Declaration, Me.mNodesVisitor, Me.mSemanticModel, IsFieldDeclaration:=False, LeadingTrivia)
                Dim localDeclarationStatement As VBS.LocalDeclarationStatementSyntax = VBFactory.LocalDeclarationStatement(
                                                        VBFactory.TokenList(modifiers),
                                                        declarators).
                                                    WithLeadingTrivia(LeadingTrivia).
                                                    WithAppendedTrailingTrivia(ConvertTrivia(node.SemicolonToken.TrailingTrivia)).WithTrailingEOL ' this picks up end of line comments
                ' Don't repeat leading comments
                If Not localDeclarationStatement.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    localDeclarationStatement = localDeclarationStatement.WithConvertedLeadingTriviaFrom(node)
                End If

                Dim StmtList As New List(Of VBS.StatementSyntax) From {
                    localDeclarationStatement
                }
                If node.SemicolonToken.HasLeadingTrivia And node.SemicolonToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    StmtList.Add(VBFactory.EmptyStatement.WithConvertedLeadingTriviaFrom(node.SemicolonToken))
                End If

                Return ReplaceStatementsWithMarkedStatements(node, VBFactory.List(StmtList))
            End Function

            Public Overrides Function VisitLocalFunctionStatement(node As CSS.LocalFunctionStatementSyntax) As SyntaxList(Of VBS.StatementSyntax)
                Dim syntaxList1 As SyntaxList(Of VBS.StatementSyntax) = VBFactory.SingletonList(Of VBS.StatementSyntax)(VBFactory.EmptyStatement.WithLeadingTrivia(node.CheckCorrectnessLeadingTrivia("Local Functions are not support by VB")).WithPrependedLeadingTrivia(ConvertTrivia(node.GetLeadingTrivia)).WithConvertedTrailingTriviaFrom(node))
                Return syntaxList1
            End Function

            Public Overrides Function VisitLockStatement(node As CSS.LockStatementSyntax) As SyntaxList(Of VBS.StatementSyntax)
                Dim LockStatement As VBS.SyncLockStatementSyntax = VBFactory.SyncLockStatement(DirectCast(node.Expression?.Accept(Me.mNodesVisitor), VBS.ExpressionSyntax)).
                                                                                            WithConvertedLeadingTriviaFrom(node)
                Dim OpenBrace As SyntaxToken = node.GetBraces.Item1
                Dim OpenBraceTrivia As New List(Of SyntaxTrivia)
                OpenBraceTrivia.AddRange(ConvertTrivia(OpenBrace.LeadingTrivia))
                OpenBraceTrivia.AddRange(ConvertTrivia(OpenBrace.TrailingTrivia))
                Dim ClosingBrace As SyntaxToken = node.GetBraces.Item2
                Dim ClosingBraceTrivia As New List(Of SyntaxTrivia)
                ClosingBraceTrivia.AddRange(ConvertTrivia(ClosingBrace.LeadingTrivia))
                ClosingBraceTrivia.AddRange(ConvertTrivia(ClosingBrace.TrailingTrivia))

                Dim Statements As SyntaxList(Of VBS.StatementSyntax) = Me.ConvertBlock(node.Statement, OpenBraceTrivia, ClosingBraceTrivia)
                Dim EndSyncLockStatement As VBS.EndBlockStatementSyntax = VBFactory.EndSyncLockStatement.WithLeadingTrivia(ClosingBraceTrivia).WithAppendedTrailingTrivia(ConvertTrivia(node.GetTrailingTrivia))
                Dim LockBlock As VBS.SyncLockBlockSyntax = VBFactory.SyncLockBlock(LockStatement.WithTrailingEOL, Statements, EndSyncLockStatement)
                Return ReplaceStatementsWithMarkedStatements(node, VBFactory.SingletonList(Of VBS.StatementSyntax)(LockBlock))
            End Function

            Public Overrides Function VisitReturnStatement(node As CSS.ReturnStatementSyntax) As SyntaxList(Of VBS.StatementSyntax)
                Dim stmt As VBS.StatementSyntax
                Dim MovedLeadingTrivia As New List(Of SyntaxTrivia)
                Dim Expression As VBS.ExpressionSyntax
                If node.Expression Is Nothing Then
                    stmt = VBFactory.ReturnStatement.
                                        WithConvertedTriviaFrom(node).
                                        WithTrailingEOL
                Else
                    Expression = DirectCast(node.Expression.Accept(Me.mNodesVisitor), VBS.ExpressionSyntax)
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
                Return ReplaceStatementsWithMarkedStatements(node, VBFactory.SingletonList(stmt))
            End Function

            Public Overrides Function VisitSwitchStatement(node As CSS.SwitchStatementSyntax) As SyntaxList(Of VBS.StatementSyntax)
                Dim stmt As VBS.StatementSyntax
                Me.mBlockInfo.Push(New BlockInfo())
                Try
                    Dim blocks As List(Of VBS.CaseBlockSyntax) = node.Sections.Select(AddressOf Me.ConvertSwitchSection).ToList
                    Dim OrderedBlocks As New List(Of VBS.CaseBlockSyntax)
                    Dim CaseElseIndex As Integer = -1
                    For i As Integer = 0 To blocks.Count - 1
                        If blocks(i).IsKind(VB.SyntaxKind.CaseElseBlock) Then
                            CaseElseIndex = i
                        Else
                            OrderedBlocks.Add(blocks(i))
                        End If
                    Next
                    If CaseElseIndex >= 0 Then
                        OrderedBlocks.Add(blocks(CaseElseIndex))
                    End If
                    Dim Expression As VBS.ExpressionSyntax = Nothing
                    If TypeOf node.Sections(0).Labels(0) Is CSS.CasePatternSwitchLabelSyntax Then
                        Dim PatternSwitch As CSS.CasePatternSwitchLabelSyntax = DirectCast(node.Sections(0).Labels(0), CSS.CasePatternSwitchLabelSyntax)
                        If TypeOf PatternSwitch.Pattern Is CSS.DeclarationPatternSyntax Then
                            Expression = VBFactory.TrueLiteralExpression(TrueKeyword)
                        ElseIf TypeOf PatternSwitch.Pattern Is CSS.ConstantPatternSyntax Then
                            ' TODO Handle
                        ElseIf TypeOf PatternSwitch.Pattern Is CSS.RecursivePatternSyntax Then
                            ' TODO Handle
                        Else
                            Stop
                        End If
                    End If
                    Dim EndSelectStatement As VBS.EndBlockStatementSyntax = VBFactory.EndBlockStatement(
                                                                    VB.SyntaxKind.EndSelectStatement,
                                                                    EndKeyword,
                                                                    SelectKeyword).
                                                                        WithConvertedTriviaFrom(node.CloseBraceToken)
                    stmt = VBFactory.SelectBlock(
                                VBFactory.SelectStatement(SelectKeyword,
                                                            CaseKeyword,
                                                            If(Expression, DirectCast(node.Expression.Accept(Me.mNodesVisitor), VBS.ExpressionSyntax))
                                                            ).WithTrailingEOL,
                                VBFactory.List(nodes:=Me.AddLabels(blocks:=OrderedBlocks.ToArray,
                                gotoLabels:=Me.mBlockInfo.Peek().GotoCaseExpressions)),
                                EndSelectStatement
                                )
                    Me.switchCount += 1
                Finally
                    Me.mBlockInfo.Pop()
                End Try
                Return ReplaceStatementsWithMarkedStatements(node, VBFactory.SingletonList(stmt.WithConvertedLeadingTriviaFrom(node)))
            End Function

            Public Overrides Function VisitThrowStatement(node As CSS.ThrowStatementSyntax) As SyntaxList(Of VBS.StatementSyntax)
                Dim stmt As VBS.StatementSyntax = If(node.Expression Is Nothing,
                            VBFactory.ThrowStatement().WithTrailingEOL,
                            VBFactory.ThrowStatement(DirectCast(node.Expression.Accept(Me.mNodesVisitor), VBS.ExpressionSyntax)).WithTrailingEOL)
                Return ReplaceStatementsWithMarkedStatements(node, VBFactory.SingletonList(stmt.WithConvertedTriviaFrom(node)))
            End Function

            Public Overrides Function VisitTryStatement(node As CSS.TryStatementSyntax) As SyntaxList(Of VBS.StatementSyntax)
                Dim OpenBraceTrailingTrivia As New List(Of SyntaxTrivia)
                Dim ClosingBraceLeadingTrivia As New List(Of SyntaxTrivia)
                Dim TryStatement As VBS.TryStatementSyntax = VBFactory.TryStatement()
                Dim CatchBlocks As SyntaxList(Of VBS.CatchBlockSyntax) = VBFactory.List(node.Catches.IndexedSelect(AddressOf Me.ConvertCatchClause))
                Dim TriviaList As New List(Of SyntaxTrivia)
                Dim LastCatchBlockIndex As Integer = CatchBlocks.Count - 1
                For i As Integer = 0 To LastCatchBlockIndex
                    Dim CatchBlock As VBS.CatchBlockSyntax = CatchBlocks(i)
                    If CatchBlock.Statements(0).IsKind(VB.SyntaxKind.EmptyStatement) Then
                        Dim TempTriviaList As New List(Of SyntaxTrivia)
                        TempTriviaList.AddRange(CatchBlock.Statements(0).GetTrailingTrivia)
                        TriviaList.AddRange(CatchBlock.GetBraces().Item2.LeadingTrivia)
                        CatchBlocks.Replace(CatchBlocks(i), VBFactory.CatchBlock(CatchBlock.CatchStatement.WithLeadingTrivia(TriviaList)))
                        TriviaList = TempTriviaList
                    Else
                        CatchBlocks = CatchBlocks.Replace(CatchBlock, CatchBlock.WithLeadingTrivia(TriviaList))
                        TriviaList.Clear()
                    End If
                Next
                If LastCatchBlockIndex >= 0 Then
                    CatchBlocks = CatchBlocks.Replace(CatchBlocks(0), CatchBlocks(0).WithConvertedTriviaFrom(node.Block.CloseBraceToken))
                End If
                Dim FinallyBlock As VBS.FinallyBlockSyntax = Nothing
                If node.Finally IsNot Nothing Then
                    Dim FinallyStatements As SyntaxList(Of VBS.StatementSyntax) = Me.ConvertBlock(node.[Finally].Block, OpenBraceTrailingTrivia, ClosingBraceLeadingTrivia)
                    'If OpenBraceTrailingTrivia.Count > 0 OrElse ClosingBraceLeadingTrivia.Count > 0 Then
                    '    Stop
                    'End If
                    FinallyBlock = VBFactory.FinallyBlock(FinallyStatements).WithPrependedLeadingTrivia(TriviaList)
                    TriviaList.Clear()
                    If FinallyBlock.Statements(0).IsKind(VB.SyntaxKind.EmptyStatement) Then
                        TriviaList.AddRange(FinallyBlock.Statements(0).GetTrailingTrivia)
                        FinallyBlock = FinallyBlock.WithTrailingTrivia(VB_EOLTrivia)
                    End If
                End If
                Dim EndTryStatement As VBS.EndBlockStatementSyntax = VBFactory.EndTryStatement()
                If node.Catches.Count > 0 Then
                    EndTryStatement = EndTryStatement.WithConvertedTriviaFrom(node.Catches.Last.Block.GetBraces.Item2)
                Else
                    EndTryStatement = EndTryStatement.WithLeadingTrivia(ClosingBraceLeadingTrivia)
                End If
                If TriviaList.Any Then
                    EndTryStatement = EndTryStatement.WithLeadingTrivia(TriviaList)
                End If
                Dim TryBlockStatements As SyntaxList(Of VBS.StatementSyntax) = Me.ConvertBlock(node.Block, OpenBraceTrailingTrivia, ClosingBraceLeadingTrivia)
                If ClosingBraceLeadingTrivia.Count > 0 Then
                    If FinallyBlock IsNot Nothing Then
                        FinallyBlock = FinallyBlock.WithPrependedLeadingTrivia(ClosingBraceLeadingTrivia)
                    End If
                End If
                Dim block As VBS.TryBlockSyntax = VBFactory.TryBlock(TryStatement,
                                                                    TryBlockStatements,
                                                                    CatchBlocks,
                                                                    FinallyBlock,
                                                                    EndTryStatement
                                                                    )
                Return VBFactory.SingletonList(Of VBS.StatementSyntax)(block.WithConvertedTriviaFrom(node))
            End Function

            Public Overrides Function VisitUnsafeStatement(node As CSS.UnsafeStatementSyntax) As SyntaxList(Of VBS.StatementSyntax)
                Return VBFactory.SingletonList(Of VBS.StatementSyntax)(FlagUnsupportedStatements(node, "Unsafe statement", CommentOutOriginalStatements:=True))
            End Function

            Public Overrides Function VisitUsingStatement(node As CSS.UsingStatementSyntax) As SyntaxList(Of VBS.StatementSyntax)
                Dim UsingStatement As VBS.UsingStatementSyntax
                Dim OpenBraceTrailingTrivia As New List(Of SyntaxTrivia)
                Dim ClosingBraceLeadingTrivia As New List(Of SyntaxTrivia)
                Dim Stmt As SyntaxList(Of VBS.StatementSyntax)
                Dim LeadingTrivia As New List(Of SyntaxTrivia)
                If node.Declaration Is Nothing Then
                    Dim UsingBlock As VBS.UsingBlockSyntax
                    LeadingTrivia.AddRange(ConvertTrivia(node.GetLeadingTrivia))
                    If node.Expression IsNot Nothing AndAlso node.Expression.IsKind(CS.SyntaxKind.ConditionalAccessExpression) Then
                        Dim CS_ConditionalAccessExpression As CSS.ConditionalAccessExpressionSyntax = DirectCast(node.Expression, CSS.ConditionalAccessExpressionSyntax)
                        Dim VB_ConditionalAccessExpression As VB.VisualBasicSyntaxNode = CS_ConditionalAccessExpression.Expression.Accept(Me.mNodesVisitor)
                        Dim Condition As VBS.BinaryExpressionSyntax = VBFactory.IsNotExpression(left:=CType(VB_ConditionalAccessExpression, VBS.ExpressionSyntax),
                                                                                                right:=NothingExpression)
                        Dim IfStatement As VBS.IfStatementSyntax = VBFactory.IfStatement(Condition)
                        UsingStatement = VBFactory.UsingStatement(VBFactory.ParseExpression($"{VB_ConditionalAccessExpression.ToString}{CS_ConditionalAccessExpression.WhenNotNull.Accept(Me.mNodesVisitor)}"), VBFactory.SeparatedList(Of VBS.VariableDeclaratorSyntax)())
                        UsingBlock = VBFactory.UsingBlock(UsingStatement.WithTrailingEOL, Me.ConvertBlock(node.Statement, OpenBraceTrailingTrivia, ClosingBraceLeadingTrivia)).WithLeadingTrivia(LeadingTrivia)
                        Dim IfStatementBlock As VBS.MultiLineIfBlockSyntax = VBFactory.MultiLineIfBlock(IfStatement, VBFactory.SingletonList(Of VBS.StatementSyntax)(UsingBlock), elseIfBlocks:=Nothing, elseBlock:=Nothing).WithLeadingTrivia(LeadingTrivia)
                        Stmt = VBFactory.SingletonList(Of VBS.StatementSyntax)(IfStatementBlock)
                        Return ReplaceStatementsWithMarkedStatements(node, Stmt)
                    Else
                        UsingStatement = VBFactory.UsingStatement(DirectCast(node.Expression?.Accept(Me.mNodesVisitor), VBS.ExpressionSyntax), VBFactory.SeparatedList(Of VBS.VariableDeclaratorSyntax)())
                    End If
                Else
                    UsingStatement = VBFactory.UsingStatement(expression:=Nothing, RemodelVariableDeclaration(node.Declaration, Me.mNodesVisitor, Me.mSemanticModel, IsFieldDeclaration:=False, LeadingTrivia))
                End If

                Dim EndUsing As VBS.EndBlockStatementSyntax = VBFactory.EndUsingStatement.WithConvertedTriviaFrom(node.Statement.GetBraces.Item2)
                Stmt = VBFactory.SingletonList(Of VBS.StatementSyntax)(VBFactory.UsingBlock(UsingStatement.WithTrailingEOL,
                                                                                            statements:=Me.ConvertBlock(node.Statement, OpenBraceTrailingTrivia, ClosingBraceLeadingTrivia),
                                                                                            EndUsing
                                                                                            ).WithLeadingTrivia(LeadingTrivia))
                Return ReplaceStatementsWithMarkedStatements(node, Stmt)
            End Function

            Public Overrides Function VisitVariableDeclaration(node As CSS.VariableDeclarationSyntax) As SyntaxList(Of VBS.StatementSyntax)
                Dim LeadingTrivia As New List(Of SyntaxTrivia)
                Dim Type As VBS.TypeSyntax = DirectCast(node.Type.Accept(Me.mNodesVisitor), VBS.TypeSyntax)
                If Type.HasLeadingTrivia Then
                    LeadingTrivia.AddRange(Type.GetLeadingTrivia)
                    Type = Type.WithLeadingTrivia(VBFactory.Whitespace(" "))
                End If
                Dim CollectedCommentTrivia As New List(Of SyntaxTrivia)
                Dim declaratorsWithoutInitializers As New List(Of CSS.VariableDeclaratorSyntax)()
                Dim Declarators As New List(Of VBS.VariableDeclaratorSyntax)
                For i As Integer = 0 To node.Variables.Count - 1
                    Dim v As CSS.VariableDeclaratorSyntax = node.Variables(i)
                    If v.Initializer Is Nothing Then
                        declaratorsWithoutInitializers.Add(v.WithTrailingTrivia(CollectedCommentTrivia))
                        Continue For
                    Else
                        Dim AsClause As VBS.SimpleAsClauseSyntax = If(node.Type.IsVar OrElse node.Type.IsKind(CS.SyntaxKind.RefType), Nothing, VBFactory.SimpleAsClause(Type))
                        Dim Value As VBS.ExpressionSyntax = DirectCast(v.Initializer.Value.Accept(Me.mNodesVisitor), VBS.ExpressionSyntax)
                        If Value Is Nothing Then
                            Value = VBFactory.IdentifierName("HandleRefExpression").WithConvertedTriviaFrom(v.Initializer.Value)
                        End If
                        If Value.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                            LeadingTrivia.AddRange(Value.GetLeadingTrivia)
                        End If
                        Dim Initializer As VBS.EqualsValueSyntax = VBFactory.EqualsValue(Value.WithLeadingTrivia(SpaceTrivia))
                        ' Get the names last to lead with var jsonWriter = new JsonWriter(stringWriter)
                        ' Which should be Dim jsonWriter_Renamed = new JsonWriter(stringWriter)
                        Dim Names As SeparatedSyntaxList(Of VBS.ModifiedIdentifierSyntax) = VBFactory.SingletonSeparatedList(CType(v.Accept(Me.mNodesVisitor), VBS.ModifiedIdentifierSyntax))
                        Dim Declator As VBS.VariableDeclaratorSyntax = VBFactory.VariableDeclarator(
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
                                        CollectedCommentTrivia.Add(t)
                                    Case VB.SyntaxKind.WhitespaceTrivia
                                        CollectedCommentTrivia.Add(t)
                                        NonCommentTrailingTrivia.Add(t)
                                    Case Else
                                        ' Directives are ignored but the results are converted. Disabled Text is deleted
                                        'Stop
                                End Select
                            Next
                            If FoundEOL Then
                                CollectedCommentTrivia.Add(VB_EOLTrivia)
                                Declator = Declator.WithTrailingTrivia(CollectedCommentTrivia)
                                CollectedCommentTrivia.Clear()
                            Else
                                Declator = Declator.WithTrailingTrivia(NonCommentTrailingTrivia)
                            End If
                            If i = node.Variables.Count - 1 Then
                                If Not Declator.HasTrailingTrivia OrElse Not Declator.GetTrailingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                    Declator = Declator.WithAppendedTrailingTrivia(VB_EOLTrivia)
                                End If
                            End If
                        End If
                        Declarators.Add(Declator)
                    End If
                Next
                If declaratorsWithoutInitializers.Count > 0 Then
                    Stop
                End If
                Dim LocalDeclarationStatement As VBS.LocalDeclarationStatementSyntax = VBFactory.LocalDeclarationStatement(
                    VBFactory.TokenList(DimModifier),
                    VBFactory.SeparatedList(Declarators)).WithoutTrivia.WithLeadingTrivia(LeadingTrivia).WithAppendedTrailingTrivia(ConvertTrivia(node.GetTrailingTrivia)) ' this picks up end of line comments
                ' Don't repeat leading comments
                If Not LocalDeclarationStatement.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    LocalDeclarationStatement = LocalDeclarationStatement.WithConvertedLeadingTriviaFrom(node)
                End If
                Return VBFactory.SingletonList(Of VBS.StatementSyntax)(LocalDeclarationStatement)
            End Function

            Public Overrides Function VisitWhileStatement(node As CSS.WhileStatementSyntax) As SyntaxList(Of VBS.StatementSyntax)
                Dim condition As VBS.ExpressionSyntax = DirectCast(node.Condition.Accept(Me.mNodesVisitor), VBS.ExpressionSyntax)
                Dim OpenBraceTrailingTrivia As New List(Of SyntaxTrivia)
                Dim ClosingBraceLeadingTrivia As New List(Of SyntaxTrivia)
                Dim WhileStatements As SyntaxList(Of VBS.StatementSyntax) = Me.ConvertBlock(node.Statement, OpenBraceTrailingTrivia, ClosingBraceLeadingTrivia)
                If OpenBraceTrailingTrivia.Count > 0 Then
                    WhileStatements = WhileStatements.Replace(WhileStatements.First, WhileStatements.First.WithPrependedLeadingTrivia(OpenBraceTrailingTrivia))
                    OpenBraceTrailingTrivia.Clear()
                End If
                Dim EndWhileStatement As VBS.EndBlockStatementSyntax = VBFactory.EndWhileStatement().WithLeadingTrivia(ClosingBraceLeadingTrivia)
                Dim block As VBS.WhileBlockSyntax = VBFactory.WhileBlock(VBFactory.WhileStatement(condition).WithConvertedLeadingTriviaFrom(node.WhileKeyword).WithTrailingEOL, WhileStatements, EndWhileStatement)
                Dim WhileStatementBlock As SyntaxList(Of VBS.StatementSyntax) = VBFactory.SingletonList(Of VBS.StatementSyntax)(block)
                Return ReplaceStatementsWithMarkedStatements(node, WhileStatementBlock)
            End Function

            Public Overrides Function VisitYieldStatement(node As CSS.YieldStatementSyntax) As SyntaxList(Of VBS.StatementSyntax)
                Me.IsInterator = True
                Dim stmt As VBS.StatementSyntax
                If node.Expression Is Nothing Then
                    stmt = VBFactory.ReturnStatement.WithTrailingEOL
                Else
                    stmt = VBFactory.YieldStatement(DirectCast(node.Expression.Accept(Me.mNodesVisitor), VBS.ExpressionSyntax)).WithTrailingEOL
                End If
                Return ReplaceStatementsWithMarkedStatements(node, VBFactory.SingletonList(stmt.WithConvertedTriviaFrom(node)))
            End Function

            Private Class BlockInfo
                Public ReadOnly GotoCaseExpressions As List(Of VB.VisualBasicSyntaxNode) = New List(Of VB.VisualBasicSyntaxNode)()
            End Class

        End Class

    End Class

End Namespace