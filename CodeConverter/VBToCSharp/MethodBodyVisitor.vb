Option Explicit On
Option Infer Off
Option Strict On

Imports System
Imports System.Collections.Generic
Imports System.Linq
Imports System.Runtime.InteropServices
Imports IVisualBasicCode.CodeConverter.Util
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CSharp
Imports Microsoft.CodeAnalysis.CSharp.Syntax
Imports ArgumentListSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.ArgumentListSyntax
Imports CSharpExtensions = Microsoft.CodeAnalysis.CSharp.CSharpExtensions
Imports ExpressionSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.ExpressionSyntax
Imports IdentifierNameSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.IdentifierNameSyntax
Imports LocalDeclarationStatementSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.LocalDeclarationStatementSyntax
Imports StatementSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.StatementSyntax
Imports SyntaxFactory = Microsoft.CodeAnalysis.CSharp.SyntaxFactory
Imports SyntaxKind = Microsoft.CodeAnalysis.CSharp.SyntaxKind
Imports TypeSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.TypeSyntax
Imports VBasic = Microsoft.CodeAnalysis.VisualBasic
Imports VBSyntax = Microsoft.CodeAnalysis.VisualBasic.Syntax
Imports System.Runtime.CompilerServices

Namespace IVisualBasicCode.CodeConverter.CSharp

    Partial Public Class VisualBasicConverter

        Class MethodBodyVisitor
            Inherits VBasic.VisualBasicSyntaxVisitor(Of SyntaxList(Of StatementSyntax))

            Private mSemanticModel As SemanticModel

            Private mNodesVisitor As NodesVisitor

            Private ReadOnly mWithBlockTempVariableNames As Stack(Of String)

            Public Property IsIterator As Boolean

            Public Sub New(ByVal semanticModel As SemanticModel, ByVal lNodesVisitor As NodesVisitor, ByVal withBlockTempVariableNames As Stack(Of String))
                Me.mSemanticModel = semanticModel
                Me.mNodesVisitor = lNodesVisitor
                Me.mWithBlockTempVariableNames = withBlockTempVariableNames
            End Sub

            Public Overrides Function DefaultVisit(ByVal node As SyntaxNode) As SyntaxList(Of StatementSyntax)
                Throw New NotImplementedException($"{node.[GetType]().FullName} not implemented!")
            End Function

            Public Overrides Function VisitStopOrEndStatement(ByVal node As VBSyntax.StopOrEndStatementSyntax) As SyntaxList(Of StatementSyntax)
                Return SingleStatement(SyntaxFactory.ParseStatement(GetCSharpEquivalentStatementText(node)))
            End Function

            Private Shared Function GetCSharpEquivalentStatementText(ByVal node As VBSyntax.StopOrEndStatementSyntax) As String
                Select Case VBasic.VisualBasicExtensions.Kind(node.StopOrEndKeyword)
                    Case VBasic.SyntaxKind.StopKeyword
                        Return "System.Diagnostics.Debugger.Break();"
                    Case VBasic.SyntaxKind.EndKeyword
                        Return "System.Environment.Exit(0);"
                    Case Else
                        Throw New NotImplementedException(CSharpExtensions.Kind(node.StopOrEndKeyword) & " not implemented!")
                End Select
            End Function

            Public Overrides Function VisitLocalDeclarationStatement(ByVal node As VBSyntax.LocalDeclarationStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim modifiers As SyntaxTokenList = ConvertModifiers(node.Modifiers, TokenContext.Local)
                Dim declarations As List(Of LocalDeclarationStatementSyntax) = New List(Of LocalDeclarationStatementSyntax)()
                For Each declarator As VBSyntax.VariableDeclaratorSyntax In node.Declarators
                    For Each decl As KeyValuePair(Of String, VariableDeclarationSyntax) In SplitVariableDeclarations(declarator, mNodesVisitor, mSemanticModel)
                        declarations.Add(SyntaxFactory.LocalDeclarationStatement(modifiers, decl.Value))
                    Next
                Next

                Return SyntaxFactory.List(Of StatementSyntax)(declarations)
            End Function

            Public Overrides Function VisitAddRemoveHandlerStatement(ByVal node As VBSyntax.AddRemoveHandlerStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim lSyntaxKind As SyntaxKind = If(node.Kind() = VBasic.SyntaxKind.AddHandlerStatement, SyntaxKind.AddAssignmentExpression, SyntaxKind.SubtractAssignmentExpression)
                Return SingleStatement(SyntaxFactory.AssignmentExpression(lSyntaxKind,
                                                                          CType(node.EventExpression.Accept(mNodesVisitor), ExpressionSyntax),
                                                                          CType(node.DelegateExpression.Accept(mNodesVisitor), ExpressionSyntax)))
            End Function

            Public Overrides Function VisitExpressionStatement(ByVal node As VBSyntax.ExpressionStatementSyntax) As SyntaxList(Of StatementSyntax)
                Return SingleStatement(CType(node.Expression.Accept(mNodesVisitor), ExpressionSyntax))
            End Function

            Public Overrides Function VisitAssignmentStatement(ByVal node As VBSyntax.AssignmentStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim kind As SyntaxKind = ConvertToken(node.Kind(), TokenContext.Local)
                Return SingleStatement(SyntaxFactory.AssignmentExpression(kind, CType(node.Left.Accept(mNodesVisitor), ExpressionSyntax), CType(node.Right.Accept(mNodesVisitor), ExpressionSyntax)))
            End Function

            Public Overrides Function VisitThrowStatement(ByVal node As VBSyntax.ThrowStatementSyntax) As SyntaxList(Of StatementSyntax)
                Return SingleStatement(SyntaxFactory.ThrowStatement(CType(node.Expression?.Accept(mNodesVisitor), ExpressionSyntax)))
            End Function

            Public Overrides Function VisitReturnStatement(ByVal node As VBSyntax.ReturnStatementSyntax) As SyntaxList(Of StatementSyntax)
                If IsIterator Then Return SingleStatement(SyntaxFactory.YieldStatement(SyntaxKind.YieldBreakStatement))
                Return SingleStatement(SyntaxFactory.ReturnStatement(CType(node.Expression?.Accept(mNodesVisitor), ExpressionSyntax)))
            End Function

            Public Overrides Function VisitContinueStatement(ByVal node As VBSyntax.ContinueStatementSyntax) As SyntaxList(Of StatementSyntax)
                Return SingleStatement(SyntaxFactory.ContinueStatement())
            End Function

            Public Overrides Function VisitYieldStatement(ByVal node As VBSyntax.YieldStatementSyntax) As SyntaxList(Of StatementSyntax)
                Return SingleStatement(SyntaxFactory.YieldStatement(SyntaxKind.YieldReturnStatement, CType(node.Expression?.Accept(mNodesVisitor), ExpressionSyntax)))
            End Function

            Public Overrides Function VisitExitStatement(ByVal node As VBSyntax.ExitStatementSyntax) As SyntaxList(Of StatementSyntax)
                Select Case VBasic.VisualBasicExtensions.Kind(node.BlockKeyword)
                    Case VBasic.SyntaxKind.SubKeyword
                        Return SingleStatement(SyntaxFactory.ReturnStatement())
                    Case VBasic.SyntaxKind.FunctionKeyword
                        Dim typeContainer As VBasic.VisualBasicSyntaxNode = If(CType(node.Ancestors().OfType(Of VBSyntax.LambdaExpressionSyntax)().FirstOrDefault(), VBasic.VisualBasicSyntaxNode), node.Ancestors().OfType(Of VBSyntax.MethodBlockSyntax)().FirstOrDefault())
                        Dim info As ITypeSymbol = typeContainer.TypeSwitch(
                            Function(ByVal e As VBSyntax.LambdaExpressionSyntax) ModelExtensions.GetTypeInfo(mSemanticModel, e).Type.GetReturnType(),
                            Function(ByVal e As VBSyntax.MethodBlockSyntax)
                                Dim type As TypeSyntax = If(CType(e.SubOrFunctionStatement.AsClause?.Type.Accept(mNodesVisitor), TypeSyntax), SyntaxFactory.ParseTypeName("object"))
                                Return mSemanticModel.GetSymbolInfo(type).Symbol.GetReturnType()
                            End Function)
                        Dim expr As ExpressionSyntax
                        If info.IsReferenceType Then
                            expr = SyntaxFactory.LiteralExpression(SyntaxKind.NullLiteralExpression)
                        ElseIf info.CanBeReferencedByName Then
                            expr = SyntaxFactory.DefaultExpression(SyntaxFactory.ParseTypeName(info.ToMinimalDisplayString(mSemanticModel, node.SpanStart)))
                        Else
                            Throw New NotSupportedException()
                        End If

                        Return SingleStatement(SyntaxFactory.ReturnStatement(expr))
                    Case Else
                        Return SingleStatement(SyntaxFactory.BreakStatement())
                End Select
            End Function

            Public Overrides Function VisitRaiseEventStatement(ByVal node As VBSyntax.RaiseEventStatementSyntax) As SyntaxList(Of StatementSyntax)
                Return SingleStatement(SyntaxFactory.ConditionalAccessExpression(CType(node.Name.Accept(mNodesVisitor), ExpressionSyntax), SyntaxFactory.InvocationExpression(SyntaxFactory.MemberBindingExpression(SyntaxFactory.IdentifierName("Invoke")), CType(node.ArgumentList.Accept(mNodesVisitor), ArgumentListSyntax))))
            End Function

            Public Overrides Function VisitSingleLineIfStatement(ByVal node As VBSyntax.SingleLineIfStatementSyntax) As SyntaxList(Of StatementSyntax)
                Dim condition As ExpressionSyntax = CType(node.Condition.Accept(mNodesVisitor), ExpressionSyntax)
                Dim block As BlockSyntax = SyntaxFactory.Block(node.Statements.SelectMany(Function(s As VBSyntax.StatementSyntax) s.Accept(Me)))
                Dim elseClause As ElseClauseSyntax = Nothing
                If node.ElseClause IsNot Nothing Then
                    Dim elseBlock As BlockSyntax = SyntaxFactory.Block(node.ElseClause.Statements.SelectMany(Function(s As VBSyntax.StatementSyntax) s.Accept(Me)))
                    elseClause = SyntaxFactory.ElseClause(elseBlock.UnpackBlock())
                End If

                Return SingleStatement(SyntaxFactory.IfStatement(condition, block.UnpackBlock(), elseClause))
            End Function

            Public Overrides Function VisitMultiLineIfBlock(ByVal node As VBSyntax.MultiLineIfBlockSyntax) As SyntaxList(Of StatementSyntax)
                Dim condition As ExpressionSyntax = CType(node.IfStatement.Condition.Accept(mNodesVisitor), ExpressionSyntax)
                Dim block As BlockSyntax = SyntaxFactory.Block(node.Statements.SelectMany(Function(s As VBSyntax.StatementSyntax) s.Accept(Me)))
                Dim elseClause As ElseClauseSyntax = Nothing
                If node.ElseBlock IsNot Nothing Then
                    Dim elseBlock As BlockSyntax = SyntaxFactory.Block(node.ElseBlock.Statements.SelectMany(Function(s As VBSyntax.StatementSyntax) s.Accept(Me)))
                    elseClause = SyntaxFactory.ElseClause(elseBlock.UnpackBlock())
                End If

                For Each [elseIf] As VBSyntax.ElseIfBlockSyntax In node.ElseIfBlocks.Reverse()
                    Dim elseBlock As BlockSyntax = SyntaxFactory.Block([elseIf].Statements.SelectMany(Function(s As VBSyntax.StatementSyntax) s.Accept(Me)))
                    Dim ifStmt As IfStatementSyntax = SyntaxFactory.IfStatement(CType([elseIf].ElseIfStatement.Condition.Accept(mNodesVisitor), ExpressionSyntax), elseBlock.UnpackBlock(), elseClause)
                    elseClause = SyntaxFactory.ElseClause(ifStmt)
                Next

                Return SingleStatement(SyntaxFactory.IfStatement(condition, block.UnpackBlock(), elseClause))
            End Function

            Public Overrides Function VisitForBlock(ByVal node As VBSyntax.ForBlockSyntax) As SyntaxList(Of StatementSyntax)
                Dim stmt As VBSyntax.ForStatementSyntax = node.ForStatement
                Dim startValue As ExpressionSyntax = CType(stmt.FromValue.Accept(mNodesVisitor), ExpressionSyntax)
                Dim declaration As VariableDeclarationSyntax = Nothing
                Dim id As ExpressionSyntax
                If TypeOf stmt.ControlVariable Is VBSyntax.VariableDeclaratorSyntax Then
                    Dim v As VBSyntax.VariableDeclaratorSyntax = CType(stmt.ControlVariable, VBSyntax.VariableDeclaratorSyntax)
                    declaration = SplitVariableDeclarations(v, mNodesVisitor, mSemanticModel).Values.Single()
                    declaration = declaration.WithVariables(SyntaxFactory.SingletonSeparatedList(declaration.Variables(0).WithInitializer(SyntaxFactory.EqualsValueClause(startValue))))
                    id = SyntaxFactory.IdentifierName(declaration.Variables(0).Identifier)
                Else
                    id = CType(stmt.ControlVariable.Accept(mNodesVisitor), ExpressionSyntax)
                    Dim symbol As ISymbol = mSemanticModel.GetSymbolInfo(stmt.ControlVariable).Symbol
                    If Not mSemanticModel.LookupSymbols(node.FullSpan.Start, name:=symbol.Name).Any() Then
                        Dim variableDeclaratorSyntax As VariableDeclaratorSyntax = SyntaxFactory.VariableDeclarator(SyntaxFactory.Identifier(symbol.Name), Nothing, SyntaxFactory.EqualsValueClause(startValue))
                        declaration = SyntaxFactory.VariableDeclaration(SyntaxFactory.IdentifierName("var"), SyntaxFactory.SingletonSeparatedList(variableDeclaratorSyntax))
                    Else
                        startValue = SyntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression, id, startValue)
                    End If
                End If

                Dim [step] As ExpressionSyntax = CType(stmt.StepClause?.StepValue.Accept(mNodesVisitor), ExpressionSyntax)
                Dim value As PrefixUnaryExpressionSyntax = TryCast([step].SkipParens(), PrefixUnaryExpressionSyntax)
                Dim condition As ExpressionSyntax
                If value Is Nothing Then
                    condition = SyntaxFactory.BinaryExpression(SyntaxKind.LessThanOrEqualExpression, id, CType(stmt.ToValue.Accept(mNodesVisitor), ExpressionSyntax))
                Else
                    condition = SyntaxFactory.BinaryExpression(SyntaxKind.GreaterThanOrEqualExpression, id, CType(stmt.ToValue.Accept(mNodesVisitor), ExpressionSyntax))
                End If

                If [step] Is Nothing Then [step] = SyntaxFactory.PostfixUnaryExpression(SyntaxKind.PostIncrementExpression, id) Else [step] = SyntaxFactory.AssignmentExpression(SyntaxKind.AddAssignmentExpression, id, [step])
                Dim block As BlockSyntax = SyntaxFactory.Block(node.Statements.SelectMany(Function(s As VBSyntax.StatementSyntax) s.Accept(Me)))
                Return SingleStatement(SyntaxFactory.ForStatement(declaration, If(declaration IsNot Nothing, SyntaxFactory.SeparatedList(Of ExpressionSyntax)(), SyntaxFactory.SingletonSeparatedList(startValue)), condition, SyntaxFactory.SingletonSeparatedList([step]), block.UnpackBlock()))
            End Function

            Public Overrides Function VisitForEachBlock(ByVal node As VBSyntax.ForEachBlockSyntax) As SyntaxList(Of StatementSyntax)
                Dim stmt As VBSyntax.ForEachStatementSyntax = node.ForEachStatement
                Dim type As TypeSyntax = Nothing
                Dim id As SyntaxToken
                If TypeOf stmt.ControlVariable Is VBSyntax.VariableDeclaratorSyntax Then
                    Dim v As VBSyntax.VariableDeclaratorSyntax = CType(stmt.ControlVariable, VBSyntax.VariableDeclaratorSyntax)
                    Dim declaration As VariableDeclarationSyntax = SplitVariableDeclarations(v, mNodesVisitor, mSemanticModel).Values.Single()
                    type = declaration.Type
                    id = declaration.Variables(0).Identifier
                Else
                    Dim v As IdentifierNameSyntax = CType(stmt.ControlVariable.Accept(mNodesVisitor), IdentifierNameSyntax)
                    id = v.Identifier
                    type = SyntaxFactory.ParseTypeName("var")
                End If

                Dim block As BlockSyntax = SyntaxFactory.Block(node.Statements.SelectMany(Function(s As VBSyntax.StatementSyntax) s.Accept(Me)))
                Return SingleStatement(SyntaxFactory.ForEachStatement(
                                       type,
                                       id,
                                       CType(stmt.Expression.Accept(mNodesVisitor), ExpressionSyntax),
                                       block.UnpackBlock()))
            End Function

            Public Overrides Function VisitSelectBlock(ByVal node As VBSyntax.SelectBlockSyntax) As SyntaxList(Of StatementSyntax)
                Dim expr As ExpressionSyntax = CType(node.SelectStatement.Expression.Accept(mNodesVisitor), ExpressionSyntax)
                Dim switchStatement As SwitchStatementSyntax = Nothing
                If ConvertToSwitch(expr, node.CaseBlocks, switchStatement) Then Return SingleStatement(switchStatement)
                Throw New NotSupportedException()
            End Function
            Public Overrides Function VisitWithBlock(ByVal node As VBSyntax.WithBlockSyntax) As SyntaxList(Of StatementSyntax)
                Dim withExpression As ExpressionSyntax = CType(node.WithStatement.Expression.Accept(mNodesVisitor), ExpressionSyntax)
                mWithBlockTempVariableNames.Push(GetUniqueVariableNameInScope(node, "withBlock"))
                Try
                    Dim lVariableDeclaratorSyntax As VariableDeclaratorSyntax = SyntaxFactory.VariableDeclarator(
                        SyntaxFactory.Identifier(mWithBlockTempVariableNames.Peek()), Nothing,
                        SyntaxFactory.EqualsValueClause(withExpression))
                    Dim lDeclaration As StatementSyntax = SyntaxFactory.LocalDeclarationStatement(SyntaxFactory.VariableDeclaration(
                                                                                                                  SyntaxFactory.IdentifierName("var"),
                                                                                                                  SyntaxFactory.SingletonSeparatedList(lVariableDeclaratorSyntax)))
                    Dim lStatements As IEnumerable(Of StatementSyntax) = node.Statements.SelectMany(Function(s As VBSyntax.StatementSyntax) s.Accept(Me))
                    Return SingleStatement(SyntaxFactory.Block({lDeclaration}.Concat(lStatements).ToArray()))
                Finally
                    mWithBlockTempVariableNames.Pop()
                End Try
            End Function

            Private Function GetUniqueVariableNameInScope(ByVal node As SyntaxNode, ByVal variableNameBase As String) As String
                Dim reservedNames As IEnumerable(Of String) = mWithBlockTempVariableNames.Concat(node.DescendantNodesAndSelf().SelectMany(Function(lSyntaxNode As SyntaxNode) mSemanticModel.LookupSymbols(lSyntaxNode.SpanStart).[Select](Function(s As ISymbol) s.Name)))
                Return NameGenerator.EnsureUniqueness(variableNameBase, reservedNames, True)
            End Function

            Private Function ConvertToSwitch(ByVal expr As ExpressionSyntax, ByVal caseBlocks As SyntaxList(Of VBSyntax.CaseBlockSyntax), <Out> ByRef switchStatement As SwitchStatementSyntax) As Boolean
                switchStatement = Nothing
                Dim sections As List(Of SwitchSectionSyntax) = New List(Of SwitchSectionSyntax)()
                For Each block As VBSyntax.CaseBlockSyntax In caseBlocks
                    Dim labels As List(Of SwitchLabelSyntax) = New List(Of SwitchLabelSyntax)()
                    For Each c As VBSyntax.CaseClauseSyntax In block.CaseStatement.Cases
                        If TypeOf c Is VBSyntax.SimpleCaseClauseSyntax Then
                            Dim s As VBSyntax.SimpleCaseClauseSyntax = CType(c, VBSyntax.SimpleCaseClauseSyntax)
                            labels.Add(SyntaxFactory.CaseSwitchLabel(CType(s.Value.Accept(mNodesVisitor), ExpressionSyntax)))
                        ElseIf TypeOf c Is VBSyntax.ElseCaseClauseSyntax Then
                            labels.Add(SyntaxFactory.DefaultSwitchLabel())
                        Else
                            Return False
                        End If
                    Next

                    Dim list As SyntaxList(Of StatementSyntax) = SingleStatement(SyntaxFactory.Block(block.Statements.SelectMany(Function(s As VBSyntax.StatementSyntax) s.Accept(Me)).Concat(SyntaxFactory.BreakStatement())))
                    sections.Add(SyntaxFactory.SwitchSection(SyntaxFactory.List(labels), list))
                Next

                switchStatement = SyntaxFactory.SwitchStatement(expr, SyntaxFactory.List(sections))
                Return True
            End Function

            Public Overrides Function VisitTryBlock(ByVal node As VBSyntax.TryBlockSyntax) As SyntaxList(Of StatementSyntax)
                Dim block As BlockSyntax = SyntaxFactory.Block(node.Statements.SelectMany(Function(s As VBSyntax.StatementSyntax) s.Accept(Me)))
                Return SingleStatement(
                    SyntaxFactory.TryStatement(
                    block,
                    SyntaxFactory.List(node.CatchBlocks.[Select](Function(c As VBSyntax.CatchBlockSyntax) CType(c.Accept(mNodesVisitor), CatchClauseSyntax))),
                    CType(node.FinallyBlock?.Accept(mNodesVisitor), FinallyClauseSyntax)))
            End Function

            Public Overrides Function VisitSyncLockBlock(ByVal node As VBSyntax.SyncLockBlockSyntax) As SyntaxList(Of StatementSyntax)
                Return SingleStatement(
                    SyntaxFactory.LockStatement(
                    CType(node.SyncLockStatement.Expression.Accept(mNodesVisitor), ExpressionSyntax),
                    SyntaxFactory.Block(node.Statements.SelectMany(Function(s As VBSyntax.StatementSyntax) s.Accept(Me))).UnpackBlock()))
            End Function

            Public Overrides Function VisitUsingBlock(ByVal node As VBSyntax.UsingBlockSyntax) As SyntaxList(Of StatementSyntax)
                If node.UsingStatement.Expression Is Nothing Then
                    Dim stmt As StatementSyntax = SyntaxFactory.Block(node.Statements.SelectMany(Function(s As VBSyntax.StatementSyntax) s.Accept(Me)))
                    For Each v As VBSyntax.VariableDeclaratorSyntax In node.UsingStatement.Variables.Reverse()
                        For Each lDeclaration As VariableDeclarationSyntax In SplitVariableDeclarations(v, mNodesVisitor, mSemanticModel).Values.Reverse()
                            stmt = SyntaxFactory.UsingStatement(lDeclaration, Nothing, stmt)
                        Next
                    Next

                    Return SingleStatement(stmt)
                Else
                    Dim expr As ExpressionSyntax = CType(node.UsingStatement.Expression.Accept(mNodesVisitor), ExpressionSyntax)
                    Return SingleStatement(SyntaxFactory.UsingStatement(Nothing, expr, SyntaxFactory.Block(node.Statements.SelectMany(Function(s As VBSyntax.StatementSyntax) s.Accept(Me))).UnpackBlock()))
                End If
            End Function

            Public Overrides Function VisitWhileBlock(ByVal node As VBSyntax.WhileBlockSyntax) As SyntaxList(Of StatementSyntax)
                Return SingleStatement(SyntaxFactory.WhileStatement(
                                       CType(node.WhileStatement.Condition.Accept(mNodesVisitor), ExpressionSyntax),
                                       SyntaxFactory.Block(node.Statements.SelectMany(Function(s As VBSyntax.StatementSyntax) s.Accept(Me))).UnpackBlock()
                                       ))
            End Function

            Public Overrides Function VisitDoLoopBlock(ByVal node As VBSyntax.DoLoopBlockSyntax) As SyntaxList(Of StatementSyntax)
                If node.DoStatement.WhileOrUntilClause IsNot Nothing Then
                    Dim stmt As VBSyntax.WhileOrUntilClauseSyntax = node.DoStatement.WhileOrUntilClause
                    If SyntaxTokenExtensions.IsKind(stmt.WhileOrUntilKeyword, VBasic.SyntaxKind.WhileKeyword) Then
                        Return SingleStatement(SyntaxFactory.WhileStatement(
                                               CType(stmt.Condition.Accept(mNodesVisitor), ExpressionSyntax),
                                               SyntaxFactory.Block(node.Statements.SelectMany(Function(s As VBSyntax.StatementSyntax) s.Accept(Me))).UnpackBlock()))
                    Else
                        Return SingleStatement(SyntaxFactory.WhileStatement(
                                               SyntaxFactory.PrefixUnaryExpression(SyntaxKind.LogicalNotExpression, CType(stmt.Condition.Accept(mNodesVisitor), ExpressionSyntax)),
                                               SyntaxFactory.Block(node.Statements.SelectMany(Function(s As VBSyntax.StatementSyntax) s.Accept(Me))).UnpackBlock()))
                    End If
                End If

                If node.LoopStatement.WhileOrUntilClause IsNot Nothing Then
                    Dim stmt As VBSyntax.WhileOrUntilClauseSyntax = node.LoopStatement.WhileOrUntilClause
                    If SyntaxTokenExtensions.IsKind(stmt.WhileOrUntilKeyword, VBasic.SyntaxKind.WhileKeyword) Then
                        Return SingleStatement(SyntaxFactory.DoStatement(
                                               SyntaxFactory.Block(node.Statements.SelectMany(Function(s As VBSyntax.StatementSyntax) s.Accept(Me))).UnpackBlock(),
                                               CType(stmt.Condition.Accept(mNodesVisitor), ExpressionSyntax)))
                    Else
                        Return SingleStatement(SyntaxFactory.DoStatement(
                                               SyntaxFactory.Block(node.Statements.SelectMany(Function(s As VBSyntax.StatementSyntax) s.Accept(Me))).UnpackBlock(),
                                               SyntaxFactory.PrefixUnaryExpression(SyntaxKind.LogicalNotExpression, CType(stmt.Condition.Accept(mNodesVisitor), ExpressionSyntax))))
                    End If
                End If

                Throw New NotSupportedException()
            End Function

            Private Function SingleStatement(ByVal statement As StatementSyntax) As SyntaxList(Of StatementSyntax)
                Return SyntaxFactory.SingletonList(statement)
            End Function

            Private Function SingleStatement(ByVal expression As ExpressionSyntax) As SyntaxList(Of StatementSyntax)
                Return SyntaxFactory.SingletonList(Of StatementSyntax)(SyntaxFactory.ExpressionStatement(expression))
            End Function
        End Class
    End Class

    Module Extensions

        <Extension()>
        Function UnpackBlock(ByVal block As BlockSyntax) As StatementSyntax
            Return If(block.Statements.Count = 1, block.Statements(0), block)
        End Function
    End Module
End Namespace
