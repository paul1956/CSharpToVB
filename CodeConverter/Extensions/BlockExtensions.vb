' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports CSharpToVBConverter.CSharpToVBVisitors.CSharpConverter
Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Friend Module BlockExtensions

    <Extension>
    Friend Function GetBodyStatements(block As CSS.BlockSyntax, visitor As MethodBodyVisitor) As SyntaxList(Of VBS.StatementSyntax)
        Dim statements As New List(Of VBS.StatementSyntax)
        For Each localFunction As CSS.LocalFunctionStatementSyntax In block.DescendantNodes().OfType(Of CSS.LocalFunctionStatementSyntax).ToList()
            Dim emptyStatement As VBS.StatementSyntax = localFunction.Accept(visitor)(0)
            If emptyStatement.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia OrElse
            emptyStatement.GetTrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                statements.Add(emptyStatement)
            End If
        Next
        For Each s As CSS.StatementSyntax In block.Statements
            If s.IsKind(CS.SyntaxKind.LocalFunctionStatement) Then
                Continue For
            End If
            statements.AddRange(s.Accept(visitor))
        Next
        If statements.Any Then
            If block.OpenBraceToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                statements(0) = statements(0).WithPrependedLeadingTrivia(block.OpenBraceToken.LeadingTrivia.ConvertTriviaList())
            End If
        End If

        Return Factory.List(statements)
    End Function

    <Extension>
    Friend Function GetEventBodyStatements(block As CSS.BlockSyntax, nodeVisitor As NodesVisitor, bodyVisitor As MethodBodyVisitor, eventLookupExpr As VBS.InvocationExpressionSyntax, eventType As VBS.TypeSyntax) As SyntaxList(Of VBS.StatementSyntax)
        Dim statements As New List(Of VBS.StatementSyntax)
        For Each localFunction As CSS.LocalFunctionStatementSyntax In block.DescendantNodes().OfType(Of CSS.LocalFunctionStatementSyntax).ToList()
            Dim emptyStatement As VBS.StatementSyntax = localFunction.Accept(bodyVisitor)(0)
            If emptyStatement.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia OrElse
            emptyStatement.GetTrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                statements.Add(emptyStatement)
            End If
        Next
        For Each s As CSS.StatementSyntax In block.Statements
            If s.IsKind(CS.SyntaxKind.LocalFunctionStatement) Then
                Continue For
            End If
            Dim cssExpr As CSS.ExpressionStatementSyntax = TryCast(s, CSS.ExpressionStatementSyntax)
            If cssExpr IsNot Nothing Then
                Dim assignStmt As CSS.AssignmentExpressionSyntax = TryCast(cssExpr.Expression, CSS.AssignmentExpressionSyntax)
                If assignStmt IsNot Nothing Then
                    Dim tempVar As VBS.IdentifierNameSyntax = Factory.IdentifierName(nodeVisitor.GetUniqueVariableNameInScope(block, "tempVar", nodeVisitor._usedIdentifiers))
                    Dim argumentList As VBS.ArgumentListSyntax = Factory.ParseArgumentList($"({eventLookupExpr.ArgumentList.Arguments(0)}, Value)")
                    If assignStmt.Kind = CS.SyntaxKind.SimpleAssignmentExpression AndAlso
                          assignStmt.Right.Kind() = CS.SyntaxKind.NullLiteralExpression Then
                        'Dim tempVar As SmallBasicCallback = TryCast(_buttonClicked("ButtonClicked"), SmallBasicCallback)
                        Dim asClause As VBS.AsClauseSyntax = Factory.SimpleAsClause(eventType) 'SmallBasicCallback
                        Dim tryCastExpression As VBS.EqualsValueSyntax = Factory.EqualsValue(Factory.TryCastExpression(eventLookupExpr, eventType))
                        Dim tryCastEventName As VBS.StatementSyntax = FactoryDimStatement(tempVar.ToString(), asClause, tryCastExpression).WithTrailingEol()
                        statements.Add(tryCastEventName)
                        ' If tempVar IsNot Nothing Then _buttonClicked.RemoveHandler("ButtonClicked", tempVar)
                        Dim simpleMemberAccess As VBS.ExpressionSyntax = Factory.InvocationExpression(Factory.MemberAccessExpression(VB.SyntaxKind.SimpleMemberAccessExpression, eventLookupExpr.Expression, DotToken, RemoveHandlerName), argumentList)
                        Dim removeHandlerStatement As VBS.StatementSyntax = Factory.ExpressionStatement(simpleMemberAccess)
                        Dim condition As VBS.ExpressionSyntax = Factory.IsNotExpression(tempVar, NothingExpression)
                        Dim ifStatement As VBS.StatementSyntax = Factory.SingleLineIfStatement(condition, Factory.SingletonList(removeHandlerStatement), elseClause:=Nothing)
                        statements.Add(ifStatement.WithTrailingEol())
                        Continue For
                    ElseIf assignStmt.Kind = CS.SyntaxKind.AddAssignmentExpression Then
                        If Not nodeVisitor._eventList.ContainsKey(eventLookupExpr.ToString()) Then
                            statements.Add(Factory.AddHandlerStatement(CType(assignStmt.Left.Accept(nodeVisitor), VBS.ExpressionSyntax), CType(assignStmt.Right.Accept(nodeVisitor), VBS.ExpressionSyntax)).WithTrailingEol())
                        Else
                            Dim simpleMemberAccess As VBS.ExpressionSyntax = Factory.InvocationExpression(Factory.MemberAccessExpression(VB.SyntaxKind.SimpleMemberAccessExpression, eventLookupExpr.Expression, DotToken, AddHandlerName), argumentList)
                            Dim addHandlerStatement As VBS.StatementSyntax = Factory.ExpressionStatement(simpleMemberAccess)
                            statements.Add(addHandlerStatement.WithTrailingEol())
                        End If
                        Continue For
                    ElseIf assignStmt.Kind = CS.SyntaxKind.SubtractAssignmentExpression Then
                        If Not nodeVisitor._eventList.ContainsKey(eventLookupExpr.ToString()) Then
                            statements.Add(Factory.RemoveHandlerStatement(CType(assignStmt.Left.Accept(nodeVisitor), VBS.ExpressionSyntax), Factory.IdentifierName("value")).WithTrailingEol())
                        Else
                            Dim simpleMemberAccess As VBS.ExpressionSyntax = Factory.InvocationExpression(Factory.MemberAccessExpression(VB.SyntaxKind.SimpleMemberAccessExpression, eventLookupExpr.Expression, DotToken, RemoveHandlerName), argumentList)
                            Dim removeHandlerStatement As VBS.StatementSyntax = Factory.ExpressionStatement(simpleMemberAccess)
                            statements.Add(removeHandlerStatement.WithTrailingEol())
                        End If
                        Continue For
                    End If
                End If
            End If
            statements.AddRange(s.Accept(bodyVisitor))
        Next
        If statements.Any Then
            If block.OpenBraceToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                statements(0) = statements(0).WithPrependedLeadingTrivia(block.OpenBraceToken.LeadingTrivia.ConvertTriviaList())
            End If
        End If

        Return Factory.List(statements)
    End Function

End Module
