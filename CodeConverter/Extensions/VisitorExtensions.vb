' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices

Imports CSharpToVBCodeConverter.ToVisualBasic.CSharpConverter
Imports CSharpToVBCodeConverter.Utilities

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CSharp.Syntax

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Friend Module VisitorExtensions

    <Extension>
    Friend Function GetBodyStatements(visitor As MethodBodyVisitor, block As BlockSyntax) As SyntaxList(Of VBS.StatementSyntax)
        Dim Statements As New List(Of VBS.StatementSyntax)
        For Each localFunction As LocalFunctionStatementSyntax In block.DescendantNodes().OfType(Of LocalFunctionStatementSyntax).ToList()
            Dim EmptyStatement As VBS.StatementSyntax = localFunction.Accept(visitor)(0)
            If EmptyStatement.ContainsCommentOrDirectiveTrivia Then
                Statements.Add(EmptyStatement)
            End If
        Next
        For Each s As StatementSyntax In block.Statements
            If s.IsKind(CS.SyntaxKind.LocalFunctionStatement) Then
                Continue For
            End If
            Statements.AddRange(s.Accept(visitor))
            If Statements.Any Then
                If block.OpenBraceToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    Dim Trivia As IEnumerable(Of SyntaxTrivia) = ConvertTrivia(block.OpenBraceToken.LeadingTrivia)
                    Statements(0) = Statements(0).WithPrependedLeadingTrivia(Trivia)
                End If
            End If
        Next

        Return VBFactory.List(Statements)
    End Function

    <Extension>
    Friend Function GetExpressionBodyStatements(visitor As NodesVisitor, NodeExpressionBody As ArrowExpressionClauseSyntax) As SyntaxList(Of VBS.StatementSyntax)
        Dim Statement As VBS.StatementSyntax
        Dim ExpressionBody As VB.VisualBasicSyntaxNode = NodeExpressionBody.Accept(visitor)
        If TypeOf ExpressionBody Is VBS.TryBlockSyntax Then
            Dim TryBlock As VBS.TryBlockSyntax = CType(ExpressionBody, VBS.TryBlockSyntax)
            Dim StatementList As SyntaxList(Of VBS.StatementSyntax) = ReplaceOneStatementWithMarkedStatements(NodeExpressionBody, TryBlock.Statements(0))
            For Each e As IndexClass(Of VBS.StatementSyntax) In TryBlock.Statements.WithIndex
                StatementList = StatementList.Add(e.Value)
            Next
            Return StatementList
        ElseIf TypeOf ExpressionBody Is VBS.AssignmentStatementSyntax OrElse
                        TypeOf ExpressionBody Is VBS.AddRemoveHandlerStatementSyntax OrElse
                        TypeOf ExpressionBody Is VBS.ThrowStatementSyntax Then
            Statement = DirectCast(ExpressionBody, VBS.StatementSyntax)
        Else
            Dim LeadingTrivia As New List(Of SyntaxTrivia)
            LeadingTrivia.AddRange(ExpressionBody.GetLeadingTrivia)
            Statement = VBFactory.ReturnStatement(DirectCast(ExpressionBody.WithLeadingTrivia(SpaceTrivia), VBS.ExpressionSyntax)).WithLeadingTrivia(LeadingTrivia)
        End If
        Return ReplaceOneStatementWithMarkedStatements(NodeExpressionBody, Statement.WithTrailingEOL)
    End Function

End Module
