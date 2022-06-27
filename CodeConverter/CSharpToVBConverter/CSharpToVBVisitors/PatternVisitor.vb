' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Text
Imports Microsoft.CodeAnalysis
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBConverter.CSharpToVBVisitors

    Partial Public Class CSharpConverter

        Partial Friend Class NodesVisitor
            Inherits CS.CSharpSyntaxVisitor(Of VB.VisualBasicSyntaxNode)

            Public Overrides Function VisitBinaryPattern(node As CSS.BinaryPatternSyntax) As VB.VisualBasicSyntaxNode
                Dim left As VBS.ExpressionSyntax = CType(node.Left.Accept(Me), VBS.ExpressionSyntax)
                Dim right As VBS.ExpressionSyntax = CType(node.Right.Accept(Me), VBS.ExpressionSyntax)
                Dim kind As VB.SyntaxKind = CS.CSharpExtensions.Kind(node).GetExpressionKind()
                Dim operatorToken As SyntaxToken = kind.GetOperatorToken(isReferenceType:=False)
                Return Factory.BinaryExpression(kind, left, operatorToken.With(SpaceTrivia, SpaceTrivia), right).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitConstantPattern(node As CSS.ConstantPatternSyntax) As VB.VisualBasicSyntaxNode
                Return node.Expression.Accept(Me)
            End Function

            Public Overrides Function VisitDeclarationPattern(node As CSS.DeclarationPatternSyntax) As VB.VisualBasicSyntaxNode
                Dim statementWithIssue As CS.CSharpSyntaxNode = GetStatementWithIssues(node)
                Dim designation As CSS.SingleVariableDesignationSyntax = DirectCast(node.Designation, CSS.SingleVariableDesignationSyntax)

                Dim expr As VBS.ExpressionSyntax = Factory.ParseExpression($"TryCast({node.Designation.Accept(Me).NormalizeWhitespace.ToFullString}, {node.Type.Accept(Me).NormalizeWhitespace.ToFullString})")

                Dim variableType As VBS.TypeSyntax = DirectCast(node.Type.Accept(Me), VBS.TypeSyntax)

                Dim declarationToBeAdded As VBS.LocalDeclarationStatementSyntax =
                    FactoryDimStatement(Me.GenerateSafeVbToken(designation.Identifier, node),
                                   Factory.SimpleAsClause(variableType),
                                   Factory.EqualsValue(NothingExpression)
                                  )

                statementWithIssue.AddMarker(declarationToBeAdded, StatementHandlingOption.PrependStatement, allowDuplicates:=True)
                Return expr
            End Function

            Public Overrides Function VisitIsPatternExpression(node As CSS.IsPatternExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim pattern As CSS.PatternSyntax = node.Pattern
                Dim vbExpr As VBS.ExpressionSyntax = DirectCast(node.Expression.Accept(Me), VBS.ExpressionSyntax)
                Dim statementWithIssue As CS.CSharpSyntaxNode = GetStatementWithIssues(node)
                Dim reportCheckCorrectness As Boolean
                Dim expr As VBS.ExpressionSyntax
                Dim varType As VBS.TypeSyntax
                Select Case True
                    Case TypeOf pattern Is CSS.DeclarationPatternSyntax
                        Dim declarationPattern As CSS.DeclarationPatternSyntax = DirectCast(pattern, CSS.DeclarationPatternSyntax)
                        Dim designationNameToken As SyntaxToken
                        If TypeOf declarationPattern.Designation Is CSS.SingleVariableDesignationSyntax Then
                            Dim designation As CSS.SingleVariableDesignationSyntax = DirectCast(declarationPattern.Designation, CSS.SingleVariableDesignationSyntax)
                            designationNameToken = Me.GenerateSafeVbToken(designation.Identifier, node)
                        ElseIf TypeOf declarationPattern.Designation Is CSS.DiscardDesignationSyntax Then
                            designationNameToken = Factory.Identifier(Me.GetUniqueVariableNameInScope(node, "_1", _usedIdentifiers))
                        End If

                        varType = CType(declarationPattern.Type.Accept(Me), VBS.TypeSyntax)
                        expr = Factory.TryCastExpression(vbExpr, varType)

                        Dim asClause As VBS.AsClauseSyntax = Factory.SimpleAsClause(varType)
                        Dim dimToBeAdded As VBS.LocalDeclarationStatementSyntax = FactoryDimStatement(designationNameToken.ToString, asClause, Nothing).WithTrailingTrivia(VbEolTrivia)

                        If statementWithIssue.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                            dimToBeAdded = dimToBeAdded.WithPrependedLeadingTrivia(statementWithIssue.GetLeadingTrivia.ConvertTriviaList())
                        End If
                        statementWithIssue.AddMarker(statement:=dimToBeAdded, StatementHandlingOption.PrependStatement, allowDuplicates:=True)
                        dimToBeAdded = FactoryDimStatement(designationNameToken,
                                                  Factory.SimpleAsClause(varType),
                                                  initializer:=Nothing
                                                 ).WithTrailingTrivia(VbEolTrivia)
                        statementWithIssue.AddMarker(dimToBeAdded, StatementHandlingOption.PrependStatement, allowDuplicates:=True)
                        Return Factory.IsNotExpression(expr, NothingExpression)
                    Case TypeOf pattern Is CSS.ConstantPatternSyntax
                        Return Factory.IsExpression(vbExpr, DirectCast(node.Pattern.Accept(Me), VBS.ExpressionSyntax))
                    Case TypeOf pattern Is CSS.UnaryPatternSyntax
                        Dim unaryPattern As CSS.UnaryPatternSyntax = DirectCast(node.Pattern, CSS.UnaryPatternSyntax)
                        If pattern.IsKind(CS.SyntaxKind.NotPattern) Then
                            Select Case True
                                Case TypeOf unaryPattern.Pattern Is CSS.ConstantPatternSyntax
                                    Return Factory.IsNotExpression(left:=vbExpr, right:=DirectCast(unaryPattern.Pattern.Accept(Me), VBS.ExpressionSyntax))
                                Case TypeOf unaryPattern.Pattern Is CSS.ParenthesizedPatternSyntax
                                    Dim bExpr As CSS.BinaryPatternSyntax = TryCast(CType(unaryPattern.Pattern, CSS.ParenthesizedPatternSyntax).Pattern, CSS.BinaryPatternSyntax)
                                    If bExpr IsNot Nothing Then
                                        Dim kind As VB.SyntaxKind = CS.CSharpExtensions.Kind(bExpr).GetExpressionKind()
                                        Dim operatorToken As SyntaxToken = kind.GetOperatorToken(isReferenceType:=False)
                                        Return Factory.BinaryExpression(kind, Factory.NotEqualsExpression(vbExpr, CType(bExpr.Left.Accept(Me), VBS.ExpressionSyntax)), operatorToken, Factory.NotEqualsExpression(vbExpr, CType(bExpr.Right.Accept(Me), VBS.ExpressionSyntax)))
                                    End If
                                Case TypeOf unaryPattern.Pattern Is CSS.DeclarationPatternSyntax
                                    Dim left As VBS.TryCastExpressionSyntax = DirectCast(unaryPattern.Pattern.Accept(Me), VBS.TryCastExpressionSyntax)
                                    left = left.WithExpression(vbExpr)
                                    Return Factory.IsExpression(left, NothingExpression)
                                Case TypeOf unaryPattern.Pattern Is CSS.RecursivePatternSyntax
                                    Dim emptyStatementWithError As VBS.EmptyStatementSyntax = FlagUnsupportedStatements(statementWithIssue,
                                                                                                                        "Recursive Pattern Syntax",
                                                                                                                        True)
                                    statementWithIssue.AddMarker(statement:=emptyStatementWithError,
                                                     StatementHandlingOption.ReplaceStatement,
                                                     allowDuplicates:=True)
                                    Return Factory.IdentifierName("DoNotCare")
                                Case Else
                                    Stop
                            End Select
                        End If
                        Throw UnreachableException(NameOf(pattern))
                    Case TypeOf pattern Is CSS.VarPatternSyntax
                        Dim designationIdent As VBS.IdentifierNameSyntax = CType(pattern.Accept(Me), VBS.IdentifierNameSyntax)
                        Dim declarationToBeAdded As VBS.LocalDeclarationStatementSyntax =
                                FactoryDimStatement(designationIdent.Identifier,
                                               asClause:=Nothing,
                                               Factory.EqualsValue(vbExpr)
                                              ).WithTrailingEol
                        If reportCheckCorrectness Then
                            declarationToBeAdded = declarationToBeAdded.WithPrependedLeadingTrivia(statementWithIssue.CheckCorrectnessLeadingTrivia(attemptToPortMade:=True, "VB has no direct equivalent To C# var pattern expressions")).WithTrailingEol
                        End If

                        statementWithIssue.AddMarker(declarationToBeAdded,
                                                 StatementHandlingOption.PrependStatement, allowDuplicates:=True)

                        Return designationIdent
                    Case TypeOf pattern Is CSS.RecursivePatternSyntax
                        Dim emptyStatementWithError As VBS.EmptyStatementSyntax = FlagUnsupportedStatements(statementWithIssue,
                                                                                        "Recursive Pattern Syntax",
                                                                                        commentOutOriginalStatements:=True)
                        statementWithIssue.AddMarker(statement:=emptyStatementWithError,
                                                 StatementHandlingOption.ReplaceStatement,
                                                 allowDuplicates:=True)
                        Return Factory.IdentifierName("DoNotCare")
                    Case TypeOf pattern Is CSS.BinaryPatternSyntax
                        Dim emptyStatementWithError As VBS.EmptyStatementSyntax = FlagUnsupportedStatements(statementWithIssue,
                                                                                        "Binary Pattern Syntax",
                                                                                        commentOutOriginalStatements:=True)
                        statementWithIssue.AddMarker(statement:=emptyStatementWithError,
                                                 StatementHandlingOption.ReplaceStatement,
                                                 allowDuplicates:=True)
                        Return Factory.IdentifierName("DoNotCare")
                    Case TypeOf pattern Is CSS.ParenthesizedPatternSyntax
                        Dim emptyStatementWithError As VBS.EmptyStatementSyntax = FlagUnsupportedStatements(statementWithIssue,
                                                                                                        "Parenthesized Pattern Syntax",
                                                                                                        commentOutOriginalStatements:=True)
                        statementWithIssue.AddMarker(statement:=emptyStatementWithError,
                                                 StatementHandlingOption.ReplaceStatement,
                                                 allowDuplicates:=True)

                        Return Factory.IdentifierName("DoNotCare")
                End Select
                Throw UnreachableException
            End Function

            Public Overrides Function VisitParenthesizedPattern(node As CSS.ParenthesizedPatternSyntax) As VB.VisualBasicSyntaxNode
                Dim expression As VBS.ExpressionSyntax = CType(node.Pattern.Accept(Me), VBS.ExpressionSyntax)
                Return Factory.ParenthesizedExpression(expression)
            End Function

            Public Overrides Function VisitRecursivePattern(node As CSS.RecursivePatternSyntax) As VB.VisualBasicSyntaxNode
                Return Factory.IdentifierName(Me.GetUniqueVariableNameInScope(node, $"RecursivePattern_{node.ToString.GetSafeVbName}", _usedIdentifiers))
            End Function

            Public Overrides Function VisitVarPattern(node As CSS.VarPatternSyntax) As VB.VisualBasicSyntaxNode
                Dim designationIdentifier As String

                If TypeOf node.Designation Is CSS.SingleVariableDesignationSyntax Then
                    designationIdentifier = DirectCast(node.Designation, CSS.SingleVariableDesignationSyntax).Identifier.ToString
                ElseIf TypeOf node.Designation Is CSS.ParenthesizedVariableDesignationSyntax Then
                    Dim designation As CSS.ParenthesizedVariableDesignationSyntax = DirectCast(node.Designation, CSS.ParenthesizedVariableDesignationSyntax)
                    Dim variableNames As New List(Of String)
                    For Each e As IndexClass(Of CSS.VariableDesignationSyntax) In designation.Variables.WithIndex
                        If e.Value.RawKind = CS.SyntaxKind.ParenthesizedVariableDesignation Then
                            Dim sBuilder As New StringBuilder
                            CreateDesignationName(ProcessVariableDesignation(CType(e.Value, CSS.ParenthesizedVariableDesignationSyntax)), sBuilder)
                            variableNames.Add(sBuilder.ToString)
                        Else
                            If e.Value.IsKind(CS.SyntaxKind.DiscardDesignation) Then
                                variableNames.Add("__DiscardDesignation__")
                            Else
                                variableNames.Add(e.Value.Accept(Me).ToString)
                            End If
                        End If
                    Next
                    designationIdentifier = Me.GetUniqueVariableNameInScope(node, "TempVar", _usedIdentifiers)
                Else
                    Throw UnreachableException
                End If
                Return Factory.IdentifierName(Me.GenerateSafeVbToken(CS.SyntaxFactory.Identifier(designationIdentifier),
                    node))
            End Function

        End Class

    End Class

End Namespace
