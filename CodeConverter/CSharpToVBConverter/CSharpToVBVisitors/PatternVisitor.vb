' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Text
Imports Extensions
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
                Dim leadingTrivia As SyntaxTriviaList = statementWithIssue.CheckCorrectnessLeadingTrivia(attemptToPortMade:=True, "VB has no direct equivalent To C# pattern variables 'is' expressions")
                Dim designation As CSS.SingleVariableDesignationSyntax = DirectCast(node.Designation, CSS.SingleVariableDesignationSyntax)

                Dim value As VBS.ExpressionSyntax = Factory.ParseExpression($"TryCast({node.Designation.Accept(Me).NormalizeWhitespace.ToFullString}, {node.Type.Accept(Me).NormalizeWhitespace.ToFullString})")

                Dim variableType As VBS.TypeSyntax = DirectCast(node.Type.Accept(Me), VBS.TypeSyntax)

                Dim declarationToBeAdded As VBS.LocalDeclarationStatementSyntax =
                    FactoryDimStatement(GenerateSafeVbToken(designation.Identifier, node, _semanticModel, _usedIdentifiers),
                                   Factory.SimpleAsClause(variableType),
                                   Factory.EqualsValue(NothingExpression)
                                  ).WithLeadingTrivia(leadingTrivia)

                statementWithIssue.AddMarker(declarationToBeAdded, StatementHandlingOption.PrependStatement, allowDuplicates:=True)
                Return value
            End Function

            Public Overrides Function VisitIsPatternExpression(node As CSS.IsPatternExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim pattern As CSS.PatternSyntax = node.Pattern
                Dim vbExpr As VBS.ExpressionSyntax = DirectCast(node.Expression.Accept(Me), VBS.ExpressionSyntax)
                Dim statementWithIssue As CS.CSharpSyntaxNode = GetStatementWithIssues(node)
                Dim reportCheckCorrectness As Boolean = True
                If node.GetAncestor(Of CSS.SwitchSectionSyntax) IsNot Nothing Then
                    reportCheckCorrectness = False
                End If

                If TypeOf pattern Is CSS.DeclarationPatternSyntax Then
                    Dim declarationPattern As CSS.DeclarationPatternSyntax = DirectCast(pattern, CSS.DeclarationPatternSyntax)
                    Dim designationNameToken As SyntaxToken
                    If TypeOf declarationPattern.Designation Is CSS.SingleVariableDesignationSyntax Then
                        Dim designation As CSS.SingleVariableDesignationSyntax = DirectCast(declarationPattern.Designation, CSS.SingleVariableDesignationSyntax)
                        designationNameToken = GenerateSafeVbToken(designation.Identifier, node, _semanticModel, _usedIdentifiers)
                    ElseIf TypeOf declarationPattern.Designation Is CSS.DiscardDesignationSyntax Then
                        designationNameToken = Factory.Identifier(node.GetUniqueVariableNameInScope("_1", _usedIdentifiers, _semanticModel))
                    End If

                    Dim varType As VBS.TypeSyntax = CType(declarationPattern.Type.Accept(Me), VBS.TypeSyntax)
                    Dim value As VBS.ExpressionSyntax = Factory.TypeOfIsExpression(vbExpr, varType)
                    Dim uniqueIdToken As SyntaxToken = Factory.Identifier(node.GetUniqueVariableNameInScope("TempVar", _usedIdentifiers, _semanticModel))

                    Dim dimToBeAdded As VBS.LocalDeclarationStatementSyntax =
                        FactoryDimStatement(uniqueIdToken,
                                       Factory.SimpleAsClause(Factory.PredefinedType(BooleanKeyword)),
                                       Factory.EqualsValue(value)).WithTrailingTrivia(VbEolTrivia)
                    If statementWithIssue.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        dimToBeAdded = dimToBeAdded.WithPrependedLeadingTrivia(statementWithIssue.GetLeadingTrivia.ConvertTriviaList())
                    End If
                    statementWithIssue.AddMarker(statement:=dimToBeAdded, StatementHandlingOption.PrependStatement, allowDuplicates:=True)

                    If varType.IsKind(VB.SyntaxKind.PredefinedType) Then
                        dimToBeAdded = FactoryDimStatement(designationNameToken,
                                                      Factory.SimpleAsClause(varType),
                                                      initializer:=Nothing
                                                     ).WithTrailingTrivia(VbEolTrivia)
                        statementWithIssue.AddMarker(dimToBeAdded, StatementHandlingOption.PrependStatement, allowDuplicates:=True)
                        Dim simpleMemberAccess As VBS.MemberAccessExpressionSyntax = Factory.SimpleMemberAccessExpression(varType, Factory.IdentifierName("TryParse"))
                        Dim nodes As New List(Of VBS.ArgumentSyntax) From {
                            Factory.SimpleArgument(Factory.SimpleMemberAccessExpression(vbExpr.WithoutTrivia, Factory.IdentifierName("ToString"))),
                            Factory.SimpleArgument(Factory.IdentifierName(designationNameToken.WithoutTrivia))
                        }

                        Dim arguments As SeparatedSyntaxList(Of VBS.ArgumentSyntax) = Factory.SeparatedList(nodes)
                        Dim argumentList As VBS.ArgumentListSyntax = Factory.ArgumentList(arguments)
                        Dim expression As VBS.InvocationExpressionSyntax = Factory.InvocationExpression(simpleMemberAccess, argumentList)
                        Dim statement As VBS.ExpressionStatementSyntax = Factory.ExpressionStatement(expression)
                        statementWithIssue.AddMarker(statement, StatementHandlingOption.PrependStatement, allowDuplicates:=True)
                    Else
                        dimToBeAdded = FactoryDimStatement(designationNameToken,
                                                           Factory.SimpleAsClause(varType.AdjustExpressionTrivia(adjustLeading:=False, directiveNotAllowed:=False)),
                                                           Factory.EqualsValue(vbExpr)
                                                          ).WithTrailingTrivia(VbEolTrivia)
                        statementWithIssue.AddMarker(dimToBeAdded, StatementHandlingOption.PrependStatement, allowDuplicates:=True)
                    End If

                    Return Factory.IdentifierName(uniqueIdToken)
                ElseIf TypeOf pattern Is CSS.ConstantPatternSyntax Then
                    Return Factory.IsExpression(vbExpr, DirectCast(node.Pattern.Accept(Me), VBS.ExpressionSyntax))
                ElseIf TypeOf pattern Is CSS.UnaryPatternSyntax Then
                    Dim unaryPattern As CSS.UnaryPatternSyntax = DirectCast(node.Pattern, CSS.UnaryPatternSyntax)
                    If pattern.IsKind(CS.SyntaxKind.NotPattern) Then
                        Select Case True
                            Case TypeOf unaryPattern.Pattern Is CSS.ConstantPatternSyntax
                                Return Factory.IsNotExpression(left:=vbExpr, right:=DirectCast(unaryPattern.Pattern.Accept(Me), VBS.ExpressionSyntax))
                            Case TypeOf unaryPattern.Pattern Is CSS.ParenthesizedPatternSyntax
                                Dim expr As CSS.PatternSyntax = CType(unaryPattern.Pattern, CSS.ParenthesizedPatternSyntax).Pattern
                                Dim bExpr As CSS.BinaryPatternSyntax = TryCast(expr, CSS.BinaryPatternSyntax)
                                If bExpr IsNot Nothing Then
                                    Dim kind As VB.SyntaxKind = CS.CSharpExtensions.Kind(bExpr).GetExpressionKind()
                                    Dim operatorToken As SyntaxToken = kind.GetOperatorToken(isReferenceType:=False)
                                    Return Factory.BinaryExpression(kind, Factory.NotEqualsExpression(vbExpr, CType(bExpr.Left.Accept(Me), VBS.ExpressionSyntax)), operatorToken, Factory.NotEqualsExpression(vbExpr, CType(bExpr.Right.Accept(Me), VBS.ExpressionSyntax)))
                                End If
                            Case TypeOf unaryPattern.Pattern Is CSS.DeclarationPatternSyntax
                                Return Factory.IsNotExpression(left:=vbExpr, right:=DirectCast(unaryPattern.Pattern.Accept(Me), VBS.ExpressionSyntax))
                            Case TypeOf unaryPattern.Pattern Is CSS.RecursivePatternSyntax
                                Dim emptyStatementWithError As VBS.EmptyStatementSyntax = FlagUnsupportedStatements(statementWithIssue,
                                                                                        "Recursive Pattern Syntax",
                                                                                        commentOutOriginalStatements:=True)
                                statementWithIssue.AddMarker(statement:=emptyStatementWithError,
                                                 StatementHandlingOption.ReplaceStatement,
                                                 allowDuplicates:=True)
                                Return Factory.IdentifierName("DoNotCare")
                            Case Else
                                Stop
                        End Select
                    End If
                    Throw UnreachableException(NameOf(pattern))
                ElseIf TypeOf pattern Is CSS.VarPatternSyntax Then
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
                ElseIf TypeOf pattern Is CSS.RecursivePatternSyntax Then
                    Dim emptyStatementWithError As VBS.EmptyStatementSyntax = FlagUnsupportedStatements(statementWithIssue,
                                                                                        "Recursive Pattern Syntax",
                                                                                        commentOutOriginalStatements:=True)
                    statementWithIssue.AddMarker(statement:=emptyStatementWithError,
                                                 StatementHandlingOption.ReplaceStatement,
                                                 allowDuplicates:=True)
                    Return Factory.IdentifierName("DoNotCare")
                ElseIf TypeOf pattern Is CSS.BinaryPatternSyntax Then
                    Dim emptyStatementWithError As VBS.EmptyStatementSyntax = FlagUnsupportedStatements(statementWithIssue,
                                                                                        "Binary Pattern Syntax",
                                                                                        commentOutOriginalStatements:=True)
                    statementWithIssue.AddMarker(statement:=emptyStatementWithError,
                                                 StatementHandlingOption.ReplaceStatement,
                                                 allowDuplicates:=True)
                    Return Factory.IdentifierName("DoNotCare")
                End If
                Throw UnreachableException
            End Function

            Public Overrides Function VisitParenthesizedPattern(node As CSS.ParenthesizedPatternSyntax) As VB.VisualBasicSyntaxNode
                Dim expression As VBS.ExpressionSyntax = CType(node.Pattern.Accept(Me), VBS.ExpressionSyntax)
                Return Factory.ParenthesizedExpression(expression)
            End Function

            Public Overrides Function VisitRecursivePattern(node As CSS.RecursivePatternSyntax) As VB.VisualBasicSyntaxNode
                Return Factory.IdentifierName(node.GetUniqueVariableNameInScope($"RecursivePattern_{node.ToString.GetSafeVbName}", _usedIdentifiers, _semanticModel))
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
                    designationIdentifier = node.GetUniqueVariableNameInScope("TempVar", _usedIdentifiers, _semanticModel)
                Else
                    Throw UnreachableException
                End If
                Return Factory.IdentifierName(GenerateSafeVbToken(CS.SyntaxFactory.Identifier(designationIdentifier),
                    node,
                    _semanticModel,
                    _usedIdentifiers))
            End Function

        End Class

    End Class

End Namespace
