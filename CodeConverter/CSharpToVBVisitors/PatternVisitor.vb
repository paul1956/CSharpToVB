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

Namespace CSharpToVBConverter.ToVisualBasic

    Partial Public Class CSharpConverter

        Partial Friend Class NodesVisitor
            Inherits CS.CSharpSyntaxVisitor(Of VB.VisualBasicSyntaxNode)

            Public Overrides Function VisitBinaryPattern(node As CSS.BinaryPatternSyntax) As VB.VisualBasicSyntaxNode
                Dim left As VBS.ExpressionSyntax = CType(node.Left.Accept(Me), VBS.ExpressionSyntax)
                Dim right As VBS.ExpressionSyntax = CType(node.Right.Accept(Me), VBS.ExpressionSyntax)
                Dim kind As VB.SyntaxKind = CS.CSharpExtensions.Kind(node).GetExpressionKind()
                Dim operatorToken As SyntaxToken = GetOperatorToken(kind, IsReferenceType:=False)
                Return Factory.BinaryExpression(kind, left, operatorToken.With(Factory.Space, Factory.Space), right).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitConstantPattern(node As CSS.ConstantPatternSyntax) As VB.VisualBasicSyntaxNode
                Return node.Expression.Accept(Me)
            End Function

            Public Overrides Function VisitDeclarationPattern(node As CSS.DeclarationPatternSyntax) As VB.VisualBasicSyntaxNode
                Dim statementWithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                Dim leadingTrivia As SyntaxTriviaList = statementWithIssue.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=True, "VB has no direct equivalent To C# pattern variables 'is' expressions")
                Dim designation As CSS.SingleVariableDesignationSyntax = DirectCast(node.Designation, CSS.SingleVariableDesignationSyntax)

                Dim value As VBS.ExpressionSyntax = Factory.ParseExpression($"TryCast({node.Designation.Accept(Me).NormalizeWhitespace.ToFullString}, {node.Type.Accept(Me).NormalizeWhitespace.ToFullString})")

                Dim variableType As VBS.TypeSyntax = DirectCast(node.Type.Accept(Me), VBS.TypeSyntax)

                Dim declarationToBeAdded As VBS.LocalDeclarationStatementSyntax =
                    FactoryDimStatement(GenerateSafeVBToken(designation.Identifier, node, _usedIdentifiers, _semanticModel),
                                   Factory.SimpleAsClause(variableType),
                                   Factory.EqualsValue(NothingExpression)
                                  ).WithLeadingTrivia(leadingTrivia)

                statementWithIssue.AddMarker(declarationToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                Return value
            End Function

            Public Overrides Function VisitIsPatternExpression(node As CSS.IsPatternExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim pattern As CSS.PatternSyntax = node.Pattern
                Dim vbExpr As VBS.ExpressionSyntax = DirectCast(node.Expression.Accept(Me), VBS.ExpressionSyntax)
                Dim statementWithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                Dim reportCheckCorrectness As Boolean = True
                If node.GetAncestor(Of CSS.SwitchSectionSyntax) IsNot Nothing Then
                    reportCheckCorrectness = False
                End If

                If TypeOf pattern Is CSS.DeclarationPatternSyntax Then
                    Dim declarationPattern As CSS.DeclarationPatternSyntax = DirectCast(pattern, CSS.DeclarationPatternSyntax)
                    Dim designationNameToken As SyntaxToken
                    If TypeOf declarationPattern.Designation Is CSS.SingleVariableDesignationSyntax Then
                        Dim designation As CSS.SingleVariableDesignationSyntax = DirectCast(declarationPattern.Designation, CSS.SingleVariableDesignationSyntax)
                        designationNameToken = GenerateSafeVBToken(designation.Identifier, node, _usedIdentifiers, _semanticModel)
                    ElseIf TypeOf declarationPattern.Designation Is CSS.DiscardDesignationSyntax Then
                        designationNameToken = Factory.Identifier(node.GetUniqueVariableNameInScope("_1", _usedIdentifiers, _semanticModel))
                    End If

                    Dim varType As VBS.TypeSyntax = CType(declarationPattern.Type.Accept(Me), VBS.TypeSyntax)
                    Dim value As VBS.ExpressionSyntax = Factory.TypeOfIsExpression(vbExpr, varType)
                    Dim uniqueIdToken As SyntaxToken = Factory.Identifier(node.GetUniqueVariableNameInScope("TempVar", _usedIdentifiers, _semanticModel))

                    Dim dimToBeAdded As VBS.LocalDeclarationStatementSyntax =
                        FactoryDimStatement(uniqueIdToken,
                                       Factory.SimpleAsClause(Factory.PredefinedType(BooleanKeyword)),
                                       Factory.EqualsValue(value)).WithTrailingTrivia(VBEOLTrivia)
                    If statementWithIssue.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        dimToBeAdded = dimToBeAdded.WithPrependedLeadingTrivia(statementWithIssue.GetLeadingTrivia.ConvertTriviaList())
                    End If
                    statementWithIssue.AddMarker(statement:=dimToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)

                    If varType.IsKind(VB.SyntaxKind.PredefinedType) Then
                        dimToBeAdded = FactoryDimStatement(designationNameToken,
                                                      Factory.SimpleAsClause(varType),
                                                      initializer:=Nothing
                                                     ).WithTrailingTrivia(VBEOLTrivia)
                        statementWithIssue.AddMarker(dimToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                        Dim simpleMemberAccess As VBS.MemberAccessExpressionSyntax = Factory.SimpleMemberAccessExpression(varType, Factory.IdentifierName("TryParse"))
                        Dim nodes As New List(Of VBS.ArgumentSyntax) From {
                            Factory.SimpleArgument(Factory.SimpleMemberAccessExpression(vbExpr.WithoutTrivia, Factory.IdentifierName("ToString"))),
                            Factory.SimpleArgument(Factory.IdentifierName(designationNameToken.WithoutTrivia))
                        }

                        Dim arguments As SeparatedSyntaxList(Of VBS.ArgumentSyntax) = Factory.SeparatedList(nodes)
                        Dim agrumentList As VBS.ArgumentListSyntax = Factory.ArgumentList(arguments)
                        Dim expression As VBS.InvocationExpressionSyntax = Factory.InvocationExpression(simpleMemberAccess, agrumentList)
                        Dim statement As VBS.ExpressionStatementSyntax = Factory.ExpressionStatement(expression)
                        statementWithIssue.AddMarker(statement, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                    Else
                        dimToBeAdded = FactoryDimStatement(designationNameToken,
                                                           Factory.SimpleAsClause(varType.AdjustExpressionTrivia(AdjustLeading:=False)),
                                                           Factory.EqualsValue(vbExpr)
                                                          ).WithTrailingTrivia(VBEOLTrivia)
                        statementWithIssue.AddMarker(dimToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
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
                                If TypeOf expr Is CSS.BinaryPatternSyntax Then
                                    Dim bExpr As CSS.BinaryPatternSyntax = CType(expr, CSS.BinaryPatternSyntax)
                                    Dim kind As VB.SyntaxKind = GetExpressionKind(CS.CSharpExtensions.Kind(bExpr))
                                    Dim operatorToken As SyntaxToken = GetOperatorToken(kind, IsReferenceType:=False)
                                    Return Factory.BinaryExpression(kind, Factory.NotEqualsExpression(vbExpr, CType(bExpr.Left.Accept(Me), VBS.ExpressionSyntax)), operatorToken, Factory.NotEqualsExpression(vbExpr, CType(bExpr.Right.Accept(Me), VBS.ExpressionSyntax)))
                                    Stop
                                End If
                            Case TypeOf unaryPattern.Pattern Is CSS.DeclarationPatternSyntax
                                Return Factory.IsNotExpression(left:=vbExpr, right:=DirectCast(unaryPattern.Pattern.Accept(Me), VBS.ExpressionSyntax))
                            Case TypeOf unaryPattern.Pattern Is CSS.RecursivePatternSyntax
                                Dim emptyStatementWithError As VBS.EmptyStatementSyntax = FlagUnsupportedStatements(statementWithIssue,
                                                                                        "Recursive Pattern Syntax",
                                                                                        CommentOutOriginalStatements:=True)
                                statementWithIssue.AddMarker(statement:=emptyStatementWithError,
                                                 StatementHandlingOption.ReplaceStatement,
                                                 AllowDuplicates:=True)
                                Return Factory.IdentifierName("DoNotCare")
                            Case Else
                                Stop
                        End Select
                    End If
                    Throw UnreachableException(NameOf(pattern))
                ElseIf TypeOf pattern Is CSS.VarPatternSyntax Then
                    Dim designationIdent As VBS.IdentifierNameSyntax = CType(pattern.Accept(Me), VBS.IdentifierNameSyntax)

                    Dim initializer As VBS.EqualsValueSyntax = Factory.EqualsValue(vbExpr)
                    Dim leadingTrivia As SyntaxTriviaList = statementWithIssue.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=True, "VB has no direct equivalent To C# var pattern expressions")
                    Dim declarationToBeAdded As VBS.LocalDeclarationStatementSyntax =
                                FactoryDimStatement(designationIdent.Identifier,
                                               asClause:=Nothing,
                                               Factory.EqualsValue(vbExpr)
                                              ).WithTrailingEOL
                    If reportCheckCorrectness Then
                        declarationToBeAdded = declarationToBeAdded.WithPrependedLeadingTrivia(statementWithIssue.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=True, "VB has no direct equivalent To C# var pattern expressions")).WithTrailingEOL
                    End If

                    statementWithIssue.AddMarker(declarationToBeAdded,
                                                 StatementHandlingOption.PrependStatement, AllowDuplicates:=True)

                    Return designationIdent
                ElseIf TypeOf pattern Is CSS.RecursivePatternSyntax Then
                    Dim emptyStatementWithError As VBS.EmptyStatementSyntax = FlagUnsupportedStatements(statementWithIssue,
                                                                                        "Recursive Pattern Syntax",
                                                                                        CommentOutOriginalStatements:=True)
                    statementWithIssue.AddMarker(statement:=emptyStatementWithError,
                                                 StatementHandlingOption.ReplaceStatement,
                                                 AllowDuplicates:=True)
                    Return Factory.IdentifierName("DoNotCare")
                ElseIf TypeOf pattern Is CSS.BinaryPatternSyntax Then
                    Dim emptyStatementWithError As VBS.EmptyStatementSyntax = FlagUnsupportedStatements(statementWithIssue,
                                                                                        "Binary Pattern Syntax",
                                                                                        CommentOutOriginalStatements:=True)
                    statementWithIssue.AddMarker(statement:=emptyStatementWithError,
                                                 StatementHandlingOption.ReplaceStatement,
                                                 AllowDuplicates:=True)
                    Return Factory.IdentifierName("DoNotCare")
                End If

                Stop
                Throw UnreachableException
            End Function

            Public Overrides Function VisitParenthesizedPattern(node As CSS.ParenthesizedPatternSyntax) As VB.VisualBasicSyntaxNode
                Dim expression As VBS.ExpressionSyntax = CType(node.Pattern.Accept(Me), VBS.ExpressionSyntax)
                Return Factory.ParenthesizedExpression(expression)
            End Function

            Public Overrides Function VisitRecursivePattern(node As CSS.RecursivePatternSyntax) As VB.VisualBasicSyntaxNode
                Return Factory.IdentifierName(node.GetUniqueVariableNameInScope($"RecursivePattern_{node.ToString.GetSafeVBName}", _usedIdentifiers, _semanticModel))
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
                    Stop
                    Throw UnreachableException
                End If
                Return Factory.IdentifierName(GenerateSafeVBToken(CS.SyntaxFactory.Identifier(designationIdentifier),
                                                                  node,
                                                                  _usedIdentifiers,
                                                                  _semanticModel))
            End Function

        End Class

    End Class

End Namespace
