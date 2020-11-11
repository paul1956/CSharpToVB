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
                Dim OperatorToken As SyntaxToken = GetOperatorToken(kind, IsReferenceType:=False)
                Return Factory.BinaryExpression(kind, left, OperatorToken.With(Factory.Space, Factory.Space), right).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitConstantPattern(node As CSS.ConstantPatternSyntax) As VB.VisualBasicSyntaxNode
                Return node.Expression.Accept(Me)
            End Function

            Public Overrides Function VisitDeclarationPattern(node As CSS.DeclarationPatternSyntax) As VB.VisualBasicSyntaxNode
                Dim StatementWithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                Dim LeadingTrivia As SyntaxTriviaList = StatementWithIssue.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=True, "VB has no direct equivalent To C# pattern variables 'is' expressions")
                Dim Designation As CSS.SingleVariableDesignationSyntax = DirectCast(node.Designation, CSS.SingleVariableDesignationSyntax)

                Dim value As VBS.ExpressionSyntax = Factory.ParseExpression($"TryCast({node.Designation.Accept(Me).NormalizeWhitespace.ToFullString}, {node.Type.Accept(Me).NormalizeWhitespace.ToFullString})")

                Dim VariableType As VBS.TypeSyntax = DirectCast(node.Type.Accept(Me), VBS.TypeSyntax)

                Dim DeclarationToBeAdded As VBS.LocalDeclarationStatementSyntax =
                    FactoryDimStatement(GenerateSafeVBToken(Designation.Identifier, node, _usedIdentifiers, _mSemanticModel),
                                   Factory.SimpleAsClause(VariableType),
                                   Factory.EqualsValue(NothingExpression)
                                  ).WithLeadingTrivia(LeadingTrivia)

                StatementWithIssue.AddMarker(DeclarationToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                Return value
            End Function

            Public Overrides Function VisitIsPatternExpression(node As CSS.IsPatternExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim Pattern As CSS.PatternSyntax = node.Pattern
                Dim VBExpression As VBS.ExpressionSyntax = DirectCast(node.Expression.Accept(Me), VBS.ExpressionSyntax)
                Dim StatementWithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                Dim ReportCheckCorrectness As Boolean = True
                If node.GetAncestor(Of CSS.SwitchSectionSyntax) IsNot Nothing Then
                    ReportCheckCorrectness = False
                End If

                If TypeOf Pattern Is CSS.DeclarationPatternSyntax Then
                    Dim DeclarationPattern As CSS.DeclarationPatternSyntax = DirectCast(Pattern, CSS.DeclarationPatternSyntax)
                    Dim designationNameToken As SyntaxToken
                    If TypeOf DeclarationPattern.Designation Is CSS.SingleVariableDesignationSyntax Then
                        Dim Designation As CSS.SingleVariableDesignationSyntax = DirectCast(DeclarationPattern.Designation, CSS.SingleVariableDesignationSyntax)
                        designationNameToken = GenerateSafeVBToken(Designation.Identifier, node, _usedIdentifiers, _mSemanticModel)
                    ElseIf TypeOf DeclarationPattern.Designation Is CSS.DiscardDesignationSyntax Then
                        designationNameToken = Factory.Identifier(node.GetUniqueVariableNameInScope("_1", _usedIdentifiers, _mSemanticModel))
                    End If

                    Dim VariableType As VBS.TypeSyntax = CType(DeclarationPattern.Type.Accept(Me), VBS.TypeSyntax)
                    Dim value As VBS.ExpressionSyntax = Factory.TypeOfIsExpression(VBExpression, VariableType)
                    Dim uniqueIdToken As SyntaxToken = Factory.Identifier(node.GetUniqueVariableNameInScope("TempVar", _usedIdentifiers, _mSemanticModel))

                    Dim DimToBeAdded As VBS.LocalDeclarationStatementSyntax =
                        FactoryDimStatement(uniqueIdToken,
                                       Factory.SimpleAsClause(Factory.PredefinedType(BooleanKeyword)),
                                       Factory.EqualsValue(value)).WithTrailingTrivia(VBEOLTrivia)
                    If StatementWithIssue.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        DimToBeAdded = DimToBeAdded.WithPrependedLeadingTrivia(StatementWithIssue.GetLeadingTrivia.ConvertTriviaList())
                    End If
                    StatementWithIssue.AddMarker(Statement:=DimToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)

                    If VariableType.IsKind(VB.SyntaxKind.PredefinedType) Then
                        DimToBeAdded = FactoryDimStatement(designationNameToken,
                                                      Factory.SimpleAsClause(VariableType),
                                                      initializer:=Nothing
                                                     ).WithTrailingTrivia(VBEOLTrivia)
                        StatementWithIssue.AddMarker(DimToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                        Dim simpleMemberAccess As VBS.MemberAccessExpressionSyntax = Factory.SimpleMemberAccessExpression(VariableType, Factory.IdentifierName("TryParse"))
                        Dim nodes As New List(Of VBS.ArgumentSyntax) From {
                            Factory.SimpleArgument(Factory.SimpleMemberAccessExpression(VBExpression.WithoutTrivia, Factory.IdentifierName("ToString"))),
                            Factory.SimpleArgument(Factory.IdentifierName(designationNameToken.WithoutTrivia))
                        }

                        Dim arguments As SeparatedSyntaxList(Of VBS.ArgumentSyntax) = Factory.SeparatedList(nodes)
                        Dim agrumentList As VBS.ArgumentListSyntax = Factory.ArgumentList(arguments)
                        Dim expression As VBS.InvocationExpressionSyntax = Factory.InvocationExpression(simpleMemberAccess, agrumentList)
                        Dim Statement As VBS.ExpressionStatementSyntax = Factory.ExpressionStatement(expression)
                        StatementWithIssue.AddMarker(Statement, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                    Else
                        DimToBeAdded =
                            FactoryDimStatement(designationNameToken,
                                           Factory.SimpleAsClause(VariableType),
                                           Factory.EqualsValue(VBExpression)
                                          ).WithTrailingTrivia(VBEOLTrivia)
                        StatementWithIssue.AddMarker(DimToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                    End If

                    Return Factory.IdentifierName(uniqueIdToken)
                ElseIf TypeOf Pattern Is CSS.ConstantPatternSyntax Then
                    Return Factory.IsExpression(VBExpression, DirectCast(node.Pattern.Accept(Me), VBS.ExpressionSyntax))
                ElseIf TypeOf Pattern Is CSS.UnaryPatternSyntax Then
                    Dim UnaryPattern As CSS.UnaryPatternSyntax = DirectCast(node.Pattern, CSS.UnaryPatternSyntax)
                    If Pattern.IsKind(CS.SyntaxKind.NotPattern) Then
                        Select Case True
                            Case TypeOf UnaryPattern.Pattern Is CSS.ConstantPatternSyntax
                                Return Factory.IsNotExpression(left:=VBExpression, right:=DirectCast(UnaryPattern.Pattern.Accept(Me), VBS.ExpressionSyntax))
                            Case TypeOf UnaryPattern.Pattern Is CSS.ParenthesizedPatternSyntax
                                Dim expr As CSS.PatternSyntax = CType(UnaryPattern.Pattern, CSS.ParenthesizedPatternSyntax).Pattern
                                If TypeOf expr Is CSS.BinaryPatternSyntax Then
                                    Dim bExpr As CSS.BinaryPatternSyntax = CType(expr, CSS.BinaryPatternSyntax)
                                    Dim kind As VB.SyntaxKind = GetExpressionKind(CS.CSharpExtensions.Kind(bExpr))
                                    Dim operatorToken As SyntaxToken = GetOperatorToken(kind, IsReferenceType:=False)
                                    Return Factory.BinaryExpression(kind, Factory.NotEqualsExpression(VBExpression, CType(bExpr.Left.Accept(Me), VBS.ExpressionSyntax)), operatorToken, Factory.NotEqualsExpression(VBExpression, CType(bExpr.Right.Accept(Me), VBS.ExpressionSyntax)))
                                    Stop
                                End If
                            Case TypeOf UnaryPattern.Pattern Is CSS.DeclarationPatternSyntax
                                Return Factory.IsNotExpression(left:=VBExpression, right:=DirectCast(UnaryPattern.Pattern.Accept(Me), VBS.ExpressionSyntax))
                            Case TypeOf UnaryPattern.Pattern Is CSS.RecursivePatternSyntax
                                Dim EmptyStatementWithError As VBS.EmptyStatementSyntax = FlagUnsupportedStatements(StatementWithIssue,
                                                                                        "Recursive Pattern Syntax",
                                                                                        CommentOutOriginalStatements:=True)
                                StatementWithIssue.AddMarker(Statement:=EmptyStatementWithError,
                                                 StatementHandlingOption.ReplaceStatement,
                                                 AllowDuplicates:=True)
                                Return Factory.IdentifierName("DoNotCare")
                            Case Else
                                Stop
                        End Select
                    End If
                    Throw UnreachableException(NameOf(Pattern))
                ElseIf TypeOf Pattern Is CSS.VarPatternSyntax Then
                    Dim DesignationIdentifier As VBS.IdentifierNameSyntax = CType(Pattern.Accept(Me), VBS.IdentifierNameSyntax)

                    Dim initializer As VBS.EqualsValueSyntax = Factory.EqualsValue(VBExpression)
                    Dim LeadingTrivia As SyntaxTriviaList = StatementWithIssue.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=True, "VB has no direct equivalent To C# var pattern expressions")
                    Dim DeclarationToBeAdded As VBS.LocalDeclarationStatementSyntax =
                                FactoryDimStatement(DesignationIdentifier.Identifier,
                                               asClause:=Nothing,
                                               Factory.EqualsValue(VBExpression)
                                              ).WithTrailingEOL
                    If ReportCheckCorrectness Then
                        DeclarationToBeAdded = DeclarationToBeAdded.WithPrependedLeadingTrivia(StatementWithIssue.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=True, "VB has no direct equivalent To C# var pattern expressions")).WithTrailingEOL
                    End If

                    StatementWithIssue.AddMarker(DeclarationToBeAdded,
                                                 StatementHandlingOption.PrependStatement, AllowDuplicates:=True)

                    Return DesignationIdentifier
                ElseIf TypeOf Pattern Is CSS.RecursivePatternSyntax Then
                    Dim EmptyStatementWithError As VBS.EmptyStatementSyntax = FlagUnsupportedStatements(StatementWithIssue,
                                                                                        "Recursive Pattern Syntax",
                                                                                        CommentOutOriginalStatements:=True)
                    StatementWithIssue.AddMarker(Statement:=EmptyStatementWithError,
                                                 StatementHandlingOption.ReplaceStatement,
                                                 AllowDuplicates:=True)
                    Return Factory.IdentifierName("DoNotCare")
                ElseIf TypeOf Pattern Is CSS.BinaryPatternSyntax Then
                    Dim EmptyStatementWithError As VBS.EmptyStatementSyntax = FlagUnsupportedStatements(StatementWithIssue,
                                                                                        "Binary Pattern Syntax",
                                                                                        CommentOutOriginalStatements:=True)
                    StatementWithIssue.AddMarker(Statement:=EmptyStatementWithError,
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
                Return Factory.IdentifierName(node.GetUniqueVariableNameInScope($"RecursivePattern_{node.ToString.GetSafeVBName}", _usedIdentifiers, _mSemanticModel))
            End Function

            Public Overrides Function VisitVarPattern(node As CSS.VarPatternSyntax) As VB.VisualBasicSyntaxNode
                Dim designationIdentifier As String

                If TypeOf node.Designation Is CSS.SingleVariableDesignationSyntax Then
                    designationIdentifier = DirectCast(node.Designation, CSS.SingleVariableDesignationSyntax).Identifier.ToString
                ElseIf TypeOf node.Designation Is CSS.ParenthesizedVariableDesignationSyntax Then
                    Dim Designation As CSS.ParenthesizedVariableDesignationSyntax = DirectCast(node.Designation, CSS.ParenthesizedVariableDesignationSyntax)
                    Dim VariableNames As New List(Of String)
                    For Each e As IndexClass(Of CSS.VariableDesignationSyntax) In Designation.Variables.WithIndex
                        If e.Value.RawKind = CS.SyntaxKind.ParenthesizedVariableDesignation Then
                            Dim sBuilder As New StringBuilder
                            CreateDesignationName(ProcessVariableDesignation(CType(e.Value, CSS.ParenthesizedVariableDesignationSyntax)), sBuilder)
                            VariableNames.Add(sBuilder.ToString)
                        Else
                            If e.Value.IsKind(CS.SyntaxKind.DiscardDesignation) Then
                                VariableNames.Add("__DiscardDesignation__")
                            Else
                                VariableNames.Add(e.Value.Accept(Me).ToString)
                            End If
                        End If
                    Next
                    designationIdentifier = node.GetUniqueVariableNameInScope("TempVar", _usedIdentifiers, _mSemanticModel)
                Else
                    Stop
                    Throw UnreachableException
                End If
                Dim DesignationNameToken As SyntaxToken = GenerateSafeVBToken(CS.SyntaxFactory.Identifier(designationIdentifier), node, _usedIdentifiers, _mSemanticModel)
                Return Factory.IdentifierName(DesignationNameToken)
            End Function
        End Class

    End Class

End Namespace
