' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

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

            Public Overrides Function VisitDiscardDesignation(node As CSS.DiscardDesignationSyntax) As VB.VisualBasicSyntaxNode
                Dim discardNameToken As SyntaxToken = GenerateSafeVBToken(node.UnderscoreToken, node, _usedIdentifiers, _semanticModel)
                Dim identExpr As VBS.IdentifierNameSyntax = Factory.IdentifierName(discardNameToken)
                Dim modifiedIdent As VBS.ModifiedIdentifierSyntax = Factory.ModifiedIdentifier(discardNameToken)
                Dim typeName As VB.VisualBasicSyntaxNode
                Dim parentExpression As CSS.DeclarationExpressionSyntax = DirectCast(node.Parent, CSS.DeclarationExpressionSyntax)
                If parentExpression IsNot Nothing Then
                    typeName = parentExpression.Type.Accept(Me)
                    If typeName.ToString = "var" Then
                        typeName = PredefinedTypeObject
                    End If
                ElseIf node.ToString = "_" Then
                    typeName = PredefinedTypeObject
                Else
                    Stop
                    typeName = PredefinedTypeObject
                End If

                Dim asClause As VBS.SimpleAsClauseSyntax = Factory.SimpleAsClause(AsKeyword.With(Factory.Space, Factory.Space), New SyntaxList(Of VBS.AttributeListSyntax), ConvertToType(typeName.NormalizeWhitespace.ToString))

                GetStatementwithIssues(node).AddMarker(FactoryDimStatement(discardNameToken,
                                                                           asClause,
                                                                           Factory.EqualsValue(NothingExpression)),
                                                       StatementHandlingOption.PrependStatement,
                                                       AllowDuplicates:=True)
                Return identExpr
            End Function

            Public Overrides Function VisitParenthesizedVariableDesignation(node As CSS.ParenthesizedVariableDesignationSyntax) As VB.VisualBasicSyntaxNode
                Dim vbVariables As List(Of VBS.ModifiedIdentifierSyntax) = ProcessVariableDesignation(node)
                Dim vbNames As SeparatedSyntaxList(Of VBS.ModifiedIdentifierSyntax) = Factory.SeparatedList(vbVariables)
                Return Factory.VariableDeclarator(vbNames, Factory.SimpleAsClause(PredefinedTypeObject), initializer:=Nothing)
            End Function

            Public Overrides Function VisitSingleVariableDesignation(Node As CSS.SingleVariableDesignationSyntax) As VB.VisualBasicSyntaxNode
                Dim identifier As SyntaxToken = GenerateSafeVBToken(Node.Identifier, Node, _usedIdentifiers, _semanticModel)
                Dim identifierExpression As VBS.IdentifierNameSyntax = Factory.IdentifierName(identifier)

                If Node.Parent.IsKind(CS.SyntaxKind.DeclarationExpression) Then
                    ' var quantity
                    Dim designation As CSS.DeclarationExpressionSyntax = CType(Node.Parent, CSS.DeclarationExpressionSyntax)
                    ' out var quantity
                    Dim declaration As CSS.ArgumentSyntax = CType(designation.Parent, CSS.ArgumentSyntax)
                    ' (item.tostring, out var quantity)
                    Dim typeName As VBS.TypeSyntax
                    If designation.Type.IsVar Then
                        typeName = Factory.PredefinedType(ObjectKeyword)
                        Dim argList As CSS.ArgumentListSyntax = TryCast(declaration.Parent, CSS.ArgumentListSyntax)
                        If argList IsNot Nothing AndAlso
                           argList.Arguments.Count = 2 AndAlso
                           argList.Parent.IsKind(CS.SyntaxKind.InvocationExpression) AndAlso
                           argList.Arguments(index:=1).Equals(declaration) Then
                            Dim csInvocation As CSS.InvocationExpressionSyntax = TryCast(argList.Parent, CSS.InvocationExpressionSyntax)
                            If csInvocation IsNot Nothing Then
                                Dim csExpr As CSS.MemberAccessExpressionSyntax = TryCast(csInvocation.Expression, CSS.MemberAccessExpressionSyntax)
                                If csExpr IsNot Nothing AndAlso csExpr.Name.Identifier.ValueText = "TryGetValue" Then
                                    Dim memberExpression As CSS.MemberAccessExpressionSyntax = CType(csInvocation.Expression, CSS.MemberAccessExpressionSyntax)
                                    If memberExpression IsNot Nothing Then
                                        Dim typeinf As TypeInfo = _semanticModel.GetTypeInfo(memberExpression.Expression)
                                        If typeinf.Type IsNot Nothing AndAlso Not typeinf.Type.IsErrorType Then
                                            typeName = typeinf.Type.ConvertToType
                                            Dim genericName As VBS.GenericNameSyntax = TryCast(typeName, VBS.GenericNameSyntax)
                                            If genericName IsNot Nothing Then
                                                Dim args As SeparatedSyntaxList(Of VBS.TypeSyntax) = genericName.TypeArgumentList.Arguments
                                                If args.Count = 2 Then
                                                    typeName = args(1)
                                                End If
                                            End If
                                        End If
                                    End If
                                End If
                            End If
                        End If
                    Else
                        typeName = CType(designation.Type.Accept(Me), VBS.TypeSyntax)
                    End If

                    Dim declarationToBeAdded As VBS.LocalDeclarationStatementSyntax =
                                FactoryDimStatement(identifier,
                                                    Factory.SimpleAsClause(typeName),
                                                    Factory.EqualsValue(NothingExpression))

                    GetStatementwithIssues(Node).AddMarker(declarationToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                ElseIf Node.Parent.IsKind(CS.SyntaxKind.DeclarationPattern) Then
                    Dim declarationPattern As CSS.DeclarationPatternSyntax = DirectCast(Node.Parent, CSS.DeclarationPatternSyntax)
                    Dim casePatternSwitchLabel As CSS.SwitchLabelSyntax = TryCast(declarationPattern.Parent, CSS.SwitchLabelSyntax)
                    If casePatternSwitchLabel IsNot Nothing Then
                        Dim switchSection As CSS.SwitchSectionSyntax = DirectCast(casePatternSwitchLabel.Parent, CSS.SwitchSectionSyntax)
                        Dim switchStatement As CSS.SwitchStatementSyntax = DirectCast(switchSection.Parent, CSS.SwitchStatementSyntax)
                        Dim switchExpression As VBS.ExpressionSyntax = DirectCast(switchStatement.Expression.Accept(Me), VBS.ExpressionSyntax)

                        Dim typeName As VBS.TypeSyntax = DirectCast(declarationPattern.Type.Accept(Me), VBS.TypeSyntax)
                        Return Factory.TypeOfIsExpression(switchExpression, typeName)
                    End If
                End If

                Return identifierExpression
            End Function

        End Class

    End Class

End Namespace
