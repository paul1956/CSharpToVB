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
                Dim discardNameToken As SyntaxToken = GenerateSafeVBToken(node.UnderscoreToken, node, _usedIdentifiers, _mSemanticModel)
                Dim IdentifierExpression As VBS.IdentifierNameSyntax = Factory.IdentifierName(discardNameToken)
                Dim ModifiedIdentifier As VBS.ModifiedIdentifierSyntax = Factory.ModifiedIdentifier(discardNameToken)
                Dim TypeName As VB.VisualBasicSyntaxNode
                Dim parentExpression As CSS.DeclarationExpressionSyntax = DirectCast(node.Parent, CSS.DeclarationExpressionSyntax)
                If parentExpression IsNot Nothing Then
                    TypeName = parentExpression.Type.Accept(Me)
                    If TypeName.ToString = "var" Then
                        TypeName = PredefinedTypeObject
                    End If
                ElseIf node.ToString = "_" Then
                    TypeName = PredefinedTypeObject
                Else
                    Stop
                    TypeName = PredefinedTypeObject
                End If

                Dim asClause As VBS.SimpleAsClauseSyntax = Factory.SimpleAsClause(AsKeyword.With(Factory.Space, Factory.Space), New SyntaxList(Of VBS.AttributeListSyntax), ConvertToType(TypeName.NormalizeWhitespace.ToString))

                Dim DeclarationStatement As VBS.LocalDeclarationStatementSyntax =
                            FactoryDimStatement(discardNameToken, asClause, Factory.EqualsValue(NothingExpression))

                GetStatementwithIssues(node).AddMarker(DeclarationStatement, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                Return IdentifierExpression
            End Function

            Public Overrides Function VisitParenthesizedVariableDesignation(node As CSS.ParenthesizedVariableDesignationSyntax) As VB.VisualBasicSyntaxNode
                Dim vbVariables As List(Of VBS.ModifiedIdentifierSyntax) = ProcessVariableDesignation(node)
                Dim vbNames As SeparatedSyntaxList(Of VBS.ModifiedIdentifierSyntax) = Factory.SeparatedList(vbVariables)
                Return Factory.VariableDeclarator(vbNames, Factory.SimpleAsClause(PredefinedTypeObject), initializer:=Nothing)
            End Function

            Public Overrides Function VisitSingleVariableDesignation(Node As CSS.SingleVariableDesignationSyntax) As VB.VisualBasicSyntaxNode
                Dim identifier As SyntaxToken = GenerateSafeVBToken(Node.Identifier, Node, _usedIdentifiers, _mSemanticModel)
                Dim identifierExpression As VBS.IdentifierNameSyntax = Factory.IdentifierName(identifier)

                If Node.Parent.IsKind(CS.SyntaxKind.DeclarationExpression) Then
                    ' var quantity
                    Dim Designation As CSS.DeclarationExpressionSyntax = CType(Node.Parent, CSS.DeclarationExpressionSyntax)
                    ' out var quantity
                    Dim Declaration As CSS.ArgumentSyntax = CType(Designation.Parent, CSS.ArgumentSyntax)
                    ' (item.tostring, out var quantity)
                    Dim TypeName As VBS.TypeSyntax
                    If Designation.Type.IsVar Then
                        TypeName = Factory.PredefinedType(ObjectKeyword)
                        Dim ArgumentList As CSS.ArgumentListSyntax = TryCast(Declaration.Parent, CSS.ArgumentListSyntax)
                        If ArgumentList IsNot Nothing AndAlso
                           ArgumentList.Arguments.Count = 2 AndAlso
                           ArgumentList.Parent.IsKind(CS.SyntaxKind.InvocationExpression) AndAlso
                           ArgumentList.Arguments(index:=1).Equals(Declaration) Then
                            Dim Invocation As CSS.InvocationExpressionSyntax = TryCast(ArgumentList.Parent, CSS.InvocationExpressionSyntax)
                            If Invocation IsNot Nothing Then
                                Dim Expression As CSS.MemberAccessExpressionSyntax = TryCast(Invocation.Expression, CSS.MemberAccessExpressionSyntax)
                                If Expression IsNot Nothing AndAlso Expression.Name.Identifier.ValueText = "TryGetValue" Then
                                    Dim memberExpression As CSS.MemberAccessExpressionSyntax = CType(Invocation.Expression, CSS.MemberAccessExpressionSyntax)
                                    If memberExpression IsNot Nothing Then
                                        Dim _Typeinfo As TypeInfo = _mSemanticModel.GetTypeInfo(memberExpression.Expression)
                                        If _Typeinfo.Type IsNot Nothing AndAlso Not _Typeinfo.Type.IsErrorType Then
                                            TypeName = _Typeinfo.Type.ConvertToType
                                            Dim GenericName As VBS.GenericNameSyntax = TryCast(TypeName, VBS.GenericNameSyntax)
                                            If GenericName IsNot Nothing Then
                                                Dim _arguments As SeparatedSyntaxList(Of VBS.TypeSyntax) = GenericName.TypeArgumentList.Arguments
                                                If _arguments.Count = 2 Then
                                                    TypeName = _arguments(1)
                                                End If
                                            End If
                                        End If
                                    End If
                                End If
                            End If
                        End If
                    Else
                        TypeName = CType(Designation.Type.Accept(Me), VBS.TypeSyntax)
                    End If

                    Dim declarationToBeAdded As VBS.LocalDeclarationStatementSyntax =
                                FactoryDimStatement(identifier,
                                                    Factory.SimpleAsClause(TypeName),
                                                    Factory.EqualsValue(NothingExpression))

                    GetStatementwithIssues(Node).AddMarker(declarationToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                ElseIf Node.Parent.IsKind(CS.SyntaxKind.DeclarationPattern) Then
                    Dim DeclarationPattern As CSS.DeclarationPatternSyntax = DirectCast(Node.Parent, CSS.DeclarationPatternSyntax)
                    Dim CasePatternSwitchLabel As CSS.SwitchLabelSyntax = TryCast(DeclarationPattern.Parent, CSS.SwitchLabelSyntax)
                    If CasePatternSwitchLabel IsNot Nothing Then
                        Dim SwitchSection As CSS.SwitchSectionSyntax = DirectCast(CasePatternSwitchLabel.Parent, CSS.SwitchSectionSyntax)
                        Dim SwitchStatement As CSS.SwitchStatementSyntax = DirectCast(SwitchSection.Parent, CSS.SwitchStatementSyntax)
                        Dim SwitchExpression As VBS.ExpressionSyntax = DirectCast(SwitchStatement.Expression.Accept(Me), VBS.ExpressionSyntax)

                        Dim TypeName As VBS.TypeSyntax = DirectCast(DeclarationPattern.Type.Accept(Me), VBS.TypeSyntax)
                        Return Factory.TypeOfIsExpression(SwitchExpression, TypeName)
                    End If
                End If

                Return identifierExpression
            End Function

        End Class

    End Class

End Namespace
