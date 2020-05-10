' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports CSharpToVBCodeConverter.Util

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBCodeConverter.DestVisualBasic

    Partial Public Class CSharpConverter

        Partial Friend Class NodesVisitor
            Inherits CS.CSharpSyntaxVisitor(Of VB.VisualBasicSyntaxNode)

            Private Function WrapTypedNameIfNecessary(name As VBS.ExpressionSyntax, originalName As CSS.ExpressionSyntax) As VB.VisualBasicSyntaxNode
                If TypeOf originalName Is CSS.InvocationExpressionSyntax Then
                    Return name
                End If
                Dim OriginalNameParent As SyntaxNode = originalName.Parent
                If TypeOf OriginalNameParent Is CSS.ArgumentSyntax OrElse
                   TypeOf OriginalNameParent Is CSS.ArrayTypeSyntax OrElse
                   TypeOf OriginalNameParent Is CSS.AttributeSyntax OrElse
                   TypeOf OriginalNameParent Is CSS.ConstantPatternSyntax OrElse
                   TypeOf OriginalNameParent Is CSS.InvocationExpressionSyntax OrElse
                   TypeOf OriginalNameParent Is CSS.MemberAccessExpressionSyntax OrElse
                   TypeOf OriginalNameParent Is CSS.MemberBindingExpressionSyntax OrElse
                   TypeOf OriginalNameParent Is CSS.NameSyntax OrElse
                   TypeOf OriginalNameParent Is CSS.ObjectCreationExpressionSyntax OrElse
                   TypeOf OriginalNameParent Is CSS.QualifiedNameSyntax OrElse
                   TypeOf OriginalNameParent Is CSS.TupleElementSyntax OrElse
                   TypeOf OriginalNameParent Is CSS.TypeArgumentListSyntax Then
                    Return name
                End If

                Dim OriginalNameParentArgumentList As SyntaxNode = OriginalNameParent.Parent
                If OriginalNameParentArgumentList IsNot Nothing AndAlso TypeOf OriginalNameParentArgumentList Is CSS.ArgumentListSyntax Then
                    Dim OriginalNameParentArgumentListParentInvocationExpression As SyntaxNode = OriginalNameParentArgumentList.Parent
                    If OriginalNameParentArgumentListParentInvocationExpression IsNot Nothing AndAlso TypeOf OriginalNameParentArgumentListParentInvocationExpression Is CSS.InvocationExpressionSyntax Then
                        Dim expression As CSS.InvocationExpressionSyntax = DirectCast(OriginalNameParentArgumentListParentInvocationExpression, CSS.InvocationExpressionSyntax)
                        If TypeOf expression?.Expression Is CSS.IdentifierNameSyntax Then
                            If DirectCast(expression.Expression, CSS.IdentifierNameSyntax).Identifier.ToString = "nameof" Then
                                Return name
                            End If
                        End If
                    End If
                End If
                Dim symbolInfo As SymbolInfo
                Try
                    symbolInfo = ModelExtensions.GetSymbolInfo(_mSemanticModel, originalName)
                Catch ex As OperationCanceledException
                    Throw
                Catch ex As ArgumentException
                    Return name
                Catch ex As Exception
                    Throw
                End Try
                Dim symbol As ISymbol = If(symbolInfo.Symbol, symbolInfo.CandidateSymbols.FirstOrDefault())
                If symbol?.IsKind(SymbolKind.Method) Then
                    Return VBFactory.AddressOfExpression(name)
                End If
                Return name
            End Function

            Public Overrides Function VisitAliasQualifiedName(node As CSS.AliasQualifiedNameSyntax) As VB.VisualBasicSyntaxNode
                If node Is Nothing Then
                    Throw New ArgumentNullException(NameOf(node))
                End If

                Return WrapTypedNameIfNecessary(VBFactory.QualifiedName(DirectCast(node.[Alias].Accept(Me), VBS.NameSyntax), DirectCast(node.Name.Accept(Me), VBS.SimpleNameSyntax)), node).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitGenericName(node As CSS.GenericNameSyntax) As VB.VisualBasicSyntaxNode
                If node Is Nothing Then
                    Throw New ArgumentNullException(NameOf(node))
                End If

                Dim TypeArgumentList As VBS.TypeArgumentListSyntax = DirectCast(node.TypeArgumentList.Accept(Me), VBS.TypeArgumentListSyntax)
                If node.Parent.IsKind(CS.SyntaxKind.MethodDeclaration) Then
                    If CType(node.Parent, CSS.MethodDeclarationSyntax).ReturnType.Equals(node) Then
                        Return WrapTypedNameIfNecessary(VBFactory.GenericName(GenerateSafeVBToken(node.Identifier, IsQualifiedName:=False, IsTypeName:=True).WithTrailingTrivia, TypeArgumentList), node)
                    End If
                End If
                If node.Parent.IsKind(CS.SyntaxKind.ObjectCreationExpression) Then
                    Return WrapTypedNameIfNecessary(VBFactory.GenericName(GenerateSafeVBToken(node.Identifier, IsQualifiedName:=False, IsTypeName:=True).WithTrailingTrivia, TypeArgumentList), node)
                End If
                Return WrapTypedNameIfNecessary(VBFactory.GenericName(GenerateSafeVBToken(node.Identifier).WithTrailingTrivia, TypeArgumentList), node)
            End Function

            Public Overrides Function VisitIdentifierName(node As CSS.IdentifierNameSyntax) As VB.VisualBasicSyntaxNode
                If node Is Nothing Then
                    Throw New ArgumentNullException(NameOf(node))
                End If

                Dim OriginalNameParent As SyntaxNode = node.Parent
                If TypeOf OriginalNameParent Is CSS.MemberAccessExpressionSyntax OrElse
                    TypeOf OriginalNameParent Is CSS.MemberBindingExpressionSyntax OrElse
                    OriginalNameParent.IsKind(CS.SyntaxKind.AsExpression) OrElse
                    OriginalNameParent.IsKind(CS.SyntaxKind.Argument) OrElse
                    (TypeOf OriginalNameParent Is CSS.NameEqualsSyntax AndAlso TypeOf OriginalNameParent.Parent Is CSS.AnonymousObjectMemberDeclaratorSyntax) Then
                    If TypeOf OriginalNameParent Is CSS.MemberAccessExpressionSyntax Then
                        Dim ParentAsMemberAccessExpression As CSS.MemberAccessExpressionSyntax = DirectCast(OriginalNameParent, CSS.MemberAccessExpressionSyntax)
                        If ParentAsMemberAccessExpression.Expression.IsKind(CS.SyntaxKind.IdentifierName) Then
                            Dim IdentifierExpression As CSS.IdentifierNameSyntax = DirectCast(ParentAsMemberAccessExpression.Expression, CSS.IdentifierNameSyntax)
                            If IdentifierExpression.Identifier.ToString = node.Identifier.ToString Then
                                Return WrapTypedNameIfNecessary(VBFactory.IdentifierName(GenerateSafeVBToken(node.Identifier)), node)
                            End If
                        End If
                    End If
                    If TypeOf OriginalNameParent Is CSS.ArgumentSyntax Then
                        If VB.SyntaxFacts.IsKeywordKind(VB.SyntaxFacts.GetKeywordKind(node.Identifier.ValueText)) Then
                            Return VBFactory.ParseName(MakeVBSafeName(node.Identifier.ValueText))
                        End If
                    End If
                    Return WrapTypedNameIfNecessary(VBFactory.IdentifierName(GenerateSafeVBToken(node.Identifier, IsQualifiedName:=True, IsTypeName:=False)), node)
                End If
                If OriginalNameParent.IsKind(CS.SyntaxKind.SimpleAssignmentExpression) Then
                    Dim AssignmentStatement As CSS.AssignmentExpressionSyntax = CType(OriginalNameParent, CSS.AssignmentExpressionSyntax)
                    If node.ToString.Equals(AssignmentStatement.Left.ToString, StringComparison.Ordinal) AndAlso AssignmentStatement.Left.ToString.Equals(AssignmentStatement.Right.ToString, StringComparison.OrdinalIgnoreCase) Then
                        If OriginalNameParent.IsParentKind(CS.SyntaxKind.ObjectInitializerExpression) Then
                            Dim name As VBS.IdentifierNameSyntax = VBFactory.IdentifierName(GenerateSafeVBToken(node.Identifier, IsQualifiedName:=True, IsTypeName:=True))
                            Return name
                        End If
                        If node.Ancestors().OfType(Of CSS.ConstructorDeclarationSyntax).Any Then
                            Dim name As VBS.IdentifierNameSyntax = VBFactory.IdentifierName(GenerateSafeVBToken(node.Identifier, IsQualifiedName:=True, IsTypeName:=False))
                            If node.Parent.IsParentKind(CS.SyntaxKind.ObjectInitializerExpression) Then
                                Return name
                            End If
                            Return VBFactory.SimpleMemberAccessExpression(VBFactory.MeExpression.WithLeadingTrivia(name.GetLeadingTrivia), name.WithoutLeadingTrivia)
                        End If
                    End If
                End If
                If TypeOf OriginalNameParent Is CSS.DeclarationExpressionSyntax Then
                    If node.ToString = "var" Then
                        Return PredefinedTypeObject
                    End If
                    Return ConvertToType(node.ToString)
                End If
                If (TypeOf OriginalNameParent Is CSS.VariableDeclarationSyntax OrElse TypeOf OriginalNameParent Is CSS.ArrayTypeSyntax) AndAlso node.Identifier.ValueText = "dynamic" Then
                    Return PredefinedTypeObject
                End If
                If TypeOf OriginalNameParent Is CSS.ParameterSyntax AndAlso node.ToString = "Variant" Then
                    Return PredefinedTypeObject
                End If
                If TypeOf OriginalNameParent Is CSS.UsingDirectiveSyntax OrElse
                    OriginalNameParent.IsKind(CS.SyntaxKind.TypeArgumentList, CS.SyntaxKind.SimpleBaseType) Then
                    Return WrapTypedNameIfNecessary(VBFactory.IdentifierName(GenerateSafeVBToken(node.Identifier, IsQualifiedName:=True, IsTypeName:=True)), node)
                End If
                If TypeOf OriginalNameParent Is CSS.ArrayTypeSyntax Then
                    Return WrapTypedNameIfNecessary(VBFactory.IdentifierName(GenerateSafeVBToken(node.Identifier, IsQualifiedName:=False, IsTypeName:=True)), node)
                End If
                ' The trivial on node reflects the wrong place on the file as order is switched so don't convert trivia here
                If TypeOf OriginalNameParent Is CSS.AliasQualifiedNameSyntax Then
                    If node.Identifier.ValueText = "global" Then
                        Dim cSharpNode As CS.CSharpSyntaxNode = GetStatementwithIssues(node, ReportErrors:=False)
                        If cSharpNode IsNot Nothing AndAlso cSharpNode.IsKind(CS.SyntaxKind.UsingDirective) Then
                            Return VBFactory.ParseExpression("[Global]")
                        Else
                            Return VBFactory.ParseExpression("Global")
                        End If
                    End If
                End If
                If TypeOf OriginalNameParent Is CSS.NameColonSyntax Then
                    Return WrapTypedNameIfNecessary(VBFactory.IdentifierName(GenerateSafeVBToken(node.Identifier, IsQualifiedName:=True, IsTypeName:=True)), node)
                End If

                Return WrapTypedNameIfNecessary(VBFactory.IdentifierName(GenerateSafeVBToken(node.Identifier, OriginalNameParent.IsKind(CS.SyntaxKind.QualifiedName), IsTypeName:=TypeOf OriginalNameParent Is CSS.InvocationExpressionSyntax)), node)
            End Function

            Public Overrides Function VisitQualifiedName(node As CSS.QualifiedNameSyntax) As VB.VisualBasicSyntaxNode
                If node Is Nothing Then
                    Throw New ArgumentNullException(NameOf(node))
                End If

                Return WrapTypedNameIfNecessary(VBFactory.QualifiedName(DirectCast(node.Left.Accept(Me), VBS.NameSyntax), DirectCast(node.Right.Accept(Me), VBS.SimpleNameSyntax)), node)
            End Function

        End Class

    End Class

End Namespace
