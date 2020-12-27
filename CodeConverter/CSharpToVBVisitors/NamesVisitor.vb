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

            Private Function GetOverloadedFormalParameterTypeOrNull(argumentChildExpression As CSS.ExpressionSyntax) As VBS.TypeSyntax
                Dim y As SymbolInfo = _semanticModel.GetSymbolInfo(argumentChildExpression)
                If TypeOf argumentChildExpression?.Parent IsNot CSS.ArgumentSyntax Then
                    Return Nothing
                End If
                Dim nameArgument As CSS.ArgumentSyntax = CType(argumentChildExpression?.Parent, CSS.ArgumentSyntax)

                If TypeOf nameArgument.Parent?.Parent IsNot CSS.InvocationExpressionSyntax Then
                    Return Nothing
                End If
                Dim ies As CSS.InvocationExpressionSyntax = CType((nameArgument.Parent?.Parent), CSS.InvocationExpressionSyntax)
                Dim argIndex As Integer = ies.ArgumentList.Arguments.IndexOf(nameArgument)
                'TODO: Deal with named parameters
                Dim symbolInfo As SymbolInfo = _semanticModel.GetSymbolInfo(ies.Expression)
                Dim destinationType As ISymbol = symbolInfo.ExtractBestMatch(Of ISymbol)(Function(m) m.GetParameters().Length > argIndex)
                If destinationType?.GetParameters.Any Then
                    Dim symbolType As ITypeSymbol = destinationType.GetParameters(argIndex).Type
                    Dim nameSyntax As VBS.NameSyntax = GetFullyQualifiedNameSyntax(symbolType)
                    Return nameSyntax
                End If

                Return Nothing
            End Function

            Private Function WrapTypedNameIfNecessary(name As VBS.ExpressionSyntax, originalName As CSS.ExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim originalNameParent As SyntaxNode = originalName.Parent

                If TypeOf originalNameParent Is CSS.AttributeSyntax OrElse
                   TypeOf originalNameParent Is CSS.TypeArgumentListSyntax OrElse
                   TypeOf originalNameParent Is CSS.MemberAccessExpressionSyntax OrElse
                   TypeOf originalNameParent Is CSS.MemberBindingExpressionSyntax OrElse
                   TypeOf originalNameParent Is CSS.InvocationExpressionSyntax OrElse
                   TypeOf originalNameParent Is CSS.SimpleBaseTypeSyntax OrElse
                   TypeOf originalNameParent Is CSS.TypeArgumentListSyntax OrElse
                   TypeOf originalNameParent.AncestorsAndSelf().OfType(Of CSS.AttributeSyntax).DefaultIfEmpty Is CSS.AttributeSyntax Then
                    Return name
                End If

                Dim isTreeNotSame As Boolean = _semanticModel.SyntaxTree IsNot originalName.SyntaxTree
                If isTreeNotSame Then
                    Return name
                End If

                Dim symbolInfo As SymbolInfo
                Try
                    symbolInfo = _semanticModel.GetSymbolInfo(originalName)
                Catch ex As OperationCanceledException
                    Throw
                Catch ex As Exception
                    Throw
                End Try
                Dim symbol As ISymbol = If(symbolInfo.Symbol, symbolInfo.CandidateSymbols.FirstOrDefault())
                If symbol?.IsKind(SymbolKind.Method) Then
                    Dim [addressOf] As VBS.UnaryExpressionSyntax = Factory.AddressOfExpression(name)
                    Dim formalParameterTypeOrNull As VB.VisualBasicSyntaxNode = Me.GetFormalParameterTypeOrNull(originalName)

                    If formalParameterTypeOrNull IsNot Nothing Then
                        Return Factory.ObjectCreationExpression(CType(formalParameterTypeOrNull, VBS.TypeSyntax)) _
                                    .WithArgumentList(ExpressionSyntaxExtensions.CreateArgList([addressOf]))
                    End If

                    Return [addressOf]
                End If
                Return name
            End Function

            Private Function GetFormalParameterTypeOrNull(originalName As CSS.ExpressionSyntax) As VBS.TypeSyntax
                Return Me.GetOverloadedFormalParameterTypeOrNull(originalName)
            End Function

            Public Overrides Function VisitAliasQualifiedName(node As CSS.AliasQualifiedNameSyntax) As VB.VisualBasicSyntaxNode
                If node Is Nothing Then
                    Throw New ArgumentNullException(NameOf(node))
                End If
                Return Me.WrapTypedNameIfNecessary(Factory.QualifiedName(DirectCast(node.Alias.Accept(Me), VBS.NameSyntax), DirectCast(node.Name.Accept(Me), VBS.SimpleNameSyntax)), node).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitGenericName(node As CSS.GenericNameSyntax) As VB.VisualBasicSyntaxNode
                If node Is Nothing Then
                    Throw New ArgumentNullException(NameOf(node))
                End If

                Dim typeArguments As VBS.TypeArgumentListSyntax = DirectCast(node.TypeArgumentList.Accept(Me), VBS.TypeArgumentListSyntax)
                If node.Parent.IsKind(CS.SyntaxKind.MethodDeclaration) Then
                    If CType(node.Parent, CSS.MethodDeclarationSyntax).ReturnType.Equals(node) Then
                        Return Me.WrapTypedNameIfNecessary(Factory.GenericName(GenerateSafeVBToken(node.Identifier, node, _semanticModel, _usedIdentifiers, IsQualifiedName:=False, IsTypeName:=True).WithTrailingTrivia, typeArguments), node)
                    End If
                End If
                If node.Parent.IsKind(CS.SyntaxKind.ObjectCreationExpression) Then
                    Return Me.WrapTypedNameIfNecessary(Factory.GenericName(GenerateSafeVBToken(node.Identifier, node, _semanticModel, _usedIdentifiers, IsQualifiedName:=False, IsTypeName:=True).WithTrailingTrivia, typeArguments), node)
                End If
                Return Me.WrapTypedNameIfNecessary(Factory.GenericName(GenerateSafeVBToken(node.Identifier, node, _usedIdentifiers, _semanticModel).WithTrailingTrivia, typeArguments), node)
            End Function

            Public Overrides Function VisitIdentifierName(node As CSS.IdentifierNameSyntax) As VB.VisualBasicSyntaxNode
                If node Is Nothing Then
                    Throw New ArgumentNullException(NameOf(node))
                End If

                Dim originalNameParent As SyntaxNode = node.Parent
                If TypeOf originalNameParent Is CSS.MemberAccessExpressionSyntax OrElse
                    TypeOf originalNameParent Is CSS.MemberBindingExpressionSyntax OrElse
                    originalNameParent.IsKind(CS.SyntaxKind.AsExpression) OrElse
                    originalNameParent.IsKind(CS.SyntaxKind.Argument) OrElse
                    (TypeOf originalNameParent Is CSS.NameEqualsSyntax AndAlso TypeOf originalNameParent.Parent Is CSS.AnonymousObjectMemberDeclaratorSyntax) Then
                    If TypeOf originalNameParent Is CSS.MemberAccessExpressionSyntax Then
                        Dim parentAsMemberAccessExpr As CSS.MemberAccessExpressionSyntax = DirectCast(originalNameParent, CSS.MemberAccessExpressionSyntax)
                        If parentAsMemberAccessExpr.Expression.IsKind(CS.SyntaxKind.IdentifierName) Then
                            Dim identifierExpr As CSS.IdentifierNameSyntax = DirectCast(parentAsMemberAccessExpr.Expression, CSS.IdentifierNameSyntax)
                            If identifierExpr.Identifier.ToString = node.Identifier.ToString Then
                                Return Me.WrapTypedNameIfNecessary(Factory.IdentifierName(GenerateSafeVBToken(node.Identifier, node, _usedIdentifiers, _semanticModel)), node)
                            End If
                        End If
                    End If
                    If TypeOf originalNameParent Is CSS.ArgumentSyntax Then
                        If VB.SyntaxFacts.IsKeywordKind(VB.SyntaxFacts.GetKeywordKind(node.Identifier.ValueText)) Then
                            Return Factory.ParseName(MakeVBSafeName(node.Identifier.ValueText))
                        End If
                        If TypeOf CType(originalNameParent, CSS.ArgumentSyntax).Expression Is CSS.IdentifierNameSyntax Then
                            If CType(CType(originalNameParent, CSS.ArgumentSyntax).Expression, CSS.IdentifierNameSyntax).Identifier.Span.Equals(node.Identifier.Span) Then
                                Return Me.WrapTypedNameIfNecessary(Factory.IdentifierName(GenerateSafeVBToken(node.Identifier, node, _semanticModel, _usedIdentifiers, IsQualifiedName:=False, IsTypeName:=False)), node)
                            End If
                        End If
                    End If
                    Return Me.WrapTypedNameIfNecessary(Factory.IdentifierName(GenerateSafeVBToken(node.Identifier, node, _semanticModel, _usedIdentifiers, IsQualifiedName:=True, IsTypeName:=False)), node)
                End If
                If originalNameParent.IsKind(CS.SyntaxKind.SimpleAssignmentExpression) Then
                    Dim assignmentStmt As CSS.AssignmentExpressionSyntax = CType(originalNameParent, CSS.AssignmentExpressionSyntax)
                    If node.ToString.Equals(assignmentStmt.Left.ToString, StringComparison.Ordinal) AndAlso assignmentStmt.Left.ToString.Equals(assignmentStmt.Right.ToString, StringComparison.OrdinalIgnoreCase) Then
                        If originalNameParent.IsParentKind(CS.SyntaxKind.ObjectInitializerExpression) Then
                            Dim name As VBS.IdentifierNameSyntax = Factory.IdentifierName(GenerateSafeVBToken(node.Identifier, node, _semanticModel, _usedIdentifiers, IsQualifiedName:=True, IsTypeName:=True))
                            Return name
                        End If
                        If node.Ancestors().OfType(Of CSS.ConstructorDeclarationSyntax).Any Then
                            Dim name As VBS.IdentifierNameSyntax = Factory.IdentifierName(GenerateSafeVBToken(node.Identifier, node, _semanticModel, _usedIdentifiers, IsQualifiedName:=True, IsTypeName:=False))
                            If node.Parent.IsParentKind(CS.SyntaxKind.ObjectInitializerExpression) Then
                                Return name
                            End If
                            Return Factory.SimpleMemberAccessExpression(Factory.MeExpression.WithLeadingTrivia(name.GetLeadingTrivia), name.WithoutLeadingTrivia)
                        End If
                    End If
                End If
                If TypeOf originalNameParent Is CSS.DeclarationExpressionSyntax Then
                    If node.ToString = "var" Then
                        Return PredefinedTypeObject
                    End If
                    Return ConvertToType(node.ToString)
                End If
                If (TypeOf originalNameParent Is CSS.VariableDeclarationSyntax OrElse TypeOf originalNameParent Is CSS.ArrayTypeSyntax) AndAlso node.Identifier.ValueText = "dynamic" Then
                    Return PredefinedTypeObject
                End If
                If TypeOf originalNameParent Is CSS.ParameterSyntax AndAlso node.ToString = "Variant" Then
                    Return PredefinedTypeObject
                End If
                If node.AncestorsAndSelf().OfType(Of CSS.UsingDirectiveSyntax).FirstOrDefault.IsKind(CS.SyntaxKind.UsingDirective) OrElse
                    originalNameParent.IsKind(CS.SyntaxKind.TypeArgumentList, CS.SyntaxKind.SimpleBaseType) Then
                    'Return Me.WrapTypedNameIfNecessary(Factory.IdentifierName(GenerateSafeVBToken(node.identifier, node, _mSemanticModel, IsQualifiedName:=True, IsTypeName:=True)), node)
                    Return Factory.IdentifierName(GenerateSafeVBToken(node.Identifier, node, _semanticModel, _usedIdentifiers, IsQualifiedName:=True, IsTypeName:=True))
                End If
                If TypeOf originalNameParent Is CSS.ArrayTypeSyntax Then
                    Return Me.WrapTypedNameIfNecessary(Factory.IdentifierName(GenerateSafeVBToken(node.Identifier, node, _semanticModel, _usedIdentifiers, IsQualifiedName:=False, IsTypeName:=True)), node)
                End If
                ' The trivial on node reflects the wrong place on the file as order is switched so don't convert trivia here
                If TypeOf originalNameParent Is CSS.AliasQualifiedNameSyntax Then
                    If node.Identifier.ValueText = "global" Then
                        Dim cSharpNode As CS.CSharpSyntaxNode = GetStatementwithIssues(node, ReportErrors:=False)
                        If cSharpNode IsNot Nothing AndAlso cSharpNode.IsKind(CS.SyntaxKind.UsingDirective) Then
                            Return Factory.ParseExpression("[Global]")
                        Else
                            Return Me.WrapTypedNameIfNecessary(Factory.ParseTypeName("Global"), node)
                        End If
                    End If
                End If
                If TypeOf originalNameParent Is CSS.NameColonSyntax Then
                    Return Me.WrapTypedNameIfNecessary(Factory.IdentifierName(GenerateSafeVBToken(node.Identifier, node, _semanticModel, _usedIdentifiers, IsQualifiedName:=True, IsTypeName:=True)), node)
                End If
                If TypeOf originalNameParent IsNot CSS.ExpressionSyntax Then
                    Dim keywordKind As VB.SyntaxKind = VB.SyntaxFacts.GetKeywordKind(node.Identifier.ValueText)
                    If VB.SyntaxFacts.IsPredefinedType(keywordKind) Then
                        Return Factory.PredefinedType(keywordKind.GetPredefinedType())
                    End If
                End If
                If TypeOf originalNameParent Is CSS.QualifiedNameSyntax Then
                    Return Factory.IdentifierName(GenerateSafeVBToken(node.Identifier, node, _semanticModel, _usedIdentifiers, originalNameParent.IsKind(CS.SyntaxKind.QualifiedName), IsTypeName:=TypeOf originalNameParent Is CSS.InvocationExpressionSyntax))
                End If
                Return Me.WrapTypedNameIfNecessary(Factory.IdentifierName(GenerateSafeVBToken(node.Identifier, node, _semanticModel, _usedIdentifiers, originalNameParent.IsKind(CS.SyntaxKind.QualifiedName), IsTypeName:=TypeOf originalNameParent Is CSS.InvocationExpressionSyntax)), node)
            End Function

            Public Overrides Function VisitQualifiedName(node As CSS.QualifiedNameSyntax) As VB.VisualBasicSyntaxNode
                If node Is Nothing Then
                    Throw New ArgumentNullException(NameOf(node))
                End If
                Dim left As VB.VisualBasicSyntaxNode = node.Left.Accept(Me)
                Dim right As VB.VisualBasicSyntaxNode = node.Right.Accept(Me)
                If node.AncestorsAndSelf().OfType(Of CSS.UsingDirectiveSyntax).Any Then
                    Return Factory.QualifiedName(DirectCast(left, VBS.NameSyntax), CType(right, VBS.SimpleNameSyntax))
                End If
                If left.ToString = "System" Then
                    If TypeOf right Is VBS.NameSyntax Then
                        Return Me.WrapTypedNameIfNecessary(CType(right, VBS.ExpressionSyntax), node)
                    Else
                        Return right
                    End If
                End If
                Return Me.WrapTypedNameIfNecessary(Factory.QualifiedName(DirectCast(left, VBS.NameSyntax), CType(GetTypeSyntaxFromPossibleAddressOf(right), VBS.SimpleNameSyntax)), node)
            End Function

        End Class

    End Class

End Namespace
