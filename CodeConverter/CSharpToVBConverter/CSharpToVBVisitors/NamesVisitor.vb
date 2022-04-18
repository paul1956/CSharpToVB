' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

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

            Private Shared Function GetOverloadedFormalParameterTypeOrNull(model As SemanticModel, argumentChildExpression As CSS.ExpressionSyntax) As VBS.TypeSyntax
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
                Dim symbolInfo As SymbolInfo = model.GetSymbolInfo(ies.Expression)
                Dim destinationType As ISymbol = symbolInfo.ExtractBestMatch(Of ISymbol)(Function(m) m.GetParameters().Length > argIndex)
                If destinationType?.GetParameters.Any Then
                    Dim symbolType As ITypeSymbol = destinationType.GetParameters(argIndex).Type
                    Dim nameSyntax As VBS.NameSyntax = symbolType.GetFullyQualifiedNameSyntax()
                    Return nameSyntax
                End If

                Return Nothing
            End Function

            Private Shared Function GetScopingBlock(node As CS.CSharpSyntaxNode) As CS.CSharpSyntaxNode
                Dim blockNode As CS.CSharpSyntaxNode = node
                While blockNode IsNot Nothing

                    If TypeOf blockNode Is CSS.BlockSyntax Then
                        Return CType(blockNode.Parent, CS.CSharpSyntaxNode)
                    End If
                    If TypeOf blockNode Is CSS.EventDeclarationSyntax Then
                        Return blockNode
                    End If

                    If TypeOf blockNode Is CSS.NamespaceDeclarationSyntax Then
                        Return blockNode
                    End If
                    If TypeOf blockNode Is CSS.PropertyDeclarationSyntax Then
                        Return blockNode
                    End If
                    If TypeOf blockNode Is CSS.MethodDeclarationSyntax Then
                        Return blockNode
                    End If

                    If TypeOf blockNode Is CSS.ClassDeclarationSyntax Then
                        Return blockNode
                    End If

                    If TypeOf blockNode Is CSS.ConversionOperatorDeclarationSyntax Then
                        Return blockNode
                    End If
                    If TypeOf blockNode Is CSS.ConstructorDeclarationSyntax Then
                        Return blockNode
                    End If
                    If TypeOf blockNode Is CSS.EnumDeclarationSyntax Then
                        Return blockNode
                    End If

                    If TypeOf blockNode Is CSS.StructDeclarationSyntax Then
                        Return blockNode
                    End If

                    If TypeOf blockNode Is CSS.UsingDirectiveSyntax Then
                        Return blockNode
                    End If
                    If TypeOf blockNode Is CSS.UsingStatementSyntax Then
                        Return CType(blockNode.Parent, CS.CSharpSyntaxNode)
                    End If
                    blockNode = CType(blockNode.Parent, CS.CSharpSyntaxNode)
                End While
                Return GetStatementWithIssues(node)
            End Function

            Private Shared Function IsKind(kind As VB.SyntaxKind, ParamArray kinds() As VB.SyntaxKind) As Boolean
                Return kinds.Contains(kind)
            End Function

            Private Function GetFormalParameterTypeOrNull(originalName As CSS.ExpressionSyntax) As VBS.TypeSyntax
                Return GetOverloadedFormalParameterTypeOrNull(_semanticModel, originalName)
            End Function

            Private Function GetNewUniqueName(convertedIdentifier As String, usedIdentifiers As Dictionary(Of String, SymbolTableEntry), isType As Boolean, node As CS.CSharpSyntaxNode) As String
                If isType Then
                    Return convertedIdentifier
                End If

                convertedIdentifier = convertedIdentifier.RemoveBrackets

                Dim uniqueId As String = Me.GetUniqueVariableNameInScope(node, convertedIdentifier, usedIdentifiers)
                If VB.SyntaxFacts.GetKeywordKind(uniqueId) = VB.SyntaxKind.None Then
                    Return uniqueId
                End If

                Return $"[{uniqueId}]"
            End Function

            Private Function GetSymbolTableEntry(csIdentifier As SyntaxToken, baseVbIdent As String, node As CS.CSharpSyntaxNode, isQualifiedNameOrTypeName As Boolean, isField As Boolean) As (IdentToken As SyntaxToken, MeNeeded As Boolean)
                If _globalSymbols.Contains(csIdentifier.ToString(), compareTypeOnly:=True, StringComparison.Ordinal) Then
                    Return (Factory.Identifier(csIdentifier.ToString()), False)
                End If
                If _usedIdentifiers.ContainsKey(baseVbIdent) Then
                    Dim symbolTableEntry As SymbolTableEntry = _usedIdentifiers(baseVbIdent)
                    Return (Factory.Identifier(symbolTableEntry.Name).WithConvertedTriviaFrom(csIdentifier), symbolTableEntry.IsProperty)
                End If

                If isField AndAlso baseVbIdent.Equals("value", StringComparison.InvariantCulture) Then
                    _usedIdentifiers.Add(baseVbIdent, New SymbolTableEntry("value", isQualifiedNameOrTypeName, True))
                    Return (Factory.Identifier("value"), False)
                End If

                If _globalSymbols.Contains(baseVbIdent, compareTypeOnly:=True, StringComparison.OrdinalIgnoreCase) Then
                    ' We have a symbol we need to rename but _globalSymbol is not in SymbolTable
                    Dim globalSymbol As String = _globalSymbols.FindAll(baseVbIdent, compareTypeOnly:=True, StringComparison.OrdinalIgnoreCase).First()
                    _usedIdentifiers.Add(globalSymbol, New SymbolTableEntry(globalSymbol, isType:=True, isProperty:=False))
                    Dim uniqueName As String = Me.GetNewUniqueName(baseVbIdent, _usedIdentifiers, isQualifiedNameOrTypeName, GetScopingBlock(node))
                    _usedIdentifiers.Add(baseVbIdent, New SymbolTableEntry(uniqueName, isType:=True, isProperty:=False))
                    Return (Factory.Identifier(uniqueName).WithConvertedTriviaFrom(csIdentifier), MeNeeded:=False)
                Else
                    For Each ident As KeyValuePair(Of String, SymbolTableEntry) In _usedIdentifiers
                        If String.Compare(ident.Key, baseVbIdent, ignoreCase:=False, Globalization.CultureInfo.InvariantCulture) = 0 Then
                            ' We have an exact match stop looking
                            Return (Factory.Identifier(ident.Key), ident.Value.IsProperty)
                        End If
                        If String.Compare(ident.Key, baseVbIdent, ignoreCase:=True, Globalization.CultureInfo.InvariantCulture) = 0 Then
                            ' If we are here we have seen the variable in a different case so fix it
                            Dim uniqueName As String = Me.GetNewUniqueName(baseVbIdent, _usedIdentifiers, isQualifiedNameOrTypeName, GetScopingBlock(node))
                            If _usedIdentifiers(ident.Key).IsType Then
                                _usedIdentifiers.Add(baseVbIdent, New SymbolTableEntry(uniqueName, isType:=False, isField))
                            Else
                                _usedIdentifiers.Add(baseVbIdent, New SymbolTableEntry(uniqueName, isQualifiedNameOrTypeName, isField))
                            End If
                            Dim symbolTableEntry As SymbolTableEntry = _usedIdentifiers(baseVbIdent)
                            Return (Factory.Identifier(symbolTableEntry.Name).WithConvertedTriviaFrom(csIdentifier), symbolTableEntry.IsProperty)
                        End If
                    Next
                End If

                Dim newIdentifier As String = baseVbIdent
                If _globalSymbols.Contains(baseVbIdent, compareTypeOnly:=True, StringComparison.Ordinal) Then
                    newIdentifier &= "1"
                End If
                If baseVbIdent.Equals("value", StringComparison.InvariantCulture) Then
                    newIdentifier = "_" & newIdentifier
                End If
                _usedIdentifiers.Add(baseVbIdent, New SymbolTableEntry(newIdentifier, isQualifiedNameOrTypeName, isField))
                Return (Factory.Identifier(newIdentifier), False)
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
                   TypeOf originalNameParent.AncestorsAndSelf().OfType(Of CSS.AttributeSyntax).DefaultIfEmpty.First() Is CSS.AttributeSyntax Then
                    Return name
                End If

                Dim isTreeNotSame As Boolean = _semanticModel.SyntaxTree IsNot originalName.SyntaxTree
                If isTreeNotSame Then
                    Return name
                End If

                Dim symbolInfo As SymbolInfo
                symbolInfo = _semanticModel.GetSymbolInfo(originalName)
                Dim symbol As ISymbol = If(symbolInfo.Symbol, symbolInfo.CandidateSymbols.FirstOrDefault())
                If symbol?.IsKind(SymbolKind.Method) Then
                    Dim [addressOf] As VBS.UnaryExpressionSyntax = Factory.AddressOfExpression(name)
                    Dim formalParameterTypeOrNull As VB.VisualBasicSyntaxNode = Me.GetFormalParameterTypeOrNull(originalName)

                    If formalParameterTypeOrNull IsNot Nothing Then
                        Return Factory.ObjectCreationExpression(CType(formalParameterTypeOrNull, VBS.TypeSyntax)) _
                                    .WithArgumentList(CreateArgList([addressOf]))
                    End If

                    Return [addressOf]
                End If
                Return name
            End Function

            '    ' This need to be looked up using current reference
            '    ' For now just include Path from System.IO
            '    If baseVbIdent.Equals("path", StringComparison.Ordinal) Then
            '        newIdentifier &= "1"
            '    End If
            '    usedIdentifiers.Add(baseVbIdent, New SymbolTableEntry(newIdentifier, isQualifiedNameOrTypeName, isField))
            '    Return (Factory.Identifier(newIdentifier), False)
            'End Function
            Friend Function MakeIdentifierUnique(csIdentifier As SyntaxToken, node As CS.CSharpSyntaxNode, isBracketNeeded As Boolean, isQualifiedNameOrTypeName As Boolean) As SyntaxToken
                Dim isField As Boolean = (node.AncestorsAndSelf().OfType(Of CSS.FieldDeclarationSyntax).Any And Not isQualifiedNameOrTypeName) OrElse (node.AncestorsAndSelf().OfType(Of CSS.AccessorDeclarationSyntax).Any AndAlso csIdentifier.ValueText.Equals("value", StringComparison.InvariantCulture))
                Dim baseIdent As String = If(isBracketNeeded, $"[{csIdentifier.ValueText}]", csIdentifier.ValueText)
                If baseIdent = "_" Then
                    baseIdent = "__"
                End If
                ' Don't Change Qualified Names
                If isQualifiedNameOrTypeName Then
                    If Not _usedIdentifiers.ContainsKey(baseIdent) Then
                        _usedIdentifiers.Add(baseIdent, New SymbolTableEntry(baseIdent, isType:=True, isField))
                    End If
                    Return Factory.Identifier(baseIdent).WithConvertedTriviaFrom(csIdentifier)
                End If

                Return Me.GetSymbolTableEntry(csIdentifier, baseIdent, node, isQualifiedNameOrTypeName, isField).IdentToken
            End Function

            ''' <summary>
            ''' Returns Safe VB Name
            ''' </summary>
            ''' <param name="id">Original Variable Name</param>
            ''' <param name="node"></param>
            ''' <param name="isQualifiedName">True if name is part of a Qualified Name and should not be renamed</param>
            ''' <param name="isTypeName"></param>
            ''' <returns></returns>
            Public Function GenerateSafeVbToken(id As SyntaxToken, node As CS.CSharpSyntaxNode, isQualifiedName As Boolean, isTypeName As Boolean) As SyntaxToken
                If node Is Nothing Then
                    Throw New ArgumentNullException(NameOf(node))
                End If
                Dim keywordKind As VB.SyntaxKind = VB.SyntaxFacts.GetKeywordKind(id.ValueText)
                If isTypeName Then
                    isQualifiedName = True
                Else
                    If VB.SyntaxFacts.IsPredefinedType(keywordKind) Then
                        Return Me.MakeIdentifierUnique(id, node, isBracketNeeded:=True, isQualifiedNameOrTypeName:=isQualifiedName)
                    End If
                End If
                ' TODO Workaround till we get a list of special variables
                If id.Text().Equals("RECT", StringComparison.Ordinal) Then
                    Return Factory.Identifier("RECT_Renamed")
                End If
                Dim idParent As SyntaxNode = id.Parent
                If VB.SyntaxFacts.IsKeywordKind(keywordKind) OrElse id.ValueText = "Yield" Then
                    Dim bracketNeeded As Boolean = True
                    If idParent Is Nothing OrElse
                    IsKind(keywordKind, VB.SyntaxKind.REMKeyword, VB.SyntaxKind.DelegateKeyword) OrElse
                    id.Text.Chars(0) = "@" Then
                        ' Do nothing
                    ElseIf TypeOf idParent.Parent Is CSS.MemberAccessExpressionSyntax Then
                        Dim memberAccessExpression As CSS.MemberAccessExpressionSyntax = CType(idParent?.Parent, CSS.MemberAccessExpressionSyntax)
                        If TypeOf memberAccessExpression.Expression IsNot CSS.GenericNameSyntax Then
                            bracketNeeded = memberAccessExpression.Expression.ToString.Equals(id.ToString, StringComparison.Ordinal)
                        End If
                        ' ReSharper disable once VBUseFirstInstead
                    ElseIf idParent.AncestorsAndSelf().OfType(Of CSS.UsingDirectiveSyntax).FirstOrDefault().IsKind(CS.SyntaxKind.UsingDirective) Then
                        id = Factory.Token(keywordKind).WithTriviaFrom(id)
                        bracketNeeded = False
                    End If
                    Return Me.MakeIdentifierUnique(id, node, bracketNeeded, isQualifiedNameOrTypeName:=isQualifiedName)
                End If
                If (Not isQualifiedName) AndAlso idParent?.IsParentKind(CS.SyntaxKind.Parameter) Then
                    Dim param As CSS.ParameterSyntax = DirectCast(idParent.Parent, CSS.ParameterSyntax)
                    Dim methodDeclaration As CSS.MethodDeclarationSyntax = TryCast(param.Parent?.Parent, CSS.MethodDeclarationSyntax)
                    isQualifiedName = methodDeclaration Is Nothing OrElse
                                      String.Compare(methodDeclaration.Identifier.ValueText, id.ValueText, ignoreCase:=True, Globalization.CultureInfo.InvariantCulture) = 0 OrElse
                                      String.Compare(param.Type.ToString, id.ValueText, ignoreCase:=False,
                                      Globalization.CultureInfo.InvariantCulture) = 0
                End If
                Return Me.MakeIdentifierUnique(id, node, isBracketNeeded:=False, isQualifiedNameOrTypeName:=isQualifiedName)
            End Function

            ''' <summary>
            ''' Returns Safe VB Name with QualifiedName and TypeName both false
            ''' </summary>
            ''' <param name="id"></param>
            ''' <returns></returns>
            ''' <param name="node"></param>
            Public Function GenerateSafeVbToken(id As SyntaxToken, node As CS.CSharpSyntaxNode) As SyntaxToken
                Return Me.GenerateSafeVbToken(id, node, isQualifiedName:=False, isTypeName:=False)
            End Function

            'Private Function GetSymbolTableEntry(csIdentifier As SyntaxToken, baseVbIdent As String, usedIdentifiers As Dictionary(Of String, SymbolTableEntry), node As CS.CSharpSyntaxNode, model As SemanticModel, isQualifiedNameOrTypeName As Boolean, isField As Boolean) As (IdentToken As SyntaxToken, MeNeeded As Boolean)
            '    ' TODO Workaround till we get a list of special variables
            '    If csIdentifier.ToString().Equals("Path", StringComparison.Ordinal) Then
            '        Return (Factory.Identifier("Path"), False)
            '    End If
            '    If usedIdentifiers.ContainsKey(baseVbIdent) Then
            '        Dim symbolTableEntry As SymbolTableEntry = usedIdentifiers(baseVbIdent)
            '        Return (Factory.Identifier(symbolTableEntry.Name).WithConvertedTriviaFrom(csIdentifier), symbolTableEntry.IsProperty)
            '    End If
            '    For Each ident As KeyValuePair(Of String, SymbolTableEntry) In usedIdentifiers
            '        If String.Compare(ident.Key, baseVbIdent, ignoreCase:=False, Globalization.CultureInfo.InvariantCulture) = 0 Then
            '            ' We have an exact match keep looking
            '            Return (Factory.Identifier(ident.Key), ident.Value.IsProperty)
            '        End If
            '        If String.Compare(ident.Key, baseVbIdent, ignoreCase:=True, Globalization.CultureInfo.InvariantCulture) = 0 Then
            '            ' If we are here we have seen the variable in a different case so fix it
            '            Dim uniqueName As String = baseVbIdent.GetNewUniqueName(usedIdentifiers, isQualifiedNameOrTypeName, node.GetScopingBlock, model)
            '            If usedIdentifiers(ident.Key).IsType Then
            '                usedIdentifiers.Add(baseVbIdent, New SymbolTableEntry(uniqueName, isType:=False, isField))
            '            Else
            '                usedIdentifiers.Add(baseVbIdent, New SymbolTableEntry(uniqueName, isQualifiedNameOrTypeName, isField))
            '            End If
            '            Dim symbolTableEntry As SymbolTableEntry = usedIdentifiers(baseVbIdent)
            '            Return (Factory.Identifier(symbolTableEntry.Name).WithConvertedTriviaFrom(csIdentifier), symbolTableEntry.IsProperty)
            '        End If
            '    Next
            '    Dim newIdentifier As String = baseVbIdent
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
                        Return Me.WrapTypedNameIfNecessary(Factory.GenericName(Me.GenerateSafeVbToken(node.Identifier, node, isQualifiedName:=False, isTypeName:=True).WithTrailingTrivia, typeArguments), node)
                    End If
                End If
                If node.Parent.IsKind(CS.SyntaxKind.ObjectCreationExpression) Then
                    Return Me.WrapTypedNameIfNecessary(Factory.GenericName(Me.GenerateSafeVbToken(node.Identifier, node, isQualifiedName:=False, isTypeName:=True).WithTrailingTrivia, typeArguments), node)
                End If
                Return Me.WrapTypedNameIfNecessary(Factory.GenericName(Me.GenerateSafeVbToken(node.Identifier, node).WithTrailingTrivia, typeArguments), node)
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
                   (TypeOf originalNameParent Is CSS.NameEqualsSyntax AndAlso
                    TypeOf originalNameParent.Parent Is CSS.AnonymousObjectMemberDeclaratorSyntax) Then
                    Dim parentAsMemberAccessExpr As CSS.MemberAccessExpressionSyntax = TryCast(originalNameParent, CSS.MemberAccessExpressionSyntax)
                    If parentAsMemberAccessExpr IsNot Nothing Then
                        If parentAsMemberAccessExpr.Expression.IsKind(CS.SyntaxKind.IdentifierName) Then
                            Dim identifierExpr As CSS.IdentifierNameSyntax =
                                    DirectCast(parentAsMemberAccessExpr.Expression, CSS.IdentifierNameSyntax)
                            If identifierExpr.Identifier.ToString = node.Identifier.ToString Then
                                Dim isQualifiedName As Boolean = Not _usedIdentifiers.ContainsKey(node.Identifier.ToString())
                                Return Me.WrapTypedNameIfNecessary(
                                        Factory.IdentifierName(Me.GenerateSafeVbToken(node.Identifier,
                                            node,
                                            isQualifiedName:=isQualifiedName,
                                            isTypeName:=isQualifiedName)
                                                                   ), node)
                            End If
                        End If
                    End If

                    Dim argument As CSS.ArgumentSyntax = TryCast(originalNameParent, CSS.ArgumentSyntax)
                    If argument IsNot Nothing Then
                        If VB.SyntaxFacts.IsKeywordKind(VB.SyntaxFacts.GetKeywordKind(node.Identifier.ValueText)) Then
                            Return Factory.ParseName(MakeVbSafeName(node.Identifier.ValueText))
                        End If
                        Dim identifierName As CSS.IdentifierNameSyntax = TryCast(argument.Expression, CSS.IdentifierNameSyntax)
                        If identifierName IsNot Nothing Then
                            If identifierName.Identifier.Span.Equals(node.Identifier.Span) Then
                                Return Me.WrapTypedNameIfNecessary(Factory.IdentifierName(Me.GenerateSafeVbToken(node.Identifier, node, isQualifiedName:=False, isTypeName:=False)), node)
                            End If
                        End If
                    End If
                    Return Me.WrapTypedNameIfNecessary(Factory.IdentifierName(Me.GenerateSafeVbToken(node.Identifier, node, isQualifiedName:=True, isTypeName:=False)), node)
                End If
                If originalNameParent.IsKind(CS.SyntaxKind.SimpleAssignmentExpression) Then
                    Dim assignmentStmt As CSS.AssignmentExpressionSyntax = CType(originalNameParent, CSS.AssignmentExpressionSyntax)
                    If node.ToString.Equals(assignmentStmt.Left.ToString, StringComparison.Ordinal) AndAlso assignmentStmt.Left.ToString.Equals(assignmentStmt.Right.ToString, StringComparison.OrdinalIgnoreCase) Then
                        If originalNameParent.IsParentKind(CS.SyntaxKind.ObjectInitializerExpression) Then
                            Dim name As VBS.IdentifierNameSyntax = Factory.IdentifierName(Me.GenerateSafeVbToken(node.Identifier, node, isQualifiedName:=True, isTypeName:=True))
                            Return name
                        End If
                        If node.Ancestors().OfType(Of CSS.ConstructorDeclarationSyntax).Any Then
                            Dim name As VBS.IdentifierNameSyntax = Factory.IdentifierName(Me.GenerateSafeVbToken(node.Identifier, node, isQualifiedName:=True, isTypeName:=False))
                            If node.Parent.IsParentKind(CS.SyntaxKind.ObjectInitializerExpression) Then
                                Return name
                            End If
                            Return Factory.SimpleMemberAccessExpression(MeExpression.WithLeadingTrivia(name.GetLeadingTrivia), name.WithoutLeadingTrivia)
                        End If
                    End If
                End If
                Dim variableDeclaration As CSS.VariableDeclarationSyntax = TryCast(originalNameParent, CSS.VariableDeclarationSyntax)
                If variableDeclaration IsNot Nothing Then
                    If variableDeclaration.Type.ToString().Equals(node.ToString(), StringComparison.Ordinal) Then
                        Return Me.WrapTypedNameIfNecessary(
                            Factory.IdentifierName(Me.GenerateSafeVbToken(node.Identifier,
                                node,
                                isQualifiedName:=False,
                                isTypeName:=True)
                                                                      ), node)

                    End If
                End If
                If TypeOf originalNameParent Is CSS.DeclarationExpressionSyntax Then
                    If node.ToString = "var" Then
                        Return PredefinedTypeObject
                    End If
                    Return ConvertToType(node.ToString)
                End If
                If node.Identifier.ValueText = "dynamic" Then
                    If TypeOf originalNameParent Is CSS.ArrayTypeSyntax Then
                        Return PredefinedTypeObject
                    End If
                    Dim parent As CSS.VariableDeclarationSyntax = TryCast(originalNameParent, CSS.VariableDeclarationSyntax)
                    If parent IsNot Nothing AndAlso parent.Variables.Count > 0 Then
                        If parent.Variables(0).Initializer Is Nothing Then
                            Return PredefinedTypeObject
                        Else
                            Dim type As (_Error As Boolean, _ITypeSymbol As VBS.TypeSyntax) = parent.Variables(0).Initializer.Value.DetermineTypeSyntax(_semanticModel)
                            If type._Error Then
                                Return PredefinedTypeObject
                            End If
                            Return type._ITypeSymbol
                        End If
                    End If
                End If
                If TypeOf originalNameParent Is CSS.ParameterSyntax AndAlso node.ToString = "Variant" Then
                    Return PredefinedTypeObject
                End If
                If node.AncestorsAndSelf().OfType(Of CSS.UsingDirectiveSyntax).FirstOrDefault.IsKind(CS.SyntaxKind.UsingDirective) OrElse
                    originalNameParent.IsKind(CS.SyntaxKind.TypeArgumentList, CS.SyntaxKind.SimpleBaseType) Then
                    'Return Me.WrapTypedNameIfNecessary(Factory.IdentifierName(GenerateSafeVBToken(node.identifier, node, _mSemanticModel, IsQualifiedName:=True, IsTypeName:=True)), node)
                    Return Factory.IdentifierName(Me.GenerateSafeVbToken(node.Identifier, node, isQualifiedName:=True, isTypeName:=True))
                End If
                If TypeOf originalNameParent Is CSS.ArrayTypeSyntax Then
                    Return Me.WrapTypedNameIfNecessary(Factory.IdentifierName(Me.GenerateSafeVbToken(node.Identifier, node, isQualifiedName:=False, isTypeName:=True)), node)
                End If
                ' The trivial on node reflects the wrong place on the file as order is switched so don't convert trivia here
                If TypeOf originalNameParent Is CSS.AliasQualifiedNameSyntax Then
                    If node.Identifier.ValueText = "global" Then
                        Dim cSharpNode As CS.CSharpSyntaxNode = GetStatementWithIssues(node, reportErrors:=False)
                        If cSharpNode IsNot Nothing AndAlso cSharpNode.IsKind(CS.SyntaxKind.UsingDirective) Then
                            Return Factory.ParseExpression("[Global]")
                        Else
                            Return Me.WrapTypedNameIfNecessary(Factory.ParseTypeName("Global"), node)
                        End If
                    End If
                End If
                If TypeOf originalNameParent Is CSS.NameColonSyntax Then
                    Return Me.WrapTypedNameIfNecessary(Factory.IdentifierName(Me.GenerateSafeVbToken(node.Identifier, node, isQualifiedName:=True, isTypeName:=True)), node)
                End If
                If TypeOf originalNameParent IsNot CSS.ExpressionSyntax Then
                    Dim keywordKind As VB.SyntaxKind = VB.SyntaxFacts.GetKeywordKind(node.Identifier.ValueText)
                    If VB.SyntaxFacts.IsPredefinedType(keywordKind) Then
                        Return Factory.PredefinedType(keywordKind.GetPredefinedType())
                    End If
                End If
                If TypeOf originalNameParent Is CSS.QualifiedNameSyntax Then
                    Return Factory.IdentifierName(Me.GenerateSafeVbToken(node.Identifier, node, originalNameParent.IsKind(CS.SyntaxKind.QualifiedName), isTypeName:=TypeOf originalNameParent Is CSS.InvocationExpressionSyntax))
                End If
                Return Me.WrapTypedNameIfNecessary(Factory.IdentifierName(Me.GenerateSafeVbToken(node.Identifier, node, originalNameParent.IsKind(CS.SyntaxKind.QualifiedName), isTypeName:=TypeOf originalNameParent Is CSS.InvocationExpressionSyntax)), node)
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
                If left.ToString = "System" AndAlso Not _originalRequest.IsProjectConversion Then
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
