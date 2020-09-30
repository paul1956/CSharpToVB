' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Collections.Immutable
Imports System.Runtime.InteropServices
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

            Private ReadOnly _addedNames As New HashSet(Of String)

            Private Shared Function CreateImplementsClauseSyntax(implementors As IEnumerable(Of ISymbol), id As SyntaxToken) As VBS.ImplementsClauseSyntax
                Return Factory.ImplementsClause(implementors.Select(Function(x)
                                                                        Dim FullyQualifiedName As VBS.NameSyntax = GetFullyQualifiedNameSyntax(TryCast(x.ContainingSymbol, INamedTypeSymbol))
                                                                        If TypeOf FullyQualifiedName Is VBS.QualifiedNameSyntax Then
                                                                            Dim left As String = CType(FullyQualifiedName, VBS.QualifiedNameSyntax).Left.ToString
                                                                            If left.StartsWith("", StringComparison.Ordinal) Then
                                                                                left = left.WithoutLeadingSystemDot
                                                                                If left.Any Then
                                                                                    FullyQualifiedName = Factory.QualifiedName(Factory.IdentifierName(left), CType(FullyQualifiedName, VBS.QualifiedNameSyntax).Right)
                                                                                End If
                                                                            End If
                                                                        End If
                                                                        Return Factory.QualifiedName(FullyQualifiedName, Factory.IdentifierName(id))
                                                                    End Function).ToArray())
            End Function

            Private Shared Function UndottedMemberName(n As String) As String
                Return n.Split("."c).Last().RemoveBrackets
            End Function

            Private Function ConvertAccessor(node As CSS.AccessorDeclarationSyntax, IsModule As Boolean, ByRef isIterator As Boolean) As VBS.AccessorBlockSyntax
                Dim blockKind As VB.SyntaxKind
                Dim stmt As VBS.AccessorStatementSyntax
                Dim body As SyntaxList(Of VBS.StatementSyntax) = Factory.List(Of VBS.StatementSyntax)()
                isIterator = False
                Dim visitor As MethodBodyVisitor = New MethodBodyVisitor(_mSemanticModel, Me)
                If node.Body IsNot Nothing Then
                    body = node.Body.GetBodyStatements(visitor)
                    isIterator = visitor.IsInterator
                ElseIf node.ExpressionBody IsNot Nothing Then
                    body = node.ExpressionBody.GetExpressionBodyStatements(Me)
                End If
                Dim Attributes As SyntaxList(Of VBS.AttributeListSyntax) = Factory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))
                Dim Modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, IsModule, TokenContext.Local).ToList
                Dim Parent As CSS.BasePropertyDeclarationSyntax = DirectCast(node.Parent.Parent, CSS.BasePropertyDeclarationSyntax)
                Dim ValueParam As VBS.ParameterSyntax
                Dim endStmt As VBS.EndBlockStatementSyntax
                Select Case CS.CSharpExtensions.Kind(node)
                    Case CS.SyntaxKind.GetAccessorDeclaration
                        blockKind = VB.SyntaxKind.GetAccessorBlock
                        stmt = Factory.GetAccessorStatement(Attributes, Factory.TokenList(Modifiers), parameterList:=Nothing)
                        endStmt = Factory.EndGetStatement(EndKeyword.WithTrailingTrivia(VBSpaceTrivia), GetKeyword)
                    Case CS.SyntaxKind.SetAccessorDeclaration
                        blockKind = VB.SyntaxKind.SetAccessorBlock
                        ValueParam = Factory.Parameter(ValueModifiedIdentifier).
                            WithAsClause(Factory.SimpleAsClause(DirectCast(Parent.Type.Accept(Me), VBS.TypeSyntax).
                            WithLeadingTrivia(VBSpaceTrivia)))
                        stmt = Factory.SetAccessorStatement(Attributes, Factory.TokenList(Modifiers), Factory.ParameterList(Factory.SingletonSeparatedList(ValueParam)))
                        endStmt = Factory.EndSetStatement(EndKeyword.WithTrailingTrivia(VBSpaceTrivia), SetKeyword)
                    Case CS.SyntaxKind.AddAccessorDeclaration
                        blockKind = VB.SyntaxKind.AddHandlerAccessorBlock
                        ValueParam = Factory.Parameter(ValueModifiedIdentifier).
                            WithAsClause(Factory.SimpleAsClause(DirectCast(Parent.Type.Accept(Me), VBS.TypeSyntax)))
                        stmt = Factory.AddHandlerAccessorStatement(Attributes, Factory.TokenList(Modifiers), Factory.ParameterList(Factory.SingletonSeparatedList(ValueParam)))
                        endStmt = Factory.EndAddHandlerStatement(EndKeyword.WithTrailingTrivia(VBSpaceTrivia), AddHandlerKeyword)
                    Case CS.SyntaxKind.RemoveAccessorDeclaration
                        blockKind = VB.SyntaxKind.RemoveHandlerAccessorBlock
                        ValueParam = Factory.Parameter(ValueModifiedIdentifier).
                            WithAsClause(Factory.SimpleAsClause(DirectCast(Parent.Type.Accept(Me), VBS.TypeSyntax)))
                        stmt = Factory.RemoveHandlerAccessorStatement(Attributes, Factory.TokenList(Modifiers), Factory.ParameterList(Factory.SingletonSeparatedList(ValueParam)))
                        endStmt = Factory.EndRemoveHandlerStatement(EndKeyword.WithTrailingTrivia(VBSpaceTrivia), RemoveHandlerKeyword)
                    Case Else
                        Throw New NotSupportedException()
                End Select
                Return Factory.AccessorBlock(blockKind,
                                             stmt.WithConvertedTriviaFrom(node.Body.GetBraces.Item1).WithTrailingEOL,
                                             body,
                                             endStmt.WithConvertedTriviaFrom(node.Body.GetBraces.Item2)).WithConvertedTriviaFrom(node)
            End Function

            Private Sub ConvertAndSplitAttributes(attributeLists As SyntaxList(Of CSS.AttributeListSyntax), <Out> ByRef Attributes As List(Of VBS.AttributeListSyntax), <Out> ByRef ReturnAttributes As SyntaxList(Of VBS.AttributeListSyntax), ByRef FinalDirectiveTrivia As SyntaxTriviaList)
                Dim retAttr As List(Of VBS.AttributeListSyntax) = New List(Of VBS.AttributeListSyntax)()
                Dim FirstAttribuate As Boolean = True
                For Each e As IndexClass(Of CSS.AttributeListSyntax) In attributeLists.WithIndex
                    Dim attrList As CSS.AttributeListSyntax = e.Value
                    If attrList.Target Is Nothing OrElse Not attrList.Target.Identifier.IsKind(CS.SyntaxKind.ReturnKeyword) Then
                        Dim item As VBS.AttributeListSyntax = DirectCast(attrList.Accept(Me), VBS.AttributeListSyntax).RemoveExtraLeadingEOL
                        If FirstAttribuate Then
                            FirstAttribuate = False
                        Else
                            Dim itemLeadingTrivia As SyntaxTriviaList = item.GetLeadingTrivia
                            If itemLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                Dim newLeadingTrivia As New SyntaxTriviaList
                                Dim FirstComment As Boolean = True
                                Dim NeedWhiteSpace As Boolean = True
                                Dim needLineContinuation As Boolean = True
                                For Each VBSyntaxTrivia As SyntaxTrivia In itemLeadingTrivia
                                    Select Case VBSyntaxTrivia.RawKind
                                        Case VB.SyntaxKind.WhitespaceTrivia
                                            newLeadingTrivia = newLeadingTrivia.Add(VBSyntaxTrivia)
                                            NeedWhiteSpace = False
                                        Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                                            If FirstComment Then
                                                FirstComment = False
                                                If NeedWhiteSpace Then
                                                    NeedWhiteSpace = False
                                                    newLeadingTrivia = newLeadingTrivia.Add(VBSpaceTrivia)
                                                End If
                                                newLeadingTrivia = newLeadingTrivia.Add(LineContinuation)
                                                newLeadingTrivia = newLeadingTrivia.Add(VBSpaceTrivia)
                                                needLineContinuation = False
                                            End If
                                            newLeadingTrivia = newLeadingTrivia.Add(VBSyntaxTrivia)
                                        Case VB.SyntaxKind.IfDirectiveTrivia
                                            If FirstComment Then
                                                FirstComment = False
                                                If NeedWhiteSpace Then
                                                    NeedWhiteSpace = False
                                                    newLeadingTrivia = newLeadingTrivia.Add(VBSpaceTrivia)
                                                End If
                                                newLeadingTrivia = newLeadingTrivia.Add(LineContinuation)
                                                newLeadingTrivia = newLeadingTrivia.Add(VBSpaceTrivia)
                                                needLineContinuation = False
                                            End If
                                            newLeadingTrivia = newLeadingTrivia.Add(Factory.CommentTrivia($"Directive not allowed here {VBSyntaxTrivia}"))
                                        Case VB.SyntaxKind.ElseDirectiveTrivia
                                            If FirstComment Then
                                                FirstComment = False
                                                If NeedWhiteSpace Then
                                                    NeedWhiteSpace = False
                                                    newLeadingTrivia = newLeadingTrivia.Add(VBSpaceTrivia)
                                                End If
                                                newLeadingTrivia = newLeadingTrivia.Add(LineContinuation)
                                                newLeadingTrivia = newLeadingTrivia.Add(VBSpaceTrivia)
                                                needLineContinuation = False
                                            End If
                                            newLeadingTrivia = newLeadingTrivia.Add(Factory.CommentTrivia($"Directive not allowed here {VBSyntaxTrivia}"))
                                        Case VB.SyntaxKind.ElseIfDirectiveTrivia
                                            If FirstComment Then
                                                FirstComment = False
                                                If NeedWhiteSpace Then
                                                    NeedWhiteSpace = False
                                                    newLeadingTrivia = newLeadingTrivia.Add(VBSpaceTrivia)
                                                End If
                                                newLeadingTrivia = newLeadingTrivia.Add(LineContinuation)
                                                newLeadingTrivia = newLeadingTrivia.Add(VBSpaceTrivia)
                                                needLineContinuation = False
                                            End If
                                            newLeadingTrivia = newLeadingTrivia.Add(Factory.CommentTrivia($"Directive not allowed here {VBSyntaxTrivia}"))
                                        Case VB.SyntaxKind.EndIfDirectiveTrivia
                                            FinalDirectiveTrivia = FinalDirectiveTrivia.Add(VBSyntaxTrivia)
                                        Case VB.SyntaxKind.EndOfLineTrivia
                                            If NeedWhiteSpace Then
                                                NeedWhiteSpace = False
                                                newLeadingTrivia = newLeadingTrivia.Add(VBSpaceTrivia)
                                            End If
                                            If needLineContinuation Then
                                                newLeadingTrivia = newLeadingTrivia.Add(LineContinuation)
                                                needLineContinuation = False
                                                NeedWhiteSpace = True
                                            End If
                                            newLeadingTrivia = newLeadingTrivia.Add(VBSyntaxTrivia)
                                        Case Else
                                            Stop
                                            NeedWhiteSpace = True
                                    End Select
                                Next
                                item = item.WithLeadingTrivia(newLeadingTrivia)
                            End If
                        End If
                        Attributes.Add(item)
                    Else
                        ' Remove trailing CRLF from return attributes
                        retAttr.Add(DirectCast(attrList.Accept(Me).With({VBSpaceTrivia}, {VBSpaceTrivia}), VBS.AttributeListSyntax))
                    End If
                Next

                ReturnAttributes = Factory.List(retAttr)
            End Sub

            Private Function CreateImplementsClauseSyntaxOrNull(memberInfo As ISymbol, ByRef id As SyntaxToken) As VBS.ImplementsClauseSyntax
                Dim originalId As SyntaxToken = id
                Dim explicitImplementors As ImmutableArray(Of ISymbol) = memberInfo.ExplicitInterfaceImplementations()
                If explicitImplementors.Any() Then
                    Dim memberNames As ILookup(Of String, ISymbol) = memberInfo.ContainingType.GetMembers().ToLookup(Function(s) UndottedMemberName(s.Name), StringComparer.OrdinalIgnoreCase)
                    Dim explicitMemberName As String = UndottedMemberName(memberInfo.Name)
                    Dim hasDuplicateNames As Boolean = memberNames(explicitMemberName).Count() > 1
                    If hasDuplicateNames Then
                        id = Factory.Identifier(NameGenerator.GenerateUniqueName(explicitMemberName, Function(n) Not memberNames.Contains(n) AndAlso _addedNames.Add(n)))
                    End If
                Else
                    Dim containingType As INamedTypeSymbol = memberInfo.ContainingType
                    Dim baseClassesAndInterfaces As IEnumerable(Of INamedTypeSymbol) = containingType.GetAllBaseClassesAndInterfaces(True)
                    explicitImplementors = baseClassesAndInterfaces.Except({containingType}) _
                        .SelectMany(Function(t) t.GetMembers().Where(Function(m) memberInfo.Name.EndsWith(m.Name, StringComparison.Ordinal))) _
                        .Where(Function(m As ISymbol) As Boolean
                                   Dim bool? As Boolean = containingType.FindImplementationForInterfaceMember(m)?.Equals(memberInfo, SymbolEqualityComparer.Default) = True
                                   Return bool.HasValue AndAlso CBool(bool)
                               End Function) _
                        .ToImmutableArray()
                End If

                Return If(Not explicitImplementors.Any(), Nothing, CreateImplementsClauseSyntax(explicitImplementors, originalId))
            End Function

            Friend Shared Function GetFullyQualifiedNameSyntax(symbol As INamespaceOrTypeSymbol, Optional allowGlobalPrefix As Boolean = True) As VBS.NameSyntax
                Select Case True
                    Case TypeOf symbol Is ITypeSymbol
                        Dim typeSyntax As VBS.TypeSyntax = CType(symbol, ITypeSymbol).ConvertToType.GetElementType
                        If TypeOf typeSyntax Is VBS.PredefinedTypeSyntax Then
                            typeSyntax = Factory.IdentifierName($"[{symbol}]")
                        End If
                        If TypeOf typeSyntax Is VBS.NullableTypeSyntax Then
                            typeSyntax = CType(typeSyntax, VBS.NullableTypeSyntax).ElementType
                        End If
                        Dim nameSyntax1 As VBS.NameSyntax = CType(typeSyntax, VBS.NameSyntax)
                        If allowGlobalPrefix Then
                            Return nameSyntax1
                        End If
                        Dim globalNameNode As VBS.GlobalNameSyntax = nameSyntax1.DescendantNodes().OfType(Of VBS.GlobalNameSyntax)().FirstOrDefault()
                        If globalNameNode IsNot Nothing Then
                            nameSyntax1 = nameSyntax1.ReplaceNodes(TryCast(globalNameNode.Parent, VBS.QualifiedNameSyntax).Yield(), Function(orig, rewrite) orig.Right)
                        End If

                        Return nameSyntax1
                    Case TypeOf symbol Is INamespaceSymbol
                        Dim ns As INamespaceSymbol = CType(symbol, INamespaceSymbol)
                        Return Factory.ParseName(ns.GetFullMetadataName())
                    Case Else
                        Throw New NotImplementedException($"Fully qualified name for {symbol.[GetType]().FullName} not implemented")
                End Select
            End Function

            Public Overrides Function VisitAnonymousObjectMemberDeclarator(node As CSS.AnonymousObjectMemberDeclaratorSyntax) As VB.VisualBasicSyntaxNode
                If node?.NameEquals Is Nothing Then
                    Return Factory.InferredFieldInitializer(DirectCast(node.Expression.Accept(Me), VBS.ExpressionSyntax)).WithConvertedTriviaFrom(node)
                Else
                    Dim nameWithTrivia As VBS.IdentifierNameSyntax = DirectCast(node.NameEquals.Name.Accept(Me), VBS.IdentifierNameSyntax)
                    Return Factory.NamedFieldInitializer(KeyKeyword,
                                                           DotToken,
                                                           name:=nameWithTrivia.WithoutLeadingTrivia,
                                                           EqualsToken,
                                                           expression:=DirectCast(node.Expression.Accept(Me), VBS.ExpressionSyntax)
                                                           ).WithConvertedTriviaFrom(node).WithPrependedLeadingTrivia(nameWithTrivia.GetLeadingTrivia)
                End If
            End Function

            Public Overrides Function VisitArrowExpressionClause(node As CSS.ArrowExpressionClauseSyntax) As VB.VisualBasicSyntaxNode
                Return node?.Expression.Accept(Me)
            End Function

            Public Overrides Function VisitConstructorDeclaration(node As CSS.ConstructorDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim Attributes As SyntaxList(Of VBS.AttributeListSyntax) = Factory.List(
                        node?.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))
                Dim Initializer As VBS.ExpressionStatementSyntax = Nothing
                If node.Initializer IsNot Nothing Then
                    Initializer = DirectCast(node.Initializer.Accept(Me), VBS.ExpressionStatementSyntax)
                End If
                Dim vbStatements As New Dictionary(Of CSS.LocalFunctionStatementSyntax, VBS.StatementSyntax)
                For Each localFunction As CSS.LocalFunctionStatementSyntax In node.DescendantNodes().OfType(Of CSS.LocalFunctionStatementSyntax).ToList()
                    Dim replacementStatement As VBS.StatementSyntax = localFunction.Accept(New MethodBodyVisitor(_mSemanticModel, Me))(0)
                    If TypeOf replacementStatement IsNot VBS.EmptyStatementSyntax OrElse replacementStatement.ContainsCommentOrDirectiveTrivia Then
                        vbStatements.Add(localFunction, replacementStatement)
                    End If
                Next
                Dim Modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, Me.IsModule, TokenContext.New).ToList

                Dim parameterList As VBS.ParameterListSyntax = DirectCast(node.ParameterList?.Accept(Me), VBS.ParameterListSyntax)
                Dim SubNewStatement As VBS.StatementSyntax =
                    Factory.SubNewStatement(Attributes,
                                              Factory.TokenList(Modifiers),
                                              parameterList
                                              ).WithTrailingEOL.
                                              RestructureAttributesAndModifiers(Attributes.Any, Modifiers.Any)

                Dim EndSubStatement As VBS.EndBlockStatementSyntax = Factory.EndSubStatement(EndKeyword.WithTrailingTrivia(VBSpaceTrivia), SubKeyword).WithTrailingEOL
                Dim Body As New SyntaxList(Of VBS.StatementSyntax)
                Dim csCloseBrace As SyntaxToken = CSCloseBraceToken
                If node.Body IsNot Nothing Then
                    csCloseBrace = node.Body.CloseBraceToken
                    For Each e As IndexClass(Of CSS.StatementSyntax) In node.Body.Statements.WithIndex
                        If TypeOf e.Value Is CSS.LocalFunctionStatementSyntax Then
                            Body = Body.AddRange(ReplaceOneStatementWithMarkedStatements(e.Value, Factory.EmptyStatement(), RemoveStatement:=True))
                        Else
                            Body = Body.AddRange(e.Value.Accept(New MethodBodyVisitor(_mSemanticModel, Me)))
                        End If
                    Next
                    EndSubStatement = Factory.EndSubStatement(EndKeyword.WithTrailingTrivia(VBSpaceTrivia), SubKeyword).WithConvertedTriviaFrom(csCloseBrace)
                    If node.Body.OpenBraceToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        Dim Trivia As SyntaxTriviaList = node.Body.OpenBraceToken.LeadingTrivia.ConvertTriviaList()
                        If Not Body.Any Then
                            EndSubStatement = EndSubStatement.WithPrependedLeadingTrivia(Trivia)
                        End If
                    End If
                ElseIf node.ExpressionBody IsNot Nothing Then
                    Body = node.ExpressionBody.GetExpressionBodyStatements(Me)
                End If
                If Initializer IsNot Nothing Then
                    Body = Body.InsertRange(0, ReplaceOneStatementWithMarkedStatements(node, Initializer))
                Else
                    Body = ReplaceStatementsWithMarkedStatements(node, Body)

                End If
                Return Factory.ConstructorBlock(CType(SubNewStatement, VBS.SubNewStatementSyntax),
                                                  Body,
                                                  EndSubStatement).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitConstructorInitializer(node As CSS.ConstructorInitializerSyntax) As VB.VisualBasicSyntaxNode
                Dim ArgumentList As VBS.ArgumentListSyntax = DirectCast(node?.ArgumentList.Accept(Me), VBS.ArgumentListSyntax)
                Dim SimpleMemberAccessExpression As VBS.MemberAccessExpressionSyntax
                Dim parent As SyntaxNode = node.Parent.Parent
                Dim MeOrMyExpression As VBS.ExpressionSyntax = If(TypeOf parent Is CSS.StructDeclarationSyntax,
                                                            DirectCast(Factory.MeExpression(), VBS.ExpressionSyntax),
                                                            Factory.MyBaseExpression()).WithConvertedLeadingTriviaFrom(node.ColonToken)

                SimpleMemberAccessExpression = Factory.SimpleMemberAccessExpression(MeOrMyExpression, Factory.IdentifierName("New"))
                Dim InvocationExpression As VBS.InvocationExpressionSyntax = Factory.InvocationExpression(SimpleMemberAccessExpression, ArgumentList)
                Return Factory.ExpressionStatement(InvocationExpression).
                                             RestructureArguments(node.ArgumentList).WithConvertedTrailingTriviaFrom(node).WithTrailingEOL
            End Function

            ''' <summary>
            ''' Creates a new object initialized to a meaningful value.
            ''' </summary>
            ''' <param name="value"></param>
            Public Overrides Function VisitConversionOperatorDeclaration(node As CSS.ConversionOperatorDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim AttributeLists As New List(Of VBS.AttributeListSyntax)
                Dim ReturnAttributes As SyntaxList(Of VBS.AttributeListSyntax) = Nothing
                Dim FinalTrailingDirective As New SyntaxTriviaList
                Me.ConvertAndSplitAttributes(node.AttributeLists, AttributeLists, ReturnAttributes, FinalTrailingDirective)
                Dim parameterList As VBS.ParameterListSyntax = DirectCast(node.ParameterList?.Accept(Me), VBS.ParameterListSyntax).
                                                                    WithRestructuredingEOLTrivia
                Dim Modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, Me.IsModule, TokenContext.Member).ToList
                Dim visitor As New MethodBodyVisitor(_mSemanticModel, Me)
                Modifiers.Add(If(node.ImplicitOrExplicitKeyword.ValueText = "explicit", NarrowingKeyword, WideningKeyword))
                Dim Type As VBS.TypeSyntax = DirectCast(node.Type.Accept(Me), VBS.TypeSyntax).With({VBSpaceTrivia}, {VBSpaceTrivia})
                Dim AsClause As VBS.SimpleAsClauseSyntax = Factory.SimpleAsClause(Nothing, Type)
                Dim OperatorStatement As VBS.OperatorStatementSyntax = Factory.OperatorStatement(Factory.List(AttributeLists), Factory.TokenList(Modifiers), CTypeKeyword, parameterList, AsClause).WithTrailingEOL
                If FinalTrailingDirective.Any Then
                    OperatorStatement = OperatorStatement.WithAppendedTrailingTrivia(FinalTrailingDirective)
                End If

                Dim body As New SyntaxList(Of VBS.StatementSyntax)
                If node.Body IsNot Nothing Then
                    body = node.Body.GetBodyStatements(visitor)
                ElseIf node.ExpressionBody IsNot Nothing Then
                    body = node.ExpressionBody.GetExpressionBodyStatements(Me)
                End If
                Dim EndOperatorStatement As VBS.EndBlockStatementSyntax = FactoryEndBlockStatement(VB.SyntaxKind.EndOperatorStatement, BlockKeyword, CollectConvertedTokenTrivia(node.Body.GetBraces.Item2, GetLeading:=True, GetTrailing:=True))
                Dim OperatorBlock As VBS.OperatorBlockSyntax = Factory.OperatorBlock(OperatorStatement, body, EndOperatorStatement).WithConvertedTriviaFrom(node)
                Return PrependStatementWithMarkedStatementTrivia(node, OperatorBlock)
            End Function

            Public Overrides Function VisitDestructorDeclaration(node As CSS.DestructorDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim AttributeLists As SyntaxList(Of VBS.AttributeListSyntax) = Factory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))
                Dim Modifiers As SyntaxTokenList = Factory.TokenList(ProtectedKeyword, OverridesKeyword)
                Dim Identifier As SyntaxToken = Factory.Identifier(NameOf(Finalize))
                Dim ParameterList As VBS.ParameterListSyntax = DirectCast(node.ParameterList?.Accept(Me), VBS.ParameterListSyntax)
                Dim body As SyntaxList(Of VBS.StatementSyntax)
                If node.Body IsNot Nothing Then
                    body = node.Body.GetBodyStatements(New MethodBodyVisitor(_mSemanticModel, Me))
                Else
                    body = node.ExpressionBody.GetExpressionBodyStatements(Me)
                End If
                Return Factory.SubBlock(subOrFunctionStatement:=Factory.SubStatement(
                                              AttributeLists,
                                              Modifiers,
                                              Identifier,
                                              typeParameterList:=Nothing,
                                              ParameterList,
                                              asClause:=Nothing,
                                              handlesClause:=Nothing,
                                              implementsClause:=Nothing),
                                              Factory.List(body)).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitEventDeclaration(node As CSS.EventDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim declaredSymbol As ISymbol = _mSemanticModel.GetDeclaredSymbol(node)
                Dim Attributes As New List(Of VBS.AttributeListSyntax)
                Dim ReturnAttributes As SyntaxList(Of VBS.AttributeListSyntax) = Nothing
                Dim FinalTrailingDirective As New SyntaxTriviaList
                Me.ConvertAndSplitAttributes(node.AttributeLists, Attributes, ReturnAttributes, FinalTrailingDirective)
                Dim Modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, Me.IsModule, TokenContext.Member).ToList
                Dim eventNameToken As SyntaxToken = GenerateSafeVBToken(node.Identifier, node, _mSemanticModel).WithTrailingTrivia(VBSpaceTrivia)
                Dim AsClause As VBS.SimpleAsClauseSyntax = Factory.SimpleAsClause(attributeLists:=ReturnAttributes, DirectCast(node.Type.Accept(Me), VBS.TypeSyntax))
                Modifiers.Add(CustomKeyword)
                Dim implementsClauseOrNothing As VBS.ImplementsClauseSyntax = If(declaredSymbol Is Nothing, Nothing, Me.CreateImplementsClauseSyntaxOrNull(declaredSymbol, eventNameToken))
                Dim stmt As VBS.EventStatementSyntax = Factory.EventStatement(attributeLists:=Factory.List(Attributes),
                                                                              Factory.TokenList(Modifiers),
                                                                              eventNameToken,
                                                                              parameterList:=Nothing,
                                                                              AsClause,
                                                                              implementsClauseOrNothing).WithTrailingEOL
                If FinalTrailingDirective.Any Then
                    stmt = stmt.WithAppendedTrailingTrivia(FinalTrailingDirective)
                End If
                Dim accessors As New List(Of VBS.AccessorBlockSyntax)
                For Each e As IndexClass(Of CSS.AccessorDeclarationSyntax) In node.AccessorList.Accessors.WithIndex
                    If e.Value.Body IsNot Nothing OrElse e.Value.ExpressionBody IsNot Nothing Then
                        accessors.Add(Me.ConvertAccessor(e.Value, Me.IsModule, isIterator:=False))
                    End If
                Next
                If accessors.Any Then
                    Return Factory.EventBlock(stmt, Factory.List(accessors)).WithConvertedTriviaFrom(node)
                End If
                Return stmt.WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitEventFieldDeclaration(node As CSS.EventFieldDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim decl As CSS.VariableDeclaratorSyntax = node.Declaration.Variables.Single()
                Dim declaredSymbol As ISymbol = _mSemanticModel.GetDeclaredSymbol(decl)
                Dim id As SyntaxToken = Factory.Identifier(MakeVBSafeName(node.Declaration.Variables.Single().Identifier.ValueText))
                Dim implementsClauseOrNothing As VBS.ImplementsClauseSyntax = If(declaredSymbol Is Nothing, Nothing, Me.CreateImplementsClauseSyntaxOrNull(declaredSymbol, id))
                Dim ReturnAttributes As New SyntaxList(Of VBS.AttributeListSyntax)
                Dim AttributeList As New List(Of VBS.AttributeListSyntax)
                Dim FinalTrailingDirective As New SyntaxTriviaList
                Me.ConvertAndSplitAttributes(node.AttributeLists, AttributeList, ReturnAttributes, FinalTrailingDirective)
                If FinalTrailingDirective.Any Then
                    Stop
                End If
                Return Factory.EventStatement(Factory.List(AttributeList),
                                     Factory.TokenList(ConvertModifiers(node.Modifiers, Me.IsModule, TokenContext.Member)),
                                                    id,
                                                    parameterList:=Nothing,
                                                    Factory.SimpleAsClause(attributeLists:=Nothing, DirectCast(node.Declaration.Type.Accept(Me), VBS.TypeSyntax)).WithTrailingEOL,
                                                    implementsClauseOrNothing).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitFieldDeclaration(node As CSS.FieldDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim _TypeInfo As TypeInfo = _mSemanticModel.GetTypeInfo(node.Declaration.Type)
                Dim variableOrConstOrReadonly As TokenContext = TokenContext.VariableOrConst
                If _TypeInfo.ConvertedType IsNot Nothing AndAlso _TypeInfo.ConvertedType.TypeKind = Microsoft.CodeAnalysis.TypeKind.Class Then
                    For Each e As IndexClass(Of CSS.VariableDeclaratorSyntax) In node.Declaration.Variables.WithIndex
                        If e.Value.Initializer IsNot Nothing AndAlso e.Value.Initializer.Value.IsKind(CS.SyntaxKind.NullLiteralExpression) Then
                            variableOrConstOrReadonly = TokenContext.Readonly
                        End If
                    Next
                End If
                Dim modifierList As New List(Of SyntaxToken)
                modifierList.AddRange(ConvertModifiers(node.Modifiers, Me.IsModule, variableOrConstOrReadonly))
                If modifierList.Count = 0 Then
                    modifierList.Add(PrivateKeyword.WithLeadingTrivia(node.Declaration.Type.GetLeadingTrivia.ConvertTriviaList()))
                End If
                Dim newLeadingTrivia As New SyntaxTriviaList
                Dim Attributes As New SyntaxList(Of VBS.AttributeListSyntax)
                If node.Modifiers.Contains(CS.SyntaxKind.VolatileKeyword) Then
                    Dim Name As VBS.TypeSyntax = Factory.ParseTypeName("Volatile")
                    Dim VolatileAttribute As SeparatedSyntaxList(Of VBS.AttributeSyntax) = Factory.SingletonSeparatedList(Factory.Attribute(Name))
                    newLeadingTrivia = newLeadingTrivia.Add(Factory.CommentTrivia("' TODO TASK: VB has no direct equivalent to C# Volatile Modifier, an Attribute was substituted."))
                    Attributes = Attributes.Add(Factory.AttributeList(VolatileAttribute).WithLeadingTrivia(newLeadingTrivia))
                    newLeadingTrivia = New SyntaxTriviaList
                End If
                If node.AttributeLists.Any Then
                    newLeadingTrivia = newLeadingTrivia.AddRange(node.AttributeLists(0).GetLeadingTrivia.ConvertTriviaList())
                Else
                    If node.Modifiers.Any Then
                        newLeadingTrivia = newLeadingTrivia.AddRange(node.Modifiers(0).LeadingTrivia.ConvertTriviaList())
                    ElseIf modifierList.Any Then
                        newLeadingTrivia = newLeadingTrivia.AddRange(modifierList(0).LeadingTrivia)
                    End If
                    If modifierList(0).HasLeadingTrivia Then
                        modifierList(0) = modifierList(0).WithLeadingTrivia(modifierList(0).LeadingTrivia.Last)
                    End If
                End If

                For Each a As CSS.AttributeListSyntax In node.AttributeLists
                    Attributes = Attributes.Add(DirectCast(a.Accept(Me), VBS.AttributeListSyntax))
                Next
                If Attributes.Any Then
                    Attributes = Attributes.Replace(Attributes(0), Attributes(0).WithLeadingTrivia(newLeadingTrivia))
                Else
                    modifierList(0) = modifierList(0).WithLeadingTrivia(newLeadingTrivia)
                End If
                Dim declarators As SeparatedSyntaxList(Of VBS.VariableDeclaratorSyntax) = node.Declaration.RemodelVariableDeclaration(Me, _mSemanticModel, IsFieldDeclaration:=True, newLeadingTrivia)
                Dim FieldDeclaration As VBS.FieldDeclarationSyntax
                Dim modifiers As SyntaxTokenList = Factory.TokenList(modifierList)
                FieldDeclaration = Factory.FieldDeclaration(Attributes, modifiers, declarators).WithLeadingTrivia(newLeadingTrivia)
                FieldDeclaration = AddSpecialCommentToField(node, FieldDeclaration)
                Return FieldDeclaration.RestructureAttributesAndModifiers(Attributes.Any, modifiers.Any).
                    WithMergedTrailingTrivia(node.SemicolonToken.CollectConvertedTokenTrivia(GetLeading:=True, GetTrailing:=True)).WithTrailingEOL
            End Function

            Public Overrides Function VisitIndexerDeclaration(node As CSS.IndexerDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim declaredSymbol As IPropertySymbol = TryCast(_mSemanticModel.GetDeclaredSymbol(node), IPropertySymbol)
                Dim id As SyntaxToken = Factory.Identifier("Item")
                Dim attributes As New List(Of VBS.AttributeListSyntax)
                Dim returnAttributes As SyntaxList(Of VBS.AttributeListSyntax) = Nothing
                Dim finalTrailingDirective As New SyntaxTriviaList
                Me.ConvertAndSplitAttributes(node.AttributeLists, attributes, returnAttributes, finalTrailingDirective)
                Dim isIterator As Boolean = False
                Dim accessors As New List(Of VBS.AccessorBlockSyntax)()
                If node.AccessorList IsNot Nothing Then
                    For Each a As CSS.AccessorDeclarationSyntax In node.AccessorList.Accessors
                        Dim _isIterator As Boolean
                        accessors.Add(Me.ConvertAccessor(a, Me.IsModule, _isIterator))
                        isIterator = isIterator Or _isIterator
                    Next
                End If

                Dim modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, Me.IsModule, TokenContext.Member).ToList
                If modifiers.Any Then
                    modifiers.Insert(0, DefaultKeyword.WithLeadingTrivia(modifiers(0).LeadingTrivia))
                    modifiers(1) = modifiers(1).WithLeadingTrivia(VBSpaceTrivia)
                End If
                Select Case accessors.Count
                    Case 0
                        Dim lastTrailingTrivia As SyntaxTriviaList = lastTrailingTrivia.AddRange(modifiers.Last.TrailingTrivia)
                        modifiers(modifiers.Count - 1) = modifiers.Last.WithLeadingTrivia(VBSpaceTrivia).WithTrailingTrivia(VBSpaceTrivia)
                        modifiers.Add(ReadOnlyKeyword.WithLeadingTrivia(VBSpaceTrivia).WithTrailingTrivia(lastTrailingTrivia))

                        Dim AccessorStatement As VBS.AccessorStatementSyntax = Factory.GetAccessorStatement()
                        Dim Body As SyntaxList(Of VBS.StatementSyntax) = node.ExpressionBody.GetExpressionBodyStatements(Me)
                        Dim EndStmt As VBS.EndBlockStatementSyntax = FactoryEndBlockStatement(VB.SyntaxKind.EndGetStatement, GetKeyword, New SyntaxTriviaList)
                        accessors.Add(Factory.AccessorBlock(VB.SyntaxKind.GetAccessorBlock, AccessorStatement, Body, EndStmt))
                    Case 1
                        Dim needKeyword As Boolean = True
                        If accessors(0).AccessorStatement.Kind() = VB.SyntaxKind.GetAccessorStatement Then
                            For Each keyword As SyntaxToken In modifiers
                                If keyword.ValueText = "ReadOnly" Then
                                    needKeyword = False
                                    Exit For
                                End If
                            Next
                            If needKeyword Then
                                modifiers.Add(ReadOnlyKeyword)
                            End If
                        Else
                            For Each keyword As SyntaxToken In modifiers
                                If keyword.ValueText = "WriteOnly" Then
                                    needKeyword = False
                                    Exit For
                                End If
                            Next
                            If needKeyword Then
                                modifiers.Add(WriteOnlyKeyword)
                            End If
                        End If
                    Case 2
                        ' Ignore
                    Case Else
                        _reportException?.Invoke(UnreachableException)
                End Select
                If isIterator Then
                    modifiers.Add(IteratorKeyword)
                End If
                Dim implementsClauseOrNothing As VBS.ImplementsClauseSyntax = If(declaredSymbol Is Nothing, Nothing, Me.CreateImplementsClauseSyntaxOrNull(declaredSymbol, id))
                Dim parameterList As VBS.ParameterListSyntax = DirectCast(node.ParameterList?.Accept(Me), VBS.ParameterListSyntax).WithRestructuredingEOLTrivia
                Dim nodeType As VBS.TypeSyntax = DirectCast(node.Type.Accept(Me), VBS.TypeSyntax)
                Dim asClause As VBS.SimpleAsClauseSyntax = Factory.SimpleAsClause(returnAttributes, nodeType.WithLeadingTrivia(VBSpaceTrivia))
                Dim stmt As VBS.PropertyStatementSyntax = Factory.PropertyStatement(Factory.List(attributes),
                                                                                    Factory.TokenList(modifiers),
                                                                                    id,
                                                                                    parameterList,
                                                                                    asClause.WithTrailingEOL,
                                                                                    initializer:=Nothing,
                                                                                    implementsClauseOrNothing).WithTrailingEOL
                If finalTrailingDirective.Any Then
                    stmt = stmt.WithAppendedTrailingTrivia(finalTrailingDirective)
                End If
                Dim accessorList As CSS.AccessorListSyntax = node.AccessorList
                Dim emptyAccessorListBodies As Boolean = True
                If accessorList IsNot Nothing Then
                    For Each a As CSS.AccessorDeclarationSyntax In accessorList.Accessors
                        If a.Body IsNot Nothing OrElse a.ExpressionBody IsNot Nothing Then
                            emptyAccessorListBodies = False
                            Exit For
                        End If
                    Next
                End If

                If emptyAccessorListBodies AndAlso node.ExpressionBody Is Nothing Then
                    Return stmt
                End If
                Return Factory.PropertyBlock(stmt,
                                               Factory.List(accessors),
                                               Factory.EndPropertyStatement(EndKeyword.WithTrailingTrivia(VBSpaceTrivia), PropertyKeyword).
                                                                                WithConvertedTriviaFrom(node.AccessorList.GetBraces.Item2)).
                                                                                WithConvertedLeadingTriviaFrom(node.Type)
            End Function

            Public Overrides Function VisitMethodDeclaration(node As CSS.MethodDeclarationSyntax) As VB.VisualBasicSyntaxNode
                If node.Modifiers.Contains(CS.SyntaxKind.UnsafeKeyword) Then
                    Return FlagUnsupportedStatements(node, "unsafe Functions", CommentOutOriginalStatements:=True)
                End If

                If node.ReturnType IsNot Nothing AndAlso TypeOf node.ReturnType Is CSS.RefTypeSyntax Then
                    Return FlagUnsupportedStatements(node, "ref return Functions", CommentOutOriginalStatements:=True)
                End If
                SyncLock s_usedStacks
                    s_usedStacks.Push(s_usedIdentifiers)
                End SyncLock

                Dim methodNameToken As SyntaxToken = GenerateSafeVBToken(node.Identifier, node, _mSemanticModel)

                Dim methodInfo As ISymbol = ModelExtensions.GetDeclaredSymbol(_mSemanticModel, node)
                Dim possibleReturnVoid As Boolean? = methodInfo?.GetReturnType()?.SpecialType = SpecialType.System_Void
                Dim returnVoid As Boolean = If(possibleReturnVoid, False)
                Dim containingType As INamedTypeSymbol = methodInfo?.ContainingType
                Dim Attributes As New List(Of VBS.AttributeListSyntax)
                Dim ReturnAttributes As SyntaxList(Of VBS.AttributeListSyntax) = Nothing
                Dim FunctionStatementTrailingTrivia As New SyntaxTriviaList
                Me.ConvertAndSplitAttributes(node.AttributeLists, Attributes, ReturnAttributes, FunctionStatementTrailingTrivia)
                Dim ParameterList As VBS.ParameterListSyntax = DirectCast(node.ParameterList?.Accept(Me), VBS.ParameterListSyntax)

                ParameterList = ParameterList.RelocateDirectivesInTrailingTrivia(FunctionStatementTrailingTrivia)

                Dim FunctionStatementLeadingTrivia As SyntaxTriviaList
                Dim vbStatements As New List(Of VBS.StatementSyntax)
                Dim visitor As New MethodBodyVisitor(_mSemanticModel, Me)
                Dim finalLeadingTrivia As New SyntaxTriviaList
                Dim finalTrailingTrivia As New SyntaxTriviaList
                If node.Body IsNot Nothing Then
                    finalLeadingTrivia = CollectConvertedTokenTrivia(node.Body.CloseBraceToken, GetLeading:=True, GetTrailing:=False)
                    finalTrailingTrivia = CollectConvertedTokenTrivia(node.Body.CloseBraceToken, GetLeading:=False, GetTrailing:=True)
                End If
                For Each e As IndexClass(Of CSS.LocalFunctionStatementSyntax) In node.DescendantNodes().OfType(Of CSS.LocalFunctionStatementSyntax).WithIndex
                    Dim localFunction As CSS.LocalFunctionStatementSyntax = e.Value
                    Dim replacementStatement As VBS.StatementSyntax = localFunction.Accept(visitor)(0)
                    If e.IsLast And finalLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        finalLeadingTrivia = New SyntaxTriviaList
                    End If
                    If TypeOf replacementStatement IsNot VBS.EmptyStatementSyntax OrElse replacementStatement.ContainsCommentOrDirectiveTrivia Then
                        vbStatements.Add(replacementStatement)
                    End If
                Next

                Dim body As SyntaxList(Of VBS.StatementSyntax)? = Nothing
                If node.Body IsNot Nothing Then
                    Dim vbStatementCollection As SyntaxList(Of VBS.StatementSyntax)
                    For Each e As IndexClass(Of CSS.StatementSyntax) In node.Body.Statements.WithIndex
                        If e.Value.IsKind(CS.SyntaxKind.LocalFunctionStatement) Then
                            vbStatementCollection = ReplaceOneStatementWithMarkedStatements(e.Value, Factory.EmptyStatement, True)
                        Else
                            vbStatementCollection = e.Value.Accept(visitor)
                            vbStatementCollection = ReplaceStatementsWithMarkedStatements(e.Value, vbStatementCollection)
                        End If
                        If e.IsFirst Then
                            If node.Body.OpenBraceToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                vbStatementCollection = vbStatementCollection.Replace(vbStatementCollection(0), vbStatementCollection(0).WithPrependedLeadingTrivia(node.Body.OpenBraceToken.LeadingTrivia.ConvertTriviaList()))
                            End If
                        End If
                        vbStatements.AddRange(ReplaceStatementsWithMarkedStatements(e.Value, vbStatementCollection))
                    Next
                    body = Factory.List(vbStatements)
                ElseIf node.ExpressionBody IsNot Nothing Then
                    If node.ExpressionBody.Expression Is Nothing Then
                        Return PrependStatementWithMarkedStatementTrivia(node, Factory.EmptyStatement.WithConvertedTriviaFrom(node))
                    End If
                    If node.ExpressionBody.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        FunctionStatementLeadingTrivia = FunctionStatementLeadingTrivia.AddRange(node.ExpressionBody.GetLeadingTrivia.ConvertTriviaList())
                        body = node.ExpressionBody.GetExpressionBodyStatements(Me)
                        If body.HasValue Then
                            body = body.Value.Replace(body.Value(0), body.Value(0).WithoutLeadingTrivia)
                        End If
                    Else
                        body = node.ExpressionBody.GetExpressionBodyStatements(Me)
                    End If
                    body = ReplaceStatementsWithMarkedStatements(node, body.Value)
                End If
                If node.Modifiers.Contains(CS.SyntaxKind.ExternKeyword) Then
                    body = Factory.List(Of VBS.StatementSyntax)()
                End If
                Dim Modifiers As List(Of SyntaxToken)
                If Me.IsModule AndAlso
                    methodNameToken.ValueText = "Main" AndAlso
                    node.Modifiers.Count = 1 AndAlso
                    node.Modifiers(0).ValueText = "static" Then
                    Modifiers = PublicModifier.ToList
                Else
                    Modifiers = ConvertModifiers(node.Modifiers, Me.IsModule, If(containingType?.IsInterfaceType() = True, TokenContext.Local, TokenContext.Member)).ToList
                End If
                If visitor.IsInterator And Not returnVoid Then
                    Modifiers.Add(IteratorKeyword)
                End If
                If node.ParameterList.Parameters.Any AndAlso node.ParameterList.Parameters(0).Modifiers.Any(CS.SyntaxKind.ThisKeyword) Then
                    Dim newLeadingTrivia As SyntaxTriviaList
                    If Attributes.Any AndAlso Attributes(0).HasLeadingTrivia Then
                        newLeadingTrivia = newLeadingTrivia.AddRange(Attributes(0).GetLeadingTrivia)
                        Attributes(0) = Attributes(0).WithLeadingTrivia(newLeadingTrivia.Last)
                    End If
                    If Attributes.Count = 0 AndAlso Modifiers.Any Then
                        newLeadingTrivia = newLeadingTrivia.AddRange(Modifiers(0).LeadingTrivia)
                        Modifiers(0) = Modifiers(0).WithLeadingTrivia(VBSpaceTrivia)
                    End If
                    Attributes.Insert(0, Factory.AttributeList(Factory.SingletonSeparatedList(ExtensionAttribute)).WithPrependedLeadingTrivia(newLeadingTrivia))
                    newLeadingTrivia = New SyntaxTriviaList
                    If Not DirectCast(node.SyntaxTree, CS.CSharpSyntaxTree).HasUsingDirective(CompilerServices) Then
                        If Not AllImports.ContainsName(CompilerServices) Then
                            AllImports.Add(ImportComilierServices)
                        End If
                    End If
                End If
                If containingType?.IsStatic = True Then
                    Dim TokenList As New List(Of SyntaxToken)
                    Dim newLeadingTrivia As New SyntaxTriviaList
                    For Each e As IndexClass(Of SyntaxToken) In Modifiers.WithIndex
                        Dim t As SyntaxToken = e.Value
                        If t.IsKind(VB.SyntaxKind.SharedKeyword) Then
                            If t.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                newLeadingTrivia = newLeadingTrivia.AddRange(t.LeadingTrivia)
                            End If
                        Else
                            TokenList.Add(t)
                        End If
                    Next
                    If TokenList.Count = 0 Then
                        Modifiers.Clear()
                        Modifiers.Add(Factory.Token(VB.SyntaxKind.EmptyToken).WithLeadingTrivia(newLeadingTrivia))
                    Else
                        If Not EndsWithSimilarTrivia(newLeadingTrivia, Modifiers(0).LeadingTrivia) Then
                            newLeadingTrivia = newLeadingTrivia.InsertRange(0, Modifiers(0).LeadingTrivia)
                            Modifiers(0) = Modifiers(0).WithLeadingTrivia(newLeadingTrivia)
                        End If
                    End If
                End If

                If Attributes.Any Then
                    FunctionStatementLeadingTrivia = FunctionStatementLeadingTrivia.AddRange(Attributes(0).GetLeadingTrivia)
                    Attributes(0) = Attributes(0).WithLeadingTrivia(VBSpaceTrivia)
                End If
                If node.ReturnType IsNot Nothing Then
                    If Modifiers.Count = 0 Then
                        FunctionStatementLeadingTrivia = FunctionStatementLeadingTrivia.AddRange(node.ReturnType.GetLeadingTrivia.ConvertTriviaList())
                    Else
                        Dim NewModifierLeadingTrivia As New SyntaxTriviaList
                        Dim csNodeLeadingTrivia As SyntaxTriviaList = node.ReturnType.GetLeadingTrivia
                        If csNodeLeadingTrivia.Any Then
                            NewModifierLeadingTrivia = NewModifierLeadingTrivia.AddRange(Modifiers(0).LeadingTrivia)
                            NewModifierLeadingTrivia = NewModifierLeadingTrivia.AddRange(csNodeLeadingTrivia.ConvertTriviaList())
                            If Not NewModifierLeadingTrivia.FirstOrDefault.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                NewModifierLeadingTrivia = NewModifierLeadingTrivia.Insert(0, VBEOLTrivia)
                            End If
                            Modifiers(0) = Modifiers(0).WithLeadingTrivia(NewModifierLeadingTrivia)
                        End If
                        If Attributes.Count = 0 AndAlso Modifiers(0).LeadingTrivia.Any Then
                            FunctionStatementLeadingTrivia = FunctionStatementLeadingTrivia.AddRange(Modifiers(0).LeadingTrivia)
                            Modifiers(0) = Modifiers(0).WithLeadingTrivia(Modifiers(0).LeadingTrivia.Last)
                        End If
                    End If
                End If
                If node.Body Is Nothing Then
                    FunctionStatementTrailingTrivia = FunctionStatementTrailingTrivia.AddRange(node.GetTrailingTrivia.ConvertTriviaList())
                Else
                    If node.Body.OpenBraceToken.HasTrailingTrivia Then
                        FunctionStatementTrailingTrivia = FunctionStatementTrailingTrivia.AddRange(node.Body.OpenBraceToken.TrailingTrivia.ConvertTriviaList())
                    Else
                        FunctionStatementTrailingTrivia = FunctionStatementTrailingTrivia.AddRange(node.GetTrailingTrivia.ConvertTriviaList())
                    End If
                End If

                Dim subOrFunctionStatement As VBS.MethodStatementSyntax
                Dim TypeParameterList As VBS.TypeParameterListSyntax = DirectCast(node.TypeParameterList?.Accept(Me), VBS.TypeParameterListSyntax)
                If TypeParameterList IsNot Nothing Then
                    TypeParameterList = TypeParameterList.WithTrailingTrivia(VBSpaceTrivia)
                End If
                Dim implementsClauseOrNothing As VBS.ImplementsClauseSyntax = If(methodInfo Is Nothing, Nothing, Me.CreateImplementsClauseSyntaxOrNull(methodInfo, methodNameToken))

                If methodNameToken.ToString = "Dispose" AndAlso TypeOf node.Parent Is CSS.ClassDeclarationSyntax Then
                    Dim ParentClass As CSS.ClassDeclarationSyntax = DirectCast(node.Parent, CSS.ClassDeclarationSyntax)
                    If ParentClass.BaseList IsNot Nothing Then
                        For Each t As CSS.SimpleBaseTypeSyntax In ParentClass.BaseList.Types
                            If TypeOf t.Type Is CSS.IdentifierNameSyntax AndAlso DirectCast(t.Type, CSS.IdentifierNameSyntax).Identifier.ValueText = "IDisposable" Then
                                Dim InterfaceMembers As VBS.QualifiedNameSyntax = Factory.QualifiedName(
                                                                                Factory.IdentifierName("IDisposable"),
                                                                                Factory.IdentifierName("Dispose")
                                                                                )
                                If ParameterList Is Nothing OrElse ParameterList.Parameters.Any Then
                                    Exit For
                                End If
                                implementsClauseOrNothing = Factory.ImplementsClause(InterfaceMembers).WithTrailingTrivia(ParameterList.GetTrailingTrivia)
                                ParameterList = ParameterList.WithTrailingTrivia(VBSpaceTrivia)
                            End If
                        Next
                    End If
                End If
                Dim EndSubOrFunction As VBS.EndBlockStatementSyntax

                If returnVoid Then
                    If node.Body IsNot Nothing Then
                        EndSubOrFunction = FactoryEndBlockStatement(VB.SyntaxKind.EndSubStatement, SubKeyword, finalTrailingTrivia)
                    ElseIf node.ExpressionBody IsNot Nothing Then
                        EndSubOrFunction = FactoryEndBlockStatement(VB.SyntaxKind.EndSubStatement, SubKeyword, CollectConvertedTokenTrivia(node.ExpressionBody.GetBraces.Item2, GetLeading:=True, GetTrailing:=True))
                    Else
                        EndSubOrFunction = Factory.EndSubStatement(EndKeyword.WithTrailingTrivia(VBSpaceTrivia), SubKeyword)
                    End If

                    If TypeParameterList IsNot Nothing OrElse ParameterList IsNot Nothing Then
                        methodNameToken = methodNameToken.WithTrailingTrivia(VBSpaceTrivia)
                    End If
                    If Attributes.Any AndAlso Attributes.Last.GetTrailingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                        If Modifiers.Any AndAlso Modifiers(0).LeadingTrivia.ContainsEOLTrivia Then
                            If Modifiers(0).LeadingTrivia(0).IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                Modifiers(0) = Modifiers(0).WithLeadingTrivia(Modifiers(0).LeadingTrivia.RemoveAt(0))
                            End If
                        End If
                    End If
                    Dim MethodStatement As VBS.MethodStatementSyntax = Factory.SubStatement(
                                                                        Factory.List(Attributes),
                                                                        Factory.TokenList(Modifiers),
                                                                        methodNameToken,
                                                                        TypeParameterList,
                                                                        ParameterList,
                                                                        asClause:=Nothing,
                                                                        handlesClause:=Nothing,
                                                                        implementsClauseOrNothing)
                    subOrFunctionStatement = DirectCast(MethodStatement.
                                                    With(FunctionStatementLeadingTrivia, FunctionStatementTrailingTrivia).
                                                    WithTrailingEOL.
                                                    RestructureAttributesAndModifiers(Attributes.Any, Modifiers.Any), VBS.MethodStatementSyntax)
                    SyncLock s_usedStacks
                        If s_usedStacks.Count > 0 Then
                            s_usedIdentifiers = DirectCast(s_usedStacks.Pop, Dictionary(Of String, SymbolTableEntry))
                        End If
                    End SyncLock
                    subOrFunctionStatement = DirectCast(PrependStatementWithMarkedStatementTrivia(node, subOrFunctionStatement), VBS.MethodStatementSyntax)
                    If body Is Nothing Then
                        If Modifiers.Contains(Function(t As SyntaxToken) t.IsKind(VB.SyntaxKind.PartialKeyword)) Then
                            Return Factory.SubBlock(subOrFunctionStatement,
                                                    statements:=Nothing,
                                                    EndSubOrFunction.With(FunctionStatementLeadingTrivia, FunctionStatementTrailingTrivia).WithPrependedLeadingTrivia(finalLeadingTrivia))
                        End If
                        Return subOrFunctionStatement
                    End If
                    Return Factory.SubBlock(subOrFunctionStatement,
                                            body.Value,
                                            EndSubOrFunction.WithPrependedLeadingTrivia(finalLeadingTrivia))
                End If
                If node.Body IsNot Nothing Then
                    EndSubOrFunction = FactoryEndBlockStatement(VB.SyntaxKind.EndFunctionStatement, FunctionKeyword, finalTrailingTrivia).WithPrependedLeadingTrivia(finalLeadingTrivia)
                Else
                    EndSubOrFunction = FactoryEndBlockStatement(VB.SyntaxKind.EndFunctionStatement, FunctionKeyword, New SyntaxTriviaList)
                End If
                Dim type As VBS.TypeSyntax = DirectCast(node.ReturnType.Accept(Me), VBS.TypeSyntax).WithLeadingTrivia(VBSpaceTrivia)

                If type Is Nothing Then
                    ' Handle ref return
                    type = Factory.ParseTypeName("HandleRef")
                Else
                    If type.ToString = "[Delegate]" Then
                        type = Factory.ParseTypeName("System.Delegate")
                    ElseIf type.ToString = "[Enum]" Then
                        type = Factory.ParseTypeName("System.Enum")
                    ElseIf type.ToString.StartsWith("[", StringComparison.Ordinal) Then
                        Dim S As String() = type.ToString.Split({"["c, "]"}, StringSplitOptions.RemoveEmptyEntries)
                        If Not (IsSpecialReservedWord(S(0)) OrElse
                                VB.SyntaxFacts.IsKeywordKind(VB.SyntaxFacts.GetKeywordKind(S(0)))) Then
                            type = Factory.ParseTypeName(type.ToString().RemoveBrackets)
                        End If
                    End If
                End If

                Dim AsClause As VBS.SimpleAsClauseSyntax = Factory.SimpleAsClause(ReturnAttributes, type.WithLeadingTrivia(VBSpaceTrivia))
                Dim ParamListTrailingTrivia As SyntaxTriviaList = ParameterList.GetTrailingTrivia
                ParameterList = ParameterList.WithTrailingTrivia(VBSpaceTrivia)
                AsClause = AsClause.WithTrailingTrivia(ParamListTrailingTrivia)

                If TypeParameterList IsNot Nothing OrElse ParameterList IsNot Nothing Then
                    methodNameToken = methodNameToken.WithTrailingTrivia(VBSpaceTrivia)
                End If
                If AsClause.GetTrailingTrivia.ContainsCommentTrivia Then
                    FunctionStatementTrailingTrivia = FunctionStatementTrailingTrivia.InsertRange(0, AsClause.GetTrailingTrivia.ToList)
                    AsClause = AsClause.WithTrailingTrivia(VBSpaceTrivia)
                End If
                Dim initialTriviaList As SyntaxTriviaList = If(Modifiers.Any, Modifiers(0).LeadingTrivia, Nothing)

                Dim MovedModifierLeadingTrivia As New SyntaxTriviaList
                If Attributes.Any AndAlso Modifiers.Any AndAlso initialTriviaList.ContainsCommentOrDirectiveTrivia Then
                    Dim FixedModifierLeadingTrivia As SyntaxTriviaList
                    For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                        Dim t As SyntaxTrivia = e.Value
                        Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(initialTriviaList, e.Index, LookaheadCount:=1)
                        Select Case t.RawKind
                            Case VB.SyntaxKind.WhitespaceTrivia
                                If nextTrivia.IsComment Then
                                    FixedModifierLeadingTrivia = FixedModifierLeadingTrivia.Add(VBSpaceTrivia)
                                    FixedModifierLeadingTrivia = FixedModifierLeadingTrivia.Add(LineContinuation)
                                    FixedModifierLeadingTrivia = FixedModifierLeadingTrivia.Add(t)
                                    FixedModifierLeadingTrivia = FixedModifierLeadingTrivia.Add(nextTrivia)
                                    e.MoveNext()
                                Else
                                    FixedModifierLeadingTrivia = FixedModifierLeadingTrivia.Add(t)
                                End If
                            Case VB.SyntaxKind.EndOfLineTrivia
                                FixedModifierLeadingTrivia = FixedModifierLeadingTrivia.Add(t)
                            Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                                FixedModifierLeadingTrivia = FixedModifierLeadingTrivia.Add(t)
                            Case Else
                                If t.IsDirective Then
                                    If Attributes.Any And node.AttributeLists(0).GetLeadingTrivia.ContainsDirectiveTrivia Then
                                        MovedModifierLeadingTrivia = MovedModifierLeadingTrivia.Add(t)
                                    End If
                                End If
                        End Select
                    Next
                    Modifiers(0) = Modifiers(0).WithLeadingTrivia(FixedModifierLeadingTrivia)
                End If
                If Attributes.Any AndAlso Attributes.Last.GetTrailingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                    If Modifiers.Any AndAlso Modifiers(0).LeadingTrivia.ContainsEOLTrivia Then
                        If Modifiers(0).LeadingTrivia(0).IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            Modifiers(0) = Modifiers(0).WithLeadingTrivia(Modifiers(0).LeadingTrivia.RemoveAt(0))
                        End If
                    End If
                End If
                subOrFunctionStatement = Factory.FunctionStatement(
                                            Factory.List(Attributes),
                                            Factory.TokenList(Modifiers),
                                            methodNameToken,
                                            TypeParameterList,
                                            ParameterList,
                                            AsClause,
                                            handlesClause:=Nothing,
                                            implementsClauseOrNothing).
                                            With(FunctionStatementLeadingTrivia, FunctionStatementTrailingTrivia)
                If ReturnAttributes.Any AndAlso
                   (Attributes.Count = 0 OrElse Attributes(0).Attributes(0).Name.ToString = "Extension") AndAlso
                   node.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    subOrFunctionStatement = subOrFunctionStatement.WithPrependedLeadingTrivia(node.GetLeadingTrivia.ConvertTriviaList())
                End If
                subOrFunctionStatement = DirectCast(PrependStatementWithMarkedStatementTrivia(node, subOrFunctionStatement), VBS.MethodStatementSyntax)
                If s_usedStacks.Count > 0 Then
                    s_usedIdentifiers = DirectCast(s_usedStacks.Pop, Dictionary(Of String, SymbolTableEntry))
                End If

                If body Is Nothing Then
                    Return subOrFunctionStatement
                End If

                Dim blockvalue As List(Of VBS.StatementSyntax) = body.Value.ToList
                If blockvalue.Any AndAlso MovedModifierLeadingTrivia.Any Then
                    blockvalue(0) = blockvalue(0).WithPrependedLeadingTrivia(MovedModifierLeadingTrivia)
                End If
                Dim blockStatements As New SyntaxList(Of VBS.StatementSyntax)
                blockStatements = blockStatements.AddRange(blockvalue)
                Return Factory.FunctionBlock(subOrFunctionStatement,
                                             blockStatements,
                                             EndSubOrFunction.RemoveExtraLeadingEOL)
            End Function

            Public Overrides Function VisitOperatorDeclaration(node As CSS.OperatorDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim Attributes As New List(Of VBS.AttributeListSyntax)
                Dim ReturnAttributes As SyntaxList(Of VBS.AttributeListSyntax) = Nothing
                Dim FinalTrailingDirective As New SyntaxTriviaList
                Me.ConvertAndSplitAttributes(node.AttributeLists, Attributes, ReturnAttributes, FinalTrailingDirective)
                Dim visitor As New MethodBodyVisitor(_mSemanticModel, Me)
                Dim body As New SyntaxList(Of VBS.StatementSyntax)
                If node.Body IsNot Nothing Then
                    body = Factory.List(node.Body.Statements.SelectMany(Function(s As CSS.StatementSyntax) s.Accept(visitor)))
                ElseIf node.ExpressionBody IsNot Nothing Then
                    body = node.ExpressionBody.GetExpressionBodyStatements(Me)
                Else
                    Stop
                End If
                Dim parameterList As VBS.ParameterListSyntax = DirectCast(node.ParameterList?.Accept(Me), VBS.ParameterListSyntax).WithRestructuredingEOLTrivia
                Dim Modifiers As SyntaxTokenList = Factory.TokenList(ConvertModifiers(node.Modifiers, Me.IsModule, TokenContext.Member))
                Dim lSyntaxKind As CS.SyntaxKind = CS.CSharpExtensions.Kind(node.OperatorToken)

                If node.ParameterList?.Parameters.FirstOrDefault() Is Nothing Then
                    Throw New NotSupportedException("Operator overloads with no parameters aren't supported")
                End If
                Dim firstParam As CSS.ParameterSyntax = node.ParameterList?.Parameters.FirstOrDefault()
                Dim firstParameterIsString As Boolean = _mSemanticModel.GetTypeInfo(firstParam.Type).ConvertedType.SpecialType = SpecialType.System_String
                Select Case lSyntaxKind
                    Case CS.SyntaxKind.MinusMinusToken
                        Return Factory.EmptyStatement.WithLeadingTrivia(node.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=False, "C# -- operator not available in VB")).WithPrependedLeadingTrivia(node.GetLeadingTrivia.ConvertTriviaList()).WithConvertedTrailingTriviaFrom(node)
                    Case CS.SyntaxKind.PercentToken
                        Dim stmt As VBS.OperatorStatementSyntax = Factory.OperatorStatement(Factory.List(Attributes), Modifiers, ModKeyword, parameterList, Factory.SimpleAsClause(ReturnAttributes, DirectCast(node.ReturnType.Accept(Me), VBS.TypeSyntax)))
                        Return Factory.OperatorBlock(stmt, body).WithConvertedTriviaFrom(node)
                    Case CS.SyntaxKind.PlusPlusToken
                        Return Factory.EmptyStatement.WithLeadingTrivia(node.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=False, "C# ++ operator not available in VB")).WithPrependedLeadingTrivia(node.GetLeadingTrivia.ConvertTriviaList()).WithConvertedTrailingTriviaFrom(node)
                    Case Else
                        Dim OperatorToken As SyntaxToken = lSyntaxKind.GetComparisonOperatorToken(firstParameterIsString)
                        Dim stmt As VBS.OperatorStatementSyntax = Factory.OperatorStatement(Factory.List(Attributes), Factory.TokenList(Modifiers), OperatorToken, parameterList, Factory.SimpleAsClause(ReturnAttributes, DirectCast(node.ReturnType.Accept(Me), VBS.TypeSyntax)))
                        If FinalTrailingDirective.Any Then
                            stmt = stmt.WithAppendedTrailingTrivia(FinalTrailingDirective)
                        End If
                        Return Factory.OperatorBlock(stmt, body).WithConvertedTriviaFrom(node)
                End Select
            End Function

            Public Overrides Function VisitPropertyDeclaration(node As CSS.PropertyDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim csAccessors As New List(Of CSS.AccessorDeclarationSyntax)
                If node.AccessorList IsNot Nothing Then
                    csAccessors.AddRange(node.AccessorList.Accessors)
                End If
                Dim implementsClauseOrNothing As VBS.ImplementsClauseSyntax
                Dim identifierValueText As String = node.Identifier.ValueText
                Dim SimpleName As VBS.IdentifierNameSyntax = Factory.IdentifierName(identifierValueText)

                Dim TypeNode As VBS.TypeSyntax = DirectCast(node.Type.Accept(Me), VBS.TypeSyntax)
                If TypeNode.ToString.Equals("dynamic", StringComparison.OrdinalIgnoreCase) Then
                    TypeNode = Factory.PredefinedType(ObjectKeyword).WithTriviaFrom(TypeNode)
                End If
                Dim ClosingNodeBraces As SyntaxToken = node.GetBraces.Item2
                Dim EndPropertyStatement As VBS.EndBlockStatementSyntax = FactoryEndBlockStatement(VB.SyntaxKind.EndPropertyStatement, PropertyKeyword, CollectConvertedTokenTrivia(ClosingNodeBraces, GetLeading:=True, GetTrailing:=True))

                Dim TypeLeadingTrivia As New SyntaxTriviaList
                TypeLeadingTrivia = TypeLeadingTrivia.AddRange(node.Type.GetLeadingTrivia.ConvertTriviaList())
                If EndsWithSimilarTrivia(TypeLeadingTrivia, Factory.TriviaList(node.GetLeadingTrivia.ConvertTriviaList())) Then
                    TypeLeadingTrivia = New SyntaxTriviaList
                End If
                Dim IdString As String = ""
                Dim interfaceMembers As SeparatedSyntaxList(Of VBS.QualifiedNameSyntax)
                If node.ExplicitInterfaceSpecifier IsNot Nothing Then
                    node.ExplicitInterfaceSpecifier.Accept(Me).TypeSwitch(
                        Sub(ExplicitInterfaceIdentifier As VBS.QualifiedNameSyntax)
                            IdString = ExplicitInterfaceIdentifier.Right.ToString
                            Dim OpenParenIndex As Integer = IdString.IndexOf("(", StringComparison.Ordinal)
                            If OpenParenIndex > 0 Then
                                IdString = IdString.Substring(startIndex:=0, OpenParenIndex)
                            End If
                            interfaceMembers = interfaceMembers.Add(Factory.QualifiedName(ExplicitInterfaceIdentifier, SimpleName))
                        End Sub,
                        Sub(GenericName As VBS.GenericNameSyntax)
                            IdString = GenericName.Identifier.ToString
                            interfaceMembers = interfaceMembers.Add(Factory.QualifiedName(GenericName, SimpleName))
                        End Sub,
                        Sub(IdentifierName As VBS.IdentifierNameSyntax)
                            IdString = IdentifierName.Identifier.ToString
                            interfaceMembers = interfaceMembers.Add(Factory.QualifiedName(IdentifierName, SimpleName))
                        End Sub,
                        Sub(__ As SyntaxNode)
                            Throw New NotImplementedException($"{__.GetType().FullName} not implemented!")
                        End Sub)
                End If

                Dim propertyNameToken As SyntaxToken
                Dim propertyStatement As VBS.PropertyStatementSyntax
                If node.ExplicitInterfaceSpecifier Is Nothing Then
                    propertyNameToken = GenerateSafeVBToken(node.Identifier, node, _mSemanticModel)
                    Dim PropertySymbol As IPropertySymbol = CType(_mSemanticModel.GetDeclaredSymbol(node), IPropertySymbol)
                    implementsClauseOrNothing = If(PropertySymbol Is Nothing, Nothing, Me.CreateImplementsClauseSyntaxOrNull(PropertySymbol, propertyNameToken))
                Else
                    propertyNameToken = Factory.Identifier($"{IdString}_{identifierValueText}")
                    implementsClauseOrNothing = Factory.ImplementsClause(interfaceMembers)
                End If
                propertyNameToken = propertyNameToken.WithTrailingTrivia(VBSpaceTrivia)
                Dim Attributes As New List(Of VBS.AttributeListSyntax)
                Dim ReturnAttributes As SyntaxList(Of VBS.AttributeListSyntax) = Nothing
                Dim FinalTrailingDirective As New SyntaxTriviaList
                Me.ConvertAndSplitAttributes(node.AttributeLists, Attributes, ReturnAttributes, FinalTrailingDirective)
                If FinalTrailingDirective.Any Then
                    Stop
                End If
                Dim isIterator As Boolean = False
                Dim accessors As New List(Of VBS.AccessorBlockSyntax)
                Dim Statements As SyntaxList(Of VBS.StatementSyntax)
                Dim CSModifiers As SyntaxTokenList = node.Modifiers
                Dim AsClause As VBS.SimpleAsClauseSyntax = Factory.SimpleAsClause(ReturnAttributes, TypeNode.WithoutTrivia)
                Dim Context As TokenContext = TokenContext.Property
                Dim LocalIsModule As Boolean = Me.IsModule OrElse node.Parent.IsKind(CS.SyntaxKind.CompilationUnit)
                If node.ExpressionBody IsNot Nothing Then
                    Dim ExpressionSyntaxNode As VB.VisualBasicSyntaxNode = node.ExpressionBody.Expression.Accept(Me).WithConvertedLeadingTriviaFrom(node.ExpressionBody.Expression)
                    If TypeOf ExpressionSyntaxNode Is VBS.ThrowStatementSyntax Then
                        Statements = Factory.SingletonList(Of VBS.StatementSyntax)(DirectCast(ExpressionSyntaxNode, VBS.ThrowStatementSyntax).WithConvertedTriviaFrom(node.ExpressionBody))
                    ElseIf TypeOf ExpressionSyntaxNode Is VBS.SingleLineIfStatementSyntax Then
                        Dim IfStatement As VBS.SingleLineIfStatementSyntax = DirectCast(ExpressionSyntaxNode, VBS.SingleLineIfStatementSyntax).WithTrailingEOL
                        Dim ReturnStatement As VBS.ReturnStatementSyntax = Factory.ReturnStatement(DirectCast(IfStatement.Condition, VBS.BinaryExpressionSyntax).Left).
                                                WithLeadingTrivia(IfStatement.Condition.GetLeadingTrivia)
                        ReturnStatement = ReturnStatement.RelocateDirectivesInLeadingTrivia
                        Dim StatementList As New List(Of VBS.StatementSyntax) From {
                            IfStatement,
                            ReturnStatement
                        }
                        Statements = ReplaceStatementsWithMarkedStatements(node, StatementList)
                    ElseIf TypeOf ExpressionSyntaxNode Is VBS.AssignmentStatementSyntax Then
                        Dim AssignmentStatement As VBS.AssignmentStatementSyntax = DirectCast(ExpressionSyntaxNode, VBS.AssignmentStatementSyntax).WithTrailingEOL
                        Dim ReturnStatement As VBS.ReturnStatementSyntax = Factory.ReturnStatement(AssignmentStatement.Left.WithoutLeadingTrivia).
                                                WithLeadingTrivia(AssignmentStatement.GetLeadingTrivia)
                        ReturnStatement = ReturnStatement.RelocateDirectivesInLeadingTrivia
                        Dim StatementList As New List(Of VBS.StatementSyntax) From {
                            AssignmentStatement,
                            ReturnStatement
                        }
                        Statements = ReplaceStatementsWithMarkedStatements(node, StatementList)
                    ElseIf TypeOf ExpressionSyntaxNode Is VBS.ExpressionSyntax Then
                        Dim ReturnedExpression As VBS.ExpressionSyntax = DirectCast(ExpressionSyntaxNode, VBS.ExpressionSyntax)
                        If ReturnedExpression Is Nothing Then
                            ' ref expression
                            Dim RefExpression As CSS.ExpressionSyntax = node.ExpressionBody.Expression
                            If RefExpression Is Nothing Then
                                Stop
                            Else
                                ReturnedExpression = DirectCast(DirectCast(RefExpression, CSS.RefExpressionSyntax).Expression.Accept(Me), VBS.ExpressionSyntax)
                            End If
                        Else
                            ReturnedExpression = ReturnedExpression.WithConvertedTriviaFrom(node.ExpressionBody)
                        End If

                        Dim ReturnStatement As VBS.ReturnStatementSyntax = Factory.ReturnStatement(ReturnedExpression.WithLeadingTrivia(VBSpaceTrivia)).
                                                WithLeadingTrivia(ReturnedExpression.GetLeadingTrivia)
                        ReturnStatement = ReturnStatement.RelocateDirectivesInLeadingTrivia
                        Statements = ReplaceOneStatementWithMarkedStatements(node.ExpressionBody, ReturnStatement)
                    Else
                        Stop
                        Throw UnreachableException
                    End If
                    accessors.Add(Factory.AccessorBlock(VB.SyntaxKind.GetAccessorBlock,
                                                        Factory.GetAccessorStatement.WithTrailingEOL,
                                                        Statements,
                                                        Factory.EndGetStatement(EndKeyword.WithTrailingTrivia(VBSpaceTrivia), GetKeyword)))
                Else
                    If node.AccessorList IsNot Nothing Then
                        If (Not node.Modifiers.Contains(CS.SyntaxKind.AbstractKeyword)) AndAlso csAccessors.Count = 2 AndAlso
                            csAccessors(0).Body Is Nothing AndAlso csAccessors(0).ExpressionBody Is Nothing AndAlso
                            csAccessors(1).Body Is Nothing AndAlso csAccessors(1).ExpressionBody Is Nothing Then
                            Dim GetModifiers As List(Of SyntaxToken) = ConvertModifiers(csAccessors(0).Modifiers, LocalIsModule, Context).ToList
                            Dim SetModifiers As List(Of SyntaxToken) = ConvertModifiers(csAccessors(1).Modifiers, LocalIsModule, Context).ToList
                            Dim propertyStatementLeadingTrivia As New SyntaxTriviaList
                            Dim GetModifier0 As String = If(GetModifiers.Any, GetModifiers(0).ValueText, "")
                            Dim SetModifier0 As String = If(SetModifiers.Any, SetModifiers(0).ValueText, "")
                            If GetModifier0 <> SetModifier0 Then
                                ' Handle
                                ' public string BuyerId { get; protected set; }
                                ' Dim _buyerId As String
                                If GetModifiers.Any AndAlso GetModifiers(0).LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                    propertyStatementLeadingTrivia = propertyStatementLeadingTrivia.AddRange(GetModifiers(0).LeadingTrivia)
                                    GetModifiers(0) = GetModifiers(0).WithoutTrivia
                                End If
                                If SetModifiers.Any AndAlso SetModifiers?(0).LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                    propertyStatementLeadingTrivia = propertyStatementLeadingTrivia.AddRange(SetModifiers(0).LeadingTrivia)
                                    SetModifiers(0) = SetModifiers(0).WithoutTrivia
                                End If
                                Dim newVariableToken As SyntaxToken = Factory.Identifier($"_{Char.ToLower(identifierValueText.Chars(0), Globalization.CultureInfo.InvariantCulture)}{identifierValueText.Substring(startIndex:=1)}")
                                Dim NewModifiedIdentifier As VBS.ModifiedIdentifierSyntax = Factory.ModifiedIdentifier(newVariableToken)
                                Dim declarators As SeparatedSyntaxList(Of VBS.VariableDeclaratorSyntax) =
                                                        Factory.SingletonSeparatedList(
                                                            Factory.VariableDeclarator(
                                                                Factory.SingletonSeparatedList(NewModifiedIdentifier),
                                                                AsClause,
                                                                initializer:=Nothing)
                                                        )
                                Dim DimStatement As VBS.StatementSyntax = Factory.LocalDeclarationStatement(Factory.TokenList(PrivateKeyword.WithTrailingTrivia(VBSpaceTrivia)), declarators).WithConvertedLeadingTriviaFrom(node).WithTrailingEOL
                                Dim StatementWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                                StatementWithIssues.AddMarker(DimStatement, StatementHandlingOption.PrependStatement, AllowDuplicates:=False)
                                Dim stmt As VBS.AccessorStatementSyntax = Factory.GetAccessorStatement(Nothing, Factory.TokenList(GetModifiers), parameterList:=Nothing)
                                Dim body As SyntaxList(Of VBS.StatementSyntax) = Factory.SingletonList(Of VBS.StatementSyntax)(Factory.ReturnStatement(Factory.IdentifierName(newVariableToken)).WithTrailingEOL)
                                accessors.Add(Factory.AccessorBlock(VB.SyntaxKind.GetAccessorBlock, stmt.WithTrailingEOL, body, Factory.EndGetStatement(EndKeyword.WithTrailingTrivia(VBSpaceTrivia), GetKeyword).WithTrailingEOL))

                                Dim ValueParam As VBS.ParameterSyntax = Factory.Parameter(ValueModifiedIdentifier).WithAsClause(AsClause)
                                stmt = Factory.SetAccessorStatement(Nothing, Factory.TokenList(SetModifiers), Factory.ParameterList(Factory.SingletonSeparatedList(ValueParam)))
                                Dim valueExpression As VBS.ExpressionSyntax = Factory.ParseExpression(newVariableToken.ValueText)
                                body = Factory.SingletonList(Of VBS.StatementSyntax)(Factory.SimpleAssignmentStatement(valueExpression, Factory.IdentifierName(ValueModifiedIdentifier.Identifier)).WithTrailingEOL)
                                accessors.Add(Factory.AccessorBlock(VB.SyntaxKind.SetAccessorBlock, stmt.WithTrailingEOL, body, Factory.EndSetStatement(EndKeyword.WithTrailingTrivia(VBSpaceTrivia), SetKeyword).WithTrailingEOL))

                                propertyStatement = Factory.PropertyStatement(Factory.List(Attributes),
                                                                              Factory.TokenList(ConvertModifiers(CSModifiers, LocalIsModule, Context)),
                                                                              propertyNameToken,
                                                                              parameterList:=Nothing,
                                                                              AsClause,
                                                                              initializer:=Nothing,
                                                                              implementsClause:=Nothing)
                                Return Factory.PropertyBlock(propertyStatement.WithPrependedLeadingTrivia(propertyStatementLeadingTrivia).WithTrailingEOL,
                                                             Factory.List(accessors),
                                                             EndPropertyStatement.WithConvertedTrailingTriviaFrom(node)).
                                        RestructureAttributesAndModifiers(Attributes.Any, HasModifiers:=True)

                            End If

                        End If
                        For Each a As CSS.AccessorDeclarationSyntax In csAccessors
                            Dim _isIterator As Boolean
                            accessors.Add(Me.ConvertAccessor(a, LocalIsModule, _isIterator))
                            isIterator = isIterator Or _isIterator
                        Next
                    End If
                End If

                Dim IsWriteOnly As Boolean = False
                If node.AccessorList IsNot Nothing AndAlso csAccessors.Count = 1 Then
                    Select Case csAccessors(0).Keyword.RawKind
                        Case CS.SyntaxKind.GetKeyword
                            CSModifiers = CSModifiers.Add(CSReadOnlyKeyword)
                        Case CS.SyntaxKind.SetKeyword
                            IsWriteOnly = True
                        Case Else
                            _reportException?.Invoke(UnreachableException)
                    End Select

                End If
                ' TODO find better way to find out if we are in interface
                If node.IsParentKind(CS.SyntaxKind.InterfaceDeclaration) Then
                    Context = TokenContext.InterfaceOrModule
                End If
                Dim Modifiers As List(Of SyntaxToken) = ConvertModifiers(CSModifiers, LocalIsModule, Context).ToList
                If isIterator Then
                    Modifiers.Add(IteratorKeyword)
                End If
                If IsWriteOnly Then
                    Modifiers.Add(WriteOnlyKeyword)
                End If

                If node.AccessorList Is Nothing Then
                    Modifiers.Add(ReadOnlyKeyword)
                End If

                Dim Initializer As VBS.EqualsValueSyntax = If(node.Initializer Is Nothing, Nothing, Factory.EqualsValue(DirectCast(node.Initializer.Value.Accept(Me), VBS.ExpressionSyntax)))
                Dim Keyword As SyntaxToken

                If Modifiers.Count = 0 Then
                    Keyword = PropertyKeyword.WithLeadingTrivia(TypeLeadingTrivia)
                    TypeLeadingTrivia = New SyntaxTriviaList
                Else
                    Keyword = PropertyKeyword
                    If TypeLeadingTrivia.Any Then
                        TypeLeadingTrivia = TypeLeadingTrivia.Insert(0, VBEOLTrivia)
                    End If
                End If
                Dim PrependedTrivia As SyntaxTriviaList = DedupLeadingTrivia(node, Keyword, Attributes, Modifiers)
                If Attributes.Any AndAlso Modifiers.Any AndAlso Modifiers(0).LeadingTrivia.ContainsCommentTrivia Then
                    Dim attriuteTrailingTrivia As SyntaxTriviaList = Attributes.Last.GetTrailingTrivia
                    If attriuteTrailingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                        If attriuteTrailingTrivia.Count = 1 Then
                            attriuteTrailingTrivia = attriuteTrailingTrivia.InsertRange(0, {VBSpaceTrivia, LineContinuation})
                        ElseIf attriuteTrailingTrivia(attriuteTrailingTrivia.Count - 2).IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                            attriuteTrailingTrivia = attriuteTrailingTrivia.InsertRange(attriuteTrailingTrivia.Count - 2, {VBSpaceTrivia, LineContinuation})
                        Else
                            attriuteTrailingTrivia = attriuteTrailingTrivia.InsertRange(0, {VBSpaceTrivia, LineContinuation})
                        End If
                    Else
                        Stop
                    End If
                    Modifiers(0) = Modifiers(0).AdjustTokenLeadingTrivia()
                    Attributes(Attributes.Count - 1) = Attributes.Last.WithTrailingTrivia(attriuteTrailingTrivia)
                End If
                propertyStatement = Factory.PropertyStatement(Factory.List(Attributes),
                                                                Factory.TokenList(Modifiers),
                                                                Keyword,
                                                                propertyNameToken.WithTrailingTrivia(VBSpaceTrivia),
                                                                parameterList:=Nothing,
                                                                AsClause,
                                                                Initializer.WithTrailingEOL,
                                                                implementsClauseOrNothing
                                                                ).WithPrependedLeadingTrivia(PrependedTrivia).WithTrailingEOL
                Dim StmtList As SyntaxList(Of VBS.StatementSyntax) = ReplaceOneStatementWithMarkedStatements(node, propertyStatement)
                Dim AddedLeadingTrivia As New SyntaxTriviaList
                Select Case StmtList.Count
                    Case 1
                        If TypeOf StmtList(0) Is VBS.EmptyStatementSyntax Then
                            AddedLeadingTrivia = AddedLeadingTrivia.AddRange(StmtList(0).GetLeadingTrivia)
                        End If
                    Case 2
                        AddedLeadingTrivia = AddedLeadingTrivia.AddRange(StmtList(0).GetLeadingTrivia)
                    Case Else

                End Select
                If node.AccessorList IsNot Nothing Then
                    Dim BodyOrExpressionBodyIsNothing As Boolean = True
                    For Each a As CSS.AccessorDeclarationSyntax In csAccessors
                        If a.Body IsNot Nothing OrElse a.ExpressionBody IsNot Nothing Then
                            BodyOrExpressionBodyIsNothing = False
                            Exit For
                        End If
                    Next
                    If BodyOrExpressionBodyIsNothing Then
                        If AddedLeadingTrivia.Any Then
                            Return propertyStatement.WithLeadingTrivia(AddedLeadingTrivia).WithConvertedTrailingTriviaFrom(node).WithTrailingEOL
                        End If
                        Return propertyStatement.WithConvertedTriviaFrom(node).WithTrailingEOL
                    End If
                End If
                If AddedLeadingTrivia.Any Then
                    Return Factory.PropertyBlock(propertyStatement.WithLeadingTrivia(AddedLeadingTrivia).WithTrailingEOL, Factory.List(accessors))
                End If

                Dim AccessorOpenBraceTrivia As SyntaxTriviaList = CollectConvertedTokenTrivia(node.AccessorList.GetBraces.Item1, GetLeading:=True, GetTrailing:=True)
                Dim AccessorClosingBraceTrivia As SyntaxTriviaList = CollectConvertedTokenTrivia(node.AccessorList.GetBraces.Item2, GetLeading:=True, GetTrailing:=True)

                EndPropertyStatement = Factory.EndPropertyStatement(EndKeyword.WithTrailingTrivia(VBSpaceTrivia), PropertyKeyword).
                                WithConvertedTriviaFrom(ClosingNodeBraces).
                                WithPrependedLeadingTrivia(AccessorClosingBraceTrivia).WithTrailingEOL
                accessors(0) = accessors(0).WithPrependedLeadingTrivia(AccessorOpenBraceTrivia).WithTrailingEOL
                Return Factory.PropertyBlock(propertyStatement.WithTrailingEOL,
                                               Factory.List(accessors),
                                               EndPropertyStatement.WithTrailingEOL).
                                        WithAppendedTrailingTrivia(TypeLeadingTrivia).
                                        RestructureAttributesAndModifiers(Attributes.Any, Modifiers.Any).NormalizeWhitespaceEx(useDefaultCasing:=True).WithTrailingEOL

            End Function

        End Class

    End Class

End Namespace
