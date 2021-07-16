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

Namespace CSharpToVBConverter.CSharpToVBVisitors

    Partial Public Class CSharpConverter

        Partial Friend Class NodesVisitor
            Inherits CS.CSharpSyntaxVisitor(Of VB.VisualBasicSyntaxNode)

            Private Shared Function CreateImplementsClauseSyntax(implementors As IEnumerable(Of ISymbol), id As SyntaxToken) As VBS.ImplementsClauseSyntax
                Return Factory.ImplementsClause(implementors.Select(Function(x)
                                                                        Dim fullyQualifiedName As VBS.NameSyntax = TryCast(x.ContainingSymbol, INamedTypeSymbol).GetFullyQualifiedNameSyntax()
                                                                        Dim qualifiedNameSyntax As VBS.QualifiedNameSyntax = TryCast(fullyQualifiedName, VBS.QualifiedNameSyntax)
                                                                        If qualifiedNameSyntax IsNot Nothing Then
                                                                            Dim left As String = qualifiedNameSyntax.Left.ToString
                                                                            If left.StartsWith("System.", StringComparison.Ordinal) Then
                                                                                left = left.WithoutLeadingSystemDot
                                                                                If left.Any Then
                                                                                    fullyQualifiedName = Factory.QualifiedName(Factory.IdentifierName(left), qualifiedNameSyntax.Right)
                                                                                End If
                                                                            End If
                                                                        End If
                                                                        Return Factory.QualifiedName(fullyQualifiedName, Factory.IdentifierName(id))
                                                                    End Function).ToArray())
            End Function

            Private Shared Function GetEventLookupExpr(eventNameToken As SyntaxToken) As VBS.InvocationExpressionSyntax
                Dim argList As VBS.ArgumentListSyntax = Factory.ParseArgumentList($"(""{eventNameToken.Text}"")")
                Dim eventName As VBS.IdentifierNameSyntax = Factory.IdentifierName($"_{Char.ToLowerInvariant(eventNameToken.Text(0))}{eventNameToken.Text.Substring(1)}")
                ' _buttonClicked("ButtonClicked")
                Return Factory.InvocationExpression(eventName, argList)
            End Function

            Private Shared Function GetEventLookupExpr(eventName As String) As VBS.InvocationExpressionSyntax
                Dim argList As VBS.ArgumentListSyntax = Factory.ParseArgumentList($"(""{Char.ToUpperInvariant(eventName(1))}{eventName.Substring(2)}"")")
                ' _buttonClicked("ButtonClicked")
                Return Factory.InvocationExpression(Factory.IdentifierName(eventName), argList)
            End Function

            Private Shared Function MemberNameWithoutDots(n As String) As String
                Return n.Split("."c).Last().RemoveBrackets
            End Function

            Private Function ConvertAccessor(node As CSS.AccessorDeclarationSyntax, localIsModule As Boolean, ByRef isIterator As Boolean) As VBS.AccessorBlockSyntax
                Dim blockKind As VB.SyntaxKind
                Dim stmt As VBS.AccessorStatementSyntax
                Dim statements As SyntaxList(Of VBS.StatementSyntax) = Factory.List(Of VBS.StatementSyntax)()
                isIterator = False
                If node.Body IsNot Nothing Then
                    Dim methodBodyVisitor As New MethodBodyVisitor(_semanticModel, Me)
                    statements = node.Body.GetBodyStatements(methodBodyVisitor)
                    isIterator = methodBodyVisitor.IsIterator
                ElseIf node.ExpressionBody IsNot Nothing Then
                    statements = node.ExpressionBody.GetExpressionBodyStatements(False, Me)
                End If
                Dim attributes As SyntaxList(Of VBS.AttributeListSyntax) = Factory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))
                Dim modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, localIsModule, TokenContext.Local).ToList
                Dim parent As CSS.BasePropertyDeclarationSyntax = DirectCast(node.Parent.Parent, CSS.BasePropertyDeclarationSyntax)
                Dim valueParam As VBS.ParameterSyntax
                Dim endStmt As VBS.EndBlockStatementSyntax
                Select Case CS.CSharpExtensions.Kind(node)
                    Case CS.SyntaxKind.GetAccessorDeclaration
                        blockKind = VB.SyntaxKind.GetAccessorBlock
                        stmt = Factory.GetAccessorStatement(attributes, Factory.TokenList(modifiers), parameterList:=Nothing)
                        endStmt = Factory.EndGetStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), GetKeyword)
                    Case CS.SyntaxKind.SetAccessorDeclaration, CS.SyntaxKind.InitAccessorDeclaration
                        blockKind = VB.SyntaxKind.SetAccessorBlock
                        valueParam = Factory.Parameter(ValueModifiedIdentifier).
                        WithAsClause(Factory.SimpleAsClause(DirectCast(parent.Type.Accept(Me), VBS.TypeSyntax).
                        WithLeadingTrivia(SpaceTrivia)))
                        stmt = Factory.SetAccessorStatement(attributes, Factory.TokenList(modifiers), Factory.ParameterList(Factory.SingletonSeparatedList(valueParam)))
                        endStmt = Factory.EndSetStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), SetKeyword)
                    Case Else
                        Throw New NotSupportedException()
                End Select
                Return Factory.AccessorBlock(blockKind,
                stmt.WithConvertedTriviaFrom(node.Body.GetBraces.Item1).WithTrailingEol,
                Me.AdjustUsingIfNeeded(statements),
                endStmt.WithConvertedTriviaFrom(node.Body.GetBraces.Item2)
                ).WithConvertedTriviaFrom(node)
            End Function

            Private Function ConvertEventAccessor(node As CSS.AccessorDeclarationSyntax, localIsModule As Boolean, eventLoopupExpr As VBS.InvocationExpressionSyntax, eventType As VBS.TypeSyntax) As VBS.AccessorBlockSyntax
                Dim blockKind As VB.SyntaxKind
                Dim stmt As VBS.AccessorStatementSyntax
                Dim statements As SyntaxList(Of VBS.StatementSyntax) = Factory.List(Of VBS.StatementSyntax)()
                If node.Body IsNot Nothing Then
                    statements = node.Body.GetEventBodyStatements(Me, New MethodBodyVisitor(_semanticModel, Me), eventLoopupExpr, eventType)
                ElseIf node.ExpressionBody IsNot Nothing Then
                    statements = node.ExpressionBody.GetExpressionBodyStatements(False, Me)
                End If
                Dim attributes As SyntaxList(Of VBS.AttributeListSyntax) = Factory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))
                Dim modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, localIsModule, TokenContext.Local).ToList
                Dim parent As CSS.BasePropertyDeclarationSyntax = DirectCast(node.Parent.Parent, CSS.BasePropertyDeclarationSyntax)
                Dim valueParam As VBS.ParameterSyntax
                Dim endStmt As VBS.EndBlockStatementSyntax
                Select Case CS.CSharpExtensions.Kind(node)
                    Case CS.SyntaxKind.AddAccessorDeclaration
                        blockKind = VB.SyntaxKind.AddHandlerAccessorBlock
                        valueParam = Factory.Parameter(ValueModifiedIdentifier).
                        WithAsClause(Factory.SimpleAsClause(DirectCast(parent.Type.Accept(Me), VBS.TypeSyntax)))
                        stmt = Factory.AddHandlerAccessorStatement(attributes, Factory.TokenList(modifiers), Factory.ParameterList(Factory.SingletonSeparatedList(valueParam)))
                        endStmt = Factory.EndAddHandlerStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), AddHandlerKeyword)
                    Case CS.SyntaxKind.RemoveAccessorDeclaration
                        blockKind = VB.SyntaxKind.RemoveHandlerAccessorBlock
                        valueParam = Factory.Parameter(ValueModifiedIdentifier).
                        WithAsClause(Factory.SimpleAsClause(DirectCast(parent.Type.Accept(Me), VBS.TypeSyntax)))
                        stmt = Factory.RemoveHandlerAccessorStatement(attributes, Factory.TokenList(modifiers), Factory.ParameterList(Factory.SingletonSeparatedList(valueParam)))
                        endStmt = Factory.EndRemoveHandlerStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), RemoveHandlerKeyword)
                    Case Else
                        Throw New NotSupportedException()
                End Select
                Return Factory.AccessorBlock(blockKind,
                stmt.WithConvertedTriviaFrom(node.Body.GetBraces.Item1).WithTrailingEol,
                Me.AdjustUsingIfNeeded(statements),
                endStmt.WithConvertedTriviaFrom(node.Body.GetBraces.Item2)
                ).WithConvertedTriviaFrom(node)
            End Function

            Private Function CreateImplementsClauseSyntaxOrNull(memberInfo As ISymbol, ByRef id As SyntaxToken) As VBS.ImplementsClauseSyntax
                Dim originalId As SyntaxToken = id
                Dim explicitImplementors As ImmutableArray(Of ISymbol) = memberInfo.ExplicitInterfaceImplementations()
                If explicitImplementors.Any() Then
                    Dim memberNames As ILookup(Of String, ISymbol) = memberInfo.ContainingType.GetMembers().ToLookup(Function(s) MemberNameWithoutDots(s.Name), StringComparer.OrdinalIgnoreCase)
                    Dim explicitMemberName As String = MemberNameWithoutDots(memberInfo.Name)
                    Dim hasDuplicateNames As Boolean = memberNames(explicitMemberName).Count() > 1
                    If hasDuplicateNames Then
                        id = Factory.Identifier(GenerateUniqueName(explicitMemberName, Function(n) Not memberNames.Contains(n) AndAlso _addedNames.Add(n)))
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

            Private Function NeedEventHandlerList(node As CSS.EventFieldDeclarationSyntax) As Boolean
                Dim result As Boolean = False
                Dim eventDeclarations As List(Of CSS.EventDeclarationSyntax) = node.Parent.DescendantNodes().OfType(Of CSS.EventDeclarationSyntax)().ToList()
                For Each eventDecl As CSS.EventDeclarationSyntax In eventDeclarations
                    If eventDecl.Type.ToString() = node.Declaration.Type.ToString() Then
                        For Each accessor As CSS.AccessorDeclarationSyntax In eventDecl.AccessorList.Accessors
                            For Each stmt As CSS.StatementSyntax In accessor.Body.Statements
                                Dim cssExpr As CSS.ExpressionStatementSyntax = TryCast(stmt, CSS.ExpressionStatementSyntax)
                                If cssExpr IsNot Nothing Then
                                    Dim assignStmt As CSS.AssignmentExpressionSyntax = TryCast(cssExpr.Expression, CSS.AssignmentExpressionSyntax)
                                    If assignStmt IsNot Nothing Then
                                        If assignStmt.Kind = CS.SyntaxKind.SimpleAssignmentExpression AndAlso
                                               assignStmt.Right.Kind() = CS.SyntaxKind.NullLiteralExpression Then
                                            Dim left As CSS.MemberAccessExpressionSyntax = TryCast(assignStmt.Left, CSS.MemberAccessExpressionSyntax)
                                            If left IsNot Nothing Then
                                                _eventList.TryAdd(GetEventLookupExpr(left.Name.Identifier.Text).ToString(), True)
                                            Else
                                                Stop
                                            End If
                                            result = True
                                            Exit For
                                        ElseIf assignStmt.Kind = CS.SyntaxKind.AddAssignmentExpression Then
                                            Continue For
                                        ElseIf assignStmt.Kind = CS.SyntaxKind.SubtractAssignmentExpression Then
                                            Continue For
                                        End If
                                    End If
                                End If
                            Next
                        Next
                    End If
                Next
                Return result
            End Function

            Friend Function ConvertAndSplitAttributes(csAttributeLists As SyntaxList(Of CSS.AttributeListSyntax), <Out> ByRef returnAttributes As SyntaxList(Of VBS.AttributeListSyntax), ByRef finalDirectiveTrivia As SyntaxTriviaList) As List(Of VBS.AttributeListSyntax)
                Dim attributeLists As New List(Of VBS.AttributeListSyntax)
                Dim retAttr As New List(Of VBS.AttributeListSyntax)()
                Dim firstAttribute As Boolean = True
                For Each e As IndexClass(Of CSS.AttributeListSyntax) In csAttributeLists.WithIndex
                    Dim attrList As CSS.AttributeListSyntax = e.Value
                    If attrList.Target Is Nothing OrElse Not attrList.Target.Identifier.IsKind(CS.SyntaxKind.ReturnKeyword) Then
                        Dim item As VBS.AttributeListSyntax = DirectCast(attrList.Accept(Me), VBS.AttributeListSyntax).RemoveExtraLeadingEol
                        If firstAttribute Then
                            firstAttribute = False
                            If Not e.IsLast Then
                                item = item.WithTrailingTrivia(item.GetTrailingTrivia.WithLastLineContinuation)
                            End If
                        Else
                            Dim itemLeadingTrivia As SyntaxTriviaList = item.GetLeadingTrivia
                            If itemLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                Dim newLeadingTrivia As New SyntaxTriviaList
                                Dim firstComment As Boolean = True
                                Dim needWhiteSpace As Boolean = True
                                Dim needLineContinuation As Boolean = True
                                Dim leadingIndex As Integer = 0
                                For Each vbSyntaxTrivia As SyntaxTrivia In itemLeadingTrivia
                                    Dim nextTrivia As SyntaxTrivia = itemLeadingTrivia.GetForwardTriviaOrDefault(leadingIndex, lookaheadCount:=1)
                                    Select Case vbSyntaxTrivia.RawKind
                                        Case VB.SyntaxKind.WhitespaceTrivia
                                            If nextTrivia.IsKind(VB.SyntaxKind.CommentTrivia) Then
                                                newLeadingTrivia = newLeadingTrivia.AddRange(SpaceLineContinue)
                                                firstComment = False
                                            End If
                                            newLeadingTrivia = newLeadingTrivia.Add(vbSyntaxTrivia)
                                            needWhiteSpace = False
                                        Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                                            If firstComment Then
                                                firstComment = False
                                                If needWhiteSpace Then
                                                    needWhiteSpace = False
                                                    newLeadingTrivia = newLeadingTrivia.Add(SpaceTrivia)
                                                End If
                                                newLeadingTrivia = newLeadingTrivia.Add(LineContinuation)
                                                newLeadingTrivia = newLeadingTrivia.Add(SpaceTrivia)
                                                needLineContinuation = False
                                            End If
                                            newLeadingTrivia = newLeadingTrivia.Add(vbSyntaxTrivia)
                                        Case VB.SyntaxKind.IfDirectiveTrivia
                                            If firstComment Then
                                                firstComment = False
                                                If needWhiteSpace Then
                                                    needWhiteSpace = False
                                                    newLeadingTrivia = newLeadingTrivia.Add(SpaceTrivia)
                                                End If
                                                newLeadingTrivia = newLeadingTrivia.Add(LineContinuation)
                                                newLeadingTrivia = newLeadingTrivia.Add(SpaceTrivia)
                                                needLineContinuation = False
                                            End If
                                            newLeadingTrivia = newLeadingTrivia.Add(Factory.CommentTrivia($"Directive not allowed here {vbSyntaxTrivia}"))
                                        Case VB.SyntaxKind.ElseDirectiveTrivia
                                            If firstComment Then
                                                firstComment = False
                                                If needWhiteSpace Then
                                                    needWhiteSpace = False
                                                    newLeadingTrivia = newLeadingTrivia.Add(SpaceTrivia)
                                                End If
                                                newLeadingTrivia = newLeadingTrivia.Add(LineContinuation)
                                                newLeadingTrivia = newLeadingTrivia.Add(SpaceTrivia)
                                                needLineContinuation = False
                                            End If
                                            newLeadingTrivia = newLeadingTrivia.Add(Factory.CommentTrivia($"Directive not allowed here {vbSyntaxTrivia}"))
                                        Case VB.SyntaxKind.ElseIfDirectiveTrivia
                                            If firstComment Then
                                                firstComment = False
                                                If needWhiteSpace Then
                                                    needWhiteSpace = False
                                                    newLeadingTrivia = newLeadingTrivia.Add(SpaceTrivia)
                                                End If
                                                newLeadingTrivia = newLeadingTrivia.Add(LineContinuation)
                                                newLeadingTrivia = newLeadingTrivia.Add(SpaceTrivia)
                                                needLineContinuation = False
                                            End If
                                            newLeadingTrivia = newLeadingTrivia.Add(Factory.CommentTrivia($"Directive not allowed here {vbSyntaxTrivia}"))
                                        Case VB.SyntaxKind.EndIfDirectiveTrivia
                                            finalDirectiveTrivia = finalDirectiveTrivia.Add(vbSyntaxTrivia)
                                        Case VB.SyntaxKind.EndOfLineTrivia
                                            If needWhiteSpace Then
                                                needWhiteSpace = False
                                                newLeadingTrivia = newLeadingTrivia.Add(SpaceTrivia)
                                            End If
                                            If needLineContinuation Then
                                                newLeadingTrivia = newLeadingTrivia.Add(LineContinuation)
                                                needLineContinuation = False
                                                needWhiteSpace = True
                                            End If
                                            newLeadingTrivia = newLeadingTrivia.Add(vbSyntaxTrivia)
                                        Case Else
                                            needWhiteSpace = True
                                    End Select
                                    leadingIndex += 1
                                Next
                                item = item.WithLeadingTrivia(newLeadingTrivia)
                            End If
                        End If
                        attributeLists.Add(item)
                    Else
                        ' Remove trailing CrLf from return attributes
                        retAttr.Add(DirectCast(attrList.Accept(Me).With({SpaceTrivia}, {SpaceTrivia}), VBS.AttributeListSyntax))
                    End If
                Next
                returnAttributes = Factory.List(retAttr)
                Return attributeLists
            End Function

            Public Function AdjustUsingIfNeeded(blockStatements As SyntaxList(Of VBS.StatementSyntax)) As SyntaxList(Of VBS.StatementSyntax)
                If _neededEndUsingCount > 0 Then
                    ' ReSharper disable once RedundantAssignment
                    For i As Integer = 1 To _neededEndUsingCount
                        blockStatements = blockStatements.Add(Factory.EndUsingStatement)
                    Next
                    _neededEndUsingCount = 0
                End If
                Return blockStatements
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
                Dim attrs As SyntaxList(Of VBS.AttributeListSyntax) = Factory.List(
                                        node?.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))
                Dim initializer As VBS.ExpressionStatementSyntax = Nothing
                If node.Initializer IsNot Nothing Then
                    initializer = DirectCast(node.Initializer.Accept(Me), VBS.ExpressionStatementSyntax)
                End If
                Dim vbStatements As New Dictionary(Of CSS.LocalFunctionStatementSyntax, VBS.StatementSyntax)
                Dim methodBodyVisitor As New MethodBodyVisitor(_semanticModel, Me)
                For Each localFunction As CSS.LocalFunctionStatementSyntax In node.DescendantNodes().OfType(Of CSS.LocalFunctionStatementSyntax).ToList()
                    Dim replacementStatement As VBS.StatementSyntax = localFunction.Accept(methodBodyVisitor)(0)
                    If TypeOf replacementStatement IsNot VBS.EmptyStatementSyntax OrElse replacementStatement.ContainsCommentOrDirectiveTrivia Then
                        vbStatements.Add(localFunction, replacementStatement)
                    End If
                Next
                Dim modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, Me.IsModule, TokenContext.New).ToList

                Dim parameterList As VBS.ParameterListSyntax = DirectCast(node.ParameterList?.Accept(Me), VBS.ParameterListSyntax)
                Dim subNewStatement As VBS.StatementSyntax =
                    Factory.SubNewStatement(attrs,
                                            Factory.TokenList(modifiers),
                                            parameterList
                                           ).WithTrailingEol _
                                            .RestructureAttributesAndModifiers(attrs.Any, modifiers.Any)

                Dim endSubStmt As VBS.EndBlockStatementSyntax = Factory.EndSubStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), SubKeyword).WithTrailingEol
                Dim statements As New SyntaxList(Of VBS.StatementSyntax)
                Dim csCloseBrace As SyntaxToken
                If node.Body IsNot Nothing Then
                    csCloseBrace = node.Body.CloseBraceToken
                    For Each e As IndexClass(Of CSS.StatementSyntax) In node.Body.Statements.WithIndex
                        If TypeOf e.Value Is CSS.LocalFunctionStatementSyntax Then
                            statements = statements.AddRange(ReplaceOneStatementWithMarkedStatements(e.Value, Factory.EmptyStatement(), removeStatement:=True))
                        Else
                            statements = statements.AddRange(e.Value.Accept(methodBodyVisitor))
                        End If
                    Next
                    statements = Me.AdjustUsingIfNeeded(statements)
                    endSubStmt = Factory.EndSubStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), SubKeyword).WithConvertedTriviaFrom(csCloseBrace)
                    If node.Body.OpenBraceToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        Dim trivia As SyntaxTriviaList = node.Body.OpenBraceToken.LeadingTrivia.ConvertTriviaList()
                        If Not statements.Any Then
                            endSubStmt = endSubStmt.WithPrependedLeadingTrivia(trivia)
                        End If
                    End If
                ElseIf node.ExpressionBody IsNot Nothing Then
                    statements = node.ExpressionBody.GetExpressionBodyStatements(False, Me)
                End If
                If initializer IsNot Nothing Then
                    statements = statements.InsertRange(0, ReplaceOneStatementWithMarkedStatements(node, initializer))
                Else
                    statements = ReplaceStatementsWithMarkedStatements(node, statements)
                End If
                Return Factory.ConstructorBlock(CType(subNewStatement, VBS.SubNewStatementSyntax),
                                                statements,
                                                endSubStmt).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitConstructorInitializer(node As CSS.ConstructorInitializerSyntax) As VB.VisualBasicSyntaxNode
                Dim argumentList As VBS.ArgumentListSyntax = DirectCast(node?.ArgumentList.Accept(Me), VBS.ArgumentListSyntax)
                Dim simpleMemberAccessExpr As VBS.MemberAccessExpressionSyntax
                Dim parent As SyntaxNode = node.Parent.Parent
                Dim meOrMyBaseExpr As VBS.ExpressionSyntax = If(TypeOf parent Is CSS.StructDeclarationSyntax,
                                                                DirectCast(MeExpression, VBS.ExpressionSyntax),
                                                                MyBaseExpression).WithConvertedLeadingTriviaFrom(node.ColonToken)

                simpleMemberAccessExpr = Factory.SimpleMemberAccessExpression(meOrMyBaseExpr, Factory.IdentifierName("New"))
                Return Factory.ExpressionStatement(Factory.InvocationExpression(simpleMemberAccessExpr, argumentList)).
                                             RestructureArguments(node.ArgumentList).WithConvertedTrailingTriviaFrom(node).WithTrailingEol
            End Function

            ''' <summary>
            ''' Creates a new object initialized to a meaningful value.
            ''' </summary>
            ''' <param name="node"></param>
            ''' <returns></returns>
            Public Overrides Function VisitConversionOperatorDeclaration(node As CSS.ConversionOperatorDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim returnAttributes As SyntaxList(Of VBS.AttributeListSyntax) = Nothing
                Dim finalTrailingDirective As New SyntaxTriviaList
                Dim attributeLists As List(Of VBS.AttributeListSyntax) = Me.ConvertAndSplitAttributes(node.AttributeLists, returnAttributes, finalTrailingDirective)
                Dim parameterList As VBS.ParameterListSyntax = DirectCast(node.ParameterList?.Accept(Me), VBS.ParameterListSyntax).
                                                                    WithRestructuredEolTrivia
                Dim modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, Me.IsModule, TokenContext.Member).ToList
                modifiers.Add(If(node.ImplicitOrExplicitKeyword.ValueText = "explicit", NarrowingKeyword, WideningKeyword))
                Dim type As VBS.TypeSyntax = DirectCast(node.Type.Accept(Me), VBS.TypeSyntax).With({SpaceTrivia}, {SpaceTrivia})
                Dim asClause As VBS.SimpleAsClauseSyntax = Factory.SimpleAsClause(Nothing, type)
                Dim operatorStatement As VBS.OperatorStatementSyntax = Factory.OperatorStatement(Factory.List(attributeLists), Factory.TokenList(modifiers), CTypeKeyword, parameterList, asClause).WithTrailingEol
                If finalTrailingDirective.Any Then
                    operatorStatement = operatorStatement.WithAppendedTrailingTrivia(finalTrailingDirective)
                End If

                Dim body As New SyntaxList(Of VBS.StatementSyntax)
                If node.Body IsNot Nothing Then
                    body = node.Body.GetBodyStatements(New MethodBodyVisitor(_semanticModel, Me))
                ElseIf node.ExpressionBody IsNot Nothing Then
                    body = node.ExpressionBody.GetExpressionBodyStatements(False, Me)
                End If
                Return PrependStatementWithMarkedStatementTrivia(
                    node,
                    Factory.OperatorBlock(operatorStatement,
                                                  body,
                                                  FactoryEndBlockStatement(VB.SyntaxKind.EndOperatorStatement,
                                                                          OperatorKeyword,
                                                                          node.Body.GetBraces.Item2.CollectConvertedTokenTrivia(getLeading:=True, getTrailing:=True))
                                                 ).WithConvertedTriviaFrom(node))
            End Function

            Public Overrides Function VisitDestructorDeclaration(node As CSS.DestructorDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim attributeLists As SyntaxList(Of VBS.AttributeListSyntax) = Factory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))
                Dim parameterList As VBS.ParameterListSyntax = DirectCast(node.ParameterList?.Accept(Me), VBS.ParameterListSyntax)
                Dim body As SyntaxList(Of VBS.StatementSyntax)
                If node.Body IsNot Nothing Then
                    body = node.Body.GetBodyStatements(New MethodBodyVisitor(_semanticModel, Me))
                Else
                    body = node.ExpressionBody.GetExpressionBodyStatements(False, Me)
                End If
                Return Factory.SubBlock(subOrFunctionStatement:=Factory.SubStatement(
                                              attributeLists,
                                              ProtectedModifier.Add(OverridesKeyword),
                                              FinalizeToken,
                                              typeParameterList:=Nothing,
                                              parameterList,
                                              asClause:=Nothing,
                                              handlesClause:=Nothing,
                                              implementsClause:=Nothing),
                                              Factory.List(body)).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitEventDeclaration(node As CSS.EventDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim returnAttributes As SyntaxList(Of VBS.AttributeListSyntax) = Nothing
                Dim finalTrailingDirective As New SyntaxTriviaList
                Dim modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, Me.IsModule, TokenContext.Member).ToList
                modifiers.Add(CustomKeyword)
                Dim declaredSymbol As ISymbol = _semanticModel.GetDeclaredSymbol(node)
                Dim eventNameToken As SyntaxToken = Me.GenerateSafeVbToken(node.Identifier, node).WithTrailingTrivia(SpaceTrivia)
                Dim typeSyntaxNode As VBS.TypeSyntax = DirectCast(node.Type.Accept(Me), VBS.TypeSyntax)
                Dim asClause As VBS.SimpleAsClauseSyntax = Factory.SimpleAsClause(attributeLists:=returnAttributes, typeSyntaxNode)
                Dim implementsClauseOrNothing As VBS.ImplementsClauseSyntax = If(declaredSymbol Is Nothing, Nothing, Me.CreateImplementsClauseSyntaxOrNull(declaredSymbol, eventNameToken))
                Dim attributeLists As List(Of VBS.AttributeListSyntax) = Me.ConvertAndSplitAttributes(node.AttributeLists, returnAttributes, finalTrailingDirective)
                Dim eventStmt As VBS.EventStatementSyntax = Factory.EventStatement(attributeLists:=Factory.List(attributeLists),
                                                                              Factory.TokenList(modifiers),
                                                                                   eventNameToken,
                                                                                   parameterList:=Nothing,
                                                                                   asClause,
                                                                                   implementsClauseOrNothing).WithTrailingEol
                If finalTrailingDirective.Any Then
                    eventStmt = eventStmt.WithAppendedTrailingTrivia(finalTrailingDirective)
                End If
                If implementsClauseOrNothing IsNot Nothing Then
                    Throw UnreachableException(NameOf(VisitEventDeclaration), 453)
                End If
                Dim accessors As New List(Of VBS.AccessorBlockSyntax)
                For Each e As IndexClass(Of CSS.AccessorDeclarationSyntax) In node.AccessorList.Accessors.WithIndex
                    If e.Value.Body IsNot Nothing OrElse e.Value.ExpressionBody IsNot Nothing Then
                        accessors.Add(Me.ConvertEventAccessor(e.Value, Me.IsModule, GetEventLookupExpr(eventNameToken), typeSyntaxNode))
                    End If
                Next
                If Not accessors.Any Then
                    Throw UnreachableException(NameOf(VisitEventDeclaration), 479)
                End If

                Dim parameterList As VBS.ParameterListSyntax = Factory.ParseParameterList("()")
                Dim eventArgumentList As VBS.ArgumentListSyntax = Factory.ParseArgumentList("()")
                Dim eventSymbol As IEventSymbol = CType(declaredSymbol, IEventSymbol)
                If eventSymbol IsNot Nothing Then
                    Dim namedType As INamedTypeSymbol = CType(eventSymbol.Type, INamedTypeSymbol)
                    Dim parameters As ImmutableArray(Of IParameterSymbol) = namedType.GetParameters
                    ' ToDo correctly fill in parameter list
                    If parameters.Length = 2 Then
                        parameterList = Factory.ParseParameterList("(sender As Object, e As EventArgs)")
                        eventArgumentList = Factory.ParseArgumentList("(sender, e)")
                    End If
                End If
                Dim eventExists As Boolean = False
                Dim statementSyntaxes As New SyntaxList(Of VBS.StatementSyntax)
                If _eventList.TryGetValue(GetEventLookupExpr(eventNameToken).ToString(), eventExists) Then

                    '    Dim tempVar As SmallBasicCallback = TryCast(_buttonClicked("ButtonClicked"), SmallBasicCallback)
                    Dim tempVar As VBS.IdentifierNameSyntax = Factory.IdentifierName(Me.GetUniqueVariableNameInScope(node, "tempVar", _usedIdentifiers))
                    Dim tryCastExpression As VBS.EqualsValueSyntax = Factory.EqualsValue(Factory.TryCastExpression(GetEventLookupExpr(eventNameToken), typeSyntaxNode))
                    Dim tryCastEventName As VBS.StatementSyntax = FactoryDimStatement(tempVar.ToString(), asClause, tryCastExpression).WithTrailingEol()
                    statementSyntaxes = statementSyntaxes.Add(tryCastEventName)
                    '    If tempVar IsNot Nothing Then tempVar2.Invoke()
                    Dim raiseEventStatement As VBS.StatementSyntax =
                            Factory.ExpressionStatement(
                                Factory.InvocationExpression(Factory.SimpleMemberAccessExpression(tempVar, InvokeName),
                                                             eventArgumentList).WithTrailingEol())
                    Dim condition As VBS.ExpressionSyntax = Factory.IsNotExpression(tempVar, NothingExpression)
                    Dim ifStatement As VBS.StatementSyntax = Factory.SingleLineIfStatement(condition, Factory.SingletonList(raiseEventStatement), elseClause:=Nothing)
                    statementSyntaxes = statementSyntaxes.Add(ifStatement)
                Else
                    statementSyntaxes = statementSyntaxes.Add(Factory.RaiseEventStatement(Factory.IdentifierName(eventNameToken)))
                End If
                Dim accessorStatement As VBS.AccessorStatementSyntax = Factory.RaiseEventAccessorStatement(Nothing, Nothing, parameterList)
                accessors.Add(Factory.RaiseEventAccessorBlock(accessorStatement, statementSyntaxes))
                Return Factory.EventBlock(eventStmt, Factory.List(accessors)).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitEventFieldDeclaration(node As CSS.EventFieldDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim id As SyntaxToken = Factory.Identifier(MakeVbSafeName(node.Declaration.Variables.Single().Identifier.ValueText))
                Dim decl As CSS.VariableDeclaratorSyntax = node.Declaration.Variables.Single()
                Dim declaredSymbol As ISymbol = _semanticModel.GetDeclaredSymbol(decl)
                Dim implementsClauseOrNothing As VBS.ImplementsClauseSyntax = If(declaredSymbol Is Nothing, Nothing, Me.CreateImplementsClauseSyntaxOrNull(declaredSymbol, id))
                If implementsClauseOrNothing Is Nothing AndAlso Me.NeedEventHandlerList(node) Then
                    Dim asNewClause As VBS.AsNewClauseSyntax = Factory.AsNewClause(DirectCast(NewEventHandlerListExpression, VBS.NewExpressionSyntax))
                    Dim modifiedIdentifier As VBS.ModifiedIdentifierSyntax = Factory.ModifiedIdentifier(id).WithTrailingTrivia(SpaceTrivia)
                    Dim names As SeparatedSyntaxList(Of VBS.ModifiedIdentifierSyntax) = Factory.SingletonSeparatedList(modifiedIdentifier)
                    Dim declarator As VBS.VariableDeclaratorSyntax = Factory.VariableDeclarator(names, asNewClause, initializer:=Nothing)
                    Dim declarators As SeparatedSyntaxList(Of VBS.VariableDeclaratorSyntax) = Factory.SingletonSeparatedList(declarator)
                    Dim fieldModifiers As New List(Of SyntaxToken) From {
                        PrivateKeyword,
                        SharedKeyword
                    }
                    Return Factory.FieldDeclaration(attributeLists:=Nothing,
                                                    Factory.TokenList(fieldModifiers),
                                                    declarators
                                                   ).WithTrailingEol
                Else
                    Dim returnAttributes As New SyntaxList(Of VBS.AttributeListSyntax)
                    Dim finalTrailingDirective As New SyntaxTriviaList
                    Dim attributeList As List(Of VBS.AttributeListSyntax) = Me.ConvertAndSplitAttributes(node.AttributeLists, returnAttributes, finalTrailingDirective)
                    If finalTrailingDirective.Any Then
                        Stop
                    End If
                    Return Factory.EventStatement(Factory.List(attributeList),
                                         Factory.TokenList(ConvertModifiers(node.Modifiers, Me.IsModule, TokenContext.Member)),
                                                        id,
                                                        parameterList:=Nothing,
                                                        Factory.SimpleAsClause(attributeLists:=Nothing, DirectCast(node.Declaration.Type.Accept(Me), VBS.TypeSyntax)).WithTrailingEol,
                                                        implementsClauseOrNothing).WithConvertedTriviaFrom(node)
                End If
            End Function

            Public Overrides Function VisitFieldDeclaration(node As CSS.FieldDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim typeInf As TypeInfo = _semanticModel.GetTypeInfo(node.Declaration.Type)
                Dim variableOrConstOrReadonly As TokenContext = TokenContext.VariableOrConst
                If typeInf.ConvertedType IsNot Nothing AndAlso typeInf.ConvertedType.TypeKind = TypeKind.Class Then
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
                Dim attributes As New SyntaxList(Of VBS.AttributeListSyntax)
                If node.Modifiers.Contains(CS.SyntaxKind.VolatileKeyword) Then
                    Dim name As VBS.TypeSyntax = Factory.ParseTypeName("Volatile")
                    Dim volatileAttribute As SeparatedSyntaxList(Of VBS.AttributeSyntax) = Factory.SingletonSeparatedList(Factory.Attribute(name))
                    newLeadingTrivia = newLeadingTrivia.Add(Factory.CommentTrivia("' TODO TASK: VB has no direct equivalent to C# Volatile Modifier, an Attribute was substituted."))
                    attributes = attributes.Add(Factory.AttributeList(volatileAttribute).WithLeadingTrivia(newLeadingTrivia))
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
                    attributes = attributes.Add(DirectCast(a.Accept(Me), VBS.AttributeListSyntax))
                Next
                If attributes.Any Then
                    attributes = attributes.Replace(attributes(0), attributes(0).WithLeadingTrivia(newLeadingTrivia))
                Else
                    modifierList(0) = modifierList(0).WithLeadingTrivia(newLeadingTrivia)
                End If
                Dim declarators As SeparatedSyntaxList(Of VBS.VariableDeclaratorSyntax) = node.Declaration.RemodelVariableDeclaration(Me, _semanticModel, isFieldDeclaration:=True, newLeadingTrivia)
                Dim fieldDeclaration As VBS.FieldDeclarationSyntax
                Dim modifiers As SyntaxTokenList = Factory.TokenList(modifierList)
                fieldDeclaration = Factory.FieldDeclaration(attributes, modifiers, declarators).WithLeadingTrivia(newLeadingTrivia)
                fieldDeclaration = AddSpecialCommentToField(node, fieldDeclaration)
                Return fieldDeclaration.RestructureAttributesAndModifiers(attributes.Any, modifiers.Any).
                    WithMergedTrailingTrivia(node.SemicolonToken.CollectConvertedTokenTrivia(getLeading:=True, getTrailing:=True)).WithTrailingEol
            End Function

            Public Overrides Function VisitIndexerDeclaration(node As CSS.IndexerDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim declaredSymbol As IPropertySymbol = TryCast(_semanticModel.GetDeclaredSymbol(node), IPropertySymbol)
                Dim id As SyntaxToken = Factory.Identifier("item")
                Dim returnAttributes As SyntaxList(Of VBS.AttributeListSyntax) = Nothing
                Dim finalTrailingDirective As New SyntaxTriviaList
                Dim attributeLists As List(Of VBS.AttributeListSyntax) = Me.ConvertAndSplitAttributes(node.AttributeLists, returnAttributes, finalTrailingDirective)
                Dim accessors As New List(Of VBS.AccessorBlockSyntax)()
                Dim isIterator As Boolean = False
                If node.AccessorList IsNot Nothing Then
                    For Each a As CSS.AccessorDeclarationSyntax In node.AccessorList.Accessors
                        Dim foundIterator As Boolean
                        accessors.Add(Me.ConvertAccessor(a, Me.IsModule, foundIterator))
                        isIterator = isIterator Or foundIterator
                    Next
                End If

                Dim modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, Me.IsModule, TokenContext.Member).ToList
                If modifiers.Any Then
                    modifiers.Insert(0, DefaultKeyword.WithLeadingTrivia(modifiers(0).LeadingTrivia))
                    modifiers(1) = modifiers(1).WithLeadingTrivia(SpaceTrivia)
                End If
                Select Case accessors.Count
                    Case 0
                        Dim lastTrailingTrivia As SyntaxTriviaList = lastTrailingTrivia.AddRange(modifiers.Last.TrailingTrivia)
                        modifiers(modifiers.Count - 1) = modifiers.Last.WithLeadingTrivia(SpaceTrivia).WithTrailingTrivia(SpaceTrivia)
                        modifiers.Add(ReadOnlyKeyword.WithLeadingTrivia(SpaceTrivia).WithTrailingTrivia(lastTrailingTrivia))

                        Dim accessorStatement As VBS.AccessorStatementSyntax = Factory.GetAccessorStatement()
                        Dim body As SyntaxList(Of VBS.StatementSyntax) = node.ExpressionBody.GetExpressionBodyStatements(False, Me)
                        Dim endStmt As VBS.EndBlockStatementSyntax = FactoryEndBlockStatement(VB.SyntaxKind.EndGetStatement, GetKeyword, New SyntaxTriviaList)
                        accessors.Add(Factory.AccessorBlock(VB.SyntaxKind.GetAccessorBlock, accessorStatement, body, endStmt))
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
                Dim parameterList As VBS.ParameterListSyntax = DirectCast(node.ParameterList?.Accept(Me), VBS.ParameterListSyntax).WithRestructuredEolTrivia
                Dim nodeType As VBS.TypeSyntax = DirectCast(node.Type.Accept(Me), VBS.TypeSyntax)
                Dim asClause As VBS.SimpleAsClauseSyntax = Factory.SimpleAsClause(returnAttributes, nodeType.WithLeadingTrivia(SpaceTrivia))
                Dim stmt As VBS.PropertyStatementSyntax = Factory.PropertyStatement(Factory.List(attributeLists),
                                                                                    Factory.TokenList(modifiers),
                                                                                    id,
                                                                                    parameterList,
                                                                                    asClause.WithTrailingEol,
                                                                                    initializer:=Nothing,
                                                                                    implementsClauseOrNothing).WithTrailingEol
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
                                               Factory.EndPropertyStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), PropertyKeyword).
                                                                                WithConvertedTriviaFrom(node.AccessorList.GetBraces.Item2)).
                                                                                WithConvertedLeadingTriviaFrom(node.Type)
            End Function

            Public Overrides Function VisitMethodDeclaration(node As CSS.MethodDeclarationSyntax) As VB.VisualBasicSyntaxNode
                If node.Modifiers.Contains(CS.SyntaxKind.UnsafeKeyword) Then
                    Return FlagUnsupportedStatements(node, "unsafe Functions", commentOutOriginalStatements:=True)
                End If

                If node.ReturnType IsNot Nothing AndAlso TypeOf node.ReturnType Is CSS.RefTypeSyntax Then
                    Return FlagUnsupportedStatements(node, "ref return Functions", commentOutOriginalStatements:=True)
                End If
                _originalRequest.UsedStacks.Push(_usedIdentifiers)

                Dim methodNameToken As SyntaxToken = Me.GenerateSafeVbToken(node.Identifier, node)

                Dim methodInfo As ISymbol = _semanticModel.GetDeclaredSymbol(node)
                Dim possibleReturnVoid As Boolean? = methodInfo?.GetReturnType()?.SpecialType = SpecialType.System_Void
                Dim isReturnVoid As Boolean = If(possibleReturnVoid, False)
                Dim containingType As INamedTypeSymbol = methodInfo?.ContainingType
                Dim returnAttributes As SyntaxList(Of VBS.AttributeListSyntax) = Nothing
                Dim functionStmtTrailingTrivia As New SyntaxTriviaList
                Dim attributeLists As List(Of VBS.AttributeListSyntax) = Me.ConvertAndSplitAttributes(node.AttributeLists, returnAttributes, functionStmtTrailingTrivia)
                Dim parameterList As VBS.ParameterListSyntax = DirectCast(node.ParameterList?.Accept(Me), VBS.ParameterListSyntax)

                Dim functionStmtLeadingTrivia As SyntaxTriviaList
                Dim vbStatements As New List(Of VBS.StatementSyntax)
                Dim finalLeadingTrivia As New SyntaxTriviaList
                If node.Body IsNot Nothing Then
                    finalLeadingTrivia = node.Body.CloseBraceToken.CollectConvertedTokenTrivia(getLeading:=True, getTrailing:=False)
                End If
                Dim methodBodyVisitor As New MethodBodyVisitor(_semanticModel, Me)
                For Each e As IndexClass(Of CSS.LocalFunctionStatementSyntax) In node.DescendantNodes().OfType(Of CSS.LocalFunctionStatementSyntax).WithIndex
                    Dim localFunction As CSS.LocalFunctionStatementSyntax = e.Value
                    Dim replacementStatement As VBS.StatementSyntax = localFunction.Accept(methodBodyVisitor)(0)
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
                            vbStatementCollection = e.Value.Accept(methodBodyVisitor)
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
                        functionStmtLeadingTrivia = functionStmtLeadingTrivia.AddRange(node.ExpressionBody.GetLeadingTrivia.ConvertTriviaList())
                        body = node.ExpressionBody.GetExpressionBodyStatements(isReturnVoid, Me)
                        If body.HasValue Then
                            body = body.Value.Replace(body.Value(0), body.Value(0).WithoutLeadingTrivia)
                        End If
                    Else
                        body = node.ExpressionBody.GetExpressionBodyStatements(isReturnVoid, Me)
                    End If
                    body = ReplaceStatementsWithMarkedStatements(node, body.Value)
                End If
                If node.Modifiers.Contains(CS.SyntaxKind.ExternKeyword) Then
                    body = Factory.List(Of VBS.StatementSyntax)()
                End If
                Dim modifiers As List(Of SyntaxToken)
                If methodNameToken.ValueText = "Main" AndAlso
                            node.Modifiers.Count = 1 AndAlso
                            node.Modifiers(0).IsKind(CS.SyntaxKind.StaticKeyword) Then
                    If Me.IsModule Then
                        modifiers = PublicModifier.ToList
                    Else
                        modifiers = PublicModifier.ToList
                        modifiers.AddRange(ConvertModifiers(node.Modifiers, Me.IsModule, If(containingType?.IsInterfaceType() = True, TokenContext.Local, TokenContext.Member)).ToList)
                        'modifiers.Remove(PrivateKeyword)
                        Dim index As Integer = modifiers.IndexOf(VB.SyntaxKind.PrivateKeyword)
                        If index > -1 Then
                            modifiers.RemoveAt(index)
                        End If
                    End If
                Else
                    modifiers = ConvertModifiers(node.Modifiers, Me.IsModule, If(containingType?.IsInterfaceType() = True, TokenContext.Local, TokenContext.Member)).ToList
                End If
                If methodBodyVisitor.IsIterator And Not isReturnVoid Then
                    modifiers.Add(IteratorKeyword)
                End If
                If node.ParameterList.Parameters.Any AndAlso node.ParameterList.Parameters(0).Modifiers.Any(CS.SyntaxKind.ThisKeyword) Then
                    Dim newLeadingTrivia As SyntaxTriviaList
                    If attributeLists.Any AndAlso attributeLists(0).HasLeadingTrivia Then
                        newLeadingTrivia = newLeadingTrivia.AddRange(attributeLists(0).GetLeadingTrivia)
                        attributeLists(0) = attributeLists(0).WithLeadingTrivia(newLeadingTrivia.Last)
                    End If
                    If attributeLists.Count = 0 AndAlso modifiers.Any Then
                        newLeadingTrivia = newLeadingTrivia.AddRange(modifiers(0).LeadingTrivia)
                        modifiers(0) = modifiers(0).WithLeadingTrivia(SpaceTrivia)
                    End If
                    attributeLists.Insert(0, Factory.AttributeList(Factory.SingletonSeparatedList(ExtensionAttribute)).WithPrependedLeadingTrivia(newLeadingTrivia))
                    If Not DirectCast(node.SyntaxTree, CS.CSharpSyntaxTree).HasUsingDirective(s_compilerServices) Then
                        If Not _allImports.ContainsName(s_compilerServices) Then
                            _allImports.Add(FactoryImportCompilerServices)
                        End If
                    End If
                End If
                If containingType?.IsStatic = True Then
                    Dim tokenList As New List(Of SyntaxToken)
                    Dim newLeadingTrivia As New SyntaxTriviaList
                    For Each e As IndexClass(Of SyntaxToken) In modifiers.WithIndex
                        Dim t As SyntaxToken = e.Value
                        If t.IsKind(VB.SyntaxKind.SharedKeyword) Then
                            If t.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                newLeadingTrivia = newLeadingTrivia.AddRange(t.LeadingTrivia)
                            End If
                        Else
                            tokenList.Add(t)
                        End If
                    Next
                    If tokenList.Count = 0 Then
                        modifiers.Clear()
                        modifiers.Add(EmptyToken.WithLeadingTrivia(newLeadingTrivia))
                    Else
                        If Not EndsWithSimilarTrivia(newLeadingTrivia, modifiers(0).LeadingTrivia) Then
                            newLeadingTrivia = newLeadingTrivia.InsertRange(0, modifiers(0).LeadingTrivia)
                            modifiers(0) = modifiers(0).WithLeadingTrivia(newLeadingTrivia)
                        End If
                    End If
                End If

                If attributeLists.Any Then
                    functionStmtLeadingTrivia = functionStmtLeadingTrivia.AddRange(attributeLists(0).GetLeadingTrivia)
                    attributeLists(0) = attributeLists(0).WithLeadingTrivia(SpaceTrivia)
                End If
                If node.ReturnType IsNot Nothing Then
                    If modifiers.Count = 0 Then
                        functionStmtLeadingTrivia = functionStmtLeadingTrivia.AddRange(node.ReturnType.GetLeadingTrivia.ConvertTriviaList())
                    Else
                        Dim newModifierLeadingTrivia As New SyntaxTriviaList
                        Dim csNodeLeadingTrivia As SyntaxTriviaList = node.ReturnType.GetLeadingTrivia
                        If csNodeLeadingTrivia.Any Then
                            newModifierLeadingTrivia = newModifierLeadingTrivia.AddRange(modifiers(0).LeadingTrivia)
                            newModifierLeadingTrivia = newModifierLeadingTrivia.AddRange(csNodeLeadingTrivia.ConvertTriviaList())

                            If Not newModifierLeadingTrivia.FirstOrDefault.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                newModifierLeadingTrivia = newModifierLeadingTrivia.Insert(0, VbEolTrivia)
                            End If
                            modifiers(0) = modifiers(0).WithLeadingTrivia(newModifierLeadingTrivia)
                        End If
                        If attributeLists.Count = 0 AndAlso modifiers(0).LeadingTrivia.Any Then
                            functionStmtLeadingTrivia = functionStmtLeadingTrivia.AddRange(modifiers(0).LeadingTrivia)
                            modifiers(0) = modifiers(0).WithLeadingTrivia(modifiers(0).LeadingTrivia.Last)
                        End If
                    End If
                End If
                If node.Body Is Nothing Then
                    functionStmtTrailingTrivia = functionStmtTrailingTrivia.AddRange(node.GetTrailingTrivia.ConvertTriviaList())
                Else
                    If node.Body.OpenBraceToken.HasTrailingTrivia Then
                        functionStmtTrailingTrivia = functionStmtTrailingTrivia.AddRange(node.Body.OpenBraceToken.TrailingTrivia.ConvertTriviaList())
                    Else
                        functionStmtTrailingTrivia = functionStmtTrailingTrivia.AddRange(node.GetTrailingTrivia.ConvertTriviaList())
                    End If
                End If

                Dim subOrFunctionStatement As VBS.MethodStatementSyntax
                Dim typeParameterList As VBS.TypeParameterListSyntax = DirectCast(node.TypeParameterList?.Accept(Me), VBS.TypeParameterListSyntax)
                If typeParameterList IsNot Nothing Then
                    typeParameterList = typeParameterList.WithTrailingTrivia(SpaceTrivia)
                End If
                Dim implementsClauseOrNothing As VBS.ImplementsClauseSyntax = If(methodInfo Is Nothing, Nothing, Me.CreateImplementsClauseSyntaxOrNull(methodInfo, methodNameToken))

                If methodNameToken.ToString = "Dispose" AndAlso TypeOf node.Parent Is CSS.ClassDeclarationSyntax Then
                    Dim parentClass As CSS.ClassDeclarationSyntax = DirectCast(node.Parent, CSS.ClassDeclarationSyntax)
                    If parentClass.BaseList IsNot Nothing Then
                        For Each t As CSS.SimpleBaseTypeSyntax In parentClass.BaseList.Types
                            Dim identifierName As CSS.IdentifierNameSyntax = TryCast(t.Type, CSS.IdentifierNameSyntax)
                            If identifierName IsNot Nothing AndAlso identifierName.Identifier.ValueText = "IDisposable" Then
                                Dim interfaceMembers As VBS.QualifiedNameSyntax = Factory.QualifiedName(
                                                                                Factory.IdentifierName("IDisposable"),
                                                                                Factory.IdentifierName("Dispose")
                                                                                )
                                If parameterList Is Nothing OrElse parameterList.Parameters.Any Then
                                    Exit For
                                End If
                                implementsClauseOrNothing = Factory.ImplementsClause(interfaceMembers).WithTrailingTrivia(parameterList.GetTrailingTrivia)
                                parameterList = parameterList.WithTrailingTrivia(SpaceTrivia)
                            End If
                        Next
                    End If
                End If
                Dim endSubOrFunction As VBS.EndBlockStatementSyntax
                Dim blockStatements As SyntaxList(Of VBS.StatementSyntax)

                If isReturnVoid Then
                    If node.Body IsNot Nothing Then
                        endSubOrFunction = FactoryEndBlockStatement(VB.SyntaxKind.EndSubStatement, SubKeyword, node.Body.CloseBraceToken.CollectConvertedTokenTrivia(getLeading:=False, getTrailing:=True))
                    ElseIf node.ExpressionBody IsNot Nothing Then
                        endSubOrFunction = FactoryEndBlockStatement(VB.SyntaxKind.EndSubStatement, SubKeyword, node.ExpressionBody.GetBraces.Item2.CollectConvertedTokenTrivia(getLeading:=True, getTrailing:=True))
                    Else
                        endSubOrFunction = Factory.EndSubStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), SubKeyword)
                    End If

                    If typeParameterList IsNot Nothing OrElse parameterList IsNot Nothing Then
                        methodNameToken = methodNameToken.WithTrailingTrivia(SpaceTrivia)
                    End If
                    If attributeLists.Any AndAlso attributeLists.Last.GetTrailingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                        If modifiers.Any AndAlso modifiers(0).LeadingTrivia.ContainsEolTrivia Then
                            If modifiers(0).LeadingTrivia(0).IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                modifiers(0) = modifiers(0).WithLeadingTrivia(modifiers(0).LeadingTrivia.RemoveAt(0))
                            End If
                        End If
                    End If
                    Dim methodStmt As VBS.MethodStatementSyntax = Factory.SubStatement(
                                                                        Factory.List(attributeLists),
                                                                        Factory.TokenList(modifiers),
                                                                        methodNameToken,
                                                                        typeParameterList,
                                                                        parameterList,
                                                                        asClause:=Nothing,
                                                                        handlesClause:=Nothing,
                                                                        implementsClauseOrNothing)
                    subOrFunctionStatement = DirectCast(methodStmt.
                                                    With(functionStmtLeadingTrivia, functionStmtTrailingTrivia).
                                                    WithTrailingEol.
                                                    RestructureAttributesAndModifiers(attributeLists.Any, modifiers.Any), VBS.MethodStatementSyntax)
                    SyncLock _originalRequest.UsedStacks
                        If _originalRequest.UsedStacks.Count > 0 Then
                            _usedIdentifiers = DirectCast(_originalRequest.UsedStacks.Pop, Dictionary(Of String, SymbolTableEntry))
                        End If
                    End SyncLock
                    subOrFunctionStatement = DirectCast(PrependStatementWithMarkedStatementTrivia(node, subOrFunctionStatement), VBS.MethodStatementSyntax)
                    If body Is Nothing Then
                        If modifiers.Contains(Function(t As SyntaxToken) t.IsKind(VB.SyntaxKind.PartialKeyword)) Then
                            Return Factory.SubBlock(subOrFunctionStatement,
                                                    statements:=Nothing,
                                                    endSubOrFunction.With(functionStmtLeadingTrivia, functionStmtTrailingTrivia).WithPrependedLeadingTrivia(finalLeadingTrivia).RemoveExtraLeadingEol.WithTrailingEol.WithTrailingEol)
                        End If
                        Return subOrFunctionStatement
                    End If
                    blockStatements = Me.AdjustUsingIfNeeded(body.Value)
                    Return Factory.SubBlock(subOrFunctionStatement,
                                            blockStatements,
                                            endSubOrFunction.WithPrependedLeadingTrivia(finalLeadingTrivia).RemoveExtraLeadingEol.WithTrailingEol)
                End If
                If node.Body IsNot Nothing Then
                    endSubOrFunction = FactoryEndBlockStatement(VB.SyntaxKind.EndFunctionStatement, FunctionKeyword, node.Body.CloseBraceToken.CollectConvertedTokenTrivia(getLeading:=False, getTrailing:=True)).WithPrependedLeadingTrivia(finalLeadingTrivia)
                Else
                    endSubOrFunction = FactoryEndBlockStatement(VB.SyntaxKind.EndFunctionStatement, FunctionKeyword, New SyntaxTriviaList)
                End If
                Dim type As VBS.TypeSyntax = DirectCast(node.ReturnType.Accept(Me), VBS.TypeSyntax).WithLeadingTrivia(SpaceTrivia)

                If type Is Nothing Then
                    ' Handle ref return
                    type = Factory.ParseTypeName("HandleRef")
                Else
                    If type.ToString = "[Delegate]" Then
                        type = Factory.ParseTypeName("System.Delegate")
                    ElseIf type.ToString = "[Enum]" Then
                        type = Factory.ParseTypeName("System.Enum")
                    ElseIf type.ToString.StartsWith("[", StringComparison.Ordinal) Then
                        Dim typeSplit As String() = type.ToString.Split({"["c, "]"c}, StringSplitOptions.RemoveEmptyEntries)
                        If Not (IsSpecialReservedWord(typeSplit(0)) OrElse
                                VB.SyntaxFacts.IsKeywordKind(VB.SyntaxFacts.GetKeywordKind(typeSplit(0)))) Then
                            type = Factory.ParseTypeName(type.ToString().RemoveBrackets)
                        End If
                    End If
                End If

                Dim asClause As VBS.SimpleAsClauseSyntax = Factory.SimpleAsClause(returnAttributes, type.WithLeadingTrivia(SpaceTrivia))
                Dim paramListTrailingTrivia As SyntaxTriviaList = parameterList.GetTrailingTrivia
                parameterList = parameterList.WithTrailingTrivia(SpaceTrivia)
                asClause = asClause.WithTrailingTrivia(paramListTrailingTrivia)

                If typeParameterList IsNot Nothing OrElse parameterList IsNot Nothing Then
                    methodNameToken = methodNameToken.WithTrailingTrivia(SpaceTrivia)
                End If
                If asClause.GetTrailingTrivia.ContainsCommentTrivia Then
                    functionStmtTrailingTrivia = functionStmtTrailingTrivia.InsertRange(0, asClause.GetTrailingTrivia.ToList)
                    asClause = asClause.WithTrailingTrivia(SpaceTrivia)
                End If
                Dim initialTriviaList As SyntaxTriviaList = If(modifiers.Any, modifiers(0).LeadingTrivia, Nothing)

                Dim movedModifierLeadingTrivia As New SyntaxTriviaList
                If attributeLists.Any AndAlso modifiers.Any AndAlso initialTriviaList.ContainsCommentOrDirectiveTrivia Then
                    Dim fixedModifierLeadingTrivia As SyntaxTriviaList
                    For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                        Dim t As SyntaxTrivia = e.Value
                        Dim nextTrivia As SyntaxTrivia = initialTriviaList.GetForwardTriviaOrDefault(e.Index, lookaheadCount:=1)
                        Select Case t.RawKind
                            Case VB.SyntaxKind.WhitespaceTrivia
                                If nextTrivia.IsComment Then
                                    fixedModifierLeadingTrivia = fixedModifierLeadingTrivia.Add(SpaceTrivia)
                                    fixedModifierLeadingTrivia = fixedModifierLeadingTrivia.Add(LineContinuation)
                                    fixedModifierLeadingTrivia = fixedModifierLeadingTrivia.Add(t)
                                    fixedModifierLeadingTrivia = fixedModifierLeadingTrivia.Add(nextTrivia)
                                    e.MoveNext()
                                Else
                                    fixedModifierLeadingTrivia = fixedModifierLeadingTrivia.Add(t)
                                End If
                            Case VB.SyntaxKind.EndOfLineTrivia
                                fixedModifierLeadingTrivia = fixedModifierLeadingTrivia.Add(t)
                            Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                                fixedModifierLeadingTrivia = fixedModifierLeadingTrivia.Add(t)
                            Case Else
                                If t.IsDirective Then
                                    If attributeLists.Any And node.AttributeLists(0).GetLeadingTrivia.ContainsDirectiveTrivia Then
                                        movedModifierLeadingTrivia = movedModifierLeadingTrivia.Add(t)
                                    End If
                                End If
                        End Select
                    Next
                    modifiers(0) = modifiers(0).WithLeadingTrivia(fixedModifierLeadingTrivia)
                End If
                If attributeLists.Any AndAlso attributeLists.Last.GetTrailingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                    If modifiers.Any AndAlso modifiers(0).LeadingTrivia.ContainsEolTrivia Then
                        If modifiers(0).LeadingTrivia(0).IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            modifiers(0) = modifiers(0).WithLeadingTrivia(modifiers(0).LeadingTrivia.RemoveAt(0))
                        End If
                    End If
                End If
                subOrFunctionStatement = Factory.FunctionStatement(
                                            Factory.List(attributeLists),
                                            Factory.TokenList(modifiers),
                                            methodNameToken,
                                            typeParameterList,
                                            parameterList,
                                            asClause,
                                            handlesClause:=Nothing,
                                            implementsClauseOrNothing).
                                            With(functionStmtLeadingTrivia, functionStmtTrailingTrivia).WithTrailingEol
                If returnAttributes.Any AndAlso
                   (attributeLists.Count = 0 OrElse attributeLists(0).Attributes(0).Name.ToString = "Extension") AndAlso
                   node.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    subOrFunctionStatement = subOrFunctionStatement.WithPrependedLeadingTrivia(node.GetLeadingTrivia.ConvertTriviaList())
                End If
                subOrFunctionStatement = DirectCast(PrependStatementWithMarkedStatementTrivia(node, subOrFunctionStatement), VBS.MethodStatementSyntax)
                If _originalRequest.UsedStacks.Count > 0 Then
                    _usedIdentifiers = DirectCast(_originalRequest.UsedStacks.Pop, Dictionary(Of String, SymbolTableEntry))
                End If

                If body Is Nothing Then
                    Return subOrFunctionStatement
                End If

                Dim blockValue As List(Of VBS.StatementSyntax) = body.Value.ToList
                If blockValue.Any AndAlso movedModifierLeadingTrivia.Any Then
                    blockValue(0) = blockValue(0).WithPrependedLeadingTrivia(movedModifierLeadingTrivia)
                End If
                blockStatements = Me.AdjustUsingIfNeeded(body.Value)
                Return Factory.FunctionBlock(subOrFunctionStatement,
                                             blockStatements,
                                             endSubOrFunction.RemoveExtraLeadingEol.WithTrailingEol)
            End Function

            Public Overrides Function VisitOperatorDeclaration(node As CSS.OperatorDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim returnAttributes As SyntaxList(Of VBS.AttributeListSyntax) = Nothing
                Dim finalTrailingDirective As New SyntaxTriviaList
                Dim attributeLists As List(Of VBS.AttributeListSyntax) = Me.ConvertAndSplitAttributes(node.AttributeLists, returnAttributes, finalTrailingDirective)
                Dim body As SyntaxList(Of VBS.StatementSyntax)
                If node.Body IsNot Nothing Then
                    Dim methodBodyVisitor As New MethodBodyVisitor(_semanticModel, Me)
                    body = Factory.List(node.Body.Statements.SelectMany(Function(s As CSS.StatementSyntax) s.Accept(methodBodyVisitor)))
                ElseIf node.ExpressionBody IsNot Nothing Then
                    body = node.ExpressionBody.GetExpressionBodyStatements(False, Me)
                Else
                    Throw UnreachableException
                End If
                Dim parameterList As VBS.ParameterListSyntax = DirectCast(node.ParameterList?.Accept(Me), VBS.ParameterListSyntax).WithRestructuredEolTrivia
                Dim modifiers As SyntaxTokenList = Factory.TokenList(ConvertModifiers(node.Modifiers, Me.IsModule, TokenContext.Member))
                Dim lSyntaxKind As CS.SyntaxKind = CS.CSharpExtensions.Kind(node.OperatorToken)

                If node.ParameterList?.Parameters.FirstOrDefault() Is Nothing Then
                    Throw New NotSupportedException("Operator overloads with no parameters aren't supported")
                End If
                Dim firstParam As CSS.ParameterSyntax = node.ParameterList?.Parameters.FirstOrDefault()
                Dim firstParameterIsString As Boolean = _semanticModel.GetTypeInfo(firstParam.Type).ConvertedType.SpecialType = SpecialType.System_String
                Select Case lSyntaxKind
                    Case CS.SyntaxKind.MinusMinusToken
                        Return Factory.EmptyStatement.WithLeadingTrivia(node.CheckCorrectnessLeadingTrivia(attemptToPortMade:=False, "C# -- operator not available in VB")).WithPrependedLeadingTrivia(node.GetLeadingTrivia.ConvertTriviaList()).WithConvertedTrailingTriviaFrom(node)
                    Case CS.SyntaxKind.PercentToken
                        Dim stmt As VBS.OperatorStatementSyntax = Factory.OperatorStatement(Factory.List(attributeLists), modifiers, ModKeyword, parameterList, Factory.SimpleAsClause(returnAttributes, DirectCast(node.ReturnType.Accept(Me), VBS.TypeSyntax)))
                        Return Factory.OperatorBlock(stmt, body).WithConvertedTriviaFrom(node)
                    Case CS.SyntaxKind.PlusPlusToken
                        Return Factory.EmptyStatement.WithLeadingTrivia(node.CheckCorrectnessLeadingTrivia(attemptToPortMade:=False, "C# ++ operator not available in VB")).WithPrependedLeadingTrivia(node.GetLeadingTrivia.ConvertTriviaList()).WithConvertedTrailingTriviaFrom(node)
                    Case Else
                        Dim operatorToken As SyntaxToken = lSyntaxKind.GetComparisonOperatorToken(firstParameterIsString)
                        Dim stmt As VBS.OperatorStatementSyntax = Factory.OperatorStatement(Factory.List(attributeLists), Factory.TokenList(modifiers), operatorToken, parameterList, Factory.SimpleAsClause(returnAttributes, DirectCast(node.ReturnType.Accept(Me), VBS.TypeSyntax)))
                        If finalTrailingDirective.Any Then
                            stmt = stmt.WithAppendedTrailingTrivia(finalTrailingDirective)
                        End If
                        Return Factory.OperatorBlock(stmt, body).WithConvertedTriviaFrom(node)
                End Select
            End Function

            Public Overrides Function VisitPropertyDeclaration(node As CSS.PropertyDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim savedNeedEndUsingCount As Integer = _neededEndUsingCount
                Try

                    _neededEndUsingCount = 0
                    Dim csAccessors As New List(Of CSS.AccessorDeclarationSyntax)
                    If node.AccessorList IsNot Nothing Then
                        csAccessors.AddRange(node.AccessorList.Accessors)
                    End If
                    Dim implementsClauseOrNothing As VBS.ImplementsClauseSyntax
                    Dim identifierValueText As String = node.Identifier.ValueText
                    Dim simpleName As VBS.IdentifierNameSyntax = Factory.IdentifierName(identifierValueText)

                    Dim typeNode As VBS.TypeSyntax = DirectCast(node.Type.Accept(Me), VBS.TypeSyntax)
                    If typeNode.ToString.Equals("dynamic", StringComparison.OrdinalIgnoreCase) Then
                        typeNode = Factory.PredefinedType(ObjectKeyword).WithTriviaFrom(typeNode)
                    End If
                    Dim closingNodeBraces As SyntaxToken = node.GetBraces.Item2
                    Dim endPropertyStatement As VBS.EndBlockStatementSyntax = FactoryEndBlockStatement(VB.SyntaxKind.EndPropertyStatement, PropertyKeyword, closingNodeBraces.CollectConvertedTokenTrivia(getLeading:=True, getTrailing:=True))

                    Dim typeLeadingTrivia As New SyntaxTriviaList
                    typeLeadingTrivia = typeLeadingTrivia.AddRange(node.Type.GetLeadingTrivia.ConvertTriviaList())
                    If EndsWithSimilarTrivia(typeLeadingTrivia, Factory.TriviaList(node.GetLeadingTrivia.ConvertTriviaList())) Then
                        typeLeadingTrivia = New SyntaxTriviaList
                    End If
                    Dim idString As String = ""
                    Dim interfaceMembers As SeparatedSyntaxList(Of VBS.QualifiedNameSyntax)
                    If node.ExplicitInterfaceSpecifier IsNot Nothing Then
                        node.ExplicitInterfaceSpecifier.Accept(Me).TypeSwitch(
                        Sub(explicitInterfaceIdentifier As VBS.QualifiedNameSyntax)
                            idString = explicitInterfaceIdentifier.Right.ToString
                            Dim openParenIndex As Integer = idString.IndexOf("(", StringComparison.Ordinal)
                            If openParenIndex > 0 Then
                                idString = idString.Substring(startIndex:=0, openParenIndex)
                            End If
                            interfaceMembers = interfaceMembers.Add(Factory.QualifiedName(explicitInterfaceIdentifier, simpleName))
                        End Sub,
                        Sub(genericName As VBS.GenericNameSyntax)
                            idString = genericName.Identifier.ToString
                            interfaceMembers = interfaceMembers.Add(Factory.QualifiedName(genericName, simpleName))
                        End Sub,
                        Sub(identifierName As VBS.IdentifierNameSyntax)
                            idString = identifierName.Identifier.ToString
                            interfaceMembers = interfaceMembers.Add(Factory.QualifiedName(identifierName, simpleName))
                        End Sub,
                        Sub(__ As SyntaxNode)
                            Throw New NotImplementedException($"{__.GetType().FullName} not implemented!")
                        End Sub)
                    End If

                    Dim propertyNameToken As SyntaxToken
                    Dim propertyStatement As VBS.PropertyStatementSyntax
                    If node.ExplicitInterfaceSpecifier Is Nothing Then
                        propertyNameToken = Me.GenerateSafeVbToken(node.Identifier, node)
                        Dim propertySymbol As IPropertySymbol = CType(_semanticModel.GetDeclaredSymbol(node), IPropertySymbol)
                        implementsClauseOrNothing = If(propertySymbol Is Nothing, Nothing, Me.CreateImplementsClauseSyntaxOrNull(propertySymbol, propertyNameToken))
                    Else
                        propertyNameToken = Factory.Identifier($"{idString.Replace("[", "").Replace("]", "")}_{identifierValueText}")
                        implementsClauseOrNothing = Factory.ImplementsClause(interfaceMembers)
                    End If
                    propertyNameToken = propertyNameToken.WithTrailingTrivia(SpaceTrivia)
                    Dim returnAttributes As SyntaxList(Of VBS.AttributeListSyntax) = Nothing
                    Dim finalTrailingDirective As New SyntaxTriviaList
                    Dim attributes As List(Of VBS.AttributeListSyntax) = Me.ConvertAndSplitAttributes(node.AttributeLists, returnAttributes, finalTrailingDirective)
                    If finalTrailingDirective.Any Then
                        Stop
                    End If
                    Dim isIterator As Boolean = False
                    Dim accessors As New List(Of VBS.AccessorBlockSyntax)
                    Dim statements As SyntaxList(Of VBS.StatementSyntax)
                    Dim csModifiers As SyntaxTokenList = node.Modifiers
                    Dim asClause As VBS.SimpleAsClauseSyntax = Factory.SimpleAsClause(returnAttributes, typeNode.WithoutTrivia)
                    Dim context As TokenContext = TokenContext.Property
                    Dim localIsModule As Boolean = Me.IsModule OrElse node.Parent.IsKind(CS.SyntaxKind.CompilationUnit)
                    If node.ExpressionBody IsNot Nothing Then
                        Dim exprSyntaxNode As VB.VisualBasicSyntaxNode = node.ExpressionBody.Expression.Accept(Me).WithConvertedLeadingTriviaFrom(node.ExpressionBody.Expression)
                        Dim statementList As New List(Of VBS.StatementSyntax)
                        Select Case True
                            Case TypeOf exprSyntaxNode Is VBS.ThrowStatementSyntax
                                statements = Factory.SingletonList(Of VBS.StatementSyntax)(DirectCast(exprSyntaxNode, VBS.ThrowStatementSyntax).WithConvertedTriviaFrom(node.ExpressionBody))
                            Case TypeOf exprSyntaxNode Is VBS.SingleLineIfStatementSyntax
                                Dim ifStatement As VBS.SingleLineIfStatementSyntax = DirectCast(exprSyntaxNode, VBS.SingleLineIfStatementSyntax).WithTrailingEol
                                Dim retStmt As VBS.ReturnStatementSyntax = Factory.ReturnStatement(DirectCast(ifStatement.Condition, VBS.BinaryExpressionSyntax).Left).
                                                WithLeadingTrivia(ifStatement.Condition.GetLeadingTrivia)
                                retStmt = retStmt.RelocateDirectivesInLeadingTrivia
                                statementList.AddRange({ifStatement,
                                                    retStmt
                                                   })
                                statements = ReplaceStatementsWithMarkedStatements(node, statementList)
                            Case TypeOf exprSyntaxNode Is VBS.AssignmentStatementSyntax
                                Dim assignmentStmt As VBS.AssignmentStatementSyntax = DirectCast(exprSyntaxNode, VBS.AssignmentStatementSyntax).WithTrailingEol
                                Dim retStmt As VBS.ReturnStatementSyntax = Factory.ReturnStatement(assignmentStmt.Left.WithoutLeadingTrivia).
                                                WithLeadingTrivia(assignmentStmt.GetLeadingTrivia)
                                retStmt = retStmt.RelocateDirectivesInLeadingTrivia
                                statementList.AddRange({assignmentStmt,
                                                    retStmt
                                                   })
                                statements = ReplaceStatementsWithMarkedStatements(node, statementList)
                            Case TypeOf exprSyntaxNode Is VBS.ExpressionSyntax
                                Dim returnedExpr As VBS.ExpressionSyntax = DirectCast(exprSyntaxNode, VBS.ExpressionSyntax)
                                If returnedExpr Is Nothing Then
                                    ' ref expression
                                    Dim refExpr As CSS.ExpressionSyntax = node.ExpressionBody.Expression
                                    If refExpr Is Nothing Then
                                        Stop
                                    Else
                                        returnedExpr = DirectCast(DirectCast(refExpr, CSS.RefExpressionSyntax).Expression.Accept(Me), VBS.ExpressionSyntax)
                                    End If
                                Else
                                    returnedExpr = returnedExpr.WithConvertedTriviaFrom(node.ExpressionBody)
                                End If

                                Dim retStmt As VBS.ReturnStatementSyntax = Factory.ReturnStatement(returnedExpr.WithLeadingTrivia(SpaceTrivia)).
                                                WithLeadingTrivia(returnedExpr.GetLeadingTrivia)
                                retStmt = retStmt.RelocateDirectivesInLeadingTrivia
                                statements = ReplaceOneStatementWithMarkedStatements(node.ExpressionBody, retStmt)
                            Case Else
                                Throw UnreachableException
                        End Select
                        statements = Me.AdjustUsingIfNeeded(statements)
                        accessors.Add(Factory.AccessorBlock(VB.SyntaxKind.GetAccessorBlock,
                                                        Factory.GetAccessorStatement.WithTrailingEol,
                                                        statements,
                                                        Factory.EndGetStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), GetKeyword)))
                    Else
                        If node.AccessorList IsNot Nothing Then
                            If (Not node.Modifiers.Contains(CS.SyntaxKind.AbstractKeyword)) AndAlso csAccessors.Count = 2 AndAlso
                            csAccessors(0).Body Is Nothing AndAlso csAccessors(0).ExpressionBody Is Nothing AndAlso
                            csAccessors(1).Body Is Nothing AndAlso csAccessors(1).ExpressionBody Is Nothing Then
                                Dim getModifiers As List(Of SyntaxToken) = ConvertModifiers(csAccessors(0).Modifiers, localIsModule, context).ToList
                                Dim setModifiers As List(Of SyntaxToken) = ConvertModifiers(csAccessors(1).Modifiers, localIsModule, context).ToList
                                Dim propertyStatementLeadingTrivia As New SyntaxTriviaList
                                Dim getModifierStr As String = If(getModifiers.Any, getModifiers(0).ValueText, "")
                                Dim setModifierStr As String = If(setModifiers.Any, setModifiers(0).ValueText, "")
                                If getModifierStr <> setModifierStr Then
                                    ' Handle
                                    ' public string BuyerId { get; protected set; }
                                    ' Dim _buyerId As String
                                    If getModifiers.Any AndAlso getModifiers(0).LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                        propertyStatementLeadingTrivia = propertyStatementLeadingTrivia.AddRange(getModifiers(0).LeadingTrivia)
                                        getModifiers(0) = getModifiers(0).WithoutTrivia
                                    End If
                                    If setModifiers.Any AndAlso setModifiers?(0).LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                        propertyStatementLeadingTrivia = propertyStatementLeadingTrivia.AddRange(setModifiers(0).LeadingTrivia)
                                        setModifiers(0) = setModifiers(0).WithoutTrivia
                                    End If
                                    Dim newVariableToken As SyntaxToken = Factory.Identifier($"_{Char.ToLower(identifierValueText.Chars(0), Globalization.CultureInfo.InvariantCulture)}{identifierValueText.Substring(1)}")
                                    Dim newModifiedIdentifier As VBS.ModifiedIdentifierSyntax = Factory.ModifiedIdentifier(newVariableToken)
                                    Dim declarators As SeparatedSyntaxList(Of VBS.VariableDeclaratorSyntax) =
                                                        Factory.SingletonSeparatedList(
                                                            Factory.VariableDeclarator(
                                                                Factory.SingletonSeparatedList(newModifiedIdentifier),
                                                                asClause,
                                                                initializer:=Nothing)
                                                        )
                                    Dim dimStatement As VBS.StatementSyntax = Factory.LocalDeclarationStatement(Factory.TokenList(PrivateKeyword.WithTrailingTrivia(SpaceTrivia)), declarators).WithConvertedLeadingTriviaFrom(node).WithTrailingEol
                                    Dim stmtWithIssues As CS.CSharpSyntaxNode = GetStatementWithIssues(node)
                                    stmtWithIssues.AddMarker(dimStatement, StatementHandlingOption.PrependStatement, allowDuplicates:=False)
                                    Dim stmt As VBS.AccessorStatementSyntax = Factory.GetAccessorStatement(Nothing, Factory.TokenList(getModifiers), parameterList:=Nothing)
                                    Dim body As SyntaxList(Of VBS.StatementSyntax) = Factory.SingletonList(Of VBS.StatementSyntax)(Factory.ReturnStatement(Factory.IdentifierName(newVariableToken)).WithTrailingEol)
                                    accessors.Add(Factory.AccessorBlock(VB.SyntaxKind.GetAccessorBlock, stmt.WithTrailingEol, body, Factory.EndGetStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), GetKeyword).WithTrailingEol))

                                    stmt = Factory.SetAccessorStatement(Nothing,
                                                                    Factory.TokenList(setModifiers),
                                                                    Factory.ParameterList(Factory.SingletonSeparatedList(Factory.Parameter(ValueModifiedIdentifier).WithAsClause(asClause))))
                                    Dim valueExpression As VBS.ExpressionSyntax = Factory.ParseExpression(newVariableToken.ValueText)
                                    body = Factory.SingletonList(Of VBS.StatementSyntax)(Factory.SimpleAssignmentStatement(valueExpression, Factory.IdentifierName(ValueModifiedIdentifier.Identifier)).WithTrailingEol)
                                    accessors.Add(Factory.AccessorBlock(VB.SyntaxKind.SetAccessorBlock, stmt.WithTrailingEol, body, Factory.EndSetStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), SetKeyword).WithTrailingEol))

                                    propertyStatement = Factory.PropertyStatement(Factory.List(attributes),
                                                                              Factory.TokenList(ConvertModifiers(csModifiers, localIsModule, context)),
                                                                              propertyNameToken,
                                                                              parameterList:=Nothing,
                                                                              asClause,
                                                                              initializer:=Nothing,
                                                                              implementsClause:=Nothing)
                                    Return Factory.PropertyBlock(propertyStatement.WithPrependedLeadingTrivia(propertyStatementLeadingTrivia).WithTrailingEol,
                                                             Factory.List(accessors),
                                                             endPropertyStatement.WithConvertedTrailingTriviaFrom(node)).
                                        RestructureAttributesAndModifiers(attributes.Any, hasModifiers:=True)

                                End If

                            End If
                            For Each a As CSS.AccessorDeclarationSyntax In csAccessors
                                Dim foundIterator As Boolean
                                accessors.Add(Me.ConvertAccessor(a, localIsModule, foundIterator))
                                isIterator = isIterator Or foundIterator
                            Next
                        End If
                    End If

                    Dim isWriteOnly As Boolean = False
                    If node.AccessorList IsNot Nothing AndAlso csAccessors.Count = 1 Then
                        Select Case csAccessors(0).Keyword.RawKind
                            Case CS.SyntaxKind.GetKeyword
                                csModifiers = csModifiers.Add(s_csReadOnlyKeyword)
                            Case CS.SyntaxKind.SetKeyword
                                isWriteOnly = True
                            Case Else
                                _reportException?.Invoke(UnreachableException)
                        End Select

                    End If
                    ' TODO find better way to find out if we are in interface
                    If node.IsParentKind(CS.SyntaxKind.InterfaceDeclaration) Then
                        context = TokenContext.InterfaceOrModule
                    End If
                    Dim modifiers As List(Of SyntaxToken) = ConvertModifiers(csModifiers, localIsModule, context).ToList
                    If isIterator Then
                        modifiers.Add(IteratorKeyword)
                    End If
                    If isWriteOnly Then
                        modifiers.Add(WriteOnlyKeyword)
                    End If

                    If node.AccessorList Is Nothing Then
                        modifiers.Add(ReadOnlyKeyword)
                    End If

                    Dim initializer As VBS.EqualsValueSyntax = If(node.Initializer Is Nothing, Nothing, Factory.EqualsValue(DirectCast(node.Initializer.Value.Accept(Me), VBS.ExpressionSyntax)))
                    Dim keyword As SyntaxToken

                    If modifiers.Count = 0 Then
                        keyword = PropertyKeyword.WithLeadingTrivia(typeLeadingTrivia)
                        typeLeadingTrivia = New SyntaxTriviaList
                    Else
                        keyword = PropertyKeyword
                        If typeLeadingTrivia.Any Then
                            typeLeadingTrivia = typeLeadingTrivia.Insert(0, VbEolTrivia)
                        End If
                    End If
                    Dim prependedTrivia As SyntaxTriviaList = node.DedupLeadingTrivia(keyword, attributes, modifiers)
                    If attributes.Any AndAlso modifiers.Any AndAlso modifiers(0).LeadingTrivia.ContainsCommentTrivia Then
                        Dim attributeTrailingTrivia As SyntaxTriviaList = attributes.Last.GetTrailingTrivia
                        If attributeTrailingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            If attributeTrailingTrivia.Count = 1 Then
                                attributeTrailingTrivia = attributeTrailingTrivia.InsertRange(0, SpaceLineContinue)
                            ElseIf attributeTrailingTrivia(attributeTrailingTrivia.Count - 2).IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                                attributeTrailingTrivia = attributeTrailingTrivia.InsertRange(attributeTrailingTrivia.Count - 2, SpaceLineContinue)
                            Else
                                attributeTrailingTrivia = attributeTrailingTrivia.InsertRange(0, SpaceLineContinue)
                            End If
                        Else
                            Stop
                        End If
                        modifiers(0) = modifiers(0).AdjustTokenLeadingTrivia()
                        attributes(attributes.Count - 1) = attributes.Last.WithTrailingTrivia(attributeTrailingTrivia)
                    End If
                    propertyStatement = Factory.PropertyStatement(Factory.List(attributes),
                                                                Factory.TokenList(modifiers),
                                                                keyword,
                                                                propertyNameToken.WithTrailingTrivia(SpaceTrivia),
                                                                parameterList:=Nothing,
                                                                asClause,
                                                                initializer.WithTrailingEol,
                                                                implementsClauseOrNothing
                                                                ).WithPrependedLeadingTrivia(prependedTrivia).WithTrailingEol
                    Dim stmtList As SyntaxList(Of VBS.StatementSyntax) = ReplaceOneStatementWithMarkedStatements(node, propertyStatement)
                    Dim addedLeadingTrivia As New SyntaxTriviaList
                    Select Case stmtList.Count
                        Case 1
                            If TypeOf stmtList(0) Is VBS.EmptyStatementSyntax Then
                                addedLeadingTrivia = addedLeadingTrivia.AddRange(stmtList(0).GetLeadingTrivia)
                            End If
                        Case 2
                            addedLeadingTrivia = addedLeadingTrivia.AddRange(stmtList(0).GetLeadingTrivia)
                    End Select
                    If node.AccessorList IsNot Nothing Then
                        Dim bodyOrExpressionBodyIsNothing As Boolean = True
                        For Each a As CSS.AccessorDeclarationSyntax In csAccessors
                            If a.Body IsNot Nothing OrElse a.ExpressionBody IsNot Nothing Then
                                bodyOrExpressionBodyIsNothing = False
                                Exit For
                            End If
                        Next
                        If bodyOrExpressionBodyIsNothing Then
                            If addedLeadingTrivia.Any Then
                                Return propertyStatement.WithLeadingTrivia(addedLeadingTrivia).WithConvertedTrailingTriviaFrom(node).WithTrailingEol
                            End If
                            Return propertyStatement.WithConvertedTriviaFrom(node).WithTrailingEol
                        End If
                    End If
                    If addedLeadingTrivia.Any Then
                        Return Factory.PropertyBlock(propertyStatement.WithLeadingTrivia(addedLeadingTrivia).WithTrailingEol, Factory.List(accessors))
                    End If

                    Dim accessorOpenBraceTrivia As SyntaxTriviaList = node.AccessorList.GetBraces.Item1.CollectConvertedTokenTrivia(getLeading:=True, getTrailing:=True)
                    Dim accessorClosingBraceTrivia As SyntaxTriviaList = node.AccessorList.GetBraces.Item2.CollectConvertedTokenTrivia(getLeading:=True, getTrailing:=True)

                    endPropertyStatement = Factory.EndPropertyStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), PropertyKeyword).
                                WithConvertedTriviaFrom(closingNodeBraces).
                                WithPrependedLeadingTrivia(accessorClosingBraceTrivia).WithTrailingEol
                    accessors(0) = accessors(0).WithPrependedLeadingTrivia(accessorOpenBraceTrivia).WithTrailingEol
                    Return Factory.PropertyBlock(propertyStatement.WithTrailingEol,
                                               Factory.List(accessors),
                                               endPropertyStatement.WithTrailingEol).
                                        WithAppendedTrailingTrivia(typeLeadingTrivia).
                                        RestructureAttributesAndModifiers(attributes.Any, modifiers.Any).NormalizeWhitespaceEx(useDefaultCasing:=True).WithTrailingEol
                Finally
                    _neededEndUsingCount = savedNeedEndUsingCount
                End Try
            End Function

        End Class

    End Class

End Namespace
