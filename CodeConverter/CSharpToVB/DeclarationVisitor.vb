' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

Imports System.Runtime.InteropServices

Imports IVisualBasicCode.CodeConverter.Util

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace IVisualBasicCode.CodeConverter.Visual_Basic

    Partial Public Class CSharpConverter

        Partial Protected Friend Class NodesVisitor
            Inherits CS.CSharpSyntaxVisitor(Of VB.VisualBasicSyntaxNode)
            Private Const CompilerServices As String = "System.Runtime.CompilerServices"
            Private ReadOnly ExtensionAttribute As VBS.AttributeSyntax = VBFactory.Attribute(Nothing, VBFactory.ParseTypeName("Extension"), VBFactory.ArgumentList())

            Private Shared Function AddBracketsIfRequired(Id As String) As String
                If IsSpecialReservedWord(Id) OrElse VB.SyntaxFacts.IsKeywordKind(VB.SyntaxFacts.GetKeywordKind(Id)) Then
                    Return $"[{Id}]"
                End If
                Return Id
            End Function

            Private Shared Function ConvertOperatorDeclarationToken(lSyntaxKind As CS.SyntaxKind) As SyntaxToken
                Select Case lSyntaxKind
                    Case CS.SyntaxKind.AmpersandToken
                        Return AndKeyword
                    Case CS.SyntaxKind.AsteriskToken
                        Return AsterickToken
                    Case CS.SyntaxKind.BarToken
                        Return OrKeyword
                    Case CS.SyntaxKind.CaretToken
                        Return XorKeyword
                    Case CS.SyntaxKind.EqualsEqualsToken
                        Return EqualsToken
                    Case CS.SyntaxKind.EqualsGreaterThanToken
                        Return GreaterThanEqualsToken
                    Case CS.SyntaxKind.ExclamationEqualsToken
                        Return LessThanGreaterThanToken
                    Case CS.SyntaxKind.ExclamationToken
                        Return NotKeyword
                    Case CS.SyntaxKind.GreaterThanEqualsToken
                        Return GreaterThanEqualsToken
                    Case CS.SyntaxKind.GreaterThanToken
                        Return GreaterThanToken
                    Case CS.SyntaxKind.GreaterThanGreaterThanToken
                        Return GreaterThanGreaterThanToken
                    Case CS.SyntaxKind.LessThanToken
                        Return LessThanToken
                    Case CS.SyntaxKind.LessThanLessThanEqualsToken
                        Return LessThanLessThanEqualsToken
                    Case CS.SyntaxKind.LessThanLessThanToken
                        Return LessThanLessThanToken
                    Case CS.SyntaxKind.LessThanEqualsToken
                        Return LessThanEqualsToken
                    Case CS.SyntaxKind.MinusToken
                        Return MinusToken
                    Case CS.SyntaxKind.PlusToken
                        Return PlusToken
                    Case CS.SyntaxKind.SlashToken
                        Return SlashToken
                    Case CS.SyntaxKind.TildeToken
                        Return NotKeyword
                    Case CS.SyntaxKind.FalseKeyword
                        Return IsFalse
                    Case CS.SyntaxKind.TrueKeyword
                        Return IsTrueKeyword
                End Select
                Throw New NotSupportedException($"Assignment Operator {lSyntaxKind.ToString} is not supported")
            End Function

            Private Shared Function GetTriviaFromUnneededToken(_SemicolonToken As SyntaxToken) As List(Of SyntaxTrivia)
                Dim NewTrailingTrivia As New List(Of SyntaxTrivia)
                If _SemicolonToken.HasLeadingTrivia Then
                    NewTrailingTrivia.AddRange(ConvertTrivia(_SemicolonToken.LeadingTrivia))
                End If
                If _SemicolonToken.HasTrailingTrivia Then
                    NewTrailingTrivia.AddRange(ConvertTrivia(_SemicolonToken.TrailingTrivia))
                End If
                Return NewTrailingTrivia
            End Function

            Private Shared Function IsSpecialReservedWord(ID As String) As Boolean
                If ID.Equals("Alias", StringComparison.InvariantCultureIgnoreCase) OrElse
                    ID.Equals("CType", StringComparison.InvariantCultureIgnoreCase) OrElse
                    ID.Equals("End", StringComparison.InvariantCultureIgnoreCase) OrElse
                    ID.Equals("Error", StringComparison.InvariantCultureIgnoreCase) OrElse
                    ID.Equals("Event", StringComparison.InvariantCultureIgnoreCase) OrElse
                    ID.Equals("Imports", StringComparison.InvariantCultureIgnoreCase) OrElse
                    ID.Equals("Module", StringComparison.InvariantCultureIgnoreCase) OrElse
                    ID.Equals("Option", StringComparison.InvariantCultureIgnoreCase) OrElse
                    ID.Equals("Optional", StringComparison.InvariantCultureIgnoreCase) Then
                    Return True
                End If
                Return False
            End Function

            Private Shared Function RelocateDirectivesInTrailingTrivia(ParameterList As VBS.ParameterListSyntax, StatementTrailingTrivia As List(Of SyntaxTrivia)) As VBS.ParameterListSyntax
                If ParameterList IsNot Nothing AndAlso ParameterList.HasTrailingTrivia AndAlso ParameterList.GetTrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                    Dim ParameterListTrailingTrivia As New List(Of SyntaxTrivia)
                    Dim TrailingTrivialist As SyntaxTriviaList = ParameterList.GetTrailingTrivia
                    Dim FoundEndIf As Boolean = False
                    For i As Integer = 0 To TrailingTrivialist.Count - 1
                        Dim Trivia As SyntaxTrivia = TrailingTrivialist(i)
                        Dim NextTrivia As SyntaxTrivia = If(i < TrailingTrivialist.Count - 1, TrailingTrivialist(i + 1), Nothing)
                        Select Case Trivia.RawKind
                            Case VB.SyntaxKind.CommentTrivia
                                ParameterListTrailingTrivia.Add(Trivia)
                            Case VB.SyntaxKind.EndOfLineTrivia
                                If FoundEndIf Then
                                    StatementTrailingTrivia.Add(Trivia)
                                Else
                                    ParameterListTrailingTrivia.Add(Trivia)
                                End If
                            Case VB.SyntaxKind.WhitespaceTrivia
                                ParameterListTrailingTrivia.Add(Trivia)
                            Case VB.SyntaxKind.EndIfDirectiveTrivia
                                FoundEndIf = True
                                If Not StatementTrailingTrivia.Any Then
                                    StatementTrailingTrivia.Add(VB_EOLTrivia)
                                End If
                                StatementTrailingTrivia.Add(Trivia)
                            Case Else
                                Stop
                        End Select
                    Next
                    ParameterList = ParameterList.WithTrailingTrivia(ParameterListTrailingTrivia)
                End If

                Return ParameterList
            End Function

            Private Function ConvertAccessor(node As CSS.AccessorDeclarationSyntax, IsModule As Boolean, ByRef isIterator As Boolean) As VBS.AccessorBlockSyntax
                Dim blockKind As VB.SyntaxKind
                Dim stmt As VBS.AccessorStatementSyntax
                Dim endStmt As VBS.EndBlockStatementSyntax
                Dim body As SyntaxList(Of VBS.StatementSyntax) = VBFactory.List(Of VBS.StatementSyntax)()
                isIterator = False
                Dim visitor As MethodBodyVisitor = New MethodBodyVisitor(mSemanticModel, Me)
                If node.Body IsNot Nothing Then
                    body = VBFactory.List(node.Body.Statements.SelectMany(Function(s As CSS.StatementSyntax) s.Accept(visitor)))
                    isIterator = visitor.IsInterator
                ElseIf node.ExpressionBody IsNot Nothing Then
                    Dim VBNode As VB.VisualBasicSyntaxNode = node.ExpressionBody.Accept(Me)
                    If TypeOf VBNode Is VBS.AssignmentStatementSyntax OrElse TypeOf VBNode Is VBS.ThrowStatementSyntax Then
                        body = VBFactory.SingletonList(DirectCast(VBNode, VBS.StatementSyntax))
                    Else
                        body = VBFactory.SingletonList(Of VBS.StatementSyntax)(
                                                    VBFactory.ReturnStatement(DirectCast(VBNode, VBS.ExpressionSyntax)).
                                                                    WithTrailingEOL
                                                                              )
                    End If
                End If
                Dim Attributes As SyntaxList(Of VBS.AttributeListSyntax) = VBFactory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))
                Dim Modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, IsModule, TokenContext.Local)
                Dim Parent As CSS.BasePropertyDeclarationSyntax = DirectCast(node.Parent.Parent, CSS.BasePropertyDeclarationSyntax)
                Dim ValueParam As VBS.ParameterSyntax

                Select Case CS.CSharpExtensions.Kind(node)
                    Case CS.SyntaxKind.GetAccessorDeclaration
                        blockKind = VB.SyntaxKind.GetAccessorBlock
                        stmt = VBFactory.GetAccessorStatement(Attributes, VBFactory.TokenList(Modifiers), parameterList:=Nothing)
                        endStmt = VBFactory.EndGetStatement()
                    Case CS.SyntaxKind.SetAccessorDeclaration
                        blockKind = VB.SyntaxKind.SetAccessorBlock
                        ValueParam = VBFactory.Parameter(Value_ModifiedIdentifier).
                            WithAsClause(VBFactory.SimpleAsClause(DirectCast(Parent.Type.Accept(Me), VBS.TypeSyntax).
                            WithLeadingTrivia(SpaceTrivia)))
                        stmt = VBFactory.SetAccessorStatement(Attributes, VBFactory.TokenList(Modifiers), VBFactory.ParameterList(VBFactory.SingletonSeparatedList(ValueParam)))
                        endStmt = VBFactory.EndSetStatement()
                    Case CS.SyntaxKind.AddAccessorDeclaration
                        blockKind = VB.SyntaxKind.AddHandlerAccessorBlock
                        ValueParam = VBFactory.Parameter(Value_ModifiedIdentifier).
                            WithAsClause(VBFactory.SimpleAsClause(DirectCast(Parent.Type.Accept(Me), VBS.TypeSyntax)))
                        stmt = VBFactory.AddHandlerAccessorStatement(Attributes, VBFactory.TokenList(Modifiers), VBFactory.ParameterList(VBFactory.SingletonSeparatedList(ValueParam)))
                        endStmt = VBFactory.EndAddHandlerStatement()
                    Case CS.SyntaxKind.RemoveAccessorDeclaration
                        blockKind = VB.SyntaxKind.RemoveHandlerAccessorBlock
                        ValueParam = VBFactory.Parameter(Value_ModifiedIdentifier).
                            WithAsClause(VBFactory.SimpleAsClause(DirectCast(Parent.Type.Accept(Me), VBS.TypeSyntax)))
                        stmt = VBFactory.RemoveHandlerAccessorStatement(Attributes, VBFactory.TokenList(Modifiers), VBFactory.ParameterList(VBFactory.SingletonSeparatedList(ValueParam)))
                        endStmt = VBFactory.EndRemoveHandlerStatement()
                    Case Else
                        Throw New NotSupportedException()
                End Select
                Dim OpenBrace As SyntaxToken = node.Body.GetBraces.Item2
                Dim CloseBrace As SyntaxToken = node.Body.GetBraces.Item2
                Return VBFactory.AccessorBlock(blockKind, stmt.WithConvertedTriviaFrom(OpenBrace).WithTrailingEOL, body, endStmt.WithConvertedTriviaFrom(CloseBrace)).WithConvertedTriviaFrom(node)
            End Function

            Private Sub ConvertAndSplitAttributes(attributeLists As SyntaxList(Of CSS.AttributeListSyntax), <Out> ByRef Attributes As List(Of VBS.AttributeListSyntax), <Out> ByRef ReturnAttributes As SyntaxList(Of VBS.AttributeListSyntax))
                Dim retAttr As List(Of VBS.AttributeListSyntax) = New List(Of VBS.AttributeListSyntax)()
                For Each attrList As CSS.AttributeListSyntax In attributeLists
                    If attrList.Target IsNot Nothing AndAlso attrList.Target.Identifier.IsKind(CS.SyntaxKind.ReturnKeyword) Then
                        ' Remove trailing CRLF from return attributes
                        retAttr.Add(DirectCast(attrList.Accept(Me).With({SpaceTrivia}, {SpaceTrivia}), VBS.AttributeListSyntax))
                    Else
                        Attributes.Add(DirectCast(attrList.Accept(Me), VBS.AttributeListSyntax))
                    End If
                Next

                ReturnAttributes = VBFactory.List(retAttr)
            End Sub

            Private Function DedupLeadingTrivia(Of T As CS.CSharpSyntaxNode)(node As T, Keyword As SyntaxToken, Attributes As List(Of VBS.AttributeListSyntax), Modifiers As List(Of SyntaxToken)) As List(Of SyntaxTrivia)
                Dim NodeLeadingTrivia As New List(Of SyntaxTrivia)
                NodeLeadingTrivia.AddRange(ConvertTrivia(node.GetLeadingTrivia))
                If Attributes.Count > 0 Then
                    If TriviaIsIdentical(Attributes(0).GetLeadingTrivia, NodeLeadingTrivia) Then
                        NodeLeadingTrivia.Clear()
                    End If
                ElseIf Modifiers.Count > 0 Then
                    If TriviaIsIdentical(Modifiers(0).LeadingTrivia, NodeLeadingTrivia) Then
                        NodeLeadingTrivia.Clear()
                    End If
                ElseIf TriviaIsIdentical(Keyword.LeadingTrivia, NodeLeadingTrivia) Then
                    NodeLeadingTrivia.Clear()
                End If

                Return NodeLeadingTrivia
            End Function

            Public Overrides Function VisitAnonymousObjectMemberDeclarator(node As CSS.AnonymousObjectMemberDeclaratorSyntax) As VB.VisualBasicSyntaxNode
                If node.NameEquals Is Nothing Then
                    Return VBFactory.InferredFieldInitializer(DirectCast(node.Expression.Accept(Me), VBS.ExpressionSyntax)).WithConvertedTriviaFrom(node)
                Else
                    Return VBFactory.NamedFieldInitializer(KeyKeyword,
                                                               DotToken,
                                                               name:=DirectCast(node.NameEquals.Name.Accept(Me), VBS.IdentifierNameSyntax),
                                                               EqualsToken,
                                                               expression:=DirectCast(node.Expression.Accept(Me), VBS.ExpressionSyntax)
                                                               ).WithConvertedTriviaFrom(node)
                End If
            End Function

            Public Overrides Function VisitArrowExpressionClause(node As CSS.ArrowExpressionClauseSyntax) As VB.VisualBasicSyntaxNode
                Return node.Expression.Accept(Me)
            End Function

            Public Overrides Function VisitConstructorDeclaration(node As CSS.ConstructorDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim Attributes As SyntaxList(Of VBS.AttributeListSyntax) = VBFactory.List(
                        node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))
                Dim Initializer As VBS.ExpressionStatementSyntax = Nothing
                If node.Initializer IsNot Nothing Then
                    Initializer = DirectCast(node.Initializer.Accept(Me), VBS.ExpressionStatementSyntax)
                End If
                Dim Modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, IsModule, TokenContext.[New])

                Dim parameterList As VBS.ParameterListSyntax = DirectCast(node.ParameterList?.Accept(Me), VBS.ParameterListSyntax)
                Dim SubNewStatement As VBS.SubNewStatementSyntax =
                    CType(VBFactory.SubNewStatement(
                                                    Attributes,
                                                    VBFactory.TokenList(Modifiers),
                                                    parameterList
                                                    ).WithTrailingEOL.
                                                    RestructureAttributesAndModifiers(
                                                                    Attributes.Count > 0,
                                                                    Modifiers.Count > 0
                                                                    ),
                            VBS.SubNewStatementSyntax)

                Dim EndSubStatement As VBS.EndBlockStatementSyntax = VBFactory.EndSubStatement.WithTrailingEOL
                Dim Body As New SyntaxList(Of VBS.StatementSyntax)
                Dim CS_CloseBraceToken As SyntaxToken = CSharpSyntaxFactory.CloseBraceToken
                Dim CloseBraceTrivia As New List(Of SyntaxTrivia)
                If node.Body IsNot Nothing Then
                    Dim Statement As New List(Of VBS.StatementSyntax)
                    For i As Integer = 0 To node.Body.Statements.Count - 1
                        Dim s As CSS.StatementSyntax = node.Body.Statements(i)
                        Statement.AddRange(s.Accept(New MethodBodyVisitor(mSemanticModel, Me)))
                    Next i
                    CS_CloseBraceToken = node.Body.CloseBraceToken
                    EndSubStatement = VBFactory.EndSubStatement().WithConvertedTriviaFrom(CS_CloseBraceToken)
                    If node.Body.OpenBraceToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        Dim Trivia As IEnumerable(Of SyntaxTrivia) = ConvertTrivia(node.Body.OpenBraceToken.LeadingTrivia)
                        If Statement.Count > 0 Then
                            Statement(0) = Statement(0).WithPrependedLeadingTrivia(Trivia)
                        Else
                            EndSubStatement = EndSubStatement.WithPrependedLeadingTrivia(Trivia)
                        End If
                    End If
                    Body = VBFactory.List(Statement)
                ElseIf node.ExpressionBody IsNot Nothing Then
                    Dim VBSyntaxNode As VB.VisualBasicSyntaxNode = node.ExpressionBody.Accept(Me)
                    If TypeOf VBSyntaxNode Is VBS.ThrowStatementSyntax Then
                        Body = VBFactory.SingletonList(Of VBS.StatementSyntax)(DirectCast(VBSyntaxNode, VBS.ThrowStatementSyntax))
                    ElseIf TypeOf VBSyntaxNode Is VBS.AssignmentStatementSyntax Then
                        Body = VBFactory.SingletonList(Of VBS.StatementSyntax)(DirectCast(VBSyntaxNode, VBS.AssignmentStatementSyntax))
                    ElseIf TypeOf VBSyntaxNode Is VBS.InvocationExpressionSyntax Then
                        Dim InvocationExpression As VBS.InvocationExpressionSyntax = DirectCast(VBSyntaxNode, VBS.InvocationExpressionSyntax)
                        Body = VBFactory.SingletonList(Of VBS.StatementSyntax)(VBFactory.ExpressionStatement(InvocationExpression))
                    Else
                        Stop
                        Throw UnreachableException
                    End If
                End If
                If Initializer IsNot Nothing Then
                    Body = Body.InsertRange(0, ReplaceStatementsWithMarkedStatements(node,
                                            VBFactory.SingletonList(Of VBS.StatementSyntax)(Initializer)))
                Else
                    Body = ReplaceStatementsWithMarkedStatements(node, Body)

                End If
                Return VBFactory.ConstructorBlock(SubNewStatement, Body, EndSubStatement).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitConstructorInitializer(node As CSS.ConstructorInitializerSyntax) As VB.VisualBasicSyntaxNode
                Dim ArgumentList As VBS.ArgumentListSyntax = DirectCast((node.ArgumentList.Accept(Me)), VBS.ArgumentListSyntax)
                Dim SimpleMemberAccessExpression As VBS.MemberAccessExpressionSyntax
                Dim MeOrMyExpression As VBS.ExpressionSyntax = If(TypeOf node.Parent.Parent Is CSS.StructDeclarationSyntax,
                                                            DirectCast(VBFactory.MeExpression(), VBS.ExpressionSyntax),
                                                            VBFactory.MyBaseExpression()).WithConvertedLeadingTriviaFrom(node.ColonToken)

                SimpleMemberAccessExpression = VBFactory.SimpleMemberAccessExpression(MeOrMyExpression, VBFactory.IdentifierName("New"))
                Dim InvocationExpression As VBS.InvocationExpressionSyntax = VBFactory.InvocationExpression(SimpleMemberAccessExpression, ArgumentList)
                Return VBFactory.ExpressionStatement(InvocationExpression).
                                             RestructureArguments(node.ArgumentList).WithConvertedTrailingTriviaFrom(node)
            End Function

            ''' <summary>
            ''' Creates a new object initialized to a meaningful value.
            ''' </summary>
            ''' <param name="value"></param>
            'INSTANT VB TODO TASK: Generic operators are not available in VB:
            'ORIGINAL LINE: public static implicit operator @Optional<T>(T value)
            'Public Shared Widening Operator DirectCast(value As T) As [Optional]
            '    Return New [Optional](Of T)(value)
            'End Operator
            Public Overrides Function VisitConversionOperatorDeclaration(node As CSS.ConversionOperatorDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim AttributeLists As New List(Of VBS.AttributeListSyntax)
                Dim ReturnAttributes As SyntaxList(Of VBS.AttributeListSyntax) = Nothing
                ConvertAndSplitAttributes(node.AttributeLists, AttributeLists, ReturnAttributes)
                Dim parameterList As VBS.ParameterListSyntax = DirectCast(node.ParameterList?.Accept(Me), VBS.ParameterListSyntax).
                                                                    WithRestructuredingEOLTrivia
                Dim Modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, IsModule, TokenContext.Member)
                Dim visitor As New MethodBodyVisitor(mSemanticModel, Me)
                Modifiers.Add(If(node.ImplicitOrExplicitKeyword.ValueText = "explicit", NarrowingKeyword, WideningKeyword))
                Dim Type As VBS.TypeSyntax = DirectCast(node.Type.Accept(Me), VBS.TypeSyntax).With({SpaceTrivia}, {SpaceTrivia})
                Dim AsClause As VBS.SimpleAsClauseSyntax = VBFactory.SimpleAsClause(Nothing, Type)
                Dim OperatorStatement As VBS.OperatorStatementSyntax = VBFactory.OperatorStatement(VBFactory.List(AttributeLists), VBFactory.TokenList(Modifiers), CTypeKeyword, parameterList, AsClause).WithTrailingEOL
                Dim Statements As New SyntaxList(Of VBS.StatementSyntax)

                If node.Body IsNot Nothing Then
                    Statements = VBFactory.List(node.Body.Statements.SelectMany(Function(s As CSS.StatementSyntax) s.Accept(visitor)))
                ElseIf node.ExpressionBody IsNot Nothing Then
                    Statements = VBFactory.SingletonList(Of VBS.StatementSyntax)(VBFactory.ReturnStatement(DirectCast(node.ExpressionBody.Accept(Me), VBS.ExpressionSyntax)).WithTrailingEOL)
                Else
                    Stop
                End If
                Dim EndOperatorStatement As VBS.EndBlockStatementSyntax = VBFactory.EndBlockStatement(VB.SyntaxKind.EndOperatorStatement, EndKeyword, BlockKeyword).WithConvertedTriviaFrom(node.Body.GetBraces.Item2)
                Dim OperatorBlock As VBS.OperatorBlockSyntax = VBFactory.OperatorBlock(OperatorStatement, Statements, EndOperatorStatement).WithConvertedTriviaFrom(node)
                Return PrependStatementWithMarkedStatementTrivia(node, OperatorBlock)
            End Function

            Public Overrides Function VisitDestructorDeclaration(node As CSS.DestructorDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim AttributeLists As SyntaxList(Of VBS.AttributeListSyntax) = VBFactory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))
                Dim Modifiers As SyntaxTokenList = VBFactory.TokenList(ProtectedKeyword, OverridesKeyword)
                Dim Identifier As SyntaxToken = VBFactory.Identifier(NameOf(Finalize))
                Dim ParameterList As VBS.ParameterListSyntax = DirectCast(node.ParameterList?.Accept(Me), VBS.ParameterListSyntax)
                Dim Statements As New List(Of VBS.StatementSyntax)
                If node.Body IsNot Nothing Then
                    For Each S As CSS.StatementSyntax In node.Body.Statements
                        Statements.AddRange(S.Accept(New MethodBodyVisitor(mSemanticModel, Me)))
                    Next
                Else
                    Dim ExpressionBody As VB.VisualBasicSyntaxNode = node.ExpressionBody.Accept(Me)

                    If TypeOf ExpressionBody Is VBS.InvocationExpressionSyntax Then
                        Dim InvocationExpression As VBS.InvocationExpressionSyntax = DirectCast(ExpressionBody, VBS.InvocationExpressionSyntax)
                        Statements.Add(VBFactory.ExpressionStatement(InvocationExpression))
                    ElseIf TypeOf ExpressionBody Is VBS.AssignmentStatementSyntax Then
                        Dim Assignment As VBS.AssignmentStatementSyntax = DirectCast(ExpressionBody, VBS.AssignmentStatementSyntax)
                        Statements.Add(VBFactory.AssignmentStatement(VB.SyntaxKind.SimpleAssignmentStatement, Assignment.Left, EqualsToken, Assignment.Right))
                    ElseIf TypeOf ExpressionBody Is VBS.ConditionalAccessExpressionSyntax Then
                        Statements.Add(VBFactory.ExpressionStatement(DirectCast(ExpressionBody, VBS.ExpressionSyntax)))
                    Else
                        Stop
                    End If
                End If
                Return VBFactory.SubBlock(subOrFunctionStatement:=VBFactory.SubStatement(
                                              AttributeLists,
                                              Modifiers,
                                              Identifier,
                                              typeParameterList:=Nothing,
                                              ParameterList,
                                              asClause:=Nothing,
                                              handlesClause:=Nothing,
                                              implementsClause:=Nothing),
                                              VBFactory.List(Statements)).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitEventDeclaration(node As CSS.EventDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim Attributes As New List(Of VBS.AttributeListSyntax)
                Dim ReturnAttributes As SyntaxList(Of VBS.AttributeListSyntax) = Nothing
                ConvertAndSplitAttributes(node.AttributeLists, Attributes, ReturnAttributes)
                Dim Modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, IsModule, TokenContext.Member)
                Dim Identifier As SyntaxToken = GenerateSafeVBToken(node.Identifier, IsQualifiedName:=False, IsTypeName:=False).WithTrailingTrivia(SpaceTrivia)
                Dim AsClause As VBS.SimpleAsClauseSyntax = VBFactory.SimpleAsClause(attributeLists:=ReturnAttributes, DirectCast(node.Type.Accept(Me), VBS.TypeSyntax))
                Modifiers.Add(CustomKeyword)
                Dim stmt As VBS.EventStatementSyntax = VBFactory.EventStatement(attributeLists:=VBFactory.List(Attributes), VBFactory.TokenList(Modifiers), Identifier, parameterList:=Nothing, AsClause, implementsClause:=Nothing).WithTrailingEOL
                Dim EmptyBody As Boolean = True
                For i As Integer = 0 To node.AccessorList.Accessors.Count - 1
                    Dim a As CSS.AccessorDeclarationSyntax = node.AccessorList.Accessors(i)
                    If a.Body IsNot Nothing Then
                        EmptyBody = False
                        Exit For
                    End If
                    If a.ExpressionBody IsNot Nothing Then
                        Exit For
                    End If
                    Return stmt.WithConvertedTriviaFrom(node)
                Next
                Dim accessors As VBS.AccessorBlockSyntax()
                accessors = node.AccessorList?.Accessors.Select(Function(a As CSS.AccessorDeclarationSyntax) ConvertAccessor(a, IsModule:=IsModule, isIterator:=False)).ToArray()
                Return VBFactory.EventBlock(stmt, VBFactory.List(accessors)).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitEventFieldDeclaration(node As CSS.EventFieldDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim id As SyntaxToken = VBFactory.Identifier(AddBracketsIfRequired(node.Declaration.Variables.Single().Identifier.ValueText))
                Dim ReturnAttributes As New SyntaxList(Of VBS.AttributeListSyntax)
                Dim AttributeList As New List(Of VBS.AttributeListSyntax)
                ConvertAndSplitAttributes(node.AttributeLists, AttributeList, ReturnAttributes)
                Return VBFactory.EventStatement(VBFactory.List(AttributeList),
                                     VBFactory.TokenList(ConvertModifiers(node.Modifiers, IsModule, TokenContext.Member)),
                                                    id,
                                                    parameterList:=Nothing,
                                                    VBFactory.SimpleAsClause(attributeLists:=Nothing, DirectCast(node.Declaration.Type.Accept(Me), VBS.TypeSyntax)),
                                                    implementsClause:=Nothing).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitFieldDeclaration(node As CSS.FieldDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim _TypeInfo As TypeInfo = ModelExtensions.GetTypeInfo(mSemanticModel, node.Declaration.Type)
                Dim variableOrConstOrReadonly As TokenContext = TokenContext.VariableOrConst
                If _TypeInfo.ConvertedType IsNot Nothing AndAlso _TypeInfo.ConvertedType.TypeKind = TypeKind.Class Then
                    For i As Integer = 0 To node.Declaration.Variables.Count - 1
                        Dim v As CSS.VariableDeclaratorSyntax = node.Declaration.Variables(i)
                        If v.Initializer IsNot Nothing AndAlso v.Initializer.Value.IsKind(CS.SyntaxKind.NullLiteralExpression) Then
                            variableOrConstOrReadonly = TokenContext.Readonly
                        End If
                    Next
                End If
                Dim modifierList As New List(Of SyntaxToken)
                modifierList.AddRange(ConvertModifiers(node.Modifiers, IsModule, variableOrConstOrReadonly))
                If modifierList.Count = 0 Then
                    modifierList.Add(PrivateKeyword.WithLeadingTrivia(ConvertTrivia(node.Declaration.Type.GetLeadingTrivia)))
                End If
                Dim LeadingTrivia As New List(Of SyntaxTrivia)
                Dim Attributes As New SyntaxList(Of VBS.AttributeListSyntax)
                If node.Modifiers.Contains(Function(t As SyntaxToken) t.IsKind(CS.SyntaxKind.VolatileKeyword)) Then
                    Dim Name As VBS.TypeSyntax = VBFactory.ParseTypeName("Volatile")
                    Dim VolatileAttribute As SeparatedSyntaxList(Of VBS.AttributeSyntax) = VBFactory.SingletonSeparatedList(VBFactory.Attribute(Name))
                    LeadingTrivia.Add(VBFactory.CommentTrivia("' TODO TASK: VB has no direct equivalent to C# Volatile Modifier, an Attribute was substituted."))
                    Attributes = Attributes.Add(VBFactory.AttributeList(VolatileAttribute).WithLeadingTrivia(LeadingTrivia))
                    LeadingTrivia.Clear()
                End If
                If node.AttributeLists.Count > 0 Then
                    LeadingTrivia.AddRange(ConvertTrivia(node.AttributeLists(0).GetLeadingTrivia))
                Else
                    If node.Modifiers.Count > 0 Then
                        LeadingTrivia.AddRange(ConvertTrivia(node.Modifiers(0).LeadingTrivia))
                    ElseIf modifierList.Count > 0 Then
                        LeadingTrivia.AddRange(modifierList(0).LeadingTrivia)
                    End If
                    If modifierList(0).HasLeadingTrivia Then
                        modifierList(0) = modifierList(0).WithLeadingTrivia(modifierList(0).LeadingTrivia.Last)
                    End If
                End If

                For Each a As CSS.AttributeListSyntax In node.AttributeLists
                    Attributes = Attributes.Add(DirectCast(a.Accept(Me), VBS.AttributeListSyntax))
                Next
                If Attributes.Count > 0 Then
                    Attributes = Attributes.Replace(Attributes(0), Attributes(0).WithLeadingTrivia(LeadingTrivia))
                Else
                    modifierList(0) = modifierList(0).WithLeadingTrivia(LeadingTrivia)
                End If
                Dim declarators As SeparatedSyntaxList(Of VBS.VariableDeclaratorSyntax) = RemodelVariableDeclaration(node.Declaration, Me, mSemanticModel, IsFieldDeclaration:=True, LeadingTrivia)
                Dim FieldDeclaration As VBS.FieldDeclarationSyntax
                Dim modifiers As SyntaxTokenList = VBFactory.TokenList(modifierList)
                FieldDeclaration = VBFactory.FieldDeclaration(Attributes, modifiers, declarators).WithLeadingTrivia(LeadingTrivia)
                FieldDeclaration = AddSpecialCommentToField(node, FieldDeclaration)
                Return FieldDeclaration.RestructureAttributesAndModifiers(Attributes.Count > 0, modifiers.Count > 0).
                    WithMergedTrailingTrivia(GetTriviaFromUnneededToken(node.SemicolonToken)).WithTrailingEOL
            End Function

            Public Overrides Function VisitIndexerDeclaration(node As CSS.IndexerDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim id As SyntaxToken = VBFactory.Identifier("Item")
                Dim Attributes As New List(Of VBS.AttributeListSyntax)
                Dim ReturnAttributes As SyntaxList(Of VBS.AttributeListSyntax) = Nothing
                ConvertAndSplitAttributes(node.AttributeLists, Attributes, ReturnAttributes)
                Dim isIterator As Boolean = False
                Dim accessors As New List(Of VBS.AccessorBlockSyntax)()
                If node.AccessorList IsNot Nothing Then
                    For Each a As CSS.AccessorDeclarationSyntax In node.AccessorList.Accessors
                        Dim _isIterator As Boolean
                        accessors.Add(ConvertAccessor(a, IsModule, _isIterator))
                        isIterator = isIterator Or _isIterator
                    Next
                End If

                Dim Modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, IsModule, TokenContext.Member)
                If Modifiers.Count > 0 Then
                    Modifiers.Insert(0, DefaultKeyword.WithLeadingTrivia(Modifiers(0).LeadingTrivia))
                    Modifiers(1) = Modifiers(1).WithLeadingTrivia(SpaceTrivia)
                End If
                Select Case accessors.Count
                    Case 0
                        Dim AccessorStatement As VBS.AccessorStatementSyntax = VBFactory.GetAccessorStatement()
                        Dim VBSyntaxNode As VB.VisualBasicSyntaxNode = node.ExpressionBody.Accept(Me)
                        Dim Body As SyntaxList(Of VBS.StatementSyntax)
                        If TypeOf VBSyntaxNode Is VBS.ExpressionSyntax Then
                            Body = VBFactory.SingletonList(Of VBS.StatementSyntax)(VBFactory.ReturnStatement(DirectCast(VBSyntaxNode, VBS.ExpressionSyntax)).WithTrailingEOL)
                        ElseIf TypeOf VBSyntaxNode Is VBS.ThrowStatementSyntax Then
                            Dim Statement As VBS.ThrowStatementSyntax = DirectCast(VBSyntaxNode, VBS.ThrowStatementSyntax)
                            Body = VBFactory.SingletonList(Of VBS.StatementSyntax)(Statement)
                        Else
                            Stop
                            Throw UnreachableException
                        End If
                        Dim EndStmt As VBS.EndBlockStatementSyntax = VBFactory.EndGetStatement()
                        accessors.Add(VBFactory.AccessorBlock(VB.SyntaxKind.GetAccessorBlock, AccessorStatement, Body, EndStmt))
                    Case 1
                        Dim NeedKeyword As Boolean = True
                        If accessors(0).AccessorStatement.Kind() = VB.SyntaxKind.GetAccessorStatement Then
                            For Each keyword As SyntaxToken In Modifiers
                                If keyword.ValueText = "ReadOnly" Then
                                    NeedKeyword = False
                                    Exit For
                                End If
                            Next
                            If NeedKeyword Then
                                Modifiers.Add(VisualBasicSyntaxFactory.ReadOnlyKeyword)
                            End If
                        Else
                            For Each keyword As SyntaxToken In Modifiers
                                If keyword.ValueText = "WriteOnly" Then
                                    NeedKeyword = False
                                    Exit For
                                End If
                            Next
                            If NeedKeyword Then
                                Modifiers.Add(WriteOnlyKeyword)
                            End If
                        End If
                    Case 2
                        ' Ignore
                    Case Else
                        Throw UnreachableException
                End Select
                If isIterator Then
                    Modifiers.Add(IteratorKeyword)
                End If
                Dim parameterList As VBS.ParameterListSyntax = DirectCast(node.ParameterList?.Accept(Me), VBS.ParameterListSyntax).WithRestructuredingEOLTrivia
                Dim NodeType As VBS.TypeSyntax = DirectCast(node.Type.Accept(Me), VBS.TypeSyntax)
                Dim AsClause As VBS.SimpleAsClauseSyntax = VBFactory.SimpleAsClause(ReturnAttributes, NodeType.WithLeadingTrivia(SpaceTrivia))
                Dim stmt As VBS.PropertyStatementSyntax = VBFactory.PropertyStatement(VBFactory.List(Attributes), VBFactory.TokenList(Modifiers), id, parameterList, AsClause, initializer:=Nothing, implementsClause:=Nothing).WithTrailingEOL
                Dim accessorList As CSS.AccessorListSyntax = node.AccessorList
                Dim EmptyAccessorListBodies As Boolean = True
                If accessorList IsNot Nothing Then
                    For Each a As CSS.AccessorDeclarationSyntax In accessorList.Accessors
                        If a.Body IsNot Nothing OrElse a.ExpressionBody IsNot Nothing Then
                            EmptyAccessorListBodies = False
                            Exit For
                        End If
                    Next
                End If

                If EmptyAccessorListBodies AndAlso node.ExpressionBody Is Nothing Then
                    Return stmt
                End If
                Dim EndPropertyStatement As VBS.EndBlockStatementSyntax = VBFactory.EndPropertyStatement().WithConvertedTriviaFrom(node.AccessorList.GetBraces.Item2)
                Return VBFactory.PropertyBlock(stmt, VBFactory.List(accessors), EndPropertyStatement).WithConvertedLeadingTriviaFrom(node.Type)
            End Function

            Public Overrides Function VisitMethodDeclaration(node As CSS.MethodDeclarationSyntax) As VB.VisualBasicSyntaxNode
                If node.Modifiers.Any(Function(m As SyntaxToken) m.IsKind(CS.SyntaxKind.UnsafeKeyword)) Then
                    Return FlagUnsupportedStatements(node, "unsafe Functions", CommentOutOriginalStatements:=True)
                End If

                If node.ReturnType IsNot Nothing AndAlso TypeOf node.ReturnType Is CSS.RefTypeSyntax Then
                    Return FlagUnsupportedStatements(node, "ref return Functions", CommentOutOriginalStatements:=True)
                End If
                SyncLock UsedStacks
                    UsedStacks.Push(UsedIdentifiers)
                End SyncLock

                Dim id As SyntaxToken = GenerateSafeVBToken(node.Identifier, IsQualifiedName:=False, IsTypeName:=False)
                Dim visitor As New MethodBodyVisitor(mSemanticModel, Me)

                Dim methodInfo As ISymbol = ModelExtensions.GetDeclaredSymbol(mSemanticModel, node)
                Dim _methodTypeSymbol As ISymbol = ModelExtensions.GetDeclaredSymbol(mSemanticModel, node)
                Dim ReturnVoid As Boolean? = methodInfo?.GetReturnType()?.SpecialType = SpecialType.System_Void
                Dim containingType As INamedTypeSymbol = methodInfo?.ContainingType
                Dim Attributes As New List(Of VBS.AttributeListSyntax)
                Dim ReturnAttributes As SyntaxList(Of VBS.AttributeListSyntax) = Nothing
                ConvertAndSplitAttributes(node.AttributeLists, Attributes, ReturnAttributes)
                Dim ParameterList As VBS.ParameterListSyntax = DirectCast(node.ParameterList?.Accept(Me), VBS.ParameterListSyntax)

                Dim FunctionStatementTrailingTrivia As New List(Of SyntaxTrivia)
                ParameterList = RelocateDirectivesInTrailingTrivia(ParameterList, FunctionStatementTrailingTrivia)
                Dim block As SyntaxList(Of VBS.StatementSyntax)? = Nothing

                If node.Body IsNot Nothing Then
                    Dim StatementNodes As New List(Of VBS.StatementSyntax)
                    For i As Integer = 0 To node.Body.Statements.Count - 1
                        Dim s As CSS.StatementSyntax = node.Body.Statements(i)
                        Dim StatementCollection As List(Of VBS.StatementSyntax) = s.Accept(visitor).ToList
                        If i = 0 Then
                            If node.Body.OpenBraceToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                StatementCollection(0) = StatementCollection(0).WithPrependedLeadingTrivia(ConvertTrivia(node.Body.OpenBraceToken.LeadingTrivia))
                            End If
                        End If
                        StatementNodes.AddRange(StatementCollection)
                    Next
                    block = VBFactory.List(StatementNodes)
                ElseIf node.ExpressionBody IsNot Nothing Then
                    Dim Expression1 As VB.VisualBasicSyntaxNode = node.ExpressionBody.Expression.Accept(Me)
                    If Expression1 Is Nothing Then
                        Return PrependStatementWithMarkedStatementTrivia(node, VBFactory.EmptyStatement.WithConvertedTriviaFrom(node))
                    End If
                    Dim Statements As New SyntaxList(Of VBS.StatementSyntax)
                    If ReturnVoid Then
                        Select Case Expression1.Kind
                            Case VB.SyntaxKind.SimpleAssignmentStatement
                                Statements = VBFactory.SingletonList(DirectCast(Expression1, VBS.StatementSyntax))
                            Case VB.SyntaxKind.InvocationExpression
                                Dim LeadingTrivia As New List(Of SyntaxTrivia)
                                LeadingTrivia.AddRange(Expression1.GetLeadingTrivia)
                                Expression1 = Expression1.WithLeadingTrivia(SpaceTrivia)
                                Statements = VBFactory.SingletonList(Of VBS.StatementSyntax)(VBFactory.CallStatement(DirectCast(Expression1, VBS.ExpressionSyntax)).WithLeadingTrivia(LeadingTrivia))
                            Case VB.SyntaxKind.ConditionalAccessExpression
                                Statements = VBFactory.SingletonList(Of VBS.StatementSyntax)(VBFactory.ExpressionStatement(DirectCast(Expression1, VBS.ExpressionSyntax)))
                            Case VB.SyntaxKind.ThrowStatement
                                Statements = VBFactory.SingletonList(DirectCast(Expression1, VBS.StatementSyntax))
                            Case VB.SyntaxKind.TryBlock
                                For Each Statement As VBS.StatementSyntax In CType(Expression1, VBS.TryBlockSyntax).Statements
                                    Statements = Statements.Add(Statement)
                                Next
                            Case Else
                                Statements = Nothing
                                Stop
                        End Select
                    Else
                        If TypeOf Expression1 Is VBS.ThrowStatementSyntax Then
                            Statements = VBFactory.SingletonList(Of VBS.StatementSyntax)(DirectCast(Expression1, VBS.ThrowStatementSyntax))
                        Else
                            Dim ReturnExpression As VBS.ExpressionSyntax
                            Dim StatementList As New List(Of VBS.StatementSyntax)
                            If TypeOf Expression1 Is VBS.AssignmentStatementSyntax Then
                                Dim Assignment As VBS.AssignmentStatementSyntax = DirectCast(Expression1, VBS.AssignmentStatementSyntax)
                                StatementList.Add(Assignment.WithoutTrivia)
                                ReturnExpression = Assignment.Left.WithTriviaFrom(Expression1)
                            Else
                                ReturnExpression = DirectCast(Expression1, VBS.ExpressionSyntax)
                            End If
                            Dim StatementLeadingTrivia As New List(Of SyntaxTrivia)
                            Dim StatementTrailingTrivia As New List(Of SyntaxTrivia)
                            If node.ExpressionBody.Expression.IsKind(CS.SyntaxKind.DefaultLiteralExpression) Then
                                StatementLeadingTrivia.InsertRange(0, CheckCorrectnessLeadingTrivia(DirectCast(Nothing, CS.CSharpSyntaxNode), "VB Does not support ""Default"""))
                                Dim ReturnStatement As VBS.ReturnStatementSyntax = VBFactory.ReturnStatement(ReturnExpression.WithLeadingTrivia(SpaceTrivia))
                                Dim ReturnStartementWithTrivia As VBS.ReturnStatementSyntax = ReturnStatement.
                                                With(StatementLeadingTrivia, ConvertTrivia(node.ExpressionBody.Expression.GetTrailingTrivia)).
                                                WithAppendedTrailingTrivia(ConvertTrivia(node.SemicolonToken.TrailingTrivia)).
                                                WithTrailingEOL
                                StatementList.Add(ReturnStartementWithTrivia)
                            Else
                                StatementList.Add(VBFactory.ReturnStatement(ReturnExpression).
                                                      With(StatementLeadingTrivia, ConvertTrivia(node.ExpressionBody.Expression.GetTrailingTrivia)).
                                                      WithAppendedTrailingTrivia(ConvertTrivia(node.SemicolonToken.TrailingTrivia)).
                                                      WithTrailingEOL
                                                  )
                            End If
                            Statements = VBFactory.List(StatementList)
                        End If
                    End If
                    block = ReplaceStatementsWithMarkedStatements(node, Statements)
                End If
                If node.Modifiers.Any(Function(m As SyntaxToken) m.IsKind(CS.SyntaxKind.ExternKeyword)) Then
                    block = VBFactory.List(Of VBS.StatementSyntax)()
                End If
                Dim Modifiers As List(Of SyntaxToken)
                If IsModule AndAlso
                    id.ValueText = "Main" AndAlso
                    node.Modifiers.Count = 1 AndAlso
                    node.Modifiers(0).ValueText = "static" Then
                    Modifiers = PublicModifier.ToList
                Else
                    Modifiers = ConvertModifiers(node.Modifiers, IsModule, If(containingType?.IsInterfaceType() = True, TokenContext.Local, TokenContext.Member))
                End If
                If visitor.IsInterator Then
                    Modifiers.Add(IteratorKeyword)
                End If
                If node.ParameterList.Parameters.Count > 0 AndAlso node.ParameterList.Parameters(0).Modifiers.Any(CS.SyntaxKind.ThisKeyword) Then
                    Dim LeadingTrivia As New List(Of SyntaxTrivia)
                    If Attributes.Count > 0 AndAlso Attributes(0).HasLeadingTrivia Then
                        LeadingTrivia.AddRange(Attributes(0).GetLeadingTrivia)
                        Attributes(0) = Attributes(0).WithLeadingTrivia(LeadingTrivia.Last)
                    End If
                    If Attributes.Count = 0 AndAlso Modifiers.Count > 0 Then
                        LeadingTrivia.AddRange(Modifiers(0).LeadingTrivia)
                        Modifiers(0) = Modifiers(0).WithLeadingTrivia(SpaceTrivia)
                    End If
                    Attributes.Insert(0, VBFactory.AttributeList(VBFactory.SingletonSeparatedList(ExtensionAttribute)).WithLeadingTrivia(LeadingTrivia))
                    LeadingTrivia.Clear()
                    If Not DirectCast(node.SyntaxTree, CS.CSharpSyntaxTree).HasUsingDirective(CompilerServices) Then
                        Dim ImportComilierServices As VBS.ImportsStatementSyntax = VBFactory.ImportsStatement(VBFactory.SingletonSeparatedList(Of VBS.ImportsClauseSyntax)(VBFactory.SimpleImportsClause(VBFactory.ParseName(CompilerServices)))).WithAppendedTrailingTrivia(VB_EOLTrivia)
                        If Not allImports.ContainsName(CompilerServices) Then
                            allImports.Add(ImportComilierServices)
                        End If
                    End If
                End If
                If containingType?.IsStatic = True Then
                    Dim TokenList As New List(Of SyntaxToken)
                    Dim NewLeadingTrivia As New List(Of SyntaxTrivia)
                    For i As Integer = 0 To Modifiers.Count - 1
                        Dim t As SyntaxToken = Modifiers(i)
                        If t.IsKind(VB.SyntaxKind.SharedKeyword) Then
                            If t.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                NewLeadingTrivia.AddRange(t.LeadingTrivia)
                            End If
                        Else
                            TokenList.Add(t)
                        End If
                    Next
                    If TokenList.Count = 0 Then
                        Modifiers.Clear()
                        Modifiers.Add(VBFactory.Token(VB.SyntaxKind.EmptyToken).WithLeadingTrivia(NewLeadingTrivia))
                    Else
                        If Not TriviaIsIdentical(Modifiers(0).LeadingTrivia, NewLeadingTrivia) Then
                            NewLeadingTrivia.InsertRange(0, Modifiers(0).LeadingTrivia)
                            Modifiers(0) = Modifiers(0).WithLeadingTrivia(NewLeadingTrivia)
                        End If
                    End If
                End If

                Dim FunctionStatementLeadingTrivia As New List(Of SyntaxTrivia)
                If Attributes.Count > 0 Then
                    FunctionStatementLeadingTrivia.AddRange(Attributes(0).GetLeadingTrivia)
                    Attributes(0) = Attributes(0).WithLeadingTrivia(SpaceTrivia)
                End If
                If node.ReturnType IsNot Nothing Then
                    If Modifiers.Count = 0 Then
                        FunctionStatementLeadingTrivia.AddRange(ConvertTrivia(node.ReturnType.GetLeadingTrivia))
                    Else
                        Dim NewModifierLeadingTrivia As New List(Of SyntaxTrivia)
                        Dim CS_NodeLeadingTrivia As SyntaxTriviaList = node.ReturnType.GetLeadingTrivia
                        If CS_NodeLeadingTrivia.Count > 0 Then
                            NewModifierLeadingTrivia.AddRange(Modifiers(0).LeadingTrivia)
                            NewModifierLeadingTrivia.AddRange(ConvertTrivia(CS_NodeLeadingTrivia))
                            If Not NewModifierLeadingTrivia.FirstOrDefault.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                NewModifierLeadingTrivia.Insert(0, VB_EOLTrivia)
                            End If
                            Modifiers(0) = Modifiers(0).WithLeadingTrivia(NewModifierLeadingTrivia)
                        End If
                        If Attributes.Count = 0 AndAlso Modifiers(0).LeadingTrivia.Count > 0 Then
                            FunctionStatementLeadingTrivia.AddRange(Modifiers(0).LeadingTrivia)
                            Modifiers(0) = Modifiers(0).WithLeadingTrivia(Modifiers(0).LeadingTrivia.Last)
                        End If
                    End If
                End If
                If node.Body Is Nothing Then
                    FunctionStatementTrailingTrivia.AddRange(ConvertTrivia(node.GetTrailingTrivia))
                Else
                    If node.Body.OpenBraceToken.HasTrailingTrivia Then
                        FunctionStatementTrailingTrivia.AddRange(ConvertTrivia(node.Body.OpenBraceToken.TrailingTrivia))
                    Else
                        FunctionStatementTrailingTrivia.AddRange(ConvertTrivia(node.GetTrailingTrivia))
                    End If
                End If

                Dim SubOrFunctionStatement As VBS.MethodStatementSyntax
                Dim EndSubOrFunctionStatement As VBS.EndBlockStatementSyntax
                Dim TypeParameterList As VBS.TypeParameterListSyntax = DirectCast(node.TypeParameterList?.Accept(Me), VBS.TypeParameterListSyntax)
                If TypeParameterList IsNot Nothing Then
                    TypeParameterList = TypeParameterList.WithTrailingTrivia(SpaceTrivia)
                End If
                Dim ImplementsClause As VBS.ImplementsClauseSyntax = ImplementedMembers.GetImplementsClauseForMethod(CType(_methodTypeSymbol, IMethodSymbol))
                If id.ToString = "Dispose" AndAlso TypeOf node.Parent Is CSS.ClassDeclarationSyntax Then
                    Dim ParentClass As CSS.ClassDeclarationSyntax = DirectCast(node.Parent, CSS.ClassDeclarationSyntax)
                    If ParentClass.BaseList IsNot Nothing Then
                        For Each t As CSS.SimpleBaseTypeSyntax In ParentClass.BaseList.Types
                            If TypeOf t.Type Is CSS.IdentifierNameSyntax AndAlso DirectCast(t.Type, CSS.IdentifierNameSyntax).Identifier.ValueText = "IDisposable" Then
                                Dim InterfaceMembers As VBS.QualifiedNameSyntax = VBFactory.QualifiedName(
                                                                                VBFactory.IdentifierName("IDisposable"),
                                                                                VBFactory.IdentifierName("Dispose")
                                                                                )
                                If ParameterList Is Nothing OrElse ParameterList.Parameters.Count > 0 Then
                                    Exit For
                                End If
                                ImplementsClause = VBFactory.ImplementsClause(InterfaceMembers).WithTrailingTrivia(ParameterList.GetTrailingTrivia)
                                ParameterList = ParameterList.WithTrailingTrivia(SpaceTrivia)
                            End If
                        Next
                    End If
                End If
                If ReturnVoid Then
                    EndSubOrFunctionStatement = VBFactory.EndSubStatement
                    If node.Body IsNot Nothing Then
                        EndSubOrFunctionStatement = EndSubOrFunctionStatement.WithConvertedTriviaFrom(node.Body.CloseBraceToken)
                    ElseIf node.ExpressionBody IsNot Nothing Then
                        EndSubOrFunctionStatement = EndSubOrFunctionStatement.WithConvertedTriviaFrom(node.ExpressionBody.GetBraces.Item2)
                    End If

                    If TypeParameterList IsNot Nothing OrElse ParameterList IsNot Nothing Then
                        id = id.WithTrailingTrivia(SpaceTrivia)
                    End If
                    Dim MethodStatement As VBS.MethodStatementSyntax = VBFactory.SubStatement(
                                                                    VBFactory.List(Attributes),
                                                                    VBFactory.TokenList(Modifiers),
                                                                    id,
                                                                    TypeParameterList,
                                                                    ParameterList,
                                                                    asClause:=Nothing,
                                                                    handlesClause:=Nothing,
                                                                    ImplementsClause)
                    SubOrFunctionStatement = DirectCast(MethodStatement.
                        With(FunctionStatementLeadingTrivia, FunctionStatementTrailingTrivia).
                        WithTrailingEOL.
                        RestructureAttributesAndModifiers(Attributes.Count > 0, Modifiers.Count > 0), VBS.MethodStatementSyntax)
                    SyncLock UsedStacks
                        If UsedStacks.Count > 0 Then
                            UsedIdentifiers = DirectCast(UsedStacks.Pop, Dictionary(Of String, SymbolTableEntry))
                        End If
                    End SyncLock
                    SubOrFunctionStatement = DirectCast(PrependStatementWithMarkedStatementTrivia(node, SubOrFunctionStatement), VBS.MethodStatementSyntax)
                    If block Is Nothing Then
                        If Modifiers.Contains(Function(t As SyntaxToken) t.IsKind(VB.SyntaxKind.PartialKeyword)) Then
                            Return VBFactory.SubBlock(SubOrFunctionStatement,
                                                      statements:=Nothing,
                                                      EndSubOrFunctionStatement)
                        End If
                        Return SubOrFunctionStatement
                    End If
                    Return VBFactory.SubBlock(SubOrFunctionStatement,
                                              block.Value,
                                              EndSubOrFunctionStatement)
                End If
                EndSubOrFunctionStatement = VBFactory.EndFunctionStatement()
                If node.Body IsNot Nothing Then
                    EndSubOrFunctionStatement = EndSubOrFunctionStatement.WithConvertedTriviaFrom(node.Body.CloseBraceToken)
                End If
                Dim type As VBS.TypeSyntax = DirectCast(node.ReturnType.Accept(Me), VBS.TypeSyntax).WithLeadingTrivia(SpaceTrivia)

                If type Is Nothing Then
                    ' Handle ref return
                    type = VBFactory.ParseTypeName("HandleRef")
                Else
                    If type.ToString = "[Delegate]" Then
                        type = VBFactory.ParseTypeName("System.Delegate")
                    ElseIf type.ToString = "[Enum]" Then
                        type = VBFactory.ParseTypeName("System.Enum")
                    ElseIf type.ToString.StartsWith("[") Then
                        Dim S As String() = type.ToString.Split({"["c, "]"}, StringSplitOptions.RemoveEmptyEntries)
                        If Not (IsSpecialReservedWord(S(0)) OrElse
                                VB.SyntaxFacts.IsKeywordKind(VB.SyntaxFacts.GetKeywordKind(S(0)))) Then
                            type = VBFactory.ParseTypeName(type.ToString.Replace("]", "").Replace("[", ""))
                        End If
                    End If
                End If

                Dim AsClause As VBS.SimpleAsClauseSyntax = VBFactory.SimpleAsClause(ReturnAttributes, type.WithLeadingTrivia(SpaceTrivia))
                Dim ParamListTrailingTrivia As SyntaxTriviaList = ParameterList.GetTrailingTrivia
                ParameterList = ParameterList.WithTrailingTrivia(SpaceTrivia)
                AsClause = AsClause.WithTrailingTrivia(ParamListTrailingTrivia)

                If TypeParameterList IsNot Nothing OrElse ParameterList IsNot Nothing Then
                    id = id.WithTrailingTrivia(SpaceTrivia)
                End If

                SubOrFunctionStatement = DirectCast(VBFactory.FunctionStatement(
                                                            VBFactory.List(Attributes),
                                                            VBFactory.TokenList(Modifiers),
                                                            id,
                                                            TypeParameterList,
                                                            ParameterList,
                                                            AsClause,
                                                            handlesClause:=Nothing,
                                                            ImplementsClause).
                                                            With(FunctionStatementLeadingTrivia, FunctionStatementTrailingTrivia).
                                                                 RestructureAttributesAndModifiers(Attributes.Count > 0, Modifiers.Count > 0), VBS.MethodStatementSyntax)
                SubOrFunctionStatement = DirectCast(PrependStatementWithMarkedStatementTrivia(node, SubOrFunctionStatement), VBS.MethodStatementSyntax)
                UsedIdentifiers = DirectCast(UsedStacks.Pop, Dictionary(Of String, SymbolTableEntry))

                If block Is Nothing Then
                    Return SubOrFunctionStatement
                End If
                Return VBFactory.FunctionBlock(SubOrFunctionStatement,
                                                    block.Value,
                                                    EndSubOrFunctionStatement)
            End Function

            Public Overrides Function VisitOperatorDeclaration(node As CSS.OperatorDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim Attributes As New List(Of VBS.AttributeListSyntax)
                Dim ReturnAttributes As SyntaxList(Of VBS.AttributeListSyntax) = Nothing
                ConvertAndSplitAttributes(node.AttributeLists, Attributes, ReturnAttributes)
                Dim visitor As New MethodBodyVisitor(mSemanticModel, Me)
                Dim body As New SyntaxList(Of VBS.StatementSyntax)
                If node.Body IsNot Nothing Then
                    body = VBFactory.List(node.Body.Statements.SelectMany(Function(s As CSS.StatementSyntax) s.Accept(visitor)))
                ElseIf node.ExpressionBody IsNot Nothing Then
                    Dim VBSyntaxNode As VB.VisualBasicSyntaxNode = node.ExpressionBody.Accept(Me)
                    If TypeOf VBSyntaxNode Is VBS.ExpressionSyntax Then
                        body = VBFactory.SingletonList(Of VBS.StatementSyntax)(VBFactory.ReturnStatement(DirectCast(VBSyntaxNode, VBS.ExpressionSyntax)).WithTrailingEOL)
                    Else
                        body = VBFactory.SingletonList(CType(VBSyntaxNode, VBS.StatementSyntax).WithTrailingEOL)
                    End If
                Else
                    Stop
                End If
                Dim parameterList As VBS.ParameterListSyntax = DirectCast(node.ParameterList?.Accept(Me), VBS.ParameterListSyntax).WithRestructuredingEOLTrivia
                Dim Modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, IsModule, TokenContext.Member)
                Dim lSyntaxKind As CS.SyntaxKind = CS.CSharpExtensions.Kind(node.OperatorToken)
                Select Case lSyntaxKind
                    Case CS.SyntaxKind.MinusMinusToken
                        Return VBFactory.EmptyStatement.WithLeadingTrivia(node.CheckCorrectnessLeadingTrivia("C# -- operator not available in VB")).WithPrependedLeadingTrivia(ConvertTrivia(node.GetLeadingTrivia)).WithConvertedTrailingTriviaFrom(node)
                    Case CS.SyntaxKind.PercentToken
                        Dim stmt As VBS.OperatorStatementSyntax = VBFactory.OperatorStatement(VBFactory.List(Attributes), VBFactory.TokenList(Modifiers), ModKeyword, parameterList, VBFactory.SimpleAsClause(ReturnAttributes, DirectCast(node.ReturnType.Accept(Me), VBS.TypeSyntax)))
                        Return VBFactory.OperatorBlock(stmt, body).WithConvertedTriviaFrom(node)
                    Case CS.SyntaxKind.PlusPlusToken
                        Return VBFactory.EmptyStatement.WithLeadingTrivia(node.CheckCorrectnessLeadingTrivia("C# ++ operator not available in VB")).WithPrependedLeadingTrivia(ConvertTrivia(node.GetLeadingTrivia)).WithConvertedTrailingTriviaFrom(node)
                    Case Else
                        Dim OperatorToken As SyntaxToken = ConvertOperatorDeclarationToken(lSyntaxKind)
                        Dim stmt As VBS.OperatorStatementSyntax = VBFactory.OperatorStatement(VBFactory.List(Attributes), VBFactory.TokenList(Modifiers), OperatorToken, parameterList, VBFactory.SimpleAsClause(ReturnAttributes, DirectCast(node.ReturnType.Accept(Me), VBS.TypeSyntax)))
                        Return VBFactory.OperatorBlock(stmt, body).WithConvertedTriviaFrom(node)
                End Select
            End Function

            Public Overrides Function VisitPropertyDeclaration(node As CSS.PropertyDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim Identifier As SyntaxToken
                Dim IdString As String = ""

                Dim ImplementsClause As VBS.ImplementsClauseSyntax
                Dim interfaceMembers As SeparatedSyntaxList(Of VBS.QualifiedNameSyntax)
                Dim SimpleName As VBS.IdentifierNameSyntax = VBFactory.IdentifierName(node.Identifier.ValueText)

                Dim ExplicitInterfaceIdentifier As VBS.QualifiedNameSyntax
                If node.ExplicitInterfaceSpecifier IsNot Nothing Then
                    Dim VisualBasicSyntaxNode1 As VB.VisualBasicSyntaxNode = node.ExplicitInterfaceSpecifier.Accept(Me)

                    If TypeOf VisualBasicSyntaxNode1 Is VBS.QualifiedNameSyntax Then
                        ExplicitInterfaceIdentifier = DirectCast(VisualBasicSyntaxNode1, VBS.QualifiedNameSyntax)
                        IdString = ExplicitInterfaceIdentifier.Right.ToString
                        Dim OpenParenIndex As Integer = IdString.IndexOf("("c)
                        If OpenParenIndex > 0 Then
                            IdString = IdString.Left(OpenParenIndex)
                        End If
                        interfaceMembers = interfaceMembers.Add(VBFactory.QualifiedName(ExplicitInterfaceIdentifier, SimpleName))
                    ElseIf TypeOf VisualBasicSyntaxNode1 Is VBS.GenericNameSyntax Then
                        Dim GenericName As VBS.GenericNameSyntax = DirectCast(VisualBasicSyntaxNode1, VBS.GenericNameSyntax)
                        interfaceMembers = interfaceMembers.Add(VBFactory.QualifiedName(GenericName, SimpleName))
                        IdString = GenericName.Identifier.ToString
                    ElseIf TypeOf VisualBasicSyntaxNode1 Is VBS.IdentifierNameSyntax Then
                        Dim IdentifierName As VBS.IdentifierNameSyntax = DirectCast(VisualBasicSyntaxNode1, VBS.IdentifierNameSyntax)
                        interfaceMembers = interfaceMembers.Add(VBFactory.QualifiedName(IdentifierName, SimpleName))
                        IdString = IdentifierName.Identifier.ToString
                    Else
                        ' Might need to handle other types here
                        Stop
                    End If
                End If

                If node.ExplicitInterfaceSpecifier Is Nothing Then
                    Identifier = GenerateSafeVBToken(node.Identifier, IsQualifiedName:=False, IsTypeName:=False)
                    Dim PropertySymbol As ISymbol = mSemanticModel.GetDeclaredSymbol(node)
                    ImplementsClause = ImplementedMembers.GetImplementsClauseForProperty(CType(PropertySymbol, IPropertySymbol))
                Else
                    Identifier = VBFactory.Identifier($"{IdString}_{node.Identifier.ValueText}")
                    ImplementsClause = VBFactory.ImplementsClause(interfaceMembers)
                End If
                Dim Attributes As New List(Of VBS.AttributeListSyntax)
                Dim ReturnAttributes As SyntaxList(Of VBS.AttributeListSyntax) = Nothing
                ConvertAndSplitAttributes(node.AttributeLists, Attributes, ReturnAttributes)
                Dim isIterator As Boolean = False
                Dim accessors As New List(Of VBS.AccessorBlockSyntax)
                Dim Statements As SyntaxList(Of VBS.StatementSyntax)
                If node.ExpressionBody IsNot Nothing Then
                    Dim ExpressionSyntaxNode As VB.VisualBasicSyntaxNode = node.ExpressionBody.Expression.Accept(Me).WithConvertedLeadingTriviaFrom(node.ExpressionBody.Expression)
                    If TypeOf ExpressionSyntaxNode Is VBS.ThrowStatementSyntax Then
                        Statements = VBFactory.SingletonList(Of VBS.StatementSyntax)(DirectCast(ExpressionSyntaxNode, VBS.ThrowStatementSyntax).WithConvertedTriviaFrom(node.ExpressionBody))
                    Else
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

                        Dim ReturnStatement As VBS.ReturnStatementSyntax = VBFactory.ReturnStatement(ReturnedExpression.WithLeadingTrivia(SpaceTrivia)).
                                                WithLeadingTrivia(ReturnedExpression.GetLeadingTrivia)
                        ReturnStatement = ReturnStatement.RelocateDirectivesInLeadingTrivia
                        Statements = ReplaceStatementsWithMarkedStatements(node, VBFactory.SingletonList(Of VBS.StatementSyntax)(ReturnStatement))
                    End If
                    accessors.Add(VBFactory.AccessorBlock(VB.SyntaxKind.GetAccessorBlock, VBFactory.GetAccessorStatement.WithTrailingEOL, Statements, VBFactory.EndGetStatement()))
                Else
                    If node.AccessorList IsNot Nothing Then
                        For Each a As CSS.AccessorDeclarationSyntax In node.AccessorList.Accessors
                            Dim _isIterator As Boolean
                            accessors.Add(ConvertAccessor(a, IsModule, _isIterator))
                            isIterator = isIterator Or _isIterator
                        Next
                    End If
                End If

                Dim CSharpModifiers As SyntaxTokenList = node.Modifiers
                Dim IsWriteOnly As Boolean = False
                If node.AccessorList IsNot Nothing AndAlso node.AccessorList.Accessors.Count = 1 Then
                    Select Case node.AccessorList.Accessors(0).Keyword.RawKind
                        Case CS.SyntaxKind.GetKeyword
                            CSharpModifiers = CSharpModifiers.Add(CSharpSyntaxFactory.ReadOnlyKeyword)
                        Case CS.SyntaxKind.SetKeyword
                            IsWriteOnly = True
                        Case Else
                            Throw UnreachableException
                    End Select

                End If
                Dim Context As TokenContext = TokenContext.Member
                ' TODO find better way to find out if we are in interface
                If node.IsParentKind(CS.SyntaxKind.InterfaceDeclaration) Then
                    Context = TokenContext.InterfaceOrModule
                End If
                Dim Modifiers As List(Of SyntaxToken) = ConvertModifiers(CSharpModifiers, IsModule, Context)
                If isIterator Then
                    Modifiers.Add(IteratorKeyword)
                End If
                If IsWriteOnly Then
                    Modifiers.Add(WriteOnlyKeyword)
                End If

                If node.AccessorList Is Nothing Then
                    Modifiers.Add(VisualBasicSyntaxFactory.ReadOnlyKeyword)
                End If

                Dim TypeNode As VBS.TypeSyntax = DirectCast(node.Type.Accept(Me), VBS.TypeSyntax)
                Dim AsClause As VBS.SimpleAsClauseSyntax = VBFactory.SimpleAsClause(ReturnAttributes, TypeNode.WithoutTrivia)
                Dim Initializer As VBS.EqualsValueSyntax = If(node.Initializer Is Nothing, Nothing, VBFactory.EqualsValue(DirectCast(node.Initializer.Value.Accept(Me), VBS.ExpressionSyntax)))
                Dim Keyword As SyntaxToken
                Dim TypeLeadingTrivia As New List(Of SyntaxTrivia)
                TypeLeadingTrivia.AddRange(ConvertTrivia(node.Type.GetLeadingTrivia))
                If TriviaIsIdentical(VBFactory.TriviaList(ConvertTrivia(node.GetLeadingTrivia)), TypeLeadingTrivia) Then
                    TypeLeadingTrivia.Clear()
                End If

                If Modifiers.Count = 0 Then
                    Keyword = PropertyKeyword.WithLeadingTrivia(TypeLeadingTrivia)
                    TypeLeadingTrivia.Clear()
                Else
                    Keyword = PropertyKeyword
                    If TypeLeadingTrivia.Count > 0 Then
                        TypeLeadingTrivia.Insert(0, VB_EOLTrivia)
                    End If
                End If
                Dim PrependedTrivia As List(Of SyntaxTrivia) = DedupLeadingTrivia(node, Keyword, Attributes, Modifiers)
                Dim PropertyStatement As VBS.PropertyStatementSyntax = VBFactory.PropertyStatement(VBFactory.List(Attributes),
                                                                                                VBFactory.TokenList(Modifiers),
                                                                                                Keyword,
                                                                                                Identifier.WithTrailingTrivia(SpaceTrivia),
                                                                                                parameterList:=Nothing,
                                                                                                AsClause,
                                                                                                Initializer,
                                                                                                ImplementsClause
                                                                                                ).WithPrependedLeadingTrivia(PrependedTrivia).WithTrailingEOL
                Dim StmtList As SyntaxList(Of VBS.StatementSyntax) = ReplaceStatementsWithMarkedStatements(node, VBFactory.SingletonList(Of VBS.StatementSyntax)(PropertyStatement))
                Dim AddedLeadingTrivia As New List(Of SyntaxTrivia)
                Select Case StmtList.Count
                    Case 1
                        If TypeOf StmtList(0) Is VBS.EmptyStatementSyntax Then
                            AddedLeadingTrivia.AddRange((StmtList(0).GetLeadingTrivia))
                        End If
                    Case 2
                        AddedLeadingTrivia.AddRange(StmtList(0).GetLeadingTrivia)
                    Case Else

                End Select
                If node.AccessorList IsNot Nothing Then
                    Dim BodyOrExpressionBodyIsNothing As Boolean = True
                    For Each a As CSS.AccessorDeclarationSyntax In node.AccessorList.Accessors
                        If a.Body IsNot Nothing OrElse a.ExpressionBody IsNot Nothing Then
                            BodyOrExpressionBodyIsNothing = False
                            Exit For
                        End If
                    Next
                    If BodyOrExpressionBodyIsNothing Then
                        If AddedLeadingTrivia.Count > 0 Then
                            Return PropertyStatement.WithLeadingTrivia(AddedLeadingTrivia).WithConvertedTrailingTriviaFrom(node).WithTrailingEOL
                        End If
                        Return PropertyStatement.WithConvertedTriviaFrom(node).WithTrailingEOL
                    End If
                End If
                If AddedLeadingTrivia.Count > 0 Then
                    Return VBFactory.PropertyBlock(PropertyStatement.WithLeadingTrivia(AddedLeadingTrivia).WithTrailingEOL, VBFactory.List(accessors))
                End If
                Dim AccessorOpenBraceTrivia As New List(Of SyntaxTrivia)
                Dim AccessorOpenBrace As SyntaxToken = node.AccessorList.GetBraces.Item1
                AccessorOpenBraceTrivia.AddRange(ConvertTrivia(AccessorOpenBrace.LeadingTrivia))
                AccessorOpenBraceTrivia.AddRange(ConvertTrivia(AccessorOpenBrace.TrailingTrivia))

                Dim AccessorClosingBraceTrivia As New List(Of SyntaxTrivia)
                Dim AccessorClosingBrace As SyntaxToken = node.AccessorList.GetBraces.Item2
                AccessorClosingBraceTrivia.AddRange(ConvertTrivia(AccessorClosingBrace.LeadingTrivia))
                AccessorClosingBraceTrivia.AddRange(ConvertTrivia(AccessorClosingBrace.TrailingTrivia))

                Dim ClosingNodeBraces As SyntaxToken = node.GetBraces.Item2
                Dim EndPropertyStatement As VBS.EndBlockStatementSyntax = VBFactory.EndPropertyStatement.
                    WithConvertedTriviaFrom(ClosingNodeBraces).
                    WithPrependedLeadingTrivia(AccessorClosingBraceTrivia)
                accessors(0) = accessors(0).WithPrependedLeadingTrivia(AccessorOpenBraceTrivia).WithTrailingEOL
                Return VBFactory.PropertyBlock(PropertyStatement.WithTrailingEOL, VBFactory.List(accessors), EndPropertyStatement).
                                        WithAppendedTrailingTrivia(TypeLeadingTrivia).
                                        RestructureAttributesAndModifiers(Attributes.Count > 0, Modifiers.Count > 0)

            End Function

        End Class

    End Class

End Namespace
