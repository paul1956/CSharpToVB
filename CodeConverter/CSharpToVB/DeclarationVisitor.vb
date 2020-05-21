' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.Runtime.InteropServices

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
            Private Const CompilerServices As String = "System.Runtime.CompilerServices"
            Private ReadOnly _extensionAttribute As VBS.AttributeSyntax = VBFactory.Attribute(Nothing, VBFactory.ParseTypeName("Extension"), VBFactory.ArgumentList())

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
                Throw New NotSupportedException($"Assignment Operator {lSyntaxKind} is not supported")
            End Function

            Private Shared Function DedupLeadingTrivia(Of T As CS.CSharpSyntaxNode)(node As T, Keyword As SyntaxToken, Attributes As List(Of VBS.AttributeListSyntax), Modifiers As List(Of SyntaxToken)) As List(Of SyntaxTrivia)
                Dim NodeLeadingTrivia As New List(Of SyntaxTrivia)
                NodeLeadingTrivia.AddRange(ConvertTrivia(node.GetLeadingTrivia))
                If Attributes.Any Then
                    If TriviaIsIdentical(Attributes(0).GetLeadingTrivia, NodeLeadingTrivia) Then
                        NodeLeadingTrivia.Clear()
                    End If
                ElseIf Modifiers.Any Then
                    If TriviaIsIdentical(Modifiers(0).LeadingTrivia, NodeLeadingTrivia) Then
                        NodeLeadingTrivia.Clear()
                    End If
                ElseIf TriviaIsIdentical(Keyword.LeadingTrivia, NodeLeadingTrivia) Then
                    NodeLeadingTrivia.Clear()
                End If

                Return NodeLeadingTrivia
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

            Private Shared Function RelocateDirectivesInTrailingTrivia(ParameterList As VBS.ParameterListSyntax, StatementTrailingTrivia As List(Of SyntaxTrivia)) As VBS.ParameterListSyntax
                If ParameterList IsNot Nothing AndAlso ParameterList.HasTrailingTrivia AndAlso ParameterList.GetTrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                    Dim ParameterListTrailingTrivia As New List(Of SyntaxTrivia)
                    Dim TrailingTrivialist As SyntaxTriviaList = ParameterList.GetTrailingTrivia
                    Dim FoundEndIf As Boolean = False
                    For Each e As IndexClass(Of SyntaxTrivia) In TrailingTrivialist.WithIndex
                        Dim NextTrivia As SyntaxTrivia = If(Not e.IsLast, TrailingTrivialist(e.Index + 1), Nothing)
                        Select Case e.Value.RawKind
                            Case VB.SyntaxKind.CommentTrivia
                                ParameterListTrailingTrivia.Add(e.Value)
                            Case VB.SyntaxKind.EndOfLineTrivia
                                If FoundEndIf Then
                                    StatementTrailingTrivia.Add(e.Value)
                                Else
                                    ParameterListTrailingTrivia.Add(e.Value)
                                End If
                            Case VB.SyntaxKind.WhitespaceTrivia
                                ParameterListTrailingTrivia.Add(e.Value)
                            Case VB.SyntaxKind.EndIfDirectiveTrivia
                                FoundEndIf = True
                                If Not StatementTrailingTrivia.Any Then
                                    StatementTrailingTrivia.Add(VBEOLTrivia)
                                End If
                                StatementTrailingTrivia.Add(e.Value)
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
                Dim visitor As MethodBodyVisitor = New MethodBodyVisitor(_mSemanticModel, Me)
                If node.Body IsNot Nothing Then
                    body = GetBodyStatements(node.Body, visitor)
                    isIterator = visitor.IsInterator
                ElseIf node.ExpressionBody IsNot Nothing Then
                    body = GetExpressionBodyStatements(node.ExpressionBody)
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
                        ValueParam = VBFactory.Parameter(ValueModifiedIdentifier).
                            WithAsClause(VBFactory.SimpleAsClause(DirectCast(Parent.Type.Accept(Me), VBS.TypeSyntax).
                            WithLeadingTrivia(SpaceTrivia)))
                        stmt = VBFactory.SetAccessorStatement(Attributes, VBFactory.TokenList(Modifiers), VBFactory.ParameterList(VBFactory.SingletonSeparatedList(ValueParam)))
                        endStmt = VBFactory.EndSetStatement()
                    Case CS.SyntaxKind.AddAccessorDeclaration
                        blockKind = VB.SyntaxKind.AddHandlerAccessorBlock
                        ValueParam = VBFactory.Parameter(ValueModifiedIdentifier).
                            WithAsClause(VBFactory.SimpleAsClause(DirectCast(Parent.Type.Accept(Me), VBS.TypeSyntax)))
                        stmt = VBFactory.AddHandlerAccessorStatement(Attributes, VBFactory.TokenList(Modifiers), VBFactory.ParameterList(VBFactory.SingletonSeparatedList(ValueParam)))
                        endStmt = VBFactory.EndAddHandlerStatement()
                    Case CS.SyntaxKind.RemoveAccessorDeclaration
                        blockKind = VB.SyntaxKind.RemoveHandlerAccessorBlock
                        ValueParam = VBFactory.Parameter(ValueModifiedIdentifier).
                            WithAsClause(VBFactory.SimpleAsClause(DirectCast(Parent.Type.Accept(Me), VBS.TypeSyntax)))
                        stmt = VBFactory.RemoveHandlerAccessorStatement(Attributes, VBFactory.TokenList(Modifiers), VBFactory.ParameterList(VBFactory.SingletonSeparatedList(ValueParam)))
                        endStmt = VBFactory.EndRemoveHandlerStatement()
                    Case Else
                        Throw New NotSupportedException()
                End Select
                Return VBFactory.AccessorBlock(blockKind, stmt.WithConvertedTriviaFrom(node.Body.GetBraces.Item1).WithTrailingEOL, body, endStmt.WithConvertedTriviaFrom(node.Body.GetBraces.Item2)).WithConvertedTriviaFrom(node)
            End Function

            Private Sub ConvertAndSplitAttributes(attributeLists As SyntaxList(Of CSS.AttributeListSyntax), <Out> ByRef Attributes As List(Of VBS.AttributeListSyntax), <Out> ByRef ReturnAttributes As SyntaxList(Of VBS.AttributeListSyntax), ByRef finalDirectiveTrivia As List(Of SyntaxTrivia))
                Dim retAttr As List(Of VBS.AttributeListSyntax) = New List(Of VBS.AttributeListSyntax)()
                Dim FirstAttribuate As Boolean = True
                For Each e As IndexClass(Of CSS.AttributeListSyntax) In attributeLists.WithIndex
                    Dim attrList As CSS.AttributeListSyntax = e.Value
                    If attrList.Target Is Nothing OrElse Not attrList.Target.Identifier.IsKind(CS.SyntaxKind.ReturnKeyword) Then
                        Dim item As VBS.AttributeListSyntax = DirectCast(attrList.Accept(Me), VBS.AttributeListSyntax)
                        If FirstAttribuate Then
                            FirstAttribuate = False
                        Else
                            Dim itemLeadingTrivia As SyntaxTriviaList = item.GetLeadingTrivia
                            If itemLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                Dim LeadingTriviaList As New List(Of SyntaxTrivia)
                                Dim FirstComment As Boolean = True
                                Dim NeedWhiteSpace As Boolean = True
                                Dim needLineContinuation As Boolean = True
                                For Each VBSyntaxTrivia As SyntaxTrivia In itemLeadingTrivia
                                    Select Case VBSyntaxTrivia.RawKind
                                        Case VB.SyntaxKind.WhitespaceTrivia
                                            LeadingTriviaList.Add(VBSyntaxTrivia)
                                            NeedWhiteSpace = False
                                        Case VB.SyntaxKind.CommentTrivia
                                            If FirstComment Then
                                                FirstComment = False
                                                If NeedWhiteSpace Then
                                                    NeedWhiteSpace = False
                                                    LeadingTriviaList.Add(SpaceTrivia)
                                                End If
                                                LeadingTriviaList.Add(LineContinuation)
                                                LeadingTriviaList.Add(SpaceTrivia)
                                                needLineContinuation = False
                                            End If
                                            LeadingTriviaList.Add(VBSyntaxTrivia)
                                        Case VB.SyntaxKind.IfDirectiveTrivia
                                            If FirstComment Then
                                                FirstComment = False
                                                If NeedWhiteSpace Then
                                                    NeedWhiteSpace = False
                                                    LeadingTriviaList.Add(SpaceTrivia)
                                                End If
                                                LeadingTriviaList.Add(LineContinuation)
                                                LeadingTriviaList.Add(SpaceTrivia)
                                                needLineContinuation = False
                                            End If
                                            LeadingTriviaList.Add(VBFactory.CommentTrivia($"Directive not allowed here {VBSyntaxTrivia}"))
                                        Case VB.SyntaxKind.ElseDirectiveTrivia
                                            If FirstComment Then
                                                FirstComment = False
                                                If NeedWhiteSpace Then
                                                    NeedWhiteSpace = False
                                                    LeadingTriviaList.Add(SpaceTrivia)
                                                End If
                                                LeadingTriviaList.Add(LineContinuation)
                                                LeadingTriviaList.Add(SpaceTrivia)
                                                needLineContinuation = False
                                            End If
                                            LeadingTriviaList.Add(VBFactory.CommentTrivia($"Directive not allowed here {VBSyntaxTrivia}"))
                                        Case VB.SyntaxKind.ElseIfDirectiveTrivia
                                            If FirstComment Then
                                                FirstComment = False
                                                If NeedWhiteSpace Then
                                                    NeedWhiteSpace = False
                                                    LeadingTriviaList.Add(SpaceTrivia)
                                                End If
                                                LeadingTriviaList.Add(LineContinuation)
                                                LeadingTriviaList.Add(SpaceTrivia)
                                                needLineContinuation = False
                                            End If
                                            LeadingTriviaList.Add(VBFactory.CommentTrivia($"Directive not allowed here {VBSyntaxTrivia}"))
                                        Case VB.SyntaxKind.EndIfDirectiveTrivia
                                            finalDirectiveTrivia.Add(VBSyntaxTrivia)
                                        Case VB.SyntaxKind.EndOfLineTrivia
                                            If NeedWhiteSpace Then
                                                NeedWhiteSpace = False
                                                LeadingTriviaList.Add(SpaceTrivia)
                                            End If
                                            If needLineContinuation Then
                                                LeadingTriviaList.Add(LineContinuation)
                                                needLineContinuation = False
                                                NeedWhiteSpace = True
                                            End If
                                            LeadingTriviaList.Add(VBSyntaxTrivia)
                                        Case Else
                                            Stop
                                            NeedWhiteSpace = True
                                    End Select
                                Next
                                item = item.WithLeadingTrivia(LeadingTriviaList)
                            End If
                        End If
                        Attributes.Add(item)
                    Else
                        ' Remove trailing CRLF from return attributes
                        retAttr.Add(DirectCast(attrList.Accept(Me).With({SpaceTrivia}, {SpaceTrivia}), VBS.AttributeListSyntax))
                    End If
                Next

                ReturnAttributes = VBFactory.List(retAttr)
            End Sub

            Public Overrides Function VisitAnonymousObjectMemberDeclarator(node As CSS.AnonymousObjectMemberDeclaratorSyntax) As VB.VisualBasicSyntaxNode
                If node?.NameEquals Is Nothing Then
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
                Return node?.Expression.Accept(Me)
            End Function

            Public Overrides Function VisitConstructorDeclaration(node As CSS.ConstructorDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim Attributes As SyntaxList(Of VBS.AttributeListSyntax) = VBFactory.List(
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
                Dim Modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, IsModule, TokenContext.[New])

                Dim parameterList As VBS.ParameterListSyntax = DirectCast(node.ParameterList?.Accept(Me), VBS.ParameterListSyntax)
                Dim SubNewStatement As VBS.StatementSyntax =
                    VBFactory.SubNewStatement(Attributes,
                                              VBFactory.TokenList(Modifiers),
                                              parameterList
                                              ).WithTrailingEOL.
                                              RestructureAttributesAndModifiers(Attributes.Any, Modifiers.Any)

                Dim EndSubStatement As VBS.EndBlockStatementSyntax = VBFactory.EndSubStatement.WithTrailingEOL
                Dim Body As New SyntaxList(Of VBS.StatementSyntax)
                Dim csCloseBraceToken As SyntaxToken = CSharpSyntaxFactory.CloseBraceToken
                Dim CloseBraceTrivia As New List(Of SyntaxTrivia)
                If node.Body IsNot Nothing Then
                    csCloseBraceToken = node.Body.CloseBraceToken
                    For Each e As IndexClass(Of CSS.StatementSyntax) In node.Body.Statements.WithIndex
                        If TypeOf e.Value Is CSS.LocalFunctionStatementSyntax Then
                            Body = Body.AddRange(ReplaceOneStatementWithMarkedStatements(e.Value, VBFactory.EmptyStatement(), RemoveStatement:=True))
                        Else
                            Body = Body.AddRange(e.Value.Accept(New MethodBodyVisitor(_mSemanticModel, Me)))
                        End If
                    Next
                    EndSubStatement = VBFactory.EndSubStatement().WithConvertedTriviaFrom(csCloseBraceToken)
                    If node.Body.OpenBraceToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        Dim Trivia As IEnumerable(Of SyntaxTrivia) = ConvertTrivia(node.Body.OpenBraceToken.LeadingTrivia)
                        If Not Body.Any Then
                            EndSubStatement = EndSubStatement.WithPrependedLeadingTrivia(Trivia)
                        End If
                    End If
                ElseIf node.ExpressionBody IsNot Nothing Then
                    Body = GetExpressionBodyStatements(node.ExpressionBody)
                End If
                If Initializer IsNot Nothing Then
                    Body = Body.InsertRange(0, ReplaceOneStatementWithMarkedStatements(node, Initializer))
                Else
                    Body = ReplaceStatementsWithMarkedStatements(node, Body)

                End If
                Return VBFactory.ConstructorBlock(CType(SubNewStatement, VBS.SubNewStatementSyntax),
                                                  Body,
                                                  EndSubStatement).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitConstructorInitializer(node As CSS.ConstructorInitializerSyntax) As VB.VisualBasicSyntaxNode
                Dim ArgumentList As VBS.ArgumentListSyntax = DirectCast((node?.ArgumentList.Accept(Me)), VBS.ArgumentListSyntax)
                Dim SimpleMemberAccessExpression As VBS.MemberAccessExpressionSyntax
                Dim parent As SyntaxNode = node.Parent.Parent
                Dim MeOrMyExpression As VBS.ExpressionSyntax = If(TypeOf parent Is CSS.StructDeclarationSyntax,
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
                Dim FinalTrailingDirective As New List(Of SyntaxTrivia)
                ConvertAndSplitAttributes(node.AttributeLists, AttributeLists, ReturnAttributes, FinalTrailingDirective)
                Dim parameterList As VBS.ParameterListSyntax = DirectCast(node.ParameterList?.Accept(Me), VBS.ParameterListSyntax).
                                                                    WithRestructuredingEOLTrivia
                Dim Modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, IsModule, TokenContext.Member)
                Dim visitor As New MethodBodyVisitor(_mSemanticModel, Me)
                Modifiers.Add(If(node.ImplicitOrExplicitKeyword.ValueText = "explicit", NarrowingKeyword, WideningKeyword))
                Dim Type As VBS.TypeSyntax = DirectCast(node.Type.Accept(Me), VBS.TypeSyntax).With({SpaceTrivia}, {SpaceTrivia})
                Dim AsClause As VBS.SimpleAsClauseSyntax = VBFactory.SimpleAsClause(Nothing, Type)
                Dim OperatorStatement As VBS.OperatorStatementSyntax = VBFactory.OperatorStatement(VBFactory.List(AttributeLists), VBFactory.TokenList(Modifiers), CTypeKeyword, parameterList, AsClause).WithTrailingEOL
                If FinalTrailingDirective.Any Then
                    OperatorStatement = OperatorStatement.WithAppendedTrailingTrivia(FinalTrailingDirective)
                End If

                Dim body As New SyntaxList(Of VBS.StatementSyntax)
                If node.Body IsNot Nothing Then
                    body = GetBodyStatements(node.Body, visitor)
                ElseIf node.ExpressionBody IsNot Nothing Then
                    body = GetExpressionBodyStatements(node.ExpressionBody)
                End If
                Dim EndOperatorStatement As VBS.EndBlockStatementSyntax = VBFactory.EndBlockStatement(VB.SyntaxKind.EndOperatorStatement, EndKeyword, BlockKeyword).WithConvertedTriviaFrom(node.Body.GetBraces.Item2)
                Dim OperatorBlock As VBS.OperatorBlockSyntax = VBFactory.OperatorBlock(OperatorStatement, body, EndOperatorStatement).WithConvertedTriviaFrom(node)
                Return PrependStatementWithMarkedStatementTrivia(node, OperatorBlock)
            End Function

            Public Overrides Function VisitDestructorDeclaration(node As CSS.DestructorDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim AttributeLists As SyntaxList(Of VBS.AttributeListSyntax) = VBFactory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))
                Dim Modifiers As SyntaxTokenList = VBFactory.TokenList(ProtectedKeyword, OverridesKeyword)
                Dim Identifier As SyntaxToken = VBFactory.Identifier(NameOf(Finalize))
                Dim ParameterList As VBS.ParameterListSyntax = DirectCast(node.ParameterList?.Accept(Me), VBS.ParameterListSyntax)
                Dim body As SyntaxList(Of VBS.StatementSyntax)
                If node.Body IsNot Nothing Then
                    body = GetBodyStatements(node.Body, New MethodBodyVisitor(_mSemanticModel, Me))
                Else
                    body = GetExpressionBodyStatements(node.ExpressionBody)
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
                                              VBFactory.List(body)).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitEventDeclaration(node As CSS.EventDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim Attributes As New List(Of VBS.AttributeListSyntax)
                Dim ReturnAttributes As SyntaxList(Of VBS.AttributeListSyntax) = Nothing
                Dim FinalTrailingDirective As New List(Of SyntaxTrivia)
                ConvertAndSplitAttributes(node.AttributeLists, Attributes, ReturnAttributes, FinalTrailingDirective)
                Dim Modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, IsModule, TokenContext.Member)
                Dim Identifier As SyntaxToken = GenerateSafeVBToken(node.Identifier).WithTrailingTrivia(SpaceTrivia)
                Dim AsClause As VBS.SimpleAsClauseSyntax = VBFactory.SimpleAsClause(attributeLists:=ReturnAttributes, DirectCast(node.Type.Accept(Me), VBS.TypeSyntax))
                Modifiers.Add(CustomKeyword)
                Dim stmt As VBS.EventStatementSyntax = VBFactory.EventStatement(attributeLists:=VBFactory.List(Attributes), VBFactory.TokenList(Modifiers), Identifier, parameterList:=Nothing, AsClause, implementsClause:=Nothing).WithTrailingEOL
                If FinalTrailingDirective.Any Then
                    stmt = stmt.WithAppendedTrailingTrivia(FinalTrailingDirective)
                End If
                Dim EmptyBody As Boolean = True
                For Each e As IndexClass(Of CSS.AccessorDeclarationSyntax) In node.AccessorList.Accessors.WithIndex
                    If e.Value.Body IsNot Nothing Then
                        EmptyBody = False
                        Exit For
                    End If
                    If e.Value.ExpressionBody IsNot Nothing Then
                        Exit For
                    End If
                    Return stmt.WithConvertedTriviaFrom(node)
                Next
                Dim accessors As VBS.AccessorBlockSyntax()
                accessors = node.AccessorList?.Accessors.Select(Function(a As CSS.AccessorDeclarationSyntax) ConvertAccessor(a, IsModule:=IsModule, isIterator:=False)).ToArray()
                Return VBFactory.EventBlock(stmt, VBFactory.List(accessors)).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitEventFieldDeclaration(node As CSS.EventFieldDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim id As SyntaxToken = VBFactory.Identifier(MakeVBSafeName(node.Declaration.Variables.Single().Identifier.ValueText))
                Dim ReturnAttributes As New SyntaxList(Of VBS.AttributeListSyntax)
                Dim AttributeList As New List(Of VBS.AttributeListSyntax)
                Dim FinalTrailingDirective As New List(Of SyntaxTrivia)
                ConvertAndSplitAttributes(node.AttributeLists, AttributeList, ReturnAttributes, FinalTrailingDirective)
                If FinalTrailingDirective.Any Then
                    Stop
                End If
                Return VBFactory.EventStatement(VBFactory.List(AttributeList),
                                     VBFactory.TokenList(ConvertModifiers(node.Modifiers, IsModule, TokenContext.Member)),
                                                    id,
                                                    parameterList:=Nothing,
                                                    VBFactory.SimpleAsClause(attributeLists:=Nothing, DirectCast(node.Declaration.Type.Accept(Me), VBS.TypeSyntax)),
                                                    implementsClause:=Nothing).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitFieldDeclaration(node As CSS.FieldDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim _TypeInfo As TypeInfo = ModelExtensions.GetTypeInfo(_mSemanticModel, node.Declaration.Type)
                Dim variableOrConstOrReadonly As TokenContext = TokenContext.VariableOrConst
                If _TypeInfo.ConvertedType IsNot Nothing AndAlso _TypeInfo.ConvertedType.TypeKind = Microsoft.CodeAnalysis.TypeKind.Class Then
                    For Each e As IndexClass(Of CSS.VariableDeclaratorSyntax) In node.Declaration.Variables.WithIndex
                        If e.Value.Initializer IsNot Nothing AndAlso e.Value.Initializer.Value.IsKind(CS.SyntaxKind.NullLiteralExpression) Then
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
                If node.Modifiers.Contains(CS.SyntaxKind.VolatileKeyword) Then
                    Dim Name As VBS.TypeSyntax = VBFactory.ParseTypeName("Volatile")
                    Dim VolatileAttribute As SeparatedSyntaxList(Of VBS.AttributeSyntax) = VBFactory.SingletonSeparatedList(VBFactory.Attribute(Name))
                    LeadingTrivia.Add(VBFactory.CommentTrivia("' TODO TASK: VB has no direct equivalent to C# Volatile Modifier, an Attribute was substituted."))
                    Attributes = Attributes.Add(VBFactory.AttributeList(VolatileAttribute).WithLeadingTrivia(LeadingTrivia))
                    LeadingTrivia.Clear()
                End If
                If node.AttributeLists.Any Then
                    LeadingTrivia.AddRange(ConvertTrivia(node.AttributeLists(0).GetLeadingTrivia))
                Else
                    If node.Modifiers.Any Then
                        LeadingTrivia.AddRange(ConvertTrivia(node.Modifiers(0).LeadingTrivia))
                    ElseIf modifierList.Any Then
                        LeadingTrivia.AddRange(modifierList(0).LeadingTrivia)
                    End If
                    If modifierList(0).HasLeadingTrivia Then
                        modifierList(0) = modifierList(0).WithLeadingTrivia(modifierList(0).LeadingTrivia.Last)
                    End If
                End If

                For Each a As CSS.AttributeListSyntax In node.AttributeLists
                    Attributes = Attributes.Add(DirectCast(a.Accept(Me), VBS.AttributeListSyntax))
                Next
                If Attributes.Any Then
                    Attributes = Attributes.Replace(Attributes(0), Attributes(0).WithLeadingTrivia(LeadingTrivia))
                Else
                    modifierList(0) = modifierList(0).WithLeadingTrivia(LeadingTrivia)
                End If
                Dim declarators As SeparatedSyntaxList(Of VBS.VariableDeclaratorSyntax) = RemodelVariableDeclaration(node.Declaration, Me, _mSemanticModel, IsFieldDeclaration:=True, LeadingTrivia)
                Dim FieldDeclaration As VBS.FieldDeclarationSyntax
                Dim modifiers As SyntaxTokenList = VBFactory.TokenList(modifierList)
                FieldDeclaration = VBFactory.FieldDeclaration(Attributes, modifiers, declarators).WithLeadingTrivia(LeadingTrivia)
                FieldDeclaration = AddSpecialCommentToField(node, FieldDeclaration)
                Return FieldDeclaration.RestructureAttributesAndModifiers(Attributes.Any, modifiers.Any).
                    WithMergedTrailingTrivia(GetTriviaFromUnneededToken(node.SemicolonToken)).WithTrailingEOL
            End Function

            Public Overrides Function VisitIndexerDeclaration(node As CSS.IndexerDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim id As SyntaxToken = VBFactory.Identifier("Item")
                Dim Attributes As New List(Of VBS.AttributeListSyntax)
                Dim ReturnAttributes As SyntaxList(Of VBS.AttributeListSyntax) = Nothing
                Dim FinalTrailingDirective As New List(Of SyntaxTrivia)
                ConvertAndSplitAttributes(node.AttributeLists, Attributes, ReturnAttributes, FinalTrailingDirective)
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
                If Modifiers.Any Then
                    Modifiers.Insert(0, DefaultKeyword.WithLeadingTrivia(Modifiers(0).LeadingTrivia))
                    Modifiers(1) = Modifiers(1).WithLeadingTrivia(SpaceTrivia)
                End If
                Select Case accessors.Count
                    Case 0
                        Dim LastTrailingTrivia As New List(Of SyntaxTrivia)
                        LastTrailingTrivia.AddRange(Modifiers.Last.TrailingTrivia)
                        Modifiers(Modifiers.Count - 1) = Modifiers.Last.WithLeadingTrivia(SpaceTrivia).WithTrailingTrivia(SpaceTrivia)
                        Modifiers.Add(VisualBasicSyntaxFactory.ReadOnlyKeyword.WithLeadingTrivia(SpaceTrivia).WithTrailingTrivia(LastTrailingTrivia))

                        Dim AccessorStatement As VBS.AccessorStatementSyntax = VBFactory.GetAccessorStatement()
                        Dim Body As SyntaxList(Of VBS.StatementSyntax) = GetExpressionBodyStatements(node.ExpressionBody)
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
                If FinalTrailingDirective.Any Then
                    stmt = stmt.WithAppendedTrailingTrivia(FinalTrailingDirective)
                End If
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
                If node.Modifiers.Contains(CS.SyntaxKind.UnsafeKeyword) Then
                    Return FlagUnsupportedStatements(node, "unsafe Functions", CommentOutOriginalStatements:=True)
                End If

                If node.ReturnType IsNot Nothing AndAlso TypeOf node.ReturnType Is CSS.RefTypeSyntax Then
                    Return FlagUnsupportedStatements(node, "ref return Functions", CommentOutOriginalStatements:=True)
                End If
                SyncLock s_usedStacks
                    s_usedStacks.Push(s_usedIdentifiers)
                End SyncLock

                Dim id As SyntaxToken = GenerateSafeVBToken(node.Identifier)

                Dim methodInfo As ISymbol = ModelExtensions.GetDeclaredSymbol(_mSemanticModel, node)
                Dim possibleReturnVoid As Boolean? = methodInfo?.GetReturnType()?.SpecialType = SpecialType.System_Void
                Dim returnVoid As Boolean = If(possibleReturnVoid, False)
                Dim containingType As INamedTypeSymbol = methodInfo?.ContainingType
                Dim Attributes As New List(Of VBS.AttributeListSyntax)
                Dim ReturnAttributes As SyntaxList(Of VBS.AttributeListSyntax) = Nothing
                Dim FunctionStatementTrailingTrivia As New List(Of SyntaxTrivia)
                ConvertAndSplitAttributes(node.AttributeLists, Attributes, ReturnAttributes, FunctionStatementTrailingTrivia)
                Dim ParameterList As VBS.ParameterListSyntax = DirectCast(node.ParameterList?.Accept(Me), VBS.ParameterListSyntax)

                ParameterList = RelocateDirectivesInTrailingTrivia(ParameterList, FunctionStatementTrailingTrivia)
                Dim body As SyntaxList(Of VBS.StatementSyntax)? = Nothing

                Dim FunctionStatementLeadingTrivia As New List(Of SyntaxTrivia)
                Dim vbStatements As New List(Of VBS.StatementSyntax)
                Dim visitor As New MethodBodyVisitor(_mSemanticModel, Me)
                For Each localFunction As CSS.LocalFunctionStatementSyntax In node.DescendantNodes().OfType(Of CSS.LocalFunctionStatementSyntax).ToList()
                    Dim replacementStatement As VBS.StatementSyntax = localFunction.Accept(visitor)(0)
                    If TypeOf replacementStatement IsNot VBS.EmptyStatementSyntax OrElse replacementStatement.ContainsCommentOrDirectiveTrivia Then
                        vbStatements.Add(replacementStatement)
                    End If
                Next
                If node.Body IsNot Nothing Then
                    Dim vbStatementCollection As List(Of VBS.StatementSyntax)
                    For Each e As IndexClass(Of CSS.StatementSyntax) In node.Body.Statements.WithIndex
                        If e.Value.IsKind(CS.SyntaxKind.LocalFunctionStatement) Then
                            vbStatementCollection = ReplaceOneStatementWithMarkedStatements(e.Value, VBFactory.EmptyStatement, True).ToList
                        Else
                            vbStatementCollection = e.Value.Accept(visitor).ToList
                            vbStatementCollection = ReplaceStatementsWithMarkedStatements(e.Value, VBFactory.List(vbStatementCollection)).ToList
                        End If
                        If e.IsFirst Then
                            If node.Body.OpenBraceToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                vbStatementCollection(0) = vbStatementCollection(0).WithPrependedLeadingTrivia(ConvertTrivia(node.Body.OpenBraceToken.LeadingTrivia))
                            End If
                        End If
                        vbStatements.AddRange(ReplaceStatementsWithMarkedStatements(e.Value, VBFactory.List(vbStatementCollection)))
                    Next
                    body = VBFactory.List(vbStatements)
                ElseIf node.ExpressionBody IsNot Nothing Then
                    If node.ExpressionBody.Expression Is Nothing Then
                        Return PrependStatementWithMarkedStatementTrivia(node, VBFactory.EmptyStatement.WithConvertedTriviaFrom(node))
                    End If
                    If node.ExpressionBody.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        FunctionStatementLeadingTrivia.AddRange(ConvertTrivia(node.ExpressionBody.GetLeadingTrivia))
                        body = GetExpressionBodyStatements(node.ExpressionBody.WithoutLeadingTrivia)
                    Else
                        body = GetExpressionBodyStatements(node.ExpressionBody)
                    End If
                    body = ReplaceStatementsWithMarkedStatements(node, CType(body, SyntaxList(Of VBS.StatementSyntax)))
                End If
                If node.Modifiers.Contains(CS.SyntaxKind.ExternKeyword) Then
                    body = VBFactory.List(Of VBS.StatementSyntax)()
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
                If visitor.IsInterator And Not returnVoid Then
                    Modifiers.Add(IteratorKeyword)
                End If
                If node.ParameterList.Parameters.Any AndAlso node.ParameterList.Parameters(0).Modifiers.Any(CS.SyntaxKind.ThisKeyword) Then
                    Dim LeadingTrivia As New List(Of SyntaxTrivia)
                    If Attributes.Any AndAlso Attributes(0).HasLeadingTrivia Then
                        LeadingTrivia.AddRange(Attributes(0).GetLeadingTrivia)
                        Attributes(0) = Attributes(0).WithLeadingTrivia(LeadingTrivia.Last)
                    End If
                    If Attributes.Count = 0 AndAlso Modifiers.Any Then
                        LeadingTrivia.AddRange(Modifiers(0).LeadingTrivia)
                        Modifiers(0) = Modifiers(0).WithLeadingTrivia(SpaceTrivia)
                    End If
                    Attributes.Insert(0, VBFactory.AttributeList(VBFactory.SingletonSeparatedList(_extensionAttribute)).WithLeadingTrivia(LeadingTrivia))
                    LeadingTrivia.Clear()
                    If Not DirectCast(node.SyntaxTree, CS.CSharpSyntaxTree).HasUsingDirective(CompilerServices) Then
                        Dim ImportComilierServices As VBS.ImportsStatementSyntax = VBFactory.ImportsStatement(VBFactory.SingletonSeparatedList(Of VBS.ImportsClauseSyntax)(VBFactory.SimpleImportsClause(VBFactory.ParseName(CompilerServices)))).WithAppendedTrailingTrivia(VBEOLTrivia)
                        If Not _allImports.ContainsName(CompilerServices) Then
                            _allImports.Add(ImportComilierServices)
                        End If
                    End If
                End If
                If containingType?.IsStatic = True Then
                    Dim TokenList As New List(Of SyntaxToken)
                    Dim NewLeadingTrivia As New List(Of SyntaxTrivia)
                    For Each e As IndexClass(Of SyntaxToken) In Modifiers.WithIndex
                        Dim t As SyntaxToken = e.Value
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

                If Attributes.Any Then
                    FunctionStatementLeadingTrivia.AddRange(Attributes(0).GetLeadingTrivia)
                    Attributes(0) = Attributes(0).WithLeadingTrivia(SpaceTrivia)
                End If
                If node.ReturnType IsNot Nothing Then
                    If Modifiers.Count = 0 Then
                        FunctionStatementLeadingTrivia.AddRange(ConvertTrivia(node.ReturnType.GetLeadingTrivia))
                    Else
                        Dim NewModifierLeadingTrivia As New List(Of SyntaxTrivia)
                        Dim csNodeLeadingTrivia As SyntaxTriviaList = node.ReturnType.GetLeadingTrivia
                        If csNodeLeadingTrivia.Any Then
                            NewModifierLeadingTrivia.AddRange(Modifiers(0).LeadingTrivia)
                            NewModifierLeadingTrivia.AddRange(ConvertTrivia(csNodeLeadingTrivia))
                            If Not NewModifierLeadingTrivia.FirstOrDefault.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                NewModifierLeadingTrivia.Insert(0, VBEOLTrivia)
                            End If
                            Modifiers(0) = Modifiers(0).WithLeadingTrivia(NewModifierLeadingTrivia)
                        End If
                        If Attributes.Count = 0 AndAlso Modifiers(0).LeadingTrivia.Any Then
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
                Dim ImplementsClause As VBS.ImplementsClauseSyntax = s_implementedMembers.GetImplementsClauseForMethod(CType(methodInfo, IMethodSymbol))
                If id.ToString = "Dispose" AndAlso TypeOf node.Parent Is CSS.ClassDeclarationSyntax Then
                    Dim ParentClass As CSS.ClassDeclarationSyntax = DirectCast(node.Parent, CSS.ClassDeclarationSyntax)
                    If ParentClass.BaseList IsNot Nothing Then
                        For Each t As CSS.SimpleBaseTypeSyntax In ParentClass.BaseList.Types
                            If TypeOf t.Type Is CSS.IdentifierNameSyntax AndAlso DirectCast(t.Type, CSS.IdentifierNameSyntax).Identifier.ValueText = "IDisposable" Then
                                Dim InterfaceMembers As VBS.QualifiedNameSyntax = VBFactory.QualifiedName(
                                                                                VBFactory.IdentifierName("IDisposable"),
                                                                                VBFactory.IdentifierName("Dispose")
                                                                                )
                                If ParameterList Is Nothing OrElse ParameterList.Parameters.Any Then
                                    Exit For
                                End If
                                ImplementsClause = VBFactory.ImplementsClause(InterfaceMembers).WithTrailingTrivia(ParameterList.GetTrailingTrivia)
                                ParameterList = ParameterList.WithTrailingTrivia(SpaceTrivia)
                            End If
                        Next
                    End If
                End If
                If returnVoid Then
                    EndSubOrFunctionStatement = VBFactory.EndSubStatement
                    If node.Body IsNot Nothing Then
                        EndSubOrFunctionStatement = EndSubOrFunctionStatement.WithConvertedTriviaFrom(node.Body.CloseBraceToken)
                    ElseIf node.ExpressionBody IsNot Nothing Then
                        EndSubOrFunctionStatement = EndSubOrFunctionStatement.WithConvertedTriviaFrom(node.ExpressionBody.GetBraces.Item2)
                    End If

                    If TypeParameterList IsNot Nothing OrElse ParameterList IsNot Nothing Then
                        id = id.WithTrailingTrivia(SpaceTrivia)
                    End If
                    If Attributes.Any AndAlso Attributes.Last.GetTrailingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                        If Modifiers.Any AndAlso Modifiers(0).LeadingTrivia.ContainsEOLTrivia Then
                            If Modifiers(0).LeadingTrivia(0).IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                Modifiers(0) = Modifiers(0).WithLeadingTrivia(Modifiers(0).LeadingTrivia.RemoveAt(0))
                            End If
                        End If
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
                    RestructureAttributesAndModifiers(Attributes.Any, Modifiers.Any), VBS.MethodStatementSyntax)
                    SyncLock s_usedStacks
                        If s_usedStacks.Count > 0 Then
                            s_usedIdentifiers = DirectCast(s_usedStacks.Pop, Dictionary(Of String, SymbolTableEntry))
                        End If
                    End SyncLock
                    SubOrFunctionStatement = DirectCast(PrependStatementWithMarkedStatementTrivia(node, SubOrFunctionStatement), VBS.MethodStatementSyntax)
                    If body Is Nothing Then
                        If Modifiers.Contains(Function(t As SyntaxToken) t.IsKind(VB.SyntaxKind.PartialKeyword)) Then
                            Return VBFactory.SubBlock(SubOrFunctionStatement,
                                                      statements:=Nothing,
                                                      EndSubOrFunctionStatement)
                        End If
                        Return SubOrFunctionStatement
                    End If
                    Return VBFactory.SubBlock(SubOrFunctionStatement,
                                              body.Value,
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
                    ElseIf type.ToString.StartsWith("[", StringComparison.Ordinal) Then
                        Dim S As String() = type.ToString.Split({"["c, "]"}, StringSplitOptions.RemoveEmptyEntries)
                        If Not (IsSpecialReservedWord(S(0)) OrElse
                                VB.SyntaxFacts.IsKeywordKind(VB.SyntaxFacts.GetKeywordKind(S(0)))) Then
                            type = VBFactory.ParseTypeName(type.ToString().
                                                                   Replace("]", "", StringComparison.Ordinal).
                                                                   Replace("[", "", StringComparison.Ordinal))
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
                If AsClause.GetTrailingTrivia.ContainsCommentTrivia Then
                    FunctionStatementTrailingTrivia.InsertRange(0, AsClause.GetTrailingTrivia.ToList)
                    AsClause = AsClause.WithTrailingTrivia(SpaceTrivia)
                End If
                Dim modifierLeadingTrivia As SyntaxTriviaList = If(Modifiers.Any, Modifiers(0).LeadingTrivia, Nothing)

                Dim MovedModifierLeadingTrivia As New List(Of SyntaxTrivia)
                If Attributes.Any AndAlso Modifiers.Any AndAlso modifierLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    Dim FixedModifierLeadingTrivia As New List(Of SyntaxTrivia)

                    For triviaIndex As Integer = 0 To modifierLeadingTrivia.Count - 1
                        Dim t As SyntaxTrivia = modifierLeadingTrivia(triviaIndex)
                        Dim NextTrivia As SyntaxTrivia = If(triviaIndex < modifierLeadingTrivia.Count - 1, modifierLeadingTrivia(triviaIndex + 1), Nothing)
                        Select Case t.RawKind
                            Case VB.SyntaxKind.WhitespaceTrivia
                                If NextTrivia.IsKind(VB.SyntaxKind.CommentTrivia) Then
                                    FixedModifierLeadingTrivia.Add(SpaceTrivia)
                                    FixedModifierLeadingTrivia.Add(LineContinuation)
                                    FixedModifierLeadingTrivia.Add(t)
                                    FixedModifierLeadingTrivia.Add(NextTrivia)
                                    triviaIndex += 1
                                Else
                                    FixedModifierLeadingTrivia.Add(t)
                                End If
                            Case VB.SyntaxKind.EndOfLineTrivia
                                FixedModifierLeadingTrivia.Add(t)
                            Case VB.SyntaxKind.CommentTrivia
                                FixedModifierLeadingTrivia.Add(t)
                            Case Else
                                If t.IsDirective Then
                                    If Attributes.Any And node.AttributeLists(0).GetLeadingTrivia.ContainsDirectiveTrivia Then
                                        MovedModifierLeadingTrivia.Add(t)
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
                SubOrFunctionStatement = VBFactory.FunctionStatement(
                                            VBFactory.List(Attributes),
                                            VBFactory.TokenList(Modifiers),
                                            id,
                                            TypeParameterList,
                                            ParameterList,
                                            AsClause,
                                            handlesClause:=Nothing,
                                            implementsClause:=ImplementsClause).
                                            With(FunctionStatementLeadingTrivia, FunctionStatementTrailingTrivia)
                If ReturnAttributes.Any AndAlso
                   (Attributes.Count = 0 OrElse Attributes(0).Attributes(0).Name.ToString = "Extension") AndAlso
                   node.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    SubOrFunctionStatement = SubOrFunctionStatement.WithPrependedLeadingTrivia(ConvertTrivia(node.GetLeadingTrivia))
                End If
                SubOrFunctionStatement = DirectCast(PrependStatementWithMarkedStatementTrivia(node, SubOrFunctionStatement), VBS.MethodStatementSyntax)
                s_usedIdentifiers = DirectCast(s_usedStacks.Pop, Dictionary(Of String, SymbolTableEntry))

                If body Is Nothing Then
                    Return SubOrFunctionStatement
                End If

                Dim blockvalue As List(Of VBS.StatementSyntax) = body.Value.ToList
                If blockvalue.Any AndAlso MovedModifierLeadingTrivia.Any Then
                    blockvalue(0) = blockvalue(0).WithPrependedLeadingTrivia(MovedModifierLeadingTrivia)
                End If
                Dim blockStatements As New SyntaxList(Of VBS.StatementSyntax)
                blockStatements = blockStatements.AddRange(blockvalue)
                Return VBFactory.FunctionBlock(SubOrFunctionStatement,
                                                    blockStatements,
                                                    EndSubOrFunctionStatement)
            End Function

            Public Overrides Function VisitOperatorDeclaration(node As CSS.OperatorDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim Attributes As New List(Of VBS.AttributeListSyntax)
                Dim ReturnAttributes As SyntaxList(Of VBS.AttributeListSyntax) = Nothing
                Dim FinalTrailingDirective As New List(Of SyntaxTrivia)
                ConvertAndSplitAttributes(node.AttributeLists, Attributes, ReturnAttributes, FinalTrailingDirective)
                Dim visitor As New MethodBodyVisitor(_mSemanticModel, Me)
                Dim body As New SyntaxList(Of VBS.StatementSyntax)
                If node.Body IsNot Nothing Then
                    body = VBFactory.List(node.Body.Statements.SelectMany(Function(s As CSS.StatementSyntax) s.Accept(visitor)))
                ElseIf node.ExpressionBody IsNot Nothing Then
                    body = GetExpressionBodyStatements(node.ExpressionBody)
                Else
                    Stop
                End If
                Dim parameterList As VBS.ParameterListSyntax = DirectCast(node.ParameterList?.Accept(Me), VBS.ParameterListSyntax).WithRestructuredingEOLTrivia
                Dim Modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, IsModule, TokenContext.Member)
                Dim lSyntaxKind As CS.SyntaxKind = CS.CSharpExtensions.Kind(node.OperatorToken)
                Select Case lSyntaxKind
                    Case CS.SyntaxKind.MinusMinusToken
                        Return VBFactory.EmptyStatement.WithLeadingTrivia(node.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=False, "C# -- operator not available in VB")).WithPrependedLeadingTrivia(ConvertTrivia(node.GetLeadingTrivia)).WithConvertedTrailingTriviaFrom(node)
                    Case CS.SyntaxKind.PercentToken
                        Dim stmt As VBS.OperatorStatementSyntax = VBFactory.OperatorStatement(VBFactory.List(Attributes), VBFactory.TokenList(Modifiers), ModKeyword, parameterList, VBFactory.SimpleAsClause(ReturnAttributes, DirectCast(node.ReturnType.Accept(Me), VBS.TypeSyntax)))
                        Return VBFactory.OperatorBlock(stmt, body).WithConvertedTriviaFrom(node)
                    Case CS.SyntaxKind.PlusPlusToken
                        Return VBFactory.EmptyStatement.WithLeadingTrivia(node.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=False, "C# ++ operator not available in VB")).WithPrependedLeadingTrivia(ConvertTrivia(node.GetLeadingTrivia)).WithConvertedTrailingTriviaFrom(node)
                    Case Else
                        Dim OperatorToken As SyntaxToken = ConvertOperatorDeclarationToken(lSyntaxKind)
                        Dim stmt As VBS.OperatorStatementSyntax = VBFactory.OperatorStatement(VBFactory.List(Attributes), VBFactory.TokenList(Modifiers), OperatorToken, parameterList, VBFactory.SimpleAsClause(ReturnAttributes, DirectCast(node.ReturnType.Accept(Me), VBS.TypeSyntax)))
                        If FinalTrailingDirective.Any Then
                            stmt = stmt.WithAppendedTrailingTrivia(FinalTrailingDirective)
                        End If
                        Return VBFactory.OperatorBlock(stmt, body).WithConvertedTriviaFrom(node)
                End Select
            End Function

            Public Overrides Function VisitPropertyDeclaration(node As CSS.PropertyDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim csAccessors As New List(Of CSS.AccessorDeclarationSyntax)
                If node.AccessorList IsNot Nothing Then
                    csAccessors.AddRange(node.AccessorList.Accessors)
                End If
                Dim ImplementsClause As VBS.ImplementsClauseSyntax
                Dim interfaceMembers As SeparatedSyntaxList(Of VBS.QualifiedNameSyntax)
                Dim identifierValueText As String = node.Identifier.ValueText
                Dim SimpleName As VBS.IdentifierNameSyntax = VBFactory.IdentifierName(identifierValueText)

                Dim ExplicitInterfaceIdentifier As VBS.QualifiedNameSyntax
                Dim IdString As String = ""
                Dim TypeNode As VBS.TypeSyntax = DirectCast(node.Type.Accept(Me), VBS.TypeSyntax)
                If TypeNode.ToString.Equals("dynamic", StringComparison.OrdinalIgnoreCase) Then
                    TypeNode = VBFactory.PredefinedType(ObjectKeyword).WithTriviaFrom(TypeNode)
                End If
                Dim ClosingNodeBraces As SyntaxToken = node.GetBraces.Item2
                Dim EndPropertyStatement As VBS.EndBlockStatementSyntax = VBFactory.EndPropertyStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), PropertyKeyword).
                                WithConvertedTriviaFrom(ClosingNodeBraces).WithTrailingEOL
                Dim TypeLeadingTrivia As New List(Of SyntaxTrivia)
                TypeLeadingTrivia.AddRange(ConvertTrivia(node.Type.GetLeadingTrivia))
                If TriviaIsIdentical(VBFactory.TriviaList(ConvertTrivia(node.GetLeadingTrivia)), TypeLeadingTrivia) Then
                    TypeLeadingTrivia.Clear()
                End If
                If node.ExplicitInterfaceSpecifier IsNot Nothing Then
                    Dim VisualBasicSyntaxNode1 As VB.VisualBasicSyntaxNode = node.ExplicitInterfaceSpecifier.Accept(Me)

                    If TypeOf VisualBasicSyntaxNode1 Is VBS.QualifiedNameSyntax Then
                        ExplicitInterfaceIdentifier = DirectCast(VisualBasicSyntaxNode1, VBS.QualifiedNameSyntax)
                        IdString = ExplicitInterfaceIdentifier.Right.ToString
                        Dim OpenParenIndex As Integer = IdString.IndexOf("(", StringComparison.Ordinal)
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
                        Throw UnreachableException
                    End If
                End If

                Dim Identifier As SyntaxToken
                Dim propertyStatement As VBS.PropertyStatementSyntax
                If node.ExplicitInterfaceSpecifier Is Nothing Then
                    Identifier = GenerateSafeVBToken(node.Identifier)
                    Dim PropertySymbol As ISymbol = _mSemanticModel.GetDeclaredSymbol(node)
                    ImplementsClause = s_implementedMembers.GetImplementsClauseForProperty(CType(PropertySymbol, IPropertySymbol))
                Else
                    Identifier = VBFactory.Identifier($"{IdString}_{identifierValueText}")
                    ImplementsClause = VBFactory.ImplementsClause(interfaceMembers)
                End If
                Identifier = Identifier.WithTrailingTrivia(SpaceTrivia)
                Dim Attributes As New List(Of VBS.AttributeListSyntax)
                Dim ReturnAttributes As SyntaxList(Of VBS.AttributeListSyntax) = Nothing
                Dim FinalTrailingDirective As New List(Of SyntaxTrivia)
                ConvertAndSplitAttributes(node.AttributeLists, Attributes, ReturnAttributes, FinalTrailingDirective)
                If FinalTrailingDirective.Any Then
                    Stop
                End If
                Dim isIterator As Boolean = False
                Dim accessors As New List(Of VBS.AccessorBlockSyntax)
                Dim Statements As SyntaxList(Of VBS.StatementSyntax)
                Dim CSharpModifiers As SyntaxTokenList = node.Modifiers
                Dim AsClause As VBS.SimpleAsClauseSyntax = VBFactory.SimpleAsClause(ReturnAttributes, TypeNode.WithoutTrivia)
                Dim Context As TokenContext = TokenContext.Property
                Dim LocalIsModule As Boolean = IsModule OrElse node.Parent.IsKind(CS.SyntaxKind.CompilationUnit)
                If node.ExpressionBody IsNot Nothing Then
                    Dim ExpressionSyntaxNode As VB.VisualBasicSyntaxNode = node.ExpressionBody.Expression.Accept(Me).WithConvertedLeadingTriviaFrom(node.ExpressionBody.Expression)
                    If TypeOf ExpressionSyntaxNode Is VBS.ThrowStatementSyntax Then
                        Statements = VBFactory.SingletonList(Of VBS.StatementSyntax)(DirectCast(ExpressionSyntaxNode, VBS.ThrowStatementSyntax).WithConvertedTriviaFrom(node.ExpressionBody))
                    ElseIf TypeOf ExpressionSyntaxNode Is VBS.SingleLineIfStatementSyntax Then
                        Dim IfStatement As VBS.SingleLineIfStatementSyntax = DirectCast(ExpressionSyntaxNode, VBS.SingleLineIfStatementSyntax).WithTrailingEOL
                        Dim ReturnStatement As VBS.ReturnStatementSyntax = VBFactory.ReturnStatement(DirectCast(IfStatement.Condition, VBS.BinaryExpressionSyntax).Left).
                                                WithLeadingTrivia(IfStatement.Condition.GetLeadingTrivia)
                        ReturnStatement = ReturnStatement.RelocateDirectivesInLeadingTrivia
                        Dim StatementList As New List(Of VBS.StatementSyntax) From {
                            IfStatement,
                            ReturnStatement
                        }
                        Statements = ReplaceStatementsWithMarkedStatements(node, VBFactory.List(StatementList))
                    ElseIf TypeOf ExpressionSyntaxNode Is VBS.AssignmentStatementSyntax Then
                        Dim AssignmentStatement As VBS.AssignmentStatementSyntax = DirectCast(ExpressionSyntaxNode, VBS.AssignmentStatementSyntax).WithTrailingEOL
                        Dim ReturnStatement As VBS.ReturnStatementSyntax = VBFactory.ReturnStatement(AssignmentStatement.Left.WithoutLeadingTrivia).
                                                WithLeadingTrivia(AssignmentStatement.GetLeadingTrivia)
                        ReturnStatement = ReturnStatement.RelocateDirectivesInLeadingTrivia
                        Dim StatementList As New List(Of VBS.StatementSyntax) From {
                            AssignmentStatement,
                            ReturnStatement
                        }
                        Statements = ReplaceStatementsWithMarkedStatements(node, VBFactory.List(StatementList))
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

                        Dim ReturnStatement As VBS.ReturnStatementSyntax = VBFactory.ReturnStatement(ReturnedExpression.WithLeadingTrivia(SpaceTrivia)).
                                                WithLeadingTrivia(ReturnedExpression.GetLeadingTrivia)
                        ReturnStatement = ReturnStatement.RelocateDirectivesInLeadingTrivia
                        Statements = ReplaceOneStatementWithMarkedStatements(node.ExpressionBody, ReturnStatement)
                    Else
                        Stop
                        Throw UnreachableException
                    End If
                    accessors.Add(VBFactory.AccessorBlock(VB.SyntaxKind.GetAccessorBlock, VBFactory.GetAccessorStatement.WithTrailingEOL, Statements, VBFactory.EndGetStatement()))
                Else
                    If node.AccessorList IsNot Nothing Then
                        If (Not node.Modifiers.Contains(CS.SyntaxKind.AbstractKeyword)) AndAlso csAccessors.Count = 2 AndAlso
                            csAccessors(0).Body Is Nothing AndAlso csAccessors(0).ExpressionBody Is Nothing AndAlso
                            csAccessors(1).Body Is Nothing AndAlso csAccessors(1).ExpressionBody Is Nothing Then
                            Dim GetModifiers As List(Of SyntaxToken) = ConvertModifiers(csAccessors(0).Modifiers, LocalIsModule, Context)
                            Dim SetModifiers As List(Of SyntaxToken) = ConvertModifiers(csAccessors(1).Modifiers, LocalIsModule, Context)
                            Dim propertyStatementLeadingTrivia As New List(Of SyntaxTrivia)
                            Dim GetModifier0 As String = If(GetModifiers.Any, GetModifiers(0).ValueText, "")
                            Dim SetModifier0 As String = If(SetModifiers.Any, SetModifiers(0).ValueText, "")
                            If GetModifier0 <> SetModifier0 Then
                                ' Handle
                                ' public string BuyerId { get; protected set; }
                                ' Dim _buyerId As String
                                If GetModifiers.Any AndAlso GetModifiers(0).LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                    propertyStatementLeadingTrivia.AddRange(GetModifiers(0).LeadingTrivia)
                                    GetModifiers(0) = GetModifiers(0).WithoutTrivia
                                End If
                                If SetModifiers.Any AndAlso SetModifiers?(0).LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                    propertyStatementLeadingTrivia.AddRange(SetModifiers(0).LeadingTrivia)
                                    SetModifiers(0) = SetModifiers(0).WithoutTrivia
                                End If
                                Dim newVariableToken As SyntaxToken = VBFactory.Identifier($"_{identifierValueText.Substring(0, 1).ToLowerInvariant}{identifierValueText.Substring(1)}")
                                Dim NewModifiedIdentifier As VBS.ModifiedIdentifierSyntax = VBFactory.ModifiedIdentifier(newVariableToken)
                                Dim declarators As SeparatedSyntaxList(Of VBS.VariableDeclaratorSyntax) =
                                                        VBFactory.SingletonSeparatedList(
                                                            VBFactory.VariableDeclarator(
                                                                VBFactory.SingletonSeparatedList(NewModifiedIdentifier),
                                                                AsClause,
                                                                initializer:=Nothing)
                                                        )
                                Dim DimStatement As VBS.StatementSyntax = VBFactory.LocalDeclarationStatement(VBFactory.TokenList(PrivateKeyword), declarators).WithConvertedLeadingTriviaFrom(node).WithTrailingEOL
                                Dim StatementWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                                StatementWithIssues.AddMarker(DimStatement, StatementHandlingOption.PrependStatement, AllowDuplicates:=False)
                                Dim stmt As VBS.AccessorStatementSyntax = VBFactory.GetAccessorStatement(Nothing, VBFactory.TokenList(GetModifiers), parameterList:=Nothing)
                                Dim body As SyntaxList(Of VBS.StatementSyntax) = VBFactory.SingletonList(Of VBS.StatementSyntax)(VBFactory.ReturnStatement(VBFactory.IdentifierName(newVariableToken)).WithTrailingEOL)
                                accessors.Add(VBFactory.AccessorBlock(VB.SyntaxKind.GetAccessorBlock, stmt.WithTrailingEOL, body, VBFactory.EndGetStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), GetKeyword).WithTrailingEOL))

                                Dim ValueParam As VBS.ParameterSyntax = VBFactory.Parameter(ValueModifiedIdentifier).WithAsClause(AsClause)
                                stmt = VBFactory.SetAccessorStatement(Nothing, VBFactory.TokenList(SetModifiers), VBFactory.ParameterList(VBFactory.SingletonSeparatedList(ValueParam)))
                                Dim valueExpression As VBS.ExpressionSyntax = VBFactory.ParseExpression(newVariableToken.ValueText)
                                body = VBFactory.SingletonList(Of VBS.StatementSyntax)(VBFactory.SimpleAssignmentStatement(valueExpression, VBFactory.IdentifierName(ValueModifiedIdentifier.Identifier)).WithTrailingEOL)
                                accessors.Add(VBFactory.AccessorBlock(VB.SyntaxKind.SetAccessorBlock, stmt.WithTrailingEOL, body, VBFactory.EndSetStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), SetKeyword).WithTrailingEOL))

                                propertyStatement = VBFactory.PropertyStatement(VBFactory.List(Attributes), VBFactory.TokenList(ConvertModifiers(CSharpModifiers, LocalIsModule, Context)), Identifier, parameterList:=Nothing, AsClause, initializer:=Nothing, implementsClause:=Nothing)
                                Return VBFactory.PropertyBlock(propertyStatement.WithPrependedLeadingTrivia(propertyStatementLeadingTrivia).WithTrailingEOL, VBFactory.List(accessors), EndPropertyStatement.WithConvertedTrailingTriviaFrom(node)).
                                        RestructureAttributesAndModifiers(Attributes.Any, HasModifiers:=True)

                            End If

                        End If
                        For Each a As CSS.AccessorDeclarationSyntax In csAccessors
                            Dim _isIterator As Boolean
                            accessors.Add(ConvertAccessor(a, LocalIsModule, _isIterator))
                            isIterator = isIterator Or _isIterator
                        Next
                    End If
                End If

                Dim IsWriteOnly As Boolean = False
                If node.AccessorList IsNot Nothing AndAlso csAccessors.Count = 1 Then
                    Select Case csAccessors(0).Keyword.RawKind
                        Case CS.SyntaxKind.GetKeyword
                            CSharpModifiers = CSharpModifiers.Add(CSharpSyntaxFactory.ReadOnlyKeyword)
                        Case CS.SyntaxKind.SetKeyword
                            IsWriteOnly = True
                        Case Else
                            Throw UnreachableException
                    End Select

                End If
                ' TODO find better way to find out if we are in interface
                If node.IsParentKind(CS.SyntaxKind.InterfaceDeclaration) Then
                    Context = TokenContext.InterfaceOrModule
                End If
                Dim Modifiers As List(Of SyntaxToken) = ConvertModifiers(CSharpModifiers, LocalIsModule, Context)
                If isIterator Then
                    Modifiers.Add(IteratorKeyword)
                End If
                If IsWriteOnly Then
                    Modifiers.Add(WriteOnlyKeyword)
                End If

                If node.AccessorList Is Nothing Then
                    Modifiers.Add(VisualBasicSyntaxFactory.ReadOnlyKeyword)
                End If

                Dim Initializer As VBS.EqualsValueSyntax = If(node.Initializer Is Nothing, Nothing, VBFactory.EqualsValue(DirectCast(node.Initializer.Value.Accept(Me), VBS.ExpressionSyntax)))
                Dim Keyword As SyntaxToken

                If Modifiers.Count = 0 Then
                    Keyword = PropertyKeyword.WithLeadingTrivia(TypeLeadingTrivia)
                    TypeLeadingTrivia.Clear()
                Else
                    Keyword = PropertyKeyword
                    If TypeLeadingTrivia.Any Then
                        TypeLeadingTrivia.Insert(0, VBEOLTrivia)
                    End If
                End If
                Dim PrependedTrivia As List(Of SyntaxTrivia) = DedupLeadingTrivia(node, Keyword, Attributes, Modifiers)
                propertyStatement = VBFactory.PropertyStatement(VBFactory.List(Attributes),
                                                                                                VBFactory.TokenList(Modifiers),
                                                                                                Keyword,
                                                                                                Identifier.WithTrailingTrivia(SpaceTrivia),
                                                                                                parameterList:=Nothing,
                                                                                                AsClause,
                                                                                                Initializer,
                                                                                                ImplementsClause
                                                                                                ).WithPrependedLeadingTrivia(PrependedTrivia).WithTrailingEOL
                Dim StmtList As SyntaxList(Of VBS.StatementSyntax) = ReplaceOneStatementWithMarkedStatements(node, propertyStatement)
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
                    Return VBFactory.PropertyBlock(propertyStatement.WithLeadingTrivia(AddedLeadingTrivia).WithTrailingEOL, VBFactory.List(accessors))
                End If
                Dim AccessorOpenBraceTrivia As New List(Of SyntaxTrivia)
                Dim AccessorOpenBrace As SyntaxToken = node.AccessorList.GetBraces.Item1
                AccessorOpenBraceTrivia.AddRange(ConvertTrivia(AccessorOpenBrace.LeadingTrivia))
                AccessorOpenBraceTrivia.AddRange(ConvertTrivia(AccessorOpenBrace.TrailingTrivia))

                Dim AccessorClosingBraceTrivia As New List(Of SyntaxTrivia)
                Dim AccessorClosingBrace As SyntaxToken = node.AccessorList.GetBraces.Item2
                AccessorClosingBraceTrivia.AddRange(ConvertTrivia(AccessorClosingBrace.LeadingTrivia))
                AccessorClosingBraceTrivia.AddRange(ConvertTrivia(AccessorClosingBrace.TrailingTrivia))

                EndPropertyStatement = VBFactory.EndPropertyStatement.
                                WithConvertedTriviaFrom(ClosingNodeBraces).
                                WithPrependedLeadingTrivia(AccessorClosingBraceTrivia)
                accessors(0) = accessors(0).WithPrependedLeadingTrivia(AccessorOpenBraceTrivia).WithTrailingEOL
                Return VBFactory.PropertyBlock(propertyStatement.WithTrailingEOL, VBFactory.List(accessors), EndPropertyStatement).
                                        WithAppendedTrailingTrivia(TypeLeadingTrivia).
                                        RestructureAttributesAndModifiers(Attributes.Any, Modifiers.Any)

            End Function

        End Class

    End Class

End Namespace
