' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Text
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax
Imports ProgressReportLibrary
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic

Namespace CSharpToVBConverter.ToVisualBasic

    Partial Public Class CSharpConverter

        Partial Friend Class NodesVisitor
            Inherits CS.CSharpSyntaxVisitor(Of VB.VisualBasicSyntaxNode)

            Private Shared Function ConvertConditionalAccessExpressionToComment(node As CSS.ConditionalAccessExpressionSyntax) As SyntaxTriviaList
                Dim LeadingTriviaList As SyntaxTriviaList
                Dim stringList As String() = node.ToFullString.SplitLines
                LeadingTriviaList = LeadingTriviaList.Add(VBEOLTrivia)
                LeadingTriviaList = LeadingTriviaList.Add(Factory.CommentTrivia($"' TODO VB does not allow directives here, original statement"))
                For Each s As String In stringList
                    LeadingTriviaList = LeadingTriviaList.Add(VBEOLTrivia)
                    LeadingTriviaList = LeadingTriviaList.Add(Factory.CommentTrivia($"'    {s}"))
                Next
                Return LeadingTriviaList
            End Function

            Private Shared Function GetTypeSyntaxFromPossibleAddressOf(vbSyntaxNode As VB.VisualBasicSyntaxNode) As TypeSyntax
                Dim addressOfExpr As UnaryExpressionSyntax = TryCast(vbSyntaxNode, UnaryExpressionSyntax)
                If addressOfExpr IsNot Nothing Then
                    vbSyntaxNode = addressOfExpr.Operand
                End If
                Return CType(vbSyntaxNode, TypeSyntax)
            End Function

            Private Shared Function MemberAccess(ParamArray nameParts() As String) As MemberAccessExpressionSyntax
                Dim lhs As MemberAccessExpressionSyntax = Nothing
                For Each namePart As String In nameParts.Skip(1)
                    lhs = Factory.MemberAccessExpression(VB.SyntaxKind.SimpleMemberAccessExpression, If(lhs, CType(Factory.IdentifierName(nameParts(0)), ExpressionSyntax)), Factory.Token(VB.SyntaxKind.DotToken), Factory.IdentifierName(namePart))
                Next
                Return lhs
            End Function

            Private Shared Function RestructureMemberAccessExpressionTrivia(node As CSS.MemberAccessExpressionSyntax, TriviaList As SyntaxTriviaList, FoundEOL As Boolean, ByRef OperatorTrailingTrivia As SyntaxTriviaList) As Boolean
                For Each Trivia As SyntaxTrivia In TriviaList
                    Select Case Trivia.RawKind
                        Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                            OperatorTrailingTrivia = OperatorTrailingTrivia.Add(Trivia)
                            FoundEOL = True
                        Case VB.SyntaxKind.EndOfLineTrivia
                            FoundEOL = True
                        Case VB.SyntaxKind.WhitespaceTrivia
                            OperatorTrailingTrivia = OperatorTrailingTrivia.Add(Factory.Space)
                        Case VB.SyntaxKind.DisableWarningDirectiveTrivia,
                             VB.SyntaxKind.EnableWarningDirectiveTrivia
                            ' Ignore
                        Case VB.SyntaxKind.IfDirectiveTrivia
                            Dim StatementWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                            StatementWithIssues.AddMarker(Factory.EmptyStatement.WithLeadingTrivia(Trivia), StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                        Case Else
                            Stop
                    End Select
                Next

                Return FoundEOL
            End Function

            Private Function ConvertLambdaExpression(node As CSS.AnonymousFunctionExpressionSyntax, block As Object, parameters As SeparatedSyntaxList(Of CSS.ParameterSyntax), Modifiers As SyntaxTokenList) As LambdaExpressionSyntax
                Dim vbNodes As New List(Of ParameterSyntax)
                Dim vbSeparators As New List(Of SyntaxToken)
                If parameters.Any Then
                    Dim csSeparators As New List(Of SyntaxToken)
                    csSeparators.AddRange(parameters.GetSeparators)

                    For index As Integer = 0 To parameters.SeparatorCount - 1
                        vbNodes.Add(DirectCast(parameters(index).Accept(Me), ParameterSyntax))
                        vbSeparators.Add(CommaToken.WithConvertedTriviaFrom(csSeparators(index)))
                    Next
                    vbNodes.Add(DirectCast(parameters.Last.Accept(Me), ParameterSyntax))
                End If

                Dim isErrorType As Boolean = True
                Dim lambdaHeader As LambdaHeaderSyntax
                Dim returnsVoid As Boolean = True
                Dim symbol As IMethodSymbol = Nothing
                Try
                    Dim _symbolInfo As SymbolInfo = _mSemanticModel.GetSymbolInfo(node)
                    symbol = TryCast(_symbolInfo.Symbol, IMethodSymbol)
                    returnsVoid = symbol.ReturnsVoid
                    If Not returnsVoid Then
                        isErrorType = Not symbol.ReturnType.IsType
                    End If
                Catch ex As OperationCanceledException
                    Throw
                Catch ex As Exception
                    Stop
                    Throw
                End Try
                Dim isFunction As Boolean = Not (returnsVoid OrElse TypeOf node.Body Is CSS.AssignmentExpressionSyntax)
                Dim modifiersList As SyntaxTokenList
                modifiersList = modifiersList.AddRange(ConvertModifiers(Modifiers, Me.IsModule, TokenContext.Local))
                Dim endSubOrFunctionStatement As EndBlockStatementSyntax
                Dim parameterList As ParameterListSyntax = Factory.ParameterList(Factory.SeparatedList(vbNodes, vbSeparators))
                Dim csBraces As (LeftBrace As SyntaxToken, RightBrace As SyntaxToken) = node.Body.GetBraces
                Dim asClause As AsClauseSyntax = Nothing
                If isFunction Then
                    Dim dontAddAsClause As Boolean = isErrorType OrElse symbol.ReturnType.ToString.Contains("?", StringComparison.Ordinal) OrElse symbol.ReturnType.ToString.StartsWith("<anonymous type: ", StringComparison.Ordinal)
                    asClause = If(dontAddAsClause,
                                    Nothing,
                                    Factory.SimpleAsClause(AsKeyword.With(Factory.Space, Factory.Space), New SyntaxList(Of AttributeListSyntax), symbol.ReturnType.ConvertToType)
                                    )
                    lambdaHeader = Factory.FunctionLambdaHeader(Factory.List(Of AttributeListSyntax)(), Factory.TokenList(modifiersList), parameterList, asClause:=CType(asClause, SimpleAsClauseSyntax))
                    endSubOrFunctionStatement = Factory.EndFunctionStatement(EndKeyword.WithTrailingTrivia(Factory.Space), FunctionKeyword).WithConvertedTriviaFrom(csBraces.RightBrace)
                Else
                    lambdaHeader = Factory.SubLambdaHeader(Factory.List(Of AttributeListSyntax)(), Factory.TokenList(modifiersList), parameterList, asClause:=Nothing)
                    endSubOrFunctionStatement = Factory.EndSubStatement(EndKeyword.WithTrailingTrivia(Factory.Space), SubKeyword).WithConvertedTriviaFrom(csBraces.RightBrace)
                End If
                If TypeOf block Is CSS.BlockSyntax Then
                    block = DirectCast(block, CSS.BlockSyntax).Statements
                End If

                Dim statements As New SyntaxList(Of StatementSyntax)
                Dim endBlock As EndBlockStatementSyntax
                If TypeOf block Is CS.CSharpSyntaxNode Then
                    Dim body As VB.VisualBasicSyntaxNode = DirectCast(block, CS.CSharpSyntaxNode).Accept(Me)
                    If TypeOf block Is CSS.ThrowExpressionSyntax Then
                        statements = Factory.SingletonList(DirectCast(body, StatementSyntax).WithTrailingEOL)
                        If isFunction Then
                            Return Factory.MultiLineLambdaExpression(VB.SyntaxKind.MultiLineFunctionLambdaExpression, lambdaHeader.WithTrailingEOL, statements, endSubOrFunctionStatement).WithConvertedTriviaFrom(node)
                        End If
                        Return Factory.MultiLineLambdaExpression(VB.SyntaxKind.MultiLineSubLambdaExpression, lambdaHeader.WithTrailingEOL, statements, endSubOrFunctionStatement).WithConvertedTriviaFrom(node)
                    End If
                    If TypeOf block Is CSS.ObjectCreationExpressionSyntax Then
                        If isFunction Then
                            statements = Factory.SingletonList(Of StatementSyntax)(Factory.ReturnStatement(DirectCast(body, NewExpressionSyntax)).WithTrailingEOL)
                            Return Factory.MultiLineLambdaExpression(VB.SyntaxKind.MultiLineFunctionLambdaExpression, lambdaHeader.WithTrailingEOL, statements, endSubOrFunctionStatement).WithConvertedTriviaFrom(node)
                        End If
                        Dim names As SeparatedSyntaxList(Of ModifiedIdentifierSyntax) = Factory.SingletonSeparatedList(Factory.ModifiedIdentifier("DoNotCare"))
                        asClause = Factory.AsNewClause(DirectCast(body, NewExpressionSyntax))
                        statements = Factory.SingletonList(Of StatementSyntax)(FactoryDimStatement("DoNotCare", asClause, initializer:=Nothing))
                        Return Factory.MultiLineLambdaExpression(VB.SyntaxKind.MultiLineSubLambdaExpression, lambdaHeader.WithTrailingEOL, statements, endSubOrFunctionStatement).WithConvertedTriviaFrom(node)
                    End If
                    If body.IsKind(VB.SyntaxKind.SimpleAssignmentStatement) Then
                        Dim simpleAssignment As AssignmentStatementSyntax = DirectCast(body, AssignmentStatementSyntax)
                        If simpleAssignment.Left.IsKind(VB.SyntaxKind.SimpleMemberAccessExpression) Then
                            Dim memberAccessExpression As MemberAccessExpressionSyntax = DirectCast(simpleAssignment.Left, MemberAccessExpressionSyntax)
                            Select Case memberAccessExpression.Expression.Kind
                                Case VB.SyntaxKind.ObjectCreationExpression, VB.SyntaxKind.SimpleMemberAccessExpression
                                    endBlock = Factory.EndSubStatement(EndKeyword.WithTrailingTrivia(Factory.Space), SubKeyword).WithTrailingEOL
                                    Dim uniqueName As String = node.GetUniqueVariableNameInScope("tempVar", _mSemanticModel)
                                    Dim nameToken As SyntaxToken = Factory.Identifier(uniqueName)
                                    Dim uniqueIdentifier As IdentifierNameSyntax = Factory.IdentifierName(nameToken)
                                    Dim dimStatement As LocalDeclarationStatementSyntax

                                    If TypeOf memberAccessExpression.Expression Is NewExpressionSyntax Then
                                        asClause = Factory.AsNewClause(DirectCast(memberAccessExpression.Expression, NewExpressionSyntax))
                                        dimStatement = FactoryDimStatement(nameToken, asClause, initializer:=Nothing)
                                    ElseIf TypeOf memberAccessExpression.Expression Is MemberAccessExpressionSyntax Then
                                        Dim memberAccess As MemberAccessExpressionSyntax = DirectCast(memberAccessExpression.Expression, MemberAccessExpressionSyntax)
                                        If TypeOf memberAccess.Expression IsNot NewExpressionSyntax Then
                                            Exit Select
                                        End If
                                        asClause = Factory.AsNewClause(DirectCast(memberAccess.Expression, NewExpressionSyntax))
                                        dimStatement = FactoryDimStatement(nameToken, asClause, initializer:=Nothing)
                                    Else
                                        Exit Select
                                    End If
                                    statements = statements.Add(dimStatement)
                                    statements = statements.Add(Factory.SimpleAssignmentStatement(Factory.QualifiedName(uniqueIdentifier, memberAccessExpression.Name), simpleAssignment.Right).WithTrailingEOL)
                                    Return Factory.MultiLineLambdaExpression(VB.SyntaxKind.MultiLineSubLambdaExpression, lambdaHeader.WithTrailingEOL, statements, endBlock).WithConvertedTriviaFrom(node)
                                Case VB.SyntaxKind.IdentifierName, VB.SyntaxKind.InvocationExpression, VB.SyntaxKind.MeExpression
                                    ' handled below
                                Case Else
                                    Stop
                            End Select
                        End If
                    End If
                    If body.ToFullString.Contains(vbCrLf, StringComparison.OrdinalIgnoreCase) Then
                        Dim syntaxList As SyntaxList(Of StatementSyntax)
                        If TypeOf body Is StatementSyntax Then
                            syntaxList = Factory.SingletonList(CType(body, StatementSyntax))
                        Else
                            syntaxList = Factory.SingletonList(Of StatementSyntax)(Factory.ReturnStatement(CType(body, ExpressionSyntax)))
                        End If
                        If isFunction Then
                            Return Factory.MultiLineLambdaExpression(VB.SyntaxKind.MultiLineFunctionLambdaExpression, lambdaHeader.WithAsClause(Nothing), syntaxList, Factory.EndFunctionStatement(EndKeyword.WithTrailingTrivia(Factory.Space), FunctionKeyword)).WithConvertedTriviaFrom(node)
                        End If
                        Return Factory.MultiLineLambdaExpression(VB.SyntaxKind.MultiLineSubLambdaExpression, lambdaHeader.WithAsClause(Nothing), syntaxList, Factory.EndSubStatement(EndKeyword.WithTrailingTrivia(Factory.Space), SubKeyword)).WithConvertedTriviaFrom(node)
                    Else
                        If isFunction Then
                            Return Factory.SingleLineLambdaExpression(VB.SyntaxKind.SingleLineFunctionLambdaExpression, lambdaHeader.WithAsClause(Nothing), body).WithConvertedTriviaFrom(node)
                        End If
                        Return Factory.SingleLineLambdaExpression(VB.SyntaxKind.SingleLineSubLambdaExpression, lambdaHeader.WithAsClause(Nothing), body).WithConvertedTriviaFrom(node)
                    End If
                End If

                ' TypeOf block Is SyntaxList(Of CSS.StatementSyntax)
                statements = statements.AddRange(Factory.List(DirectCast(block, SyntaxList(Of CSS.StatementSyntax)).SelectMany(Function(s As CSS.StatementSyntax) s.Accept(New MethodBodyVisitor(_mSemanticModel, Me)))))
                Dim expression As ExpressionSyntax = Nothing
                If asClause Is Nothing AndAlso statements.Count = 1 AndAlso statements(0).TryUnpackExpression(expression) Then
                    Return Factory.SingleLineLambdaExpression(
                            If(isFunction,
                                VB.SyntaxKind.SingleLineFunctionLambdaExpression,
                                VB.SyntaxKind.SingleLineSubLambdaExpression),
                                lambdaHeader.WithAsClause(Nothing),
                                expression).WithConvertedTriviaFrom(node).
                                            WithPrependedLeadingTrivia(statements(0).GetLeadingTrivia
                            )
                End If

                Dim expressionKind As VB.SyntaxKind
                If isFunction Then
                    endBlock = Factory.EndFunctionStatement(EndKeyword.WithTrailingTrivia(Factory.Space), FunctionKeyword).WithConvertedTriviaFrom(csBraces.RightBrace)
                    expressionKind = VB.SyntaxKind.MultiLineFunctionLambdaExpression
                Else
                    endBlock = Factory.EndSubStatement(EndKeyword.WithTrailingTrivia(Factory.Space), SubKeyword).WithConvertedTriviaFrom(csBraces.RightBrace)
                    expressionKind = VB.SyntaxKind.MultiLineSubLambdaExpression
                End If
                Return Factory.MultiLineLambdaExpression(kind:=expressionKind,
                                                               lambdaHeader.WithTrailingEOL,
                                                               statements,
                                                               endBlock)
            End Function

            Private Function IsConcatenateStringsExpression(node As CSS.BinaryExpressionSyntax) As Boolean
                If Not node.IsKind(CS.SyntaxKind.AddExpression) Then
                    Return False
                End If
                If Me.IsStringExpression(node.Left) AndAlso Me.IsStringExpression(node.Right) Then
                    Return True
                End If
                Dim LeftNodeTypeInfo As TypeInfo
                Dim RightNodeTypeInfo As TypeInfo
                Try
                    LeftNodeTypeInfo = _mSemanticModel.GetTypeInfo(node.Left)
                    RightNodeTypeInfo = _mSemanticModel.GetTypeInfo(node.Right)
                Catch ex As OperationCanceledException
                    Throw
                Catch ex As Exception
                    Stop
                    Throw
                End Try
                Dim LeftAndRightIsString As Boolean? = LeftNodeTypeInfo.ConvertedType?.SpecialType = SystemString OrElse RightNodeTypeInfo.ConvertedType?.SpecialType = SystemString
                Return LeftAndRightIsString.HasValue AndAlso LeftAndRightIsString.Value
            End Function

            Private Function IsNameOfExpression(node As CSS.InvocationExpressionSyntax) As Boolean
                Dim isIdentifierName As Boolean = TypeOf node.Expression Is CSS.IdentifierNameSyntax
                Dim methodIdentifier As CSS.IdentifierNameSyntax = If(isIdentifierName, CType(node.Expression, CSS.IdentifierNameSyntax), Nothing)
                Return isIdentifierName AndAlso methodIdentifier?.Identifier.Text = "nameof" AndAlso _mSemanticModel.GetSymbolInfo(methodIdentifier).ExtractBestMatch(Of ISymbol)() Is Nothing
            End Function

            Private Function IsStringExpression(Node As SyntaxNode) As Boolean
                If Node.IsKind(CS.SyntaxKind.StringLiteralExpression, CS.SyntaxKind.CharacterLiteralExpression, CS.SyntaxKind.InterpolatedStringExpression) Then
                    Return True
                End If
                ' Extra to pick up more strings
                If Node.ToString.
                        TrimEnd(")"c).
                        TrimEnd("("c).
                        EndsWith("ToString", StringComparison.OrdinalIgnoreCase) Then
                    Return True
                End If
                Dim _Typeinfo As TypeInfo
                Try
                    If TypeOf Node Is CSS.BinaryExpressionSyntax Then
                        Dim BinaryExpression As CSS.BinaryExpressionSyntax = CType(Node, CSS.BinaryExpressionSyntax)
                        If Not BinaryExpression.IsKind(CS.SyntaxKind.AddExpression) Then
                            Return False
                        End If
                        _Typeinfo = _mSemanticModel.GetTypeInfo(BinaryExpression)
                        If _Typeinfo.IsString Then
                            Return True
                        End If
                        _Typeinfo = _mSemanticModel.GetTypeInfo(BinaryExpression.Left)
                        If _Typeinfo.IsString Then
                            Return True
                        End If
                        _Typeinfo = _mSemanticModel.GetTypeInfo(BinaryExpression.Right)
                    ElseIf TypeOf Node Is CSS.MemberAccessExpressionSyntax Then
                        _Typeinfo = _mSemanticModel.GetTypeInfo(CType(Node, CSS.MemberAccessExpressionSyntax).Expression)
                    Else
                        _Typeinfo = _mSemanticModel.GetTypeInfo(Node)
                    End If
                Catch ex As Exception
                    Stop
                    Throw
                End Try
                Return _Typeinfo.IsString
            End Function

            Private Function MakeAssignmentStatement(node As CSS.AssignmentExpressionSyntax, LeftNode As ExpressionSyntax, RightNode As ExpressionSyntax) As StatementSyntax
                If TypeOf LeftNode Is UnaryExpressionSyntax Then
                    Dim addressOfExpr As UnaryExpressionSyntax = CType(LeftNode, UnaryExpressionSyntax)
                    LeftNode = addressOfExpr.Operand
                End If
                If CS.CSharpExtensions.Kind(node) = CS.SyntaxKind.CoalesceAssignmentExpression Then
                    Dim PossibleNullNode As ExpressionSyntax = DirectCast(node.Right.Accept(Me).WithLeadingTrivia(Factory.Space), ExpressionSyntax)
                    Dim rightBinaryExpression As BinaryConditionalExpressionSyntax = Factory.BinaryConditionalExpression(LeftNode.WithoutTrivia, PossibleNullNode)
                    Dim AssignmentStatement As AssignmentStatementSyntax = Factory.SimpleAssignmentStatement(LeftNode, rightBinaryExpression)
                    Dim newLeadingTrivia As New SyntaxTriviaList
                    If AssignmentStatement.HasLeadingTrivia Then
                        newLeadingTrivia = newLeadingTrivia.AddRange(AssignmentStatement.GetLeadingTrivia)
                        AssignmentStatement = AssignmentStatement.WithLeadingTrivia(Factory.Space)
                    End If
                    Dim AssignmentStatements As SyntaxList(Of StatementSyntax) = Factory.SingletonList(Of StatementSyntax)(AssignmentStatement)
                    Return AssignmentStatement.With(newLeadingTrivia, node.GetTrailingTrivia.ConvertTriviaList())
                End If
                Dim kind As VB.SyntaxKind = CS.CSharpExtensions.Kind(node).GetExpressionKind()
                Dim OperatorToken As SyntaxToken = GetOperatorToken(kind, IsReferenceType:=False)
                If node.Right.IsKind(CS.SyntaxKind.CoalesceExpression) Then
                    Dim csRight As CSS.BinaryExpressionSyntax = DirectCast(node.Right, CSS.BinaryExpressionSyntax)
                    If csRight.Right.IsKind(CS.SyntaxKind.ThrowExpression) Then
                        Dim TestNode As ExpressionSyntax = RightNode.WithLeadingTrivia(Factory.Space)
                        Dim SecondExpression As ThrowStatementSyntax = DirectCast(csRight.Right.Accept(Me).WithConvertedTriviaFrom(csRight.Right), ThrowStatementSyntax).WithTrailingEOL
                        Dim Statements As SyntaxList(Of StatementSyntax) = Factory.SingletonList(Of StatementSyntax)(SecondExpression)

                        Dim testTrailingTrivia As SyntaxTriviaList = TestNode.GetTrailingTrivia
                        Select Case testTrailingTrivia.Count
                            Case 0
                                testTrailingTrivia = testTrailingTrivia.Add(Factory.Space)
                            Case 1
                                Select Case testTrailingTrivia(0).RawKind
                                    Case VB.SyntaxKind.WhitespaceTrivia
                                    Case VB.SyntaxKind.EndOfLineTrivia
                                        testTrailingTrivia = testTrailingTrivia.InsertRange(0, {Factory.Space, LineContinuation, Factory.Space})
                                    Case Else
                                        Stop
                                End Select
                            Case Else
                        End Select
                        TestNode = TestNode.WithTrailingTrivia(testTrailingTrivia)
                        Dim Condition As ExpressionSyntax = Factory.IsExpression(TestNode, NothingExpression)
                        Dim IfBlock As SingleLineIfStatementSyntax =
                                            Factory.SingleLineIfStatement(Condition,
                                                                          Statements,
                                                                          elseClause:=Nothing
                                                                         )
                        Dim StatementWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                        StatementWithIssues.AddMarker(IfBlock, StatementHandlingOption.PrependStatement, AllowDuplicates:=False)
                        RightNode = DirectCast(csRight.Left.Accept(Me), ExpressionSyntax)
                    Else
                        RightNode = DirectCast(node.Right.Accept(Me), ExpressionSyntax)
                    End If
                Else
                    If RightNode Is Nothing Then
                        RightNode = Factory.IdentifierName("HandleRefExpression")
                    End If
                End If
                If node.IsKind(CS.SyntaxKind.AndAssignmentExpression,
                               CS.SyntaxKind.OrAssignmentExpression,
                               CS.SyntaxKind.ExclusiveOrAssignmentExpression,
                               CS.SyntaxKind.ModuloAssignmentExpression) Then
                    Return Factory.SimpleAssignmentStatement(LeftNode, Factory.BinaryExpression(kind, LeftNode.WithoutTrivia, OperatorToken, RightNode.WithoutTrivia))
                End If
                If kind = VB.SyntaxKind.AddAssignmentStatement AndAlso
                    RightNode.IsKind(VB.SyntaxKind.ObjectCreationExpression) Then
                    Dim RightNodeObjectCreation As ObjectCreationExpressionSyntax = DirectCast(RightNode, ObjectCreationExpressionSyntax)
                    If RightNodeObjectCreation.ArgumentList.Arguments.Count = 1 AndAlso
                        RightNodeObjectCreation.ArgumentList.Arguments(0).IsKind(VB.SyntaxKind.SimpleArgument) Then
                        If DirectCast(RightNodeObjectCreation.ArgumentList.Arguments(0), SimpleArgumentSyntax).Expression.IsKind(VB.SyntaxKind.AddressOfExpression) Then
                            Return Factory.AddHandlerStatement(LeftNode.WithLeadingTrivia(Factory.Space), RightNode).WithLeadingTrivia(LeftNode.GetLeadingTrivia)
                        End If
                    End If
                End If
                Return Factory.AssignmentStatement(kind,
                                                   LeftNode.AdjustNodeTrivia(SeparatorFollows:=True),
                                                   GetOperatorToken(kind, IsReferenceType:=False),
                                                   RightNode)
            End Function

            Private Sub MarkPatchInlineAssignHelper(node As CS.CSharpSyntaxNode)
                Dim parentDefinition As CSS.BaseTypeDeclarationSyntax = node.AncestorsAndSelf().OfType(Of CSS.BaseTypeDeclarationSyntax).FirstOrDefault()
                InlineAssignHelperMarkers.Add(parentDefinition)
            End Sub

            Private Function ReduceArrayUpperBoundExpression(expr As CSS.ExpressionSyntax) As ExpressionSyntax
                Dim constant As [Optional](Of Object) = _mSemanticModel.GetConstantValue(expr)
                If constant.HasValue AndAlso TypeOf constant.Value Is Integer Then
                    Return Factory.NumericLiteralExpression(Factory.Literal(CInt(constant.Value) - 1))
                End If
                Return Factory.BinaryExpression(kind:=VB.SyntaxKind.SubtractExpression, left:=DirectCast(expr.Accept(Me), ExpressionSyntax), operatorToken:=MinusToken, right:=ExpressionD1)
            End Function

            Private Function TryCreateRaiseEventStatement(invokedCsExpression As CSS.ExpressionSyntax, argumentListSyntax As CSS.ArgumentListSyntax, ByRef visitInvocationExpression As VB.VisualBasicSyntaxNode) As Boolean
                Dim isInvoked As Boolean = TypeOf invokedCsExpression Is CSS.MemberAccessExpressionSyntax
                Dim csMemberAccess As CSS.MemberAccessExpressionSyntax = If(isInvoked, CType(invokedCsExpression, CSS.MemberAccessExpressionSyntax), Nothing)
                If isInvoked AndAlso IsInvokeIdentifier(csMemberAccess.Name) Then
                    invokedCsExpression = csMemberAccess.Expression
                End If

                If _commonConversions.IsEventHandlerIdentifier(invokedCsExpression) Then
                    Dim expressionSyntax As ExpressionSyntax = CType(invokedCsExpression.Accept(Me), ExpressionSyntax)
                    Dim identifierName As IdentifierNameSyntax = Me.GetIdentifierNameFromName(expressionSyntax)
                    Dim argumentList As ArgumentListSyntax = CType(argumentListSyntax.Accept(Me), ArgumentListSyntax)
                    visitInvocationExpression = Factory.RaiseEventStatement(identifierName, argumentList)
                    Return True
                End If

                visitInvocationExpression = Nothing
                Return False
            End Function

            Public Overrides Function VisitAnonymousMethodExpression(node As CSS.AnonymousMethodExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim Parameters As New SeparatedSyntaxList(Of CSS.ParameterSyntax)
                If node.ParameterList IsNot Nothing Then
                    Parameters = CType(node.ParameterList?.Parameters, SeparatedSyntaxList(Of CSS.ParameterSyntax))
                End If
                Return Me.ConvertLambdaExpression(node, block:=node.Block.Statements, Parameters, Modifiers:=Factory.TokenList(node.AsyncKeyword)).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitAnonymousObjectCreationExpression(node As CSS.AnonymousObjectCreationExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim FieldInitializers As New List(Of FieldInitializerSyntax)
                For Each e As IndexClass(Of CSS.AnonymousObjectMemberDeclaratorSyntax) In node.Initializers.WithIndex
                    Dim Initializer As CSS.AnonymousObjectMemberDeclaratorSyntax = e.Value
                    Dim LeadingTrivia As SyntaxTriviaList = Factory.TriviaList(Initializer.GetLeadingTrivia.ConvertTriviaList())
                    Dim removeLeadingTrivia As Boolean = False
                    If LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        If Not LeadingTrivia.ContainsEndIfTrivia Then
                            LeadingTrivia = LeadingTrivia.Insert(0, Factory.CommentTrivia(" ' TODO: Comment moved from middle of expression to end, check for correct placement"))
                        End If
                        Dim Statement As EmptyStatementSyntax = Factory.EmptyStatement.WithLeadingTrivia(LeadingTrivia)
                        GetStatementwithIssues(node).AddMarker(Statement, StatementHandlingOption.AppendEmptyStatement, AllowDuplicates:=True)
                        removeLeadingTrivia = True
                    End If
                    Dim fieldInitializer As FieldInitializerSyntax = DirectCast(Initializer.Accept(Me), FieldInitializerSyntax)
                    If removeLeadingTrivia Then
                        fieldInitializer = fieldInitializer.WithoutLeadingTrivia
                    End If
                    Dim Field As FieldInitializerSyntax = fieldInitializer.NormalizeWhitespaceEx(useDefaultCasing:=True, PreserveCRLF:=True)
                    Dim FirstTrivia As Boolean = True
                    Dim FoundComment As Boolean = False
                    Dim FieldLeadingTrivia As SyntaxTriviaList = Field.GetLeadingTrivia
                    Dim Comment As String = ""
                    Dim NewFieldLeadingTrivia As SyntaxTriviaList
                    For Each trivia As IndexClass(Of SyntaxTrivia) In FieldLeadingTrivia.WithIndex
                        Dim t As SyntaxTrivia = trivia.Value
                        Select Case t.RawKind
                            Case VB.SyntaxKind.WhitespaceTrivia
                                If FirstTrivia = True Then
                                    NewFieldLeadingTrivia = NewFieldLeadingTrivia.Add(t)
                                End If
                            Case VB.SyntaxKind.EndOfLineTrivia
                                If FoundComment Then
                                    If trivia.IsLast Then
                                        NewFieldLeadingTrivia = NewFieldLeadingTrivia.Add(t)
                                    End If
                                    ' skip EOL's
                                Else
                                    NewFieldLeadingTrivia = NewFieldLeadingTrivia.Add(t)
                                End If
                            Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                                If FoundComment Then
                                    Comment &= t.ToString.TrimStart.TrimStart("'"c)
                                Else
                                    FoundComment = True
                                    Comment = t.ToString
                                End If
                            Case Else
                                Stop
                        End Select
                        FirstTrivia = False
                    Next
                    If Comment.Any Then
                        NewFieldLeadingTrivia = NewFieldLeadingTrivia.Add(Factory.CommentTrivia(Comment))
                    End If
                    FieldInitializers.Add(Field.WithLeadingTrivia(NewFieldLeadingTrivia))
                Next
                If FieldInitializers.Any Then
                    Return Factory.AnonymousObjectCreationExpression(Factory.ObjectMemberInitializer(Factory.SeparatedList(FieldInitializers)))
                End If
                Return Factory.ObjectCreationExpression(Factory.PredefinedType(ObjectKeyword))
            End Function

            Public Overrides Function VisitArrayCreationExpression(node As CSS.ArrayCreationExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim upperBoundArguments As IEnumerable(Of ArgumentSyntax) = node.Type.RankSpecifiers.First()?.Sizes.Where(Function(s As CSS.ExpressionSyntax) Not (TypeOf s Is CSS.OmittedArraySizeExpressionSyntax)).Select(Function(s As CSS.ExpressionSyntax) DirectCast(Factory.SimpleArgument(Me.ReduceArrayUpperBoundExpression(s)), ArgumentSyntax))
                Dim cleanUpperBounds As New List(Of ArgumentSyntax)
                For Each Argument As ArgumentSyntax In upperBoundArguments
                    If Argument.ToString <> "-1" Then
                        cleanUpperBounds.Add(Argument)
                    End If
                Next
                upperBoundArguments = cleanUpperBounds
                Dim rankSpecifiers As IEnumerable(Of ArrayRankSpecifierSyntax) = node.Type.RankSpecifiers.Select(Function(rs As CSS.ArrayRankSpecifierSyntax) DirectCast(rs.Accept(Me), ArrayRankSpecifierSyntax))
                Dim AttributeLists As SyntaxList(Of AttributeListSyntax) = Factory.List(Of AttributeListSyntax)()
                Dim VBNode As VB.VisualBasicSyntaxNode = node.Type.ElementType.Accept(Me)
                Dim ArrayType As TypeSyntax
                If TypeOf VBNode Is TypeSyntax Then
                    ArrayType = DirectCast(VBNode, TypeSyntax)
                Else
                    Stop
                    Throw UnreachableException
                End If
                Dim ArrayBounds As ArgumentListSyntax = If(upperBoundArguments.Any(), Factory.ArgumentList(arguments:=Factory.SeparatedList(upperBoundArguments)), Nothing)
                Dim RankSpecifiersList As SyntaxList(Of ArrayRankSpecifierSyntax) = If(upperBoundArguments.Any(), Factory.List(rankSpecifiers.Skip(1)), Factory.List(rankSpecifiers))
                Dim Initializer As CollectionInitializerSyntax = If(DirectCast(node.Initializer?.Accept(Me), CollectionInitializerSyntax), Factory.CollectionInitializer())
                Return Factory.ArrayCreationExpression(
                                                    NewKeyword.WithTrailingTrivia(Factory.Space),
                                                    AttributeLists,
                                                    ArrayType,
                                                    ArrayBounds,
                                                    RankSpecifiersList,
                                                    Initializer
                                                            ).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitAssignmentExpression(node As CSS.AssignmentExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim left As ExpressionSyntax
                Dim right As ExpressionSyntax
                If node.IsReturnValueDiscarded Then
                    left = CType(node.Left.Accept(Me), ExpressionSyntax)
                    right = CType(node.Right.Accept(Me), ExpressionSyntax)
                    If _mSemanticModel.GetTypeInfo(node.Right).ConvertedType.IsDelegateType() Then
                        If _mSemanticModel.GetSymbolInfo(node.Left).Symbol?.Kind <> SymbolKind.Event Then
                            Dim kind1? As CS.SyntaxKind = node.GetAncestor(Of CSS.AccessorDeclarationSyntax)()?.Kind()
                            If kind1 IsNot Nothing AndAlso (kind1.Value = CS.SyntaxKind.AddAccessorDeclaration OrElse kind1.Value = CS.SyntaxKind.RemoveAccessorDeclaration) Then
                                Dim methodName As String = If(kind1.Value = CS.SyntaxKind.AddAccessorDeclaration, "Combine", "Remove")
                                Dim delegateMethod As MemberAccessExpressionSyntax = MemberAccess("[Delegate]", methodName)
                                Dim invokeDelegateMethod As InvocationExpressionSyntax = Factory.InvocationExpression(delegateMethod, CreateArgList({left, right}))
                                Return Factory.SimpleAssignmentStatement(left, invokeDelegateMethod)
                            End If
                        Else
                            Dim leftLeadingTrivia As SyntaxTriviaList = left.GetLeadingTrivia
                            If node.OperatorToken.IsKind(CS.SyntaxKind.PlusEqualsToken) Then
                                Return Factory.AddHandlerStatement(left.WithLeadingTrivia(Factory.Space), right).WithLeadingTrivia(left.GetLeadingTrivia)
                            End If
                            If node.OperatorToken.IsKind(CS.SyntaxKind.MinusEqualsToken) Then
                                Return Factory.RemoveHandlerStatement(left.WithLeadingTrivia(Factory.Space), right).WithLeadingTrivia(left.GetLeadingTrivia)
                            End If
                        End If

                    End If
                    Return Me.MakeAssignmentStatement(node, left, right)
                End If

                If TypeOf node.Parent Is CSS.ForStatementSyntax OrElse TypeOf node.Parent Is CSS.ParenthesizedLambdaExpressionSyntax Then
                    Return Me.MakeAssignmentStatement(node, CType(node.Left.Accept(Me), ExpressionSyntax), CType(node.Right.Accept(Me), ExpressionSyntax)).WithConvertedTriviaFrom(node)
                End If

                If TypeOf node.Parent Is CSS.InitializerExpressionSyntax Then
                    If TypeOf node.Left Is CSS.ImplicitElementAccessSyntax Then
                        Return Factory.CollectionInitializer(Factory.SeparatedList({CType(node.Left.Accept(Me), ExpressionSyntax),
                                                                                   CType(node.Right.Accept(Me), ExpressionSyntax)})).WithConvertedTriviaFrom(node)
                    End If
                    If node.Parent.IsKind(CS.SyntaxKind.ObjectInitializerExpression) Then
                        Dim nodeRight As VB.VisualBasicSyntaxNode = node.Right.Accept(Me)
                        If TypeOf nodeRight Is ObjectMemberInitializerSyntax Then
                            Dim objectMemberInitializer As ObjectMemberInitializerSyntax = DirectCast(nodeRight, ObjectMemberInitializerSyntax)
                            Dim attributeLists As SyntaxList(Of AttributeListSyntax) = Nothing
                            Dim argumentList As ArgumentListSyntax = Nothing
                            Dim toDoObjectNameGoesHere As TypeSyntax = Factory.ParseTypeName("TODO_ObjectNameGoesHere")
                            Dim objectCreationExpression As ObjectCreationExpressionSyntax = Factory.ObjectCreationExpression(NewKeyword, attributeLists, toDoObjectNameGoesHere, argumentList, objectMemberInitializer)
                            If node.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                Stop
                            End If
                            Return Factory.NamedFieldInitializer(DirectCast(CType(node.Left.Accept(Me), ExpressionSyntax),
                                                                 IdentifierNameSyntax).WithoutLeadingTrivia,
                                objectCreationExpression).WithConvertedTrailingTriviaFrom(node)
                        ElseIf TypeOf nodeRight Is CollectionInitializerSyntax Then
                            Dim collection As CollectionInitializerSyntax = CType(nodeRight, CollectionInitializerSyntax)
                            Dim simpleAssignement As CSS.AssignmentExpressionSyntax = node
                            Dim tInfo As TypeInfo = _mSemanticModel.GetTypeInfo(simpleAssignement.Left)
                            Dim initializer As ObjectCollectionInitializerSyntax = Factory.ObjectCollectionInitializer(FromKeyword, collection)
                            Dim type1 As TypeSyntax
                            If tInfo.Type Is Nothing OrElse tInfo.Type.IsErrorType Then
                                type1 = ConvertSimpleTypeToType("UnknownTypeTryConvertProject")
                            Else
                                type1 = ConvertToType(tInfo.Type).GetElementType
                            End If
                            Dim objectCreationExpression As ObjectCreationExpressionSyntax =
                                        Factory.ObjectCreationExpression(NewKeyword,
                                                                         attributeLists:=Nothing,
                                                                         type1,
                                                                         argumentList:=Nothing,
                                                                         initializer
                                                                        )
                            Return Factory.NamedFieldInitializer(DirectCast(CType(node.Left.Accept(Me), ExpressionSyntax), IdentifierNameSyntax).WithoutLeadingTrivia,
                                                                   objectCreationExpression).WithConvertedTrailingTriviaFrom(node)
                        End If
                        Dim vbNode As VB.VisualBasicSyntaxNode = node.Left.Accept(Me)
                        If TypeOf vbNode Is IdentifierNameSyntax Then
                            If node.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                GetStatementwithIssues(node).AddMarker(Factory.EmptyStatement.WithLeadingTrivia(node.GetLeadingTrivia.ConvertTriviaList()), StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                            End If
                            Return Factory.NamedFieldInitializer(DirectCast(vbNode.WithoutLeadingTrivia, IdentifierNameSyntax), DirectCast(nodeRight, ExpressionSyntax)).WithConvertedTrailingTriviaFrom(node)
                        End If
                        If TypeOf vbNode Is MemberAccessExpressionSyntax Then
                            Dim kind As VB.SyntaxKind = GetExpressionKind(CS.CSharpExtensions.Kind(node))
                            Dim OperatorToken As SyntaxToken = GetOperatorToken(kind, IsReferenceType:=False)
                            Return Factory.AssignmentStatement(kind, DirectCast(vbNode, MemberAccessExpressionSyntax), OperatorToken, DirectCast(nodeRight, ExpressionSyntax)).WithConvertedTriviaFrom(node)
                        Else
                            Stop
                        End If
                    End If
                End If

                If TypeOf node.Parent Is CSS.ArrowExpressionClauseSyntax AndAlso
                    TypeOf node.Left IsNot CSS.TupleExpressionSyntax Then
                    Return Factory.SimpleAssignmentStatement(CType(node.Left.Accept(Me), ExpressionSyntax), CType(node.Right.Accept(Me), ExpressionSyntax))
                End If
                Dim RightTypeInfo As TypeInfo = _mSemanticModel.GetTypeInfo(node.Right)
                Dim IsDelegate As Boolean
                If RightTypeInfo.ConvertedType IsNot Nothing Then
                    IsDelegate = RightTypeInfo.ConvertedType.IsDelegateType
                    If Not IsDelegate Then
                        IsDelegate = RightTypeInfo.ConvertedType.ToString().StartsWith("System.EventHandler", StringComparison.Ordinal)
                    End If
                Else
                    If RightTypeInfo.Type IsNot Nothing Then
                        IsDelegate = RightTypeInfo.Type.IsDelegateType
                        If Not IsDelegate Then
                            IsDelegate = RightTypeInfo.Type.ToString.StartsWith("System.EventHandler", StringComparison.Ordinal)
                        End If
                    End If
                End If
                If IsDelegate OrElse node.Right.IsKind(CS.SyntaxKind.ParenthesizedLambdaExpression) Then
                    If node.OperatorToken.IsKind(CS.SyntaxKind.PlusEqualsToken) Then
                        Return Factory.AddHandlerStatement(CType(node.Left.Accept(Me), ExpressionSyntax).WithLeadingTrivia(Factory.Space), CType(node.Right.Accept(Me), ExpressionSyntax)).WithConvertedTriviaFrom(node)
                    End If

                    If node.OperatorToken.IsKind(CS.SyntaxKind.MinusEqualsToken) Then
                        ' TODO capture and leading comments from node.left
                        Return Factory.RemoveHandlerStatement(CType(node.Left.Accept(Me), ExpressionSyntax).WithLeadingTrivia(Factory.Space), CType(node.Right.Accept(Me), ExpressionSyntax)).WithConvertedTriviaFrom(node)
                    End If
                End If

                If node.Left.IsKind(CS.SyntaxKind.DeclarationExpression, CS.SyntaxKind.TupleExpression) Then
                    Dim dimModifiersTokens As SyntaxTokenList = Factory.TokenList(
                                                DimKeyword.WithConvertedLeadingTriviaFrom(node.Left.GetFirstToken())
                                                )
                    Dim initializer As EqualsValueSyntax = Factory.EqualsValue(CType(node.Right.Accept(Me), ExpressionSyntax))
                    Dim StatementList As New SyntaxList(Of StatementSyntax)
                    Dim VariableNames As New List(Of String)
                    If node.Left.IsKind(CS.SyntaxKind.DeclarationExpression) Then
                        Dim nodeLeft As CSS.DeclarationExpressionSyntax = DirectCast(node.Left, CSS.DeclarationExpressionSyntax)
                        Dim designation As CSS.ParenthesizedVariableDesignationSyntax = DirectCast(nodeLeft.Designation, CSS.ParenthesizedVariableDesignationSyntax)
                        For Each e As IndexClass(Of CSS.VariableDesignationSyntax) In designation.Variables.WithIndex
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
                        Dim tupleType As TupleTypeSyntax = Nothing
                        Dim simpleAs As SimpleAsClauseSyntax = Nothing
                        Dim identifierName As String = node.GetUniqueVariableNameInScope("TempVar", _mSemanticModel)
                        Dim tempIdentifier As SeparatedSyntaxList(Of ModifiedIdentifierSyntax)
                        If RightTypeInfo.ConvertedType IsNot Nothing AndAlso Not RightTypeInfo.ConvertedType.IsErrorType Then
                            If TypeOf RightTypeInfo.Type Is INamedTypeSymbol Then
                                Dim possibleTupleType As INamedTypeSymbol = DirectCast(RightTypeInfo.ConvertedType, INamedTypeSymbol)
                                If possibleTupleType.IsTupleType Then
                                    identifierName = node.GetUniqueVariableNameInScope("TupleTempVar", _mSemanticModel)
                                    tempIdentifier = Factory.SingletonSeparatedList(Factory.ModifiedIdentifier(identifierName))
                                    tupleType = CType(possibleTupleType.TupleElements(0).ContainingType.ToString.ConvertCSStringToName, TupleTypeSyntax)
                                    If tupleType.Elements.All(Function(t As TupleElementSyntax) As Boolean
                                                                  If TypeOf t IsNot TypedTupleElementSyntax Then
                                                                      Return False
                                                                  End If
                                                                  Dim typedTupleElement As TypedTupleElementSyntax = CType(t, TypedTupleElementSyntax)
                                                                  Return typedTupleElement.Type.RawKind <> VB.SyntaxKind.PredefinedType
                                                              End Function) Then
                                        simpleAs = Factory.SimpleAsClause(AsKeyword.WithTrailingTrivia(Factory.Space), attributeLists:=Nothing, tupleType.WithLeadingTrivia(Factory.Space)).WithLeadingTrivia(Factory.Space)
                                    End If
                                Else
                                    simpleAs = Factory.SimpleAsClause(
                                            AsKeyword.WithTrailingTrivia(Factory.Space),
                                            attributeLists:=Nothing,
                                            ConvertToType(possibleTupleType.ToString).WithLeadingTrivia(Factory.Space)
                                            ).WithLeadingTrivia(Factory.Space)
                                End If
                            End If
                        End If
                        If tempIdentifier.Count = 0 Then
                            tempIdentifier = Factory.SingletonSeparatedList(Factory.ModifiedIdentifier(identifierName))
                        End If
                        StatementList = StatementList.Add(FactoryDimStatement(tempIdentifier(0).Identifier, simpleAs, initializer).
                                                            WithPrependedLeadingTrivia(Factory.CommentTrivia($" ' TODO: Visual Basic has no equivalent to C# deconstruction declarations, an attempt was made to convert."), VBEOLTrivia))

                        For variableIndex As Integer = 0 To VariableNames.Count - 1
                            If VariableNames(variableIndex) = "__DiscardDesignation__" Then
                                Continue For
                            End If
                            Dim AsClause As AsClauseSyntax = Nothing
                            If nodeLeft.Type Is Nothing Then
                                Stop
                            Else
                                Dim TempType As TypeSyntax = CType(nodeLeft.Type.Accept(Me), TypeSyntax)
                                If nodeLeft.Type.IsVar OrElse tupleType Is Nothing Then
                                    AsClause = Nothing
                                Else
                                    If TypeOf tupleType.Elements(variableIndex) Is NamedTupleElementSyntax Then
                                        TempType = CType(tupleType.Elements(variableIndex), NamedTupleElementSyntax).AsClause.Type
                                    Else
                                        TempType = Factory.ParseTypeName(tupleType.Elements(variableIndex).ToString)
                                    End If
                                    AsClause = Factory.SimpleAsClause(TempType)
                                End If
                            End If
                            initializer = Factory.EqualsValue(Factory.InvocationExpression(Factory.ParseExpression($"{identifierName}.Item{variableIndex + 1}")))

                            StatementList = StatementList.Add(FactoryDimStatement(VariableNames(variableIndex), AsClause, initializer))
                        Next
                    End If

                    ' Handle assignment to a Tuple of Variables that already exist
                    If node.Left.IsKind(CS.SyntaxKind.TupleExpression) Then
                        Dim LeftTupleNode As TupleExpressionSyntax = DirectCast(CType(node.Left.Accept(Me), ExpressionSyntax).WithConvertedTriviaFrom(node.Left), TupleExpressionSyntax)

                        VariableNames = New List(Of String)
                        Dim IdentifierName As String = node.GetUniqueVariableNameInScope("TupleTempVar", _mSemanticModel)
                        For Each Argument As ArgumentSyntax In LeftTupleNode.Arguments
                            VariableNames.Add(Argument.ToString)
                        Next
                        Dim TupleList As New List(Of String)
                        If RightTypeInfo.Type Is Nothing OrElse RightTypeInfo.Type.IsErrorType Then
                            For Each a As CSS.ArgumentSyntax In DirectCast(node.Left, CSS.TupleExpressionSyntax).Arguments
                                If TypeOf a.Expression Is CSS.DeclarationExpressionSyntax Then
                                    Dim t As CSS.DeclarationExpressionSyntax = DirectCast(a.Expression, CSS.DeclarationExpressionSyntax)
                                    TupleList.Add(ConvertToType(t.Type.ToString).ToString)
                                ElseIf TypeOf a.Expression Is CSS.IdentifierNameSyntax Then
                                    TupleList.Add("Object")
                                Else
                                    ' We are going to ignore this
                                    TupleList.Add("_")
                                End If
                            Next
                        Else
                            If TypeOf RightTypeInfo.Type Is INamedTypeSymbol Then
                                Dim Type As INamedTypeSymbol = DirectCast(RightTypeInfo.ConvertedType, INamedTypeSymbol)
                                TupleList.Add(Type.ConvertToType.ToFullString)
                            ElseIf TypeOf RightTypeInfo.Type Is ITypeSymbol Then
                                Try
                                    For Each a As CSS.ArgumentSyntax In DirectCast(node.Left, CSS.TupleExpressionSyntax).Arguments
                                        If TypeOf a.Expression Is CSS.DeclarationExpressionSyntax Then
                                            Dim t As CSS.DeclarationExpressionSyntax = DirectCast(a.Expression, CSS.DeclarationExpressionSyntax)
                                            TupleList.Add(ConvertToType(t.Type.ToString).ToString)
                                        ElseIf TypeOf a.Expression Is CSS.IdentifierNameSyntax Then
                                            TupleList.Add("Object")
                                        Else
                                            ' We are going to ignore this
                                            TupleList.Add("_")
                                        End If
                                    Next
                                Catch ex As OperationCanceledException
                                    Throw
                                Catch ex As Exception
                                    Stop
                                    Throw
                                End Try
                            Else
                                Stop
                            End If
                        End If
                        Dim builder As New StringBuilder()
                        builder.Append("("c)
                        For Each e As IndexClass(Of String) In TupleList.WithIndex
                            builder.Append(e.Value & ", ")
                        Next
                        builder.Append(TupleList.Last & ")")
                        Dim TupleType As String = builder.ToString

                        Dim TupleType2 As TypeSyntax = Factory.ParseTypeName(TupleType).WithLeadingTrivia(Factory.Space)
                        Dim SimpleAs As SimpleAsClauseSyntax = Factory.SimpleAsClause(AsKeyword.With(Factory.Space, Factory.Space), attributeLists:=Nothing, TupleType2).WithLeadingTrivia(Factory.Space)
                        StatementList = StatementList.Add(FactoryDimStatement(IdentifierName, SimpleAs, initializer))
                        For Each e As IndexClass(Of String) In VariableNames.WithIndex
                            If e.Value = "__" Then
                                Continue For
                            End If
                            Dim NewLeftNode As ExpressionSyntax = Factory.IdentifierName(e.Value)
                            Dim NewRightNode As ExpressionSyntax = Factory.InvocationExpression(Factory.ParseExpression($"{IdentifierName}.Item{e.Index + 1}"))
                            Dim kind As VB.SyntaxKind = GetExpressionKind(CS.CSharpExtensions.Kind(node))
                            Dim OperatorToken As SyntaxToken = GetOperatorToken(kind, IsReferenceType:=False)
                            Dim AssignmentStatement As AssignmentStatementSyntax = Factory.AssignmentStatement(kind,
                                                                                                    NewLeftNode,
                                                                                                    OperatorToken,
                                                                                                    NewRightNode)
                            StatementList = StatementList.Add(AssignmentStatement)
                        Next
                    End If
                    Dim TryStatement As TryStatementSyntax = Factory.TryStatement.
                        WithLeadingTrivia(VBEOLTrivia).
                        WithPrependedLeadingTrivia(Factory.CommentTrivia("' TODO: This Try Block can be removed"))
                    Dim Throwstatement As SyntaxList(Of StatementSyntax) = Factory.SingletonList(Of StatementSyntax)(Factory.ThrowStatement)
                    Dim CatchBlock As SyntaxList(Of CatchBlockSyntax) = Factory.SingletonList(Factory.CatchBlock(Factory.CatchStatement,
                                                                                                                         Throwstatement)
                                                                                                    )
                    Return Factory.TryBlock(TryStatement,
                                             StatementList,
                                             CatchBlock,
                                             finallyBlock:=Nothing,
                                             Factory.EndTryStatement(EndKeyword.WithTrailingTrivia(Factory.Space), TryKeyword).WithTrailingEOL
                                             )
                End If

                Me.MarkPatchInlineAssignHelper(node)
                Return Factory.InvocationExpression(
                    expression:=Factory.IdentifierName("__InlineAssignHelper"),
                    argumentList:=Factory.ArgumentList(Factory.SeparatedList(New ArgumentSyntax() {
                                                                            Factory.SimpleArgument(CType(node.Left.Accept(Me), ExpressionSyntax)),
                                                                            Factory.SimpleArgument(CType(node.Right.Accept(Me), ExpressionSyntax))
                                                                                                       })
                                                        )).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitAwaitExpression(node As CSS.AwaitExpressionSyntax) As VB.VisualBasicSyntaxNode
                Return Factory.AwaitExpression(AwaitKeyword.WithTrailingTrivia(Factory.Space), expression:=DirectCast(node.Expression.Accept(Me), ExpressionSyntax)).WithConvertedLeadingTriviaFrom(node)
            End Function

            Public Overrides Function VisitBaseExpression(node As CSS.BaseExpressionSyntax) As VB.VisualBasicSyntaxNode
                Return Factory.MyBaseExpression()
            End Function

            Public Overrides Function VisitBinaryExpression(node As CSS.BinaryExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim operatorToken As SyntaxToken
                Try
                    Dim retExp As ExpressionSyntax
                    Dim foundEOL As Boolean = False
                    Dim kind As VB.SyntaxKind
                    Dim leftNode As VB.VisualBasicSyntaxNode
                    Dim rightNode As VB.VisualBasicSyntaxNode
                    Dim leftExp As ExpressionSyntax
                    Dim rightExp As ExpressionSyntax
                    If Me.IsConcatenateStringsExpression(node) Then
                        Dim csNodesOrTokens As New List(Of SyntaxNodeOrToken)
                        For Each n As SyntaxNodeOrToken In node.DescendantNodesAndTokens(Function(a As SyntaxNode) (TypeOf a Is CSS.BinaryExpressionSyntax))
                            If n.IsNode Then
                                If Not CType(n, SyntaxNode).IsKind(CS.SyntaxKind.AddExpression) Then
                                    csNodesOrTokens.Add(n)
                                End If
                            Else
                                csNodesOrTokens.Add(n)
                            End If
                        Next
                        kind = VB.SyntaxKind.ConcatenateExpression
                        Dim vbNode As VB.VisualBasicSyntaxNode = CType(csNodesOrTokens(0), CSS.ExpressionSyntax).Accept(Me)
                        retExp = CType(vbNode.ConvertAndModifyNodeTrivia(csNodesOrTokens, 0, IsStatement:=False), ExpressionSyntax)
                        For nodeOrTokenIndex As Integer = 1 To csNodesOrTokens.Count - 1 Step 2
                            operatorToken = ConvertAndModifyTokenTrivia(AmpersandToken, csNodesOrTokens, nodeOrTokenIndex)
                            vbNode = CType(csNodesOrTokens(nodeOrTokenIndex + 1), CSS.ExpressionSyntax).Accept(Me)
                            rightNode = vbNode.ConvertAndModifyNodeTrivia(csNodesOrTokens, nodeOrTokenIndex + 1, IsStatement:=False)
                            retExp = Factory.ConcatenateExpression(retExp,
                                                                   operatorToken,
                                                                   DirectCast(rightNode, ExpressionSyntax))
                        Next
                        'Debug.WriteLine(retExp.ToFullString)
                        Return retExp
                    End If
                    kind = GetExpressionKind(CS.CSharpExtensions.Kind(node))
                    leftNode = node.Left.Accept(Me)
                    rightNode = node.Right.Accept(Me)
                    Select Case node.Kind
                        Case CS.SyntaxKind.CoalesceExpression
                            leftExp = DirectCast(node.Left.Accept(Me), ExpressionSyntax).AdjustExpressionTrivia(AdjustLeading:=True)
                            Dim IfKeywordWithTrivia As SyntaxToken = IfKeyword
                            'Dim commaTokenWithTrivia As SyntaxToken = CommaToken
                            If TypeOf rightNode Is ExpressionSyntax Then
                                rightExp = DirectCast(rightNode, ExpressionSyntax).AdjustExpressionTrivia(AdjustLeading:=True)
                                If leftExp.ContainsEOLTrivia OrElse leftExp.ContainsCommentOrDirectiveTrivia Then
                                    If leftExp.HasLeadingTrivia Then
                                        IfKeywordWithTrivia = IfKeywordWithTrivia.WithLeadingTrivia(leftExp.GetLeadingTrivia)
                                    End If
                                    If node.OperatorToken.HasLeadingTrivia AndAlso node.OperatorToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                        Dim StatementTrivia As SyntaxTriviaList = node.OperatorToken.LeadingTrivia.ConvertTriviaList()
                                        GetStatementwithIssues(node).AddMarker(
                                                        Factory.EmptyStatement.WithLeadingTrivia(StatementTrivia),
                                                        StatementHandlingOption.AppendEmptyStatement,
                                                        AllowDuplicates:=True)
                                        rightExp = rightExp.WithLeadingTrivia(StatementTrivia.Last)
                                    End If
                                End If
                                Dim lastLeadingTrivia As SyntaxTrivia = rightExp.GetLeadingTrivia.LastOrDefault

                                If (Not (lastLeadingTrivia.IsWhitespace AndAlso Not lastLeadingTrivia.Span.IsEmpty)) AndAlso node.OperatorToken.LeadingTrivia.LastOrDefault.IsWhitespace Then
                                    rightExp = rightExp.WithLeadingTrivia(node.OperatorToken.LeadingTrivia.Last.ConvertTrivia())
                                End If

                                retExp = Factory.BinaryConditionalExpression(IfKeywordWithTrivia,
                                                                             OpenParenToken,
                                                                             leftExp,
                                                                             CommaToken,
                                                                             rightExp,
                                                                             CloseParenToken)
                                Return retExp
                            End If
                            If TypeOf rightNode Is ThrowStatementSyntax Then
                                Dim condition As ExpressionSyntax = Factory.IsExpression(leftExp, NothingExpression)
                                Dim ifStatement As IfStatementSyntax = Factory.IfStatement(IfKeywordWithTrivia, condition, ThenKeyword)

                                Dim statements As SyntaxList(Of StatementSyntax) = Factory.SingletonList(DirectCast(rightNode, StatementSyntax))
                                Dim ifBlock As StatementSyntax = Factory.SingleLineIfStatement(IfKeywordWithTrivia,
                                                                                               condition,
                                                                                               ThenKeyword,
                                                                                               statements,
                                                                                               elseClause:=Nothing).WithTrailingEOL
                                GetStatementwithIssues(node).AddMarker(ifBlock, StatementHandlingOption.PrependStatement, AllowDuplicates:=False)
                                Return leftNode
                            Else
                                Stop
                            End If
                        Case CS.SyntaxKind.AsExpression
                            leftExp = DirectCast(leftNode, ExpressionSyntax)
                            Dim commaTokenWithTrivia As SyntaxToken = CommaToken
                            If leftExp.ContainsEOLTrivia Then
                                leftExp = leftExp.WithRestructuredingEOLTrivia
                                commaTokenWithTrivia = commaTokenWithTrivia.WithTrailingTrivia(VBEOLTrivia)
                            End If
                            Return Factory.TryCastExpression(
                                                TryCastKeyword,
                                                OpenParenToken,
                                                leftExp,
                                                commaTokenWithTrivia,
                                                DirectCast(rightNode, TypeSyntax),
                                                CloseParenToken)
                        Case CS.SyntaxKind.IsExpression
                            Dim newLeadingTrivia As New SyntaxTriviaList
                            If leftNode.HasLeadingTrivia AndAlso leftNode.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                newLeadingTrivia = leftNode.GetLeadingTrivia
                                leftNode = leftNode.WithoutTrivia.WithTrailingTrivia(Factory.Space)
                            End If
                            If rightNode.HasLeadingTrivia AndAlso rightNode.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                newLeadingTrivia = newLeadingTrivia.AddRange(rightNode.GetLeadingTrivia)
                                rightNode = rightNode.WithoutTrivia.WithTrailingTrivia(Factory.Space)
                            End If
                            Return Factory.TypeOfIsExpression(DirectCast(leftNode, ExpressionSyntax), DirectCast(rightNode, TypeSyntax)).WithLeadingTrivia(newLeadingTrivia)
                        Case CS.SyntaxKind.EqualsExpression, CS.SyntaxKind.NotEqualsExpression
                            Dim otherArgument As ExpressionSyntax = Nothing
                            If node.Left.IsKind(CS.SyntaxKind.NullLiteralExpression) Then
                                otherArgument = DirectCast(rightNode, ExpressionSyntax).With({Factory.Space}, {Factory.Space})
                            ElseIf node.Left.IsKind(CS.SyntaxKind.SizeOfExpression) AndAlso DirectCast(node.Left, CSS.SizeOfExpressionSyntax).Type.IsKind(CS.SyntaxKind.PointerType) Then
                                leftExp = IntPrtSizeExpression
                            End If

                            If node.Right.IsKind(CS.SyntaxKind.NullLiteralExpression) Then
                                otherArgument = DirectCast(leftNode, ExpressionSyntax).With({Factory.Space}, {Factory.Space})
                            ElseIf node.Right.IsKind(CS.SyntaxKind.SizeOfExpression) AndAlso DirectCast(node.Right, CSS.SizeOfExpressionSyntax).Type.IsKind(CS.SyntaxKind.PointerType) Then
                                rightNode = IntPrtSizeExpression
                            End If

                            If otherArgument IsNot Nothing Then
                                If node.IsKind(CS.SyntaxKind.EqualsExpression) Then
                                    Return Factory.IsExpression(otherArgument, NothingExpression.WithConvertedTriviaFrom(node.Right))
                                End If
                                Return Factory.IsNotExpression(otherArgument, NothingExpression.WithConvertedTriviaFrom(node.Right))
                            End If
                    End Select

                    ' Handle all other expressions here
                    rightExp = DirectCast(rightNode, ExpressionSyntax).AdjustExpressionTrivia(AdjustLeading:=True)
                    Dim leftTrailingTrivia As SyntaxTriviaList = leftNode.GetTrailingTrivia
                    If leftTrailingTrivia.ToList.Count = 1 AndAlso leftTrailingTrivia(0).ToString.Trim = "?" Then
                        Dim originalIdentifier As IdentifierNameSyntax = rightExp.DescendantNodes.
                                                                           OfType(Of IdentifierNameSyntax).
                                                                           First(Function(b As IdentifierNameSyntax) b.Kind() = VB.SyntaxKind.IdentifierName)
                        Dim newIdentifierWithQuestionMark As IdentifierNameSyntax =
                                    Factory.IdentifierName($"{leftNode}?")
                        Return rightExp.ReplaceNode(originalIdentifier, newIdentifierWithQuestionMark)
                    End If

                    leftExp = CType(leftNode, ExpressionSyntax).AdjustExpressionTrivia(AdjustLeading:=True)
                    Dim isReferenceType As Boolean = IsReferenceComparison(node.Left, node.Right, _mSemanticModel)
                    operatorToken = GetOperatorToken(kind, isReferenceType).WithConvertedTriviaFrom(node.OperatorToken)
                    If operatorToken.HasLeadingTrivia Then
                        operatorToken = operatorToken.AdjustTokenLeadingTrivia()
                    End If

                    If operatorToken.HasTrailingTrivia Then
                        operatorToken = operatorToken.AdjustTokenTrailingTrivia(RemoveTrailingLineContinuation:=False)
                    End If

                    If isReferenceType AndAlso operatorToken.IsKind(VB.SyntaxKind.IsKeyword, VB.SyntaxKind.IsNotKeyword) Then
                        If operatorToken.IsKind(VB.SyntaxKind.IsKeyword) Then
                            retExp = Factory.IsExpression(leftExp,
                                                          operatorToken,
                                                          rightExp
                                                          )
                        Else
                            retExp = Factory.IsNotExpression(leftExp,
                                                             operatorToken,
                                                             rightExp
                                                             )
                        End If
                    Else
                        retExp = Factory.BinaryExpression(kind,
                                                          leftExp,
                                                          operatorToken,
                                                          rightExp
                                                          )
                    End If
                    Return retExp
                Catch ex As InsufficientExecutionStackException
                    _reportException?.Invoke(ex)
                    Return Nothing
                Catch ex As OperationCanceledException
                    Throw
                Catch ex As Exception
                    Stop
                    'Throw
                End Try
                Throw UnreachableException
            End Function

            Public Overrides Function VisitCastExpression(node As CSS.CastExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim CTypeExpressionSyntax As VB.VisualBasicSyntaxNode
                Dim newTrailingTrivia As SyntaxTriviaList
                Try
                    Dim type As ITypeSymbol = _mSemanticModel.GetTypeInfo(node.Type).Type
                    Dim Expression As ExpressionSyntax = DirectCast(node.Expression.Accept(Me), ExpressionSyntax)
                    newTrailingTrivia = newTrailingTrivia.AddRange(Expression.GetTrailingTrivia)
                    newTrailingTrivia = newTrailingTrivia.AddRange(node.GetTrailingTrivia.ConvertTriviaList())
                    Expression = Expression.WithoutTrivia

                    Dim ExpressionTypeStr As String = _mSemanticModel.GetTypeInfo(node.Expression).Type?.ToString
                    Select Case type.SpecialType
                        Case SpecialType.System_Object
                            CTypeExpressionSyntax = Factory.PredefinedCastExpression(CObjKeyword, Expression)
                        Case SpecialType.System_Boolean
                            CTypeExpressionSyntax = Factory.PredefinedCastExpression(CBoolKeyword, Expression)
                        Case SpecialType.System_Char
                            Dim TestAgainst As String() = {"int", "UShort"}
                            If node.Parent.IsKind(CS.SyntaxKind.AttributeArgument) Then
                                CTypeExpressionSyntax = Expression
                            ElseIf TestAgainst.Contains(ExpressionTypeStr, StringComparer.OrdinalIgnoreCase) Then
                                CTypeExpressionSyntax = Factory.ParseExpression($"ChrW({Expression})")
                            Else
                                CTypeExpressionSyntax = Factory.PredefinedCastExpression(CCharKeyword, Expression)
                            End If
                        Case SpecialType.System_SByte
                            CTypeExpressionSyntax = Factory.PredefinedCastExpression(CSByteKeyword, Expression)
                        Case SpecialType.System_Byte
                            If Expression.IsKind(VB.SyntaxKind.CharacterLiteralExpression) Then
                                CTypeExpressionSyntax = Factory.ParseExpression($"AscW({Expression})")
                            Else
                                CTypeExpressionSyntax = Factory.PredefinedCastExpression(CByteKeyword, Expression)
                            End If
                        Case SpecialType.System_Int16
                            Dim FixExpression As ExpressionSyntax = Factory.IdentifierName("Fix")
                            Dim ArgumentList As ArgumentListSyntax = Factory.ArgumentList(Factory.SingletonSeparatedList(Of ArgumentSyntax)(Factory.SimpleArgument(Expression)))
                            CTypeExpressionSyntax = Factory.PredefinedCastExpression(CShortKeyword, Factory.InvocationExpression(FixExpression, ArgumentList))
                        Case SpecialType.System_UInt16
                            If ExpressionTypeStr = "char" Then
                                CTypeExpressionSyntax = Factory.ParseExpression(text:=$"ChrW({Expression})")
                            Else
                                CTypeExpressionSyntax = Factory.PredefinedCastExpression(CUShortKeyword, Expression)
                            End If
                        Case SpecialType.System_Int32
                            If ExpressionTypeStr = "char" Then
                                CTypeExpressionSyntax = Factory.ParseExpression($"ChrW({Expression})").WithTrailingTrivia(newTrailingTrivia)
                            Else
                                Dim FixExpression As ExpressionSyntax = Factory.IdentifierName("Fix")
                                Dim ArgumentList As ArgumentListSyntax = Factory.ArgumentList(Factory.SingletonSeparatedList(Of ArgumentSyntax)(Factory.SimpleArgument(Expression)))
                                CTypeExpressionSyntax = Factory.PredefinedCastExpression(CIntKeyword, Factory.InvocationExpression(FixExpression, ArgumentList))
                            End If
                        Case SpecialType.System_UInt32
                            CTypeExpressionSyntax = Factory.PredefinedCastExpression(CUIntKeyword, Expression)
                        Case SpecialType.System_Int64
                            Dim FixExpression As ExpressionSyntax = Factory.IdentifierName("Fix")
                            Dim ArgumentList As ArgumentListSyntax = Factory.ArgumentList(Factory.SingletonSeparatedList(Of ArgumentSyntax)(Factory.SimpleArgument(Expression)))
                            CTypeExpressionSyntax = Factory.PredefinedCastExpression(CLngKeyword, Factory.InvocationExpression(FixExpression, ArgumentList))
                        Case SpecialType.System_UInt64
                            CTypeExpressionSyntax = Factory.PredefinedCastExpression(CULngKeyword, Expression)
                        Case SpecialType.System_Decimal
                            CTypeExpressionSyntax = Factory.PredefinedCastExpression(CDecKeyword, Expression)
                        Case SpecialType.System_Single
                            CTypeExpressionSyntax = Factory.PredefinedCastExpression(CSngKeyword, Expression)
                        Case SpecialType.System_Double
                            CTypeExpressionSyntax = Factory.PredefinedCastExpression(CDblKeyword, Expression)
                        Case SpecialType.System_String
                            CTypeExpressionSyntax = Factory.PredefinedCastExpression(CStrKeyword, Expression)
                        Case SpecialType.System_DateTime
                            CTypeExpressionSyntax = Factory.PredefinedCastExpression(CDateKeyword, Expression)
                        Case Else
                            ' Added support to correctly handle AddressOf
                            Dim TypeOrAddressOf As VB.VisualBasicSyntaxNode = node.Type.Accept(Me)
                            If TypeOrAddressOf.IsKind(VB.SyntaxKind.AddressOfExpression) Then
                                Dim AddressOf1 As UnaryExpressionSyntax = DirectCast(TypeOrAddressOf, UnaryExpressionSyntax)
                                If AddressOf1.Operand.ToString.StartsWith("&", StringComparison.OrdinalIgnoreCase) Then
                                    Dim StatementWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                                    StatementWithIssues.AddMarker(FlagUnsupportedStatements(StatementWithIssues, "pointers", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=True)
                                    CTypeExpressionSyntax = Expression
                                ElseIf node.Type.IsKind(CS.SyntaxKind.PointerType) Then
                                    Return Factory.CTypeExpression(Expression, Factory.ParseTypeName("IntPtr"))
                                Else
                                    CTypeExpressionSyntax = Factory.CTypeExpression(Expression, Factory.ParseTypeName(AddressOf1.Operand.ToString.RemoveAll("&")))
                                End If
                            ElseIf node.Type.DetermineType(_mSemanticModel)._ITypeSymbol.IsDelegateType Then
                                If Expression.IsKind(VB.SyntaxKind.AddressOfExpression) Then
                                    CTypeExpressionSyntax = Factory.CTypeExpression(Expression, DirectCast(TypeOrAddressOf, TypeSyntax))
                                Else
                                    CTypeExpressionSyntax = Factory.CTypeExpression(Factory.AddressOfExpression(Expression), DirectCast(TypeOrAddressOf, TypeSyntax))
                                End If
                            Else
                                CTypeExpressionSyntax = Factory.CTypeExpression(Expression, DirectCast(TypeOrAddressOf, TypeSyntax))
                            End If
                    End Select
                Catch ex As OperationCanceledException
                    Throw
                Catch ex As Exception
                    Stop
                    Throw
                End Try
                Return CTypeExpressionSyntax.WithConvertedLeadingTriviaFrom(node).WithTrailingTrivia(newTrailingTrivia)
            End Function

            Public Overrides Function VisitCheckedExpression(node As CSS.CheckedExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim Unchecked As Boolean = node.Keyword.IsKind(CS.SyntaxKind.UncheckedKeyword)
                Dim StatementWithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                Dim LeadingTrivia As SyntaxTriviaList = StatementWithIssue.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=True, "VB has no direct equivalent To C# unchecked")
                ' Only notify once on one line TODO Merge the comments

                Dim Expression As ExpressionSyntax = DirectCast(node.Expression.Accept(Me), ExpressionSyntax)
                If TypeOf Expression Is PredefinedCastExpressionSyntax Then
                    Dim CastExpression As PredefinedCastExpressionSyntax = DirectCast(Expression, PredefinedCastExpressionSyntax)
                    If Unchecked Then
                        StatementWithIssue.AddMarker(Factory.EmptyStatement.WithLeadingTrivia(LeadingTrivia), StatementHandlingOption.PrependStatement, AllowDuplicates:=False)
                        Return Factory.ParseExpression($"{CastExpression.Keyword}(Val(""&H"" & Hex({CastExpression.Expression})))")
                    Else
                        Return Factory.ParseExpression($"{CastExpression.Keyword}({CastExpression.Expression})")
                    End If
                End If
                If TypeOf Expression Is BinaryConditionalExpressionSyntax OrElse
                    TypeOf Expression Is BinaryExpressionSyntax OrElse
                    TypeOf Expression Is InvocationExpressionSyntax OrElse
                    TypeOf Expression Is LiteralExpressionSyntax OrElse
                    TypeOf Expression Is ObjectCreationExpressionSyntax Then
                    If Unchecked Then
                        StatementWithIssue.AddMarker(Factory.EmptyStatement.WithLeadingTrivia(LeadingTrivia), StatementHandlingOption.PrependStatement, AllowDuplicates:=False)
                        Return Factory.ParseExpression($"Unchecked({Expression})")
                    Else
                        Return Expression
                    End If
                End If
                If TypeOf Expression Is CTypeExpressionSyntax OrElse
                    TypeOf Expression Is TernaryConditionalExpressionSyntax OrElse
                    TypeOf Expression Is UnaryExpressionSyntax OrElse
                    TypeOf Expression Is ParenthesizedExpressionSyntax OrElse
                    TypeOf Expression Is IdentifierNameSyntax Then
                    Return Expression
                End If
                Throw UnreachableException
            End Function

            Public Overrides Function VisitConditionalAccessExpression(node As CSS.ConditionalAccessExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim expression As ExpressionSyntax = DirectCast(node.Expression.Accept(Me), ExpressionSyntax)
                Dim TrailingTriviaList As SyntaxTriviaList
                If expression.ContainsEOLTrivia Then
                    TrailingTriviaList = TrailingTriviaList.AddRange(expression.WithRestructuredingEOLTrivia.GetTrailingTrivia)
                    expression = expression.WithoutTrailingTrivia
                End If
                If node.OperatorToken.ContainsDirectives Then
                    Dim StatementWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                    StatementWithIssues.AddMarker(Factory.EmptyStatement().WithLeadingTrivia(ConvertConditionalAccessExpressionToComment(node)).WithTrailingEOL, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                End If
                Return Factory.ConditionalAccessExpression(expression, QuestionToken.WithTrailingTrivia(TrailingTriviaList), DirectCast(node.WhenNotNull.Accept(Me), ExpressionSyntax)).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitConditionalExpression(node As CSS.ConditionalExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim NodesOrTokens As New List(Of SyntaxNodeOrToken) From {
                    node.Condition,
                    node.ColonToken,
                    node.WhenTrue,
                    node.QuestionToken,
                    node.WhenFalse
                }

                Dim Condition As ExpressionSyntax = DirectCast(node.Condition.Accept(Me).ConvertAndModifyNodeTrivia(NodesOrTokens, 0, IsStatement:=False), ExpressionSyntax)

                Dim csWhenTrue As CSS.ExpressionSyntax = node.WhenTrue
                Dim WhenTrue As ExpressionSyntax = Nothing
                If Not csWhenTrue.IsKind(CS.SyntaxKind.ThrowExpression) Then
                    WhenTrue = DirectCast(node.WhenTrue.Accept(Me).ConvertAndModifyNodeTrivia(NodesOrTokens, Index:=2, IsStatement:=False), ExpressionSyntax)
                    If WhenTrue.GetTrailingTrivia.ContainsCommentTrivia Then
                        WhenTrue = WhenTrue.WithTrailingEOL(RemoveLastLineContinuation:=False)
                    End If
                End If

                Dim FirstCommaToken As SyntaxToken = CommaToken.ConvertAndModifyTokenTrivia(NodesOrTokens, Index:=1)

                Dim csWhenFalse As CSS.ExpressionSyntax = node.WhenFalse
                Dim WhenFalse As ExpressionSyntax = Nothing
                If Not csWhenFalse.IsKind(CS.SyntaxKind.ThrowExpression) Then
                    WhenFalse = DirectCast(ConvertAndModifyNodeTrivia(node.WhenFalse.Accept(Me), NodesOrTokens, Index:=4, IsStatement:=True), ExpressionSyntax)
                    If WhenFalse.GetTrailingTrivia.ContainsCommentTrivia Then
                        WhenFalse = WhenFalse.WithTrailingEOL
                    End If
                End If

                Dim IfKeywordWithTrivia As SyntaxToken = IfKeyword.WithConvertedLeadingTriviaFrom(node.Condition.GetFirstToken)
                Dim SecondCommaToken As SyntaxToken = ConvertAndModifyTokenTrivia(CommaToken, NodesOrTokens, Index:=3)
                If Not SecondCommaToken.TrailingTrivia.ContainsCommentOrDirectiveTrivia AndAlso (WhenFalse Is Nothing OrElse Not WhenFalse.GetLeadingTrivia.Any) Then
                    SecondCommaToken = SecondCommaToken.WithTrailingTrivia(Factory.Space)
                End If
                If Not csWhenFalse.IsKind(CS.SyntaxKind.ThrowExpression) AndAlso Not csWhenTrue.IsKind(CS.SyntaxKind.ThrowExpression) Then
                    Return Factory.TernaryConditionalExpression(
                    IfKeywordWithTrivia,
                    OpenParenToken,
                    Condition.WithoutTrivia,
                    FirstCommaToken,
                    WhenTrue,
                    SecondCommaToken,
                    WhenFalse,
                    CloseParenToken)
                End If
                Dim ThrowStatement As ThrowStatementSyntax
                Dim ResultExpression As ExpressionSyntax
                If Not csWhenFalse.IsKind(CS.SyntaxKind.ThrowExpression) Then
                    ThrowStatement = DirectCast(csWhenTrue.Accept(Me).WithConvertedTriviaFrom(csWhenTrue), ThrowStatementSyntax).WithTrailingEOL
                    ResultExpression = DirectCast(csWhenFalse.Accept(Me).WithConvertedTriviaFrom(csWhenFalse), ExpressionSyntax)
                Else
                    Condition = Factory.NotExpression(Condition.WithoutTrivia)
                    ThrowStatement = DirectCast(csWhenFalse.Accept(Me).WithConvertedTriviaFrom(csWhenFalse), ThrowStatementSyntax).WithTrailingEOL
                    ResultExpression = DirectCast(csWhenTrue.Accept(Me).WithConvertedTriviaFrom(csWhenTrue), ExpressionSyntax)
                End If
                Dim Statements As SyntaxList(Of StatementSyntax) = Factory.SingletonList(Of StatementSyntax)(ThrowStatement)

                Dim IfBlock As SingleLineIfStatementSyntax = Factory.SingleLineIfStatement(Condition.WithTrailingTrivia({Factory.Space}),
                                                                                                 Statements,
                                                                                                 elseClause:=Nothing
                                                                                                 )
                Dim StatementWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                StatementWithIssues.AddMarker(IfBlock, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                Return ResultExpression
            End Function

            Public Overrides Function VisitDeclarationExpression(Node As CSS.DeclarationExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim Value As IdentifierNameSyntax
                Return Node.Designation.Accept(Me)
                If Node.Designation.IsKind(CS.SyntaxKind.SingleVariableDesignation) Then
                    Dim SingleVariableDesignation As CSS.SingleVariableDesignationSyntax = DirectCast(Node.Designation, CSS.SingleVariableDesignationSyntax)
                    Value = DirectCast(SingleVariableDesignation.Accept(Me), IdentifierNameSyntax).WithConvertedTriviaFrom(Node)
                    Return Value
                End If
                If Node.Designation.IsKind(CS.SyntaxKind.ParenthesizedVariableDesignation) Then
                    Dim ParenthesizedVariableDesignation As CSS.ParenthesizedVariableDesignationSyntax = DirectCast(Node.Designation, CSS.ParenthesizedVariableDesignationSyntax)

                    Dim DeclarationToBeAdded As LocalDeclarationStatementSyntax =
                        FactoryDimStatement(DirectCast(ParenthesizedVariableDesignation.Accept(Me), VariableDeclaratorSyntax))

                    Dim StatementWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(Node)
                    StatementWithIssues.AddMarker(DeclarationToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=False)
                    Return Factory.IdentifierName(Node.Designation.ToString.RemoveAll(",", " ", "(", ")"))
                End If
                If Node.Designation.IsKind(CS.SyntaxKind.DiscardDesignation) Then
                    Dim DiscardDesignation As CSS.DiscardDesignationSyntax = DirectCast(Node.Designation, CSS.DiscardDesignationSyntax)
                    Value = DirectCast(DiscardDesignation.Accept(Me), IdentifierNameSyntax).WithConvertedTriviaFrom(Node)
                    Return Value

                End If
                Throw UnreachableException
            End Function

            Public Overrides Function VisitDefaultExpression(node As CSS.DefaultExpressionSyntax) As VB.VisualBasicSyntaxNode
                Return Factory.ParseExpression($"CType(Nothing, {node.Type.Accept(Me).WithLeadingTrivia(Factory.Space)})").WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitElementAccessExpression(node As CSS.ElementAccessExpressionSyntax) As VB.VisualBasicSyntaxNode
                If node.ArgumentList.Arguments.Count = 1 AndAlso node.ArgumentList.Arguments(0).Expression.IsKind(CS.SyntaxKind.RangeExpression) Then
                    Dim RangeExpression As CSS.RangeExpressionSyntax = CType(node.ArgumentList.Arguments(0).Expression, CSS.RangeExpressionSyntax)
                    Dim LeftOperand As VB.VisualBasicSyntaxNode = RangeExpression.LeftOperand?.Accept(Me)
                    Dim RightOperand As ExpressionSyntax
                    Dim LeftExpression As ExpressionSyntax = DirectCast(node.Expression.Accept(Me), ExpressionSyntax)
                    If RangeExpression.RightOperand?.IsKind(CS.SyntaxKind.IndexExpression) Then
                        Dim OffsetFromLength As ExpressionSyntax = CType(RangeExpression.RightOperand.Accept(Me), ExpressionSyntax)
                        RightOperand = Factory.ParseExpression($"{LeftExpression}.Length{OffsetFromLength}")
                    Else
                        RightOperand = CType(RangeExpression.RightOperand?.Accept(Me), ExpressionSyntax)
                    End If
                    If LeftOperand Is Nothing Then
                        Return Factory.ParseExpression($"{LeftExpression}.Substring(0, {RightOperand})")
                    Else
                        If RightOperand Is Nothing Then
                            Return Factory.ParseExpression($"{LeftExpression}.Substring({LeftOperand})")
                        Else
                            Return Factory.ParseExpression($"{LeftExpression}.Substring({LeftOperand}, {RightOperand})")
                        End If
                    End If

                End If
                Dim argumentList As ArgumentListSyntax = DirectCast(node.ArgumentList.Accept(Me), ArgumentListSyntax)
                Dim expression As ExpressionSyntax
                If node.Expression.IsKind(CS.SyntaxKind.BaseExpression) Then
                    If node.GetAncestor(Of CSS.IndexerDeclarationSyntax).IsKind(CS.SyntaxKind.IndexerDeclaration) Then
                        expression = Factory.ParseExpression($"MyBase.Item")
                    ElseIf node.GetAncestor(Of CSS.PropertyDeclarationSyntax) IsNot Nothing Then
                        Return Factory.ParseExpression($"MyBase.Item({argumentList.Arguments(0).WithoutTrivia}))")
                    Else
                        Return Factory.ParseName($"MyBase.{argumentList.Arguments(0)}")
                    End If
                ElseIf node.Expression.IsKind(CS.SyntaxKind.ObjectCreationExpression) Then
                    expression = DirectCast(node.Expression.Accept(Me), ExpressionSyntax)
                    Dim UniqueName As String = node.GetUniqueVariableNameInScope("tempVar", _mSemanticModel)

                    Dim AsClause As AsClauseSyntax = Factory.AsNewClause(DirectCast(expression, NewExpressionSyntax))
                    Dim DimStatement As LocalDeclarationStatementSyntax = FactoryDimStatement(UniqueName, AsClause, initializer:=Nothing)
                    GetStatementwithIssues(node).AddMarker(DimStatement, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                    expression = Factory.IdentifierName(Factory.Identifier(UniqueName)).WithTriviaFrom(expression)
                Else
                    expression = DirectCast(node.Expression.Accept(Me), ExpressionSyntax)
                End If

                Return Factory.InvocationExpression(expression, argumentList.WithoutLeadingTrivia)
            End Function

            Public Overrides Function VisitElementBindingExpression(node As CSS.ElementBindingExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim Arguments0 As VB.VisualBasicSyntaxNode = node.ArgumentList.Arguments(0).Accept(Me)
                Dim expression As ExpressionSyntax = Factory.ParseExpression(Arguments0.ToString)
                Dim ParenthesizedExpression As ParenthesizedExpressionSyntax = Factory.ParenthesizedExpression(expression)
                Return Factory.InvocationExpression(ParenthesizedExpression)
            End Function

            Public Overrides Function VisitImplicitArrayCreationExpression(node As CSS.ImplicitArrayCreationExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim csSeparators As IEnumerable(Of SyntaxToken) = node.Initializer.Expressions.GetSeparators
                Dim ExpressionItems As New List(Of ExpressionSyntax)
                Dim NamedFieldItems As New List(Of FieldInitializerSyntax)
                Dim Separators As New List(Of SyntaxToken)
                For Each e As IndexClass(Of CSS.ExpressionSyntax) In node.Initializer.Expressions.WithIndex
                    Dim ItemWithTrivia As VB.VisualBasicSyntaxNode
                    Try
                        ItemWithTrivia = e.Value.Accept(Me).WithConvertedTriviaFrom(e.Value).RemoveExtraLeadingEOL.NormalizeWhitespaceEx(useDefaultCasing:=True, indentation:="    ")
                        Dim LeadingTrivia As SyntaxTriviaList = e.Value.GetLeadingTrivia
                        If LeadingTrivia.Any AndAlso LeadingTrivia.Last.IsKind(CS.SyntaxKind.WhitespaceTrivia) Then
                            ItemWithTrivia = ItemWithTrivia.WithPrependedLeadingTrivia(LeadingTrivia.Last.ConvertTrivia())
                        End If
                        If TypeOf ItemWithTrivia Is NamedFieldInitializerSyntax Then
                            NamedFieldItems.Add(DirectCast(ItemWithTrivia, NamedFieldInitializerSyntax))
                        ElseIf TypeOf ItemWithTrivia Is AssignmentStatementSyntax Then
                            Dim StatementwithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                            StatementwithIssues.AddMarker(FlagUnsupportedStatements(StatementwithIssues, $"C# Assignment Expression", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=False)
                            Return Nothing
                        Else
                            ExpressionItems.Add(DirectCast(ItemWithTrivia, ExpressionSyntax))
                        End If
                    Catch ex As OperationCanceledException
                        Throw
                    Catch ex As Exception
                        Stop
                        Throw
                    End Try
                    If Not e.IsLast Then
                        Separators.Add(CommaToken.WithConvertedTrailingTriviaFrom(csSeparators(e.Index)))
                    End If
                Next
                Dim OpenBraceTokenWithTrivia As SyntaxToken = OpenBraceToken.WithConvertedTriviaFrom(node.Initializer.OpenBraceToken)
                Dim CloseBraceTokenWithTrivia As SyntaxToken = CloseBraceToken.WithConvertedTrailingTriviaFrom(node.Initializer.CloseBraceToken)
                If node.Parent.IsKind(CS.SyntaxKind.ElementAccessExpression) Then
                    CloseBraceTokenWithTrivia = CloseBraceTokenWithTrivia.WithTrailingTrivia(Factory.Space)
                End If
                If ExpressionItems.Any Then
                    RestructureNodesAndSeparators(OpenBraceTokenWithTrivia, ExpressionItems, Separators, CloseBraceTokenWithTrivia)
                    Dim ExpressionInitializers As SeparatedSyntaxList(Of ExpressionSyntax) = Factory.SeparatedList(ExpressionItems, Separators)
                    Return Factory.CollectionInitializer(OpenBraceTokenWithTrivia, ExpressionInitializers, CloseBraceTokenWithTrivia).WithConvertedLeadingTriviaFrom(node.NewKeyword)
                Else
                    RestructureNodesAndSeparators(OpenBraceTokenWithTrivia, NamedFieldItems, Separators, CloseBraceTokenWithTrivia)
                    Dim Initializers As SeparatedSyntaxList(Of FieldInitializerSyntax) = Factory.SeparatedList(NamedFieldItems)
                    Return Factory.AnonymousObjectCreationExpression(Factory.ObjectMemberInitializer(Initializers))
                End If
            End Function

            Public Overrides Function VisitInitializerExpression(node As CSS.InitializerExpressionSyntax) As VB.VisualBasicSyntaxNode
                Try
                    Dim csSeparators As IEnumerable(Of SyntaxToken) = node.Expressions.GetSeparators
                    Dim Expressions As New List(Of ExpressionSyntax)
                    Dim Fields As New List(Of FieldInitializerSyntax)
                    Dim Separators As New List(Of SyntaxToken)
                    Dim ExpressionLastIndex As Integer = node.Expressions.Count - 1
                    Dim FinalSeparator As Boolean = csSeparators.Any AndAlso ExpressionLastIndex <> csSeparators.Count
                    Dim OpenBraceTokenWithTrivia As SyntaxToken = OpenBraceToken.WithConvertedTriviaFrom(node.OpenBraceToken)
                    Dim ReportProgress As Boolean = ExpressionLastIndex > 500

                    If ReportProgress Then
                        s_originalRequest.Progress?.Report(New ProgressReport(0, node.Expressions.Count))
                    End If
                    ' Figuring out this without using Accept is complicated below is safe but not fast
                    Dim ItemIsField As Boolean = node.Expressions.Any AndAlso TypeOf node.Expressions(0).Accept(Me) Is FieldInitializerSyntax
                    Dim FoundEOF As Boolean = False
                    For expressionIndex As Integer = 0 To ExpressionLastIndex
                        If ReportProgress Then
                            s_originalRequest.Progress?.Report(New ProgressReport(expressionIndex + 1, node.Expressions.Count))
                        End If

                        If s_originalRequest.CancelToken.IsCancellationRequested Then
                            Exit For
                        End If
                        Dim Item As VB.VisualBasicSyntaxNode = node.Expressions(expressionIndex).Accept(Me)
                        Try
                            If ItemIsField Then
                                Fields.Add(DirectCast(Item.RemoveExtraLeadingEOL, FieldInitializerSyntax))
                            Else

                                Expressions.Add(DirectCast(Item.RemoveExtraLeadingEOL, ExpressionSyntax))
                            End If
                        Catch ex As OperationCanceledException
                            Throw
                        Catch ex As Exception
                            Stop
                            Throw
                        End Try

                        If ExpressionLastIndex > expressionIndex Then
                            Separators.Add(CommaToken.WithConvertedTrailingTriviaFrom(csSeparators(expressionIndex)))
                        Else
                            If FinalSeparator Then
                                Dim trailingTrivia As New SyntaxTriviaList
                                If ItemIsField Then
                                    Fields(expressionIndex) = Fields(expressionIndex).WithAppendedTrailingTrivia(csSeparators.Last.TrailingTrivia.ConvertTriviaList())
                                    trailingTrivia = Fields(expressionIndex).GetTrailingTrivia
                                Else
                                    Expressions(expressionIndex) = Expressions(expressionIndex).WithAppendedTrailingTrivia(csSeparators.Last.TrailingTrivia.ConvertTriviaList())
                                    trailingTrivia = Expressions(expressionIndex).GetTrailingTrivia
                                End If
                                FoundEOF = trailingTrivia.Any AndAlso trailingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia)
                            End If
                        End If
                    Next
                    Dim CloseBracketLeadingTriva As SyntaxTriviaList = node.CloseBraceToken.LeadingTrivia.ConvertTriviaList()
                    If CloseBracketLeadingTriva.Any Then
                        If CloseBracketLeadingTriva.First.IsComment Then
                            CloseBracketLeadingTriva = CloseBracketLeadingTriva.Insert(1, VBEOLTrivia)
                        End If
                    End If
                    If CloseBracketLeadingTriva.ContainsCommentOrDirectiveTrivia Then
                        Dim FoundCommentOrDirective As Boolean = False
                        Dim NewCLoseBracketLeadingTriva As SyntaxTriviaList
                        For Each e As IndexClass(Of SyntaxTrivia) In CloseBracketLeadingTriva.WithIndex
                            Dim t As SyntaxTrivia = e.Value
                            If FoundCommentOrDirective OrElse t.IsDirective Or t.IsComment Then
                                If Not (FoundEOF OrElse FoundCommentOrDirective) Then
                                    NewCLoseBracketLeadingTriva = NewCLoseBracketLeadingTriva.Add(VBEOLTrivia)
                                    FoundEOF = False
                                End If
                                FoundCommentOrDirective = True
                                NewCLoseBracketLeadingTriva = NewCLoseBracketLeadingTriva.Add(t)
                                Continue For
                            End If
                            Select Case t.RawKind
                                Case VB.SyntaxKind.WhitespaceTrivia
                                    NewCLoseBracketLeadingTriva = NewCLoseBracketLeadingTriva.Add(t)
                                Case VB.SyntaxKind.EndOfLineTrivia
                                    NewCLoseBracketLeadingTriva = NewCLoseBracketLeadingTriva.Add(VBEOLTrivia)
                                    FoundEOF = True
                            End Select
                        Next
                        CloseBracketLeadingTriva = NewCLoseBracketLeadingTriva
                    End If

                    Dim CloseBraceTokenWithTrivia As SyntaxToken = CloseBraceToken.With(CloseBracketLeadingTriva,
                                                                                        node.CloseBraceToken.TrailingTrivia.ConvertTriviaList())
                    If node.IsKind(CS.SyntaxKind.ObjectInitializerExpression) Then
                        Dim WithKeywordWithTrivia As SyntaxToken = WithKeyword.WithTrailingTrivia(VBEOLTrivia)
                        If Fields.Any Then
                            RestructureNodesAndSeparators(OpenBraceTokenWithTrivia, Fields, Separators, CloseBraceTokenWithTrivia)
                            Return Factory.ObjectMemberInitializer(WithKeywordWithTrivia, OpenBraceTokenWithTrivia, Factory.SeparatedList(Fields, Separators), CloseBraceTokenWithTrivia).WithConvertedTriviaFrom(node)
                        End If
                        RestructureNodesAndSeparators(OpenBraceTokenWithTrivia, Expressions, Separators, CloseBraceTokenWithTrivia)

                        If Expressions.Any Then
                            If Not Expressions(ExpressionLastIndex).ContainsEOLTrivia Then
                                Expressions(ExpressionLastIndex) = Expressions(ExpressionLastIndex).WithAppendedEOL
                                Return Factory.ObjectCollectionInitializer(Factory.CollectionInitializer(OpenBraceTokenWithTrivia, Factory.SeparatedList(Expressions.OfType(Of ExpressionSyntax), Separators), CloseBraceTokenWithTrivia))
                            End If
                        Else
                            Return Factory.CollectionInitializer(OpenBraceTokenWithTrivia, Factory.SeparatedList(Expressions.OfType(Of ExpressionSyntax), Separators), CloseBraceTokenWithTrivia.WithoutTrivia).WithTrailingTrivia(CloseBraceTokenWithTrivia.LeadingTrivia)
                        End If
                    End If

                    RestructureNodesAndSeparators(OpenBraceTokenWithTrivia, Expressions, Separators, CloseBraceTokenWithTrivia)
                    If node.IsKind(CS.SyntaxKind.ArrayInitializerExpression) OrElse node.IsKind(CS.SyntaxKind.CollectionInitializerExpression) Then
                        Dim initializers As SeparatedSyntaxList(Of ExpressionSyntax) = Factory.SeparatedList(Expressions, Separators)

                        Dim CollectionInitializer As CollectionInitializerSyntax = Factory.CollectionInitializer(OpenBraceTokenWithTrivia, initializers, CloseBraceTokenWithTrivia)
                        Return CollectionInitializer
                    End If
                    Return Factory.CollectionInitializer(OpenBraceTokenWithTrivia.RemoveExtraEOL, Factory.SeparatedList(Expressions, Separators), CloseBraceTokenWithTrivia)
                Catch ex As OperationCanceledException
                    Throw
                Catch ex As Exception
                    Stop
                    Throw
                End Try
                Throw UnreachableException
            End Function

            Public Overrides Function VisitInterpolatedStringExpression(node As CSS.InterpolatedStringExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim content As New List(Of InterpolatedStringContentSyntax)
                For Each e As IndexClass(Of CSS.InterpolatedStringContentSyntax) In node.Contents.WithIndex
                    content.Add(DirectCast(e.Value.Accept(Me), InterpolatedStringContentSyntax))
                Next
                Return Factory.InterpolatedStringExpression(content.ToArray()).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitInterpolatedStringText(node As CSS.InterpolatedStringTextSyntax) As VB.VisualBasicSyntaxNode
                Dim CSharpToken As SyntaxToken = node.TextToken
                Dim TextToken As SyntaxToken = CSharpToken.ConvertToInterpolatedStringTextToken
                Return Factory.InterpolatedStringText(TextToken).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitInterpolation(node As CSS.InterpolationSyntax) As VB.VisualBasicSyntaxNode
                Dim alignmentClause As InterpolationAlignmentClauseSyntax = Nothing
                If node.AlignmentClause IsNot Nothing Then
                    alignmentClause = CType(node.AlignmentClause.Accept(Me), InterpolationAlignmentClauseSyntax)
                End If
                Dim formatClause As InterpolationFormatClauseSyntax = Nothing
                If node.FormatClause IsNot Nothing Then
                    formatClause = CType(node.FormatClause.Accept(Me), InterpolationFormatClauseSyntax)
                End If
                Return Factory.Interpolation(OpenBraceToken, DirectCast(node.Expression.Accept(Me), ExpressionSyntax), alignmentClause, formatClause, CloseBraceToken).WithConvertedLeadingTriviaFrom(node)
            End Function

            Public Overrides Function VisitInterpolationAlignmentClause(node As CSS.InterpolationAlignmentClauseSyntax) As VB.VisualBasicSyntaxNode
                Return Factory.InterpolationAlignmentClause(Factory.Token(VB.SyntaxKind.CommaToken), CType(node.Value.Accept(Me), ExpressionSyntax))
            End Function

            Public Overrides Function VisitInterpolationFormatClause(node As CSS.InterpolationFormatClauseSyntax) As VB.VisualBasicSyntaxNode
                Dim formatStringToken As SyntaxToken = Factory.InterpolatedStringTextToken(SyntaxTriviaList.Empty, node.FormatStringToken.Text, node.FormatStringToken.ValueText, SyntaxTriviaList.Empty)
                Return Factory.InterpolationFormatClause(Factory.Token(VB.SyntaxKind.ColonToken), formatStringToken)
            End Function

            Public Overrides Function VisitInvocationExpression(node As CSS.InvocationExpressionSyntax) As VB.VisualBasicSyntaxNode
                If Me.IsNameOfExpression(node) Then
                    Try
                        Dim argument As CSS.ExpressionSyntax = node.ArgumentList.Arguments.Single.Expression
                        Dim convertedExpression As ExpressionSyntax = CType(argument.Accept(Me), ExpressionSyntax)
                        Dim ues As UnaryExpressionSyntax = If(TypeOf convertedExpression Is UnaryExpressionSyntax, CType(convertedExpression, UnaryExpressionSyntax), Nothing)
                        If ues IsNot Nothing Then
                            ' Don't wrap nameof operand in "AddressOf" if it's a method
                            convertedExpression = ues.Operand
                        End If

                        Return Factory.NameOfExpression(convertedExpression).WithConvertedTriviaFrom(node)
                    Catch ex As OperationCanceledException
                        Throw
                    Catch
                        Stop
                        Throw
                    End Try
                    _reportException?.Invoke(UnreachableException)
                End If
                Dim _visitInvocationExpression As VB.VisualBasicSyntaxNode = Nothing
                If Me.TryCreateRaiseEventStatement(node.Expression, node.ArgumentList, _visitInvocationExpression) Then
                    Return _visitInvocationExpression
                End If

                Dim vbEventExpression As ExpressionSyntax = DirectCast(node.Expression.Accept(Me), ExpressionSyntax).WithoutLeadingSystemDot
                Dim ArgumentList1 As ArgumentListSyntax = DirectCast(node.ArgumentList.Accept(Me), ArgumentListSyntax)
                Dim invocationExpression As InvocationExpressionSyntax = Factory.InvocationExpression(vbEventExpression.AdjustExpressionTrivia(AdjustLeading:=False), ArgumentList1)
                Dim objectCreationExpression As CSS.ObjectCreationExpressionSyntax = TryCast(node.Expression.DescendantNodesAndSelf().OfType(Of CSS.MemberAccessExpressionSyntax).FirstOrDefault?.Expression, CSS.ObjectCreationExpressionSyntax)
                If objectCreationExpression IsNot Nothing Then
                    If TypeOf node.Parent Is CSS.ExpressionStatementSyntax AndAlso objectCreationExpression IsNot Nothing Then
                        Return Factory.CallStatement(invocationExpression.WithLeadingTrivia(Factory.Space))
                    End If
                    Return invocationExpression
                End If
                Dim NewTrailingTrivia As SyntaxTriviaList = ArgumentList1.GetTrailingTrivia
                NewTrailingTrivia = NewTrailingTrivia.AddRange(node.GetTrailingTrivia.ConvertTriviaList())
                If NewTrailingTrivia.Count = 2 Then
                    Dim LastTrivia As SyntaxTrivia = NewTrailingTrivia.Last
                    Select Case NewTrailingTrivia.First.RawKind
                        Case VB.SyntaxKind.EndOfLineTrivia
                            If LastTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                NewTrailingTrivia = NewTrailingTrivia.RemoveAt(1)
                            ElseIf LastTrivia.IsKind(VB.SyntaxKind.WhitespaceTrivia) OrElse
                                LastTrivia.IsKind(VB.SyntaxKind.EndIfDirectiveTrivia) Then
                                ' Ignore, it belongs here
                            Else
                                Stop
                            End If
                        Case VB.SyntaxKind.WhitespaceTrivia
                            If LastTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                NewTrailingTrivia = NewTrailingTrivia.RemoveAt(1)
                            ElseIf LastTrivia.IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                                NewTrailingTrivia = NewTrailingTrivia.RemoveAt(1)
                            Else
                                Stop
                            End If
                        Case Else
                            Stop
                    End Select
                End If
                Dim methodInfo As TypeInfo = _mSemanticModel.GetTypeInfo(node.Expression)
                If methodInfo.Type?.Name = "Func" Then
                    Return Factory.InvocationExpression(invocationExpression.WithoutTrailingTrivia, Factory.ArgumentList()).WithTrailingTrivia(NewTrailingTrivia)
                End If
                Return invocationExpression.WithTrailingTrivia(NewTrailingTrivia)
            End Function

            Public Overrides Function VisitLiteralExpression(node As CSS.LiteralExpressionSyntax) As VB.VisualBasicSyntaxNode
                ' now this looks somehow like a hack... is there a better way?
                If node.IsKind(CS.SyntaxKind.StringLiteralExpression) Then
                    ' @"" have no escapes except quotes (ASCII and Unicode)
                    If node.Token.Text.StartsWith("@", StringComparison.Ordinal) Then
                        Return Factory.StringLiteralExpression(
                                                    token:=Factory.StringLiteralToken(
                                                    text:=node.Token.Text.
                                                        Substring(startIndex:=1).
                                                        Replace(UnicodeOpenQuote, UnicodeDoubleOpenQuote, StringComparison.Ordinal).
                                                        Replace(UnicodeCloseQuote, UnicodeDoubleCloseQuote, StringComparison.Ordinal).
                                                        NormalizeLineEndings,
                                                    value:=DirectCast(node.Token.Value, String).
                                                        Replace(Quote, DoubleQuote, StringComparison.Ordinal).
                                                        Replace(UnicodeOpenQuote, UnicodeDoubleOpenQuote, StringComparison.Ordinal).
                                                        Replace(UnicodeCloseQuote, UnicodeDoubleCloseQuote, StringComparison.Ordinal).NormalizeLineEndings)
                                                                ).WithConvertedTriviaFrom(node.Token)
                    End If
                    If DirectCast(node.Token.Value, String) <> node.Token.ValueText Then
                        Return Factory.InterpolatedStringExpression(
                                            Factory.InterpolatedStringText(node.Token.WithoutTrivia.ConvertToInterpolatedStringTextToken)
                                                                      )
                    End If
                    Return GetLiteralExpression(node.Token.Value, node.Token, Me).WithConvertedTriviaFrom(node.Token)
                End If

                If node.IsKind(CS.SyntaxKind.DefaultLiteralExpression) Then
                    Select Case node.Parent.RawKind
                        Case CS.SyntaxKind.ReturnStatement
                            Dim FnStmt As CSS.MethodDeclarationSyntax = node.Parent.GetAncestor(Of CSS.MethodDeclarationSyntax)
                            If FnStmt IsNot Nothing Then
                                Return Factory.CTypeExpression(
                                                    NothingExpression,
                                                    CType(FnStmt.ReturnType.Accept(Me), TypeSyntax)
                                                    )
                            End If
                        Case CS.SyntaxKind.EqualsValueClause
                            Dim LocalDecStmt As CSS.LocalDeclarationStatementSyntax = node.Parent.GetAncestor(Of CSS.LocalDeclarationStatementSyntax)
                            If LocalDecStmt IsNot Nothing Then
                                If LocalDecStmt.Declaration.Type IsNot Nothing Then
                                    Return Factory.CTypeExpression(
                                                        NothingExpression,
                                                        CType(LocalDecStmt.Declaration.Type.Accept(Me).WithoutLeadingTrivia, TypeSyntax)
                                                        )
                                End If
                            End If
                            Dim VariableDeclaration As CSS.VariableDeclarationSyntax = node.Parent.GetAncestor(Of CSS.VariableDeclarationSyntax)
                            If VariableDeclaration IsNot Nothing Then
                                If VariableDeclaration.Type IsNot Nothing Then
                                    Return Factory.CTypeExpression(
                                                        NothingExpression,
                                                        CType(VariableDeclaration.Type.Accept(Me), TypeSyntax)
                                                        )
                                End If
                            ElseIf node.Parent.Parent.RawKind = CS.SyntaxKind.Parameter Then
                                Dim Parameter As CSS.ParameterSyntax = CType(node.Parent.Parent, CSS.ParameterSyntax)
                                If Parameter.Type IsNot Nothing Then
                                    Return Factory.CTypeExpression(
                                                        NothingExpression,
                                                        CType(Parameter.Type.Accept(Me), TypeSyntax)
                                                        )
                                End If
                            Else
                                Stop
                            End If
                        Case CS.SyntaxKind.SimpleAssignmentExpression
                            Dim LeftNodeTypeInfo As TypeInfo = _mSemanticModel.GetTypeInfo(DirectCast(node.Parent, CSS.AssignmentExpressionSyntax).Left)
                            If LeftNodeTypeInfo.Type Is Nothing OrElse LeftNodeTypeInfo.Type.IsErrorType Then
                                Return NothingExpression
                            End If
                            If LeftNodeTypeInfo.Type?.IsTupleType Then
                                Dim ElementList As List(Of String) = LeftNodeTypeInfo.Type.ToString.ConvertTypeTupleToTypeStrings(True)
                                Return Factory.CTypeExpression(NothingExpression,
                                                               Factory.ParseTypeName($"({String.Join(", ", ElementList)})"))
                            Else
                                Return If(LeftNodeTypeInfo.Type.Name Is "",
                                            NothingExpression,
                                            CType(Factory.CTypeExpression(NothingExpression, ConvertToType(LeftNodeTypeInfo.Type.Name)), ExpressionSyntax))
                            End If
                        Case CS.SyntaxKind.ConditionalExpression
                            Dim LeftNodeTypeInfo As TypeInfo = _mSemanticModel.GetTypeInfo(DirectCast(node.Parent, CSS.ConditionalExpressionSyntax).WhenTrue)
                            If LeftNodeTypeInfo.Type Is Nothing OrElse LeftNodeTypeInfo.Type.IsErrorType Then
                                Return NothingExpression
                            End If
                            Dim _Type As TypeSyntax = If(LeftNodeTypeInfo.Type.IsTupleType, LeftNodeTypeInfo.Type.ToString.ConvertCSStringToName, ConvertToType(LeftNodeTypeInfo.Type.Name))
                            Return Factory.CTypeExpression(
                                                      NothingExpression,
                                                      _Type
                                                      )
                        Case CS.SyntaxKind.EqualsExpression, CS.SyntaxKind.NotEqualsExpression
                            Return Factory.CTypeExpression(
                                                      NothingExpression,
                                                      Factory.PredefinedType(BooleanKeyword)
                                                      )
                        Case CS.SyntaxKind.Argument, CS.SyntaxKind.SimpleLambdaExpression
                            Return NothingExpression
                        Case CS.SyntaxKind.ArrowExpressionClause, CS.SyntaxKind.CoalesceExpression, CS.SyntaxKind.ParenthesizedLambdaExpression, CS.SyntaxKind.SwitchExpressionArm
                            ' TODO Handle better
                            Return NothingExpression
                        Case CS.SyntaxKind.SuppressNullableWarningExpression
                            ' TODO Handle better
                            Return NothingExpression
                        Case CS.SyntaxKind.ArrayInitializerExpression
                            Return NothingExpression
                        Case Else
                            Stop
                    End Select
                    ' TODO Handle better
                    Return NothingExpression
                End If

                If node.IsKind(CS.SyntaxKind.CharacterLiteralExpression) Then
                    If node.Token.Text.RemoveAll("'").Length <= 2 Then
                        Return GetLiteralExpression(node.Token.Value, node.Token, Me).WithConvertedTriviaFrom(node.Token)
                    End If
                End If
                If node.Token.ValueText.Contains("\", StringComparison.Ordinal) Then
                    Stop
                End If

                Return GetLiteralExpression(node.Token.Value, node.Token, Me).WithConvertedTriviaFrom(node.Token)
            End Function

            Public Overrides Function VisitMemberAccessExpression(node As CSS.MemberAccessExpressionSyntax) As VB.VisualBasicSyntaxNode
                If node.IsKind(CS.SyntaxKind.PointerMemberAccessExpression) Then
                    Dim StatementWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                    StatementWithIssues.AddMarker(FlagUnsupportedStatements(StatementWithIssues, $"Pointer Member Access Expressions", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=False)
                End If

                Dim Expression As ExpressionSyntax = DirectCast(node.Expression.Accept(Me), ExpressionSyntax)

                Dim expressionTrailingTrivia As SyntaxTriviaList = Expression.GetTrailingTrivia
                If TypeOf Expression Is NewExpressionSyntax AndAlso TypeOf Expression IsNot ArrayCreationExpressionSyntax Then
                    Dim expressionWithTrivia As ExpressionSyntax = CType(node.Expression.Accept(Me), ExpressionSyntax)
                    Return Me.WrapTypedNameIfNecessary(Factory.MemberAccessExpression(VB.SyntaxKind.SimpleMemberAccessExpression,
                                                                                        expressionWithTrivia.AdjustExpressionTrivia(AdjustLeading:=True),
                                                                                        DotToken,
                                                                                        CType(node.Name.Accept(Me), SimpleNameSyntax)
                                                                                        ),
                                                       node).WithConvertedTriviaFrom(node)
                ElseIf TypeOf Expression Is CollectionInitializerSyntax Then
                    Dim UniqueName As String = node.GetUniqueVariableNameInScope("tempVar", _mSemanticModel)
                    Dim UniqueIdentifier As IdentifierNameSyntax = Factory.IdentifierName(Factory.Identifier(UniqueName))
                    Dim Initializer As EqualsValueSyntax = Factory.EqualsValue(Expression)
                    Dim DimStatement As LocalDeclarationStatementSyntax =
                            FactoryDimStatement(UniqueName, asClause:=Nothing, Initializer).WithLeadingTrivia(Expression.GetLeadingTrivia).WithTrailingEOL
                    Dim StatementWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                    StatementWithIssues.AddMarker(DimStatement, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                    Expression = UniqueIdentifier.WithTriviaFrom(Expression)
                End If

                Dim NeedOperatorEOL As Boolean = False
                Dim NewNameLeadingTrivia As New SyntaxTriviaList
                Dim operatorTrailingTrivia As New SyntaxTriviaList
                Dim OldNameLeadingTrivia As SyntaxTriviaList = node.Name.GetLeadingTrivia.ConvertTriviaList()
                If OldNameLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    Dim initialTrailingTrivia As SyntaxTriviaList = node.OperatorToken.TrailingTrivia.ConvertTriviaList()

                    For Each t As SyntaxTrivia In initialTrailingTrivia
                        Select Case t.RawKind
                            Case VB.SyntaxKind.WhitespaceTrivia
                                operatorTrailingTrivia = operatorTrailingTrivia.Add(t)
                            Case VB.SyntaxKind.EndOfLineTrivia
                                NeedOperatorEOL = True
                            Case Else
                                Stop
                        End Select
                    Next
                    For Each e As IndexClass(Of SyntaxTrivia) In OldNameLeadingTrivia.WithIndex
                        Dim t As SyntaxTrivia = e.Value
                        Select Case t.RawKind
                            Case VB.SyntaxKind.WhitespaceTrivia
                                NewNameLeadingTrivia = NewNameLeadingTrivia.Add(t)
                            Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                                operatorTrailingTrivia = operatorTrailingTrivia.Add(t)
                            Case VB.SyntaxKind.EndOfLineTrivia
                                ' Ignore
                            Case Else
                                Stop
                        End Select
                    Next
                Else
                    operatorTrailingTrivia = node.Name.GetLeadingTrivia.ConvertTriviaList()
                End If
                If NeedOperatorEOL Then
                    operatorTrailingTrivia = operatorTrailingTrivia.Add(VBEOLTrivia)
                End If
                Dim OperatorToken As SyntaxToken = DotToken.With(node.OperatorToken.LeadingTrivia.ConvertTriviaList(), operatorTrailingTrivia)
                Dim Name As SimpleNameSyntax = DirectCast(node.Name.Accept(Me).With(NewNameLeadingTrivia, node.Name.GetTrailingTrivia.ConvertTriviaList()), SimpleNameSyntax)
                Dim ValueText As String = Name.Identifier.ValueText
                Dim keywordKind As VB.SyntaxKind = VB.SyntaxFacts.GetKeywordKind(ValueText)
                If VB.SyntaxFacts.IsKeywordKind(keywordKind) Then
                    Dim nameSyntax As IdentifierNameSyntax = Factory.IdentifierName($"[{ValueText}]")
                    Name = Name.WithIdentifier(nameSyntax.Identifier)
                End If
                operatorTrailingTrivia = New SyntaxTriviaList

                If expressionTrailingTrivia.LastOrDefault.IsKind(VB.SyntaxKind.EndOfLineTrivia) AndAlso Not expressionTrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                    expressionTrailingTrivia = expressionTrailingTrivia.InsertRange(expressionTrailingTrivia.Count - 1, {Factory.Space, LineContinuation})
                    Expression = Expression.WithTrailingTrivia(expressionTrailingTrivia)
                    OperatorToken = OperatorToken.AdjustTokenLeadingTrivia()
                ElseIf Expression.GetLastToken.ContainsEOLTrivia Then
                    Dim FoundEOL As Boolean = False
                    FoundEOL = RestructureMemberAccessExpressionTrivia(node,
                                                                       Factory.TriviaList(expressionTrailingTrivia),
                                                                       FoundEOL,
                                                                       operatorTrailingTrivia)
                    FoundEOL = RestructureMemberAccessExpressionTrivia(node,
                                                                       OperatorToken.LeadingTrivia,
                                                                       FoundEOL,
                                                                       operatorTrailingTrivia)

                    If FoundEOL Then
                        operatorTrailingTrivia = operatorTrailingTrivia.Add(VBEOLTrivia)
                    End If
                    Expression = Expression.WithoutTrailingTrivia
                    OperatorToken = OperatorToken.WithoutTrivia.WithTrailingTrivia(operatorTrailingTrivia)
                    Name = Name.WithLeadingTrivia(Factory.Space)
                End If
                Dim expressionName As MemberAccessExpressionSyntax = Factory.MemberAccessExpression(VB.SyntaxKind.SimpleMemberAccessExpression, Expression, OperatorToken, Name)
                Return Me.WrapTypedNameIfNecessary(expressionName, node).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitMemberBindingExpression(node As CSS.MemberBindingExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim name As SimpleNameSyntax = DirectCast(node.Name.Accept(Me), SimpleNameSyntax)
                Return Factory.SimpleMemberAccessExpression(name:=name)
            End Function

            Public Overrides Function VisitObjectCreationExpression(node As CSS.ObjectCreationExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim type1 As TypeSyntax = GetTypeSyntaxFromPossibleAddressOf(node.Type.Accept(Me))

                Dim argumentList As ArgumentListSyntax = DirectCast(node.ArgumentList?.Accept(Me), ArgumentListSyntax)
                If argumentList IsNot Nothing Then
                    If type1.ToString.EndsWith("EventHandler", StringComparison.Ordinal) AndAlso
                        argumentList.Arguments.Count = 1 Then
                        argumentList = Factory.ArgumentList(Factory.SingletonSeparatedList(Of ArgumentSyntax)(Factory.SimpleArgument(Factory.AddressOfExpression(DirectCast(argumentList.Arguments(0), SimpleArgumentSyntax).Expression))))
                    End If
                    type1 = type1.WithTrailingTrivia(Factory.Space)
                End If
                Dim PossibleInitializer As VB.VisualBasicSyntaxNode = node.Initializer?.Accept(Me)
                Dim initializer As ObjectCollectionInitializerSyntax = Nothing
                If PossibleInitializer IsNot Nothing Then
                    type1 = type1.WithTrailingTrivia(Factory.Space)
                    Select Case PossibleInitializer.Kind
                        Case VB.SyntaxKind.CollectionInitializer
                            initializer = Factory.ObjectCollectionInitializer(initializer:=DirectCast(PossibleInitializer, CollectionInitializerSyntax))
                        Case VB.SyntaxKind.ObjectCollectionInitializer
                            initializer = DirectCast(PossibleInitializer, ObjectCollectionInitializerSyntax)
                        Case VB.SyntaxKind.ObjectMemberInitializer
                            ' Remove trailing trivia before with
                            If argumentList IsNot Nothing Then
                                argumentList = argumentList.WithCloseParenToken(CloseParenToken)
                            End If
                            Dim memberinitializer As ObjectMemberInitializerSyntax = DirectCast(PossibleInitializer, ObjectMemberInitializerSyntax)
                            Return Factory.ObjectCreationExpression(NewKeyword, Factory.List(Of AttributeListSyntax)(), type1.WithTrailingTrivia(Factory.Space), argumentList, memberinitializer)

                        Case Else
                            _reportException?.Invoke(UnexpectedValue(NameOf(PossibleInitializer)))
                    End Select
                End If
                If argumentList IsNot Nothing AndAlso initializer?.GetFirstToken.IsKind(VB.SyntaxKind.FromKeyword) Then
                    argumentList = argumentList.WithTrailingTrivia(Factory.Space)
                End If
                If argumentList?.ContainsDirectives Then
                    Dim NewArgumentList As New List(Of ArgumentSyntax)
                    Dim NewSeparatorList As New List(Of SyntaxToken)
                    Dim FoundEOL As Boolean = False

                    For index As Integer = 0 To argumentList.Arguments.Count - 2
                        NewArgumentList.Add(argumentList.Arguments(index).RemoveDirectiveTrivia(FoundEOL))
                        NewSeparatorList.Add(argumentList.Arguments.GetSeparator(index).RemoveDirectiveTrivia(FoundEOL))
                    Next
                    NewArgumentList.Add(argumentList.Arguments(argumentList.Arguments.Count - 1).RemoveDirectiveTrivia(FoundEOL))
                    argumentList = argumentList.WithArguments(Factory.SeparatedList(NewArgumentList, NewSeparatorList))
                End If
                If argumentList?.Arguments.Count = 0 Then
                    argumentList = Nothing
                End If
                Return Factory.ObjectCreationExpression(NewKeyword.WithConvertedTriviaFrom(node.NewKeyword), Factory.List(Of AttributeListSyntax)(), type1, argumentList, initializer)
            End Function

            Public Overrides Function VisitParenthesizedExpression(node As CSS.ParenthesizedExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim Expression As ExpressionSyntax = DirectCast(node.Expression.Accept(Me), ExpressionSyntax)
                If TypeOf Expression Is CTypeExpressionSyntax OrElse
                   TypeOf Expression Is IdentifierNameSyntax OrElse
                   TypeOf Expression Is InvocationExpressionSyntax OrElse
                   TypeOf Expression Is QueryExpressionSyntax OrElse
                    TypeOf Expression Is TryCastExpressionSyntax Then
                    Return Expression.WithTrailingTrivia(Factory.Space)
                End If
                Dim DeclarationToBeAdded As LocalDeclarationStatementSyntax
                If TypeOf node.Parent Is CSS.MemberAccessExpressionSyntax OrElse
                   TypeOf node.Parent Is CSS.ElementAccessExpressionSyntax OrElse
                    TypeOf node.Parent Is CSS.ConditionalAccessExpressionSyntax Then
                    ' Statement with issues points to "Statement" Probably an Expression Statement. If this is part of a single Line If we need to go higher
                    Dim StatementWithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                    ' Statement with issues points to "Statement" Probably an Expression Statement. If this is part of an ElseIf we need to go higher
                    Dim Initializer As EqualsValueSyntax = Factory.EqualsValue(Expression)
                    If TypeOf node.Parent Is CSS.MemberAccessExpressionSyntax OrElse TypeOf node.Parent Is CSS.ElementAccessExpressionSyntax Then
                        If node.Expression.IsKind(CS.SyntaxKind.AddExpression) Then
                            Return Factory.ParenthesizedExpression(OpenParenToken.WithConvertedTriviaFrom(node.OpenParenToken), Expression, CloseParenToken.WithConvertedTriviaFrom(node.CloseParenToken))
                        End If
                        Dim UniqueName As String = node.GetUniqueVariableNameInScope("tempVar", _mSemanticModel)
                        Dim UniqueIdentifier As IdentifierNameSyntax = Factory.IdentifierName(Factory.Identifier(UniqueName))
                        If TypeOf Expression Is TernaryConditionalExpressionSyntax Then
                            Dim TExpression As TernaryConditionalExpressionSyntax = DirectCast(Expression, TernaryConditionalExpressionSyntax)
                            If TExpression.Condition.IsKind(VB.SyntaxKind.IdentifierName) Then
                                Dim IfStatement As IfStatementSyntax =
                                   Factory.IfStatement(IfKeyword, TExpression.Condition, ThenKeyword).WithConvertedLeadingTriviaFrom(node)
                                Dim IfBlockStatements As New SyntaxList(Of StatementSyntax)
                                IfBlockStatements = IfBlockStatements.Add(Factory.SimpleAssignmentStatement(left:=UniqueIdentifier, right:=TExpression.WhenTrue).WithoutLastLineContinuation)
                                Dim ElseBlockStatements As New SyntaxList(Of StatementSyntax)
                                ElseBlockStatements = ElseBlockStatements.Add(Factory.SimpleAssignmentStatement(left:=UniqueIdentifier, right:=TExpression.WhenFalse).WithoutLastLineContinuation)
                                Dim ElseBlock As ElseBlockSyntax = Factory.ElseBlock(ElseBlockStatements)
                                Dim IfBlockToBeAdded As StatementSyntax = Factory.MultiLineIfBlock(
                                                                    IfStatement,
                                                                    IfBlockStatements,
                                                                    Nothing,
                                                                    ElseBlock,
                                                                    Factory.EndIfStatement(EndKeyword.WithTrailingTrivia(Factory.Space), IfKeyword).
                                                                                                    WithConvertedTrailingTriviaFrom(node).
                                                                                                    WithTrailingEOL)

                                DeclarationToBeAdded = FactoryDimStatement(UniqueName, asClause:=Nothing, initializer:=Nothing).WithPrependedLeadingTrivia(Factory.CommentTrivia($" ' TODO: Check, VB does not directly support MemberAccess off a Conditional If Expression")).WithTrailingEOL

                                StatementWithIssue.AddMarker(DeclarationToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                                StatementWithIssue.AddMarker(IfBlockToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                                Return UniqueIdentifier
                            End If
                        ElseIf TypeOf Expression Is BinaryConditionalExpressionSyntax Then
                            Dim BExpression As BinaryConditionalExpressionSyntax = DirectCast(Expression, BinaryConditionalExpressionSyntax)
                            If BExpression.FirstExpression.IsKind(VB.SyntaxKind.IdentifierName) Then
                                Dim FirstIdentifier As IdentifierNameSyntax = DirectCast(BExpression.FirstExpression, IdentifierNameSyntax)
                                Dim IfStatement As IfStatementSyntax =
                                   Factory.IfStatement(IfKeyword, Factory.IsExpression(left:=FirstIdentifier, right:=NothingExpression), ThenKeyword).WithConvertedLeadingTriviaFrom(node)
                                Dim Statements As New SyntaxList(Of StatementSyntax)
                                Statements = Statements.Add(Factory.SimpleAssignmentStatement(left:=FirstIdentifier, right:=BExpression.SecondExpression))

                                Dim IfBlockToBeAdded As StatementSyntax = Factory.MultiLineIfBlock(
                                                                    ifStatement:=IfStatement,
                                                                    statements:=Statements,
                                                                    elseIfBlocks:=Nothing,
                                                                    elseBlock:=Nothing,
                                                                    Factory.EndIfStatement(EndKeyword.WithTrailingTrivia(Factory.Space), IfKeyword).WithConvertedTrailingTriviaFrom(node).WithTrailingEOL).
                                 WithPrependedLeadingTrivia(Factory.CommentTrivia($"' TODO: Check, VB does not directly support MemberAccess off a Conditional If Expression")).WithTrailingEOL
                                StatementWithIssue.AddMarker(IfBlockToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                                Return UniqueIdentifier
                            End If
                        End If
                        Dim AwaitNotSupported As String = ""
                        If Initializer.Value.IsKind(VB.SyntaxKind.AwaitExpression) AndAlso Not IsDecedentOfAsyncMethod(node) Then
                            Initializer = Factory.EqualsValue(CType(Initializer.Value, AwaitExpressionSyntax).Expression)
                            AwaitNotSupported = " Await removed, in non Async Function,"
                        End If
                        DeclarationToBeAdded = FactoryDimStatement(UniqueName, asClause:=Nothing, Initializer).WithPrependedLeadingTrivia(Factory.CommentTrivia($" ' TODO: Check,{AwaitNotSupported} VB does not directly support MemberAccess off a Conditional If Expression")).WithTrailingEOL
                        StatementWithIssue.AddMarker(DeclarationToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=False)
                        Return UniqueIdentifier
                    ElseIf TypeOf node.Parent Is CSS.ConditionalAccessExpressionSyntax Then
                        Dim UniqueName As String = node.GetUniqueVariableNameInScope("tempVar", _mSemanticModel)
                        DeclarationToBeAdded = FactoryDimStatement(UniqueName, asClause:=Nothing, Initializer).WithConvertedTriviaFrom(node).WithTrailingEOL
                        StatementWithIssue.AddMarker(DeclarationToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=False)
                        Return Factory.IdentifierName(Factory.Identifier(UniqueName))
                    End If
                End If

                Return Factory.ParenthesizedExpression(Expression.WithoutTrailingTrivia).WithTrailingTrivia(Expression.GetTrailingTrivia)
            End Function

            Public Overrides Function VisitParenthesizedLambdaExpression(node As CSS.ParenthesizedLambdaExpressionSyntax) As VB.VisualBasicSyntaxNode
                Return Me.ConvertLambdaExpression(node, node.Body, node.ParameterList.Parameters, Factory.TokenList(node.AsyncKeyword))
            End Function

            Public Overrides Function VisitPostfixUnaryExpression(node As CSS.PostfixUnaryExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim CSExpressionKind As CS.SyntaxKind = CS.CSharpExtensions.Kind(node)
                Dim vbOperandExpression As ExpressionSyntax = DirectCast(node.Operand.Accept(Me), ExpressionSyntax)
                If CSExpressionKind = CS.SyntaxKind.SuppressNullableWarningExpression Then
                    Return vbOperandExpression
                End If
                Dim kind As VB.SyntaxKind = GetExpressionKind(CSExpressionKind)
                If TypeOf node.Parent Is CSS.ExpressionStatementSyntax OrElse TypeOf node.Parent Is CSS.ForStatementSyntax Then
                    Try
                        Dim operandTypeInfo As TypeInfo = _mSemanticModel.GetTypeInfo(node.Operand)
                        If operandTypeInfo.ConvertedType.ToString = "char" Then
                            Dim convertedperandExpression As ExpressionSyntax = Factory.ParseExpression($"ChrW(AscW({vbOperandExpression}))").WithTriviaFrom(vbOperandExpression)
                            kind = If(kind = VB.SyntaxKind.AddAssignmentStatement, VB.SyntaxKind.AddExpression, VB.SyntaxKind.SubtractExpression)
                            Dim mathExpression As ExpressionSyntax = Factory.BinaryExpression(kind, convertedperandExpression, GetOperatorToken(kind, IsReferenceType:=False), ExpressionD1)
                            Return Factory.AssignmentStatement(VB.SyntaxKind.SimpleAssignmentStatement,
                                                               vbOperandExpression,
                                                               EqualsToken,
                                                               mathExpression)

                        End If
                    Catch ex As OperationCanceledException
                        Throw
                    Catch ex As Exception
                        ' ignore
                    End Try

                    Return Factory.AssignmentStatement(GetExpressionKind(CS.CSharpExtensions.Kind(node)),
                                                            vbOperandExpression,
                                                            GetOperatorToken(kind, IsReferenceType:=False),
                                                            ExpressionD1)
                Else
                    Dim OperatorName As String
                    Dim minMax As String
                    Dim op As VB.SyntaxKind
                    If kind = VB.SyntaxKind.AddAssignmentStatement Then
                        OperatorName = "Increment"
                        minMax = "Min"
                        op = VB.SyntaxKind.SubtractExpression
                    Else
                        OperatorName = "Decrement"
                        minMax = "Max"
                        op = VB.SyntaxKind.AddExpression
                    End If
                    Dim vbMathExpression As NameSyntax = Factory.ParseName("Math." & minMax)
                    Dim vbInterlockedExpressionName As NameSyntax = Factory.ParseName("Threading.Interlocked." & OperatorName)

                    Dim vbOperandArgument As SimpleArgumentSyntax = Factory.SimpleArgument(vbOperandExpression)

                    Dim vbOperandArgumentList As ArgumentListSyntax = Factory.ArgumentList(Factory.SingletonSeparatedList(Of ArgumentSyntax)(vbOperandArgument))
                    Dim vbArgumentInvocationExpression As InvocationExpressionSyntax = Factory.InvocationExpression(vbInterlockedExpressionName, vbOperandArgumentList)
                    Dim vbSecondArgumentSyntax As SimpleArgumentSyntax = Factory.SimpleArgument(Factory.BinaryExpression(op,
                                                                                                                                   vbOperandExpression,
                                                                                                                                   GetOperatorToken(op, IsReferenceType:=False),
                                                                                                                                   ExpressionD1)
                                                                                                                                   )
                    Return Factory.InvocationExpression(
                        vbMathExpression,
                        Factory.ArgumentList(Factory.SeparatedList(New ArgumentSyntax() {Factory.SimpleArgument(vbArgumentInvocationExpression),
                                                                                                      vbSecondArgumentSyntax}))).WithConvertedTriviaFrom(node)
                End If
            End Function

            Public Overrides Function VisitPrefixUnaryExpression(node As CSS.PrefixUnaryExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim kind As VB.SyntaxKind = GetExpressionKind(CS.CSharpExtensions.Kind(node))
                If kind = CS.SyntaxKind.PointerIndirectionExpression Then
                    Dim StatementWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                    StatementWithIssues.AddMarker(FlagUnsupportedStatements(StatementWithIssues, "IndirectPointer Expressions", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=False)
                    Return NothingExpression
                End If
                If kind = CS.SyntaxKind.IndexExpression Then
                    Dim Operand As VB.VisualBasicSyntaxNode = node.Operand.Accept(Me)
                    Return Factory.ParseExpression($"-{Operand}")
                    Stop
                End If
                Dim vbOperandExpression As ExpressionSyntax = DirectCast(node.Operand.Accept(Me), ExpressionSyntax)
                If TypeOf node.Parent Is CSS.ExpressionStatementSyntax Then
                    Return Factory.AssignmentStatement(kind,
                                                       vbOperandExpression,
                                                       GetOperatorToken(kind, IsReferenceType:=False),
                                                       ExpressionD1).WithConvertedTriviaFrom(node)
                End If
                If kind = VB.SyntaxKind.AddAssignmentStatement OrElse kind = VB.SyntaxKind.SubtractAssignmentStatement Then
                    If node.Parent.IsKind(CS.SyntaxKind.ForStatement) Then
                        If kind = VB.SyntaxKind.AddAssignmentStatement Then
                            Return Factory.AddAssignmentStatement(vbOperandExpression.WithTrailingTrivia(Factory.Space),
                                                                    GetOperatorToken(kind, IsReferenceType:=False),
                                                                    ExpressionD1).WithConvertedTriviaFrom(node)
                        Else
                            Return Factory.SubtractAssignmentStatement(vbOperandExpression.WithTrailingTrivia(Factory.Space),
                                                                             GetOperatorToken(kind, IsReferenceType:=False),
                                                                             ExpressionD1).WithConvertedTriviaFrom(node)
                        End If
                    Else
                        Dim operatorName As String = If(kind = VB.SyntaxKind.AddAssignmentStatement, "Increment", "Decrement")
                        Dim MathExpression As NameSyntax = Factory.ParseName("Threading.Interlocked." & operatorName)
                        Return Factory.InvocationExpression(MathExpression,
                                                              Factory.ArgumentList(Factory.SeparatedList(
                                                                                        New ArgumentSyntax() {Factory.SimpleArgument(vbOperandExpression)})
                                                                                    )
                                                             )
                    End If
                End If
                If kind = VB.SyntaxKind.AddressOfExpression Then
                    Dim SpaceTriviaList As SyntaxTriviaList
                    SpaceTriviaList = SpaceTriviaList.Add(Factory.Space)
                    Dim AddressOfToken As SyntaxToken = AddressOfKeyword.With(SpaceTriviaList, SpaceTriviaList)
                    Return Factory.AddressOfExpression(AddressOfToken, vbOperandExpression).WithConvertedTriviaFrom(node)
                End If
                Return Factory.UnaryExpression(kind,
                                               GetOperatorToken(kind, IsReferenceType:=False),
                                               vbOperandExpression.WithLeadingTrivia(Factory.Space)
                                              ).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitSimpleLambdaExpression(node As CSS.SimpleLambdaExpressionSyntax) As VB.VisualBasicSyntaxNode
                Return Me.ConvertLambdaExpression(node, node.Body, Factory.SingletonSeparatedList(node.Parameter), Factory.TokenList(node.AsyncKeyword)).WithConvertedTriviaFrom(node)
            End Function

            ''' <summary>
            ''' Maps sizeof to Len(New {Type})
            ''' </summary>
            ''' <param name="node"></param>
            ''' <returns></returns>ThrowExpressionSyntax
            ''' <remarks>Added by PC</remarks>
            Public Overrides Function VisitSizeOfExpression(node As CSS.SizeOfExpressionSyntax) As VB.VisualBasicSyntaxNode
                Return Factory.ParseExpression($"Len(New {node.Type}()) ")
            End Function

            Public Overrides Function VisitThisExpression(node As CSS.ThisExpressionSyntax) As VB.VisualBasicSyntaxNode
                Return Factory.MeExpression().WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitThrowExpression(node As CSS.ThrowExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim Expression As ExpressionSyntax = DirectCast(node.Expression.Accept(Me), ExpressionSyntax)
                Dim ThrowStatement As ThrowStatementSyntax = Factory.ThrowStatement(Expression)
                Dim ParentNode As SyntaxNode = node.Parent
                Return ThrowStatement.WithTrailingEOL
            End Function

            Public Overrides Function VisitTupleElement(node As CSS.TupleElementSyntax) As VB.VisualBasicSyntaxNode
                Try
                    If String.IsNullOrWhiteSpace(node.Identifier.ValueText) Then
                        Dim typedTupleElementSyntax1 As TypedTupleElementSyntax = Factory.TypedTupleElement(DirectCast(node.Type.Accept(Me), TypeSyntax))
                        Return typedTupleElementSyntax1
                    End If
                    Return Factory.NamedTupleElement(GenerateSafeVBToken(node.Identifier, node, _mSemanticModel).WithConvertedTriviaFrom(node.Type), Factory.SimpleAsClause(AsKeyword.WithTrailingTrivia(Factory.Space), attributeLists:=New SyntaxList(Of AttributeListSyntax), DirectCast(node.Type.Accept(Me).WithConvertedTriviaFrom(node.Identifier), TypeSyntax)))
                Catch ex As OperationCanceledException
                    Throw
                Catch ex As Exception
                    Stop
                    Throw
                End Try
                Throw UnreachableException
            End Function

            ''' <summary>
            ''' This only returns for Names VB
            ''' </summary>
            ''' <param name="node"></param>
            ''' <returns></returns>
            Public Overrides Function VisitTupleExpression(node As CSS.TupleExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim lArgumentSyntax As New List(Of SimpleArgumentSyntax)
                If TypeOf node.Arguments(0).Expression IsNot CSS.DeclarationExpressionSyntax Then
                    For Each e As IndexClass(Of CSS.ArgumentSyntax) In node.Arguments.WithIndex
                        Dim argument As SimpleArgumentSyntax = DirectCast(e.Value.Accept(Me), SimpleArgumentSyntax)
                        Dim afterWhiteSpace As Boolean = False
                        Dim initialTriviaList As SyntaxTriviaList = argument.GetLeadingTrivia
                        Dim finalLeadingTrivia As New SyntaxTriviaList
                        For j As Integer = 0 To initialTriviaList.Count - 1
                            Dim Trivia As SyntaxTrivia = initialTriviaList(j)
                            Select Case Trivia.RawKind
                                Case VB.SyntaxKind.WhitespaceTrivia
                                    afterWhiteSpace = True
                                    finalLeadingTrivia = finalLeadingTrivia.Add(Trivia)
                                Case VB.SyntaxKind.EndOfLineTrivia
                                    finalLeadingTrivia = finalLeadingTrivia.Add(Trivia)
                                    afterWhiteSpace = False
                                    If j < initialTriviaList.Count - 1 Then
                                        If finalLeadingTrivia.Count = 0 Then
                                            finalLeadingTrivia = finalLeadingTrivia.Add(Factory.Space)
                                            finalLeadingTrivia = finalLeadingTrivia.Add(LineContinuation)
                                        End If
                                    End If
                                Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                                    If Not finalLeadingTrivia.Last.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                                        If Not afterWhiteSpace Then
                                            finalLeadingTrivia = finalLeadingTrivia.Add(Factory.Space)
                                        End If
                                        finalLeadingTrivia = finalLeadingTrivia.Add(LineContinuation)
                                    End If

                                    finalLeadingTrivia = finalLeadingTrivia.Add(Trivia)
                                    If j < initialTriviaList.Count - 1 AndAlso Not initialTriviaList(j + 1).IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                        finalLeadingTrivia = finalLeadingTrivia.Add(VBEOLTrivia)
                                    End If
                                Case VB.SyntaxKind.DisableWarningDirectiveTrivia
                                    GetStatementwithIssues(node).AddMarker(Factory.EmptyStatement.WithLeadingTrivia(Trivia), StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                                Case VB.SyntaxKind.EnableWarningDirectiveTrivia
                                    GetStatementwithIssues(node).AddMarker(Factory.EmptyStatement.WithLeadingTrivia(Trivia), StatementHandlingOption.AppendEmptyStatement, AllowDuplicates:=True)
                                Case VB.SyntaxKind.LineContinuationTrivia
                                    If finalLeadingTrivia.Last.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                                        Continue For
                                    End If
                                    afterWhiteSpace = False
                                    finalLeadingTrivia = finalLeadingTrivia.Add(LineContinuation)
                                Case Else
                                    Stop
                            End Select
                        Next
                        lArgumentSyntax.Add(argument.WithLeadingTrivia(finalLeadingTrivia))
                    Next
                    Return Factory.TupleExpression(Factory.SeparatedList(lArgumentSyntax))
                End If
                For Each a As CSS.ArgumentSyntax In node.Arguments
                    Dim Identifier As IdentifierNameSyntax
                    If a.Expression.IsKind(CS.SyntaxKind.IdentifierName) Then
                        Identifier = DirectCast(DirectCast(a.Expression, CSS.IdentifierNameSyntax).Accept(Me), IdentifierNameSyntax)
                    Else
                        Dim d As CSS.DeclarationExpressionSyntax = DirectCast(a.Expression, CSS.DeclarationExpressionSyntax)
                        Identifier = DirectCast(d.Designation.Accept(Me), IdentifierNameSyntax)
                    End If
                    lArgumentSyntax.Add(Factory.SimpleArgument(Identifier))
                Next
                Return Factory.TupleExpression(Factory.SeparatedList(lArgumentSyntax))
            End Function

            Public Overrides Function VisitTupleType(node As CSS.TupleTypeSyntax) As VB.VisualBasicSyntaxNode
                Dim tupleElementList As New List(Of TupleElementSyntax)
                tupleElementList.AddRange(node.Elements.Select(Function(a As CSS.TupleElementSyntax) DirectCast(a.Accept(Me), TupleElementSyntax)))
                Return Factory.TupleType(tupleElementList.ToArray)
            End Function

            Public Overrides Function VisitTypeOfExpression(node As CSS.TypeOfExpressionSyntax) As VB.VisualBasicSyntaxNode
                If TypeOf node.Type Is CSS.GenericNameSyntax Then
                    Dim NodeType As CSS.GenericNameSyntax = DirectCast(node.Type, CSS.GenericNameSyntax)
                    Dim ArgumentList As CSS.TypeArgumentListSyntax = NodeType.TypeArgumentList
                    If ArgumentList.Arguments.Count = 1 AndAlso ArgumentList.Arguments(0).IsKind(CS.SyntaxKind.OmittedTypeArgument) Then
                        Return Factory.GetTypeExpression(Factory.ParseTypeName($"{NodeType.Identifier.ValueText}()"))
                    End If
                End If
                Return Factory.GetTypeExpression(GetTypeSyntaxFromPossibleAddressOf(node.Type.Accept(Me)))
            End Function

        End Class

    End Class

End Namespace
