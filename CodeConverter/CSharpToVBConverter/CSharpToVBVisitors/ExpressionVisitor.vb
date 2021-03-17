' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Text
Imports Extensions
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax
Imports ProgressReportLibrary
Imports Utilities
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic

Namespace CSharpToVBConverter.CSharpToVBVisitors

    Partial Public Class CSharpConverter

        Partial Friend Class NodesVisitor
            Inherits CS.CSharpSyntaxVisitor(Of VB.VisualBasicSyntaxNode)

            Private Shared Function ConvertConditionalAccessExpressionToComment(node As CSS.ConditionalAccessExpressionSyntax) As SyntaxTriviaList
                Dim leadingTriviaList As SyntaxTriviaList
                Dim stringList As String() = node.ToFullString.SplitLines
                leadingTriviaList = leadingTriviaList.Add(VbEolTrivia)
                leadingTriviaList = leadingTriviaList.Add(Factory.CommentTrivia($"' TODO VB does not allow directives here, original statement"))
                For Each s As String In stringList
                    leadingTriviaList = leadingTriviaList.Add(VbEolTrivia)
                    leadingTriviaList = leadingTriviaList.Add(Factory.CommentTrivia($"'    {s}"))
                Next
                Return leadingTriviaList
            End Function

            Private Shared Function FindMatchingStatement(indexedStatementList As List(Of (Integer, StatementSyntax)), name As SimpleNameSyntax, ByRef statementIndex As Integer, ByRef statement As LocalDeclarationStatementSyntax) As Boolean
                For statementIndex = 0 To indexedStatementList.Count - 1
                    Dim stmtTuple As (nodeIndex As Integer, stmt As StatementSyntax) = indexedStatementList(statementIndex)
                    statement = CType(stmtTuple.stmt, LocalDeclarationStatementSyntax)
                    If statement.Declarators(0).Names(0).Identifier.ValueText = name.Identifier.ValueText Then
                        Return True
                    End If
                Next
                Return False
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
                    lhs = Factory.MemberAccessExpression(VB.SyntaxKind.SimpleMemberAccessExpression,
                                                         If(lhs, CType(Factory.IdentifierName(nameParts(0)), ExpressionSyntax)),
                                                         DotToken,
                                                         Factory.IdentifierName(namePart))
                Next
                Return lhs
            End Function

            Private Shared Function RestructureMemberAccessExpressionTrivia(node As CSS.MemberAccessExpressionSyntax, triviaList As SyntaxTriviaList, foundEol As Boolean, ByRef operatorTrailingTrivia As SyntaxTriviaList) As Boolean
                For Each trivia As SyntaxTrivia In triviaList
                    Select Case trivia.RawKind
                        Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                            operatorTrailingTrivia = operatorTrailingTrivia.Add(trivia)
                            foundEol = True
                        Case VB.SyntaxKind.EndOfLineTrivia
                            foundEol = True
                        Case VB.SyntaxKind.WhitespaceTrivia
                            operatorTrailingTrivia = operatorTrailingTrivia.Add(SpaceTrivia)
                        Case VB.SyntaxKind.DisableWarningDirectiveTrivia,
                             VB.SyntaxKind.EnableWarningDirectiveTrivia
                            ' Ignore
                        Case VB.SyntaxKind.IfDirectiveTrivia
                            Dim stmtWithIssues As CS.CSharpSyntaxNode = GetStatementWithIssues(node)
                            stmtWithIssues.AddMarker(Factory.EmptyStatement.WithLeadingTrivia(trivia), StatementHandlingOption.PrependStatement, allowDuplicates:=True)
                        Case Else
                            Stop
                    End Select
                Next

                Return foundEol
            End Function

            Private Function ConvertLambdaExpression(node As CSS.AnonymousFunctionExpressionSyntax, block As Object, parameters As SeparatedSyntaxList(Of CSS.ParameterSyntax), modifiers As SyntaxTokenList) As LambdaExpressionSyntax
                Dim vbNodes As New List(Of ParameterSyntax)
                Dim vbSeparators As New List(Of SyntaxToken)
                Dim savedNeedEndUsingCount As Integer = Me.NeededEndUsingCount
                Try
                    Me.NeededEndUsingCount = 0
                    If parameters.Any Then
                        Dim csSeparators As New List(Of SyntaxToken)
                        csSeparators.AddRange(parameters.GetSeparators)

                        For index As Integer = 0 To parameters.SeparatorCount - 1
                            vbNodes.Add(DirectCast(parameters(index).Accept(Me), ParameterSyntax))
                            vbSeparators.Add(CommaToken.WithConvertedTriviaFrom(csSeparators(index)))
                        Next
                        If parameters.Last.Identifier.ValueText <> "_" OrElse parameters.Count > 1 Then
                            vbNodes.Add(DirectCast(parameters.Last.Accept(Me), ParameterSyntax))
                        End If
                    End If

                    Dim isErrorType As Boolean = True
                    Dim lambdaHeader As LambdaHeaderSyntax
                    Dim returnsVoid As Boolean
                    Dim symbol As IMethodSymbol
                    Try
                        symbol = TryCast(_semanticModel.GetSymbolInfo(node).Symbol, IMethodSymbol)
                        returnsVoid = symbol.ReturnsVoid
                        If Not returnsVoid Then
                            isErrorType = Not symbol.ReturnType.IsType
                        End If
                    Catch ex As OperationCanceledException
                        Throw
                    Catch ex As Exception
                        Throw
                    End Try
                    Dim isFunction As Boolean = Not (returnsVoid OrElse TypeOf node.Body Is CSS.AssignmentExpressionSyntax)
                    Dim modifiersList As SyntaxTokenList
                    modifiersList = modifiersList.AddRange(ConvertModifiers(modifiers, Me.IsModule, TokenContext.Local))
                    Dim endSubOrFunctionStatement As EndBlockStatementSyntax
                    Dim parameterList As ParameterListSyntax = Factory.ParameterList(Factory.SeparatedList(vbNodes, vbSeparators))
                    Dim csBraces As (LeftBrace As SyntaxToken, RightBrace As SyntaxToken) = node.Body.GetBraces
                    Dim asClause As AsClauseSyntax = Nothing
                    If isFunction Then
                        Dim doNotAddAsClause As Boolean = isErrorType OrElse symbol.ReturnType.ToString.Contains("?", StringComparison.Ordinal) OrElse symbol.ReturnType.ToString.StartsWith("<anonymous type: ", StringComparison.Ordinal)
                        asClause = If(doNotAddAsClause,
                                    Nothing,
                                    Factory.SimpleAsClause(AsKeyword.With(SpaceTrivia, SpaceTrivia), New SyntaxList(Of AttributeListSyntax), symbol.ReturnType.ConvertToType)
                                    )
                        lambdaHeader = Factory.FunctionLambdaHeader(Factory.List(Of AttributeListSyntax)(), modifiersList, parameterList, asClause:=CType(asClause, SimpleAsClauseSyntax))
                        endSubOrFunctionStatement = Factory.EndFunctionStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), FunctionKeyword).WithConvertedTriviaFrom(csBraces.RightBrace)
                    Else
                        lambdaHeader = Factory.SubLambdaHeader(Factory.List(Of AttributeListSyntax)(), modifiersList, parameterList, asClause:=Nothing)
                        endSubOrFunctionStatement = Factory.EndSubStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), SubKeyword).WithConvertedTriviaFrom(csBraces.RightBrace)
                    End If
                    Dim blockSyntax As CSS.BlockSyntax = TryCast(block, CSS.BlockSyntax)
                    If blockSyntax IsNot Nothing Then
                        block = blockSyntax.Statements
                    End If

                    Dim statements As New SyntaxList(Of StatementSyntax)
                    Dim endBlock As EndBlockStatementSyntax
                    Dim cSharpNode As CS.CSharpSyntaxNode = TryCast(block, CS.CSharpSyntaxNode)
                    If cSharpNode IsNot Nothing Then
                        Dim body As VB.VisualBasicSyntaxNode = cSharpNode.Accept(Me)
                        If TypeOf block Is CSS.ThrowExpressionSyntax Then
                            statements = Factory.SingletonList(DirectCast(body, StatementSyntax).WithTrailingEol)
                            statements = Me.AdjustUsingIfNeeded(statements)
                            If isFunction Then
                                Return Factory.MultiLineLambdaExpression(VB.SyntaxKind.MultiLineFunctionLambdaExpression, lambdaHeader.WithTrailingEol, statements, endSubOrFunctionStatement).WithConvertedTriviaFrom(node)
                            End If
                            Return Factory.MultiLineLambdaExpression(VB.SyntaxKind.MultiLineSubLambdaExpression, lambdaHeader.WithTrailingEol, statements, endSubOrFunctionStatement).WithConvertedTriviaFrom(node)
                        End If
                        If TypeOf block Is CSS.ObjectCreationExpressionSyntax Then
                            If isFunction Then
                                statements = Factory.SingletonList(Of StatementSyntax)(Factory.ReturnStatement(DirectCast(body, NewExpressionSyntax)).WithTrailingEol)
                                statements = Me.AdjustUsingIfNeeded(statements)
                                Return Factory.MultiLineLambdaExpression(VB.SyntaxKind.MultiLineFunctionLambdaExpression, lambdaHeader.WithTrailingEol, statements, endSubOrFunctionStatement).WithConvertedTriviaFrom(node)
                            End If
                            asClause = Factory.AsNewClause(DirectCast(body, NewExpressionSyntax))
                            statements = Factory.SingletonList(Of StatementSyntax)(FactoryDimStatement("DoNotCare", asClause, initializer:=Nothing))
                            statements = Me.AdjustUsingIfNeeded(statements)
                            Return Factory.MultiLineLambdaExpression(VB.SyntaxKind.MultiLineSubLambdaExpression, lambdaHeader.WithTrailingEol, statements, endSubOrFunctionStatement).WithConvertedTriviaFrom(node)
                        End If
                        If body.IsKind(VB.SyntaxKind.SimpleAssignmentStatement) Then
                            Dim simpleAssignment As AssignmentStatementSyntax = DirectCast(body, AssignmentStatementSyntax)
                            If simpleAssignment.Left.IsKind(VB.SyntaxKind.SimpleMemberAccessExpression) Then
                                Dim memberAccessExpression As MemberAccessExpressionSyntax = DirectCast(simpleAssignment.Left, MemberAccessExpressionSyntax)
                                Select Case memberAccessExpression.Expression.Kind
                                    Case VB.SyntaxKind.ObjectCreationExpression, VB.SyntaxKind.SimpleMemberAccessExpression
                                        endBlock = Factory.EndSubStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), SubKeyword).WithTrailingEol
                                        Dim uniqueName As String = node.GetUniqueVariableNameInScope("tempVar", _usedIdentifiers, _semanticModel)
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
                                        statements = statements.Add(Factory.SimpleAssignmentStatement(Factory.QualifiedName(uniqueIdentifier, memberAccessExpression.Name), simpleAssignment.Right).WithTrailingEol)
                                        statements = Me.AdjustUsingIfNeeded(statements)
                                        Return Factory.MultiLineLambdaExpression(VB.SyntaxKind.MultiLineSubLambdaExpression, lambdaHeader.WithTrailingEol, statements, endBlock).WithConvertedTriviaFrom(node)
                                    Case VB.SyntaxKind.IdentifierName, VB.SyntaxKind.InvocationExpression, VB.SyntaxKind.MeExpression
                                        ' handled below
                                    Case Else
                                        Stop
                                End Select
                            End If
                        End If
                        If body.ToFullString.Contains(vbCrLf, StringComparison.OrdinalIgnoreCase) Then
                            If TypeOf body Is StatementSyntax Then
                                statements = Factory.SingletonList(CType(body, StatementSyntax))
                            Else
                                statements = Factory.SingletonList(Of StatementSyntax)(Factory.ReturnStatement(CType(body, ExpressionSyntax)))
                            End If
                            statements = Me.AdjustUsingIfNeeded(statements)

                            If isFunction Then
                                Return Factory.MultiLineLambdaExpression(VB.SyntaxKind.MultiLineFunctionLambdaExpression, lambdaHeader.WithAsClause(Nothing), statements, Factory.EndFunctionStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), FunctionKeyword)).WithConvertedTriviaFrom(node)
                            End If
                            Return Factory.MultiLineLambdaExpression(VB.SyntaxKind.MultiLineSubLambdaExpression, lambdaHeader.WithAsClause(Nothing), statements, Factory.EndSubStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), SubKeyword)).WithConvertedTriviaFrom(node)
                        Else
                            If isFunction Then
                                Return Factory.SingleLineLambdaExpression(VB.SyntaxKind.SingleLineFunctionLambdaExpression, lambdaHeader.WithAsClause(Nothing), body).WithConvertedTriviaFrom(node)
                            End If
                            Return Factory.SingleLineLambdaExpression(VB.SyntaxKind.SingleLineSubLambdaExpression, lambdaHeader.WithAsClause(Nothing), body).WithConvertedTriviaFrom(node)
                        End If
                    End If

                    ' TypeOf block Is SyntaxList(Of CSS.StatementSyntax)
                    statements = statements.AddRange(Factory.List(DirectCast(block, SyntaxList(Of CSS.StatementSyntax)).SelectMany(Function(s As CSS.StatementSyntax) s.Accept(New MethodBodyVisitor(_semanticModel, Me)))))
                    statements = Me.AdjustUsingIfNeeded(statements)
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
                        endBlock = Factory.EndFunctionStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), FunctionKeyword).WithConvertedTriviaFrom(csBraces.RightBrace)
                        expressionKind = VB.SyntaxKind.MultiLineFunctionLambdaExpression
                    Else
                        endBlock = Factory.EndSubStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), SubKeyword).WithConvertedTriviaFrom(csBraces.RightBrace)
                        expressionKind = VB.SyntaxKind.MultiLineSubLambdaExpression
                    End If
                    Return Factory.MultiLineLambdaExpression(kind:=expressionKind,
                                                               lambdaHeader.WithTrailingEol,
                                                               statements,
                                                               endBlock)
                Finally
                    Me.NeededEndUsingCount = savedNeedEndUsingCount
                End Try

            End Function

            Private Function IsConcatenateStringsExpression(node As CSS.BinaryExpressionSyntax) As Boolean
                If Not node.IsKind(CS.SyntaxKind.AddExpression) Then
                    Return False
                End If
                If Me.IsStringExpression(node.Left) AndAlso Me.IsStringExpression(node.Right) Then
                    Return True
                End If
                Dim leftTypeInfo As TypeInfo
                Dim rightTypeInfo As TypeInfo
                Try
                    leftTypeInfo = _semanticModel.GetTypeInfo(node.Left)
                    rightTypeInfo = _semanticModel.GetTypeInfo(node.Right)
                Catch ex As OperationCanceledException
                    Throw
                Catch ex As Exception
                    Throw
                End Try
                Dim leftAndRightIsString As Boolean? = leftTypeInfo.ConvertedType?.SpecialType = SpecialType.System_String OrElse rightTypeInfo.ConvertedType?.SpecialType = SpecialType.System_String
                Return leftAndRightIsString.HasValue AndAlso leftAndRightIsString.Value
            End Function

            Private Function IsNameOfExpression(node As CSS.InvocationExpressionSyntax) As Boolean
                Dim isIdentifierName As Boolean = TypeOf node.Expression Is CSS.IdentifierNameSyntax
                Dim methodIdentifier As CSS.IdentifierNameSyntax = If(isIdentifierName, CType(node.Expression, CSS.IdentifierNameSyntax), Nothing)
                Return isIdentifierName AndAlso methodIdentifier?.Identifier.Text = "nameof" AndAlso _semanticModel.GetSymbolInfo(methodIdentifier).ExtractBestMatch(Of ISymbol)() Is Nothing
            End Function

            Private Function IsStringExpression(node As SyntaxNode) As Boolean
                If node.IsKind(CS.SyntaxKind.StringLiteralExpression, CS.SyntaxKind.CharacterLiteralExpression, CS.SyntaxKind.InterpolatedStringExpression) Then
                    Return True
                End If
                ' Extra to pick up more strings
                If node.ToString.
                        TrimEnd(")"c).
                        TrimEnd("("c).
                        EndsWith("ToString", StringComparison.OrdinalIgnoreCase) Then
                    Return True
                End If
                Dim typeInf As TypeInfo
                Try
                    If TypeOf node Is CSS.BinaryExpressionSyntax Then
                        Dim binExpr As CSS.BinaryExpressionSyntax = CType(node, CSS.BinaryExpressionSyntax)
                        If Not binExpr.IsKind(CS.SyntaxKind.AddExpression) Then
                            Return False
                        End If
                        typeInf = _semanticModel.GetTypeInfo(binExpr)
                        If typeInf.IsString Then
                            Return True
                        End If
                        typeInf = _semanticModel.GetTypeInfo(binExpr.Left)
                        If typeInf.IsString Then
                            Return True
                        End If
                        typeInf = _semanticModel.GetTypeInfo(binExpr.Right)
                    ElseIf TypeOf node Is CSS.MemberAccessExpressionSyntax Then
                        typeInf = _semanticModel.GetTypeInfo(CType(node, CSS.MemberAccessExpressionSyntax).Expression)
                    Else
                        typeInf = _semanticModel.GetTypeInfo(node)
                    End If
                Catch ex As Exception
                    Throw
                End Try
                Return typeInf.IsString
            End Function

            Private Function MakeAssignmentStatement(node As CSS.AssignmentExpressionSyntax, leftNode As ExpressionSyntax, rightNode As ExpressionSyntax) As StatementSyntax
                Dim addressOfExpr As UnaryExpressionSyntax = TryCast(leftNode, UnaryExpressionSyntax)
                If addressOfExpr IsNot Nothing Then
                    leftNode = addressOfExpr.Operand
                End If
                If CS.CSharpExtensions.Kind(node) = CS.SyntaxKind.CoalesceAssignmentExpression Then
                    Dim possibleNullNode As ExpressionSyntax = DirectCast(node.Right.Accept(Me).WithLeadingTrivia(SpaceTrivia), ExpressionSyntax)
                    Dim rightBinaryExpression As BinaryConditionalExpressionSyntax = Factory.BinaryConditionalExpression(leftNode.WithoutTrivia, possibleNullNode)
                    Dim assignmentStmt As AssignmentStatementSyntax = Factory.SimpleAssignmentStatement(leftNode, rightBinaryExpression)
                    Dim newLeadingTrivia As New SyntaxTriviaList
                    If assignmentStmt.HasLeadingTrivia Then
                        newLeadingTrivia = newLeadingTrivia.AddRange(assignmentStmt.GetLeadingTrivia)
                        assignmentStmt = assignmentStmt.WithLeadingTrivia(SpaceTrivia)
                    End If
                    Return assignmentStmt.With(newLeadingTrivia, node.GetTrailingTrivia.ConvertTriviaList())
                End If
                Dim kind As VB.SyntaxKind = CS.CSharpExtensions.Kind(node).GetExpressionKind()
                Dim operatorToken As SyntaxToken = kind.GetOperatorToken(isReferenceType:=False)
                If node.Right.IsKind(CS.SyntaxKind.CoalesceExpression) Then
                    Dim csRight As CSS.BinaryExpressionSyntax = DirectCast(node.Right, CSS.BinaryExpressionSyntax)
                    If csRight.Right.IsKind(CS.SyntaxKind.ThrowExpression) Then
                        Dim testNode As ExpressionSyntax = rightNode.WithLeadingTrivia(SpaceTrivia)
                        Dim rightExpression As ThrowStatementSyntax = DirectCast(csRight.Right.Accept(Me).WithConvertedTriviaFrom(csRight.Right), ThrowStatementSyntax).WithTrailingEol
                        Dim statements As SyntaxList(Of StatementSyntax) = Factory.SingletonList(Of StatementSyntax)(rightExpression)

                        Dim testTrailingTrivia As SyntaxTriviaList = testNode.GetTrailingTrivia
                        Select Case testTrailingTrivia.Count
                            Case 0
                                testTrailingTrivia = testTrailingTrivia.Add(SpaceTrivia)
                            Case 1
                                Select Case testTrailingTrivia(0).RawKind
                                    Case VB.SyntaxKind.WhitespaceTrivia
                                    Case VB.SyntaxKind.EndOfLineTrivia
                                        testTrailingTrivia = testTrailingTrivia.InsertRange(0, SpaceLineContinueSpace)
                                    Case Else
                                        Throw UnexpectedValue(testTrailingTrivia.Count, NameOf(testTrailingTrivia.Count))
                                End Select
                        End Select
                        testNode = testNode.WithTrailingTrivia(testTrailingTrivia)
                        Dim condition As ExpressionSyntax = Factory.IsExpression(testNode, NothingExpression)
                        Dim ifBlock As SingleLineIfStatementSyntax =
                                            Factory.SingleLineIfStatement(condition,
                                                                          statements,
                                                                          elseClause:=Nothing
                                                                         )
                        Dim stmtWithIssues As CS.CSharpSyntaxNode = GetStatementWithIssues(node)
                        stmtWithIssues.AddMarker(ifBlock, StatementHandlingOption.PrependStatement, allowDuplicates:=False)
                        rightNode = DirectCast(csRight.Left.Accept(Me), ExpressionSyntax)
                    Else
                        rightNode = DirectCast(node.Right.Accept(Me), ExpressionSyntax)
                    End If
                Else
                    If rightNode Is Nothing Then
                        rightNode = Factory.IdentifierName("HandleRefExpression")
                    End If
                End If
                If node.IsKind(CS.SyntaxKind.AndAssignmentExpression,
                               CS.SyntaxKind.OrAssignmentExpression,
                               CS.SyntaxKind.ExclusiveOrAssignmentExpression,
                               CS.SyntaxKind.ModuloAssignmentExpression) Then
                    Return Factory.SimpleAssignmentStatement(leftNode, Factory.BinaryExpression(kind, leftNode.WithoutTrivia, operatorToken, rightNode.WithoutTrivia))
                End If
                If kind = VB.SyntaxKind.AddAssignmentStatement AndAlso
                    rightNode.IsKind(VB.SyntaxKind.ObjectCreationExpression) Then
                    Dim rightObjectCreation As ObjectCreationExpressionSyntax = DirectCast(rightNode, ObjectCreationExpressionSyntax)
                    If rightObjectCreation.ArgumentList.Arguments.Count = 1 AndAlso
                        rightObjectCreation.ArgumentList.Arguments(0).IsKind(VB.SyntaxKind.SimpleArgument) Then
                        If DirectCast(rightObjectCreation.ArgumentList.Arguments(0), SimpleArgumentSyntax).Expression.IsKind(VB.SyntaxKind.AddressOfExpression) Then
                            Return Factory.AddHandlerStatement(leftNode.WithLeadingTrivia(SpaceTrivia), rightNode).WithLeadingTrivia(leftNode.GetLeadingTrivia)
                        End If
                    End If
                End If
                Return Factory.AssignmentStatement(kind,
                                                   leftNode.AdjustNodeTrivia(separatorFollows:=True), kind.GetOperatorToken(isReferenceType:=False),
                                                   rightNode)
            End Function

            Private Function ReduceArrayUpperBoundExpression(expr As CSS.ExpressionSyntax) As ExpressionSyntax
                Dim constant As [Optional](Of Object) = _semanticModel.GetConstantValue(expr)
                If constant.HasValue AndAlso TypeOf constant.Value Is Integer Then
                    Return Factory.NumericLiteralExpression(Factory.Literal(CInt(constant.Value) - 1))
                End If
                Return Factory.BinaryExpression(kind:=VB.SyntaxKind.SubtractExpression, left:=DirectCast(expr.Accept(Me), ExpressionSyntax), operatorToken:=MinusToken, right:=OneExpression)
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
                Dim parameters As New SeparatedSyntaxList(Of CSS.ParameterSyntax)
                If node.ParameterList IsNot Nothing Then
                    parameters = CType(node.ParameterList?.Parameters, SeparatedSyntaxList(Of CSS.ParameterSyntax))
                End If
                Return Me.ConvertLambdaExpression(node, node.Block.Statements, parameters, Factory.TokenList(node.AsyncKeyword)).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitAnonymousObjectCreationExpression(node As CSS.AnonymousObjectCreationExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim fieldInitializers As New List(Of FieldInitializerSyntax)
                For Each e As IndexClass(Of CSS.AnonymousObjectMemberDeclaratorSyntax) In node.Initializers.WithIndex
                    Dim initializer As CSS.AnonymousObjectMemberDeclaratorSyntax = e.Value
                    Dim leadingTrivia As SyntaxTriviaList = Factory.TriviaList(initializer.GetLeadingTrivia.ConvertTriviaList())
                    Dim removeLeadingTrivia As Boolean = False
                    If leadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        If Not leadingTrivia.ContainsEndIfTrivia Then
                            leadingTrivia = leadingTrivia.Insert(0, Factory.CommentTrivia(" ' TODO: Comment moved from middle of expression to end, check for correct placement"))
                        End If
                        Dim statement As EmptyStatementSyntax = Factory.EmptyStatement.WithLeadingTrivia(leadingTrivia)
                        GetStatementWithIssues(node).AddMarker(statement, StatementHandlingOption.AppendEmptyStatement, allowDuplicates:=True)
                        removeLeadingTrivia = True
                    End If
                    Dim fieldInitializer As FieldInitializerSyntax = DirectCast(initializer.Accept(Me), FieldInitializerSyntax)
                    If removeLeadingTrivia Then
                        fieldInitializer = fieldInitializer.WithoutLeadingTrivia
                    End If
                    Dim field As FieldInitializerSyntax = fieldInitializer.NormalizeWhitespaceEx(useDefaultCasing:=True, PreserveCRLF:=True)
                    Dim firstTrivia As Boolean = True
                    Dim foundComment As Boolean = False
                    Dim fieldLeadingTrivia As SyntaxTriviaList = field.GetLeadingTrivia
                    Dim comment As String = ""
                    Dim newFieldLeadingTrivia As SyntaxTriviaList
                    For Each trivia As IndexClass(Of SyntaxTrivia) In fieldLeadingTrivia.WithIndex
                        Dim t As SyntaxTrivia = trivia.Value
                        Select Case t.RawKind
                            Case VB.SyntaxKind.WhitespaceTrivia
                                If firstTrivia = True Then
                                    newFieldLeadingTrivia = newFieldLeadingTrivia.Add(t)
                                End If
                            Case VB.SyntaxKind.EndOfLineTrivia
                                If foundComment Then
                                    If trivia.IsLast Then
                                        newFieldLeadingTrivia = newFieldLeadingTrivia.Add(t)
                                    End If
                                    ' skip any EOL
                                Else
                                    newFieldLeadingTrivia = newFieldLeadingTrivia.Add(t)
                                End If
                            Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                                If foundComment Then
                                    comment &= t.ToString.TrimStart.TrimStart("'"c)
                                Else
                                    foundComment = True
                                    comment = t.ToString
                                End If
                            Case Else
                                Stop
                        End Select
                        firstTrivia = False
                    Next
                    If comment.Any Then
                        newFieldLeadingTrivia = newFieldLeadingTrivia.Add(Factory.CommentTrivia(comment))
                    End If
                    fieldInitializers.Add(field.WithLeadingTrivia(newFieldLeadingTrivia))
                Next
                If fieldInitializers.Any Then
                    Return Factory.AnonymousObjectCreationExpression(Factory.ObjectMemberInitializer(Factory.SeparatedList(fieldInitializers)))
                End If
                Return Factory.ObjectCreationExpression(Factory.PredefinedType(ObjectKeyword))
            End Function

            Public Overrides Function VisitArrayCreationExpression(node As CSS.ArrayCreationExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim upperBoundArguments As IEnumerable(Of ArgumentSyntax) = node.Type.RankSpecifiers.First()?.Sizes.Where(Function(s As CSS.ExpressionSyntax) Not (TypeOf s Is CSS.OmittedArraySizeExpressionSyntax)).Select(Function(s As CSS.ExpressionSyntax) DirectCast(Factory.SimpleArgument(Me.ReduceArrayUpperBoundExpression(s)), ArgumentSyntax))
                Dim cleanUpperBounds As New List(Of ArgumentSyntax)
                For Each argument As ArgumentSyntax In upperBoundArguments
                    If argument.ToString <> "-1" Then
                        cleanUpperBounds.Add(argument)
                    End If
                Next
                upperBoundArguments = cleanUpperBounds
                Dim rankSpecifiers As IEnumerable(Of ArrayRankSpecifierSyntax) = node.Type.RankSpecifiers.Select(Function(rs As CSS.ArrayRankSpecifierSyntax) DirectCast(rs.Accept(Me), ArrayRankSpecifierSyntax))
                Dim attributeLists As SyntaxList(Of AttributeListSyntax) = Factory.List(Of AttributeListSyntax)()
                Dim vbNode As VB.VisualBasicSyntaxNode = node.Type.ElementType.Accept(Me)
                Dim arrayType As TypeSyntax
                If TypeOf vbNode Is TypeSyntax Then
                    arrayType = DirectCast(vbNode, TypeSyntax)
                Else
                    Throw UnreachableException
                End If
                Dim arrayBounds As ArgumentListSyntax = If(upperBoundArguments.Any(), Factory.ArgumentList(arguments:=Factory.SeparatedList(upperBoundArguments)), Nothing)
                Dim rankSpecifiersList As SyntaxList(Of ArrayRankSpecifierSyntax) = If(upperBoundArguments.Any(), Factory.List(rankSpecifiers.Skip(1)), Factory.List(rankSpecifiers))
                Dim initializer As CollectionInitializerSyntax = If(DirectCast(node.Initializer?.Accept(Me), CollectionInitializerSyntax), Factory.CollectionInitializer())
                Return Factory.ArrayCreationExpression(
                                                    NewKeyword.WithTrailingTrivia(SpaceTrivia),
                                                    attributeLists,
                                                    arrayType,
                                                    arrayBounds,
                                                    rankSpecifiersList,
                                                    initializer
                                                            ).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitAssignmentExpression(node As CSS.AssignmentExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim left As ExpressionSyntax
                Dim right As ExpressionSyntax
                If node.IsReturnValueDiscarded Then
                    left = CType(node.Left.Accept(Me), ExpressionSyntax)
                    right = CType(node.Right.Accept(Me), ExpressionSyntax)
                    If _semanticModel.GetTypeInfo(node.Right).ConvertedType.IsDelegateType() Then
                        If _semanticModel.GetSymbolInfo(node.Left).Symbol?.Kind <> SymbolKind.Event Then
                            Dim kind1? As CS.SyntaxKind = node.GetAncestor(Of CSS.AccessorDeclarationSyntax)()?.Kind()
                            If kind1 IsNot Nothing AndAlso (kind1.Value = CS.SyntaxKind.AddAccessorDeclaration OrElse kind1.Value = CS.SyntaxKind.RemoveAccessorDeclaration) Then
                                Dim methodName As String = If(kind1.Value = CS.SyntaxKind.AddAccessorDeclaration, "Combine", "Remove")
                                Dim delegateMethod As MemberAccessExpressionSyntax = MemberAccess("[Delegate]", methodName)
                                Dim invokeDelegateMethod As InvocationExpressionSyntax = Factory.InvocationExpression(delegateMethod, CreateArgList({left, right}))
                                Return Factory.SimpleAssignmentStatement(left, invokeDelegateMethod)
                            End If
                        Else
                            If node.OperatorToken.IsKind(CS.SyntaxKind.PlusEqualsToken) Then
                                Return Factory.AddHandlerStatement(left.WithLeadingTrivia(SpaceTrivia), right).WithLeadingTrivia(left.GetLeadingTrivia)
                            End If
                            If node.OperatorToken.IsKind(CS.SyntaxKind.MinusEqualsToken) Then
                                Return Factory.RemoveHandlerStatement(left.WithLeadingTrivia(SpaceTrivia), right).WithLeadingTrivia(left.GetLeadingTrivia)
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
                        Dim nodeRight As VB.VisualBasicSyntaxNode = node.Right.Accept(Me)
                        Dim objectMemberInitializerSyntax As ObjectMemberInitializerSyntax = TryCast(nodeRight, ObjectMemberInitializerSyntax)
                        If objectMemberInitializerSyntax IsNot Nothing Then
                            Dim initializers As SeparatedSyntaxList(Of ExpressionSyntax)
                            For Each field As NamedFieldInitializerSyntax In objectMemberInitializerSyntax.Initializers
                                initializers = initializers.Add(Factory.EqualsExpression(field.Name, field.Expression))
                            Next
                            nodeRight = Factory.CollectionInitializer(initializers)
                        End If
                        Return Factory.CollectionInitializer(Factory.SeparatedList({CType(node.Left.Accept(Me), ExpressionSyntax),
                                                                                   CType(nodeRight, ExpressionSyntax)})).WithConvertedTriviaFrom(node)
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
                            Dim simpleAssignment As CSS.AssignmentExpressionSyntax = node
                            Dim tInfo As TypeInfo = _semanticModel.GetTypeInfo(simpleAssignment.Left)
                            Dim initializer As ObjectCollectionInitializerSyntax = Factory.ObjectCollectionInitializer(FromKeyword, collection)
                            Dim type1 As TypeSyntax
                            If tInfo.Type Is Nothing OrElse tInfo.Type.IsErrorType Then
                                type1 = ConvertSimpleTypeToType("UnknownTypeTryConvertProject")
                            Else
                                type1 = tInfo.Type.ConvertToType().GetElementType
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
                                GetStatementWithIssues(node).AddMarker(Factory.EmptyStatement.WithLeadingTrivia(node.GetLeadingTrivia.ConvertTriviaList()), StatementHandlingOption.PrependStatement, allowDuplicates:=True)
                            End If
                            Return Factory.NamedFieldInitializer(DirectCast(vbNode.WithoutLeadingTrivia, IdentifierNameSyntax), DirectCast(nodeRight, ExpressionSyntax)).WithConvertedTrailingTriviaFrom(node)
                        End If
                        If TypeOf vbNode Is MemberAccessExpressionSyntax Then
                            Dim kind As VB.SyntaxKind = CS.CSharpExtensions.Kind(node).GetExpressionKind()
                            Dim operatorToken As SyntaxToken = kind.GetOperatorToken(isReferenceType:=False)
                            Return Factory.AssignmentStatement(kind, DirectCast(vbNode, MemberAccessExpressionSyntax), operatorToken, DirectCast(nodeRight, ExpressionSyntax)).WithConvertedTriviaFrom(node)
                        Else
                            Stop
                        End If
                    End If
                End If

                If TypeOf node.Parent Is CSS.ArrowExpressionClauseSyntax AndAlso
                    TypeOf node.Left IsNot CSS.TupleExpressionSyntax Then
                    Return Factory.SimpleAssignmentStatement(CType(node.Left.Accept(Me), ExpressionSyntax), CType(node.Right.Accept(Me), ExpressionSyntax))
                End If
                Dim rightTypeInfo As TypeInfo = _semanticModel.GetTypeInfo(node.Right)
                Dim isDelegate As Boolean
                If rightTypeInfo.ConvertedType IsNot Nothing Then
                    isDelegate = rightTypeInfo.ConvertedType.IsDelegateType
                    If Not isDelegate Then
                        isDelegate = rightTypeInfo.ConvertedType.ToString().StartsWith("System.EventHandler", StringComparison.Ordinal)
                    End If
                Else
                    If rightTypeInfo.Type IsNot Nothing Then
                        isDelegate = rightTypeInfo.Type.IsDelegateType
                        If Not isDelegate Then
                            isDelegate = rightTypeInfo.Type.ToString.StartsWith("System.EventHandler", StringComparison.Ordinal)
                        End If
                    End If
                End If
                If isDelegate OrElse node.Right.IsKind(CS.SyntaxKind.ParenthesizedLambdaExpression) Then
                    If node.OperatorToken.IsKind(CS.SyntaxKind.PlusEqualsToken) Then
                        Return Factory.AddHandlerStatement(CType(node.Left.Accept(Me), ExpressionSyntax).WithLeadingTrivia(SpaceTrivia), CType(node.Right.Accept(Me), ExpressionSyntax)).WithConvertedTriviaFrom(node)
                    End If

                    If node.OperatorToken.IsKind(CS.SyntaxKind.MinusEqualsToken) Then
                        ' TODO capture and leading comments from node.left
                        Return Factory.RemoveHandlerStatement(CType(node.Left.Accept(Me), ExpressionSyntax).WithLeadingTrivia(SpaceTrivia), CType(node.Right.Accept(Me), ExpressionSyntax)).WithConvertedTriviaFrom(node)
                    End If
                End If

                If node.Left.IsKind(CS.SyntaxKind.DeclarationExpression, CS.SyntaxKind.TupleExpression) Then
                    Dim initializer As EqualsValueSyntax = Factory.EqualsValue(CType(node.Right.Accept(Me), ExpressionSyntax))
                    Dim statementList As New SyntaxList(Of StatementSyntax)
                    Dim variableNames As New List(Of String)
                    If node.Left.IsKind(CS.SyntaxKind.DeclarationExpression) Then
                        Dim nodeLeft As CSS.DeclarationExpressionSyntax = DirectCast(node.Left, CSS.DeclarationExpressionSyntax)
                        Dim designation As CSS.ParenthesizedVariableDesignationSyntax = DirectCast(nodeLeft.Designation, CSS.ParenthesizedVariableDesignationSyntax)
                        For Each e As IndexClass(Of CSS.VariableDesignationSyntax) In designation.Variables.WithIndex
                            If e.Value.RawKind = CS.SyntaxKind.ParenthesizedVariableDesignation Then
                                Dim sBuilder As New StringBuilder
                                CreateDesignationName(ProcessVariableDesignation(CType(e.Value, CSS.ParenthesizedVariableDesignationSyntax)), sBuilder)
                                variableNames.Add(sBuilder.ToString)
                            Else
                                If e.Value.IsKind(CS.SyntaxKind.DiscardDesignation) Then
                                    variableNames.Add($"__DiscardDesignation{e.Index}")
                                Else
                                    variableNames.Add(e.Value.Accept(Me).ToString)
                                End If
                            End If
                        Next
                        Dim tupleType As TupleTypeSyntax = Nothing
                        Dim simpleAs As SimpleAsClauseSyntax = Nothing
                        Dim identifierName As String = node.GetUniqueVariableNameInScope("TempVar", _usedIdentifiers, _semanticModel)
                        Dim tempIdentifier As SeparatedSyntaxList(Of ModifiedIdentifierSyntax)
                        If rightTypeInfo.ConvertedType IsNot Nothing AndAlso Not rightTypeInfo.ConvertedType.IsErrorType Then
                            If TypeOf rightTypeInfo.Type Is INamedTypeSymbol Then
                                Dim possibleTupleType As INamedTypeSymbol = DirectCast(rightTypeInfo.ConvertedType, INamedTypeSymbol)
                                If possibleTupleType.IsTupleType Then
                                    identifierName = node.GetUniqueVariableNameInScope("TupleTempVar", _usedIdentifiers, _semanticModel)
                                    tempIdentifier = Factory.SingletonSeparatedList(Factory.ModifiedIdentifier(identifierName))
                                    tupleType = CType(possibleTupleType.TupleElements(0).ContainingType.ToString.ConvertCsStringToName, TupleTypeSyntax)
                                    If tupleType.Elements.All(Function(t As TupleElementSyntax) As Boolean
                                                                  If TypeOf t IsNot TypedTupleElementSyntax Then
                                                                      Return False
                                                                  End If
                                                                  Dim typedTupleElement As TypedTupleElementSyntax = CType(t, TypedTupleElementSyntax)
                                                                  Return typedTupleElement.Type.RawKind <> VB.SyntaxKind.PredefinedType
                                                              End Function) Then
                                        simpleAs = Factory.SimpleAsClause(AsKeyword.WithTrailingTrivia(SpaceTrivia), attributeLists:=Nothing, tupleType.WithLeadingTrivia(SpaceTrivia)).WithLeadingTrivia(SpaceTrivia)
                                    End If
                                Else
                                    simpleAs = Factory.SimpleAsClause(
                                            AsKeyword.WithTrailingTrivia(SpaceTrivia),
                                            attributeLists:=Nothing,
                                            ConvertToType(possibleTupleType.ToString).WithLeadingTrivia(SpaceTrivia)
                                            ).WithLeadingTrivia(SpaceTrivia)
                                End If
                            End If
                        End If
                        If tempIdentifier.Count = 0 Then
                            tempIdentifier = Factory.SingletonSeparatedList(Factory.ModifiedIdentifier(identifierName))
                        End If
                        statementList = statementList.Add(FactoryDimStatement(tempIdentifier(0).Identifier, simpleAs, initializer).
                                                            WithPrependedLeadingTrivia(Factory.CommentTrivia($" ' TODO: Visual Basic has no equivalent to C# deconstruction declarations, an attempt was made to convert."), VbEolTrivia))

                        For variableIndex As Integer = 0 To variableNames.Count - 1
                            If variableNames(variableIndex) = $"__DiscardDesignation{variableIndex}" Then
                                Continue For
                            End If
                            Dim asClause As AsClauseSyntax = Nothing
                            If nodeLeft.Type Is Nothing Then
                                Stop
                            Else
                                Dim tempType As TypeSyntax
                                If Not nodeLeft.Type.IsVar AndAlso tupleType IsNot Nothing Then
                                    If TypeOf tupleType.Elements(variableIndex) Is NamedTupleElementSyntax Then
                                        tempType =
                                            CType(tupleType.Elements(variableIndex), NamedTupleElementSyntax).AsClause.
                                                Type
                                    Else
                                        tempType = Factory.ParseTypeName(tupleType.Elements(variableIndex).ToString)
                                    End If
                                    asClause = Factory.SimpleAsClause(tempType)
                                Else
                                End If
                            End If
                            initializer = Factory.EqualsValue(Factory.InvocationExpression(Factory.ParseExpression($"{identifierName}.item{variableIndex + 1}")))

                            statementList = statementList.Add(FactoryDimStatement(variableNames(variableIndex), asClause, initializer))
                        Next
                    End If

                    ' Handle assignment to a Tuple of Variables that already exist
                    If node.Left.IsKind(CS.SyntaxKind.TupleExpression) Then
                        Dim leftTupleNode As TupleExpressionSyntax = DirectCast(CType(node.Left.Accept(Me), ExpressionSyntax).WithConvertedTriviaFrom(node.Left), TupleExpressionSyntax)

                        variableNames = New List(Of String)
                        Dim identifierName As String = node.GetUniqueVariableNameInScope("TupleTempVar", _usedIdentifiers, _semanticModel)
                        For Each argument As ArgumentSyntax In leftTupleNode.Arguments
                            variableNames.Add(argument.ToString)
                        Next
                        Dim tupleList As New List(Of String)
                        Dim builder As New StringBuilder()
                        If rightTypeInfo.Type Is Nothing OrElse rightTypeInfo.Type.IsErrorType Then
                            For Each a As CSS.ArgumentSyntax In DirectCast(node.Left, CSS.TupleExpressionSyntax).Arguments
                                If TypeOf a.Expression Is CSS.DeclarationExpressionSyntax Then
                                    Dim t As CSS.DeclarationExpressionSyntax = DirectCast(a.Expression, CSS.DeclarationExpressionSyntax)
                                    tupleList.Add(ConvertToType(t.Type.ToString).ToString)
                                ElseIf TypeOf a.Expression Is CSS.IdentifierNameSyntax Then
                                    tupleList.Add("Object")
                                Else
                                    ' We are going to ignore this
                                    tupleList.Add("_")
                                End If
                            Next
                        Else
                            If TypeOf rightTypeInfo.Type Is INamedTypeSymbol Then
                                Dim type As INamedTypeSymbol = DirectCast(rightTypeInfo.ConvertedType, INamedTypeSymbol)
                                Dim typeAsFullString As String = type.ConvertToType.ToFullString
                                builder = builder.Append(typeAsFullString)
                            ElseIf TypeOf rightTypeInfo.Type Is ITypeSymbol Then
                                Try
                                    For Each a As CSS.ArgumentSyntax In DirectCast(node.Left, CSS.TupleExpressionSyntax).Arguments
                                        If TypeOf a.Expression Is CSS.DeclarationExpressionSyntax Then
                                            Dim t As CSS.DeclarationExpressionSyntax = DirectCast(a.Expression, CSS.DeclarationExpressionSyntax)
                                            tupleList.Add(ConvertToType(t.Type.ToString).ToString)
                                        ElseIf TypeOf a.Expression Is CSS.IdentifierNameSyntax Then
                                            tupleList.Add("Object")
                                        Else
                                            ' We are going to ignore this
                                            tupleList.Add("_")
                                        End If
                                    Next
                                Catch ex As OperationCanceledException
                                    Throw
                                Catch ex As Exception
                                    Throw
                                End Try
                            Else
                                Stop
                            End If
                        End If
                        If tupleList.Count > 0 Then
                            builder.Append("("c)
                            For Each e As IndexClass(Of String) In tupleList.WithIndex
                                If Not e.IsLast Then
                                    builder.Append(e.Value & ", ")
                                End If
                            Next
                            builder.Append(tupleList.Last & ")")
                        End If
                        Dim tupleTypeStr As String = builder.ToString

                        Dim tupleType As TypeSyntax = Factory.ParseTypeName(tupleTypeStr).WithLeadingTrivia(SpaceTrivia)
                        Dim simpleAs As SimpleAsClauseSyntax = Factory.SimpleAsClause(AsKeyword.With(SpaceTrivia, SpaceTrivia), attributeLists:=Nothing, tupleType).WithLeadingTrivia(SpaceTrivia)
                        statementList = statementList.Add(FactoryDimStatement(identifierName, simpleAs, initializer).WithTrailingTrivia(ColonTrivia))
                        Dim statementNode As CS.CSharpSyntaxNode = GetStatementWithIssues(node)
                        Dim indexedStatementList As List(Of (Integer, StatementSyntax)) = statementNode.GetStatementsForNode(VB.SyntaxKind.LocalDeclarationStatement)

                        For Each e As IndexClass(Of String) In variableNames.WithIndex
                            If e.Value = "__" Then
                                _discardHelperMarkers.Add(node.AncestorsAndSelf().OfType(Of CSS.BaseTypeDeclarationSyntax).FirstOrDefault())
                                Continue For
                            End If

                            Dim statement As LocalDeclarationStatementSyntax = Nothing

                            Dim newLeftNode As IdentifierNameSyntax = Factory.IdentifierName(e.Value)
                            Dim assignmentStmt As StatementSyntax
                            Dim newRightNode As ExpressionSyntax = Factory.InvocationExpression(Factory.ParseExpression($"{identifierName}.item{e.Index + 1}"))
                            Dim stmtIndex As Integer = Nothing
                            If FindMatchingStatement(indexedStatementList, newLeftNode, stmtIndex, statement) Then
                                With statement
                                    Dim variableDeclarator As VariableDeclaratorSyntax = Factory.VariableDeclarator(statement.Declarators(0).Names, statement.Declarators(0).AsClause, Factory.EqualsValue(newRightNode))
                                    Dim declarators As SeparatedSyntaxList(Of VariableDeclaratorSyntax) = Factory.SingletonSeparatedList(variableDeclarator)
                                    assignmentStmt = Factory.LocalDeclarationStatement(.Modifiers, declarators)
                                End With
                                RemoveMarkedStatement(statementNode, statement)
                            Else
                                Dim kind As VB.SyntaxKind = CS.CSharpExtensions.Kind(node).GetExpressionKind()
                                Dim operatorToken As SyntaxToken = kind.GetOperatorToken(isReferenceType:=False)
                                assignmentStmt = Factory.AssignmentStatement(kind,
                                                                             newLeftNode,
                                                                             operatorToken,
                                                                             newRightNode)
                            End If
                            If e.IsLast Then
                                statementList = statementList.Add(assignmentStmt.WithLeadingTrivia(SpaceTrivia))
                            Else
                                statementList = statementList.Add(assignmentStmt.With({SpaceTrivia}, ColonTrivia))
                            End If
                        Next
                    End If
                    Dim tryStatement As TryStatementSyntax = Factory.TryStatement.
                        WithLeadingTrivia(VbEolTrivia).
                        WithPrependedLeadingTrivia(Factory.CommentTrivia("' TODO: This Try Block can be removed"))
                    Dim throwStatement As SyntaxList(Of StatementSyntax) = Factory.SingletonList(Of StatementSyntax)(Factory.ThrowStatement)
                    Dim catchBlock As SyntaxList(Of CatchBlockSyntax) = Factory.SingletonList(Factory.CatchBlock(Factory.CatchStatement,
                                                                                                                         throwStatement)
                                                                                                    )
                    Return Factory.TryBlock(tryStatement,
                                             statementList,
                                             catchBlock,
                                             finallyBlock:=Nothing,
                                             Factory.EndTryStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), TryKeyword).WithTrailingEol
                                             )
                End If

                _inlineAssignHelperMarkers.Add(node.AncestorsAndSelf().OfType(Of CSS.BaseTypeDeclarationSyntax).FirstOrDefault())
                Return Factory.InvocationExpression(
                    expression:=Factory.IdentifierName("__InlineAssignHelper"),
                    argumentList:=Factory.ArgumentList(Factory.SeparatedList((New ArgumentSyntax() {
                                                                            Factory.SimpleArgument(CType(node.Left.Accept(Me).WithoutTrailingTrivia, ExpressionSyntax)),
                                                                            Factory.SimpleArgument(CType(node.Right.Accept(Me).WithoutTrailingTrivia, ExpressionSyntax))
                                                                                                       }))
                                                        )).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitAwaitExpression(node As CSS.AwaitExpressionSyntax) As VB.VisualBasicSyntaxNode
                Return Factory.AwaitExpression(AwaitKeyword.WithTrailingTrivia(SpaceTrivia), expression:=DirectCast(node.Expression.Accept(Me), ExpressionSyntax)).WithConvertedLeadingTriviaFrom(node)
            End Function

            Public Overrides Function VisitBaseExpression(node As CSS.BaseExpressionSyntax) As VB.VisualBasicSyntaxNode
                Return MyBaseExpression
            End Function

            Public Overrides Function VisitBinaryExpression(node As CSS.BinaryExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim operatorToken As SyntaxToken
                Try
                    Dim retExp As ExpressionSyntax
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
                        Dim vbNode As VB.VisualBasicSyntaxNode = CType(CType(csNodesOrTokens(0), SyntaxNode), CSS.ExpressionSyntax).Accept(Me)
                        retExp = CType(vbNode.ConvertAndModifyNodeTrivia(csNodesOrTokens, 0, isStatement:=False), ExpressionSyntax)
                        For nodeOrTokenIndex As Integer = 1 To csNodesOrTokens.Count - 1 Step 2
                            operatorToken = AmpersandToken.ConvertAndModifyTokenTrivia(csNodesOrTokens, nodeOrTokenIndex)
                            vbNode = CType(CType(csNodesOrTokens(nodeOrTokenIndex + 1), SyntaxNode), CSS.ExpressionSyntax).Accept(Me)
                            rightNode = vbNode.ConvertAndModifyNodeTrivia(csNodesOrTokens, nodeOrTokenIndex + 1, isStatement:=False)
                            retExp = Factory.ConcatenateExpression(retExp,
                                                                   operatorToken,
                                                                   DirectCast(rightNode, ExpressionSyntax))
                        Next
                        'Debug.WriteLine(retExp.ToFullString)
                        Return retExp
                    End If
                    Dim kind As VisualBasic.SyntaxKind = CS.CSharpExtensions.Kind(node).GetExpressionKind()
                    leftNode = node.Left.Accept(Me)
                    rightNode = node.Right.Accept(Me)
                    Select Case node.Kind
                        Case CS.SyntaxKind.CoalesceExpression
                            leftExp = DirectCast(node.Left.Accept(Me), ExpressionSyntax).AdjustExpressionTrivia(adjustLeading:=True, directiveNotAllowed:=False)
                            Dim ifKeywordWithTrivia As SyntaxToken = IfKeyword
                            Dim visualBasicSyntaxNode As ExpressionSyntax = TryCast(rightNode, ExpressionSyntax)
                            If visualBasicSyntaxNode IsNot Nothing Then
                                rightExp = visualBasicSyntaxNode.AdjustExpressionTrivia(adjustLeading:=True, directiveNotAllowed:=False)
                                If leftExp.ContainsEolTrivia OrElse leftExp.ContainsCommentOrDirectiveTrivia Then
                                    If leftExp.HasLeadingTrivia Then
                                        ifKeywordWithTrivia = ifKeywordWithTrivia.WithLeadingTrivia(leftExp.GetLeadingTrivia)
                                    End If
                                    If node.OperatorToken.HasLeadingTrivia AndAlso node.OperatorToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                        Dim stmtTrivia As SyntaxTriviaList = node.OperatorToken.LeadingTrivia.ConvertTriviaList()
                                        GetStatementWithIssues(node).AddMarker(
                                            Factory.EmptyStatement.WithLeadingTrivia(stmtTrivia),
                                            StatementHandlingOption.AppendEmptyStatement,
                                            allowDuplicates:=True)
                                        rightExp = rightExp.WithLeadingTrivia(stmtTrivia.Last)
                                    End If
                                End If

                                Dim lastLeadingTrivia As SyntaxTrivia = rightExp.GetLeadingTrivia.LastOrDefault
                                If (Not (lastLeadingTrivia.IsWhitespace AndAlso Not lastLeadingTrivia.Span.IsEmpty)) AndAlso node.OperatorToken.LeadingTrivia.LastOrDefault.IsWhitespace Then
                                    rightExp = rightExp.WithLeadingTrivia({node.OperatorToken.LeadingTrivia.Last}.ToSyntaxTriviaList.ConvertTriviaList())
                                End If

                                Return Factory.BinaryConditionalExpression(ifKeywordWithTrivia,
                                                                             openParenToken,
                                                                             leftExp,
                                                                             CommaToken,
                                                                             rightExp,
                                                                             CloseParenToken)
                            End If
                            If TypeOf rightNode Is ThrowStatementSyntax Then
                                Dim condition As ExpressionSyntax = Factory.IsExpression(leftExp, NothingExpression)

                                Dim statements As SyntaxList(Of StatementSyntax) = Factory.SingletonList(DirectCast(rightNode, StatementSyntax))
                                Dim ifBlock As StatementSyntax = Factory.SingleLineIfStatement(ifKeywordWithTrivia,
                                                                                               condition,
                                                                                               ThenKeyword,
                                                                                               statements,
                                                                                               elseClause:=Nothing).WithTrailingEol
                                GetStatementWithIssues(node).AddMarker(ifBlock, StatementHandlingOption.PrependStatement, allowDuplicates:=False)
                                Return leftNode
                            Else
                                Stop
                            End If
                        Case CS.SyntaxKind.AsExpression
                            leftExp = DirectCast(leftNode, ExpressionSyntax)
                            Dim commaTokenWithTrivia As SyntaxToken = CommaToken
                            If leftExp.ContainsEolTrivia Then
                                leftExp = leftExp.WithRestructuredEolTrivia
                                commaTokenWithTrivia = commaTokenWithTrivia.WithTrailingTrivia(VbEolTrivia)
                            End If
                            Return Factory.TryCastExpression(
                                                TryCastKeyword,
                                                openParenToken,
                                                leftExp,
                                                commaTokenWithTrivia,
                                                DirectCast(rightNode, TypeSyntax),
                                                CloseParenToken)
                        Case CS.SyntaxKind.IsExpression
                            Dim newLeadingTrivia As New SyntaxTriviaList
                            If leftNode.HasLeadingTrivia AndAlso leftNode.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                newLeadingTrivia = leftNode.GetLeadingTrivia
                                leftNode = leftNode.WithoutTrivia.WithTrailingTrivia(SpaceTrivia)
                            End If
                            If rightNode.HasLeadingTrivia AndAlso rightNode.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                newLeadingTrivia = newLeadingTrivia.AddRange(rightNode.GetLeadingTrivia)
                                rightNode = rightNode.WithoutTrivia.WithTrailingTrivia(SpaceTrivia)
                            End If
                            Return Factory.TypeOfIsExpression(DirectCast(leftNode, ExpressionSyntax), DirectCast(rightNode, TypeSyntax)).WithLeadingTrivia(newLeadingTrivia)
                        Case CS.SyntaxKind.EqualsExpression, CS.SyntaxKind.NotEqualsExpression
                            Dim otherArgument As ExpressionSyntax = Nothing
                            If node.Left.IsKind(CS.SyntaxKind.NullLiteralExpression) Then
                                otherArgument = DirectCast(rightNode, ExpressionSyntax).With({SpaceTrivia}, {SpaceTrivia})
                            End If

                            If node.Right.IsKind(CS.SyntaxKind.NullLiteralExpression) Then
                                otherArgument = DirectCast(leftNode, ExpressionSyntax).With({SpaceTrivia}, {SpaceTrivia})
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
                    rightExp = DirectCast(rightNode, ExpressionSyntax).AdjustExpressionTrivia(adjustLeading:=True, directiveNotAllowed:=True).RemoveExtraLeadingEol
                    Dim leftTrailingTrivia As SyntaxTriviaList = leftNode.GetTrailingTrivia
                    If leftTrailingTrivia.ToList.Count = 1 AndAlso leftTrailingTrivia(0).ToString.Trim = "?" Then
                        Dim originalIdentifier As IdentifierNameSyntax = rightExp.DescendantNodes.
                                                                           OfType(Of IdentifierNameSyntax).
                                                                           First(Function(b As IdentifierNameSyntax) b.Kind() = VB.SyntaxKind.IdentifierName)
                        Dim newIdentifierWithQuestionMark As IdentifierNameSyntax =
                                    Factory.IdentifierName($"{leftNode}?")
                        Return rightExp.ReplaceNode(originalIdentifier, newIdentifierWithQuestionMark)
                    End If

                    leftExp = CType(leftNode, ExpressionSyntax).AdjustExpressionTrivia(adjustLeading:=True, directiveNotAllowed:=False)
                    Dim isReferenceType As Boolean = node.Left.IsReferenceComparison(node.Right, _semanticModel)
                    operatorToken = kind.GetOperatorToken(isReferenceType).WithConvertedTriviaFrom(node.OperatorToken)
                    If operatorToken.HasLeadingTrivia Then
                        operatorToken = operatorToken.AdjustTokenLeadingTrivia()
                    End If

                    If operatorToken.HasTrailingTrivia Then
                        operatorToken = operatorToken.AdjustTokenTrailingTrivia(removeTrailingLineContinuation:=False)
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
                    Throw
                End Try
            End Function

            Public Overrides Function VisitCastExpression(node As CSS.CastExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim cTypeExpression As VB.VisualBasicSyntaxNode
                Dim newTrailingTrivia As SyntaxTriviaList
                Try
                    Dim type As ITypeSymbol = _semanticModel.GetTypeInfo(node.Type).Type
                    Dim expr As ExpressionSyntax = DirectCast(node.Expression.Accept(Me), ExpressionSyntax)
                    newTrailingTrivia = newTrailingTrivia.AddRange(expr.GetTrailingTrivia)
                    newTrailingTrivia = newTrailingTrivia.AddRange(node.GetTrailingTrivia.ConvertTriviaList())
                    expr = expr.WithoutTrivia

                    Dim exprTypeStr As String = _semanticModel.GetTypeInfo(node.Expression).Type?.ToString
                    Dim fixExpr As ExpressionSyntax = Factory.IdentifierName("Fix")
                    Select Case type.SpecialType
                        Case SpecialType.System_Object
                            cTypeExpression = Factory.PredefinedCastExpression(CObjKeyword, expr)
                        Case SpecialType.System_Boolean
                            cTypeExpression = Factory.PredefinedCastExpression(CBoolKeyword, expr)
                        Case SpecialType.System_Char
                            If node.Parent.IsKind(CS.SyntaxKind.AttributeArgument) Then
                                cTypeExpression = expr
                            ElseIf {"int", "UShort"}.Contains(exprTypeStr, StringComparer.OrdinalIgnoreCase) Then
                                cTypeExpression = Factory.ParseExpression($"ChrW({expr})")
                            Else
                                cTypeExpression = Factory.PredefinedCastExpression(CCharKeyword, expr)
                            End If
                        Case SpecialType.System_SByte
                            cTypeExpression = Factory.PredefinedCastExpression(CSByteKeyword, expr)
                        Case SpecialType.System_Byte
                            If expr.IsKind(VB.SyntaxKind.CharacterLiteralExpression) Then
                                cTypeExpression = Factory.ParseExpression($"AscW({expr})")
                            Else
                                cTypeExpression = Factory.PredefinedCastExpression(CByteKeyword, expr)
                            End If
                        Case SpecialType.System_Int16
                            Dim argumentList As ArgumentListSyntax = Factory.ArgumentList(Factory.SingletonSeparatedList(Of ArgumentSyntax)(Factory.SimpleArgument(expr)))
                            cTypeExpression = Factory.PredefinedCastExpression(CShortKeyword, Factory.InvocationExpression(fixExpr, argumentList))
                        Case SpecialType.System_UInt16
                            If exprTypeStr = "char" Then
                                cTypeExpression = Factory.ParseExpression(text:=$"AscW({expr})")
                            Else
                                cTypeExpression = Factory.PredefinedCastExpression(CUShortKeyword, expr)
                            End If
                        Case SpecialType.System_Int32
                            If exprTypeStr = "char" Then
                                cTypeExpression = Factory.ParseExpression($"ChrW({expr})").WithTrailingTrivia(newTrailingTrivia)
                            Else
                                Dim argumentList As ArgumentListSyntax = Factory.ArgumentList(Factory.SingletonSeparatedList(Of ArgumentSyntax)(Factory.SimpleArgument(expr)))
                                cTypeExpression = Factory.PredefinedCastExpression(CIntKeyword, Factory.InvocationExpression(fixExpr, argumentList))
                            End If
                        Case SpecialType.System_UInt32
                            cTypeExpression = Factory.PredefinedCastExpression(CUIntKeyword, expr)
                        Case SpecialType.System_Int64
                            Dim argumentList As ArgumentListSyntax = Factory.ArgumentList(Factory.SingletonSeparatedList(Of ArgumentSyntax)(Factory.SimpleArgument(expr)))
                            cTypeExpression = Factory.PredefinedCastExpression(CLngKeyword, Factory.InvocationExpression(fixExpr, argumentList))
                        Case SpecialType.System_UInt64
                            cTypeExpression = Factory.PredefinedCastExpression(CULngKeyword, expr)
                        Case SpecialType.System_Decimal
                            cTypeExpression = Factory.PredefinedCastExpression(CDecKeyword, expr)
                        Case SpecialType.System_Single
                            cTypeExpression = Factory.PredefinedCastExpression(CSngKeyword, expr)
                        Case SpecialType.System_Double
                            cTypeExpression = Factory.PredefinedCastExpression(CDblKeyword, expr)
                        Case SpecialType.System_String
                            cTypeExpression = Factory.PredefinedCastExpression(CStrKeyword, expr)
                        Case SpecialType.System_DateTime
                            cTypeExpression = Factory.PredefinedCastExpression(CDateKeyword, expr)
                        Case Else
                            ' Added support to correctly handle AddressOf
                            Dim typeOrAddressOf As VB.VisualBasicSyntaxNode = node.Type.Accept(Me)
                            If typeOrAddressOf.IsKind(VB.SyntaxKind.AddressOfExpression) Then
                                Dim unaryExpression As UnaryExpressionSyntax = DirectCast(typeOrAddressOf, UnaryExpressionSyntax)
                                If unaryExpression.Operand.ToString.StartsWith("&", StringComparison.OrdinalIgnoreCase) Then
                                    Dim stmtWithIssues As CS.CSharpSyntaxNode = GetStatementWithIssues(node)
                                    stmtWithIssues.AddMarker(FlagUnsupportedStatements(stmtWithIssues, "pointers", commentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, allowDuplicates:=True)
                                    cTypeExpression = expr
                                ElseIf node.Type.IsKind(CS.SyntaxKind.PointerType) Then
                                    Return Factory.CTypeExpression(expr, Factory.ParseTypeName("IntPtr"))
                                Else
                                    cTypeExpression = Factory.CTypeExpression(expr, Factory.ParseTypeName(unaryExpression.Operand.ToString.RemoveAll("&")))
                                End If
                            ElseIf node.Type.DetermineType(_semanticModel)._ITypeSymbol.IsDelegateType Then
                                If expr.IsKind(VB.SyntaxKind.AddressOfExpression) Then
                                    cTypeExpression = Factory.CTypeExpression(expr, DirectCast(typeOrAddressOf, TypeSyntax))
                                ElseIf expr.IsKind(VB.SyntaxKind.ParenthesizedExpression) Then
                                    cTypeExpression = Factory.CTypeExpression(CType(expr, ParenthesizedExpressionSyntax).Expression, DirectCast(typeOrAddressOf, TypeSyntax))
                                Else
                                    cTypeExpression = Factory.CTypeExpression(Factory.AddressOfExpression(expr), DirectCast(typeOrAddressOf, TypeSyntax))
                                End If
                            Else
                                cTypeExpression = Factory.CTypeExpression(expr, DirectCast(typeOrAddressOf, TypeSyntax))
                            End If
                    End Select
                Catch ex As OperationCanceledException
                    Throw
                Catch ex As Exception
                    Throw
                End Try
                Return cTypeExpression.WithConvertedLeadingTriviaFrom(node).WithTrailingTrivia(newTrailingTrivia)
            End Function

            Public Overrides Function VisitCheckedExpression(node As CSS.CheckedExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim unchecked As Boolean = node.Keyword.IsKind(CS.SyntaxKind.UncheckedKeyword)
                Dim statementWithIssue As CS.CSharpSyntaxNode = GetStatementWithIssues(node)
                Dim leadingTrivia As SyntaxTriviaList = statementWithIssue.CheckCorrectnessLeadingTrivia(attemptToPortMade:=True, "VB has no direct equivalent To C# unchecked")
                ' Only notify once on one line TODO Merge the comments

                Dim expression As ExpressionSyntax = DirectCast(node.Expression.Accept(Me), ExpressionSyntax)
                If TypeOf expression Is PredefinedCastExpressionSyntax Then
                    Dim castExpression As PredefinedCastExpressionSyntax = DirectCast(expression, PredefinedCastExpressionSyntax)
                    If unchecked Then
                        statementWithIssue.AddMarker(Factory.EmptyStatement.WithLeadingTrivia(leadingTrivia), StatementHandlingOption.PrependStatement, allowDuplicates:=False)
                        Return Factory.ParseExpression($"{castExpression.Keyword}(Val(""&H"" & Hex({castExpression.Expression})))")
                    Else
                        Return Factory.ParseExpression($"{castExpression.Keyword}({castExpression.Expression})")
                    End If
                End If
                If TypeOf expression Is BinaryConditionalExpressionSyntax OrElse
                    TypeOf expression Is BinaryExpressionSyntax OrElse
                    TypeOf expression Is InvocationExpressionSyntax OrElse
                    TypeOf expression Is LiteralExpressionSyntax OrElse
                    TypeOf expression Is ObjectCreationExpressionSyntax Then
                    If unchecked Then
                        statementWithIssue.AddMarker(Factory.EmptyStatement.WithLeadingTrivia(leadingTrivia), StatementHandlingOption.PrependStatement, allowDuplicates:=False)
                        Return Factory.ParseExpression($"Unchecked({expression})")
                    Else
                        Return expression
                    End If
                End If
                If TypeOf expression Is CTypeExpressionSyntax OrElse
                    TypeOf expression Is TernaryConditionalExpressionSyntax OrElse
                    TypeOf expression Is UnaryExpressionSyntax OrElse
                    TypeOf expression Is ParenthesizedExpressionSyntax OrElse
                    TypeOf expression Is IdentifierNameSyntax Then
                    Return expression
                End If
                Throw UnreachableException
            End Function

            Public Overrides Function VisitConditionalAccessExpression(node As CSS.ConditionalAccessExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim expression As ExpressionSyntax = DirectCast(node.Expression.Accept(Me), ExpressionSyntax)
                Dim trailingTriviaList As SyntaxTriviaList
                If expression.ContainsEolTrivia Then
                    trailingTriviaList = trailingTriviaList.AddRange(expression.WithRestructuredEolTrivia.GetTrailingTrivia)
                    expression = expression.WithoutTrailingTrivia
                End If
                If node.OperatorToken.ContainsDirectives Then
                    Dim stmtWithIssues As CS.CSharpSyntaxNode = GetStatementWithIssues(node)
                    stmtWithIssues.AddMarker(Factory.EmptyStatement().WithLeadingTrivia(ConvertConditionalAccessExpressionToComment(node)).WithTrailingEol, StatementHandlingOption.PrependStatement, allowDuplicates:=True)
                End If
                Return Factory.ConditionalAccessExpression(expression, QuestionToken.WithTrailingTrivia(trailingTriviaList), DirectCast(node.WhenNotNull.Accept(Me), ExpressionSyntax)).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitConditionalExpression(node As CSS.ConditionalExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim nodesOrTokens As New List(Of SyntaxNodeOrToken) From {
                    node.Condition,
                    node.ColonToken,
                    node.WhenTrue,
                    node.QuestionToken,
                    node.WhenFalse
                }

                Dim condition As ExpressionSyntax = DirectCast(node.Condition.Accept(Me).ConvertAndModifyNodeTrivia(nodesOrTokens, 0, isStatement:=False), ExpressionSyntax)

                Dim csWhenTrue As CSS.ExpressionSyntax = node.WhenTrue
                Dim whenTrue As ExpressionSyntax = Nothing
                If Not csWhenTrue.IsKind(CS.SyntaxKind.ThrowExpression) Then
                    whenTrue = DirectCast(node.WhenTrue.Accept(Me).ConvertAndModifyNodeTrivia(nodesOrTokens, index:=2, isStatement:=False), ExpressionSyntax)
                    If whenTrue.GetTrailingTrivia.ContainsCommentTrivia Then
                        whenTrue = whenTrue.WithTrailingEol(removeLastLineContinuation:=False)
                    End If
                End If

                Dim firstCommaToken As SyntaxToken = CommaToken.ConvertAndModifyTokenTrivia(nodesOrTokens, index:=1)

                Dim csWhenFalse As CSS.ExpressionSyntax = node.WhenFalse
                Dim whenFalse As ExpressionSyntax = Nothing
                If Not csWhenFalse.IsKind(CS.SyntaxKind.ThrowExpression) Then
                    whenFalse = DirectCast(node.WhenFalse.Accept(Me).ConvertAndModifyNodeTrivia(nodesOrTokens, index:=4, isStatement:=True), ExpressionSyntax)
                    If whenFalse.GetTrailingTrivia.ContainsCommentTrivia Then
                        whenFalse = whenFalse.WithTrailingEol
                    End If
                End If

                Dim ifKeywordWithTrivia As SyntaxToken = IfKeyword.WithConvertedLeadingTriviaFrom(node.Condition.GetFirstToken)
                Dim secondCommaToken As SyntaxToken = CommaToken.ConvertAndModifyTokenTrivia(nodesOrTokens, index:=3)
                If Not secondCommaToken.TrailingTrivia.ContainsCommentOrDirectiveTrivia AndAlso (whenFalse Is Nothing OrElse Not whenFalse.GetLeadingTrivia.Any) Then
                    secondCommaToken = secondCommaToken.WithTrailingTrivia(SpaceTrivia)
                End If
                If Not (csWhenFalse.IsKind(CS.SyntaxKind.ThrowExpression) OrElse csWhenTrue.IsKind(CS.SyntaxKind.ThrowExpression)) Then
                    Return Factory.TernaryConditionalExpression(
                    ifKeywordWithTrivia,
                    openParenToken,
                    condition.WithoutTrivia,
                    firstCommaToken,
                    whenTrue,
                    secondCommaToken,
                    whenFalse,
                    CloseParenToken)
                End If
                Dim throwStmt As ThrowStatementSyntax
                Dim resultExpr As ExpressionSyntax
                If Not csWhenFalse.IsKind(CS.SyntaxKind.ThrowExpression) Then
                    throwStmt = DirectCast(csWhenTrue.Accept(Me).WithConvertedTriviaFrom(csWhenTrue), ThrowStatementSyntax).WithTrailingEol
                    resultExpr = DirectCast(csWhenFalse.Accept(Me).WithConvertedTriviaFrom(csWhenFalse), ExpressionSyntax)
                Else
                    condition = Factory.NotExpression(condition.WithoutTrivia)
                    throwStmt = DirectCast(csWhenFalse.Accept(Me).WithConvertedTriviaFrom(csWhenFalse), ThrowStatementSyntax).WithTrailingEol
                    resultExpr = DirectCast(csWhenTrue.Accept(Me).WithConvertedTriviaFrom(csWhenTrue), ExpressionSyntax)
                End If
                Dim stmtList As SyntaxList(Of StatementSyntax) = Factory.SingletonList(Of StatementSyntax)(throwStmt)

                Dim ifBlock As SingleLineIfStatementSyntax = Factory.SingleLineIfStatement(condition.WithTrailingTrivia({SpaceTrivia}),
                                                                                                 stmtList,
                                                                                                 elseClause:=Nothing
                                                                                                 )
                Dim stmtWithIssues As CS.CSharpSyntaxNode = GetStatementWithIssues(node)
                stmtWithIssues.AddMarker(ifBlock, StatementHandlingOption.PrependStatement, allowDuplicates:=True)
                Return resultExpr
            End Function

            Public Overrides Function VisitDeclarationExpression(node As CSS.DeclarationExpressionSyntax) As VB.VisualBasicSyntaxNode
                Return node.Designation.Accept(Me)
            End Function

            Public Overrides Function VisitDefaultExpression(node As CSS.DefaultExpressionSyntax) As VB.VisualBasicSyntaxNode
                Return Factory.ParseExpression($"CType(Nothing, {node.Type.Accept(Me).WithLeadingTrivia(SpaceTrivia)})").WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitElementAccessExpression(node As CSS.ElementAccessExpressionSyntax) As VB.VisualBasicSyntaxNode
                If node.ArgumentList.Arguments.Count = 1 AndAlso node.ArgumentList.Arguments(0).Expression.IsKind(CS.SyntaxKind.RangeExpression) Then
                    Dim rangeExpression As CSS.RangeExpressionSyntax = CType(node.ArgumentList.Arguments(0).Expression, CSS.RangeExpressionSyntax)
                    Dim leftOperand As VB.VisualBasicSyntaxNode = rangeExpression.LeftOperand?.Accept(Me)
                    Dim rightOperand As ExpressionSyntax
                    Dim leftExpression As ExpressionSyntax = DirectCast(node.Expression.Accept(Me), ExpressionSyntax)
                    If rangeExpression.RightOperand?.IsKind(CS.SyntaxKind.IndexExpression) Then
                        Dim offsetFromLength As ExpressionSyntax = CType(rangeExpression.RightOperand.Accept(Me), ExpressionSyntax)
                        rightOperand = Factory.ParseExpression($"{leftExpression}.Length{offsetFromLength}")
                    Else
                        rightOperand = CType(rangeExpression.RightOperand?.Accept(Me), ExpressionSyntax)
                    End If
                    If leftOperand Is Nothing Then
                        Return Factory.ParseExpression($"{leftExpression}.Substring(0, {rightOperand})")
                    Else
                        If rightOperand Is Nothing Then
                            Return Factory.ParseExpression($"{leftExpression}.Substring({leftOperand})")
                        Else
                            Return Factory.ParseExpression($"{leftExpression}.Substring({leftOperand}, {rightOperand})")
                        End If
                    End If

                End If
                Dim argumentList As ArgumentListSyntax = DirectCast(node.ArgumentList.Accept(Me), ArgumentListSyntax)
                Dim expression As ExpressionSyntax
                If node.Expression.IsKind(CS.SyntaxKind.BaseExpression) Then
                    If node.GetAncestor(Of CSS.IndexerDeclarationSyntax).IsKind(CS.SyntaxKind.IndexerDeclaration) Then
                        expression = Factory.ParseExpression($"MyBase.item")
                    ElseIf node.GetAncestor(Of CSS.PropertyDeclarationSyntax) IsNot Nothing Then
                        Return Factory.ParseExpression($"MyBase.item({argumentList.Arguments(0).WithoutTrivia}))")
                    Else
                        Return Factory.ParseName($"MyBase.{argumentList.Arguments(0)}")
                    End If
                ElseIf node.Expression.IsKind(CS.SyntaxKind.ObjectCreationExpression) Then
                    expression = DirectCast(node.Expression.Accept(Me), ExpressionSyntax)
                    Dim uniqueName As String = node.GetUniqueVariableNameInScope("tempVar", _usedIdentifiers, _semanticModel)

                    Dim asClause As AsClauseSyntax = Factory.AsNewClause(DirectCast(expression, NewExpressionSyntax))
                    Dim dimStatement As LocalDeclarationStatementSyntax = FactoryDimStatement(uniqueName, asClause, initializer:=Nothing)
                    GetStatementWithIssues(node).AddMarker(dimStatement, StatementHandlingOption.PrependStatement, allowDuplicates:=True)
                    expression = Factory.IdentifierName(Factory.Identifier(uniqueName)).WithTriviaFrom(expression)
                Else
                    expression = DirectCast(node.Expression.Accept(Me), ExpressionSyntax)
                End If

                Return Factory.InvocationExpression(expression, argumentList.WithoutLeadingTrivia)
            End Function

            Public Overrides Function VisitElementBindingExpression(node As CSS.ElementBindingExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim arguments As VB.VisualBasicSyntaxNode = node.ArgumentList.Arguments(0).Accept(Me)
                Dim expression As ExpressionSyntax = Factory.ParseExpression(arguments.ToString)
                Dim parenthesizedExpression As ParenthesizedExpressionSyntax = Factory.ParenthesizedExpression(expression)
                Return Factory.InvocationExpression(parenthesizedExpression)
            End Function

            Public Overrides Function VisitImplicitArrayCreationExpression(node As CSS.ImplicitArrayCreationExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim expressionItems As New List(Of ExpressionSyntax)
                Dim namedFieldItems As New List(Of FieldInitializerSyntax)
                Dim separators As New List(Of SyntaxToken)
                Dim csSeparators As IEnumerable(Of SyntaxToken) = node.Initializer.Expressions.GetSeparators
                For Each e As IndexClass(Of CSS.ExpressionSyntax) In node.Initializer.Expressions.WithIndex
                    Dim itemWithTrivia As VB.VisualBasicSyntaxNode
                    Try
                        itemWithTrivia = e.Value.Accept(Me).WithConvertedTriviaFrom(e.Value).RemoveExtraLeadingEol.NormalizeWhitespaceEx(useDefaultCasing:=True, indentation:="    ")
                        Dim leadingTrivia As SyntaxTriviaList = e.Value.GetLeadingTrivia
                        If leadingTrivia.Any AndAlso leadingTrivia.Last.IsKind(CS.SyntaxKind.WhitespaceTrivia) Then
                            itemWithTrivia = itemWithTrivia.WithPrependedLeadingTrivia(Factory.Whitespace(leadingTrivia.Last.ToString))
                        End If
                        If TypeOf itemWithTrivia Is NamedFieldInitializerSyntax Then
                            namedFieldItems.Add(DirectCast(itemWithTrivia, NamedFieldInitializerSyntax))
                        ElseIf TypeOf itemWithTrivia Is AssignmentStatementSyntax Then
                            Dim stmtWithIssues As CS.CSharpSyntaxNode = GetStatementWithIssues(node)
                            stmtWithIssues.AddMarker(FlagUnsupportedStatements(stmtWithIssues, $"C# Assignment Expression", commentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, allowDuplicates:=False)
                            Return Nothing
                        Else
                            expressionItems.Add(DirectCast(itemWithTrivia, ExpressionSyntax))
                        End If
                    Catch ex As OperationCanceledException
                        Throw
                    Catch ex As Exception
                        Throw
                    End Try
                    If Not e.IsLast Then
                        separators.Add(CommaToken.WithConvertedTrailingTriviaFrom(csSeparators(e.Index)))
                    End If
                Next
                Dim openBraceTokenWithTrivia As SyntaxToken = OpenBraceToken.WithConvertedTriviaFrom(node.Initializer.OpenBraceToken)
                Dim closeBraceTokenWithTrivia As SyntaxToken = CloseBraceToken.WithConvertedTrailingTriviaFrom(node.Initializer.CloseBraceToken)
                If node.Parent.IsKind(CS.SyntaxKind.ElementAccessExpression) Then
                    closeBraceTokenWithTrivia = closeBraceTokenWithTrivia.WithTrailingTrivia(SpaceTrivia)
                End If
                If expressionItems.Any Then
                    RestructureNodesAndSeparators(openBraceTokenWithTrivia, expressionItems, separators, closeBraceTokenWithTrivia)
                    Dim exprInitializers As SeparatedSyntaxList(Of ExpressionSyntax) = Factory.SeparatedList(expressionItems, separators)
                    Return Factory.CollectionInitializer(openBraceTokenWithTrivia, exprInitializers, closeBraceTokenWithTrivia).WithConvertedLeadingTriviaFrom(node.NewKeyword)
                Else
                    RestructureNodesAndSeparators(openBraceTokenWithTrivia, namedFieldItems, separators, closeBraceTokenWithTrivia)
                    Dim initializers As SeparatedSyntaxList(Of FieldInitializerSyntax) = Factory.SeparatedList(namedFieldItems)
                    Return Factory.AnonymousObjectCreationExpression(Factory.ObjectMemberInitializer(initializers))
                End If
            End Function

            Public Overrides Function VisitInitializerExpression(node As CSS.InitializerExpressionSyntax) As VB.VisualBasicSyntaxNode
                Try
                    Dim csSeparators As IEnumerable(Of SyntaxToken) = node.Expressions.GetSeparators
                    Dim expressions As New List(Of ExpressionSyntax)
                    Dim fields As New List(Of FieldInitializerSyntax)
                    Dim separators As New List(Of SyntaxToken)
                    Dim exprLastIndex As Integer = node.Expressions.Count - 1
                    Dim finalSeparator As Boolean = csSeparators.Any AndAlso exprLastIndex <> csSeparators.Count
                    Dim openBraceTokenWithTrivia As SyntaxToken = OpenBraceToken.WithConvertedTriviaFrom(node.OpenBraceToken)
                    Dim reportProgress As Boolean = exprLastIndex > 500

                    If reportProgress Then
                        _originalRequest.Progress?.Report(New ProgressReport(0, node.Expressions.Count))
                    End If
                    ' Figuring out this without using Accept is complicated below is safe but not fast
                    Dim itemIsField As Boolean = node.Expressions.Any AndAlso TypeOf node.Expressions(0).Accept(Me) Is FieldInitializerSyntax
                    Dim foundEof As Boolean = False
                    For expressionIndex As Integer = 0 To exprLastIndex
                        If reportProgress Then
                            _originalRequest.Progress?.Report(New ProgressReport(expressionIndex + 1, node.Expressions.Count))
                        End If

                        If _originalRequest.CancelToken.IsCancellationRequested Then
                            Exit For
                        End If
                        Dim item As VB.VisualBasicSyntaxNode = node.Expressions(expressionIndex).Accept(Me)
                        Try
                            If itemIsField Then
                                fields.Add(DirectCast(item.RemoveExtraLeadingEol, FieldInitializerSyntax))
                            Else
                                expressions.Add(DirectCast(item.RemoveExtraLeadingEol, ExpressionSyntax))
                            End If
                        Catch ex As OperationCanceledException
                            Throw
                        Catch ex As Exception
                            Throw
                        End Try

                        If exprLastIndex > expressionIndex Then
                            separators.Add(CommaToken.WithConvertedTrailingTriviaFrom(csSeparators(expressionIndex)))
                        Else
                            If finalSeparator Then
                                Dim trailingTrivia As SyntaxTriviaList
                                If itemIsField Then
                                    fields(expressionIndex) = fields(expressionIndex).WithAppendedTrailingTrivia(csSeparators.Last.TrailingTrivia.ConvertTriviaList())
                                    trailingTrivia = fields(expressionIndex).GetTrailingTrivia
                                Else
                                    expressions(expressionIndex) = expressions(expressionIndex).WithAppendedTrailingTrivia(csSeparators.Last.TrailingTrivia.ConvertTriviaList())
                                    trailingTrivia = expressions(expressionIndex).GetTrailingTrivia
                                End If
                                foundEof = trailingTrivia.Any AndAlso trailingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia)
                            End If
                        End If
                    Next
                    Dim closeBracketLeadingTrivia As SyntaxTriviaList = node.CloseBraceToken.LeadingTrivia.ConvertTriviaList()
                    If closeBracketLeadingTrivia.Any Then
                        If closeBracketLeadingTrivia.First.IsComment Then
                            closeBracketLeadingTrivia = closeBracketLeadingTrivia.Insert(1, VbEolTrivia)
                        End If
                    End If
                    If closeBracketLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        Dim foundCommentOrDirective As Boolean = False
                        Dim newCLoseBracketLeadingTrivia As SyntaxTriviaList
                        For Each e As IndexClass(Of SyntaxTrivia) In closeBracketLeadingTrivia.WithIndex
                            Dim t As SyntaxTrivia = e.Value
                            If foundCommentOrDirective OrElse t.IsDirective Or t.IsComment Then
                                If Not (foundEof OrElse foundCommentOrDirective) Then
                                    newCLoseBracketLeadingTrivia = newCLoseBracketLeadingTrivia.Add(VbEolTrivia)
                                    foundEof = False
                                End If
                                foundCommentOrDirective = True
                                newCLoseBracketLeadingTrivia = newCLoseBracketLeadingTrivia.Add(t)
                                Continue For
                            End If
                            Select Case t.RawKind
                                Case VB.SyntaxKind.WhitespaceTrivia
                                    newCLoseBracketLeadingTrivia = newCLoseBracketLeadingTrivia.Add(t)
                                Case VB.SyntaxKind.EndOfLineTrivia
                                    newCLoseBracketLeadingTrivia = newCLoseBracketLeadingTrivia.Add(VbEolTrivia)
                                    foundEof = True
                            End Select
                        Next
                        closeBracketLeadingTrivia = newCLoseBracketLeadingTrivia
                    End If

                    Dim closeBraceTokenWithTrivia As SyntaxToken = CloseBraceToken.With(closeBracketLeadingTrivia,
                                                                                        node.CloseBraceToken.TrailingTrivia.ConvertTriviaList())
                    If node.IsKind(CS.SyntaxKind.ObjectInitializerExpression) Then
                        Dim withKeywordWithTrivia As SyntaxToken = WithKeyword.WithTrailingTrivia(VbEolTrivia)
                        If fields.Any Then
                            RestructureNodesAndSeparators(openBraceTokenWithTrivia, fields, separators, closeBraceTokenWithTrivia)
                            Return Factory.ObjectMemberInitializer(withKeywordWithTrivia, openBraceTokenWithTrivia, Factory.SeparatedList(fields, separators), closeBraceTokenWithTrivia).WithConvertedTriviaFrom(node)
                        End If
                        RestructureNodesAndSeparators(openBraceTokenWithTrivia, expressions, separators, closeBraceTokenWithTrivia)
                        If expressions.Any Then
                            If Not expressions(exprLastIndex).ContainsEolTrivia Then
                                expressions(exprLastIndex) = expressions(exprLastIndex).WithAppendedEol
                                Return Factory.ObjectCollectionInitializer(Factory.CollectionInitializer(openBraceTokenWithTrivia, Factory.SeparatedList(expressions.OfType(Of ExpressionSyntax), separators), closeBraceTokenWithTrivia))
                            End If
                        Else
                            Return Factory.CollectionInitializer(openBraceTokenWithTrivia, Factory.SeparatedList(expressions.OfType(Of ExpressionSyntax), separators), closeBraceTokenWithTrivia.WithoutTrivia).WithTrailingTrivia(closeBraceTokenWithTrivia.LeadingTrivia)
                        End If
                    End If

                    RestructureNodesAndSeparators(openBraceTokenWithTrivia, expressions, separators, closeBraceTokenWithTrivia)
                    If node.IsKind(CS.SyntaxKind.ArrayInitializerExpression) OrElse node.IsKind(CS.SyntaxKind.CollectionInitializerExpression) Then
                        Dim initializers As SeparatedSyntaxList(Of ExpressionSyntax) = Factory.SeparatedList(expressions, separators)

                        Return Factory.CollectionInitializer(openBraceTokenWithTrivia, initializers, closeBraceTokenWithTrivia)
                    End If
                    Return Factory.CollectionInitializer(openBraceTokenWithTrivia.RemoveExtraEol, Factory.SeparatedList(expressions, separators), closeBraceTokenWithTrivia)
                Catch ex As OperationCanceledException
                    Throw
                Catch ex As Exception
                    Throw
                End Try
            End Function

            Public Overrides Function VisitInterpolatedStringExpression(node As CSS.InterpolatedStringExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim content As New List(Of InterpolatedStringContentSyntax)
                For Each e As IndexClass(Of CSS.InterpolatedStringContentSyntax) In node.Contents.WithIndex
                    content.Add(DirectCast(e.Value.Accept(Me), InterpolatedStringContentSyntax))
                Next
                Return Factory.InterpolatedStringExpression(content.ToArray()).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitInterpolatedStringText(node As CSS.InterpolatedStringTextSyntax) As VB.VisualBasicSyntaxNode
                Dim csToken As SyntaxToken = node.TextToken
                Dim textToken As SyntaxToken = csToken.ConvertToInterpolatedStringTextToken
                Return Factory.InterpolatedStringText(textToken).WithConvertedTriviaFrom(node)
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
                Return Factory.InterpolationAlignmentClause(CommaToken, CType(node.Value.Accept(Me), ExpressionSyntax))
            End Function

            Public Overrides Function VisitInterpolationFormatClause(node As CSS.InterpolationFormatClauseSyntax) As VB.VisualBasicSyntaxNode
                Dim formatStringToken As SyntaxToken = Factory.InterpolatedStringTextToken(SyntaxTriviaList.Empty, node.FormatStringToken.Text, node.FormatStringToken.ValueText, SyntaxTriviaList.Empty)
                Return Factory.InterpolationFormatClause(ColonToken, formatStringToken)
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
                        Throw
                    End Try
                End If
                Dim vbInvocationExpression As VB.VisualBasicSyntaxNode = Nothing
                If Me.TryCreateRaiseEventStatement(node.Expression, node.ArgumentList, vbInvocationExpression) Then
                    Return vbInvocationExpression
                End If

                Dim vbEventExpression As ExpressionSyntax = DirectCast(node.Expression.Accept(Me), ExpressionSyntax).WithoutLeadingSystemDot
                Dim argList As ArgumentListSyntax = DirectCast(node.ArgumentList.Accept(Me), ArgumentListSyntax)
                Dim invocationExpression As InvocationExpressionSyntax = Factory.InvocationExpression(vbEventExpression.AdjustExpressionTrivia(adjustLeading:=False, directiveNotAllowed:=False), argList)
                Dim objectCreationExpression As CSS.ObjectCreationExpressionSyntax = TryCast(node.Expression.DescendantNodesAndSelf().OfType(Of CSS.MemberAccessExpressionSyntax).FirstOrDefault?.Expression, CSS.ObjectCreationExpressionSyntax)
                If objectCreationExpression IsNot Nothing Then
                    If TypeOf node.Parent Is CSS.ExpressionStatementSyntax AndAlso objectCreationExpression IsNot Nothing Then
                        Return Factory.CallStatement(invocationExpression.WithLeadingTrivia(SpaceTrivia))
                    End If
                    Return invocationExpression
                End If
                Dim newTrailingTrivia As SyntaxTriviaList = argList.GetTrailingTrivia
                newTrailingTrivia = newTrailingTrivia.AddRange(node.GetTrailingTrivia.ConvertTriviaList())
                If newTrailingTrivia.Count = 2 Then
                    Dim lastTrivia As SyntaxTrivia = newTrailingTrivia.Last
                    Select Case newTrailingTrivia.First.RawKind
                        Case VB.SyntaxKind.EndOfLineTrivia
                            If lastTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                newTrailingTrivia = newTrailingTrivia.RemoveAt(1)
                            ElseIf lastTrivia.IsKind(VB.SyntaxKind.WhitespaceTrivia) OrElse
                                lastTrivia.IsKind(VB.SyntaxKind.EndIfDirectiveTrivia) Then
                                ' Ignore, it belongs here
                            Else
                                Stop
                            End If
                        Case VB.SyntaxKind.WhitespaceTrivia
                            If lastTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                newTrailingTrivia = newTrailingTrivia.RemoveAt(1)
                            ElseIf lastTrivia.IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                                newTrailingTrivia = newTrailingTrivia.RemoveAt(1)
                            Else
                                Stop
                            End If
                        Case Else
                            Stop
                    End Select
                End If
                Dim methodInfo As TypeInfo = _semanticModel.GetTypeInfo(node.Expression)
                If methodInfo.Type?.Name = "Func" Then
                    Return Factory.InvocationExpression(invocationExpression.WithoutTrailingTrivia, Factory.ArgumentList()).WithTrailingTrivia(newTrailingTrivia)
                End If
                Return invocationExpression.WithTrailingTrivia(newTrailingTrivia)
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
                                                        value:=node.Token.ValueText.
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
                            Dim parentMethod As CSS.MethodDeclarationSyntax = node.Parent.GetAncestor(Of CSS.MethodDeclarationSyntax)
                            If parentMethod IsNot Nothing Then
                                Return Factory.CTypeExpression(
                                                    NothingExpression,
                                                    CType(parentMethod.ReturnType.Accept(Me), TypeSyntax)
                                                    )
                            End If
                        Case CS.SyntaxKind.EqualsValueClause
                            Dim localDecStmt As CSS.LocalDeclarationStatementSyntax = node.Parent.GetAncestor(Of CSS.LocalDeclarationStatementSyntax)
                            If localDecStmt IsNot Nothing Then
                                If localDecStmt.Declaration.Type IsNot Nothing Then
                                    Return Factory.CTypeExpression(
                                                        NothingExpression,
                                                        CType(localDecStmt.Declaration.Type.Accept(Me).WithoutLeadingTrivia, TypeSyntax)
                                                        )
                                End If
                            End If
                            Dim varDeclaration As CSS.VariableDeclarationSyntax = node.Parent.GetAncestor(Of CSS.VariableDeclarationSyntax)
                            If varDeclaration IsNot Nothing Then
                                If varDeclaration.Type IsNot Nothing Then
                                    Return Factory.CTypeExpression(
                                                        NothingExpression,
                                                        CType(varDeclaration.Type.Accept(Me), TypeSyntax)
                                                        )
                                End If
                            ElseIf node.Parent.Parent.RawKind = CS.SyntaxKind.Parameter Then
                                Dim parameter As CSS.ParameterSyntax = CType(node.Parent.Parent, CSS.ParameterSyntax)
                                If parameter.Type IsNot Nothing Then
                                    Return Factory.CTypeExpression(
                                                        NothingExpression,
                                                        CType(parameter.Type.Accept(Me), TypeSyntax)
                                                        )
                                End If
                            Else
                                Stop
                            End If
                        Case CS.SyntaxKind.SimpleAssignmentExpression
                            Dim leftTypeInfo As TypeInfo = _semanticModel.GetTypeInfo(DirectCast(node.Parent, CSS.AssignmentExpressionSyntax).Left)
                            If leftTypeInfo.Type Is Nothing OrElse leftTypeInfo.Type.IsErrorType Then
                                Return NothingExpression
                            End If
                            If leftTypeInfo.Type?.IsTupleType Then
                                Return Factory.CTypeExpression(
                                    NothingExpression,
                                    Factory.ParseTypeName($"({String.Join(", ", leftTypeInfo.Type.ToString.ConvertTypeTupleToTypeStrings(True))})"))
                            Else
                                Return If(leftTypeInfo.Type.Name Is "", NothingExpression, CType(Factory.CTypeExpression(NothingExpression, ConvertToType(leftTypeInfo.Type.Name)), ExpressionSyntax))
                            End If
                        Case CS.SyntaxKind.ConditionalExpression
                            Dim leftNodeTypeInfo As TypeInfo = _semanticModel.GetTypeInfo(DirectCast(node.Parent, CSS.ConditionalExpressionSyntax).WhenTrue)
                            If leftNodeTypeInfo.Type Is Nothing OrElse leftNodeTypeInfo.Type.IsErrorType Then
                                Return NothingExpression
                            End If
                            Dim type As TypeSyntax = If(leftNodeTypeInfo.Type.IsTupleType, leftNodeTypeInfo.Type.ToString.ConvertCsStringToName, ConvertToType(leftNodeTypeInfo.Type.Name))
                            Return Factory.CTypeExpression(
                                                      NothingExpression,
                                                      type
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
                        Case CS.SyntaxKind.CastExpression
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
                    Dim stmtWithIssues As CS.CSharpSyntaxNode = GetStatementWithIssues(node)
                    stmtWithIssues.AddMarker(FlagUnsupportedStatements(stmtWithIssues, $"Pointer Member Access Expressions", commentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, allowDuplicates:=False)
                End If

                Dim expression As ExpressionSyntax = DirectCast(node.Expression.Accept(Me), ExpressionSyntax)

                Dim expressionTrailingTrivia As SyntaxTriviaList = expression.GetTrailingTrivia
                If TypeOf expression Is NewExpressionSyntax AndAlso TypeOf expression IsNot ArrayCreationExpressionSyntax Then
                    Dim expressionWithTrivia As ExpressionSyntax = CType(node.Expression.Accept(Me), ExpressionSyntax)
                    Return Me.WrapTypedNameIfNecessary(Factory.MemberAccessExpression(VB.SyntaxKind.SimpleMemberAccessExpression,
                                                                                        expressionWithTrivia.AdjustExpressionTrivia(adjustLeading:=True, directiveNotAllowed:=False),
                                                                                        DotToken,
                                                                                        CType(node.Name.Accept(Me), SimpleNameSyntax)
                                                                                        ),
                                                       node).WithConvertedTriviaFrom(node)
                ElseIf TypeOf expression Is CollectionInitializerSyntax Then
                    Dim uniqueName As String = node.GetUniqueVariableNameInScope("tempVar", _usedIdentifiers, _semanticModel)
                    Dim uniqueIdentifier As IdentifierNameSyntax = Factory.IdentifierName(Factory.Identifier(uniqueName))
                    Dim initializer As EqualsValueSyntax = Factory.EqualsValue(expression)
                    Dim dimStatement As LocalDeclarationStatementSyntax =
                            FactoryDimStatement(uniqueName, asClause:=Nothing, initializer).WithLeadingTrivia(expression.GetLeadingTrivia).WithTrailingEol
                    Dim stmtWithIssues As CS.CSharpSyntaxNode = GetStatementWithIssues(node)
                    stmtWithIssues.AddMarker(dimStatement, StatementHandlingOption.PrependStatement, allowDuplicates:=True)
                    expression = uniqueIdentifier.WithTriviaFrom(expression)
                End If

                Dim needOperatorEol As Boolean = False
                Dim newNameLeadingTrivia As New SyntaxTriviaList
                Dim operatorTrailingTrivia As New SyntaxTriviaList
                Dim oldNameLeadingTrivia As SyntaxTriviaList = node.Name.GetLeadingTrivia.ConvertTriviaList()
                If oldNameLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    Dim initialTrailingTrivia As SyntaxTriviaList = node.OperatorToken.TrailingTrivia.ConvertTriviaList()

                    For Each t As SyntaxTrivia In initialTrailingTrivia
                        Select Case t.RawKind
                            Case VB.SyntaxKind.WhitespaceTrivia
                                operatorTrailingTrivia = operatorTrailingTrivia.Add(t)
                            Case VB.SyntaxKind.EndOfLineTrivia
                                needOperatorEol = True
                            Case Else
                                Stop
                        End Select
                    Next
                    For Each e As IndexClass(Of SyntaxTrivia) In oldNameLeadingTrivia.WithIndex
                        Dim t As SyntaxTrivia = e.Value
                        Select Case t.RawKind
                            Case VB.SyntaxKind.WhitespaceTrivia
                                newNameLeadingTrivia = newNameLeadingTrivia.Add(t)
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
                If needOperatorEol Then
                    operatorTrailingTrivia = operatorTrailingTrivia.Add(VbEolTrivia)
                End If
                Dim operatorToken As SyntaxToken = DotToken.With(node.OperatorToken.LeadingTrivia.ConvertTriviaList(), operatorTrailingTrivia)
                Dim name As SimpleNameSyntax = DirectCast(node.Name.Accept(Me).With(newNameLeadingTrivia, node.Name.GetTrailingTrivia.ConvertTriviaList()), SimpleNameSyntax)
                Dim keywordKind As VB.SyntaxKind = VB.SyntaxFacts.GetKeywordKind(name.Identifier.ValueText)
                If VB.SyntaxFacts.IsKeywordKind(keywordKind) Then
                    Dim nameSyntax As IdentifierNameSyntax = Factory.IdentifierName($"[{name.Identifier.ValueText}]")
                    name = name.WithIdentifier(nameSyntax.Identifier)
                End If
                operatorTrailingTrivia = New SyntaxTriviaList

                If expressionTrailingTrivia.LastOrDefault.IsKind(VB.SyntaxKind.EndOfLineTrivia) AndAlso Not expressionTrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                    expressionTrailingTrivia = expressionTrailingTrivia.InsertRange(expressionTrailingTrivia.Count - 1, SpaceLineContinue)
                    expression = expression.WithTrailingTrivia(expressionTrailingTrivia)
                    operatorToken = operatorToken.AdjustTokenLeadingTrivia()
                ElseIf expression.GetLastToken.ContainsEolTrivia Then
                    Dim foundEol As Boolean = False
                    foundEol = RestructureMemberAccessExpressionTrivia(node,
                                                                       Factory.TriviaList(expressionTrailingTrivia),
                                                                       foundEol,
                                                                       operatorTrailingTrivia)
                    foundEol = RestructureMemberAccessExpressionTrivia(node,
                                                                       operatorToken.LeadingTrivia,
                                                                       foundEol,
                                                                       operatorTrailingTrivia)

                    If foundEol Then
                        operatorTrailingTrivia = operatorTrailingTrivia.Add(VbEolTrivia)
                    End If
                    expression = expression.WithoutTrailingTrivia
                    operatorToken = operatorToken.WithoutTrivia.WithTrailingTrivia(operatorTrailingTrivia)
                    name = name.WithLeadingTrivia(SpaceTrivia)
                End If
                Dim expressionName As MemberAccessExpressionSyntax = Factory.MemberAccessExpression(VB.SyntaxKind.SimpleMemberAccessExpression, expression, operatorToken, name)
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
                    type1 = type1.WithTrailingTrivia(SpaceTrivia)
                End If
                Dim possibleInitializer As VB.VisualBasicSyntaxNode = node.Initializer?.Accept(Me)
                Dim initializer As ObjectCollectionInitializerSyntax = Nothing
                If possibleInitializer IsNot Nothing Then
                    type1 = type1.WithTrailingTrivia(SpaceTrivia)
                    Select Case possibleInitializer.Kind
                        Case VB.SyntaxKind.CollectionInitializer
                            initializer = Factory.ObjectCollectionInitializer(initializer:=DirectCast(possibleInitializer, CollectionInitializerSyntax))
                        Case VB.SyntaxKind.ObjectCollectionInitializer
                            initializer = DirectCast(possibleInitializer, ObjectCollectionInitializerSyntax)
                        Case VB.SyntaxKind.ObjectMemberInitializer
                            ' Remove trailing trivia before with
                            If argumentList IsNot Nothing Then
                                argumentList = argumentList.WithCloseParenToken(CloseParenToken)
                            End If
                            Dim memberInitializer As ObjectMemberInitializerSyntax = DirectCast(possibleInitializer, ObjectMemberInitializerSyntax)
                            Return Factory.ObjectCreationExpression(NewKeyword, Factory.List(Of AttributeListSyntax)(), type1.WithTrailingTrivia(SpaceTrivia), argumentList, memberInitializer)

                        Case Else
                            _reportException?.Invoke(UnexpectedValue(NameOf(possibleInitializer)))
                    End Select
                End If
                If argumentList IsNot Nothing AndAlso initializer?.GetFirstToken.IsKind(VB.SyntaxKind.FromKeyword) Then
                    argumentList = argumentList.WithTrailingTrivia(SpaceTrivia)
                End If
                If argumentList?.ContainsDirectives Then
                    Dim newArgList As New List(Of ArgumentSyntax)
                    Dim newSeparatorList As New List(Of SyntaxToken)
                    Dim foundEol As Boolean = False

                    For index As Integer = 0 To argumentList.Arguments.Count - 2
                        newArgList.Add(argumentList.Arguments(index))
                        newSeparatorList.Add(argumentList.Arguments.GetSeparator(index).RemoveDirectiveTrivia(foundEol))
                    Next
                    newArgList.Add(argumentList.Arguments(argumentList.Arguments.Count - 1))
                    argumentList = argumentList.WithArguments(Factory.SeparatedList(newArgList, newSeparatorList))
                End If
                If argumentList?.Arguments.Count = 0 Then
                    argumentList = Nothing
                End If
                Return Factory.ObjectCreationExpression(NewKeyword.WithConvertedTriviaFrom(node.NewKeyword).AdjustTokenTrailingTrivia(removeTrailingLineContinuation:=False), Factory.List(Of AttributeListSyntax)(), type1, argumentList, initializer)
            End Function

            Public Overrides Function VisitParenthesizedExpression(node As CSS.ParenthesizedExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim expr As ExpressionSyntax = DirectCast(node.Expression.Accept(Me), ExpressionSyntax)
                If TypeOf expr Is CTypeExpressionSyntax OrElse
                   TypeOf expr Is IdentifierNameSyntax OrElse
                   TypeOf expr Is InvocationExpressionSyntax OrElse
                   TypeOf expr Is QueryExpressionSyntax OrElse
                   TypeOf expr Is TryCastExpressionSyntax OrElse
                   TypeOf expr Is LiteralExpressionSyntax Then
                    Return expr.WithTrailingTrivia(SpaceTrivia)
                End If
                Dim declarationToBeAdded As LocalDeclarationStatementSyntax
                If TypeOf node.Parent Is CSS.MemberAccessExpressionSyntax OrElse
                    TypeOf node.Parent Is CSS.ElementAccessExpressionSyntax OrElse
                    TypeOf node.Parent Is CSS.ConditionalAccessExpressionSyntax Then
                    ' statement with issues points to "statement" Probably an Expression statement. If this is part of a single Line If we need to go higher
                    Dim statementWithIssue As CS.CSharpSyntaxNode = GetStatementWithIssues(node)
                    ' statement with issues points to "statement" Probably an Expression statement. If this is part of an ElseIf we need to go higher
                    Dim initializer As EqualsValueSyntax = Factory.EqualsValue(expr)
                    If TypeOf node.Parent Is CSS.MemberAccessExpressionSyntax OrElse TypeOf node.Parent Is CSS.ElementAccessExpressionSyntax Then
                        If node.Expression.IsKind(CS.SyntaxKind.AddExpression) Then
                            Return Factory.ParenthesizedExpression(openParenToken.WithConvertedTriviaFrom(node.OpenParenToken), expr, CloseParenToken.WithConvertedTriviaFrom(node.CloseParenToken))
                        End If
                        Dim uniqueName As String = node.GetUniqueVariableNameInScope("tempVar", _usedIdentifiers, _semanticModel)
                        Dim uniqueIdentifier As IdentifierNameSyntax = Factory.IdentifierName(Factory.Identifier(uniqueName))
                        If TypeOf expr Is TernaryConditionalExpressionSyntax Then
                            Dim tenaryExp As TernaryConditionalExpressionSyntax = DirectCast(expr, TernaryConditionalExpressionSyntax)
                            If tenaryExp.Condition.IsKind(VB.SyntaxKind.IdentifierName) Then
                                Dim ifStatement As IfStatementSyntax =
                                   Factory.IfStatement(IfKeyword, tenaryExp.Condition, ThenKeyword).WithConvertedLeadingTriviaFrom(node)
                                Dim ifBlockStatements As New SyntaxList(Of StatementSyntax)
                                ifBlockStatements = ifBlockStatements.Add(Factory.SimpleAssignmentStatement(left:=uniqueIdentifier, right:=tenaryExp.WhenTrue).WithoutLastLineContinuation)
                                Dim elseBlockStatements As New SyntaxList(Of StatementSyntax)
                                elseBlockStatements = elseBlockStatements.Add(Factory.SimpleAssignmentStatement(left:=uniqueIdentifier, right:=tenaryExp.WhenFalse).WithoutLastLineContinuation)
                                Dim elseBlock As ElseBlockSyntax = Factory.ElseBlock(elseBlockStatements)
                                Dim ifBlockToBeAdded As StatementSyntax = Factory.MultiLineIfBlock(
                                                                    ifStatement,
                                                                    ifBlockStatements,
                                                                    Nothing,
                                                                    elseBlock,
                                                                    Factory.EndIfStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), IfKeyword).
                                                                                                    WithConvertedTrailingTriviaFrom(node).
                                                                                                    WithTrailingEol)

                                declarationToBeAdded = FactoryDimStatement(uniqueName, asClause:=Nothing, initializer:=Nothing).WithPrependedLeadingTrivia(Factory.CommentTrivia($" ' TODO: Check, VB does not directly support MemberAccess off a Conditional If Expression")).WithTrailingEol

                                statementWithIssue.AddMarker(declarationToBeAdded, StatementHandlingOption.PrependStatement, allowDuplicates:=True)
                                statementWithIssue.AddMarker(ifBlockToBeAdded, StatementHandlingOption.PrependStatement, allowDuplicates:=True)
                                Return uniqueIdentifier
                            End If
                        ElseIf TypeOf expr Is BinaryConditionalExpressionSyntax Then
                            Dim binExpr As BinaryConditionalExpressionSyntax = DirectCast(expr, BinaryConditionalExpressionSyntax)
                            If binExpr.FirstExpression.IsKind(VB.SyntaxKind.IdentifierName) Then
                                Dim firstIdentifier As IdentifierNameSyntax = DirectCast(binExpr.FirstExpression, IdentifierNameSyntax)
                                Dim ifStatement As IfStatementSyntax =
                                   Factory.IfStatement(IfKeyword, Factory.IsExpression(left:=firstIdentifier, right:=NothingExpression), ThenKeyword).WithConvertedLeadingTriviaFrom(node)
                                Dim statements As New SyntaxList(Of StatementSyntax)
                                statements = statements.Add(Factory.SimpleAssignmentStatement(left:=firstIdentifier, right:=binExpr.SecondExpression))

                                Dim ifBlockToBeAdded As StatementSyntax = Factory.MultiLineIfBlock(
                                                                    ifStatement:=ifStatement,
                                                                    statements:=statements,
                                                                    elseIfBlocks:=Nothing,
                                                                    elseBlock:=Nothing,
                                                                    Factory.EndIfStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), IfKeyword).WithConvertedTrailingTriviaFrom(node).WithTrailingEol).
                                 WithPrependedLeadingTrivia(Factory.CommentTrivia($"' TODO: Check, VB does not directly support MemberAccess off a Conditional If Expression")).WithTrailingEol
                                statementWithIssue.AddMarker(ifBlockToBeAdded, StatementHandlingOption.PrependStatement, allowDuplicates:=True)
                                Return uniqueIdentifier
                            End If
                        End If
                        Dim awaitNotSupported As String = ""
                        If initializer.Value.IsKind(VB.SyntaxKind.AwaitExpression) AndAlso Not IsDecedentOfAsyncMethod(node) Then
                            initializer = Factory.EqualsValue(CType(initializer.Value, AwaitExpressionSyntax).Expression)
                            awaitNotSupported = " Await removed, in non Async Function,"
                        End If
                        declarationToBeAdded = FactoryDimStatement(uniqueName, asClause:=Nothing, initializer).WithPrependedLeadingTrivia(Factory.CommentTrivia($" ' TODO: Check,{awaitNotSupported} VB does not directly support MemberAccess off a Conditional If Expression")).WithTrailingEol
                        statementWithIssue.AddMarker(declarationToBeAdded, StatementHandlingOption.PrependStatement, allowDuplicates:=False)
                        Return uniqueIdentifier
                    ElseIf TypeOf node.Parent Is CSS.ConditionalAccessExpressionSyntax Then
                        Dim uniqueName As String = node.GetUniqueVariableNameInScope("tempVar", _usedIdentifiers, _semanticModel)
                        declarationToBeAdded = FactoryDimStatement(uniqueName, asClause:=Nothing, initializer).WithConvertedTriviaFrom(node).WithTrailingEol
                        statementWithIssue.AddMarker(declarationToBeAdded, StatementHandlingOption.PrependStatement, allowDuplicates:=False)
                        Return Factory.IdentifierName(Factory.Identifier(uniqueName))
                    End If
                End If

                Return Factory.ParenthesizedExpression(expr.WithoutTrailingTrivia).WithTrailingTrivia(expr.GetTrailingTrivia)
            End Function

            Public Overrides Function VisitParenthesizedLambdaExpression(node As CSS.ParenthesizedLambdaExpressionSyntax) As VB.VisualBasicSyntaxNode
                Return Me.ConvertLambdaExpression(node, node.Body, node.ParameterList.Parameters, Factory.TokenList(node.AsyncKeyword))
            End Function

            Public Overrides Function VisitPostfixUnaryExpression(node As CSS.PostfixUnaryExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim csExprKind As CS.SyntaxKind = CS.CSharpExtensions.Kind(node)
                Dim leftExpr As ExpressionSyntax = DirectCast(node.Operand.Accept(Me), ExpressionSyntax)
                If csExprKind = CS.SyntaxKind.SuppressNullableWarningExpression Then
                    Return leftExpr
                End If
                Dim kind As VB.SyntaxKind = csExprKind.GetExpressionKind()
                If TypeOf node.Parent Is CSS.ExpressionStatementSyntax OrElse TypeOf node.Parent Is CSS.ForStatementSyntax Then
                    Try
                        Dim operandTypeInfo As TypeInfo = _semanticModel.GetTypeInfo(node.Operand)
                        If operandTypeInfo.ConvertedType.ToString = "char" Then
                            Dim convertedOperandExpression As ExpressionSyntax = Factory.ParseExpression($"ChrW(AscW({leftExpr}))").WithTriviaFrom(leftExpr)
                            kind = If(kind = VB.SyntaxKind.AddAssignmentStatement, VB.SyntaxKind.AddExpression, VB.SyntaxKind.SubtractExpression)
                            Dim mathExpression As ExpressionSyntax = Factory.BinaryExpression(kind, convertedOperandExpression, kind.GetOperatorToken(isReferenceType:=False), OneExpression)
                            Return Factory.AssignmentStatement(VB.SyntaxKind.SimpleAssignmentStatement,
                                                               leftExpr,
                                                               EqualsToken,
                                                               mathExpression)

                        End If
                    Catch ex As OperationCanceledException
                        Throw
                    Catch ex As Exception
                        Stop
                        ' ignore
                    End Try

                    Return Factory.AssignmentStatement(CS.CSharpExtensions.Kind(node).GetExpressionKind(),
                                                       leftExpr, kind.GetOperatorToken(isReferenceType:=False),
                                                       OneExpression)
                Else
                    Dim operatorName As IdentifierNameSyntax
                    Dim minMax As IdentifierNameSyntax
                    Dim op As VB.SyntaxKind
                    If kind = VB.SyntaxKind.AddAssignmentStatement Then
                        operatorName = IncrementIdentifier
                        minMax = MinIdentifier
                        op = VB.SyntaxKind.SubtractExpression
                    Else
                        operatorName = DecrementIdentifier
                        minMax = MaxIdentifier
                        op = VB.SyntaxKind.AddExpression
                    End If
                    Dim vbMathExpression As MemberAccessExpressionSyntax = Factory.MemberAccessExpression(VB.SyntaxKind.SimpleMemberAccessExpression, MathIdentifier, DotToken, minMax)
                    Dim vbInterlockedExpressionName As MemberAccessExpressionSyntax = Factory.MemberAccessExpression(VB.SyntaxKind.SimpleMemberAccessExpression, InterlockedIdentifier, DotToken, operatorName)

                    Dim vbOperandArgument As SimpleArgumentSyntax = Factory.SimpleArgument(leftExpr)

                    Dim vbOperandArgumentList As ArgumentListSyntax = Factory.ArgumentList(Factory.SingletonSeparatedList(Of ArgumentSyntax)(vbOperandArgument))
                    Dim vbArgumentInvocationExpression As InvocationExpressionSyntax = Factory.InvocationExpression(vbInterlockedExpressionName, vbOperandArgumentList)
                    Dim vbSecondArgumentSyntax As SimpleArgumentSyntax = Factory.SimpleArgument(Factory.BinaryExpression(op,
                                                                                                                         leftExpr, op.GetOperatorToken(isReferenceType:=False),
                                                                                                                         OneExpression)
                                                                                                                        )
                    Return Factory.InvocationExpression(
                        vbMathExpression,
                        Factory.ArgumentList(Factory.SeparatedList(New ArgumentSyntax() {Factory.SimpleArgument(vbArgumentInvocationExpression),
                                                                                                      vbSecondArgumentSyntax}))).WithConvertedTriviaFrom(node)
                End If
            End Function

            Public Overrides Function VisitPrefixUnaryExpression(node As CSS.PrefixUnaryExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim kind As VB.SyntaxKind = CS.CSharpExtensions.Kind(node).GetExpressionKind()
                If kind = CS.SyntaxKind.PointerIndirectionExpression Then
                    Dim stmtWithIssues As CS.CSharpSyntaxNode = GetStatementWithIssues(node)
                    stmtWithIssues.AddMarker(FlagUnsupportedStatements(stmtWithIssues, "IndirectPointer Expressions", commentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, allowDuplicates:=False)
                    Return NothingExpression
                End If
                If kind = CS.SyntaxKind.IndexExpression Then
                    Dim operand As VB.VisualBasicSyntaxNode = node.Operand.Accept(Me)
                    Return Factory.ParseExpression($"-{operand}")
                End If
                Dim vbOperandExpression As ExpressionSyntax = DirectCast(node.Operand.Accept(Me), ExpressionSyntax)
                If TypeOf node.Parent Is CSS.ExpressionStatementSyntax Then
                    Return Factory.AssignmentStatement(kind,
                                                       vbOperandExpression, kind.GetOperatorToken(isReferenceType:=False),
                                                       OneExpression).WithConvertedTriviaFrom(node)
                End If
                If kind = VB.SyntaxKind.AddAssignmentStatement OrElse kind = VB.SyntaxKind.SubtractAssignmentStatement Then
                    If node.Parent.IsKind(CS.SyntaxKind.ForStatement) Then
                        If kind = VB.SyntaxKind.AddAssignmentStatement Then
                            Return Factory.AddAssignmentStatement(vbOperandExpression.WithTrailingTrivia(SpaceTrivia),
                                                                  kind.GetOperatorToken(isReferenceType:=False),
                                                                  OneExpression).WithConvertedTriviaFrom(node)
                        Else
                            Return Factory.SubtractAssignmentStatement(vbOperandExpression.WithTrailingTrivia(SpaceTrivia),
                                                                       kind.GetOperatorToken(isReferenceType:=False),
                                                                       OneExpression).WithConvertedTriviaFrom(node)
                        End If
                    Else
                        Dim operatorName As IdentifierNameSyntax = If(kind = VB.SyntaxKind.AddAssignmentStatement, IncrementIdentifier, DecrementIdentifier)
                        Dim mathExpr As MemberAccessExpressionSyntax = Factory.MemberAccessExpression(VB.SyntaxKind.SimpleMemberAccessExpression, InterlockedIdentifier, DotToken, operatorName)
                        Return Factory.InvocationExpression(mathExpr,
                                                              Factory.ArgumentList(Factory.SeparatedList(
                                                                                        New ArgumentSyntax() {Factory.SimpleArgument(vbOperandExpression)})
                                                                                    )
                                                             )
                    End If
                End If
                If kind = VB.SyntaxKind.AddressOfExpression Then
                    Dim spaceTriviaList As SyntaxTriviaList
                    spaceTriviaList = spaceTriviaList.Add(SpaceTrivia)
                    Dim addressOfToken As SyntaxToken = AddressOfKeyword.With(spaceTriviaList, spaceTriviaList)
                    Return Factory.AddressOfExpression(addressOfToken, vbOperandExpression).WithConvertedTriviaFrom(node)
                End If
                Return Factory.UnaryExpression(kind,
                                               kind.GetOperatorToken(isReferenceType:=False),
                                               vbOperandExpression.WithLeadingTrivia(SpaceTrivia)
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
                Return MeExpression.WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitThrowExpression(node As CSS.ThrowExpressionSyntax) As VB.VisualBasicSyntaxNode
                Return Factory.ThrowStatement(DirectCast(node.Expression.Accept(Me), ExpressionSyntax)).WithTrailingEol
            End Function

            Public Overrides Function VisitTupleElement(node As CSS.TupleElementSyntax) As VB.VisualBasicSyntaxNode
                Try
                    If String.IsNullOrWhiteSpace(node.Identifier.ValueText) Then
                        Dim typedTupleElementSyntax1 As TypedTupleElementSyntax = Factory.TypedTupleElement(DirectCast(node.Type.Accept(Me), TypeSyntax))
                        Return typedTupleElementSyntax1
                    End If
                    Return Factory.NamedTupleElement(GenerateSafeVbToken(node.Identifier, node, _semanticModel, _usedIdentifiers).WithConvertedTriviaFrom(node.Type), Factory.SimpleAsClause(AsKeyword.WithTrailingTrivia(SpaceTrivia), attributeLists:=New SyntaxList(Of AttributeListSyntax), DirectCast(node.Type.Accept(Me).WithConvertedTriviaFrom(node.Identifier), TypeSyntax)))
                Catch ex As OperationCanceledException
                    Throw
                Catch ex As Exception
                    Throw
                End Try
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
                            Dim trivia As SyntaxTrivia = initialTriviaList(j)
                            Select Case trivia.RawKind
                                Case VB.SyntaxKind.WhitespaceTrivia
                                    afterWhiteSpace = True
                                    finalLeadingTrivia = finalLeadingTrivia.Add(trivia)
                                Case VB.SyntaxKind.EndOfLineTrivia
                                    finalLeadingTrivia = finalLeadingTrivia.Add(trivia)
                                    afterWhiteSpace = False
                                    If j < initialTriviaList.Count - 1 Then
                                        If finalLeadingTrivia.Count = 0 Then
                                            finalLeadingTrivia = finalLeadingTrivia.Add(SpaceTrivia)
                                            finalLeadingTrivia = finalLeadingTrivia.Add(LineContinuation)
                                        End If
                                    End If
                                Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                                    If Not finalLeadingTrivia.Last.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                                        If Not afterWhiteSpace Then
                                            finalLeadingTrivia = finalLeadingTrivia.Add(SpaceTrivia)
                                        End If
                                        finalLeadingTrivia = finalLeadingTrivia.Add(LineContinuation)
                                    End If

                                    finalLeadingTrivia = finalLeadingTrivia.Add(trivia)
                                    If j < initialTriviaList.Count - 1 AndAlso Not initialTriviaList(j + 1).IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                        finalLeadingTrivia = finalLeadingTrivia.Add(VbEolTrivia)
                                    End If
                                Case VB.SyntaxKind.DisableWarningDirectiveTrivia
                                    GetStatementWithIssues(node).AddMarker(Factory.EmptyStatement.WithLeadingTrivia(trivia), StatementHandlingOption.PrependStatement, allowDuplicates:=True)
                                Case VB.SyntaxKind.EnableWarningDirectiveTrivia
                                    GetStatementWithIssues(node).AddMarker(Factory.EmptyStatement.WithLeadingTrivia(trivia), StatementHandlingOption.AppendEmptyStatement, allowDuplicates:=True)
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
                    Dim identifier As IdentifierNameSyntax
                    If a.Expression.IsKind(CS.SyntaxKind.IdentifierName) Then
                        identifier = DirectCast(DirectCast(a.Expression, CSS.IdentifierNameSyntax).Accept(Me), IdentifierNameSyntax)
                    Else
                        Dim d As CSS.DeclarationExpressionSyntax = DirectCast(a.Expression, CSS.DeclarationExpressionSyntax)
                        identifier = DirectCast(d.Designation.Accept(Me), IdentifierNameSyntax)
                    End If
                    lArgumentSyntax.Add(Factory.SimpleArgument(identifier))
                Next
                Return Factory.TupleExpression(Factory.SeparatedList(lArgumentSyntax))
            End Function

            Public Overrides Function VisitTupleType(node As CSS.TupleTypeSyntax) As VB.VisualBasicSyntaxNode
                Dim tupleElementList As New List(Of TupleElementSyntax)
                tupleElementList.AddRange(node.Elements.Select(Function(a As CSS.TupleElementSyntax) DirectCast(a.Accept(Me), TupleElementSyntax)))
                Return Factory.TupleType(tupleElementList.ToArray)
            End Function

            Public Overrides Function VisitTypeOfExpression(node As CSS.TypeOfExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim nodeType As CSS.GenericNameSyntax = TryCast(node.Type, CSS.GenericNameSyntax)
                If nodeType IsNot Nothing Then
                    Dim argumentList As CSS.TypeArgumentListSyntax = nodeType.TypeArgumentList
                    If argumentList.Arguments.Count = 1 AndAlso argumentList.Arguments(0).IsKind(CS.SyntaxKind.OmittedTypeArgument) Then
                        Return Factory.GetTypeExpression(Factory.ParseTypeName($"{nodeType.Identifier.ValueText}()"))
                    End If
                End If
                Return Factory.GetTypeExpression(GetTypeSyntaxFromPossibleAddressOf(node.Type.Accept(Me)))
            End Function

        End Class

    End Class

End Namespace
