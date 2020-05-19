' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.InteropServices
Imports System.Text

Imports CSharpToVBCodeConverter.Util

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Simplification
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory

Namespace CSharpToVBCodeConverter.DestVisualBasic

    Partial Public Class CSharpConverter

        Partial Friend Class NodesVisitor
            Inherits CS.CSharpSyntaxVisitor(Of VB.VisualBasicSyntaxNode)
            Private Const IDictionary As String = "IDictionary"
            Private Const IEnumerable As String = "IEnumerable"
            Private Const IEnumerableOf As String = "IEnumerable(Of "

            Private Shared Function ConvertStatementIntoComment(node As CSS.ConditionalAccessExpressionSyntax) As List(Of SyntaxTrivia)
                Dim LeadTriviaList As New List(Of SyntaxTrivia)
                Dim stringList As String() = node.ToFullString.SplitLines
                LeadTriviaList.Add(VBEOLTrivia)
                LeadTriviaList.Add(VBFactory.CommentTrivia($"' TODO VB does not allow directives here, original statement"))
                For Each s As String In stringList
                    LeadTriviaList.Add(VBEOLTrivia)
                    LeadTriviaList.Add(VBFactory.CommentTrivia($"'    {s}"))
                Next
                Return LeadTriviaList
            End Function

            Private Shared Function ConvertToInterpolatedStringTextToken(CSharpToken As SyntaxToken) As SyntaxToken
                Dim TokenString As String = ConvertCSharpEscapes(CSharpToken.ValueText)
                Return VBFactory.InterpolatedStringTextToken(TokenString, TokenString)
            End Function

            Private Shared Function GetTypeSyntaxFromInterface(expressionConvertedType As ITypeSymbol) As TypeSyntax

                If Not expressionConvertedType.AllInterfaces.Any Then
                    If expressionConvertedType.ToString.EndsWith("IArityEnumerable", StringComparison.Ordinal) Then
                        Return PredefinedTypeInteger
                    End If
                    Return PredefinedTypeObject
                End If
                For Each NamedType As INamedTypeSymbol In expressionConvertedType.AllInterfaces
                    Dim index As Integer = NamedType.ToString.IndexOf(IEnumerableOf, StringComparison.Ordinal)
                    Dim NewType As String
                    If index > 0 Then
                        NewType = NamedType.ToString.Substring(index + IEnumerableOf.Length)
                        Return VBFactory.ParseName(NewType)
                    End If
                    index = NamedType.ToString.IndexOf(IDictionary, StringComparison.Ordinal)
                    If index > 0 Then
                        Return ConvertToType(NamedType)
                    End If
                    index = NamedType.ToString.IndexOf(IEnumerable, StringComparison.Ordinal)
                    If index > 0 Then
                        Return ConvertToType(NamedType)
                    End If
                Next

                Dim index1 As Integer = expressionConvertedType.ToString.IndexOf(IEnumerableOf, StringComparison.Ordinal)
                If index1 > 0 Then
                    Dim NewType As String = expressionConvertedType.ToString.Substring(index1 + IEnumerableOf.Length)
                    Return VBFactory.ParseName(NewType)
                End If
                Return Nothing
            End Function

            Private Shared Function RestructureTrivia(node As CSS.MemberAccessExpressionSyntax, TriviaList As SyntaxTriviaList, FoundEOL As Boolean, ByRef OperatorTrailingTrivia As List(Of SyntaxTrivia)) As Boolean
                For Each Trivia As SyntaxTrivia In TriviaList
                    Select Case Trivia.RawKind
                        Case VB.SyntaxKind.CommentTrivia
                            OperatorTrailingTrivia.Add(Trivia)
                            FoundEOL = True
                        Case VB.SyntaxKind.EndOfLineTrivia
                            FoundEOL = True
                        Case VB.SyntaxKind.WhitespaceTrivia
                            OperatorTrailingTrivia.Add(SpaceTrivia)
                        Case VB.SyntaxKind.DisableWarningDirectiveTrivia,
                             VB.SyntaxKind.EnableWarningDirectiveTrivia
                            ' Ignore
                        Case VB.SyntaxKind.IfDirectiveTrivia
                            Dim StatementWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                            StatementWithIssues.AddMarker(VBFactory.EmptyStatement.WithLeadingTrivia(Trivia), StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                        Case Else
                            Stop
                    End Select
                Next

                Return FoundEOL
            End Function

            Private Shared Function UnpackExpressionFromStatement(statementSyntax As StatementSyntax, <Out> ByRef expression As ExpressionSyntax) As Boolean
                If TypeOf statementSyntax Is ReturnStatementSyntax Then
                    expression = DirectCast(statementSyntax, ReturnStatementSyntax).Expression
                ElseIf TypeOf statementSyntax Is YieldStatementSyntax Then
                    expression = DirectCast(statementSyntax, YieldStatementSyntax).Expression
                Else
                    expression = Nothing
                End If

                Return expression IsNot Nothing
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
                    Dim _symbolInfo As SymbolInfo = ModelExtensions.GetSymbolInfo(_mSemanticModel, node)
                    symbol = TryCast(_symbolInfo.Symbol, IMethodSymbol)
                    returnsVoid = symbol.ReturnsVoid
                    isErrorType = symbol.ReturnType.IsErrorType
                Catch ex As ArgumentException
                    ' Ignore this is expected
                Catch ex As OperationCanceledException
                    Throw
                Catch ex As Exception
                    Stop
                End Try
                Dim isFunction As Boolean = Not (returnsVoid OrElse TypeOf node.Body Is CSS.AssignmentExpressionSyntax)
                Dim modifiersList As List(Of SyntaxToken) = ConvertModifiers(Modifiers, IsModule, TokenContext.Local)
                Dim endSubOrFunctionStatement As EndBlockStatementSyntax
                Dim parameterList As ParameterListSyntax = VBFactory.ParameterList(VBFactory.SeparatedList(vbNodes, vbSeparators))
                Dim csBraces As (LeftBrace As SyntaxToken, RightBrace As SyntaxToken) = node.Body.GetBraces
                Dim asClause As AsClauseSyntax
                If isFunction Then
                    Dim dontAddAsClause As Boolean = isErrorType OrElse symbol.ReturnType.ToString.Contains("?", StringComparison.Ordinal) OrElse symbol.ReturnType.ToString.StartsWith("<anonymous type: ", StringComparison.Ordinal)
                    asClause = If(dontAddAsClause,
                                    Nothing,
                                    VBFactory.SimpleAsClause(ConvertToType(symbol.ReturnType))
                                    )
                    lambdaHeader = VBFactory.FunctionLambdaHeader(VBFactory.List(Of AttributeListSyntax)(), VBFactory.TokenList(modifiersList), parameterList, asClause:=CType(asClause, SimpleAsClauseSyntax))
                    endSubOrFunctionStatement = VBFactory.EndFunctionStatement().WithConvertedTriviaFrom(csBraces.RightBrace)
                Else
                    lambdaHeader = VBFactory.SubLambdaHeader(VBFactory.List(Of AttributeListSyntax)(), VBFactory.TokenList(modifiersList), parameterList, asClause:=Nothing)
                    endSubOrFunctionStatement = VBFactory.EndSubStatement().WithConvertedTriviaFrom(csBraces.RightBrace)
                End If
                If TypeOf block Is CSS.BlockSyntax Then
                    block = DirectCast(block, CSS.BlockSyntax).Statements
                End If

                Dim statements As New SyntaxList(Of StatementSyntax)
                Dim endBlock As EndBlockStatementSyntax
                If TypeOf block Is CS.CSharpSyntaxNode Then
                    Dim body As VB.VisualBasicSyntaxNode = DirectCast(block, CS.CSharpSyntaxNode).Accept(Me)
                    If TypeOf block Is CSS.ThrowExpressionSyntax Then
                        statements = VBFactory.SingletonList(DirectCast(body, StatementSyntax).WithTrailingEOL)
                        If isFunction Then
                            Return VBFactory.MultiLineLambdaExpression(VB.SyntaxKind.MultiLineFunctionLambdaExpression, lambdaHeader.WithTrailingEOL, statements, endSubOrFunctionStatement).WithConvertedTriviaFrom(node)
                        End If
                        Return VBFactory.MultiLineLambdaExpression(VB.SyntaxKind.MultiLineSubLambdaExpression, lambdaHeader.WithTrailingEOL, statements, endSubOrFunctionStatement).WithConvertedTriviaFrom(node)
                    End If
                    If TypeOf block Is CSS.ObjectCreationExpressionSyntax Then
                        If isFunction Then
                            statements = VBFactory.SingletonList(Of StatementSyntax)(VBFactory.ReturnStatement(DirectCast(body, NewExpressionSyntax)).WithTrailingEOL)
                            Return VBFactory.MultiLineLambdaExpression(VB.SyntaxKind.MultiLineFunctionLambdaExpression, lambdaHeader.WithTrailingEOL, statements, endSubOrFunctionStatement).WithConvertedTriviaFrom(node)
                        End If
                        Dim names As SeparatedSyntaxList(Of ModifiedIdentifierSyntax) = VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier("DoNotCare"))
                        asClause = VBFactory.AsNewClause(DirectCast(body, NewExpressionSyntax))
                        Dim declarators As SeparatedSyntaxList(Of VariableDeclaratorSyntax) = VBFactory.SingletonSeparatedList(VBFactory.VariableDeclarator(names, asClause, initializer:=Nothing))
                        statements = VBFactory.SingletonList(Of StatementSyntax)(VBFactory.LocalDeclarationStatement(DimModifier, declarators))
                        Return VBFactory.MultiLineLambdaExpression(VB.SyntaxKind.MultiLineSubLambdaExpression, lambdaHeader.WithTrailingEOL, statements, endSubOrFunctionStatement).WithConvertedTriviaFrom(node)
                    End If
                    If body.IsKind(VB.SyntaxKind.SimpleAssignmentStatement) Then
                        Dim simpleAssignment As AssignmentStatementSyntax = DirectCast(body, AssignmentStatementSyntax)
                        If simpleAssignment.Left.IsKind(VB.SyntaxKind.SimpleMemberAccessExpression) Then
                            Dim memberAccessExpression As MemberAccessExpressionSyntax = DirectCast(simpleAssignment.Left, MemberAccessExpressionSyntax)
                            Select Case memberAccessExpression.Expression.Kind
                                Case VB.SyntaxKind.ObjectCreationExpression, VB.SyntaxKind.SimpleMemberAccessExpression
                                    endBlock = VBFactory.EndBlockStatement(VB.SyntaxKind.EndSubStatement, SubKeyword)
                                    Dim uniqueName As String = MethodBodyVisitor.GetUniqueVariableNameInScope(node, "tempVar", _mSemanticModel)
                                    Dim uniqueIdentifier As IdentifierNameSyntax = VBFactory.IdentifierName(VBFactory.Identifier(uniqueName))
                                    Dim names As SeparatedSyntaxList(Of ModifiedIdentifierSyntax) = VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(uniqueName))
                                    Dim dimStatement As LocalDeclarationStatementSyntax

                                    If TypeOf memberAccessExpression.Expression Is NewExpressionSyntax Then
                                        asClause = VBFactory.AsNewClause(DirectCast(memberAccessExpression.Expression, NewExpressionSyntax))
                                        Dim variableDeclaration As SeparatedSyntaxList(Of VariableDeclaratorSyntax) = VBFactory.SingletonSeparatedList(VBFactory.VariableDeclarator(names, asClause, initializer:=Nothing))
                                        dimStatement = VBFactory.LocalDeclarationStatement(DimModifier, variableDeclaration)
                                    ElseIf TypeOf memberAccessExpression.Expression Is MemberAccessExpressionSyntax Then
                                        Dim memberAccess As MemberAccessExpressionSyntax = DirectCast(memberAccessExpression.Expression, MemberAccessExpressionSyntax)
                                        If TypeOf memberAccess.Expression IsNot NewExpressionSyntax Then
                                            Exit Select
                                        End If
                                        asClause = VBFactory.AsNewClause(DirectCast(memberAccess.Expression, NewExpressionSyntax))
                                        Dim VariableDeclaration As SeparatedSyntaxList(Of VariableDeclaratorSyntax) = VBFactory.SingletonSeparatedList(VBFactory.VariableDeclarator(names, asClause, initializer:=Nothing))
                                        dimStatement = VBFactory.LocalDeclarationStatement(DimModifier, VariableDeclaration)
                                    Else
                                        Exit Select
                                    End If
                                    statements = statements.Add(dimStatement)
                                    statements = statements.Add(VBFactory.SimpleAssignmentStatement(VBFactory.QualifiedName(uniqueIdentifier, memberAccessExpression.Name), simpleAssignment.Right).WithTrailingEOL)
                                    Return VBFactory.MultiLineLambdaExpression(VB.SyntaxKind.MultiLineSubLambdaExpression, lambdaHeader.WithTrailingEOL, statements, endBlock).WithConvertedTriviaFrom(node)
                                Case VB.SyntaxKind.IdentifierName, VB.SyntaxKind.InvocationExpression, VB.SyntaxKind.MeExpression
                                    ' handled below
                                Case Else
                                    Stop
                            End Select
                        End If
                    End If
                    If isFunction Then
                        Return VBFactory.SingleLineLambdaExpression(VB.SyntaxKind.SingleLineFunctionLambdaExpression, lambdaHeader.WithAsClause(Nothing), body).WithConvertedTriviaFrom(node)
                    End If
                    Return VBFactory.SingleLineLambdaExpression(VB.SyntaxKind.SingleLineSubLambdaExpression, lambdaHeader.WithAsClause(Nothing), body).WithConvertedTriviaFrom(node)
                End If

                ' TypeOf block Is SyntaxList(Of CSS.StatementSyntax)
                statements = statements.AddRange(VBFactory.List(DirectCast(block, SyntaxList(Of CSS.StatementSyntax)).SelectMany(Function(s As CSS.StatementSyntax) s.Accept(New MethodBodyVisitor(_mSemanticModel, Me)))))
                Dim expression As ExpressionSyntax = Nothing
                If statements.Count = 1 AndAlso UnpackExpressionFromStatement(statements(0), expression) Then
                    Return VBFactory.SingleLineLambdaExpression(
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
                    endBlock = VBFactory.EndBlockStatement(VB.SyntaxKind.EndFunctionStatement, FunctionKeyword).WithConvertedTriviaFrom(csBraces.RightBrace)
                    expressionKind = VB.SyntaxKind.MultiLineFunctionLambdaExpression
                Else
                    endBlock = VBFactory.EndBlockStatement(VB.SyntaxKind.EndSubStatement, SubKeyword).WithConvertedTriviaFrom(csBraces.RightBrace)
                    expressionKind = VB.SyntaxKind.MultiLineSubLambdaExpression
                End If
                Return VBFactory.MultiLineLambdaExpression(kind:=expressionKind,
                                                               lambdaHeader.WithTrailingEOL,
                                                               statements,
                                                               endBlock)
            End Function

            Private Function IsConcatenateStringsExpression(node As CSS.BinaryExpressionSyntax) As Boolean
                If Not node.IsKind(CS.SyntaxKind.AddExpression) Then
                    Return False
                End If
                If IsStringExpression(node.Left) OrElse IsStringExpression(node.Right) Then
                    Return True
                End If
                Dim LeftNodeTypeInfo As TypeInfo
                Dim RightNodeTypeInfo As TypeInfo
                Try
                    LeftNodeTypeInfo = ModelExtensions.GetTypeInfo(_mSemanticModel, node.Left)
                    RightNodeTypeInfo = ModelExtensions.GetTypeInfo(_mSemanticModel, node.Right)
                Catch ex As ArgumentException
                    ' ignore
                Catch ex As OperationCanceledException
                    Throw
                Catch ex As Exception
                    Stop
                End Try
                Dim LeftAndRightIsString As Boolean? = LeftNodeTypeInfo.ConvertedType?.SpecialType = SystemString OrElse RightNodeTypeInfo.ConvertedType?.SpecialType = SystemString
                Return LeftAndRightIsString.HasValue AndAlso LeftAndRightIsString.Value
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
                        _Typeinfo = ModelExtensions.GetTypeInfo(_mSemanticModel, BinaryExpression)
                        If _Typeinfo.IsString Then
                            Return True
                        End If
                        _Typeinfo = ModelExtensions.GetTypeInfo(_mSemanticModel, BinaryExpression.Left)
                        If _Typeinfo.IsString Then
                            Return True
                        End If
                        _Typeinfo = ModelExtensions.GetTypeInfo(_mSemanticModel, BinaryExpression.Right)
                    ElseIf TypeOf Node Is CSS.MemberAccessExpressionSyntax Then
                        _Typeinfo = ModelExtensions.GetTypeInfo(_mSemanticModel, CType(Node, CSS.MemberAccessExpressionSyntax).Expression)
                    Else
                        _Typeinfo = ModelExtensions.GetTypeInfo(_mSemanticModel, Node)
                    End If
                Catch ex As ArgumentException
                Catch ex As Exception
                    Stop
                    Throw
                End Try
                Return _Typeinfo.IsString
            End Function

            Private Function MakeAssignmentStatement(node As CSS.AssignmentExpressionSyntax) As StatementSyntax
                Dim LeftNode As ExpressionSyntax = DirectCast(node.Left.Accept(Me), ExpressionSyntax)
                If CS.CSharpExtensions.Kind(node) = CS.SyntaxKind.CoalesceAssignmentExpression Then
                    Dim PossibleNullNode As ExpressionSyntax = DirectCast(node.Right.Accept(Me).WithLeadingTrivia(SpaceTrivia), ExpressionSyntax)
                    Dim rightBinaryExpression As BinaryConditionalExpressionSyntax = VBFactory.BinaryConditionalExpression(LeftNode.WithoutTrivia, PossibleNullNode)
                    Dim AssignmentStatement As AssignmentStatementSyntax = VBFactory.SimpleAssignmentStatement(LeftNode, rightBinaryExpression)
                    Dim LeadingTrivia As New List(Of SyntaxTrivia)
                    If AssignmentStatement.HasLeadingTrivia Then
                        LeadingTrivia.AddRange(AssignmentStatement.GetLeadingTrivia)
                        AssignmentStatement = AssignmentStatement.WithLeadingTrivia(SpaceTrivia)
                    End If
                    Dim AssignmentStatements As SyntaxList(Of StatementSyntax) = VBFactory.SingletonList(Of StatementSyntax)(AssignmentStatement)
                    Return AssignmentStatement.With(LeadingTrivia, ConvertTrivia(node.GetTrailingTrivia))
                End If
                Dim kind As VB.SyntaxKind = ConvertCSExpressionsKindToVBKind(CS.CSharpExtensions.Kind(node))
                Dim OperatorToken As SyntaxToken = ExpressionKindToOperatorToken(kind)
                Dim RightNode As ExpressionSyntax
                If node.Right.IsKind(CS.SyntaxKind.CoalesceExpression) Then
                    Dim csRight As CSS.BinaryExpressionSyntax = DirectCast(node.Right, CSS.BinaryExpressionSyntax)
                    If csRight.Right.IsKind(CS.SyntaxKind.ThrowExpression) Then
                        Dim TestNode As ExpressionSyntax = DirectCast(csRight.Left.Accept(Me).WithLeadingTrivia(SpaceTrivia), ExpressionSyntax)
                        Dim SecondExpression As ThrowStatementSyntax = DirectCast(csRight.Right.Accept(Me).WithConvertedTriviaFrom(csRight.Right), ThrowStatementSyntax).WithTrailingEOL
                        Dim Statements As SyntaxList(Of StatementSyntax) = VBFactory.SingletonList(Of StatementSyntax)(SecondExpression)

                        Dim Condition As ExpressionSyntax = VBFactory.IsExpression(TestNode, NothingExpression)
                        Dim IfBlock As SingleLineIfStatementSyntax = VBFactory.SingleLineIfStatement(Condition,
                                                                                                          Statements,
                                                                                                          elseClause:=Nothing)
                        Dim StatementWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                        StatementWithIssues.AddMarker(IfBlock, StatementHandlingOption.PrependStatement, AllowDuplicates:=False)
                        RightNode = DirectCast(csRight.Left.Accept(Me), ExpressionSyntax)
                    Else
                        RightNode = DirectCast(node.Right.Accept(Me), ExpressionSyntax)
                    End If
                Else
                    RightNode = DirectCast(node.Right.Accept(Me), ExpressionSyntax)
                    RightNode = If(RightNode, VBFactory.IdentifierName("HandleRefExpression"))
                End If
                If node.IsKind(CS.SyntaxKind.AndAssignmentExpression,
                               CS.SyntaxKind.OrAssignmentExpression,
                               CS.SyntaxKind.ExclusiveOrAssignmentExpression,
                               CS.SyntaxKind.ModuloAssignmentExpression) Then
                    Return VBFactory.SimpleAssignmentStatement(LeftNode, VBFactory.BinaryExpression(kind, LeftNode.WithoutTrivia, OperatorToken, RightNode.WithoutTrivia))
                End If
                If kind = VB.SyntaxKind.AddAssignmentStatement AndAlso
                    RightNode.IsKind(VB.SyntaxKind.ObjectCreationExpression) Then
                    Dim RightNodeObjectCreation As ObjectCreationExpressionSyntax = DirectCast(RightNode, ObjectCreationExpressionSyntax)
                    If RightNodeObjectCreation.ArgumentList.Arguments.Count = 1 AndAlso
                        RightNodeObjectCreation.ArgumentList.Arguments(0).IsKind(VB.SyntaxKind.SimpleArgument) Then
                        If DirectCast(RightNodeObjectCreation.ArgumentList.Arguments(0), SimpleArgumentSyntax).Expression.IsKind(VB.SyntaxKind.AddressOfExpression) Then
                            Return VBFactory.AddHandlerStatement(LeftNode.WithLeadingTrivia(SpaceTrivia), RightNode).WithLeadingTrivia(LeftNode.GetLeadingTrivia)
                        End If
                    End If
                End If
                Return VBFactory.AssignmentStatement(kind,
                                                CType(LeftNode.WithModifiedNodeTrivia(SeparatorFollows:=True), ExpressionSyntax),
                                                ExpressionKindToOperatorToken(kind),
                                                RightNode)
            End Function

            Private Sub MarkPatchInlineAssignHelper(node As CS.CSharpSyntaxNode)
                Dim parentDefinition As CSS.BaseTypeDeclarationSyntax = node.AncestorsAndSelf().OfType(Of CSS.BaseTypeDeclarationSyntax)().FirstOrDefault()
                _inlineAssignHelperMarkers.Add(parentDefinition)
            End Sub

            Private Function ReduceArrayUpperBoundExpression(expr As CSS.ExpressionSyntax) As ExpressionSyntax
                Dim constant As [Optional](Of Object) = _mSemanticModel.GetConstantValue(expr)
                If constant.HasValue AndAlso TypeOf constant.Value Is Integer Then
                    Return VBFactory.NumericLiteralExpression(VBFactory.Literal(CInt(constant.Value) - 1))
                End If
                Return VBFactory.BinaryExpression(kind:=VB.SyntaxKind.SubtractExpression, left:=DirectCast(expr.Accept(Me), ExpressionSyntax), operatorToken:=MinusToken, right:=VBFactory.NumericLiteralExpression(VBFactory.Literal(1)))
            End Function

            Public Shared Function ConvertAndModifyNodeTrivia(Node As VB.VisualBasicSyntaxNode, NodesOrTokens As List(Of SyntaxNodeOrToken), Index As Integer) As VB.VisualBasicSyntaxNode
                If NodesOrTokens Is Nothing Then
                    Throw New ArgumentNullException(NameOf(NodesOrTokens))
                End If
                Dim afterWhiteSpace As Boolean = False
                Dim afterLineContinuation As Boolean = False
                Dim initialTriviaList As List(Of SyntaxTrivia) = ConvertTrivia(NodesOrTokens(Index).GetLeadingTrivia).ToList
                Dim initialTriviaListUBound As Integer = initialTriviaList.Count - 1
                Dim firstTrivia As Boolean = True
                Dim finalLeadingTriviaList As New List(Of SyntaxTrivia)
                For triviaListIndex As Integer = 0 To initialTriviaListUBound
                    Dim Trivia As SyntaxTrivia = initialTriviaList(triviaListIndex)
                    Dim nextTrivia As SyntaxTrivia = If(triviaListIndex < initialTriviaListUBound, initialTriviaList(triviaListIndex + 1), New SyntaxTrivia)
                    Select Case Trivia.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            afterLineContinuation = False
                            afterWhiteSpace = True
                            firstTrivia = False
                            finalLeadingTriviaList.Add(Trivia)
                        Case VB.SyntaxKind.EndOfLineTrivia
                            ' we want to skip any leading trivia
                            If Not firstTrivia Then
                                finalLeadingTriviaList.Add(Trivia)
                                afterLineContinuation = False
                                afterWhiteSpace = False
                                If Index < NodesOrTokens.Count - 1 Then
                                    If finalLeadingTriviaList.Count = 0 Then
                                        finalLeadingTriviaList.Add(SpaceTrivia)
                                        finalLeadingTriviaList.Add(LineContinuation)
                                    End If
                                End If
                            End If
                        Case VB.SyntaxKind.CommentTrivia
                            firstTrivia = False
                            If Not afterWhiteSpace Then
                                finalLeadingTriviaList.Add(SpaceTrivia)
                            End If
                            finalLeadingTriviaList.Add(LineContinuation)
                            finalLeadingTriviaList.Add(Trivia)
                            If Not nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                finalLeadingTriviaList.Add(VBEOLTrivia)
                            End If
                        Case VB.SyntaxKind.DisableWarningDirectiveTrivia, VB.SyntaxKind.EnableWarningDirectiveTrivia
                            firstTrivia = False
                            Stop
                        Case VB.SyntaxKind.IfDirectiveTrivia
                            firstTrivia = False
                            finalLeadingTriviaList.AddRange(DirectiveNotAllowedHere(Trivia))
                        Case VB.SyntaxKind.DisabledTextTrivia
                            firstTrivia = False
                            finalLeadingTriviaList.AddRange(DirectiveNotAllowedHere(Trivia))
                        Case VB.SyntaxKind.ElseDirectiveTrivia
                            firstTrivia = False
                            finalLeadingTriviaList.AddRange(DirectiveNotAllowedHere(Trivia))
                        Case VB.SyntaxKind.EndIfDirectiveTrivia
                            firstTrivia = False
                            finalLeadingTriviaList.AddRange(DirectiveNotAllowedHere(Trivia))
                        Case Else
                            Stop
                    End Select
                Next
                initialTriviaList.Clear()
                initialTriviaList.AddRange(ConvertTrivia(NodesOrTokens(Index).GetTrailingTrivia))
                initialTriviaListUBound = initialTriviaList.Count - 1

                Dim finalTrailingTriviaList As New List(Of SyntaxTrivia)
                For initialTriviaIndex As Integer = 0 To initialTriviaList.Count - 1
                    Dim trivia As SyntaxTrivia = initialTriviaList(initialTriviaIndex)
                    Select Case trivia.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            finalTrailingTriviaList.Add(trivia)
                        Case VB.SyntaxKind.EndOfLineTrivia
                            ' What to do depends on whats next
                            If Index < NodesOrTokens.Count - 1 Then
                                Dim j As Integer
                                Dim NewWhiteSpaceString As String = ""
                                If initialTriviaIndex < initialTriviaListUBound Then
                                    For j = initialTriviaIndex + 1 To initialTriviaListUBound
                                        If initialTriviaList(j).RawKind = VB.SyntaxKind.WhitespaceTrivia Then
                                            NewWhiteSpaceString &= initialTriviaList(j).ToString
                                            initialTriviaIndex += 1
                                        Else
                                            Exit For
                                        End If
                                    Next
                                End If
                                If j = 0 OrElse j < initialTriviaListUBound AndAlso initialTriviaList(j).RawKind = VB.SyntaxKind.CommentTrivia Then
                                    If String.IsNullOrWhiteSpace(NewWhiteSpaceString) Then
                                        finalTrailingTriviaList.Add(SpaceTrivia)
                                    Else
                                        finalTrailingTriviaList.Add(VBFactory.WhitespaceTrivia(NewWhiteSpaceString))
                                    End If
                                    finalTrailingTriviaList.Add(LineContinuation)
                                    finalTrailingTriviaList.Add(trivia)
                                    afterLineContinuation = True
                                Else
                                    finalTrailingTriviaList.Add(trivia)
                                    If Not String.IsNullOrWhiteSpace(NewWhiteSpaceString) Then
                                        finalTrailingTriviaList.Add(VBFactory.WhitespaceTrivia(NewWhiteSpaceString))
                                    End If
                                End If
                            Else
                                finalTrailingTriviaList.Add(trivia)
                                afterLineContinuation = False
                                afterWhiteSpace = False
                            End If
                        Case VB.SyntaxKind.CommentTrivia
                            If Not afterWhiteSpace Then
                                finalTrailingTriviaList.Add(SpaceTrivia)
                            End If
                            If Not afterLineContinuation Then
                                finalTrailingTriviaList.Add(LineContinuation)
                                finalTrailingTriviaList.Add(SpaceTrivia)
                            End If
                            finalTrailingTriviaList.Add(trivia)
                            afterLineContinuation = False
                            afterWhiteSpace = False
                        Case Else
                            Stop
                    End Select
                Next
                If Node Is Nothing Then
                    Throw New ArgumentNullException(NameOf(Node))
                End If
                Return Node.With(finalLeadingTriviaList, finalTrailingTriviaList)
            End Function

            Public Shared Function ConvertAndModifyTokenTrivia(Token As SyntaxToken, NodesOrTokens As List(Of SyntaxNodeOrToken), Index As Integer) As SyntaxToken
                If NodesOrTokens Is Nothing Then
                    Throw New ArgumentNullException(NameOf(NodesOrTokens))
                End If
                Dim initialTriviaList As List(Of SyntaxTrivia) = ConvertTrivia(NodesOrTokens(Index).GetLeadingTrivia).ToList
                Dim initialTriviaListUBound As Integer = initialTriviaList.Count - 1
                Dim afterWhiteSpace As Boolean = False
                Dim afterLineContinuation As Boolean = False
                Dim finalLeadingTriviaList As New List(Of SyntaxTrivia)
                For initialTriviaIndex As Integer = 0 To initialTriviaListUBound
                    Dim Trivia As SyntaxTrivia = initialTriviaList(initialTriviaIndex)
                    Select Case Trivia.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            afterLineContinuation = False
                            afterWhiteSpace = True
                            finalLeadingTriviaList.Add(Trivia)
                        Case VB.SyntaxKind.EndOfLineTrivia
                            finalLeadingTriviaList.Add(Trivia)
                            afterLineContinuation = False
                            afterWhiteSpace = False
                            ' What I do depends on whats next
                            If initialTriviaIndex < initialTriviaListUBound Then
                                Dim j As Integer
                                Dim newWhiteSpaceString As String = ""
                                For j = initialTriviaIndex + 1 To initialTriviaListUBound
                                    If initialTriviaList(j).RawKind = VB.SyntaxKind.WhitespaceTrivia Then
                                        newWhiteSpaceString &= initialTriviaList(j).ToString
                                        initialTriviaIndex += 1
                                    Else
                                        Exit For
                                    End If
                                Next
                                If j < initialTriviaListUBound AndAlso initialTriviaList(j).RawKind = VB.SyntaxKind.CommentTrivia Then
                                    If String.IsNullOrWhiteSpace(newWhiteSpaceString) Then
                                        finalLeadingTriviaList.Add(SpaceTrivia)
                                    Else
                                        finalLeadingTriviaList.Add(VBFactory.WhitespaceTrivia(newWhiteSpaceString))
                                    End If
                                    finalLeadingTriviaList.Add(LineContinuation)
                                    afterLineContinuation = True
                                Else
                                    If Not String.IsNullOrWhiteSpace(newWhiteSpaceString) Then
                                        finalLeadingTriviaList.Add(VBFactory.WhitespaceTrivia(newWhiteSpaceString))
                                    End If
                                End If
                            End If
                        Case VB.SyntaxKind.CommentTrivia
                            If Not afterWhiteSpace Then
                                finalLeadingTriviaList.Add(SpaceTrivia)
                            End If
                            If Not afterLineContinuation Then
                                finalLeadingTriviaList.Add(LineContinuation)
                                finalLeadingTriviaList.Add(SpaceTrivia)
                            End If
                            finalLeadingTriviaList.Add(Trivia)
                            afterLineContinuation = False
                            afterWhiteSpace = False
                        Case VB.SyntaxKind.EndIfDirectiveTrivia
                            finalLeadingTriviaList.AddRange(DirectiveNotAllowedHere(Trivia))
                            finalLeadingTriviaList.Add(VBEOLTrivia)
                            afterLineContinuation = False
                            afterWhiteSpace = False
                        Case Else
                            Stop
                    End Select
                Next
                initialTriviaList.Clear()
                initialTriviaList.AddRange(ConvertTrivia(NodesOrTokens(Index).GetTrailingTrivia))
                Dim FinalTrailingTriviaList As New List(Of SyntaxTrivia)
                For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                    Dim Trivia As SyntaxTrivia = e.Value
                    Select Case Trivia.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            FinalTrailingTriviaList.Add(Trivia)
                            afterWhiteSpace = True
                        Case VB.SyntaxKind.EndOfLineTrivia
                            FinalTrailingTriviaList.Add(Trivia)
                            afterWhiteSpace = False
                        Case VB.SyntaxKind.CommentTrivia
                            If Not afterWhiteSpace = True Then
                                FinalTrailingTriviaList.Add(SpaceTrivia)
                            End If
                            FinalTrailingTriviaList.Add(LineContinuation)
                            FinalTrailingTriviaList.Add(Trivia)
                            afterWhiteSpace = False
                        Case Else
                            Stop
                    End Select
                Next
                Return Token.With(finalLeadingTriviaList, FinalTrailingTriviaList)
            End Function

            Public Shared Function GetElementType(_ITypeSymbol As ITypeSymbol) As TypeSyntax
                Dim _TypeSyntax As TypeSyntax = ConvertToType(_ITypeSymbol)
                If _TypeSyntax.IsKind(VB.SyntaxKind.ArrayType) Then
                    If DirectCast(_ITypeSymbol, IArrayTypeSymbol).ElementType.TypeKind = Microsoft.CodeAnalysis.TypeKind.Array Then
                        Return _TypeSyntax.NormalizeWhitespace
                    End If
                    Return DirectCast(_TypeSyntax, ArrayTypeSyntax).ElementType.NormalizeWhitespace
                End If
                If TypeOf _TypeSyntax Is QualifiedNameSyntax Then
                    Dim Right As SimpleNameSyntax = DirectCast(_TypeSyntax, QualifiedNameSyntax).Right
                    If TypeOf Right Is IdentifierNameSyntax Then
                        Dim TypeSyntax As TypeSyntax = GetTypeSyntaxFromInterface(_ITypeSymbol)
                        Return TypeSyntax
                    End If
                    Dim GenericdName As GenericNameSyntax = DirectCast(Right, GenericNameSyntax)
                    If GenericdName.TypeArgumentList.Arguments.Count = 1 Then
                        Return GenericdName.TypeArgumentList.Arguments(0)
                    Else
                        Return VBFactory.ParseTypeName(GenericdName.TypeArgumentList.Arguments.ToString & ")")
                    End If
                End If
                If TypeOf _TypeSyntax Is GenericNameSyntax Then
                    Dim GenericdName As GenericNameSyntax = CType(_TypeSyntax, GenericNameSyntax)
                    If GenericdName.TypeArgumentList.Arguments.Count = 1 Then
                        Return GenericdName.TypeArgumentList.Arguments(0)
                    Else
                        Return GetTypeSyntaxFromInterface(_ITypeSymbol)
                    End If
                End If
                If TypeOf _TypeSyntax Is IdentifierNameSyntax Then
                    Return VBFactory.ParseTypeName(_TypeSyntax.ToString)
                End If

                If _TypeSyntax.IsKind(VB.SyntaxKind.PredefinedType) Then
                    Select Case DirectCast(_TypeSyntax, PredefinedTypeSyntax).Keyword.ValueText.ToUpperInvariant
                        Case "BOOL", "BOOLEAN"
                            Return PredefinedTypeBoolean
                        Case "BYTE"
                            Return PredefinedTypeByte
                        Case "CHAR"
                            Return PredefinedTypeChar
                        Case "DECIMAL"
                            Return PredefinedTypeDecimal
                        Case "DOUBLE"
                            Return PredefinedTypeDouble
                        Case "INT", "Integer"
                            Return PredefinedTypeInteger
                        Case "SBYTE"
                            Return PredefinedTypeSByte
                        Case "SHORT"
                            Return PredefinedTypeShort
                        Case "UINT"
                            Return PredefinedTypeUInteger
                        Case "ULONG"
                            Return PredefinedTypeULong
                        Case "USHORT"
                            Return PredefinedTypeUShort
                        Case "STRING"
                            Return PredefinedTypeString
                        Case "OBJECT"
                            Return PredefinedTypeObject
                        Case Else
                            Stop
                    End Select
                End If
                Return VBFactory.PredefinedType(ObjectKeyword)
            End Function

            Public Overrides Function VisitAnonymousMethodExpression(node As CSS.AnonymousMethodExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim Parameters As New SeparatedSyntaxList(Of CSS.ParameterSyntax)
                If node.ParameterList IsNot Nothing Then
                    Parameters = CType((node.ParameterList?.Parameters), SeparatedSyntaxList(Of CSS.ParameterSyntax))
                End If
                Return ConvertLambdaExpression(node:=node, block:=node.Block.Statements, parameters:=Parameters, Modifiers:=VBFactory.TokenList(node.AsyncKeyword)).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitAnonymousObjectCreationExpression(node As CSS.AnonymousObjectCreationExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim FieldInitializers As New List(Of FieldInitializerSyntax)
                For Each e As IndexClass(Of CSS.AnonymousObjectMemberDeclaratorSyntax) In node.Initializers.WithIndex
                    Dim Initializer As CSS.AnonymousObjectMemberDeclaratorSyntax = e.Value
                    Dim LeadingTrivia As SyntaxTriviaList = VBFactory.TriviaList(ConvertTrivia(Initializer.GetLeadingTrivia))
                    If LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        If Not LeadingTrivia.ContainsEndIfTrivia Then
                            LeadingTrivia = LeadingTrivia.Insert(0, VBFactory.CommentTrivia(" ' TODO: Comment moved from middle of expression to end, check for correct placement"))
                        End If
                        Dim Statement As EmptyStatementSyntax = VBFactory.EmptyStatement.WithLeadingTrivia(LeadingTrivia)
                        GetStatementwithIssues(node).AddMarker(Statement, StatementHandlingOption.AppendEmptyStatement, AllowDuplicates:=True)
                        Initializer = Initializer.WithoutLeadingTrivia
                    End If
                    Dim Field As FieldInitializerSyntax = DirectCast(Initializer.Accept(Me), FieldInitializerSyntax).NormalizeWhitespaceEx(useDefaultCasing:=True, PreserveCRLF:=True)
                    Dim FirstTrivia As Boolean = True
                    Dim FoundComment As Boolean = False
                    Dim FieldLeadingTrivia As SyntaxTriviaList = Field.GetLeadingTrivia
                    Dim Comment As String = ""
                    Dim NewFieldLeadingTrivia As New List(Of SyntaxTrivia)
                    For Each trivia As IndexClass(Of SyntaxTrivia) In FieldLeadingTrivia.WithIndex
                        Dim t As SyntaxTrivia = trivia.Value
                        Select Case t.RawKind
                            Case VB.SyntaxKind.WhitespaceTrivia
                                If FirstTrivia = True Then
                                    NewFieldLeadingTrivia.Add(t)
                                End If
                            Case VB.SyntaxKind.EndOfLineTrivia
                                If FoundComment Then
                                    If trivia.IsLast Then
                                        NewFieldLeadingTrivia.Add(t)
                                    End If
                                    ' skip EOL's
                                Else
                                    NewFieldLeadingTrivia.Add(t)
                                End If
                            Case VB.SyntaxKind.CommentTrivia
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
                    If Comment.Length > 0 Then
                        NewFieldLeadingTrivia.Add(VBFactory.CommentTrivia(Comment))
                    End If
                    FieldInitializers.Add(Field.WithLeadingTrivia(NewFieldLeadingTrivia))
                Next
                Return VBFactory.AnonymousObjectCreationExpression(VBFactory.ObjectMemberInitializer(VBFactory.SeparatedList(FieldInitializers)))
            End Function

            Public Overrides Function VisitArrayCreationExpression(node As CSS.ArrayCreationExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim upperBoundArguments As IEnumerable(Of ArgumentSyntax) = node.Type.RankSpecifiers.First()?.Sizes.Where(Function(s As CSS.ExpressionSyntax) Not (TypeOf s Is CSS.OmittedArraySizeExpressionSyntax)).Select(Function(s As CSS.ExpressionSyntax) DirectCast(VBFactory.SimpleArgument(ReduceArrayUpperBoundExpression(s)), ArgumentSyntax))
                Dim cleanUpperBounds As New List(Of ArgumentSyntax)
                For Each Argument As ArgumentSyntax In upperBoundArguments
                    If Argument.ToString <> "-1" Then
                        cleanUpperBounds.Add(Argument)
                    End If
                Next
                upperBoundArguments = cleanUpperBounds
                Dim rankSpecifiers As IEnumerable(Of ArrayRankSpecifierSyntax) = node.Type.RankSpecifiers.Select(Function(rs As CSS.ArrayRankSpecifierSyntax) DirectCast(rs.Accept(Me), ArrayRankSpecifierSyntax))
                Dim AttributeLists As SyntaxList(Of AttributeListSyntax) = VBFactory.List(Of AttributeListSyntax)()
                Dim VBNode As VB.VisualBasicSyntaxNode = node.Type.ElementType.Accept(Me)
                Dim ArrayType As TypeSyntax
                If TypeOf VBNode Is TypeSyntax Then
                    ArrayType = DirectCast(VBNode, TypeSyntax)
                Else
                    Stop
                    Throw UnreachableException
                End If
                Dim ArrayBounds As ArgumentListSyntax = If(upperBoundArguments.Any(), VBFactory.ArgumentList(arguments:=VBFactory.SeparatedList(upperBoundArguments)), Nothing)
                Dim RankSpecifiersList As SyntaxList(Of ArrayRankSpecifierSyntax) = If(upperBoundArguments.Any(), VBFactory.List(rankSpecifiers.Skip(1)), VBFactory.List(rankSpecifiers))
                Dim Initializer As CollectionInitializerSyntax = If(DirectCast(node.Initializer?.Accept(Me), CollectionInitializerSyntax), VBFactory.CollectionInitializer())
                Return VBFactory.ArrayCreationExpression(
                                                    NewKeyword.WithTrailingTrivia(SpaceTrivia),
                                                    AttributeLists,
                                                    ArrayType,
                                                    ArrayBounds,
                                                    RankSpecifiersList,
                                                    Initializer
                                                            ).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitAssignmentExpression(node As CSS.AssignmentExpressionSyntax) As VB.VisualBasicSyntaxNode
                If TypeOf node.Parent Is CSS.ExpressionStatementSyntax OrElse TypeOf node.Parent Is CSS.ArrowExpressionClauseSyntax Then
                    Dim RightTypeInfo As TypeInfo = ModelExtensions.GetTypeInfo(_mSemanticModel, node.Right)
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
                            Return VBFactory.AddHandlerStatement(DirectCast(node.Left.Accept(Me).WithLeadingTrivia(SpaceTrivia), ExpressionSyntax), DirectCast(node.Right.Accept(Me), ExpressionSyntax)).WithConvertedTriviaFrom(node)
                        End If

                        If node.OperatorToken.IsKind(CS.SyntaxKind.MinusEqualsToken) Then
                            ' TODO capture and leading comments from node.left
                            Return VBFactory.RemoveHandlerStatement(DirectCast(node.Left.Accept(Me), ExpressionSyntax).WithLeadingTrivia(SpaceTrivia), DirectCast(node.Right.Accept(Me), ExpressionSyntax)).WithConvertedTriviaFrom(node)
                        End If
                    End If
                    If node.Left.IsKind(CS.SyntaxKind.DeclarationExpression, CS.SyntaxKind.TupleExpression) Then
                        Dim rightNode As ExpressionSyntax = DirectCast(node.Right.Accept(Me).WithConvertedTriviaFrom(node.Right), ExpressionSyntax)
                        Dim dimModifiersTokens As SyntaxTokenList = VBFactory.TokenList(
                                                    DimKeyword.WithConvertedLeadingTriviaFrom(node.Left.GetFirstToken())
                                                    )
                        Dim initializer As EqualsValueSyntax = VBFactory.EqualsValue(rightNode)
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
                            Dim identifierName As String = MethodBodyVisitor.GetUniqueVariableNameInScope(node, "TempVar", _mSemanticModel)
                            Dim tempIdentifier As SeparatedSyntaxList(Of ModifiedIdentifierSyntax)
                            If RightTypeInfo.ConvertedType IsNot Nothing AndAlso Not RightTypeInfo.ConvertedType.IsErrorType Then
                                If TypeOf RightTypeInfo.Type Is INamedTypeSymbol Then
                                    Dim possibleTupleType As INamedTypeSymbol = DirectCast(RightTypeInfo.ConvertedType, INamedTypeSymbol)
                                    If possibleTupleType.IsTupleType Then
                                        identifierName = MethodBodyVisitor.GetUniqueVariableNameInScope(node, "TupleTempVar", _mSemanticModel)
                                        tempIdentifier = VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(identifierName))
                                        tupleType = CType(ConvertCSTupleToVBType(possibleTupleType.TupleElements(0).ContainingType.ToString), TupleTypeSyntax)
                                        If tupleType.Elements.All(Function(t As TupleElementSyntax) As Boolean
                                                                      If Not TypeOf t Is TypedTupleElementSyntax Then
                                                                          Return False
                                                                      End If
                                                                      Dim typedTupleElement As TypedTupleElementSyntax = CType(t, TypedTupleElementSyntax)
                                                                      Return typedTupleElement.Type.RawKind <> VB.SyntaxKind.PredefinedType
                                                                  End Function) Then
                                            simpleAs = VBFactory.SimpleAsClause(AsKeyword.WithTrailingTrivia(SpaceTrivia), attributeLists:=Nothing, tupleType.WithLeadingTrivia(SpaceTrivia)).WithLeadingTrivia(SpaceTrivia)
                                        End If
                                    Else
                                        simpleAs = VBFactory.SimpleAsClause(
                                                AsKeyword.WithTrailingTrivia(SpaceTrivia),
                                                attributeLists:=Nothing,
                                                ConvertToType(possibleTupleType.ToString).WithLeadingTrivia(SpaceTrivia)
                                                ).WithLeadingTrivia(SpaceTrivia)
                                    End If
                                End If
                            End If
                            If tempIdentifier.Count = 0 Then
                                tempIdentifier = VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(identifierName))
                            End If
                            Dim variableDeclaration As SeparatedSyntaxList(Of VariableDeclaratorSyntax) = VBFactory.SingletonSeparatedList(VBFactory.VariableDeclarator(tempIdentifier, asClause:=simpleAs, initializer))
                            Dim dimStatement As LocalDeclarationStatementSyntax = VBFactory.LocalDeclarationStatement(dimModifiersTokens, variableDeclaration).WithPrependedLeadingTrivia(VBFactory.CommentTrivia($" ' TODO: Visual Basic has no equivalent to C# deconstruction declarations, an attempt was made to convert."), VBEOLTrivia)
                            StatementList = StatementList.Add(dimStatement)

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
                                            TempType = VBFactory.ParseTypeName(tupleType.Elements(variableIndex).ToString)
                                        End If
                                        AsClause = VBFactory.SimpleAsClause(TempType)
                                    End If
                                End If
                                initializer = VBFactory.EqualsValue(VBFactory.InvocationExpression(VBFactory.ParseExpression($"{identifierName}.Item{variableIndex + 1}")))
                                Dim Declarators As SeparatedSyntaxList(Of VariableDeclaratorSyntax) = VBFactory.SingletonSeparatedList(
                                                                            VBFactory.VariableDeclarator(
                                                                            VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(VariableNames(variableIndex))),
                                                                            AsClause,
                                                                            initializer)
                                                                        )
                                Dim AssignmentStatement As LocalDeclarationStatementSyntax = VBFactory.LocalDeclarationStatement(DimModifier, Declarators)
                                StatementList = StatementList.Add(AssignmentStatement)
                            Next
                        End If

                        ' Handle assignment to a Tuple of Variables that already exist
                        If node.Left.IsKind(CS.SyntaxKind.TupleExpression) Then
                            Dim LeftTupleNode As TupleExpressionSyntax = DirectCast(node.Left.Accept(Me).WithConvertedTriviaFrom(node.Left), TupleExpressionSyntax)

                            VariableNames = New List(Of String)
                            Dim IdentifierName As String = MethodBodyVisitor.GetUniqueVariableNameInScope(node, "TupleTempVar", _mSemanticModel)
                            For Each Argument As ArgumentSyntax In LeftTupleNode.Arguments
                                VariableNames.Add(Argument.ToString)
                            Next
                            Dim TupleList As New List(Of String)
                            If RightTypeInfo.Type Is Nothing OrElse RightTypeInfo.Type.IsErrorType Then
                                For Each a As CSS.ArgumentSyntax In DirectCast(node.Left, CSS.TupleExpressionSyntax).Arguments
                                    If TypeOf a.Expression Is CSS.DeclarationExpressionSyntax Then
                                        Dim t As CSS.DeclarationExpressionSyntax = DirectCast(a.Expression, CSS.DeclarationExpressionSyntax)
                                        TupleList.Add(ConvertToType(t.[Type].ToString).ToString)
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
                                    If Type.IsTupleType Then
                                        Dim namedTypes As String = Type.TupleElements(0).ContainingType.ToString
                                        Dim TupleTypeName As TupleTypeSyntax = CType(VBFactory.ParseTypeName(ConvertTupleToVBTypeStrings(namedTypes, True)(0)), TupleTypeSyntax)
                                        For Each typeName As TupleElementSyntax In TupleTypeName.Elements
                                            ' Need to convert Types !!!!!!!
                                            TupleList.Add(typeName.ToString)
                                        Next
                                    Else
                                        TupleList.Add(ConvertToType(Type.ToString).ToString)
                                    End If
                                ElseIf TypeOf RightTypeInfo.Type Is ITypeSymbol Then
                                    Try
                                        For Each a As CSS.ArgumentSyntax In DirectCast(node.Left, CSS.TupleExpressionSyntax).Arguments
                                            If TypeOf a.Expression Is CSS.DeclarationExpressionSyntax Then
                                                Dim t As CSS.DeclarationExpressionSyntax = DirectCast(a.Expression, CSS.DeclarationExpressionSyntax)
                                                TupleList.Add(ConvertToType(t.[Type].ToString).ToString)
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
                                    End Try
                                Else
                                    Stop
                                End If
                            End If
                            Dim builder As New StringBuilder()
                            builder.Append("(")
                            For Each e As IndexClass(Of String) In TupleList.WithIndex
                                builder.Append(e.Value & ", ")
                            Next
                            builder.Append(TupleList.Last & ")")
                            Dim TupleType As String = builder.ToString

                            Dim TupleType2 As TypeSyntax = VBFactory.ParseTypeName(TupleType).WithLeadingTrivia(SpaceTrivia)
                            Dim SimpleAs As SimpleAsClauseSyntax = VBFactory.SimpleAsClause(AsKeyword.WithTrailingTrivia(SpaceTrivia), attributeLists:=Nothing, TupleType2).WithLeadingTrivia(SpaceTrivia)
                            Dim Names As SeparatedSyntaxList(Of ModifiedIdentifierSyntax) = VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(IdentifierName))
                            Dim VariableDeclaration As SeparatedSyntaxList(Of VariableDeclaratorSyntax) = VBFactory.SingletonSeparatedList(VBFactory.VariableDeclarator(Names, SimpleAs, initializer))
                            Dim DimStatement As LocalDeclarationStatementSyntax = VBFactory.LocalDeclarationStatement(dimModifiersTokens, VariableDeclaration)
                            StatementList = StatementList.Add(DimStatement)
                            For Each e As IndexClass(Of String) In VariableNames.WithIndex
                                If e.Value = "underscore" Then
                                    Continue For
                                End If
                                Dim NewLeftNode As ExpressionSyntax = VBFactory.IdentifierName(e.Value)
                                Dim NewRightNode As ExpressionSyntax = VBFactory.InvocationExpression(VBFactory.ParseExpression($"{IdentifierName}.Item{e.Index + 1}"))
                                Dim kind As VB.SyntaxKind = ConvertCSExpressionsKindToVBKind(CS.CSharpExtensions.Kind(node))
                                Dim OperatorToken As SyntaxToken = ExpressionKindToOperatorToken(kind)
                                Dim AssignmentStatement As AssignmentStatementSyntax = VBFactory.AssignmentStatement(kind,
                                                                                                        NewLeftNode,
                                                                                                        OperatorToken,
                                                                                                        NewRightNode)
                                StatementList = StatementList.Add(AssignmentStatement)
                            Next
                        End If
                        Dim TryStatement As TryStatementSyntax = VBFactory.TryStatement.
                            WithLeadingTrivia(VBEOLTrivia).
                            WithPrependedLeadingTrivia(VBFactory.CommentTrivia("' TODO: This Try Block can be removed"))
                        Dim Throwstatement As SyntaxList(Of StatementSyntax) = VBFactory.SingletonList(Of StatementSyntax)(VBFactory.ThrowStatement)
                        Dim CatchBlock As SyntaxList(Of CatchBlockSyntax) = VBFactory.SingletonList(VBFactory.CatchBlock(VBFactory.CatchStatement,
                                                                                                                             Throwstatement)
                                                                                                        )
                        Return VBFactory.TryBlock(TryStatement,
                                                 StatementList,
                                                 CatchBlock,
                                                 finallyBlock:=Nothing,
                                                 VBFactory.EndBlockStatement(VB.SyntaxKind.EndTryStatement, TryKeyword)
                                                 )
                    End If

                    Return MakeAssignmentStatement(node)
                End If

                If TypeOf node.Parent Is CSS.ForStatementSyntax OrElse TypeOf node.Parent Is CSS.ParenthesizedLambdaExpressionSyntax Then
                    Return MakeAssignmentStatement(node).WithConvertedTriviaFrom(node)
                End If

                If TypeOf node.Parent Is CSS.InitializerExpressionSyntax Then
                    If TypeOf node.Left Is CSS.ImplicitElementAccessSyntax Then
                        Return VBFactory.CollectionInitializer(VBFactory.SeparatedList({DirectCast(node.Left.Accept(Me), ExpressionSyntax), DirectCast(node.Right.Accept(Me), ExpressionSyntax)})).WithConvertedTriviaFrom(node)
                    End If
                    If node.Parent.IsKind(CS.SyntaxKind.ObjectInitializerExpression) Then
                        Dim nodeRight As VB.VisualBasicSyntaxNode = node.Right.Accept(Me)
                        If TypeOf nodeRight Is ObjectMemberInitializerSyntax Then
                            Dim objectMemberInitializer As ObjectMemberInitializerSyntax = DirectCast(nodeRight, ObjectMemberInitializerSyntax)
                            Dim attributeLists As SyntaxList(Of AttributeListSyntax) = Nothing
                            Dim argumentList As ArgumentListSyntax = Nothing
                            Dim toDoObjectNameGoesHere As TypeSyntax = VBFactory.ParseTypeName("TODO_ObjectNameGoesHere")
                            Dim objectCreationExpression As ObjectCreationExpressionSyntax = VBFactory.ObjectCreationExpression(NewKeyword, attributeLists, toDoObjectNameGoesHere, argumentList, objectMemberInitializer)
                            If node.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                Stop
                            End If
                            Return VBFactory.NamedFieldInitializer(
                                DirectCast(node.Left.Accept(Me), IdentifierNameSyntax).WithoutLeadingTrivia,
                                objectCreationExpression).WithConvertedTrailingTriviaFrom(node)
                        ElseIf TypeOf nodeRight Is CollectionInitializerSyntax Then
                            Dim collection As CollectionInitializerSyntax = CType(nodeRight, CollectionInitializerSyntax)
                            Dim simpleAssignement As CSS.AssignmentExpressionSyntax = node
                            Dim tInfo As TypeInfo = ModelExtensions.GetTypeInfo(_mSemanticModel, simpleAssignement.Left)
                            Dim initializer As ObjectCollectionInitializerSyntax = VBFactory.ObjectCollectionInitializer(FromKeyword, collection)
                            Dim type1 As TypeSyntax
                            If tInfo.Type Is Nothing OrElse tInfo.Type.IsErrorType Then
                                type1 = ConvertToType("UnknownTypeTryConvertProject")
                            Else
                                type1 = ConvertToType(tInfo.Type.ToString, False)
                            End If
                            Dim objectCreationExpression As ObjectCreationExpressionSyntax = VBFactory.ObjectCreationExpression(NewKeyword,
                                                                      attributeLists:=Nothing,
                                                                      type1,
                                                                      argumentList:=Nothing,
                                                                      initializer
                                                                      )
                            Return VBFactory.NamedFieldInitializer(DirectCast(node.Left.Accept(Me), IdentifierNameSyntax).WithoutLeadingTrivia,
                                                                   objectCreationExpression).WithConvertedTrailingTriviaFrom(node)
                        End If
                        Dim vbNode As VB.VisualBasicSyntaxNode = node.Left.Accept(Me)
                        If TypeOf vbNode Is IdentifierNameSyntax Then
                            If node.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                GetStatementwithIssues(node).AddMarker(VBFactory.EmptyStatement.WithLeadingTrivia(ConvertTrivia(node.GetLeadingTrivia)), StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                            End If
                            Return VBFactory.NamedFieldInitializer(DirectCast(vbNode.WithoutLeadingTrivia, IdentifierNameSyntax), DirectCast(nodeRight, ExpressionSyntax)).WithConvertedTrailingTriviaFrom(node)
                        End If
                        If TypeOf vbNode Is MemberAccessExpressionSyntax Then
                            Dim kind As VB.SyntaxKind = ConvertCSExpressionsKindToVBKind(CS.CSharpExtensions.Kind(node))
                            Dim OperatorToken As SyntaxToken = ExpressionKindToOperatorToken(kind)
                            Return VBFactory.AssignmentStatement(kind, DirectCast(vbNode, MemberAccessExpressionSyntax), OperatorToken, DirectCast(nodeRight, ExpressionSyntax)).WithConvertedTriviaFrom(node)
                        Else
                            Stop
                        End If
                    End If
                End If

                Dim leftExpression As ExpressionSyntax = DirectCast(node.Left.Accept(Me), ExpressionSyntax)
                Dim rightExpression As ExpressionSyntax = DirectCast(node.Right.Accept(Me), ExpressionSyntax)
                If TypeOf node.Parent Is CSS.ArrowExpressionClauseSyntax Then
                    Return VBFactory.SimpleAssignmentStatement(leftExpression, rightExpression)
                End If
                MarkPatchInlineAssignHelper(node)
                Return VBFactory.InvocationExpression(expression:=VBFactory.IdentifierName("__InlineAssignHelper"), argumentList:=VBFactory.ArgumentList(VBFactory.SeparatedList((New ArgumentSyntax() {VBFactory.SimpleArgument(leftExpression), VBFactory.SimpleArgument(rightExpression)})))).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitAwaitExpression(node As CSS.AwaitExpressionSyntax) As VB.VisualBasicSyntaxNode
                Return VBFactory.AwaitExpression(AwaitKeyword.WithTrailingTrivia(SpaceTrivia), expression:=DirectCast(node.Expression.Accept(Me), ExpressionSyntax)).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitBaseExpression(node As CSS.BaseExpressionSyntax) As VB.VisualBasicSyntaxNode
                Return VBFactory.MyBaseExpression()
            End Function

            Public Overrides Function VisitBinaryExpression(node As CSS.BinaryExpressionSyntax) As VB.VisualBasicSyntaxNode
                Try
                    Dim foundEOL As Boolean = False
                    Dim kind As VB.SyntaxKind
                    Dim leftVBNode As VB.VisualBasicSyntaxNode
                    Dim rightVBNode As VB.VisualBasicSyntaxNode
                    Dim vbOperatorToken As SyntaxToken
                    If IsConcatenateStringsExpression(node) Then
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
                        leftVBNode = ConvertAndModifyNodeTrivia(vbNode, csNodesOrTokens, 0)
                        For nodeOrTokenIndex As Integer = 1 To csNodesOrTokens.Count - 1 Step 2
                            vbOperatorToken = ConvertAndModifyTokenTrivia(AmpersandToken, csNodesOrTokens, nodeOrTokenIndex)
                            vbNode = CType(csNodesOrTokens(nodeOrTokenIndex + 1), CSS.ExpressionSyntax).Accept(Me)
                            rightVBNode = ConvertAndModifyNodeTrivia(vbNode, csNodesOrTokens, nodeOrTokenIndex + 1)
                            leftVBNode = VBFactory.ConcatenateExpression(
                                                            DirectCast(leftVBNode, ExpressionSyntax),
                                                            vbOperatorToken,
                                                            DirectCast(rightVBNode, ExpressionSyntax)
                                                            )
                        Next
                        Return leftVBNode
                    End If
                    kind = ConvertCSExpressionsKindToVBKind(CS.CSharpExtensions.Kind(node))
                    leftVBNode = node.Left.Accept(Me).WithConvertedTriviaFrom(node.Left)
                    Dim leftExpression As ExpressionSyntax = DirectCast(leftVBNode, ExpressionSyntax)
                    rightVBNode = node.Right.Accept(Me).WithConvertedTriviaFrom(node.Right)
                    Dim commaTokenWithTrivia As SyntaxToken = CommaToken
                    Select Case node.Kind
                        Case CS.SyntaxKind.CoalesceExpression
                            Dim IfKeywordWithTrivia As SyntaxToken = IfKeyword
                            If TypeOf rightVBNode Is ExpressionSyntax Then
                                Dim secondExpression As ExpressionSyntax = DirectCast(rightVBNode, ExpressionSyntax)
                                Dim separatorTrailingTrivia As New List(Of SyntaxTrivia)
                                If leftExpression.ContainsEOLTrivia OrElse leftExpression.ContainsCommentOrDirectiveTrivia Then
                                    Dim ifLeadingTrivia As New List(Of SyntaxTrivia)
                                    If leftExpression.HasLeadingTrivia Then
                                        IfKeywordWithTrivia = IfKeywordWithTrivia.WithLeadingTrivia(leftExpression.GetLeadingTrivia)
                                    End If
                                    If leftExpression.HasTrailingTrivia Then
                                        For Each t As SyntaxTrivia In leftExpression.GetTrailingTrivia
                                            Select Case t.RawKind
                                                Case VB.SyntaxKind.CommentTrivia
                                                    separatorTrailingTrivia.Add(SpaceTrivia)
                                                    separatorTrailingTrivia.Add(t)
                                                    separatorTrailingTrivia.Add(SpaceTrivia)
                                                    foundEOL = True
                                                Case VB.SyntaxKind.EndOfLineTrivia
                                                    foundEOL = True
                                                Case VB.SyntaxKind.WhitespaceTrivia
                                                    ' ignore
                                                Case Else
                                                    Stop
                                            End Select
                                        Next
                                        If foundEOL Then
                                            separatorTrailingTrivia.Add(VBEOLTrivia)
                                            foundEOL = False
                                        End If
                                        leftExpression = leftExpression.With({SpaceTrivia}, {SpaceTrivia})
                                    End If
                                    commaTokenWithTrivia = commaTokenWithTrivia.WithTrailingTrivia(separatorTrailingTrivia)
                                    If node.OperatorToken.HasLeadingTrivia AndAlso node.OperatorToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                        Dim StatementTrivia As List(Of SyntaxTrivia) = ConvertTrivia(node.OperatorToken.LeadingTrivia).ToList
                                        GetStatementwithIssues(node).AddMarker(
                                                        VBFactory.EmptyStatement.WithLeadingTrivia(StatementTrivia),
                                                        StatementHandlingOption.AppendEmptyStatement,
                                                        AllowDuplicates:=True)
                                        secondExpression = secondExpression.WithLeadingTrivia(StatementTrivia.Last)
                                    End If
                                End If
                                Dim binaryConditionalExpressionSyntax1 As BinaryConditionalExpressionSyntax = VBFactory.BinaryConditionalExpression(
                                                            IfKeywordWithTrivia,
                                                            OpenParenToken,
                                                            leftExpression,
                                                            commaTokenWithTrivia,
                                                            secondExpression,
                                                            CloseParenToken)
                                Return binaryConditionalExpressionSyntax1
                            End If
                            If TypeOf rightVBNode Is ThrowStatementSyntax Then
                                Dim condition As ExpressionSyntax = VBFactory.IsExpression(leftExpression.WithTrailingTrivia(SpaceTrivia), NothingExpression)
                                Dim ifStatement As IfStatementSyntax = VBFactory.IfStatement(IfKeywordWithTrivia, condition, ThenKeyword)

                                Dim statements As SyntaxList(Of StatementSyntax) = VBFactory.SingletonList(DirectCast(rightVBNode, StatementSyntax))
                                Dim ifBlock As StatementSyntax = VBFactory.SingleLineIfStatement(IfKeywordWithTrivia,
                                                                                               condition,
                                                                                               ThenKeyword,
                                                                                               statements,
                                                                                               elseClause:=Nothing).WithTrailingEOL
                                GetStatementwithIssues(node).AddMarker(ifBlock, StatementHandlingOption.PrependStatement, AllowDuplicates:=False)
                                Return leftVBNode
                            Else
                                Stop
                            End If
                        Case CS.SyntaxKind.AsExpression
                            Dim firstExpression As ExpressionSyntax = DirectCast(leftVBNode, ExpressionSyntax)
                            If firstExpression.ContainsEOLTrivia Then
                                firstExpression = firstExpression.WithRestructuredingEOLTrivia
                                commaTokenWithTrivia = commaTokenWithTrivia.WithTrailingTrivia(VBEOLTrivia)
                            End If
                            Return VBFactory.TryCastExpression(
                                                TryCastKeyword,
                                                OpenParenToken,
                                                firstExpression,
                                                commaTokenWithTrivia,
                                                DirectCast(rightVBNode, TypeSyntax),
                                                CloseParenToken)
                        Case CS.SyntaxKind.IsExpression
                            Dim NewLeadingTrivia As New List(Of SyntaxTrivia)
                            If leftVBNode.HasLeadingTrivia AndAlso leftVBNode.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                NewLeadingTrivia.AddRange(leftVBNode.GetLeadingTrivia)
                                leftVBNode = leftVBNode.WithoutTrivia.WithTrailingTrivia(SpaceTrivia)
                            End If
                            If rightVBNode.HasLeadingTrivia AndAlso rightVBNode.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                NewLeadingTrivia.AddRange(rightVBNode.GetLeadingTrivia)
                                rightVBNode = rightVBNode.WithoutTrivia.WithTrailingTrivia(SpaceTrivia)
                            End If
                            Return VBFactory.TypeOfIsExpression(DirectCast(leftVBNode, ExpressionSyntax), DirectCast(rightVBNode, TypeSyntax)).WithLeadingTrivia(NewLeadingTrivia)
                        Case CS.SyntaxKind.EqualsExpression, CS.SyntaxKind.NotEqualsExpression
                            Dim otherArgument As ExpressionSyntax = Nothing
                            If node.Left.IsKind(CS.SyntaxKind.NullLiteralExpression) Then
                                otherArgument = DirectCast(rightVBNode, ExpressionSyntax).With({SpaceTrivia}, {SpaceTrivia})
                            ElseIf node.Left.IsKind(CS.SyntaxKind.SizeOfExpression) AndAlso DirectCast(node.Left, CSS.SizeOfExpressionSyntax).Type.IsKind(CS.SyntaxKind.PointerType) Then
                                leftExpression = IntPrtSizeExpression
                            End If

                            If node.Right.IsKind(CS.SyntaxKind.NullLiteralExpression) Then
                                otherArgument = DirectCast(leftVBNode, ExpressionSyntax).With({SpaceTrivia}, {SpaceTrivia})
                            ElseIf node.Right.IsKind(CS.SyntaxKind.SizeOfExpression) AndAlso DirectCast(node.Right, CSS.SizeOfExpressionSyntax).Type.IsKind(CS.SyntaxKind.PointerType) Then
                                rightVBNode = IntPrtSizeExpression
                            End If

                            If otherArgument IsNot Nothing Then
                                If node.IsKind(CS.SyntaxKind.EqualsExpression) Then
                                    Return VBFactory.IsExpression(otherArgument, NothingExpression.WithConvertedTriviaFrom(node.Right))
                                End If
                                Return VBFactory.IsNotExpression(otherArgument, NothingExpression.WithConvertedTriviaFrom(node.Right))
                            End If
                            ' Remainder handled after case
                        Case CS.SyntaxKind.AddExpression
                            ' Handled after case
                    End Select

                    ' Handle all other expressions here
                    Dim leftTrailingTrivia As SyntaxTriviaList = leftExpression.GetTrailingTrivia
                    Dim rightExpression As ExpressionSyntax = DirectCast(rightVBNode, ExpressionSyntax)
                    If leftTrailingTrivia.ToList.Count = 1 AndAlso leftTrailingTrivia(0).ToString.Trim = "?" Then
                        Dim originalIdentifier As IdentifierNameSyntax = rightExpression.DescendantNodes.
                                                                           OfType(Of IdentifierNameSyntax).
                                                                           First(Function(b As IdentifierNameSyntax) b.Kind() = VB.SyntaxKind.IdentifierName)
                        Dim newIdentifierWithQuestionMark As IdentifierNameSyntax =
                                    VBFactory.IdentifierName($"{DirectCast(node.Left.Accept(Me), ExpressionSyntax)}?")
                        Return rightExpression.ReplaceNode(originalIdentifier, newIdentifierWithQuestionMark)
                    End If

                    Dim movedTrailingTrivia As New List(Of SyntaxTrivia)
                    If leftExpression.HasLeadingTrivia AndAlso leftExpression.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        movedTrailingTrivia.AddRange(leftExpression.GetLeadingTrivia)
                        leftExpression = leftExpression.WithLeadingTrivia(SpaceTrivia)
                    End If
                    If leftExpression.HasTrailingTrivia Then
                        For Each t As SyntaxTrivia In leftExpression.GetTrailingTrivia
                            Select Case t.RawKind
                                Case VB.SyntaxKind.CommentTrivia
                                    movedTrailingTrivia.Add(SpaceTrivia)
                                    movedTrailingTrivia.Add(t)
                                    movedTrailingTrivia.Add(SpaceTrivia)
                                Case VB.SyntaxKind.EndOfLineTrivia
                                    foundEOL = True
                                Case VB.SyntaxKind.WhitespaceTrivia
                                Case Else
                                    Stop
                            End Select
                        Next
                        leftExpression = leftExpression.WithTrailingTrivia(SpaceTrivia)
                    End If
                    Dim operatorToken As SyntaxToken = ExpressionKindToOperatorToken(kind).WithConvertedTriviaFrom(node.OperatorToken)
                    If operatorToken.HasLeadingTrivia Then
                        If operatorToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                            movedTrailingTrivia.AddRange(operatorToken.LeadingTrivia)
                            operatorToken = operatorToken.WithLeadingTrivia(SpaceTrivia)
                        ElseIf operatorToken.LeadingTrivia.ContainsEOLTrivia Then
                            operatorToken = operatorToken.WithLeadingTrivia(SpaceTrivia)
                        End If
                    End If

                    If operatorToken.HasTrailingTrivia Then
                        Dim newOperatorTrailingTrivia As New List(Of SyntaxTrivia)
                        For Each t As SyntaxTrivia In operatorToken.TrailingTrivia
                            Select Case t.RawKind
                                Case VB.SyntaxKind.CommentTrivia
                                    If foundEOL Then
                                        newOperatorTrailingTrivia.AddRange(movedTrailingTrivia)
                                        newOperatorTrailingTrivia.Add(SpaceTrivia)
                                        newOperatorTrailingTrivia.Add(t)
                                        newOperatorTrailingTrivia.Add(SpaceTrivia)
                                        movedTrailingTrivia.Clear()
                                    Else
                                        newOperatorTrailingTrivia.Add(t)
                                    End If
                                Case VB.SyntaxKind.EndOfLineTrivia
                                    foundEOL = True
                                Case VB.SyntaxKind.WhitespaceTrivia
                                    If foundEOL Then
                                        movedTrailingTrivia.Add(t)
                                    Else
                                        newOperatorTrailingTrivia.Add(t)
                                    End If
                                Case Else
                                    Stop
                            End Select
                        Next
                        If foundEOL Then
                            newOperatorTrailingTrivia.Add(VBEOLTrivia)
                        End If
                        operatorToken = operatorToken.WithTrailingTrivia(newOperatorTrailingTrivia)
                    End If

                    Dim rightNode As ExpressionSyntax = DirectCast(node.Right.Accept(Me), ExpressionSyntax)

                    If node.Right.HasTrailingTrivia Then
                        movedTrailingTrivia.Clear()
                        foundEOL = False
                        For Each t As SyntaxTrivia In rightNode.GetTrailingTrivia
                            Select Case t.RawKind
                                Case VB.SyntaxKind.CommentTrivia
                                    movedTrailingTrivia.Add(SpaceTrivia)
                                    movedTrailingTrivia.Add(t)
                                    movedTrailingTrivia.Add(SpaceTrivia)

                                Case VB.SyntaxKind.EndOfLineTrivia
                                    foundEOL = True
                                Case VB.SyntaxKind.WhitespaceTrivia
                                    movedTrailingTrivia.Add(t)
                                Case Else
                                    Stop
                            End Select
                        Next
                        If foundEOL Then
                            movedTrailingTrivia.Add(VBEOLTrivia)
                        End If
                    End If
                    Dim rightLeadingTrivia As New List(Of SyntaxTrivia)
                    If operatorToken.TrailingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                        For Each e As IndexClass(Of SyntaxTrivia) In rightNode.GetLeadingTrivia.WithIndex
                            Dim t As SyntaxTrivia = e.Value
                            Dim nextTrivia As SyntaxTrivia = If(Not e.IsLast, rightNode.GetLeadingTrivia(e.Index + 1), New SyntaxTrivia)
                            Select Case t.RawKind
                                Case VB.SyntaxKind.WhitespaceTrivia
                                    If nextTrivia.IsKind(VB.SyntaxKind.CommentTrivia) Then
                                        rightLeadingTrivia.Add(SpaceTrivia)
                                        rightLeadingTrivia.Add(LineContinuation)
                                    End If
                            End Select
                            rightLeadingTrivia.Add(t)
                        Next
                    Else
                        rightLeadingTrivia.Add(SpaceTrivia)
                    End If
                    rightExpression = rightNode.With(rightLeadingTrivia, movedTrailingTrivia)
                    Dim binaryExpressionSyntax3 As BinaryExpressionSyntax = VBFactory.BinaryExpression(
                                                    kind,
                                                    leftExpression,
                                                    operatorToken,
                                                    rightExpression
                                                    )
                    Return binaryExpressionSyntax3
                Catch ex As InsufficientExecutionStackException
                    _reportException?.Invoke(ex)
                    Return Nothing
                Catch ex As OperationCanceledException
                    Throw
                Catch ex As Exception
                    Stop
                    Throw
                End Try
                Throw UnreachableException
            End Function

            Public Overrides Function VisitCastExpression(node As CSS.CastExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim CTypeExpressionSyntax As VB.VisualBasicSyntaxNode
                Dim newTrailingTrivia As New List(Of SyntaxTrivia)
                Try
                    Dim type As ITypeSymbol = ModelExtensions.GetTypeInfo(_mSemanticModel, node.Type).Type
                    Dim Expression As ExpressionSyntax = DirectCast(node.Expression.Accept(Me), ExpressionSyntax)
                    newTrailingTrivia.AddRange(Expression.GetTrailingTrivia)
                    newTrailingTrivia.AddRange(ConvertTrivia(node.GetTrailingTrivia))
                    Expression = Expression.WithoutTrivia

                    Dim ExpressionTypeStr As String = ModelExtensions.GetTypeInfo(_mSemanticModel, node.Expression).Type?.ToString
                    Select Case type.SpecialType
                        Case SpecialType.System_Object
                            CTypeExpressionSyntax = VBFactory.PredefinedCastExpression(CObjKeyword, Expression)
                        Case SpecialType.System_Boolean
                            CTypeExpressionSyntax = VBFactory.PredefinedCastExpression(CBoolKeyword, Expression)
                        Case SpecialType.System_Char
                            Dim TestAgainst As String() = {"int", "UShort"}
                            If node.Parent.IsKind(CS.SyntaxKind.AttributeArgument) Then
                                CTypeExpressionSyntax = Expression
                            ElseIf TestAgainst.Contains(ExpressionTypeStr, StringComparer.OrdinalIgnoreCase) Then
                                CTypeExpressionSyntax = VBFactory.ParseExpression($"ChrW({Expression})")
                            Else
                                CTypeExpressionSyntax = VBFactory.PredefinedCastExpression(CCharKeyword, Expression)
                            End If
                        Case SpecialType.System_SByte
                            CTypeExpressionSyntax = VBFactory.PredefinedCastExpression(CSByteKeyword, Expression)
                        Case SpecialType.System_Byte
                            If Expression.IsKind(VB.SyntaxKind.CharacterLiteralExpression) Then
                                CTypeExpressionSyntax = VBFactory.ParseExpression($"AscW({Expression})")
                            Else
                                CTypeExpressionSyntax = VBFactory.PredefinedCastExpression(CByteKeyword, Expression)
                            End If
                        Case SpecialType.System_Int16
                            Dim FixExpression As ExpressionSyntax = VBFactory.IdentifierName("Fix")
                            Dim ArgumentList As ArgumentListSyntax = VBFactory.ArgumentList(VBFactory.SingletonSeparatedList(Of ArgumentSyntax)(VBFactory.SimpleArgument(Expression)))
                            CTypeExpressionSyntax = VBFactory.PredefinedCastExpression(CShortKeyword, VBFactory.InvocationExpression(FixExpression, ArgumentList))
                        Case SpecialType.System_UInt16
                            If ExpressionTypeStr = "char" Then
                                CTypeExpressionSyntax = VBFactory.ParseExpression(text:=$"ChrW({Expression})")
                            Else
                                CTypeExpressionSyntax = VBFactory.PredefinedCastExpression(CUShortKeyword, Expression)
                            End If
                        Case SpecialType.System_Int32
                            If ExpressionTypeStr = "char" Then
                                CTypeExpressionSyntax = VBFactory.ParseExpression($"ChrW({Expression})").WithTrailingTrivia(newTrailingTrivia)
                            Else
                                Dim FixExpression As ExpressionSyntax = VBFactory.IdentifierName("Fix")
                                Dim ArgumentList As ArgumentListSyntax = VBFactory.ArgumentList(VBFactory.SingletonSeparatedList(Of ArgumentSyntax)(VBFactory.SimpleArgument(Expression)))
                                CTypeExpressionSyntax = VBFactory.PredefinedCastExpression(CIntKeyword, VBFactory.InvocationExpression(FixExpression, ArgumentList))
                            End If
                        Case SpecialType.System_UInt32
                            CTypeExpressionSyntax = VBFactory.PredefinedCastExpression(CUIntKeyword, Expression)
                        Case SpecialType.System_Int64
                            Dim FixExpression As ExpressionSyntax = VBFactory.IdentifierName("Fix")
                            Dim ArgumentList As ArgumentListSyntax = VBFactory.ArgumentList(VBFactory.SingletonSeparatedList(Of ArgumentSyntax)(VBFactory.SimpleArgument(Expression)))
                            CTypeExpressionSyntax = VBFactory.PredefinedCastExpression(CLngKeyword, VBFactory.InvocationExpression(FixExpression, ArgumentList))
                        Case SpecialType.System_UInt64
                            CTypeExpressionSyntax = VBFactory.PredefinedCastExpression(CULngKeyword, Expression)
                        Case SpecialType.System_Decimal
                            CTypeExpressionSyntax = VBFactory.PredefinedCastExpression(CDecKeyword, Expression)
                        Case SpecialType.System_Single
                            CTypeExpressionSyntax = VBFactory.PredefinedCastExpression(CSngKeyword, Expression)
                        Case SpecialType.System_Double
                            CTypeExpressionSyntax = VBFactory.PredefinedCastExpression(CDblKeyword, Expression)
                        Case SpecialType.System_String
                            CTypeExpressionSyntax = VBFactory.PredefinedCastExpression(CStrKeyword, Expression)
                        Case SpecialType.System_DateTime
                            CTypeExpressionSyntax = VBFactory.PredefinedCastExpression(CDateKeyword, Expression)
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
                                    Return VBFactory.CTypeExpression(Expression, VBFactory.ParseTypeName("IntPtr"))
                                Else
                                    CTypeExpressionSyntax = VBFactory.CTypeExpression(Expression, VBFactory.ParseTypeName(AddressOf1.Operand.
                                                                                                                                ToString.
                                                                                                                                Replace("&", "", StringComparison.OrdinalIgnoreCase)))
                                End If
                            Else
                                CTypeExpressionSyntax = VBFactory.CTypeExpression(Expression, DirectCast(TypeOrAddressOf, TypeSyntax))
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
                        StatementWithIssue.AddMarker(VBFactory.EmptyStatement.WithLeadingTrivia(LeadingTrivia), StatementHandlingOption.PrependStatement, AllowDuplicates:=False)
                        Return VBFactory.ParseExpression($"{CastExpression.Keyword}(Val(""&H"" & Hex({CastExpression.Expression})))")
                    Else
                        Return VBFactory.ParseExpression($"{CastExpression.Keyword}({CastExpression.Expression})")
                    End If
                End If
                If TypeOf Expression Is BinaryConditionalExpressionSyntax OrElse
                    TypeOf Expression Is BinaryExpressionSyntax OrElse
                    TypeOf Expression Is InvocationExpressionSyntax OrElse
                    TypeOf Expression Is LiteralExpressionSyntax OrElse
                    TypeOf Expression Is ObjectCreationExpressionSyntax Then
                    If Unchecked Then
                        StatementWithIssue.AddMarker(VBFactory.EmptyStatement.WithLeadingTrivia(LeadingTrivia), StatementHandlingOption.PrependStatement, AllowDuplicates:=False)
                        Return VBFactory.ParseExpression($"Unchecked({Expression})")
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
                Dim TrailingTriviaList As New List(Of SyntaxTrivia)
                If expression.ContainsEOLTrivia Then
                    TrailingTriviaList.AddRange(expression.WithRestructuredingEOLTrivia.GetTrailingTrivia)
                    expression = expression.WithoutTrailingTrivia
                End If
                If node.OperatorToken.ContainsDirectives Then
                    Dim StatementWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                    StatementWithIssues.AddMarker(VBFactory.EmptyStatement().WithLeadingTrivia(ConvertStatementIntoComment(node)).WithTrailingEOL, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                End If
                Return VBFactory.ConditionalAccessExpression(expression, QuestionToken.WithTrailingTrivia(TrailingTriviaList), DirectCast(node.WhenNotNull.Accept(Me), ExpressionSyntax)).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitConditionalExpression(node As CSS.ConditionalExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim NodesOrTokens As New List(Of SyntaxNodeOrToken) From {
                    node.Condition,
                    node.ColonToken,
                    node.WhenTrue,
                    node.QuestionToken,
                    node.WhenFalse
                }
                Dim NewItemLeadingTrivia As New List(Of SyntaxTrivia)
                Dim NewItemTrailingTrivia As New List(Of SyntaxTrivia)
                Dim NewSeparatorTrailingTrivia As New List(Of SyntaxTrivia)

                Dim Condition As ExpressionSyntax = DirectCast(ConvertAndModifyNodeTrivia(node.Condition.Accept(Me), NodesOrTokens, 0), ExpressionSyntax)

                Dim csWhenTrue As CSS.ExpressionSyntax = node.WhenTrue
                Dim WhenTrue As ExpressionSyntax = Nothing
                If Not csWhenTrue.IsKind(CS.SyntaxKind.ThrowExpression) Then
                    WhenTrue = DirectCast(ConvertAndModifyNodeTrivia(node.WhenTrue.Accept(Me), NodesOrTokens, Index:=2), ExpressionSyntax)
                End If

                Dim FirstCommaToken As SyntaxToken = ConvertAndModifyTokenTrivia(CommaToken, NodesOrTokens, Index:=1)

                Dim csWhenFalse As CSS.ExpressionSyntax = node.WhenFalse
                Dim WhenFalse As ExpressionSyntax = Nothing
                If Not csWhenFalse.IsKind(CS.SyntaxKind.ThrowExpression) Then
                    WhenFalse = DirectCast(ConvertAndModifyNodeTrivia(node.WhenFalse.Accept(Me), NodesOrTokens, Index:=4), ExpressionSyntax)
                End If

                Dim IfKeywordWithTrivia As SyntaxToken = IfKeyword.WithConvertedLeadingTriviaFrom(node.Condition.GetFirstToken)
                Dim SecondCommaToken As SyntaxToken = ConvertAndModifyTokenTrivia(CommaToken, NodesOrTokens, Index:=3)

                If Not csWhenFalse.IsKind(CS.SyntaxKind.ThrowExpression) AndAlso Not csWhenTrue.IsKind(CS.SyntaxKind.ThrowExpression) Then
                    Return VBFactory.TernaryConditionalExpression(
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
                    Condition = VBFactory.NotExpression(Condition.WithoutTrivia)
                    ThrowStatement = DirectCast(csWhenFalse.Accept(Me).WithConvertedTriviaFrom(csWhenFalse), ThrowStatementSyntax).WithTrailingEOL
                    ResultExpression = DirectCast(csWhenTrue.Accept(Me).WithConvertedTriviaFrom(csWhenTrue), ExpressionSyntax)
                End If
                Dim Statements As SyntaxList(Of StatementSyntax) = VBFactory.SingletonList(Of StatementSyntax)(ThrowStatement)

                Dim IfBlock As SingleLineIfStatementSyntax = VBFactory.SingleLineIfStatement(Condition.WithTrailingTrivia({SpaceTrivia}),
                                                                                                 Statements,
                                                                                                 elseClause:=Nothing
                                                                                                 )
                Dim StatementWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                StatementWithIssues.AddMarker(IfBlock, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                Return ResultExpression
            End Function

            Public Overrides Function VisitDeclarationExpression(Node As CSS.DeclarationExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim Value As IdentifierNameSyntax
                If Node.Designation.IsKind(CS.SyntaxKind.SingleVariableDesignation) Then
                    Dim SingleVariableDesignation As CSS.SingleVariableDesignationSyntax = DirectCast(Node.Designation, CSS.SingleVariableDesignationSyntax)
                    Value = DirectCast(SingleVariableDesignation.Accept(Me), IdentifierNameSyntax).WithConvertedTriviaFrom(Node)
                    Return Value
                End If
                If Node.Designation.IsKind(CS.SyntaxKind.ParenthesizedVariableDesignation) Then
                    Dim ParenthesizedVariableDesignation As CSS.ParenthesizedVariableDesignationSyntax = DirectCast(Node.Designation, CSS.ParenthesizedVariableDesignationSyntax)
                    Dim VariableDeclaration As VariableDeclaratorSyntax = DirectCast(ParenthesizedVariableDesignation.Accept(Me), VariableDeclaratorSyntax)

                    Dim DeclarationToBeAdded As LocalDeclarationStatementSyntax =
                    VBFactory.LocalDeclarationStatement(
                                        DimModifier,
                                        VBFactory.SingletonSeparatedList(VariableDeclaration)
                                        )

                    Dim StatementWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(Node)
                    StatementWithIssues.AddMarker(DeclarationToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=False)
                    Return VBFactory.IdentifierName(Node.Designation.ToString.
                                                                    Replace(",", "", StringComparison.Ordinal).
                                                                    Replace(" ", "", StringComparison.Ordinal).
                                                                    Replace("(", "", StringComparison.Ordinal).
                                                                    Replace(")", "", StringComparison.Ordinal))
                End If
                If Node.Designation.IsKind(CS.SyntaxKind.DiscardDesignation) Then
                    Dim DiscardDesignation As CSS.DiscardDesignationSyntax = DirectCast(Node.Designation, CSS.DiscardDesignationSyntax)
                    Value = DirectCast(DiscardDesignation.Accept(Me), IdentifierNameSyntax).WithConvertedTriviaFrom(Node)
                    Return Value

                End If
                Throw UnreachableException
            End Function

            Public Overrides Function VisitDefaultExpression(node As CSS.DefaultExpressionSyntax) As VB.VisualBasicSyntaxNode
                Return VBFactory.ParseExpression($"CType(Nothing, {node.Type.Accept(Me).WithLeadingTrivia(SpaceTrivia)})").WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitDiscardDesignation(node As CSS.DiscardDesignationSyntax) As VB.VisualBasicSyntaxNode
                Dim Identifier As SyntaxToken = GenerateSafeVBToken(node.UnderscoreToken)
                Dim IdentifierExpression As IdentifierNameSyntax = VBFactory.IdentifierName(Identifier)
                Dim ModifiedIdentifier As ModifiedIdentifierSyntax = VBFactory.ModifiedIdentifier(Identifier)
                Dim SeparatedSyntaxListOfModifiedIdentifier As SeparatedSyntaxList(Of ModifiedIdentifierSyntax) =
                    VBFactory.SingletonSeparatedList(
                        ModifiedIdentifier
                        )
                Dim Parent As CSS.DeclarationExpressionSyntax
                Dim TypeName As VB.VisualBasicSyntaxNode
                If TypeOf node.Parent Is CSS.DeclarationExpressionSyntax Then
                    Parent = DirectCast(node.Parent, CSS.DeclarationExpressionSyntax)
                    TypeName = Parent.Type.Accept(Me)
                    If TypeName.ToString = "var" Then
                        TypeName = PredefinedTypeObject
                    End If
                ElseIf node.ToString = "_" Then
                    TypeName = PredefinedTypeObject
                Else
                    Stop
                    TypeName = PredefinedTypeObject
                End If

                Dim SeparatedListOfvariableDeclarations As SeparatedSyntaxList(Of VariableDeclaratorSyntax) =
                    VBFactory.SingletonSeparatedList(
                        VBFactory.VariableDeclarator(
                            SeparatedSyntaxListOfModifiedIdentifier,
                            VBFactory.SimpleAsClause(ConvertToType(TypeName.NormalizeWhitespace.ToString)),
                            VBFactory.EqualsValue(NothingExpression)
                                )
                         )

                Dim DeclarationStatement As LocalDeclarationStatementSyntax =
                    VBFactory.LocalDeclarationStatement(DimModifier,
                                                            SeparatedListOfvariableDeclarations).
                                                    WithAdditionalAnnotations(Simplifier.Annotation)

                GetStatementwithIssues(node).AddMarker(DeclarationStatement, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                Return IdentifierExpression
            End Function

            Public Overrides Function VisitElementAccessExpression(node As CSS.ElementAccessExpressionSyntax) As VB.VisualBasicSyntaxNode
                If node.ArgumentList.Arguments.Count = 1 AndAlso node.ArgumentList.Arguments(0).Expression.IsKind(CS.SyntaxKind.RangeExpression) Then
                    Dim RangeExpression As CSS.RangeExpressionSyntax = CType(node.ArgumentList.Arguments(0).Expression, CSS.RangeExpressionSyntax)
                    Dim LeftOperand As VB.VisualBasicSyntaxNode = RangeExpression.LeftOperand?.Accept(Me)
                    Dim RightOperand As ExpressionSyntax
                    Dim LeftExpression As ExpressionSyntax = DirectCast(node.Expression.Accept(Me), ExpressionSyntax)
                    If RangeExpression.RightOperand?.IsKind(CS.SyntaxKind.IndexExpression) Then
                        Dim OffsetFromLength As ExpressionSyntax = CType(RangeExpression.RightOperand.Accept(Me), ExpressionSyntax)
                        RightOperand = VBFactory.ParseExpression($"{LeftExpression}.Length{OffsetFromLength}")
                    Else
                        RightOperand = CType(RangeExpression.RightOperand?.Accept(Me), ExpressionSyntax)
                    End If
                    If LeftOperand Is Nothing Then
                        Return VBFactory.ParseExpression($"{LeftExpression}.Substring(0, {RightOperand})")
                    Else
                        If RightOperand Is Nothing Then
                            Return VBFactory.ParseExpression($"{LeftExpression}.Substring({LeftOperand})")
                        Else
                            Return VBFactory.ParseExpression($"{LeftExpression}.Substring({LeftOperand}, {RightOperand})")
                        End If
                    End If

                End If
                Dim argumentList As ArgumentListSyntax = DirectCast(node.ArgumentList.Accept(Me), ArgumentListSyntax)
                Dim expression As ExpressionSyntax
                If node.Expression.IsKind(CS.SyntaxKind.BaseExpression) Then
                    If node.GetAncestor(Of CSS.IndexerDeclarationSyntax).IsKind(CS.SyntaxKind.IndexerDeclaration) Then
                        expression = VBFactory.ParseExpression($"MyBase.Item")
                    ElseIf node.GetAncestor(Of CSS.PropertyDeclarationSyntax) IsNot Nothing Then
                        Return VBFactory.ParseExpression($"MyBase.Item({argumentList.Arguments(0).WithoutTrivia}))")
                    Else
                        Return VBFactory.ParseName($"MyBase.{argumentList.Arguments(0)}")
                    End If
                ElseIf node.Expression.IsKind(CS.SyntaxKind.ObjectCreationExpression) Then
                    expression = DirectCast(node.Expression.Accept(Me), ExpressionSyntax)
                    Dim UniqueName As String = MethodBodyVisitor.GetUniqueVariableNameInScope(node, "tempVar", _mSemanticModel)
                    Dim UniqueIdentifier As IdentifierNameSyntax = VBFactory.IdentifierName(VBFactory.Identifier(UniqueName))
                    Dim Names As SeparatedSyntaxList(Of ModifiedIdentifierSyntax) = VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(UniqueName))

                    Dim AsClause As AsClauseSyntax = VBFactory.AsNewClause(DirectCast(expression, NewExpressionSyntax))
                    Dim VariableDeclaration As SeparatedSyntaxList(Of VariableDeclaratorSyntax) = VBFactory.SingletonSeparatedList(VBFactory.VariableDeclarator(Names, AsClause, initializer:=Nothing))
                    Dim DimStatement As LocalDeclarationStatementSyntax = VBFactory.LocalDeclarationStatement(DimModifier, VariableDeclaration)
                    Dim StatementWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                    StatementWithIssues.AddMarker(DimStatement, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                    expression = UniqueIdentifier.WithTriviaFrom(expression)
                Else
                    expression = DirectCast(node.Expression.Accept(Me), ExpressionSyntax)
                End If

                Return VBFactory.InvocationExpression(expression, argumentList.WithoutLeadingTrivia)
            End Function

            Public Overrides Function VisitElementBindingExpression(node As CSS.ElementBindingExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim Arguments0 As VB.VisualBasicSyntaxNode = node.ArgumentList.Arguments(0).Accept(Me)
                Dim expression As ExpressionSyntax = VBFactory.ParseExpression(Arguments0.ToString)
                Dim ParenthesizedExpression As ParenthesizedExpressionSyntax = VBFactory.ParenthesizedExpression(expression)
                Return VBFactory.InvocationExpression(ParenthesizedExpression)
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
                            ItemWithTrivia = ItemWithTrivia.WithPrependedLeadingTrivia(ConvertTrivia(LeadingTrivia.Last))
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
                Dim CloseBraceTokenWithTrivia As SyntaxToken = VisualBasicSyntaxFactory.CloseBraceToken.WithConvertedTrailingTriviaFrom(node.Initializer.CloseBraceToken)
                If node.Parent.IsKind(CS.SyntaxKind.ElementAccessExpression) Then
                    CloseBraceTokenWithTrivia = CloseBraceTokenWithTrivia.WithTrailingTrivia(SpaceTrivia)
                End If
                If ExpressionItems.Any Then
                    RestructureNodesAndSeparators(OpenBraceTokenWithTrivia, ExpressionItems, Separators, CloseBraceTokenWithTrivia)
                    Dim ExpressionInitializers As SeparatedSyntaxList(Of ExpressionSyntax) = VBFactory.SeparatedList(ExpressionItems, Separators)
                    Return VBFactory.CollectionInitializer(OpenBraceTokenWithTrivia, ExpressionInitializers, CloseBraceTokenWithTrivia).WithConvertedLeadingTriviaFrom(node.NewKeyword)
                Else
                    RestructureNodesAndSeparators(OpenBraceTokenWithTrivia, NamedFieldItems, Separators, CloseBraceTokenWithTrivia)
                    Dim Initializers As SeparatedSyntaxList(Of FieldInitializerSyntax) = VBFactory.SeparatedList(NamedFieldItems)
                    Return VBFactory.AnonymousObjectCreationExpression(VBFactory.ObjectMemberInitializer(Initializers))
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
                        End Try

                        If ExpressionLastIndex > expressionIndex Then
                            Separators.Add(CommaToken.WithConvertedTrailingTriviaFrom(csSeparators(expressionIndex)))
                        Else
                            If FinalSeparator Then
                                If ItemIsField Then
                                    Fields(expressionIndex) = Fields(expressionIndex).WithAppendedTrailingTrivia(ConvertTrivia(csSeparators.Last.TrailingTrivia))
                                Else
                                    Expressions(expressionIndex) = Expressions(expressionIndex).WithAppendedTrailingTrivia(ConvertTrivia(csSeparators.Last.TrailingTrivia))
                                End If
                            End If
                        End If
                    Next
                    Dim CloseBracketLeadingTriva As List(Of SyntaxTrivia) = ConvertTrivia(node.CloseBraceToken.LeadingTrivia).ToList
                    If CloseBracketLeadingTriva.Any Then
                        If CloseBracketLeadingTriva.First.IsKind(VB.SyntaxKind.CommentTrivia) Then
                            CloseBracketLeadingTriva.Insert(1, VBEOLTrivia)
                        End If
                    End If
                    Dim CLoseBracketTrailingTriva As List(Of SyntaxTrivia) = ConvertTrivia(node.CloseBraceToken.TrailingTrivia).ToList
                    If CloseBracketLeadingTriva.ContainsCommentOrDirectiveTrivia Then
                        Dim FoundEOF As Boolean = False
                        Dim FoundCommentOrDirective As Boolean = False
                        Dim NewCLoseBracketLeadingTriva As New List(Of SyntaxTrivia)
                        For Each e As IndexClass(Of SyntaxTrivia) In CloseBracketLeadingTriva.WithIndex
                            Dim t As SyntaxTrivia = e.Value
                            If FoundCommentOrDirective OrElse t.IsDirective Or t.IsComment Then
                                If Not (FoundEOF OrElse FoundCommentOrDirective) Then
                                    NewCLoseBracketLeadingTriva.Add(VBEOLTrivia)
                                    FoundEOF = False
                                End If
                                FoundCommentOrDirective = True
                                NewCLoseBracketLeadingTriva.Add(t)
                                Continue For
                            End If
                            Select Case t.RawKind
                                Case VB.SyntaxKind.WhitespaceTrivia
                                    NewCLoseBracketLeadingTriva.Add(t)
                                Case VB.SyntaxKind.EndOfLineTrivia
                                    NewCLoseBracketLeadingTriva.Add(VBEOLTrivia)
                                    FoundEOF = True
                            End Select
                        Next
                        CloseBracketLeadingTriva = NewCLoseBracketLeadingTriva
                    End If

                    Dim CloseBraceTokenWithTrivia As SyntaxToken = VisualBasicSyntaxFactory.CloseBraceToken.With(CloseBracketLeadingTriva, CLoseBracketTrailingTriva)
                    If node.IsKind(CS.SyntaxKind.ObjectInitializerExpression) Then
                        Dim WithKeywordWithTrivia As SyntaxToken = WithKeyword.WithTrailingTrivia(VBEOLTrivia)
                        If Fields.Any Then
                            RestructureNodesAndSeparators(OpenBraceTokenWithTrivia, Fields, Separators, CloseBraceTokenWithTrivia)
                            Return VBFactory.ObjectMemberInitializer(WithKeywordWithTrivia, OpenBraceTokenWithTrivia, VBFactory.SeparatedList(Fields, Separators), CloseBraceTokenWithTrivia).WithConvertedTriviaFrom(node)
                        End If
                        RestructureNodesAndSeparators(OpenBraceTokenWithTrivia, Expressions, Separators, CloseBraceTokenWithTrivia)

                        If Expressions.Any Then
                            If Not Expressions(ExpressionLastIndex).ContainsEOLTrivia Then
                                Expressions(ExpressionLastIndex) = Expressions(ExpressionLastIndex).WithAppendedTrailingTrivia(VBEOLTrivia)
                                Return VBFactory.ObjectCollectionInitializer(VBFactory.CollectionInitializer(OpenBraceTokenWithTrivia, VBFactory.SeparatedList(Expressions.OfType(Of ExpressionSyntax), Separators), CloseBraceTokenWithTrivia))
                            End If
                        Else
                            Return VBFactory.ObjectCollectionInitializer(VBFactory.CollectionInitializer(OpenBraceTokenWithTrivia, VBFactory.SeparatedList(Expressions.OfType(Of ExpressionSyntax), Separators), CloseBraceTokenWithTrivia.WithoutTrivia)).WithTrailingTrivia(CloseBraceTokenWithTrivia.LeadingTrivia)
                        End If
                    End If

                    RestructureNodesAndSeparators(OpenBraceTokenWithTrivia, Expressions, Separators, CloseBraceTokenWithTrivia)
                    If node.IsKind(CS.SyntaxKind.ArrayInitializerExpression) OrElse node.IsKind(CS.SyntaxKind.CollectionInitializerExpression) Then
                        Dim initializers As SeparatedSyntaxList(Of ExpressionSyntax) = VBFactory.SeparatedList(Expressions, Separators)

                        Dim CollectionInitializer As CollectionInitializerSyntax = VBFactory.CollectionInitializer(OpenBraceTokenWithTrivia, initializers, CloseBraceTokenWithTrivia)
                        Return CollectionInitializer
                    End If
                    Return VBFactory.CollectionInitializer(OpenBraceTokenWithTrivia.RemoveExtraEOL, VBFactory.SeparatedList(Expressions, Separators), CloseBraceTokenWithTrivia)
                Catch ex As OperationCanceledException
                    Throw
                Catch ex As Exception
                    Stop
                End Try
                Throw UnreachableException
            End Function

            Public Overrides Function VisitInterpolatedStringExpression(node As CSS.InterpolatedStringExpressionSyntax) As VB.VisualBasicSyntaxNode
                Return VBFactory.InterpolatedStringExpression(node.Contents.Select(Function(c As CSS.InterpolatedStringContentSyntax) DirectCast(c.Accept(Me), InterpolatedStringContentSyntax)).ToArray()).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitInterpolatedStringText(node As CSS.InterpolatedStringTextSyntax) As VB.VisualBasicSyntaxNode
                Dim CSharpToken As SyntaxToken = node.TextToken
                Dim TextToken As SyntaxToken = ConvertToInterpolatedStringTextToken(CSharpToken)
                Return VBFactory.InterpolatedStringText(TextToken).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitInterpolation(node As CSS.InterpolationSyntax) As VB.VisualBasicSyntaxNode
                Return VBFactory.Interpolation(DirectCast(node.Expression.Accept(Me), ExpressionSyntax)).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitInterpolationFormatClause(node As CSS.InterpolationFormatClauseSyntax) As VB.VisualBasicSyntaxNode
                Return MyBase.VisitInterpolationFormatClause(node).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitInvocationExpression(node As CSS.InvocationExpressionSyntax) As VB.VisualBasicSyntaxNode
                If node.Expression.ToString.Equals("NameOf", StringComparison.OrdinalIgnoreCase) Then
                    Try
                        Dim IdentifierOrMember As String = node.ArgumentList.Arguments(0).Accept(Me).ToString
                        Return VBFactory.NameOfExpression(VBFactory.IdentifierName(IdentifierOrMember)).WithConvertedTriviaFrom(node)
                    Catch ex As OperationCanceledException
                        Throw
                    Catch ex As Exception
                        Stop
                    End Try
                    Throw UnreachableException
                End If
                Dim Expression As ExpressionSyntax = DirectCast(node.Expression.Accept(Me), ExpressionSyntax).WithoutTrailingTrivia
                Dim ArgumentList1 As ArgumentListSyntax = DirectCast(node.ArgumentList.Accept(Me), ArgumentListSyntax)
                Dim NewTrailingTrivia As List(Of SyntaxTrivia) = ArgumentList1.GetTrailingTrivia.ToList
                NewTrailingTrivia.AddRange(ConvertTrivia(node.GetTrailingTrivia))
                If NewTrailingTrivia.Count = 2 Then
                    Dim LastTrivia As SyntaxTrivia = NewTrailingTrivia.Last
                    Select Case NewTrailingTrivia.First.RawKind
                        Case VB.SyntaxKind.EndOfLineTrivia
                            If LastTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                NewTrailingTrivia.RemoveAt(1)
                            ElseIf LastTrivia.IsKind(VB.SyntaxKind.WhitespaceTrivia) OrElse
                                LastTrivia.IsKind(VB.SyntaxKind.EndIfDirectiveTrivia) Then
                                ' Ignore, it belongs here
                            Else
                                Stop
                            End If
                        Case VB.SyntaxKind.WhitespaceTrivia
                            If LastTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                NewTrailingTrivia.RemoveAt(1)
                            ElseIf LastTrivia.IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                                NewTrailingTrivia.RemoveAt(1)
                            Else
                                Stop
                            End If
                        Case Else
                            Stop
                    End Select
                End If
                Dim invocationExpressionSyntax1 As InvocationExpressionSyntax = VBFactory.InvocationExpression(Expression, ArgumentList1).WithConvertedLeadingTriviaFrom(node)
                Dim methodInfo As TypeInfo
                Try
                    methodInfo = ModelExtensions.GetTypeInfo(_mSemanticModel, node.Expression)
                    If methodInfo.Type?.Name = "Func" Then
                        Return VBFactory.InvocationExpression(invocationExpressionSyntax1.WithoutTrailingTrivia, VBFactory.ArgumentList()).WithTrailingTrivia(NewTrailingTrivia)
                    End If
                Catch ex As ArgumentException
                    ' ignored handled below
                Catch ex As Exception
                    Throw
                End Try
                Return invocationExpressionSyntax1.WithTrailingTrivia(NewTrailingTrivia)
            End Function

            ''' <summary>
            '''
            ''' </summary>
            ''' <param name="node"></param>
            ''' <returns></returns>
            ''' <remarks>Added by PC</remarks>
            Public Overrides Function VisitIsPatternExpression(node As CSS.IsPatternExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim Pattern As CSS.PatternSyntax = node.Pattern
                Dim VBExpression As ExpressionSyntax = DirectCast(node.Expression.Accept(Me), ExpressionSyntax)
                Dim StatementWithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                Dim ReportCheckCorrectness As Boolean = True
                If node.GetAncestor(Of CSS.SwitchSectionSyntax) IsNot Nothing Then
                    ReportCheckCorrectness = False
                End If

                If TypeOf Pattern Is CSS.DeclarationPatternSyntax Then
                    Dim DeclarationPattern As CSS.DeclarationPatternSyntax = DirectCast(Pattern, CSS.DeclarationPatternSyntax)
                    Dim Designation As CSS.SingleVariableDesignationSyntax = DirectCast(DeclarationPattern.Designation, CSS.SingleVariableDesignationSyntax)
                    Dim SeparatedList As SeparatedSyntaxList(Of ModifiedIdentifierSyntax)
                    Dim sepListVarDecl As SeparatedSyntaxList(Of VariableDeclaratorSyntax)
                    Dim DeclarationToBeAdded As LocalDeclarationStatementSyntax
                    Dim VariableType As TypeSyntax = CType(DeclarationPattern.Type.Accept(Me), TypeSyntax)
                    Dim value As ExpressionSyntax = VBFactory.TypeOfIsExpression(VBExpression, VariableType)
                    Dim uniqueIdToken As SyntaxToken = VBFactory.Identifier(MethodBodyVisitor.GetUniqueVariableNameInScope(node, "TempVar", _mSemanticModel))
                    Dim VariableName As ModifiedIdentifierSyntax =
                        VBFactory.ModifiedIdentifier(uniqueIdToken)
                    SeparatedList = VBFactory.SingletonSeparatedList(VariableName)
                    sepListVarDecl = VBFactory.SingletonSeparatedList(
                            node:=VBFactory.VariableDeclarator(
                                names:=SeparatedList,
                                asClause:=VBFactory.SimpleAsClause(VBFactory.PredefinedType(BooleanKeyword)),
                                initializer:=VBFactory.EqualsValue(value)
                                    )
                             )

                    DeclarationToBeAdded =
                        VBFactory.LocalDeclarationStatement(DimModifier,
                                                            sepListVarDecl
                                                            ).WithTrailingTrivia(VBEOLTrivia).WithAdditionalAnnotations(Simplifier.Annotation)
                    If StatementWithIssue.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        DeclarationToBeAdded = DeclarationToBeAdded.WithPrependedLeadingTrivia(ConvertTrivia(StatementWithIssue.GetLeadingTrivia))
                    End If
                    StatementWithIssue.AddMarker(Statement:=DeclarationToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)

                    Dim Identifier As SyntaxToken = GenerateSafeVBToken(id:=Designation.Identifier)
                    VariableName = VBFactory.ModifiedIdentifier(Identifier.WithTrailingTrivia(SpaceTrivia))
                    SeparatedList = VBFactory.SingletonSeparatedList(VariableName)
                    If VariableType.IsKind(VB.SyntaxKind.PredefinedType) Then
                        sepListVarDecl = VBFactory.SingletonSeparatedList(
                                node:=VBFactory.VariableDeclarator(
                                    names:=SeparatedList,
                                    asClause:=VBFactory.SimpleAsClause(VariableType),
                                    initializer:=Nothing)
                                 )

                        DeclarationToBeAdded =
                            VBFactory.LocalDeclarationStatement(DimModifier,
                                                                sepListVarDecl
                                                                ).WithTrailingTrivia(VBEOLTrivia).
                                                                  WithAdditionalAnnotations(Simplifier.Annotation)
                        StatementWithIssue.AddMarker(Statement:=DeclarationToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                        Dim simpleMemberAccess As MemberAccessExpressionSyntax = VBFactory.SimpleMemberAccessExpression(VariableType, VBFactory.IdentifierName("TryParse"))
                        Dim nodes As New List(Of ArgumentSyntax) From {
                            VBFactory.SimpleArgument(VBFactory.SimpleMemberAccessExpression(VBExpression.WithoutTrivia, VBFactory.IdentifierName("ToString"))),
                            VBFactory.SimpleArgument(VBFactory.IdentifierName(Identifier.WithoutTrivia))
                        }

                        Dim arguments As SeparatedSyntaxList(Of ArgumentSyntax) = VBFactory.SeparatedList(nodes)
                        Dim agrumentList As ArgumentListSyntax = VBFactory.ArgumentList(arguments)
                        Dim expression As InvocationExpressionSyntax = VBFactory.InvocationExpression(simpleMemberAccess, agrumentList)
                        Dim Statement As ExpressionStatementSyntax = VBFactory.ExpressionStatement(expression)
                        StatementWithIssue.AddMarker(Statement, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                    Else
                        sepListVarDecl = VBFactory.SingletonSeparatedList(
                                node:=VBFactory.VariableDeclarator(
                                    names:=SeparatedList,
                                    asClause:=VBFactory.SimpleAsClause(VariableType),
                                    initializer:=VBFactory.EqualsValue(VBExpression)
                                        )
                                 )

                        DeclarationToBeAdded =
                            VBFactory.LocalDeclarationStatement(
                                                                DimModifier,
                                                                sepListVarDecl
                                                                ).WithTrailingTrivia(VBEOLTrivia).
                                                                  WithAdditionalAnnotations(Simplifier.Annotation)
                        StatementWithIssue.AddMarker(Statement:=DeclarationToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                    End If

                    Return VBFactory.IdentifierName(uniqueIdToken)
                ElseIf TypeOf Pattern Is CSS.ConstantPatternSyntax Then
                    Dim ConstantPattern As CSS.ConstantPatternSyntax = DirectCast(node.Pattern, CSS.ConstantPatternSyntax)
                    If ConstantPattern.Expression.IsKind(CS.SyntaxKind.NullLiteralExpression) Then
                        Return VBFactory.IsExpression(left:=VBExpression, right:=NothingExpression)
                    End If
                    Return VBFactory.EqualsExpression(left:=VBExpression, right:=DirectCast(ConstantPattern.Expression.Accept(Me), ExpressionSyntax))
                ElseIf TypeOf Pattern Is CSS.VarPatternSyntax Then
                    Dim VarPattern As CSS.VarPatternSyntax = DirectCast(Pattern, CSS.VarPatternSyntax)
                    Dim IdentifierName As String
                    If TypeOf VarPattern.Designation Is CSS.SingleVariableDesignationSyntax Then
                        IdentifierName = DirectCast(VarPattern.Designation, CSS.SingleVariableDesignationSyntax).Identifier.ToString
                    ElseIf TypeOf VarPattern.Designation Is CSS.ParenthesizedVariableDesignationSyntax Then
                        Dim Designation As CSS.ParenthesizedVariableDesignationSyntax = DirectCast(VarPattern.Designation, CSS.ParenthesizedVariableDesignationSyntax)
                        Dim VariableNames As New List(Of String)
                        For Each e As IndexClass(Of CSS.VariableDesignationSyntax) In Designation.Variables.WithIndex
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
                        IdentifierName = MethodBodyVisitor.GetUniqueVariableNameInScope(node, "TempVar", _mSemanticModel)
                    Else
                        Stop
                        Throw UnreachableException()
                    End If
                    Dim SafeIdToken As SyntaxToken = GenerateSafeVBToken(CS.SyntaxFactory.Identifier(IdentifierName))
                    Dim Name As IdentifierNameSyntax = VBFactory.IdentifierName(SafeIdToken)

                    Dim VariableName As ModifiedIdentifierSyntax = VBFactory.ModifiedIdentifier(SafeIdToken.WithTrailingTrivia(SpaceTrivia))

                    Dim Declarators As SeparatedSyntaxList(Of VariableDeclaratorSyntax) = VBFactory.SingletonSeparatedList(
                            node:=VBFactory.VariableDeclarator(VBFactory.SingletonSeparatedList(
                            VariableName),
                            asClause:=Nothing, initializer:=VBFactory.EqualsValue(VBExpression)).WithTrailingEOL
                             )

                    Dim LeadingTrivia As SyntaxTriviaList = StatementWithIssue.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=True, "VB has no direct equivalent To C# var pattern expressions")
                    Dim DeclarationToBeAdded As LocalDeclarationStatementSyntax = VBFactory.LocalDeclarationStatement(DimModifier,
                        Declarators).WithAdditionalAnnotations(Simplifier.Annotation)
                    If ReportCheckCorrectness Then
                        DeclarationToBeAdded = DeclarationToBeAdded.WithPrependedLeadingTrivia(StatementWithIssue.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=True, "VB has no direct equivalent To C# var pattern expressions")).WithTrailingEOL
                    End If

                    StatementWithIssue.AddMarker(Statement:=DeclarationToBeAdded,
                                                 StatementHandlingOption.PrependStatement, AllowDuplicates:=True)

                    Return Name
                ElseIf TypeOf Pattern Is CSS.RecursivePatternSyntax Then
                    Dim EmptyStatementWithError As EmptyStatementSyntax = FlagUnsupportedStatements(StatementWithIssue,
                                                                                            "Recursive Pattern Syntax",
                                                                                            CommentOutOriginalStatements:=True)
                    StatementWithIssue.AddMarker(Statement:=EmptyStatementWithError,
                                                 StatementHandlingOption.ReplaceStatement,
                                                 AllowDuplicates:=True)
                    Return VBFactory.IdentifierName("DoNotCare")
                Else
                    Stop
                End If
                Throw UnreachableException
            End Function

            Public Overrides Function VisitLiteralExpression(node As CSS.LiteralExpressionSyntax) As VB.VisualBasicSyntaxNode
                ' now this looks somehow like a hack... is there a better way?
                If node.IsKind(CS.SyntaxKind.StringLiteralExpression) Then
                    ' @"" have no escapes except quotes (ASCII and Unicode)
                    If node.Token.Text.StartsWith("@", StringComparison.Ordinal) Then
                        Return VBFactory.StringLiteralExpression(
                                                    token:=VBFactory.StringLiteralToken(
                                                    text:=node.Token.Text.
                                                        Substring(1).
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
                        Return VBFactory.InterpolatedStringExpression(
                                            VBFactory.InterpolatedStringText(
                                                                ConvertToInterpolatedStringTextToken(node.Token.WithoutTrivia))
                                                                            )
                    End If
                    Return GetLiteralExpression(node.Token.Value, node.Token, Me).WithConvertedTriviaFrom(node.Token)
                End If

                If node.IsKind(CS.SyntaxKind.DefaultLiteralExpression) Then
                    Select Case node.Parent.RawKind
                        Case CS.SyntaxKind.ReturnStatement
                            Dim FnStmt As CSS.MethodDeclarationSyntax = node.Parent.GetAncestor(Of CSS.MethodDeclarationSyntax)
                            If FnStmt IsNot Nothing Then
                                Return VBFactory.CTypeExpression(
                                                    NothingExpression,
                                                    CType(FnStmt.ReturnType.Accept(Me), TypeSyntax)
                                                    )
                            End If
                        Case CS.SyntaxKind.EqualsValueClause
                            Dim LocalDecStmt As CSS.LocalDeclarationStatementSyntax = node.Parent.GetAncestor(Of CSS.LocalDeclarationStatementSyntax)
                            If LocalDecStmt IsNot Nothing Then
                                If LocalDecStmt.Declaration.Type IsNot Nothing Then
                                    Return VBFactory.CTypeExpression(
                                                        NothingExpression,
                                                        CType(LocalDecStmt.Declaration.Type.Accept(Me).WithoutLeadingTrivia, TypeSyntax)
                                                        )
                                End If
                            End If
                            Dim VariableDeclaration As CSS.VariableDeclarationSyntax = node.Parent.GetAncestor(Of CSS.VariableDeclarationSyntax)
                            If VariableDeclaration IsNot Nothing Then
                                If VariableDeclaration.Type IsNot Nothing Then
                                    Return VBFactory.CTypeExpression(
                                                        NothingExpression,
                                                        CType(VariableDeclaration.Type.Accept(Me), TypeSyntax)
                                                        )
                                End If
                            ElseIf node.Parent.Parent.RawKind = CS.SyntaxKind.Parameter Then
                                Dim Parameter As CSS.ParameterSyntax = CType(node.Parent.Parent, CSS.ParameterSyntax)
                                If Parameter.Type IsNot Nothing Then
                                    Return VBFactory.CTypeExpression(
                                                        NothingExpression,
                                                        CType(Parameter.Type.Accept(Me), TypeSyntax)
                                                        )
                                End If
                            Else
                                Stop
                            End If
                        Case CS.SyntaxKind.SimpleAssignmentExpression
                            Dim LeftNodeTypeInfo As TypeInfo = ModelExtensions.GetTypeInfo(_mSemanticModel, DirectCast(node.Parent, CSS.AssignmentExpressionSyntax).Left)
                            If LeftNodeTypeInfo.Type Is Nothing OrElse LeftNodeTypeInfo.Type.IsErrorType Then
                                Return NothingExpression
                            End If
                            If LeftNodeTypeInfo.Type?.IsTupleType Then
                                Dim TypeList As New List(Of String)
                                Dim TupleNameString As String = LeftNodeTypeInfo.Type.ToString
                                TupleNameString = TupleNameString.Substring(1, TupleNameString.Length - 2)
                                For Each s As String In TupleNameString.Split(","c)
                                    If s.Trim.Contains(" ", StringComparison.Ordinal) Then
                                        TypeList.Add(MakeVBSafeName(s.Trim.Split(" "c)(0).
                                                                           Replace("<", "(Of ", StringComparison.Ordinal).
                                                                           Replace(">", ")", StringComparison.Ordinal)))
                                    Else
                                        TypeList.Add(MakeVBSafeName(s.Trim.
                                                                           Replace("<", "(Of ", StringComparison.Ordinal).
                                                                           Replace(">", ")", StringComparison.Ordinal)))
                                    End If
                                Next
                                Return VBFactory.CTypeExpression(NothingExpression,
                                                                 VBFactory.ParseTypeName($"({String.Join(", ", TypeList)})"))
                            Else
                                Return If(LeftNodeTypeInfo.Type.Name Is "",
                                            NothingExpression,
                                            CType(VBFactory.CTypeExpression(NothingExpression, ConvertToType(LeftNodeTypeInfo.Type.Name)), ExpressionSyntax))
                            End If
                        Case CS.SyntaxKind.ConditionalExpression
                            Dim LeftNodeTypeInfo As TypeInfo = ModelExtensions.GetTypeInfo(_mSemanticModel, DirectCast(node.Parent, CSS.ConditionalExpressionSyntax).WhenTrue)
                            If LeftNodeTypeInfo.Type Is Nothing OrElse LeftNodeTypeInfo.Type.IsErrorType Then
                                Return NothingExpression
                            End If
                            Dim _Type As TypeSyntax = If(LeftNodeTypeInfo.Type.IsTupleType, ConvertCSTupleToVBType(LeftNodeTypeInfo.Type.ToString), ConvertToType(LeftNodeTypeInfo.Type.Name))
                            Return VBFactory.CTypeExpression(
                                                      NothingExpression,
                                                      _Type
                                                      )
                        Case CS.SyntaxKind.EqualsExpression, CS.SyntaxKind.NotEqualsExpression
                            Return VBFactory.CTypeExpression(
                                                      NothingExpression,
                                                      VBFactory.PredefinedType(BooleanKeyword)
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
                    If node.Token.Text.Replace("'", "", StringComparison.Ordinal).Length <= 2 Then
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

                Dim Expression As ExpressionSyntax = DirectCast(node.Expression.Accept(Me), ExpressionSyntax).WithConvertedTriviaFrom(node.Expression)
                Dim ValueText As String
                If node.Expression.IsKind(CS.SyntaxKind.IdentifierName) Then
                    ValueText = CType(Expression, IdentifierNameSyntax).Identifier.ValueText
                    If ValueText.EndsWith("_renamed", StringComparison.OrdinalIgnoreCase) Then
                        Expression = VBFactory.ParseExpression($"Me.{ValueText.Replace("_renamed", "", StringComparison.OrdinalIgnoreCase)})").WithTriviaFrom(Expression)
                    End If
                End If

                If TypeOf Expression Is NewExpressionSyntax AndAlso Not TypeOf Expression Is ArrayCreationExpressionSyntax Then
                    Dim UniqueName As String = MethodBodyVisitor.GetUniqueVariableNameInScope(node, "tempVar", _mSemanticModel)
                    Dim UniqueIdentifier As IdentifierNameSyntax = VBFactory.IdentifierName(VBFactory.Identifier(UniqueName))
                    Dim Names As SeparatedSyntaxList(Of ModifiedIdentifierSyntax) = VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(UniqueName))
                    Dim AsClause As AsClauseSyntax = VBFactory.AsNewClause(DirectCast(Expression.With({SpaceTrivia}, {SpaceTrivia}), NewExpressionSyntax))
                    Dim VariableDeclaration As SeparatedSyntaxList(Of VariableDeclaratorSyntax) = VBFactory.SingletonSeparatedList(VBFactory.VariableDeclarator(Names, AsClause, initializer:=Nothing))
                    Dim DimStatement As LocalDeclarationStatementSyntax = VBFactory.LocalDeclarationStatement(DimModifier, VariableDeclaration).WithLeadingTrivia(Expression.GetLeadingTrivia).WithTrailingEOL
                    Dim StatementWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                    StatementWithIssues.AddMarker(DimStatement.WithTrailingEOL, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                    Expression = UniqueIdentifier.WithLeadingTrivia(Expression.GetLeadingTrivia.Last).WithTrailingTrivia(Expression.GetTrailingTrivia)
                ElseIf TypeOf Expression Is CollectionInitializerSyntax Then
                    Dim UniqueName As String = MethodBodyVisitor.GetUniqueVariableNameInScope(node, "tempVar", _mSemanticModel)
                    Dim UniqueIdentifier As IdentifierNameSyntax = VBFactory.IdentifierName(VBFactory.Identifier(UniqueName))
                    Dim Names As SeparatedSyntaxList(Of ModifiedIdentifierSyntax) = VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(UniqueName))
                    Dim Initializer As EqualsValueSyntax = VBFactory.EqualsValue(Expression)
                    Dim VariableDeclaration As SeparatedSyntaxList(Of VariableDeclaratorSyntax) = VBFactory.SingletonSeparatedList(VBFactory.VariableDeclarator(Names, asClause:=Nothing, Initializer))
                    Dim DimStatement As LocalDeclarationStatementSyntax = VBFactory.LocalDeclarationStatement(DimModifier, VariableDeclaration).WithTrailingEOL
                    Dim StatementWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                    StatementWithIssues.AddMarker(DimStatement, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                    Expression = UniqueIdentifier.WithTriviaFrom(Expression)
                End If

                Dim NeedOperatorEOL As Boolean = False
                Dim NewNameLeadingTrivia As New List(Of SyntaxTrivia)
                Dim OperatorTrailingTrivia As New List(Of SyntaxTrivia)
                Dim OldNameLeadingTrivia As List(Of SyntaxTrivia) = ConvertTrivia(node.Name.GetLeadingTrivia).ToList
                If OldNameLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    For Each t As SyntaxTrivia In ConvertTrivia(node.OperatorToken.TrailingTrivia)
                        Select Case t.RawKind
                            Case VB.SyntaxKind.WhitespaceTrivia
                                OperatorTrailingTrivia.Add(t)
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
                                NewNameLeadingTrivia.Add(t)
                            Case VB.SyntaxKind.CommentTrivia
                                OperatorTrailingTrivia.Add(t)
                            Case VB.SyntaxKind.EndOfLineTrivia
                                ' Ignore
                            Case Else
                                Stop
                        End Select
                    Next
                Else
                    OperatorTrailingTrivia = ConvertTrivia(node.Name.GetLeadingTrivia).ToList
                End If
                If NeedOperatorEOL Then
                    OperatorTrailingTrivia.Add(VBEOLTrivia)
                End If
                Dim OperatorToken As SyntaxToken = DotToken.With(ConvertTrivia(node.OperatorToken.LeadingTrivia).ToList, OperatorTrailingTrivia)
                Dim Name As SimpleNameSyntax = DirectCast(node.Name.Accept(Me).With(NewNameLeadingTrivia, ConvertTrivia(node.Name.GetTrailingTrivia)), SimpleNameSyntax)
                ValueText = Name.Identifier.ValueText
                Dim keywordKind As VB.SyntaxKind = VB.SyntaxFacts.GetKeywordKind(ValueText)
                If VB.SyntaxFacts.IsKeywordKind(keywordKind) Then
                    Dim nameSyntax As IdentifierNameSyntax = VBFactory.IdentifierName($"[{ValueText}]")
                    Name = Name.WithIdentifier(nameSyntax.Identifier)
                End If
                OperatorTrailingTrivia.Clear()

                If Expression.GetLastToken.ContainsEOLTrivia Then
                    Dim FoundEOL As Boolean = False
                    FoundEOL = RestructureTrivia(node, TriviaList:=Expression.GetTrailingTrivia, FoundEOL, OperatorTrailingTrivia)
                    FoundEOL = RestructureTrivia(node, TriviaList:=OperatorToken.LeadingTrivia, FoundEOL, OperatorTrailingTrivia)

                    If FoundEOL Then
                        OperatorTrailingTrivia.Add(VBEOLTrivia)
                    End If
                    Expression = Expression.WithoutTrailingTrivia
                    OperatorToken = OperatorToken.WithoutTrivia.WithTrailingTrivia(OperatorTrailingTrivia)
                    Name = Name.WithLeadingTrivia(SpaceTrivia)
                End If
                Return WrapTypedNameIfNecessary(name:=VBFactory.MemberAccessExpression(VB.SyntaxKind.SimpleMemberAccessExpression, Expression, OperatorToken, Name), originalName:=node).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitMemberBindingExpression(node As CSS.MemberBindingExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim name As SimpleNameSyntax = DirectCast(node.Name.Accept(Me), SimpleNameSyntax)
                Return VBFactory.SimpleMemberAccessExpression(name:=name)
            End Function

            Public Overrides Function VisitObjectCreationExpression(node As CSS.ObjectCreationExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim type1 As TypeSyntax = DirectCast(node.Type.Accept(Me), TypeSyntax)

                Dim argumentList As ArgumentListSyntax = DirectCast(node.ArgumentList?.Accept(Me), ArgumentListSyntax)
                If argumentList IsNot Nothing Then
                    If type1.ToString.EndsWith("EventHandler", StringComparison.Ordinal) AndAlso
                        argumentList.Arguments.Count = 1 Then
                        argumentList = VBFactory.ArgumentList(VBFactory.SingletonSeparatedList(Of ArgumentSyntax)(VBFactory.SimpleArgument(VBFactory.AddressOfExpression(DirectCast(argumentList.Arguments(0), SimpleArgumentSyntax).Expression))))
                    End If
                    type1 = type1.WithTrailingTrivia(SpaceTrivia)
                End If
                Dim PossibleInitializer As VB.VisualBasicSyntaxNode = node.Initializer?.Accept(Me)
                Dim initializer As ObjectCollectionInitializerSyntax = Nothing
                If PossibleInitializer IsNot Nothing Then
                    type1 = type1.WithTrailingTrivia(SpaceTrivia)
                    Select Case PossibleInitializer.Kind
                        Case VB.SyntaxKind.CollectionInitializer
                            initializer = VBFactory.ObjectCollectionInitializer(initializer:=DirectCast(PossibleInitializer, CollectionInitializerSyntax))
                        Case VB.SyntaxKind.ObjectCollectionInitializer
                            initializer = DirectCast(PossibleInitializer, ObjectCollectionInitializerSyntax)
                        Case VB.SyntaxKind.ObjectMemberInitializer
                            ' Remove trailing trivia before with
                            If argumentList IsNot Nothing Then
                                argumentList = argumentList.WithCloseParenToken(CloseParenToken)
                            End If
                            Dim memberinitializer As ObjectMemberInitializerSyntax = DirectCast(PossibleInitializer, ObjectMemberInitializerSyntax)
                            Return VBFactory.ObjectCreationExpression(NewKeyword, VBFactory.List(Of AttributeListSyntax)(), type1.WithTrailingTrivia(SpaceTrivia), argumentList, memberinitializer)

                        Case Else
                            Throw UnexpectedValue(NameOf(PossibleInitializer))
                    End Select
                End If
                If argumentList IsNot Nothing AndAlso initializer?.GetFirstToken.IsKind(VB.SyntaxKind.FromKeyword) Then
                    argumentList = argumentList.WithTrailingTrivia(SpaceTrivia)
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
                    argumentList = argumentList.WithArguments(VBFactory.SeparatedList(NewArgumentList, NewSeparatorList))
                End If
                If argumentList?.Arguments.Count = 0 Then
                    argumentList = Nothing
                End If
                Return VBFactory.ObjectCreationExpression(VBFactory.List(Of AttributeListSyntax)(), type1, argumentList, initializer)
            End Function

            Public Overrides Function VisitParenthesizedExpression(node As CSS.ParenthesizedExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim Expression As ExpressionSyntax = DirectCast(node.Expression.Accept(Me), ExpressionSyntax)
                If TypeOf Expression Is CTypeExpressionSyntax OrElse
                   TypeOf Expression Is IdentifierNameSyntax OrElse
                   TypeOf Expression Is InvocationExpressionSyntax OrElse
                   TypeOf Expression Is QueryExpressionSyntax OrElse
                    TypeOf Expression Is TryCastExpressionSyntax Then
                    Return Expression.WithTrailingTrivia(SpaceTrivia)
                End If
                Dim DeclarationToBeAdded As LocalDeclarationStatementSyntax
                If TypeOf node.Parent Is CSS.MemberAccessExpressionSyntax OrElse
                   TypeOf node.Parent Is CSS.ElementAccessExpressionSyntax OrElse
                    TypeOf node.Parent Is CSS.ConditionalAccessExpressionSyntax Then
                    ' Statement with issues points to "Statement" Probably an Expression Statement. If this is part of a single Line If we need to go higher
                    Dim StatementWithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                    ' Statement with issues points to "Statement" Probably an Expression Statement. If this is part of an ElseIf we need to go higher
                    Dim VariableDeclaration As VariableDeclaratorSyntax
                    Dim Initializer As EqualsValueSyntax = VBFactory.EqualsValue(Expression)
                    If TypeOf node.Parent Is CSS.MemberAccessExpressionSyntax OrElse TypeOf node.Parent Is CSS.ElementAccessExpressionSyntax Then
                        If node.Expression.IsKind(CS.SyntaxKind.AddExpression) Then
                            Return VBFactory.ParenthesizedExpression(OpenParenToken.WithConvertedTriviaFrom(node.OpenParenToken), Expression, CloseParenToken.WithConvertedTriviaFrom(node.CloseParenToken))
                        End If
                        Dim UniqueName As String = MethodBodyVisitor.GetUniqueVariableNameInScope(node, "tempVar", _mSemanticModel)
                        Dim Names As SeparatedSyntaxList(Of ModifiedIdentifierSyntax) = VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(UniqueName))
                        Dim UniqueIdentifier As IdentifierNameSyntax = VBFactory.IdentifierName(VBFactory.Identifier(UniqueName))
                        If TypeOf Expression Is TernaryConditionalExpressionSyntax Then
                            Dim TExpression As TernaryConditionalExpressionSyntax = DirectCast(Expression, TernaryConditionalExpressionSyntax)
                            If TExpression.Condition.IsKind(VB.SyntaxKind.IdentifierName) Then
                                Dim IfStatement As IfStatementSyntax =
                                   VBFactory.IfStatement(IfKeyword, TExpression.Condition, ThenKeyword).WithConvertedLeadingTriviaFrom(node)
                                Dim EndIfStatement As EndBlockStatementSyntax = VBFactory.EndIfStatement(EndKeyword, IfKeyword).WithConvertedTrailingTriviaFrom(node)
                                Dim IfBlockStatements As New SyntaxList(Of StatementSyntax)
                                IfBlockStatements = IfBlockStatements.Add(VBFactory.SimpleAssignmentStatement(left:=UniqueIdentifier, right:=TExpression.WhenTrue).RemoveLineContinuation)
                                Dim ElseBlockStatements As New SyntaxList(Of StatementSyntax)
                                ElseBlockStatements = ElseBlockStatements.Add(VBFactory.SimpleAssignmentStatement(left:=UniqueIdentifier, right:=TExpression.WhenFalse).RemoveLineContinuation)
                                Dim ElseBlock As ElseBlockSyntax = VBFactory.ElseBlock(ElseBlockStatements)
                                Dim IfBlockToBeAdded As StatementSyntax = VBFactory.MultiLineIfBlock(
                                                                    IfStatement,
                                                                    IfBlockStatements,
                                                                    Nothing,
                                                                    ElseBlock,
                                                                    EndIfStatement)

                                VariableDeclaration = VBFactory.VariableDeclarator(Names, asClause:=Nothing, initializer:=Nothing)
                                DeclarationToBeAdded =
                                   VBFactory.LocalDeclarationStatement(
                                    DimModifier,
                                    VBFactory.SingletonSeparatedList(VariableDeclaration)
                                    ).WithPrependedLeadingTrivia(VBFactory.CommentTrivia($" ' TODO: Check, VB does not directly support MemberAccess off a Conditional If Expression")).WithTrailingEOL

                                StatementWithIssue.AddMarker(DeclarationToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                                StatementWithIssue.AddMarker(IfBlockToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                                Return UniqueIdentifier
                            End If
                        ElseIf TypeOf Expression Is BinaryConditionalExpressionSyntax Then
                            Dim BExpression As BinaryConditionalExpressionSyntax = DirectCast(Expression, BinaryConditionalExpressionSyntax)
                            If BExpression.FirstExpression.IsKind(VB.SyntaxKind.IdentifierName) Then
                                Dim FirstIdentifier As IdentifierNameSyntax = DirectCast(BExpression.FirstExpression, IdentifierNameSyntax)
                                Dim IfStatement As IfStatementSyntax =
                                   VBFactory.IfStatement(IfKeyword, VBFactory.IsExpression(left:=FirstIdentifier, right:=NothingExpression), ThenKeyword).WithConvertedLeadingTriviaFrom(node)
                                Dim EndIfStatement As EndBlockStatementSyntax = VBFactory.EndIfStatement(EndKeyword, IfKeyword).WithConvertedTrailingTriviaFrom(node)
                                Dim Statements As New SyntaxList(Of StatementSyntax)
                                Statements = Statements.Add(VBFactory.SimpleAssignmentStatement(left:=FirstIdentifier, right:=BExpression.SecondExpression))

                                Dim IfBlockToBeAdded As StatementSyntax = VBFactory.MultiLineIfBlock(
                                                                    ifStatement:=IfStatement,
                                                                    statements:=Statements,
                                                                    elseIfBlocks:=Nothing,
                                                                    elseBlock:=Nothing,
                                                                    endIfStatement:=EndIfStatement).
                                 WithPrependedLeadingTrivia(VBFactory.CommentTrivia($"' TODO: Check, VB does not directly support MemberAccess off a Conditional If Expression")).WithTrailingEOL
                                StatementWithIssue.AddMarker(IfBlockToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                                Return UniqueIdentifier
                            End If
                        End If
                        Dim AwaitNotSupported As String = ""
                        If Initializer.Value.IsKind(VB.SyntaxKind.AwaitExpression) AndAlso Not IsDecedentOfAsyncMethod(node) Then
                            Initializer = VBFactory.EqualsValue(CType(Initializer.Value, AwaitExpressionSyntax).Expression)
                            AwaitNotSupported = " Await removed, in non Async Function,"
                        End If
                        VariableDeclaration = VBFactory.VariableDeclarator(Names, asClause:=Nothing, Initializer)
                        DeclarationToBeAdded =
                                   VBFactory.LocalDeclarationStatement(
                                    DimModifier,
                                    VBFactory.SingletonSeparatedList(VariableDeclaration)
                                    ).WithPrependedLeadingTrivia(VBFactory.CommentTrivia($" ' TODO: Check,{AwaitNotSupported} VB does not directly support MemberAccess off a Conditional If Expression")).WithTrailingEOL

                        StatementWithIssue.AddMarker(DeclarationToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=False)
                        Return UniqueIdentifier
                    ElseIf TypeOf node.Parent Is CSS.ConditionalAccessExpressionSyntax Then
                        Dim UniqueName As String = MethodBodyVisitor.GetUniqueVariableNameInScope(node, "tempVar", _mSemanticModel)
                        Dim Names As SeparatedSyntaxList(Of ModifiedIdentifierSyntax) = VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(UniqueName))
                        VariableDeclaration = VBFactory.VariableDeclarator(Names, asClause:=Nothing, Initializer)
                        DeclarationToBeAdded = VBFactory.LocalDeclarationStatement(
                                            DimModifier,
                                            VBFactory.SingletonSeparatedList(VariableDeclaration)
                                            ).WithConvertedTriviaFrom(node).WithTrailingEOL

                        ' Statement with issues points to "Statement" Probably an Expression Statement. If this is part of a single Line If we need to go higher
                        StatementWithIssue.AddMarker(DeclarationToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=False)
                        Return VBFactory.IdentifierName(VBFactory.Identifier(UniqueName))
                    End If
                End If

                'If Expression.ContainsCommentOrDirectiveTrivia Then
                '    Expression = Expression.RestructureCommentTrivia
                'End If
                Dim TrailingTrivia As List(Of SyntaxTrivia) = Expression.GetTrailingTrivia.ToList
                Dim parenthesizedExpressionSyntax1 As ParenthesizedExpressionSyntax = VBFactory.ParenthesizedExpression(Expression.WithoutTrailingTrivia).WithTrailingTrivia(TrailingTrivia)
                Return parenthesizedExpressionSyntax1
            End Function

            Public Overrides Function VisitParenthesizedLambdaExpression(node As CSS.ParenthesizedLambdaExpressionSyntax) As VB.VisualBasicSyntaxNode
                Return ConvertLambdaExpression(node, node.Body, node.ParameterList.Parameters, VBFactory.TokenList(node.AsyncKeyword))
            End Function

            Public Overrides Function VisitParenthesizedVariableDesignation(node As CSS.ParenthesizedVariableDesignationSyntax) As VB.VisualBasicSyntaxNode
                Dim vbVariables As List(Of ModifiedIdentifierSyntax) = ProcessVariableDesignation(node)
                Dim vbNames As SeparatedSyntaxList(Of ModifiedIdentifierSyntax) = VBFactory.SeparatedList(vbVariables)
                Return VBFactory.VariableDeclarator(vbNames, VBFactory.SimpleAsClause(PredefinedTypeObject), initializer:=Nothing)
            End Function

            Public Overrides Function VisitPostfixUnaryExpression(node As CSS.PostfixUnaryExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim CSExpressionKind As CS.SyntaxKind = CS.CSharpExtensions.Kind(node)
                Dim vbOperandExpression As ExpressionSyntax = DirectCast(node.Operand.Accept(Me), ExpressionSyntax)
                If CSExpressionKind = CS.SyntaxKind.SuppressNullableWarningExpression Then
                    Return vbOperandExpression
                End If
                Dim kind As VB.SyntaxKind = ConvertCSExpressionsKindToVBKind(CSExpressionKind)
                If TypeOf node.Parent Is CSS.ExpressionStatementSyntax OrElse TypeOf node.Parent Is CSS.ForStatementSyntax Then
                    Return VBFactory.AssignmentStatement(ConvertCSExpressionsKindToVBKind(CS.CSharpExtensions.Kind(node)),
                                                            vbOperandExpression,
                                                            ExpressionKindToOperatorToken(kind),
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
                    Dim vbMathExpression As NameSyntax = VBFactory.ParseName("Math." & minMax)
                    Dim vbInterlockedExpressionName As NameSyntax = VBFactory.ParseName("System.Threading.Interlocked." & OperatorName)

                    Dim vbOperandArgument As SimpleArgumentSyntax = VBFactory.SimpleArgument(vbOperandExpression)

                    Dim vbOperandArgumentList As ArgumentListSyntax = VBFactory.ArgumentList(VBFactory.SingletonSeparatedList(Of ArgumentSyntax)(vbOperandArgument))
                    Dim vbArgumentInvocationExpression As InvocationExpressionSyntax = VBFactory.InvocationExpression(vbInterlockedExpressionName, vbOperandArgumentList)
                    Dim vbSecondArgumentSyntax As SimpleArgumentSyntax = VBFactory.SimpleArgument(VBFactory.BinaryExpression(op,
                                                                                                                                   vbOperandExpression,
                                                                                                                                   ExpressionKindToOperatorToken(op),
                                                                                                                                   ExpressionD1)
                                                                                                                                   )
                    Return VBFactory.InvocationExpression(
                        vbMathExpression,
                        VBFactory.ArgumentList(VBFactory.SeparatedList((New ArgumentSyntax() {VBFactory.SimpleArgument(vbArgumentInvocationExpression),
                                                                                                      vbSecondArgumentSyntax})))).WithConvertedTriviaFrom(node)
                End If
            End Function

            Public Overrides Function VisitPrefixUnaryExpression(node As CSS.PrefixUnaryExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim kind As VB.SyntaxKind = ConvertCSExpressionsKindToVBKind(CS.CSharpExtensions.Kind(node))
                If kind = CS.SyntaxKind.PointerIndirectionExpression Then
                    Dim StatementWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                    StatementWithIssues.AddMarker(FlagUnsupportedStatements(StatementWithIssues, "IndirectPointer Expressions", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=False)
                    Return NothingExpression
                End If
                If kind = CS.SyntaxKind.IndexExpression Then
                    Dim Operand As VB.VisualBasicSyntaxNode = node.Operand.Accept(Me)
                    Return VBFactory.ParseExpression($"-{Operand}")
                    Stop
                End If
                Dim vbOperandExpression As ExpressionSyntax = DirectCast(node.Operand.Accept(Me), ExpressionSyntax)
                If TypeOf node.Parent Is CSS.ExpressionStatementSyntax Then
                    Return VBFactory.AssignmentStatement(kind,
                                                             vbOperandExpression,
                                                             ExpressionKindToOperatorToken(kind),
                                                             ExpressionD1).WithConvertedTriviaFrom(node)
                End If
                If kind = VB.SyntaxKind.AddAssignmentStatement OrElse kind = VB.SyntaxKind.SubtractAssignmentStatement Then
                    If node.Parent.IsKind(CS.SyntaxKind.ForStatement) Then
                        If kind = VB.SyntaxKind.AddAssignmentStatement Then
                            Return VBFactory.AddAssignmentStatement(vbOperandExpression.WithTrailingTrivia(SpaceTrivia),
                                                                    ExpressionKindToOperatorToken(kind),
                                                                    ExpressionD1).WithConvertedTriviaFrom(node)
                        Else
                            Return VBFactory.SubtractAssignmentStatement(vbOperandExpression.WithTrailingTrivia(SpaceTrivia),
                                                                             ExpressionKindToOperatorToken(kind),
                                                                             ExpressionD1).WithConvertedTriviaFrom(node)
                        End If
                    Else
                        Dim operatorName As String = If(kind = VB.SyntaxKind.AddAssignmentStatement, "Increment", "Decrement")
                        Dim MathExpression As NameSyntax = VBFactory.ParseName("System.Threading.Interlocked." & operatorName)
                        Return VBFactory.InvocationExpression(MathExpression, VBFactory.ArgumentList(VBFactory.SeparatedList((New ArgumentSyntax() {VBFactory.SimpleArgument(vbOperandExpression)}))))
                    End If
                End If
                If kind = VB.SyntaxKind.AddressOfExpression Then
                    Dim SpaceTriviaList As SyntaxTriviaList
                    SpaceTriviaList = SpaceTriviaList.Add(SpaceTrivia)
                    Dim AddressOfToken As SyntaxToken = AddressOfKeyword.With(SpaceTriviaList, SpaceTriviaList)
                    Return VBFactory.AddressOfExpression(AddressOfToken, vbOperandExpression).WithConvertedTriviaFrom(node)
                End If
                Return VBFactory.UnaryExpression(kind,
                                                     ExpressionKindToOperatorToken(kind),
                                                     vbOperandExpression.WithLeadingTrivia(SpaceTrivia)
                                                     ).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitSimpleLambdaExpression(node As CSS.SimpleLambdaExpressionSyntax) As VB.VisualBasicSyntaxNode
                Return ConvertLambdaExpression(node, node.Body, VBFactory.SingletonSeparatedList(node.Parameter), VBFactory.TokenList(node.AsyncKeyword)).WithConvertedTriviaFrom(node)
            End Function

            ''' <summary>
            ''' Maps sizeof to Len(New {Type})
            ''' </summary>
            ''' <param name="node"></param>
            ''' <returns></returns>ThrowExpressionSyntax
            ''' <remarks>Added by PC</remarks>
            Public Overrides Function VisitSizeOfExpression(node As CSS.SizeOfExpressionSyntax) As VB.VisualBasicSyntaxNode
                Return VBFactory.ParseExpression($"Len(New {node.Type}()) ")
            End Function

            Public Overrides Function VisitThisExpression(node As CSS.ThisExpressionSyntax) As VB.VisualBasicSyntaxNode
                Return VBFactory.MeExpression().WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitThrowExpression(node As CSS.ThrowExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim Expression As ExpressionSyntax = DirectCast(node.Expression.Accept(Me), ExpressionSyntax)
                Dim ThrowStatement As ThrowStatementSyntax = VBFactory.ThrowStatement(Expression).WithTrailingEOL
                Dim ParentNode As SyntaxNode = node.Parent
                Return ThrowStatement.WithTrailingEOL
            End Function

            Public Overrides Function VisitTupleElement(node As CSS.TupleElementSyntax) As VB.VisualBasicSyntaxNode
                Try
                    If String.IsNullOrWhiteSpace(node.Identifier.ValueText) Then
                        Dim typedTupleElementSyntax1 As TypedTupleElementSyntax = VBFactory.TypedTupleElement(DirectCast(node.Type.Accept(Me), TypeSyntax))
                        Return typedTupleElementSyntax1
                    End If
                    Dim namedTupleElementSyntax1 As NamedTupleElementSyntax = VBFactory.NamedTupleElement(GenerateSafeVBToken(node.Identifier).WithConvertedTriviaFrom(node.Type), VBFactory.SimpleAsClause(AsKeyword.WithTrailingTrivia(SpaceTrivia), attributeLists:=New SyntaxList(Of AttributeListSyntax), DirectCast(node.Type.Accept(Me).WithConvertedTriviaFrom(node.Identifier), TypeSyntax)))
                    Return namedTupleElementSyntax1
                Catch ex As OperationCanceledException
                    Throw
                Catch ex As Exception
                    Stop
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
                        Dim initialTriviaList As List(Of SyntaxTrivia) = argument.GetLeadingTrivia.ToList
                        Dim finalLeadingTriviaList As New List(Of SyntaxTrivia)
                        For j As Integer = 0 To initialTriviaList.Count - 1
                            Dim Trivia As SyntaxTrivia = initialTriviaList(j)
                            Select Case Trivia.RawKind
                                Case VB.SyntaxKind.WhitespaceTrivia
                                    afterWhiteSpace = True
                                    finalLeadingTriviaList.Add(Trivia)
                                Case VB.SyntaxKind.EndOfLineTrivia
                                    finalLeadingTriviaList.Add(Trivia)
                                    afterWhiteSpace = False
                                    If j < initialTriviaList.Count - 1 Then
                                        If finalLeadingTriviaList.Count = 0 Then
                                            finalLeadingTriviaList.Add(SpaceTrivia)
                                            finalLeadingTriviaList.Add(LineContinuation)
                                        End If
                                    End If
                                Case VB.SyntaxKind.CommentTrivia
                                    If Not finalLeadingTriviaList.Last.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                                        If Not afterWhiteSpace Then
                                            finalLeadingTriviaList.Add(SpaceTrivia)
                                        End If
                                        finalLeadingTriviaList.Add(LineContinuation)
                                    End If

                                    finalLeadingTriviaList.Add(Trivia)
                                    If j < initialTriviaList.Count - 1 AndAlso Not initialTriviaList(j + 1).IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                        finalLeadingTriviaList.Add(VBEOLTrivia)
                                    End If
                                Case VB.SyntaxKind.DisableWarningDirectiveTrivia
                                    GetStatementwithIssues(node).AddMarker(VBFactory.EmptyStatement.WithLeadingTrivia(Trivia), StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                                Case VB.SyntaxKind.EnableWarningDirectiveTrivia
                                    GetStatementwithIssues(node).AddMarker(VBFactory.EmptyStatement.WithLeadingTrivia(Trivia), StatementHandlingOption.AppendEmptyStatement, AllowDuplicates:=True)
                                Case VB.SyntaxKind.LineContinuationTrivia
                                    If finalLeadingTriviaList.Last.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                                        Continue For
                                    End If
                                    afterWhiteSpace = False
                                    finalLeadingTriviaList.Add(LineContinuation)
                                Case Else
                                    Stop
                            End Select
                        Next
                        lArgumentSyntax.Add(argument.WithLeadingTrivia(finalLeadingTriviaList))
                    Next
                    Return VBFactory.TupleExpression(VBFactory.SeparatedList(lArgumentSyntax))
                End If
                For Each a As CSS.ArgumentSyntax In node.Arguments
                    Dim Identifier As IdentifierNameSyntax
                    If a.Expression.IsKind(CS.SyntaxKind.IdentifierName) Then
                        Identifier = DirectCast(DirectCast(a.Expression, CSS.IdentifierNameSyntax).Accept(Me), IdentifierNameSyntax)
                    Else
                        Dim d As CSS.DeclarationExpressionSyntax = DirectCast(a.Expression, CSS.DeclarationExpressionSyntax)
                        Identifier = DirectCast(d.Designation.Accept(Me), IdentifierNameSyntax)
                    End If
                    lArgumentSyntax.Add(VBFactory.SimpleArgument(Identifier))
                Next
                Return VBFactory.TupleExpression(VBFactory.SeparatedList(lArgumentSyntax))
            End Function

            Public Overrides Function VisitTupleType(node As CSS.TupleTypeSyntax) As VB.VisualBasicSyntaxNode
                Dim tupleElementList As New List(Of TupleElementSyntax)
                tupleElementList.AddRange(node.Elements.Select(Function(a As CSS.TupleElementSyntax) DirectCast(a.Accept(Me), TupleElementSyntax)))
                Return VBFactory.TupleType(tupleElementList.ToArray)
            End Function

            Public Overrides Function VisitTypeOfExpression(node As CSS.TypeOfExpressionSyntax) As VB.VisualBasicSyntaxNode
                If TypeOf node.Type Is CSS.GenericNameSyntax Then
                    Dim NodeType As CSS.GenericNameSyntax = DirectCast(node.Type, CSS.GenericNameSyntax)
                    Dim ArgumentList As CSS.TypeArgumentListSyntax = NodeType.TypeArgumentList
                    If ArgumentList.Arguments.Count = 1 AndAlso ArgumentList.Arguments(0).IsKind(CS.SyntaxKind.OmittedTypeArgument) Then
                        Return VBFactory.GetTypeExpression(VBFactory.ParseTypeName($"{NodeType.Identifier.ValueText}()"))
                    End If
                End If
                Dim VBSyntaxNode As VB.VisualBasicSyntaxNode = node.Type.Accept(Me)
                If VBSyntaxNode.IsKind(VB.SyntaxKind.AddressOfExpression) Then
                    VBSyntaxNode = DirectCast(VBSyntaxNode, UnaryExpressionSyntax).Operand
                End If
                Return VBFactory.GetTypeExpression(DirectCast(VBSyntaxNode, TypeSyntax))
            End Function

        End Class

    End Class

End Namespace
