' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

Imports System.Runtime.InteropServices

Imports CSharpToVBCodeConverter.Util

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Simplification
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

Imports VBMsgBox

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory

Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBCodeConverter.Visual_Basic

    Partial Public Class CSharpConverter

        Partial Protected Friend Class NodesVisitor
            Inherits CS.CSharpSyntaxVisitor(Of VB.VisualBasicSyntaxNode)
            Private Const IDictionary As String = "IDictionary"
            Private Const IEnumerable As String = "IEnumerable"
            Private Const IEnumerableOf As String = "IEnumerable(Of "

            Private Function ConvertLambdaExpression(node As CSS.AnonymousFunctionExpressionSyntax, block As Object, parameters As SeparatedSyntaxList(Of CSS.ParameterSyntax), CS_Modifiers As SyntaxTokenList) As LambdaExpressionSyntax
                Dim NodesList As New List(Of ParameterSyntax)
                Dim Separators As New List(Of SyntaxToken)
                If parameters.Count > 0 Then
                    Dim SeparatorCount As Integer = parameters.SeparatorCount
                    Dim CS_Separators As New List(Of SyntaxToken)
                    CS_Separators.AddRange(parameters.GetSeparators)

                    For i As Integer = 0 To SeparatorCount - 1
                        Dim p As ParameterSyntax = DirectCast(parameters(i).Accept(Me), ParameterSyntax)
                        NodesList.Add(p)
                        Separators.Add(CommaToken.WithConvertedTriviaFrom(CS_Separators(i)))
                    Next
                    NodesList.Add(DirectCast(parameters.Last.Accept(Me), ParameterSyntax))
                End If

                Dim header As LambdaHeaderSyntax
                Dim symbol As IMethodSymbol = TryCast(ModelExtensions.GetSymbolInfo(mSemanticModel, node).Symbol, IMethodSymbol)
                Dim Modifiers As List(Of SyntaxToken) = ConvertModifiers(CS_Modifiers, IsModule, TokenContext.Local)
                Dim EndSubOrFunctionStatement As EndBlockStatementSyntax
                Dim parameterList As ParameterListSyntax = VBFactory.ParameterList(VBFactory.SeparatedList(NodesList, Separators))
                Dim IsFunction As Boolean = Not (symbol.ReturnsVoid OrElse TypeOf node.Body Is CSS.AssignmentExpressionSyntax)
                Dim Braces As (SyntaxToken, SyntaxToken) = node.Body.GetBraces
                If IsFunction Then
                    Dim AddAsClause As Boolean = symbol.ReturnType.IsErrorType OrElse symbol.ReturnType.ToString.Contains("?", StringComparison.InvariantCulture)
                    Dim AsClause As SimpleAsClauseSyntax = If(AddAsClause,
                                                                  Nothing,
                                                                  VBFactory.SimpleAsClause(ConvertToType(symbol.ReturnType))
                                                                  )
                    header = VBFactory.FunctionLambdaHeader(VBFactory.List(Of AttributeListSyntax)(), VBFactory.TokenList(Modifiers), parameterList, asClause:=AsClause)
                    EndSubOrFunctionStatement = VBFactory.EndFunctionStatement().WithConvertedTriviaFrom(Braces.Item2)
                Else
                    header = VBFactory.SubLambdaHeader(VBFactory.List(Of AttributeListSyntax)(), VBFactory.TokenList(Modifiers), parameterList, asClause:=Nothing)
                    EndSubOrFunctionStatement = VBFactory.EndSubStatement().WithConvertedTriviaFrom(Braces.Item2)
                End If
                If TypeOf block Is CSS.BlockSyntax Then
                    block = DirectCast(block, CSS.BlockSyntax).Statements
                End If

                Dim Statements As New SyntaxList(Of StatementSyntax)
                Dim EndBlock As EndBlockStatementSyntax
                If TypeOf block Is CS.CSharpSyntaxNode Then
                    Dim body As VB.VisualBasicSyntaxNode = DirectCast(block, CS.CSharpSyntaxNode).Accept(Me)
                    If TypeOf block Is CSS.ThrowExpressionSyntax Then
                        Statements = VBFactory.SingletonList(DirectCast(body, StatementSyntax).WithTrailingEOL)
                        If IsFunction Then
                            Return VBFactory.MultiLineLambdaExpression(VB.SyntaxKind.MultiLineFunctionLambdaExpression, header.WithTrailingEOL, Statements, EndSubOrFunctionStatement).WithConvertedTriviaFrom(node)
                        End If
                        Return VBFactory.MultiLineLambdaExpression(VB.SyntaxKind.MultiLineSubLambdaExpression, header.WithTrailingEOL, Statements, EndSubOrFunctionStatement).WithConvertedTriviaFrom(node)
                    End If
                    If TypeOf block Is CSS.ObjectCreationExpressionSyntax Then
                        If IsFunction Then
                            Statements = VBFactory.SingletonList(Of StatementSyntax)(VBFactory.ReturnStatement(DirectCast(body, NewExpressionSyntax)).WithTrailingEOL)
                            Return VBFactory.MultiLineLambdaExpression(VB.SyntaxKind.MultiLineFunctionLambdaExpression, header.WithTrailingEOL, Statements, EndSubOrFunctionStatement).WithConvertedTriviaFrom(node)
                        End If
                        Dim Names As SeparatedSyntaxList(Of ModifiedIdentifierSyntax) = VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier("DoNotCare"))
                        Dim AsClause As AsClauseSyntax = VBFactory.AsNewClause(DirectCast(body, NewExpressionSyntax))
                        Dim Declarators As SeparatedSyntaxList(Of VariableDeclaratorSyntax) = VBFactory.SingletonSeparatedList(VBFactory.VariableDeclarator(Names, AsClause, initializer:=Nothing))
                        Statements = VBFactory.SingletonList(Of StatementSyntax)(VBFactory.LocalDeclarationStatement(DimModifier, Declarators))
                        Return VBFactory.MultiLineLambdaExpression(VB.SyntaxKind.MultiLineSubLambdaExpression, header.WithTrailingEOL, Statements, EndSubOrFunctionStatement).WithConvertedTriviaFrom(node)
                    End If
                    If body.IsKind(VB.SyntaxKind.SimpleAssignmentStatement) Then
                        Dim SimpleAssignment As AssignmentStatementSyntax = DirectCast(body, AssignmentStatementSyntax)
                        If SimpleAssignment.Left.IsKind(VB.SyntaxKind.SimpleMemberAccessExpression) Then
                            Dim MemberAccessExpression As MemberAccessExpressionSyntax = DirectCast(SimpleAssignment.Left, MemberAccessExpressionSyntax)
                            Select Case MemberAccessExpression.Expression.Kind
                                Case VB.SyntaxKind.ObjectCreationExpression, VB.SyntaxKind.SimpleMemberAccessExpression
                                    EndBlock = VBFactory.EndBlockStatement(VB.SyntaxKind.EndSubStatement, SubKeyword)
                                    Dim UniqueName As String = MethodBodyVisitor.GetUniqueVariableNameInScope(node, "tempVar", mSemanticModel)
                                    Dim UniqueIdentifier As IdentifierNameSyntax = VBFactory.IdentifierName(VBFactory.Identifier(UniqueName))
                                    Dim Names As SeparatedSyntaxList(Of ModifiedIdentifierSyntax) = VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(UniqueName))
                                    Dim DimStatement As LocalDeclarationStatementSyntax

                                    If TypeOf MemberAccessExpression.Expression Is NewExpressionSyntax Then
                                        Dim AsClause As AsClauseSyntax = VBFactory.AsNewClause(DirectCast(MemberAccessExpression.Expression, NewExpressionSyntax))
                                        Dim VariableDeclaration As SeparatedSyntaxList(Of VariableDeclaratorSyntax) = VBFactory.SingletonSeparatedList(VBFactory.VariableDeclarator(Names, AsClause, initializer:=Nothing))
                                        DimStatement = VBFactory.LocalDeclarationStatement(DimModifier, VariableDeclaration)
                                    ElseIf TypeOf MemberAccessExpression.Expression Is MemberAccessExpressionSyntax Then
                                        Dim MemberAccess As MemberAccessExpressionSyntax = DirectCast(MemberAccessExpression.Expression, MemberAccessExpressionSyntax)
                                        If TypeOf MemberAccess.Expression IsNot NewExpressionSyntax Then
                                            Exit Select
                                        End If
                                        Dim AsClause As AsClauseSyntax = VBFactory.AsNewClause(DirectCast(MemberAccess.Expression, NewExpressionSyntax))
                                        Dim VariableDeclaration As SeparatedSyntaxList(Of VariableDeclaratorSyntax) = VBFactory.SingletonSeparatedList(VBFactory.VariableDeclarator(Names, AsClause, initializer:=Nothing))
                                        DimStatement = VBFactory.LocalDeclarationStatement(DimModifier, VariableDeclaration)
                                    Else
                                        Exit Select
                                    End If
                                    Statements = Statements.Add(DimStatement)
                                    Statements = Statements.Add(VBFactory.SimpleAssignmentStatement(VBFactory.QualifiedName(UniqueIdentifier, MemberAccessExpression.Name), SimpleAssignment.Right).WithTrailingEOL)
                                    Return VBFactory.MultiLineLambdaExpression(VB.SyntaxKind.MultiLineSubLambdaExpression, header.WithTrailingEOL, Statements, EndBlock).WithConvertedTriviaFrom(node)
                                Case VB.SyntaxKind.IdentifierName, VB.SyntaxKind.InvocationExpression, VB.SyntaxKind.MeExpression
                                    ' handled below
                                Case Else
                                    Stop
                            End Select
                        End If
                    End If
                    If IsFunction Then
                        Return VBFactory.SingleLineLambdaExpression(VB.SyntaxKind.SingleLineFunctionLambdaExpression, header.WithAsClause(Nothing), body).WithConvertedTriviaFrom(node)
                    End If
                    Return VBFactory.SingleLineLambdaExpression(VB.SyntaxKind.SingleLineSubLambdaExpression, header.WithAsClause(Nothing), body).WithConvertedTriviaFrom(node)
                End If

                ' TypeOf block Is SyntaxList(Of CSS.StatementSyntax)
                Statements = Statements.AddRange(VBFactory.List(DirectCast(block, SyntaxList(Of CSS.StatementSyntax)).SelectMany(Function(s As CSS.StatementSyntax) s.Accept(New MethodBodyVisitor(mSemanticModel, Me)))))
                Dim expression As ExpressionSyntax = Nothing
                If Statements.Count = 1 AndAlso UnpackExpressionFromStatement(Statements(0), expression) Then
                    Dim lSyntaxKind As VB.SyntaxKind = If(IsFunction, VB.SyntaxKind.SingleLineFunctionLambdaExpression, VB.SyntaxKind.SingleLineSubLambdaExpression)
                    Return VBFactory.SingleLineLambdaExpression(lSyntaxKind, header.WithAsClause(Nothing), expression).WithConvertedTriviaFrom(node).WithPrependedLeadingTrivia(Statements(0).GetLeadingTrivia)
                End If

                Dim ExpressionKind As VB.SyntaxKind
                If IsFunction Then
                    EndBlock = VBFactory.EndBlockStatement(VB.SyntaxKind.EndFunctionStatement, FunctionKeyword).WithConvertedTriviaFrom(Braces.Item2)
                    ExpressionKind = VB.SyntaxKind.MultiLineFunctionLambdaExpression
                Else
                    EndBlock = VBFactory.EndBlockStatement(VB.SyntaxKind.EndSubStatement, SubKeyword).WithConvertedTriviaFrom(Braces.Item2)
                    ExpressionKind = VB.SyntaxKind.MultiLineSubLambdaExpression
                End If
                Return VBFactory.MultiLineLambdaExpression(kind:=ExpressionKind,
                                                               header.WithTrailingEOL,
                                                               Statements,
                                                               EndBlock)
            End Function

            Private Function ConvertToInterpolatedStringTextToken(CSharpToken As SyntaxToken) As SyntaxToken
                Dim TokenString As String = ConvertCSharpEscapes(CSharpToken.ValueText)
                Return VBFactory.InterpolatedStringTextToken(TokenString, TokenString)
            End Function

            Private Shared Function CovertStringToTupleType(TupleString As String) As TypeSyntax
                Dim TupleElements As New List(Of TupleElementSyntax)
                For Each t As String In TupleString.Substring(1, TupleString.Length - 2).Split(","c)
                    Dim TuplePart() As String = t.Trim.Split(" "c)
                    If TuplePart.Count = 1 Then
                        Dim typedTupleElementSyntax1 As TypedTupleElementSyntax = VBFactory.TypedTupleElement(ConvertToType(TuplePart(0)))
                        TupleElements.Add(typedTupleElementSyntax1)
                    Else
                        Dim Identifier As SyntaxToken = CS.SyntaxFactory.Identifier(TuplePart(1))
                        Dim namedTupleElementSyntax1 As NamedTupleElementSyntax = VBFactory.NamedTupleElement(GenerateSafeVBToken(Identifier, IsQualifiedName:=False, IsTypeName:=False), VBFactory.SimpleAsClause(ConvertToType(TuplePart(0))))
                        TupleElements.Add(namedTupleElementSyntax1)
                    End If
                Next
                Return VBFactory.TupleType(TupleElements.ToArray)
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
                    LeftNodeTypeInfo = ModelExtensions.GetTypeInfo(mSemanticModel, node.Left)
                    RightNodeTypeInfo = ModelExtensions.GetTypeInfo(mSemanticModel, node.Right)
                Catch ex As Exception
                    ' ignore
                End Try
                Dim LeftAndRightIsString As Boolean? = LeftNodeTypeInfo.ConvertedType?.SpecialType = System_String OrElse RightNodeTypeInfo.ConvertedType?.SpecialType = System_String
                Return LeftAndRightIsString.HasValue AndAlso LeftAndRightIsString.Value
            End Function

            Private Function IsStringExpression(Node As SyntaxNode) As Boolean
                If Node.IsKind(CS.SyntaxKind.StringLiteralExpression, CS.SyntaxKind.CharacterLiteralExpression, CS.SyntaxKind.InterpolatedStringExpression) Then
                    Return True
                End If
                ' Extra to pick up more strings
                If Node.ToString.
                        Replace("(", "", StringComparison.InvariantCulture).
                        Replace(")", "", StringComparison.InvariantCulture).
                        EndsWith("tostring", StringComparison.InvariantCultureIgnoreCase) Then
                    Return True
                End If
                Dim _Typeinfo As TypeInfo = ModelExtensions.GetTypeInfo(mSemanticModel, Node)
                If _Typeinfo.Type?.ToString = "string" Then
                    Return True
                End If
                ' Need to check Type to see if we have any kind of string
                Return False
            End Function

            Private Function MakeAssignmentStatement(node As CSS.AssignmentExpressionSyntax) As StatementSyntax
                Dim LeftNode As ExpressionSyntax = DirectCast(node.Left.Accept(Me), ExpressionSyntax)
                If CS.CSharpExtensions.Kind(node) = CS.SyntaxKind.CoalesceAssignmentExpression Then
                    Dim PossibleNullNode As ExpressionSyntax = DirectCast(node.Right.Accept(Me).WithLeadingTrivia(SpaceTrivia), ExpressionSyntax)
                    Dim IsNothingCondition As ExpressionSyntax = VBFactory.IsExpression(LeftNode, NothingExpression).With({SpaceTrivia}, {SpaceTrivia})
                    Dim AssignmentStatement As AssignmentStatementSyntax = VBFactory.SimpleAssignmentStatement(LeftNode, PossibleNullNode)
                    Dim LeadingTrivia As New List(Of SyntaxTrivia)
                    If AssignmentStatement.HasLeadingTrivia Then
                        LeadingTrivia.AddRange(AssignmentStatement.GetLeadingTrivia)
                        AssignmentStatement = AssignmentStatement.WithLeadingTrivia(SpaceTrivia)
                    End If
                    Dim AssignmentStatements As SyntaxList(Of StatementSyntax) = VBFactory.SingletonList(Of StatementSyntax)(AssignmentStatement)
                    Return VBFactory.SingleLineIfStatement(IsNothingCondition, AssignmentStatements, elseClause:=Nothing).With(LeadingTrivia, ConvertTrivia(node.GetTrailingTrivia))
                End If
                Dim kind As VB.SyntaxKind = ConvertCSExpressionsKindToVBKind(CS.CSharpExtensions.Kind(node))
                Dim OperatorToken As SyntaxToken = ExpressionKindToOperatorToken(kind)
                Dim RightNode As ExpressionSyntax
                If node.Right.IsKind(CS.SyntaxKind.CoalesceExpression) Then
                    Dim CS_Right As CSS.BinaryExpressionSyntax = DirectCast(node.Right, CSS.BinaryExpressionSyntax)
                    If CS_Right.Right.IsKind(CS.SyntaxKind.ThrowExpression) Then
                        Dim TestNode As ExpressionSyntax = DirectCast(CS_Right.Left.Accept(Me).WithLeadingTrivia(SpaceTrivia), ExpressionSyntax)
                        Dim SecondExpression As ThrowStatementSyntax = DirectCast(CS_Right.Right.Accept(Me).WithConvertedTriviaFrom(CS_Right.Right), ThrowStatementSyntax).WithTrailingEOL
                        Dim Statements As SyntaxList(Of StatementSyntax) = VBFactory.SingletonList(Of StatementSyntax)(SecondExpression)

                        Dim Condition As ExpressionSyntax = VBFactory.IsExpression(TestNode, NothingExpression)
                        Dim IfBlock As SingleLineIfStatementSyntax = VBFactory.SingleLineIfStatement(Condition,
                                                                                                          Statements,
                                                                                                          elseClause:=Nothing)
                        Dim StatementWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                        StatementWithIssues.AddMarker(IfBlock, StatementHandlingOption.PrependStatement, AllowDuplicates:=False)
                        RightNode = DirectCast(CS_Right.Left.Accept(Me), ExpressionSyntax)
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
                inlineAssignHelperMarkers.Add(parentDefinition)
            End Sub

            Private Function ReduceArrayUpperBoundExpression(expr As CSS.ExpressionSyntax) As ExpressionSyntax
                Dim constant As [Optional](Of Object) = mSemanticModel.GetConstantValue(expr)
                If constant.HasValue AndAlso TypeOf constant.Value Is Integer Then
                    Return VBFactory.NumericLiteralExpression(VBFactory.Literal(CInt(constant.Value) - 1))
                End If
                Return VBFactory.BinaryExpression(kind:=VB.SyntaxKind.SubtractExpression, left:=DirectCast(expr.Accept(Me), ExpressionSyntax), operatorToken:=MinusToken, right:=VBFactory.NumericLiteralExpression(VBFactory.Literal(1)))
            End Function

            Private Function RestructureTrivia(TriviaList As SyntaxTriviaList, FoundEOL As Boolean, ByRef OperatorTrailingTrivia As List(Of SyntaxTrivia)) As Boolean
                For Each Trivia As SyntaxTrivia In TriviaList
                    Select Case Trivia.RawKind
                        Case VB.SyntaxKind.CommentTrivia
                            OperatorTrailingTrivia.Add(Trivia)
                            FoundEOL = True
                        Case VB.SyntaxKind.EndOfLineTrivia
                            FoundEOL = True
                        Case VB.SyntaxKind.WhitespaceTrivia
                            OperatorTrailingTrivia.Add(SpaceTrivia)
                        Case VB.SyntaxKind.DisableWarningDirectiveTrivia, VB.SyntaxKind.EnableWarningDirectiveTrivia
                            ' Ignore
                        Case Else
                            Stop
                    End Select
                Next

                Return FoundEOL
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Private Shared Function UnpackExpressionFromStatement(statementSyntax As StatementSyntax, <Out> ByRef expression As ExpressionSyntax) As Boolean
                If TypeOf statementSyntax Is ReturnStatementSyntax Then
                    expression = (DirectCast(statementSyntax, ReturnStatementSyntax)).Expression
                ElseIf TypeOf statementSyntax Is YieldStatementSyntax Then
                    expression = (DirectCast(statementSyntax, YieldStatementSyntax)).Expression
                Else
                    expression = Nothing
                End If

                Return expression IsNot Nothing
            End Function

            Public Shared Function ConvertAndModifyNodeTrivia(Node As VB.VisualBasicSyntaxNode, NodesOrTokens As List(Of SyntaxNodeOrToken), Index As Integer) As VB.VisualBasicSyntaxNode
                If NodesOrTokens Is Nothing Then
                    Throw New ArgumentNullException(NameOf(NodesOrTokens))
                End If
                Dim AfterWhiteSpace As Boolean = False
                Dim AfterLineContinuation As Boolean = False
                Dim InitialTriviaList As List(Of SyntaxTrivia) = ConvertTrivia(NodesOrTokens(Index).GetLeadingTrivia).ToList
                Dim InitialTriviaListUBound As Integer = InitialTriviaList.Count - 1
                Dim FirstTrivia As Boolean = True
                Dim FinalLeadingTriviaList As New List(Of SyntaxTrivia)
                For i As Integer = 0 To InitialTriviaListUBound
                    Dim Trivia As SyntaxTrivia = InitialTriviaList(i)
                    Select Case Trivia.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            AfterLineContinuation = False
                            AfterWhiteSpace = True
                            FirstTrivia = False
                            FinalLeadingTriviaList.Add(Trivia)
                        Case VB.SyntaxKind.EndOfLineTrivia
                            ' we want to skip any leading trivia
                            If Not FirstTrivia Then
                                FinalLeadingTriviaList.Add(Trivia)
                                AfterLineContinuation = False
                                AfterWhiteSpace = False
                                If Index < NodesOrTokens.Count - 1 Then
                                    If FinalLeadingTriviaList.Count = 0 Then
                                        FinalLeadingTriviaList.Add(SpaceTrivia)
                                        FinalLeadingTriviaList.Add(LineContinuation)
                                    End If
                                End If
                            End If
                        Case VB.SyntaxKind.CommentTrivia
                            FirstTrivia = False
                            If Not AfterWhiteSpace Then
                                FinalLeadingTriviaList.Add(SpaceTrivia)
                            End If
                            FinalLeadingTriviaList.Add(LineContinuation)
                            FinalLeadingTriviaList.Add(Trivia)
                            If i < InitialTriviaListUBound AndAlso Not InitialTriviaList(i + 1).IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                FinalLeadingTriviaList.Add(VB_EOLTrivia)
                            End If
                        Case VB.SyntaxKind.DisableWarningDirectiveTrivia, VB.SyntaxKind.EnableWarningDirectiveTrivia
                            FirstTrivia = False
                            Stop
                            'GetStatementwithIssues(CS_Node).AddMarker(VBFactory.EmptyStatement.WithLeadingTrivia(Trivia), StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                        Case VB.SyntaxKind.IfDirectiveTrivia
                            FirstTrivia = False
                            FinalLeadingTriviaList.AddRange(DirectiveNotAllowedHere(Trivia))
                        Case VB.SyntaxKind.DisabledTextTrivia
                            FirstTrivia = False
                            FinalLeadingTriviaList.AddRange(DirectiveNotAllowedHere(Trivia))
                        Case VB.SyntaxKind.ElseDirectiveTrivia
                            FirstTrivia = False
                            FinalLeadingTriviaList.AddRange(DirectiveNotAllowedHere(Trivia))
                        Case VB.SyntaxKind.EndIfDirectiveTrivia
                            FirstTrivia = False
                            FinalLeadingTriviaList.AddRange(DirectiveNotAllowedHere(Trivia))
                        Case Else
                            Stop
                    End Select
                Next
                InitialTriviaList.Clear()
                InitialTriviaList.AddRange(ConvertTrivia(NodesOrTokens(Index).GetTrailingTrivia))
                InitialTriviaListUBound = InitialTriviaList.Count - 1

                Dim FinalTrailingTriviaList As New List(Of SyntaxTrivia)
                For i As Integer = 0 To InitialTriviaListUBound
                    Dim Trivia As SyntaxTrivia = InitialTriviaList(i)
                    Select Case Trivia.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            FinalTrailingTriviaList.Add(Trivia)
                        Case VB.SyntaxKind.EndOfLineTrivia
                            ' What to do depends on whats next
                            If Index < NodesOrTokens.Count - 1 Then
                                Dim j As Integer
                                Dim NewWhiteSpaceString As String = ""
                                If i < InitialTriviaListUBound Then
                                    For j = i + 1 To InitialTriviaListUBound
                                        If InitialTriviaList(j).RawKind = VB.SyntaxKind.WhitespaceTrivia Then
                                            NewWhiteSpaceString &= InitialTriviaList(j).ToString
                                            i += 1
                                        Else
                                            Exit For
                                        End If
                                    Next
                                End If
                                If j = 0 OrElse j < InitialTriviaListUBound AndAlso InitialTriviaList(j).RawKind = VB.SyntaxKind.CommentTrivia Then
                                    If String.IsNullOrWhiteSpace(NewWhiteSpaceString) Then
                                        FinalTrailingTriviaList.Add(SpaceTrivia)
                                    Else
                                        FinalTrailingTriviaList.Add(VBFactory.WhitespaceTrivia(NewWhiteSpaceString))
                                    End If
                                    FinalTrailingTriviaList.Add(LineContinuation)
                                    FinalTrailingTriviaList.Add(Trivia)
                                    AfterLineContinuation = True
                                Else
                                    FinalTrailingTriviaList.Add(Trivia)
                                    If Not String.IsNullOrWhiteSpace(NewWhiteSpaceString) Then
                                        FinalTrailingTriviaList.Add(VBFactory.WhitespaceTrivia(NewWhiteSpaceString))
                                    End If
                                End If
                            Else
                                FinalTrailingTriviaList.Add(Trivia)
                                AfterLineContinuation = False
                                AfterWhiteSpace = False
                            End If
                        Case VB.SyntaxKind.CommentTrivia
                            If Not AfterWhiteSpace Then
                                FinalTrailingTriviaList.Add(SpaceTrivia)
                            End If
                            If Not AfterLineContinuation Then
                                FinalTrailingTriviaList.Add(LineContinuation)
                                FinalTrailingTriviaList.Add(SpaceTrivia)
                            End If
                            FinalTrailingTriviaList.Add(Trivia)
                            AfterLineContinuation = False
                            AfterWhiteSpace = False
                        Case Else
                            Stop
                    End Select
                Next
                If Node Is Nothing Then
                    Throw New ArgumentNullException(NameOf(Node))
                End If
                Return Node.With(FinalLeadingTriviaList, FinalTrailingTriviaList)
            End Function

            Public Shared Function ConvertAndModifyTokenTrivia(Token As SyntaxToken, NodesOrTokens As List(Of SyntaxNodeOrToken), Index As Integer) As SyntaxToken
                If NodesOrTokens Is Nothing Then
                    Throw New ArgumentNullException(NameOf(NodesOrTokens))
                End If
                Dim InitialTriviaList As List(Of SyntaxTrivia) = ConvertTrivia(NodesOrTokens(Index).GetLeadingTrivia).ToList
                Dim InitialTriviaListUBound As Integer = InitialTriviaList.Count - 1
                Dim AfterWhiteSpace As Boolean = False
                Dim AfterLineContinuation As Boolean = False
                Dim FinalLeadingTriviaList As New List(Of SyntaxTrivia)
                For i As Integer = 0 To InitialTriviaListUBound
                    Dim Trivia As SyntaxTrivia = InitialTriviaList(i)
                    Select Case Trivia.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            AfterLineContinuation = False
                            AfterWhiteSpace = True
                            FinalLeadingTriviaList.Add(Trivia)
                        Case VB.SyntaxKind.EndOfLineTrivia
                            FinalLeadingTriviaList.Add(Trivia)
                            AfterLineContinuation = False
                            AfterWhiteSpace = False
                            ' What I do depends on whats next
                            If i < InitialTriviaListUBound Then
                                Dim j As Integer
                                Dim NewWhiteSpaceString As String = ""
                                For j = i + 1 To InitialTriviaListUBound
                                    If InitialTriviaList(j).RawKind = VB.SyntaxKind.WhitespaceTrivia Then
                                        NewWhiteSpaceString &= InitialTriviaList(j).ToString
                                        i += 1
                                    Else
                                        Exit For
                                    End If
                                Next
                                If j < InitialTriviaListUBound AndAlso InitialTriviaList(j).RawKind = VB.SyntaxKind.CommentTrivia Then
                                    If String.IsNullOrWhiteSpace(NewWhiteSpaceString) Then
                                        FinalLeadingTriviaList.Add(SpaceTrivia)
                                    Else
                                        FinalLeadingTriviaList.Add(VBFactory.WhitespaceTrivia(NewWhiteSpaceString))
                                    End If
                                    FinalLeadingTriviaList.Add(LineContinuation)
                                    AfterLineContinuation = True
                                Else
                                    If Not String.IsNullOrWhiteSpace(NewWhiteSpaceString) Then
                                        FinalLeadingTriviaList.Add(VBFactory.WhitespaceTrivia(NewWhiteSpaceString))
                                    End If
                                End If
                            End If
                        Case VB.SyntaxKind.CommentTrivia
                            If Not AfterWhiteSpace Then
                                FinalLeadingTriviaList.Add(SpaceTrivia)
                            End If
                            If Not AfterLineContinuation Then
                                FinalLeadingTriviaList.Add(LineContinuation)
                                FinalLeadingTriviaList.Add(SpaceTrivia)
                            End If
                            FinalLeadingTriviaList.Add(Trivia)
                            AfterLineContinuation = False
                            AfterWhiteSpace = False
                        Case VB.SyntaxKind.EndIfDirectiveTrivia
                            FinalLeadingTriviaList.AddRange(DirectiveNotAllowedHere(Trivia))
                            FinalLeadingTriviaList.Add(VB_EOLTrivia)
                            AfterLineContinuation = False
                            AfterWhiteSpace = False
                        Case Else
                            Stop
                    End Select
                Next
                InitialTriviaList.Clear()
                InitialTriviaList.AddRange(ConvertTrivia(NodesOrTokens(Index).GetTrailingTrivia))
                InitialTriviaListUBound = InitialTriviaList.Count - 1

                Dim FinalTrailingTriviaList As New List(Of SyntaxTrivia)
                For i As Integer = 0 To InitialTriviaListUBound
                    Dim Trivia As SyntaxTrivia = InitialTriviaList(i)
                    Select Case Trivia.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            FinalTrailingTriviaList.Add(Trivia)
                            AfterWhiteSpace = True
                        Case VB.SyntaxKind.EndOfLineTrivia
                            FinalTrailingTriviaList.Add(Trivia)
                            AfterWhiteSpace = False
                        Case VB.SyntaxKind.CommentTrivia
                            If Not AfterWhiteSpace = True Then
                                FinalTrailingTriviaList.Add(SpaceTrivia)
                            End If
                            FinalTrailingTriviaList.Add(LineContinuation)
                            FinalTrailingTriviaList.Add(Trivia)
                            AfterWhiteSpace = False
                        Case Else
                            Stop
                    End Select
                Next
                Return Token.With(FinalLeadingTriviaList, FinalTrailingTriviaList)
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="TupleElement can't Be Nothing")>
            Public Shared Function ConvertToTupleElement(TupleElement As IFieldSymbol) As TupleElementSyntax
                If TupleElement.Type Is Nothing Then
                    Return VBFactory.NamedTupleElement(TupleElement.Name.ToString(Globalization.CultureInfo.InvariantCulture))
                End If
                Dim AsClause As SimpleAsClauseSyntax = VBFactory.SimpleAsClause(ConvertToType(TupleElement.Type))
                Return VBFactory.NamedTupleElement(VBFactory.Identifier(AddBracketsIfRequired(TupleElement.Name)), AsClause)
            End Function

            Public Shared Function ConvertToType(PossibleTupleType As ITypeSymbol) As TypeSyntax
                If PossibleTupleType Is Nothing Then
                    Throw New ArgumentNullException(NameOf(PossibleTupleType))
                End If
                If PossibleTupleType.IsKind(SymbolKind.ArrayType) Then
                    Dim ElementType As TypeSyntax = ConvertToType(DirectCast(PossibleTupleType, IArrayTypeSymbol).ElementType)
                    If TypeOf ElementType Is ArrayTypeSyntax Then
                        Return ElementType
                    End If
                    Return VBFactory.ArrayType(ElementType)
                End If
                If PossibleTupleType.IsTupleType Then
                    Dim TupleElementList As New List(Of TupleElementSyntax)
                    For Each TupleElement As IFieldSymbol In DirectCast(PossibleTupleType, INamedTypeSymbol).TupleElements
                        TupleElementList.Add(ConvertToTupleElement(TupleElement))
                    Next
                    Return VBFactory.TupleType(TupleElementList.ToArray)
                End If
                If PossibleTupleType.Name = "Tuple" Then
                    Dim TupleElementList As New List(Of TypeSyntax)
                    For Each TupleElement As ITypeSymbol In DirectCast(PossibleTupleType, INamedTypeSymbol).TypeArguments
                        TupleElementList.Add(ConvertToType(TupleElement))
                    Next
                    Return VBFactory.GenericName("Tuple", VBFactory.TypeArgumentList(VBFactory.SeparatedList(TupleElementList)))
                End If
                Dim PossibleName As String = PossibleTupleType.ToString.Trim
                Dim StartIndex As Integer = PossibleName.IndexOf("<", StringComparison.InvariantCulture)
                If StartIndex > 0 Then
                    Dim IndexOfLastGreaterThan As Integer = PossibleName.LastIndexOf(">", StringComparison.InvariantCulture)
                    Dim Name As String = PossibleName.Substring(0, StartIndex)
                    Dim PossibleTypes As String = PossibleName.Substring(StartIndex + 1, IndexOfLastGreaterThan - StartIndex - 1)
                    If PossibleTupleType.ToString.StartsWith("System.Func", StringComparison.InvariantCulture) Then
                        Dim DictionaryTypeElement As New List(Of TypeSyntax)
                        While PossibleTypes.Length > 0
                            Dim EndIndex As Integer
                            ' Tuple
                            If PossibleTypes.StartsWith("(", StringComparison.InvariantCulture) Then
                                ' Tuple
                                EndIndex = PossibleTypes.LastIndexOf(")", StringComparison.InvariantCulture)
                                DictionaryTypeElement.Add(CovertStringToTupleType(PossibleTypes.Substring(0, EndIndex + 1).Trim))
                                EndIndex += 1
                            Else
                                ' Type
                                EndIndex = PossibleTypes.IndexOf(",", StringComparison.InvariantCulture)
                                Dim FirstLessThan As Integer = PossibleTypes.IndexOf("<", StringComparison.InvariantCulture)
                                EndIndex = If(EndIndex = -1 OrElse (FirstLessThan <> -1 AndAlso FirstLessThan < EndIndex), PossibleTypes.Length, EndIndex)
                                DictionaryTypeElement.Add(ConvertToType(PossibleTypes.Substring(0, EndIndex) _
                                                                                     .Replace("<", "(Of ", StringComparison.InvariantCulture) _
                                                                                     .Replace(">", ")", StringComparison.InvariantCulture).Trim))
                            End If
                            If EndIndex + 1 < PossibleTypes.Length Then
                                PossibleTypes = PossibleTypes.Substring(EndIndex + 1).Trim
                            Else
                                Exit While
                            End If
                        End While
                        Return VBFactory.GenericName(Name, VBFactory.TypeArgumentList(VBFactory.SeparatedList(DictionaryTypeElement)))
                    End If
                    ' Could be dictionary or List
                    If TypeOf PossibleTupleType Is INamedTypeSymbol AndAlso PossibleName.Contains(",", StringComparison.InvariantCulture) Then
                        Dim NamedType As INamedTypeSymbol = CType(PossibleTupleType, INamedTypeSymbol)
                        Dim DictionaryTypeElement As New List(Of TypeSyntax)
                        If Not NamedType.TypeArguments.Any Then
                            Return PredefinedTypeObject
                        End If
                        For Each Element As ITypeSymbol In NamedType.TypeArguments
                            DictionaryTypeElement.Add(ConvertToType(Element))
                        Next
                        Return VBFactory.GenericName(Name,
                                                     VBFactory.TypeArgumentList(OpenParenToken,
                                                                                OfKeyword.WithTrailingTrivia(SpaceTrivia),
                                                                                VBFactory.SeparatedList(DictionaryTypeElement),
                                                                                CloseParenToken
                                                                                )
                                                    )
                    End If
                End If
                Return ConvertToType(PossibleName.Replace("<", "(Of ", StringComparison.InvariantCulture).Replace(">", ")", StringComparison.InvariantCulture))
            End Function

            Public Shared Function GetElementType(_ITypeSymbol As ITypeSymbol) As TypeSyntax
                Dim _TypeSyntax As TypeSyntax = ConvertToType(_ITypeSymbol)
                If _TypeSyntax.IsKind(VB.SyntaxKind.ArrayType) Then
                    If DirectCast(_ITypeSymbol, IArrayTypeSymbol).ElementType.TypeKind = TypeKind.Array Then
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
                        Case "STRING"
                            Return VBFactory.PredefinedType(CharKeyword)
                        Case "OBJECT"
                            Return VBFactory.PredefinedType(ObjectKeyword)
                        Case Else
                            Stop
                    End Select
                End If
                Return VBFactory.PredefinedType(ObjectKeyword)
            End Function

            Private Shared Function GetTypeSyntaxFromInterface(expressionConvertedType As ITypeSymbol) As TypeSyntax

                If Not expressionConvertedType.AllInterfaces.Any Then
                    If expressionConvertedType.ToString.EndsWith("IArityEnumerable", StringComparison.InvariantCulture) Then
                        Return PredefinedTypeInteger
                    End If
                    Return PredefinedTypeObject
                End If
                For Each NamedType As INamedTypeSymbol In expressionConvertedType.AllInterfaces
                    Dim index As Integer = NamedType.ToString.IndexOf(IEnumerableOf, StringComparison.InvariantCulture)
                    Dim NewType As String
                    If index > 0 Then
                        NewType = NamedType.ToString.Substring(index + IEnumerableOf.Length)
                        Return VBFactory.ParseName(NewType)
                    End If
                    index = NamedType.ToString.IndexOf(IDictionary, StringComparison.InvariantCulture)
                    If index > 0 Then
                        Return ConvertToType(NamedType)
                    End If
                    index = NamedType.ToString.IndexOf(IEnumerable, StringComparison.InvariantCulture)
                    If index > 0 Then
                        Return ConvertToType(NamedType)
                    End If
                Next

                Dim index1 As Integer = expressionConvertedType.ToString.IndexOf(IEnumerableOf, StringComparison.InvariantCulture)
                If index1 > 0 Then
                    Dim NewType As String = expressionConvertedType.ToString.Substring(index1 + IEnumerableOf.Length)
                    Return VBFactory.ParseName(NewType)
                End If
                Return Nothing
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="TupleElement can't Be Nothing")>
            Public Overrides Function VisitAnonymousMethodExpression(node As CSS.AnonymousMethodExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim Parameters As New SeparatedSyntaxList(Of CSS.ParameterSyntax)
                If node.ParameterList IsNot Nothing Then
                    Parameters = CType((node.ParameterList?.Parameters), SeparatedSyntaxList(Of CSS.ParameterSyntax))
                End If
                Return ConvertLambdaExpression(node:=node, block:=node.Block.Statements, parameters:=Parameters, CS_Modifiers:=VBFactory.TokenList(node.AsyncKeyword)).WithConvertedTriviaFrom(node)
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitAnonymousObjectCreationExpression(node As CSS.AnonymousObjectCreationExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim FieldInitializers As New List(Of FieldInitializerSyntax)
                For i As Integer = 0 To node.Initializers.Count - 1
                    Dim Initializer As CSS.AnonymousObjectMemberDeclaratorSyntax = node.Initializers(i)
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
                    For j As Integer = 0 To FieldLeadingTrivia.Count - 1
                        Dim t As SyntaxTrivia = FieldLeadingTrivia(j)
                        Select Case t.RawKind
                            Case VB.SyntaxKind.WhitespaceTrivia
                                If FirstTrivia = True Then
                                    NewFieldLeadingTrivia.Add(t)
                                End If
                            Case VB.SyntaxKind.EndOfLineTrivia
                                If FoundComment Then
                                    If j = FieldLeadingTrivia.Count - 1 Then
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

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
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

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitAssignmentExpression(node As CSS.AssignmentExpressionSyntax) As VB.VisualBasicSyntaxNode
                'Dim errorHandler As EventHandler(Of AnalyzerLoadFailureEventArgs) = Sub(o, e) errors.Add(e)
                If TypeOf node.Parent Is CSS.ExpressionStatementSyntax OrElse TypeOf node.Parent Is CSS.ArrowExpressionClauseSyntax Then
                    Dim RightTypeInfo As TypeInfo = ModelExtensions.GetTypeInfo(mSemanticModel, node.Right)
                    Dim IsDelegate As Boolean
                    If RightTypeInfo.ConvertedType IsNot Nothing Then
                        IsDelegate = RightTypeInfo.ConvertedType.IsDelegateType
                        If Not IsDelegate Then
                            IsDelegate = RightTypeInfo.ConvertedType.ToString().StartsWith("System.EventHandler", StringComparison.InvariantCulture)
                        End If
                    Else
                        If RightTypeInfo.Type IsNot Nothing Then
                            IsDelegate = RightTypeInfo.Type.IsDelegateType
                            If Not IsDelegate Then
                                IsDelegate = RightTypeInfo.Type.ToString.StartsWith("System.EventHandler", StringComparison.InvariantCulture)
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
                        Dim TupleName As String
                        Dim RightNode As ExpressionSyntax = DirectCast(node.Right.Accept(Me).WithConvertedTriviaFrom(node.Right), ExpressionSyntax)
                        Dim Initializer As EqualsValueSyntax = VBFactory.EqualsValue(RightNode)
                        Dim DimModifiersTokenList As SyntaxTokenList = VBFactory.TokenList(
                                                    DimKeyword.WithConvertedLeadingTriviaFrom(node.Left.GetFirstToken())
                                                    )
                        Dim StatementList As New SyntaxList(Of StatementSyntax)
                        Dim VariableNames As New List(Of String)
                        If node.Left.IsKind(CS.SyntaxKind.DeclarationExpression) Then
                            TupleName = MethodBodyVisitor.GetUniqueVariableNameInScope(node, "TupleTempVar", mSemanticModel)
                            Dim NodeLeft As CSS.DeclarationExpressionSyntax = DirectCast(node.Left, CSS.DeclarationExpressionSyntax)
                            Dim Designation As CSS.ParenthesizedVariableDesignationSyntax = DirectCast(NodeLeft.Designation, CSS.ParenthesizedVariableDesignationSyntax)
                            For i As Integer = 0 To Designation.Variables.Count - 1
                                Dim variableDesignation As CSS.VariableDesignationSyntax = Designation.Variables(i)
                                If variableDesignation.IsKind(CS.SyntaxKind.DiscardDesignation) Then
                                    VariableNames.Add("_")
                                Else
                                    VariableNames.Add(variableDesignation.Accept(Me).ToString)
                                End If
                            Next

                            Dim TempTupleIdentifier As SeparatedSyntaxList(Of ModifiedIdentifierSyntax) = VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(TupleName))
                            Dim VariableDeclaration As SeparatedSyntaxList(Of VariableDeclaratorSyntax) = VBFactory.SingletonSeparatedList(VBFactory.VariableDeclarator(TempTupleIdentifier, asClause:=Nothing, Initializer))
                            Dim DimStatement As LocalDeclarationStatementSyntax = VBFactory.LocalDeclarationStatement(DimModifiersTokenList, VariableDeclaration).WithPrependedLeadingTrivia(VBFactory.CommentTrivia($" ' TODO: VB has no equivalent to C# deconstruction declarations, an attempt was made to convert."), VB_EOLTrivia)
                            StatementList = StatementList.Add(DimStatement)

                            For i As Integer = 0 To VariableNames.Count - 1
                                If VariableNames(i) = "_" Then
                                    Continue For
                                End If
                                Dim AsClause As AsClauseSyntax = Nothing
                                If NodeLeft.Type Is Nothing Then
                                    Stop
                                Else
                                    AsClause = VBFactory.SimpleAsClause(CType(NodeLeft.Type.Accept(Me), TypeSyntax))
                                End If
                                Dim TupleExpression As ExpressionSyntax = VBFactory.ParseExpression($"{TupleName}.Item{(i + 1)}")
                                Initializer = VBFactory.EqualsValue(VBFactory.InvocationExpression(TupleExpression))
                                Dim Declarators As SeparatedSyntaxList(Of VariableDeclaratorSyntax) = VBFactory.SingletonSeparatedList(
                                                                            VBFactory.VariableDeclarator(
                                                                            VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(VariableNames(i))),
                                                                            AsClause,
                                                                            Initializer)
                                                                        )
                                Dim AssignmentStatement As LocalDeclarationStatementSyntax = VBFactory.LocalDeclarationStatement(DimModifier, Declarators)
                                StatementList = StatementList.Add(AssignmentStatement)
                            Next
                        End If

                        ' Handle assignment to a Tuple of Variables that already exist
                        If node.Left.IsKind(CS.SyntaxKind.TupleExpression) Then
                            Dim LeftTupleNode As TupleExpressionSyntax = DirectCast(node.Left.Accept(Me).WithConvertedTriviaFrom(node.Left), TupleExpressionSyntax)

                            VariableNames = New List(Of String)
                            TupleName = MethodBodyVisitor.GetUniqueVariableNameInScope(node, "TupleTempVar", mSemanticModel)
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
                                        Dim NamedTypes As String = Type.TupleElements(0).ContainingType.ToString
                                        Dim TypeNames() As String = NamedTypes.Substring(1, NamedTypes.Length - 2).Split(","c)
                                        For i As Integer = 0 To TypeNames.Count - 1
                                            ' Need to convert Types !!!!!!!
                                            TupleList.Add(ConvertNamedTypeToTypeString(TypeNames(i).Trim))

                                        Next
                                    Else
                                        TupleList.AddRange(ConvertTupleToTypeStrings(Type.ToString))
                                    End If
                                ElseIf TypeOf RightTypeInfo.Type Is ITypeSymbol Then
                                    For i As Integer = 1 To LeftTupleNode.Arguments.Count
                                        TupleList.Add("Object")
                                    Next
                                Else
                                    Stop
                                End If
                            End If
                            Dim builder As New System.Text.StringBuilder()
                            builder.Append("(")
                            For i As Integer = 0 To TupleList.Count - 2
                                builder.Append(TupleList(i) & ", ")
                            Next
                            builder.Append(TupleList.Last & ")")
                            Dim TupleType As String = builder.ToString

                            Dim SimpleAs As SimpleAsClauseSyntax = VBFactory.SimpleAsClause(AsKeyword.WithTrailingTrivia(SpaceTrivia), attributeLists:=Nothing, type:=VBFactory.ParseTypeName(TupleType).WithLeadingTrivia(SpaceTrivia)).WithLeadingTrivia(SpaceTrivia)
                            Dim Names As SeparatedSyntaxList(Of ModifiedIdentifierSyntax) = VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(TupleName))
                            Dim VariableDeclaration As SeparatedSyntaxList(Of VariableDeclaratorSyntax) = VBFactory.SingletonSeparatedList(VBFactory.VariableDeclarator(Names, SimpleAs, Initializer))
                            Dim DimStatement As LocalDeclarationStatementSyntax = VBFactory.LocalDeclarationStatement(DimModifiersTokenList, VariableDeclaration)
                            StatementList = StatementList.Add(DimStatement)

                            For i As Integer = 0 To TupleList.Count - 1
                                If TupleList(i) = "_" Then
                                    Continue For
                                End If
                                Dim NewLeftNode As ExpressionSyntax = VBFactory.IdentifierName(VariableNames(i))
                                Dim NewRightNode As ExpressionSyntax = VBFactory.InvocationExpression(VBFactory.ParseExpression($"{TupleName}.Item{(i + 1)}"))
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
                            WithLeadingTrivia(VB_EOLTrivia).
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
                    Else
                        Dim NodeRight As VB.VisualBasicSyntaxNode = node.Right.Accept(Me)
                        If TypeOf NodeRight Is ObjectMemberInitializerSyntax Then
                            Dim ObjectMemberInitializer As ObjectMemberInitializerSyntax = DirectCast(NodeRight, ObjectMemberInitializerSyntax)
                            Dim AttributeLists As SyntaxList(Of AttributeListSyntax) = Nothing
                            Dim ArgumentList As ArgumentListSyntax = Nothing
                            Dim Type1 As TypeSyntax = VBFactory.ParseTypeName("TODO_ObjectNameGoesHere")
                            Dim ObjectCreationExpression As ObjectCreationExpressionSyntax = VBFactory.ObjectCreationExpression(NewKeyword, AttributeLists, Type1, ArgumentList, initializer:=ObjectMemberInitializer)
                            Dim IdentifierName As IdentifierNameSyntax = DirectCast(node.Left.Accept(Me), IdentifierNameSyntax)
                            If node.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                Stop
                            End If
                            Return VBFactory.NamedFieldInitializer(IdentifierName.WithoutLeadingTrivia, ObjectCreationExpression).WithConvertedTrailingTriviaFrom(node)
                        End If

                        Dim VisualBasicNode As VB.VisualBasicSyntaxNode = node.Left.Accept(Me)
                        If TypeOf VisualBasicNode Is IdentifierNameSyntax Then
                            If node.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                GetStatementwithIssues(node).AddMarker(VBFactory.EmptyStatement.WithLeadingTrivia(ConvertTrivia(node.GetLeadingTrivia)), StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                            End If
                            Return VBFactory.NamedFieldInitializer(DirectCast(VisualBasicNode.WithoutLeadingTrivia, IdentifierNameSyntax), DirectCast(NodeRight, ExpressionSyntax)).WithConvertedTrailingTriviaFrom(node)
                        ElseIf TypeOf VisualBasicNode Is MemberAccessExpressionSyntax Then
                            Dim kind As VB.SyntaxKind = ConvertCSExpressionsKindToVBKind(CS.CSharpExtensions.Kind(node))
                            Dim OperatorToken As SyntaxToken = ExpressionKindToOperatorToken(kind)
                            Return VBFactory.AssignmentStatement(kind, DirectCast(VisualBasicNode, MemberAccessExpressionSyntax), OperatorToken, DirectCast(NodeRight, ExpressionSyntax)).WithConvertedTriviaFrom(node)
                        Else
                            Stop
                        End If
                    End If
                End If

                Dim LeftExpression As ExpressionSyntax = DirectCast(node.Left.Accept(Me), ExpressionSyntax)
                Dim RightExpression As ExpressionSyntax = DirectCast(node.Right.Accept(Me), ExpressionSyntax)
                If TypeOf node.Parent Is CSS.ArrowExpressionClauseSyntax Then
                    Return VBFactory.SimpleAssignmentStatement(LeftExpression, RightExpression)
                End If
                MarkPatchInlineAssignHelper(node)
                Return VBFactory.InvocationExpression(expression:=VBFactory.IdentifierName("__InlineAssignHelper"), argumentList:=VBFactory.ArgumentList(VBFactory.SeparatedList((New ArgumentSyntax() {VBFactory.SimpleArgument(LeftExpression), VBFactory.SimpleArgument(RightExpression)})))).WithConvertedTriviaFrom(node)
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitAwaitExpression(node As CSS.AwaitExpressionSyntax) As VB.VisualBasicSyntaxNode
                Return VBFactory.AwaitExpression(expression:=DirectCast(node.Expression.Accept(Me), ExpressionSyntax)).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitBaseExpression(node As CSS.BaseExpressionSyntax) As VB.VisualBasicSyntaxNode
                Return VBFactory.MyBaseExpression()
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitBinaryExpression(node As CSS.BinaryExpressionSyntax) As VB.VisualBasicSyntaxNode
                Try
                    Dim FoundEOL As Boolean = False
                    Dim kind As VB.SyntaxKind
                    Dim LeftVBNode As VB.VisualBasicSyntaxNode
                    Dim RightVBNode As VB.VisualBasicSyntaxNode
                    Dim VBOperatorToken As SyntaxToken
                    If IsConcatenateStringsExpression(node) Then
                        Dim CS_NodesOrTokens As New List(Of SyntaxNodeOrToken)
                        For Each n As SyntaxNodeOrToken In node.DescendantNodesAndTokens(Function(a As SyntaxNode) (TypeOf a Is CSS.BinaryExpressionSyntax))
                            If n.IsNode Then
                                If Not CType(n, SyntaxNode).IsKind(CS.SyntaxKind.AddExpression) Then
                                    CS_NodesOrTokens.Add(n)
                                End If
                            Else
                                CS_NodesOrTokens.Add(n)
                            End If
                        Next
                        kind = VB.SyntaxKind.ConcatenateExpression
                        Dim VBNode As VB.VisualBasicSyntaxNode = CType(CS_NodesOrTokens(0), CSS.ExpressionSyntax).Accept(Me)
                        LeftVBNode = ConvertAndModifyNodeTrivia(VBNode, CS_NodesOrTokens, 0)
                        For i As Integer = 1 To CS_NodesOrTokens.Count - 1
                            VBOperatorToken = ConvertAndModifyTokenTrivia(AmpersandToken, CS_NodesOrTokens, i)
                            VBNode = CType(CS_NodesOrTokens(i + 1), CSS.ExpressionSyntax).Accept(Me)
                            RightVBNode = ConvertAndModifyNodeTrivia(VBNode, CS_NodesOrTokens, i + 1)
                            LeftVBNode = VBFactory.ConcatenateExpression(
                                                            DirectCast(LeftVBNode, ExpressionSyntax),
                                                            VBOperatorToken,
                                                            DirectCast(RightVBNode, ExpressionSyntax)
                                                            )
                            i += 1
                        Next
                        Return LeftVBNode
                    End If
                    kind = ConvertCSExpressionsKindToVBKind(CS.CSharpExtensions.Kind(node))
                    LeftVBNode = node.Left.Accept(Me).WithConvertedTriviaFrom(node.Left)
                    Dim LeftExpression As ExpressionSyntax = DirectCast(LeftVBNode, ExpressionSyntax)
                    RightVBNode = node.Right.Accept(Me).WithConvertedTriviaFrom(node.Right)
                    Dim CommaTokenWithTrivia As SyntaxToken = CommaToken
                    Select Case node.Kind
                        Case CS.SyntaxKind.CoalesceExpression
                            Dim IfKeywordWithTrivia As SyntaxToken = IfKeyword
                            If TypeOf RightVBNode Is ExpressionSyntax Then
                                Dim SecondExpression As ExpressionSyntax = DirectCast(RightVBNode, ExpressionSyntax)
                                Dim SeparatorTrailingTrivia As New List(Of SyntaxTrivia)
                                If LeftExpression.ContainsEOLTrivia OrElse LeftExpression.ContainsCommentOrDirectiveTrivia Then
                                    Dim IfLeadingTrivia As New List(Of SyntaxTrivia)
                                    If LeftExpression.HasLeadingTrivia Then
                                        IfKeywordWithTrivia = IfKeywordWithTrivia.WithLeadingTrivia(LeftExpression.GetLeadingTrivia)
                                    End If
                                    If LeftExpression.HasTrailingTrivia Then
                                        For Each t As SyntaxTrivia In LeftExpression.GetTrailingTrivia
                                            Select Case t.RawKind
                                                Case VB.SyntaxKind.CommentTrivia
                                                    SeparatorTrailingTrivia.Add(SpaceTrivia)
                                                    SeparatorTrailingTrivia.Add(t)
                                                    SeparatorTrailingTrivia.Add(SpaceTrivia)
                                                    FoundEOL = True
                                                Case VB.SyntaxKind.EndOfLineTrivia
                                                    FoundEOL = True
                                                Case VB.SyntaxKind.WhitespaceTrivia
                                                    ' ignore
                                                Case Else
                                                    Stop
                                            End Select
                                        Next
                                        If FoundEOL Then
                                            SeparatorTrailingTrivia.Add(VB_EOLTrivia)
                                            FoundEOL = False
                                        End If
                                        LeftExpression = LeftExpression.With({SpaceTrivia}, {SpaceTrivia})
                                    End If
                                    CommaTokenWithTrivia = CommaTokenWithTrivia.WithTrailingTrivia(SeparatorTrailingTrivia)
                                    If node.OperatorToken.HasLeadingTrivia AndAlso node.OperatorToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                        Dim StatementTrivia As List(Of SyntaxTrivia) = ConvertTrivia(node.OperatorToken.LeadingTrivia).ToList
                                        GetStatementwithIssues(node).AddMarker(
                                                        VBFactory.EmptyStatement.WithLeadingTrivia(StatementTrivia),
                                                        StatementHandlingOption.AppendEmptyStatement,
                                                        AllowDuplicates:=True)
                                        SecondExpression = SecondExpression.WithLeadingTrivia(StatementTrivia.Last)
                                    End If
                                End If
                                Dim binaryConditionalExpressionSyntax1 As BinaryConditionalExpressionSyntax = VBFactory.BinaryConditionalExpression(
                                                            IfKeywordWithTrivia,
                                                            OpenParenToken,
                                                            LeftExpression,
                                                            CommaTokenWithTrivia,
                                                            SecondExpression,
                                                            CloseParenToken)
                                Return binaryConditionalExpressionSyntax1
                            End If
                            If TypeOf RightVBNode Is ThrowStatementSyntax Then
                                Dim Condition As ExpressionSyntax = VBFactory.IsExpression(LeftExpression.WithTrailingTrivia(SpaceTrivia), NothingExpression)
                                Dim IfStatement As IfStatementSyntax = VBFactory.IfStatement(IfKeywordWithTrivia, Condition, ThenKeyword)

                                Dim Statements As SyntaxList(Of StatementSyntax) = VBFactory.SingletonList(DirectCast(RightVBNode, StatementSyntax))
                                Dim IfBlock As StatementSyntax = VBFactory.SingleLineIfStatement(IfKeywordWithTrivia,
                                                                                               Condition,
                                                                                               ThenKeyword,
                                                                                               Statements,
                                                                                               elseClause:=Nothing).WithTrailingEOL
                                GetStatementwithIssues(node).AddMarker(IfBlock, StatementHandlingOption.PrependStatement, AllowDuplicates:=False)
                                Return LeftVBNode
                            Else
                                Stop
                            End If
                        Case CS.SyntaxKind.AsExpression
                            Dim FirstExpression As ExpressionSyntax = DirectCast(LeftVBNode, ExpressionSyntax)
                            If FirstExpression.ContainsEOLTrivia Then
                                FirstExpression = FirstExpression.WithRestructuredingEOLTrivia
                                CommaTokenWithTrivia = CommaTokenWithTrivia.WithTrailingTrivia(VB_EOLTrivia)
                            End If
                            Dim Type As TypeSyntax = DirectCast(RightVBNode, TypeSyntax)
                            Dim TryCastExpression As TryCastExpressionSyntax = VBFactory.TryCastExpression(
                                                TryCastKeyword,
                                                OpenParenToken,
                                                FirstExpression,
                                                CommaTokenWithTrivia,
                                                Type,
                                                CloseParenToken)
                            Return TryCastExpression
                        Case CS.SyntaxKind.IsExpression
                            Dim NewLeadingTrivia As New List(Of SyntaxTrivia)
                            If LeftVBNode.HasLeadingTrivia AndAlso LeftVBNode.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                NewLeadingTrivia.AddRange(LeftVBNode.GetLeadingTrivia)
                                LeftVBNode = LeftVBNode.WithoutTrivia.WithTrailingTrivia(SpaceTrivia)
                            End If
                            If RightVBNode.HasLeadingTrivia AndAlso RightVBNode.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                NewLeadingTrivia.AddRange(RightVBNode.GetLeadingTrivia)
                                RightVBNode = RightVBNode.WithoutTrivia.WithTrailingTrivia(SpaceTrivia)
                            End If
                            Return VBFactory.TypeOfIsExpression(DirectCast(LeftVBNode, ExpressionSyntax), DirectCast(RightVBNode, TypeSyntax)).WithLeadingTrivia(NewLeadingTrivia)
                        Case CS.SyntaxKind.EqualsExpression, CS.SyntaxKind.NotEqualsExpression
                            Dim otherArgument As ExpressionSyntax = Nothing
                            If node.Left.IsKind(CS.SyntaxKind.NullLiteralExpression) Then
                                otherArgument = DirectCast(RightVBNode, ExpressionSyntax).With({SpaceTrivia}, {SpaceTrivia})
                            ElseIf node.Left.IsKind(CS.SyntaxKind.SizeOfExpression) AndAlso DirectCast(node.Left, CSS.SizeOfExpressionSyntax).Type.IsKind(CS.SyntaxKind.PointerType) Then
                                LeftExpression = IntPrtSizeExpression
                            End If

                            If node.Right.IsKind(CS.SyntaxKind.NullLiteralExpression) Then
                                otherArgument = DirectCast(LeftVBNode, ExpressionSyntax).With({SpaceTrivia}, {SpaceTrivia})
                            ElseIf node.Right.IsKind(CS.SyntaxKind.SizeOfExpression) AndAlso DirectCast(node.Right, CSS.SizeOfExpressionSyntax).Type.IsKind(CS.SyntaxKind.PointerType) Then
                                RightVBNode = IntPrtSizeExpression
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
                    Dim LeftTrailingTrivia As SyntaxTriviaList = LeftExpression.GetTrailingTrivia
                    Dim RightExpression As ExpressionSyntax = DirectCast(RightVBNode, ExpressionSyntax)
                    If LeftTrailingTrivia.ToList.Count = 1 AndAlso LeftTrailingTrivia(0).ToString.Trim = "?" Then
                        Dim OldIdentifier As IdentifierNameSyntax = RightExpression.DescendantNodes.
                                                                           OfType(Of IdentifierNameSyntax).
                                                                           First(Function(b As IdentifierNameSyntax) b.Kind() = VB.SyntaxKind.IdentifierName)
                        Dim NewIdentifierWithQuestionMark As IdentifierNameSyntax =
                                    VBFactory.IdentifierName($"{DirectCast(node.Left.Accept(Me), ExpressionSyntax).ToString}?")
                        Return RightExpression.ReplaceNode(OldIdentifier, NewIdentifierWithQuestionMark)
                    End If

                    Dim MovedTrailingTrivia As New List(Of SyntaxTrivia)
                    If LeftExpression.HasLeadingTrivia AndAlso LeftExpression.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        MovedTrailingTrivia.AddRange(LeftExpression.GetLeadingTrivia)
                        LeftExpression = LeftExpression.WithLeadingTrivia(SpaceTrivia)
                    End If
                    If LeftExpression.HasTrailingTrivia Then
                        For Each t As SyntaxTrivia In LeftExpression.GetTrailingTrivia
                            Select Case t.RawKind
                                Case VB.SyntaxKind.CommentTrivia
                                    MovedTrailingTrivia.Add(SpaceTrivia)
                                    MovedTrailingTrivia.Add(t)
                                    MovedTrailingTrivia.Add(SpaceTrivia)
                                Case VB.SyntaxKind.EndOfLineTrivia
                                    FoundEOL = True
                                Case VB.SyntaxKind.WhitespaceTrivia
                                Case Else
                                    Stop
                            End Select
                        Next
                        LeftExpression = LeftExpression.WithTrailingTrivia(SpaceTrivia)
                    End If
                    Dim operatorToken As SyntaxToken = ExpressionKindToOperatorToken(kind).WithConvertedTriviaFrom(node.OperatorToken)
                    If operatorToken.HasLeadingTrivia Then
                        If operatorToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                            MovedTrailingTrivia.AddRange(operatorToken.LeadingTrivia)
                            operatorToken = operatorToken.WithLeadingTrivia(SpaceTrivia)
                        ElseIf operatorToken.LeadingTrivia.ContainsEOLTrivia Then
                            operatorToken = operatorToken.WithLeadingTrivia(SpaceTrivia)
                        End If
                    End If

                    If operatorToken.HasTrailingTrivia Then
                        Dim NewOperatorTrailingTrivia As New List(Of SyntaxTrivia)
                        For Each t As SyntaxTrivia In operatorToken.TrailingTrivia
                            Select Case t.RawKind
                                Case VB.SyntaxKind.CommentTrivia
                                    If FoundEOL Then
                                        NewOperatorTrailingTrivia.AddRange(MovedTrailingTrivia)
                                        NewOperatorTrailingTrivia.Add(SpaceTrivia)
                                        NewOperatorTrailingTrivia.Add(t)
                                        NewOperatorTrailingTrivia.Add(SpaceTrivia)
                                        MovedTrailingTrivia.Clear()
                                    Else
                                        NewOperatorTrailingTrivia.Add(t)
                                    End If
                                Case VB.SyntaxKind.EndOfLineTrivia
                                    FoundEOL = True
                                Case VB.SyntaxKind.WhitespaceTrivia
                                    If FoundEOL Then
                                        MovedTrailingTrivia.Add(t)
                                    Else
                                        NewOperatorTrailingTrivia.Add(t)
                                    End If
                                Case Else
                                    Stop
                            End Select
                        Next
                        If FoundEOL Then
                            NewOperatorTrailingTrivia.Add(VB_EOLTrivia)
                        End If
                        operatorToken = operatorToken.WithTrailingTrivia(NewOperatorTrailingTrivia)
                    End If

                    Dim RightNode As ExpressionSyntax = DirectCast(node.Right.Accept(Me), ExpressionSyntax)

                    If node.Right.HasTrailingTrivia Then
                        MovedTrailingTrivia.Clear()
                        FoundEOL = False
                        For Each t As SyntaxTrivia In RightNode.GetTrailingTrivia
                            Select Case t.RawKind
                                Case VB.SyntaxKind.CommentTrivia
                                    MovedTrailingTrivia.Add(SpaceTrivia)
                                    MovedTrailingTrivia.Add(t)
                                    MovedTrailingTrivia.Add(SpaceTrivia)

                                Case VB.SyntaxKind.EndOfLineTrivia
                                    FoundEOL = True
                                Case VB.SyntaxKind.WhitespaceTrivia
                                    MovedTrailingTrivia.Add(t)
                                Case Else
                                    Stop
                            End Select
                        Next
                        If FoundEOL Then
                            MovedTrailingTrivia.Add(VB_EOLTrivia)
                        End If
                    End If
                    RightExpression = RightNode.With({SpaceTrivia}, MovedTrailingTrivia)
                    Dim binaryExpressionSyntax3 As BinaryExpressionSyntax = VBFactory.BinaryExpression(
                                                    kind,
                                                    LeftExpression,
                                                    operatorToken,
                                                    RightExpression
                                                    )
                    Return binaryExpressionSyntax3
                Catch ex As InsufficientExecutionStackException
                    MsgBox(ex.Message, MsgBoxStyle.Critical, "Stack Overflow")
                    Return Nothing
                Catch ex As Exception
                    Stop
                    Throw
                End Try
                Throw UnreachableException
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitCastExpression(node As CSS.CastExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim CTypeExpressionSyntax As VB.VisualBasicSyntaxNode
                Dim NewTrailingTrivia As New List(Of SyntaxTrivia)
                Try
                    Dim type As ITypeSymbol = ModelExtensions.GetTypeInfo(mSemanticModel, node.Type).Type
                    Dim Expression As ExpressionSyntax = DirectCast(node.Expression.Accept(Me), ExpressionSyntax)
                    NewTrailingTrivia.AddRange(Expression.GetTrailingTrivia)
                    NewTrailingTrivia.AddRange(ConvertTrivia(node.GetTrailingTrivia))
                    Expression = Expression.WithoutTrivia

                    Dim ExpressionTypeStr As String = ModelExtensions.GetTypeInfo(mSemanticModel, node.Expression).Type?.ToString
                    Select Case type.SpecialType
                        Case SpecialType.System_Object
                            CTypeExpressionSyntax = VBFactory.PredefinedCastExpression(CObjKeyword, Expression)
                        Case SpecialType.System_Boolean
                            CTypeExpressionSyntax = VBFactory.PredefinedCastExpression(CBoolKeyword, Expression)
                        Case SpecialType.System_Char
                            Dim TestAgainst As String() = {"int", "ushort"}
                            If (node.Parent.IsKind(CS.SyntaxKind.AttributeArgument)) Then
                                CTypeExpressionSyntax = Expression
                            ElseIf TestAgainst.Contains(ExpressionTypeStr, StringComparer.OrdinalIgnoreCase) Then
                                CTypeExpressionSyntax = VBFactory.ParseExpression($"ChrW({Expression.ToString})")
                            Else
                                CTypeExpressionSyntax = VBFactory.PredefinedCastExpression(CCharKeyword, Expression)
                            End If
                        Case SpecialType.System_SByte
                            CTypeExpressionSyntax = VBFactory.PredefinedCastExpression(CSByteKeyword, Expression)
                        Case SpecialType.System_Byte
                            If Expression.IsKind(VB.SyntaxKind.CharacterLiteralExpression) Then
                                CTypeExpressionSyntax = VBFactory.ParseExpression($"AscW({Expression.ToString})")
                            Else
                                CTypeExpressionSyntax = VBFactory.PredefinedCastExpression(CByteKeyword, Expression)
                            End If
                        Case SpecialType.System_Int16
                            Dim FixExpression As ExpressionSyntax = VBFactory.IdentifierName("Fix")
                            Dim ArgumentList As ArgumentListSyntax = VBFactory.ArgumentList(VBFactory.SingletonSeparatedList(Of ArgumentSyntax)(VBFactory.SimpleArgument(Expression)))
                            CTypeExpressionSyntax = VBFactory.PredefinedCastExpression(CShortKeyword, VBFactory.InvocationExpression(FixExpression, ArgumentList))
                        Case SpecialType.System_UInt16
                            If ExpressionTypeStr = "char" Then
                                CTypeExpressionSyntax = VBFactory.ParseExpression(text:=$"ChrW({Expression.ToString})")
                            Else
                                CTypeExpressionSyntax = VBFactory.PredefinedCastExpression(CUShortKeyword, Expression)
                            End If
                        Case SpecialType.System_Int32
                            If ExpressionTypeStr = "char" Then
                                CTypeExpressionSyntax = VBFactory.ParseExpression($"ChrW({Expression.ToString})").WithTrailingTrivia(NewTrailingTrivia)
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
                                If AddressOf1.Operand.ToString.StartsWith("&", StringComparison.InvariantCultureIgnoreCase) Then
                                    Dim StatementWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                                    StatementWithIssues.AddMarker(FlagUnsupportedStatements(StatementWithIssues, "pointers", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=True)
                                    CTypeExpressionSyntax = Expression
                                ElseIf node.Type.IsKind(CS.SyntaxKind.PointerType) Then
                                    Return VBFactory.CTypeExpression(Expression, VBFactory.ParseTypeName("IntPtr"))
                                Else
                                    CTypeExpressionSyntax = VBFactory.CTypeExpression(Expression, VBFactory.ParseTypeName(AddressOf1.Operand.
                                                                                                                                ToString.
                                                                                                                                Replace("&", "", StringComparison.InvariantCultureIgnoreCase)))
                                End If
                            Else
                                CTypeExpressionSyntax = VBFactory.CTypeExpression(Expression, DirectCast(TypeOrAddressOf, TypeSyntax))
                            End If
                    End Select
                Catch ex As Exception
                    Stop
                    Throw
                End Try
                Return CTypeExpressionSyntax.WithConvertedLeadingTriviaFrom(node).WithTrailingTrivia(NewTrailingTrivia)
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitCheckedExpression(node As CSS.CheckedExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim Unchecked As Boolean = node.Keyword.ValueText <> "checked"
                Dim msg As String = If(Unchecked, "VB has no direct equivalent to C# unchecked:", "VB default math is equivalent to C# checked:")
                Dim StatementWithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                Dim LeadingTrivia As SyntaxTriviaList = CheckCorrectnessLeadingTrivia(StatementWithIssue, msg)
                ' Only notify once on one line TODO Merge the comments
                StatementWithIssue.AddMarker(VBFactory.EmptyStatement.WithLeadingTrivia(LeadingTrivia), StatementHandlingOption.PrependStatement, AllowDuplicates:=False)

                Dim Expression As ExpressionSyntax = DirectCast(node.Expression.Accept(Me), ExpressionSyntax)
                If TypeOf Expression Is PredefinedCastExpressionSyntax Then
                    Dim CastExpression As PredefinedCastExpressionSyntax = DirectCast(Expression, PredefinedCastExpressionSyntax)
                    If Unchecked Then
                        Return VBFactory.ParseExpression($"{CastExpression.Keyword.ToString}(Val(""&H"" & Hex({CastExpression.Expression.ToString})))")
                    Else
                        Return VBFactory.ParseExpression($"{CastExpression.Keyword.ToString}({CastExpression.Expression.ToString})")
                    End If
                End If
                If TypeOf Expression Is BinaryExpressionSyntax OrElse
                    TypeOf Expression Is InvocationExpressionSyntax OrElse
                    TypeOf Expression Is LiteralExpressionSyntax OrElse
                    TypeOf Expression Is ObjectCreationExpressionSyntax Then
                    If Unchecked Then
                        Return VBFactory.ParseExpression($"Unchecked({Expression.ToString})")
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

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitConditionalAccessExpression(node As CSS.ConditionalAccessExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim expression As ExpressionSyntax = DirectCast(node.Expression.Accept(Me), ExpressionSyntax)
                Dim TrailingTriviaList As New List(Of SyntaxTrivia)
                If expression.ContainsEOLTrivia Then
                    TrailingTriviaList.AddRange(expression.WithRestructuredingEOLTrivia.GetTrailingTrivia)
                    expression = expression.WithoutTrailingTrivia
                End If
                Return VBFactory.ConditionalAccessExpression(expression, QuestionToken.WithTrailingTrivia(TrailingTriviaList), DirectCast(node.WhenNotNull.Accept(Me), ExpressionSyntax)).WithConvertedTriviaFrom(node)
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
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

                Dim CS_WhenTrue As CSS.ExpressionSyntax = node.WhenTrue
                Dim WhenTrue As ExpressionSyntax = Nothing
                If Not CS_WhenTrue.IsKind(CS.SyntaxKind.ThrowExpression) Then
                    WhenTrue = DirectCast(ConvertAndModifyNodeTrivia(node.WhenTrue.Accept(Me), NodesOrTokens, 2), ExpressionSyntax)
                End If

                Dim FirstCommaToken As SyntaxToken = ConvertAndModifyTokenTrivia(CommaToken, NodesOrTokens, 1)

                Dim CS_WhenFalse As CSS.ExpressionSyntax = node.WhenFalse
                Dim WhenFalse As ExpressionSyntax = Nothing
                If Not CS_WhenFalse.IsKind(CS.SyntaxKind.ThrowExpression) Then
                    WhenFalse = DirectCast(ConvertAndModifyNodeTrivia(node.WhenFalse.Accept(Me), NodesOrTokens, 4), ExpressionSyntax)
                End If

                Dim IfKeywordWithTrivia As SyntaxToken = IfKeyword.WithConvertedLeadingTriviaFrom(node.Condition.GetFirstToken)
                Dim SecondCommaToken As SyntaxToken = ConvertAndModifyTokenTrivia(CommaToken, NodesOrTokens, 3)

                If Not CS_WhenFalse.IsKind(CS.SyntaxKind.ThrowExpression) AndAlso Not CS_WhenTrue.IsKind(CS.SyntaxKind.ThrowExpression) Then
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
                If Not CS_WhenFalse.IsKind(CS.SyntaxKind.ThrowExpression) Then
                    ThrowStatement = DirectCast(CS_WhenTrue.Accept(Me).WithConvertedTriviaFrom(CS_WhenTrue), ThrowStatementSyntax).WithTrailingEOL
                    ResultExpression = DirectCast(CS_WhenFalse.Accept(Me).WithConvertedTriviaFrom(CS_WhenFalse), ExpressionSyntax)
                Else
                    Condition = VBFactory.NotExpression(Condition.WithoutTrivia)
                    ThrowStatement = DirectCast(CS_WhenFalse.Accept(Me).WithConvertedTriviaFrom(CS_WhenFalse), ThrowStatementSyntax).WithTrailingEOL
                    ResultExpression = DirectCast(CS_WhenTrue.Accept(Me).WithConvertedTriviaFrom(CS_WhenTrue), ExpressionSyntax)
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

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
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
                                                                    Replace(",", "", StringComparison.InvariantCulture).
                                                                    Replace(" ", "", StringComparison.InvariantCulture).
                                                                    Replace("(", "", StringComparison.InvariantCulture).
                                                                    Replace(")", "", StringComparison.InvariantCulture))
                End If
                If Node.Designation.IsKind(CS.SyntaxKind.DiscardDesignation) Then
                    Dim DiscardDesignation As CSS.DiscardDesignationSyntax = DirectCast(Node.Designation, CSS.DiscardDesignationSyntax)
                    Value = DirectCast(DiscardDesignation.Accept(Me), IdentifierNameSyntax).WithConvertedTriviaFrom(Node)
                    Return Value

                End If
                Throw UnreachableException
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitDefaultExpression(node As CSS.DefaultExpressionSyntax) As VB.VisualBasicSyntaxNode
                Return VBFactory.ParseExpression($"CType(Nothing, {node.Type.Accept(Me).WithoutLeadingTrivia})").WithConvertedTriviaFrom(node)
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitDiscardDesignation(node As CSS.DiscardDesignationSyntax) As VB.VisualBasicSyntaxNode
                Dim Identifier As SyntaxToken = GenerateSafeVBToken(node.UnderscoreToken, IsQualifiedName:=False, IsTypeName:=False)
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

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitElementAccessExpression(node As CSS.ElementAccessExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim argumentList As ArgumentListSyntax = DirectCast(node.ArgumentList.Accept(Me), ArgumentListSyntax)
                Dim expression As ExpressionSyntax
                If node.Expression.IsKind(CS.SyntaxKind.BaseExpression) Then
                    If node.GetAncestor(Of CSS.IndexerDeclarationSyntax).IsKind(CS.SyntaxKind.IndexerDeclaration) Then
                        expression = VBFactory.ParseExpression($"MyBase.Item")
                    ElseIf node.GetAncestor(Of CSS.PropertyDeclarationSyntax) IsNot Nothing Then
                        Return VBFactory.ParseExpression($"MyBase.Item({argumentList.Arguments(0).WithoutTrivia})")
                    Else
                        Return VBFactory.ParseName($"MyBase.{argumentList.Arguments(0)}")
                    End If
                ElseIf node.Expression.IsKind(CS.SyntaxKind.ObjectCreationExpression) Then
                    expression = DirectCast(node.Expression.Accept(Me), ExpressionSyntax)
                    Dim UniqueName As String = MethodBodyVisitor.GetUniqueVariableNameInScope(node, "tempVar", mSemanticModel)
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

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitElementBindingExpression(node As CSS.ElementBindingExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim Arguments0 As VB.VisualBasicSyntaxNode = node.ArgumentList.Arguments(0).Accept(Me)
                Dim expression As ExpressionSyntax = VBFactory.ParseExpression(Arguments0.ToString)
                Dim ParenthesizedExpression As ParenthesizedExpressionSyntax = VBFactory.ParenthesizedExpression(expression)
                Return VBFactory.InvocationExpression(ParenthesizedExpression)
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitImplicitArrayCreationExpression(node As CSS.ImplicitArrayCreationExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim CS_Separators As IEnumerable(Of SyntaxToken) = node.Initializer.Expressions.GetSeparators
                Dim ExpressionItems As New List(Of ExpressionSyntax)
                Dim NamedFieldItems As New List(Of FieldInitializerSyntax)
                Dim Separators As New List(Of SyntaxToken)
                Dim SeparatorCount As Integer = node.Initializer.Expressions.Count - 1
                For i As Integer = 0 To SeparatorCount
                    Dim e As CSS.ExpressionSyntax = node.Initializer.Expressions(i)
                    Dim ItemWithTrivia As VB.VisualBasicSyntaxNode
                    Try
                        ItemWithTrivia = e.Accept(Me).WithConvertedTriviaFrom(e).RemoveExtraLeadingEOL.NormalizeWhitespaceEx(useDefaultCasing:=True, indentation:="    ")
                        If TypeOf ItemWithTrivia Is NamedFieldInitializerSyntax Then
                            NamedFieldItems.Add(DirectCast(ItemWithTrivia, NamedFieldInitializerSyntax))
                        ElseIf TypeOf ItemWithTrivia Is AssignmentStatementSyntax Then
                            Dim StatementwithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                            StatementwithIssues.AddMarker(FlagUnsupportedStatements(StatementwithIssues, $"C# Assignment Expression", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=False)
                            Return Nothing
                        Else
                            ExpressionItems.Add(DirectCast(ItemWithTrivia, ExpressionSyntax))
                        End If
                    Catch ex As Exception
                        Stop
                        Throw
                    End Try
                    If SeparatorCount > i Then
                        Separators.Add(CommaToken.WithConvertedTrailingTriviaFrom(CS_Separators(i)))
                    End If
                Next
                Dim OpenBraceTokenWithTrivia As SyntaxToken = OpenBraceToken.WithConvertedTriviaFrom(node.Initializer.OpenBraceToken)
                Dim CloseBraceTokenWithTrivia As SyntaxToken = VisualBasicSyntaxFactory.CloseBraceToken.WithConvertedTrailingTriviaFrom(node.Initializer.CloseBraceToken)
                If node.Parent.IsKind(CS.SyntaxKind.ElementAccessExpression) Then
                    CloseBraceTokenWithTrivia = CloseBraceTokenWithTrivia.WithTrailingTrivia(SpaceTrivia)
                End If
                If ExpressionItems.Count > 0 Then
                    RestructureNodesAndSeparators(OpenBraceTokenWithTrivia, ExpressionItems, Separators, CloseBraceTokenWithTrivia)
                    Dim ExpressionInitializers As SeparatedSyntaxList(Of ExpressionSyntax) = VBFactory.SeparatedList(ExpressionItems, Separators)
                    Return VBFactory.CollectionInitializer(OpenBraceTokenWithTrivia, ExpressionInitializers, CloseBraceTokenWithTrivia).WithConvertedLeadingTriviaFrom(node.NewKeyword)
                Else
                    RestructureNodesAndSeparators(OpenBraceTokenWithTrivia, NamedFieldItems, Separators, CloseBraceTokenWithTrivia)
                    Dim Initializers As SeparatedSyntaxList(Of FieldInitializerSyntax) = VBFactory.SeparatedList(NamedFieldItems)
                    Return VBFactory.AnonymousObjectCreationExpression(VBFactory.ObjectMemberInitializer(Initializers))
                End If
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitInitializerExpression(node As CSS.InitializerExpressionSyntax) As VB.VisualBasicSyntaxNode
                Try

                    Dim CS_Separators As IEnumerable(Of SyntaxToken) = node.Expressions.GetSeparators
                    Dim Expressions As New List(Of ExpressionSyntax)
                    Dim Fields As New List(Of FieldInitializerSyntax)
                    Dim Separators As New List(Of SyntaxToken)
                    Dim ExpressionLastIndex As Integer = node.Expressions.Count - 1
                    Dim FinalSeparator As Boolean = CS_Separators.Any AndAlso ExpressionLastIndex <> CS_Separators.Count
                    Dim OpenBraceTokenWithTrivia As SyntaxToken = OpenBraceToken.WithConvertedTriviaFrom(node.OpenBraceToken)
#If NETCOREAPP3_0 Then
                    Dim ReportProgress As Boolean = ExpressionLastIndex > 500

                    If ReportProgress Then
                        OriginalRequest.ProgressBar?.SetTotalItems(ExpressionLastIndex)
                    End If
#End If
                    ' Figuring out this without using Accept is complicated below is safe but not fast
                    Dim ItemIsField As Boolean = node.Expressions.Any AndAlso TypeOf node.Expressions(0).Accept(Me) Is FieldInitializerSyntax
                    For i As Integer = 0 To ExpressionLastIndex
#If NETCOREAPP3_0 Then
                        If ReportProgress Then
                            OriginalRequest.ProgressBar?.UpdateProgress(1)
                        End If
#End If
                        Dim Item As VB.VisualBasicSyntaxNode = node.Expressions(i).Accept(Me)
                        Try
                            If ItemIsField Then
                                Fields.Add(DirectCast(Item.RemoveExtraLeadingEOL, FieldInitializerSyntax))
                            Else
                                Expressions.Add(DirectCast(Item.RemoveExtraLeadingEOL, ExpressionSyntax))
                            End If
                        Catch ex As Exception
                            Stop
                        End Try

                        If ExpressionLastIndex > i Then
                            Separators.Add(CommaToken.WithConvertedTrailingTriviaFrom(CS_Separators(i)))
                        Else
                            If FinalSeparator Then
                                If ItemIsField Then
                                    Fields(i) = Fields(i).WithAppendedTrailingTrivia(ConvertTrivia(CS_Separators.Last.TrailingTrivia))
                                Else
                                    Expressions(i) = Expressions(i).WithAppendedTrailingTrivia(ConvertTrivia(CS_Separators.Last.TrailingTrivia))
                                End If
                            End If
                        End If
                    Next
                    Dim CLoseBracketLeadingTriva As List(Of SyntaxTrivia) = ConvertTrivia(node.CloseBraceToken.LeadingTrivia).ToList
                    If CLoseBracketLeadingTriva.Any Then
                        If CLoseBracketLeadingTriva.First.IsKind(VB.SyntaxKind.CommentTrivia) Then
                            CLoseBracketLeadingTriva.Insert(1, VB_EOLTrivia)
                        End If
                    End If
                    Dim CLoseBracketTrailingTriva As List(Of SyntaxTrivia) = ConvertTrivia(node.CloseBraceToken.TrailingTrivia).ToList
                    If CLoseBracketLeadingTriva.ContainsCommentOrDirectiveTrivia Then
                        Dim FoundEOF As Boolean = False
                        Dim FoundCommentOrDirective As Boolean = False
                        Dim NewCLoseBracketLeadingTriva As New List(Of SyntaxTrivia)
                        For i As Integer = 0 To CLoseBracketLeadingTriva.Count - 1
                            Dim t As SyntaxTrivia = CLoseBracketLeadingTriva(i)
                            If FoundCommentOrDirective OrElse t.IsDirective Or t.IsComment Then
                                If Not (FoundEOF OrElse FoundCommentOrDirective) Then
                                    NewCLoseBracketLeadingTriva.Add(VB_EOLTrivia)
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
                                    NewCLoseBracketLeadingTriva.Add(VB_EOLTrivia)
                                    FoundEOF = True
                            End Select
                        Next
                        CLoseBracketLeadingTriva = NewCLoseBracketLeadingTriva
                    End If

                    Dim CloseBraceTokenWithTrivia As SyntaxToken = VisualBasicSyntaxFactory.CloseBraceToken.With(CLoseBracketLeadingTriva, CLoseBracketTrailingTriva)
                    If node.IsKind(CS.SyntaxKind.ObjectInitializerExpression) Then
                        Dim WithKeywordWithTrivia As SyntaxToken = WithKeyword.WithTrailingTrivia(VB_EOLTrivia)
                        If Fields.Count > 0 Then
                            RestructureNodesAndSeparators(OpenBraceTokenWithTrivia, Fields, Separators, CloseBraceTokenWithTrivia)
                            Return VBFactory.ObjectMemberInitializer(WithKeywordWithTrivia, OpenBraceTokenWithTrivia, VBFactory.SeparatedList(Fields, Separators), CloseBraceTokenWithTrivia).WithConvertedTriviaFrom(node)
                        End If
                        RestructureNodesAndSeparators(OpenBraceTokenWithTrivia, Expressions, Separators, CloseBraceTokenWithTrivia)

                        If Expressions.Count > 0 Then
                            If Not Expressions(ExpressionLastIndex).ContainsEOLTrivia Then
                                Expressions(ExpressionLastIndex) = Expressions(ExpressionLastIndex).WithAppendedTrailingTrivia(VB_EOLTrivia)
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
                Catch ex As Exception
                    Stop
                End Try
                Throw UnreachableException
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitInterpolatedStringExpression(node As CSS.InterpolatedStringExpressionSyntax) As VB.VisualBasicSyntaxNode
                Return VBFactory.InterpolatedStringExpression(node.Contents.Select(Function(c As CSS.InterpolatedStringContentSyntax) DirectCast(c.Accept(Me), InterpolatedStringContentSyntax)).ToArray()).WithConvertedTriviaFrom(node)
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitInterpolatedStringText(node As CSS.InterpolatedStringTextSyntax) As VB.VisualBasicSyntaxNode
                Dim CSharpToken As SyntaxToken = node.TextToken
                Dim TextToken As SyntaxToken = ConvertToInterpolatedStringTextToken(CSharpToken)
                Return VBFactory.InterpolatedStringText(TextToken).WithConvertedTriviaFrom(node)
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitInterpolation(node As CSS.InterpolationSyntax) As VB.VisualBasicSyntaxNode
                Return VBFactory.Interpolation(DirectCast(node.Expression.Accept(Me), ExpressionSyntax)).WithConvertedTriviaFrom(node)
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitInterpolationFormatClause(node As CSS.InterpolationFormatClauseSyntax) As VB.VisualBasicSyntaxNode
                Return MyBase.VisitInterpolationFormatClause(node).WithConvertedTriviaFrom(node)
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitInvocationExpression(node As CSS.InvocationExpressionSyntax) As VB.VisualBasicSyntaxNode
                If node.Expression.ToString() = "nameof" Then
                    Try
                        Dim IdentifierOrMember As String = node.ArgumentList.Arguments(0).Accept(Me).ToString
                        Return VBFactory.NameOfExpression(VBFactory.IdentifierName(IdentifierOrMember)).WithConvertedTriviaFrom(node)
                    Catch ex As Exception
                        Stop
                    End Try
                    Throw UnreachableException
                End If
                Dim Expression As ExpressionSyntax = DirectCast(node.Expression.Accept(Me), ExpressionSyntax).WithoutTrailingTrivia

                Dim ArgumentList1 As ArgumentListSyntax = DirectCast(node.ArgumentList.Accept(Me), ArgumentListSyntax)
                Dim NewTrailingTrivia As List(Of SyntaxTrivia) = ArgumentList1.GetTrailingTrivia.ToList
                NewTrailingTrivia.AddRange(ConvertTrivia(node.GetTrailingTrivia))
                Select Case NewTrailingTrivia.Count
                    Case 0, 1
                    Case 2
                        Select Case NewTrailingTrivia.First.RawKind
                            Case VB.SyntaxKind.EndOfLineTrivia
                                If NewTrailingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                    NewTrailingTrivia.RemoveAt(1)
                                ElseIf NewTrailingTrivia.Last.IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                                Else
                                    Stop
                                End If
                            Case VB.SyntaxKind.WhitespaceTrivia
                                If NewTrailingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                    NewTrailingTrivia.RemoveAt(1)
                                ElseIf NewTrailingTrivia.Last.IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                                    NewTrailingTrivia.RemoveAt(1)
                                Else
                                    Stop
                                End If
                            Case Else
                                Stop
                        End Select
                    Case Else

                End Select
                Dim invocationExpressionSyntax1 As InvocationExpressionSyntax = VBFactory.InvocationExpression(Expression, ArgumentList1).WithConvertedLeadingTriviaFrom(node).WithTrailingTrivia(NewTrailingTrivia)
                Return invocationExpressionSyntax1
            End Function

            ''' <summary>
            '''
            ''' </summary>
            ''' <param name="node"></param>
            ''' <returns></returns>
            ''' <remarks>Added by PC</remarks>
            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
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

                    Dim VariableType As TypeSyntax = CType(DeclarationPattern.Type.Accept(Me), TypeSyntax)
                    Dim value As ExpressionSyntax = VBFactory.TryCastExpression(VBExpression, VariableType)

                    Dim Identifier As SyntaxToken = GenerateSafeVBToken(id:=Designation.Identifier, IsQualifiedName:=False, IsTypeName:=False)
                    Dim VariableName As ModifiedIdentifierSyntax = VBFactory.ModifiedIdentifier(Identifier.WithTrailingTrivia(SpaceTrivia))
                    Dim SeparatedSyntaxListOfModifiedIdentifier As SeparatedSyntaxList(Of ModifiedIdentifierSyntax) =
                        VBFactory.SingletonSeparatedList(
                            VariableName)

                    Dim SeparatedListOfvariableDeclarations As SeparatedSyntaxList(Of VariableDeclaratorSyntax) =
                        VBFactory.SingletonSeparatedList(
                            node:=VBFactory.VariableDeclarator(
                                names:=SeparatedSyntaxListOfModifiedIdentifier,
                                asClause:=VBFactory.SimpleAsClause(VariableType),
                                initializer:=VBFactory.EqualsValue(value)
                                    )
                             )

                    Dim DeclarationToBeAdded As LocalDeclarationStatementSyntax = VBFactory.LocalDeclarationStatement(DimModifier,
                                                                                                                          SeparatedListOfvariableDeclarations
                                                                                                                          ).WithTrailingTrivia(VB_EOLTrivia).
                                                                                                                          WithAdditionalAnnotations(Simplifier.Annotation)
                    If ReportCheckCorrectness Then
                        DeclarationToBeAdded = DeclarationToBeAdded.WithPrependedLeadingTrivia(
                                                                        CheckCorrectnessLeadingTrivia(
                                                                                StatementWithIssue,
                                                                                "VB has no direct equivalent To C# pattern variables 'is' expressions")
                                                                                )
                    End If

                    StatementWithIssue.AddMarker(Statement:=DeclarationToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                    Return VBFactory.IsNotExpression(VBFactory.IdentifierName(Identifier.ToString), NothingExpression)
                ElseIf TypeOf Pattern Is CSS.ConstantPatternSyntax Then
                    Dim ConstantPattern As CSS.ConstantPatternSyntax = DirectCast(node.Pattern, CSS.ConstantPatternSyntax)
                    If ConstantPattern.Expression.IsKind(CS.SyntaxKind.NullLiteralExpression) Then
                        Return VBFactory.IsExpression(left:=VBExpression, right:=NothingExpression)
                    End If
                    Return VBFactory.EqualsExpression(left:=VBExpression, right:=DirectCast(ConstantPattern.Expression.Accept(Me), ExpressionSyntax))
                ElseIf TypeOf Pattern Is CSS.VarPatternSyntax Then
                    Dim VarPattern As CSS.VarPatternSyntax = DirectCast(Pattern, CSS.VarPatternSyntax)
                    Dim SingleVariableDesignation As CSS.SingleVariableDesignationSyntax = DirectCast(VarPattern.Designation, CSS.SingleVariableDesignationSyntax)
                    Dim Identifier As SyntaxToken = GenerateSafeVBToken(id:=SingleVariableDesignation.Identifier, IsQualifiedName:=False, IsTypeName:=False)
                    Dim Name As IdentifierNameSyntax = VBFactory.IdentifierName(Identifier.ToString)

                    Dim VariableName As ModifiedIdentifierSyntax = VBFactory.ModifiedIdentifier(Identifier.WithTrailingTrivia(SpaceTrivia))

                    Dim Declarators As SeparatedSyntaxList(Of VariableDeclaratorSyntax) = VBFactory.SingletonSeparatedList(
                            node:=VBFactory.VariableDeclarator(VBFactory.SingletonSeparatedList(
                            VariableName),
                            asClause:=Nothing, initializer:=VBFactory.EqualsValue(VBExpression)).WithTrailingEOL
                             )

                    Dim LeadingTrivia As SyntaxTriviaList = CheckCorrectnessLeadingTrivia(StatementWithIssue, "VB has no direct equivalent To C# var pattern expressions")
                    Dim DeclarationToBeAdded As LocalDeclarationStatementSyntax = VBFactory.LocalDeclarationStatement(DimModifier,
                        Declarators).WithAdditionalAnnotations(Simplifier.Annotation)
                    If ReportCheckCorrectness Then
                        DeclarationToBeAdded = DeclarationToBeAdded.WithPrependedLeadingTrivia(CheckCorrectnessLeadingTrivia(StatementWithIssue, "VB has no direct equivalent To C# var pattern expressions")).WithTrailingEOL
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

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitLiteralExpression(node As CSS.LiteralExpressionSyntax) As VB.VisualBasicSyntaxNode
                ' now this looks somehow like a hack... is there a better way?
                If node.IsKind(CS.SyntaxKind.StringLiteralExpression) Then
                    ' @"" have no escapes except quotes (ASCII and Unicode)
                    If node.Token.Text.StartsWith("@", StringComparison.InvariantCulture) Then
                        Return VBFactory.StringLiteralExpression(
                                                    token:=VBFactory.StringLiteralToken(
                                                    text:=node.Token.Text.
                                                        Substring(1).
                                                        Replace(UnicodeOpenQuote, UnicodeDoubleOpenQuote, StringComparison.InvariantCulture).
                                                        Replace(UnicodeCloseQuote, UnicodeDoubleCloseQuote, StringComparison.InvariantCulture).
                                                        NormalizeLineEndings,
                                                    value:=DirectCast(node.Token.Value, String).
                                                        Replace(Quote, DoubleQuote, StringComparison.InvariantCulture).
                                                        Replace(UnicodeOpenQuote, UnicodeDoubleOpenQuote, StringComparison.InvariantCulture).
                                                        Replace(UnicodeCloseQuote, UnicodeDoubleCloseQuote, StringComparison.InvariantCulture).NormalizeLineEndings)
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
                            Dim LeftNodeTypeInfo As TypeInfo = ModelExtensions.GetTypeInfo(mSemanticModel, DirectCast(node.Parent, CSS.AssignmentExpressionSyntax).Left)
                            If LeftNodeTypeInfo.Type Is Nothing OrElse LeftNodeTypeInfo.Type.IsErrorType Then
                                Return NothingExpression
                            End If
                            If LeftNodeTypeInfo.Type?.IsTupleType Then
                                Dim TypeList As New List(Of String)
                                Dim TupleNameString As String = LeftNodeTypeInfo.Type.ToString
                                TupleNameString = TupleNameString.Substring(1, TupleNameString.Length - 2)
                                For Each s As String In TupleNameString.Split(","c)
                                    If s.Trim.Contains(" ", StringComparison.InvariantCulture) Then
                                        TypeList.Add(AddBracketsIfRequired(s.Trim.Split(" "c)(0).
                                                                           Replace("<", "(Of ", StringComparison.InvariantCulture).
                                                                           Replace(">", ")", StringComparison.InvariantCulture)))
                                    Else
                                        TypeList.Add(AddBracketsIfRequired(s.Trim.
                                                                           Replace("<", "(Of ", StringComparison.InvariantCulture).
                                                                           Replace(">", ")", StringComparison.InvariantCulture)))
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
                            Dim LeftNodeTypeInfo As TypeInfo = ModelExtensions.GetTypeInfo(mSemanticModel, DirectCast(node.Parent, CSS.ConditionalExpressionSyntax).WhenTrue)
                            If LeftNodeTypeInfo.Type Is Nothing OrElse LeftNodeTypeInfo.Type.IsErrorType Then
                                Return NothingExpression
                            End If
                            Dim _Type As TypeSyntax = If(LeftNodeTypeInfo.Type.IsTupleType, CovertStringToTupleType(LeftNodeTypeInfo.Type.ToString), ConvertToType(LeftNodeTypeInfo.Type.Name))
                            Return VBFactory.CTypeExpression(
                                                      NothingExpression,
                                                      _Type
                                                      )
                        Case CS.SyntaxKind.EqualsExpression, CS.SyntaxKind.NotEqualsExpression
                            Return VBFactory.CTypeExpression(
                                                      NothingExpression,
                                                      ConvertToType("bool")
                                                      )
                        Case CS.SyntaxKind.Argument, CS.SyntaxKind.SimpleLambdaExpression
                            Return NothingExpression
                        Case CS.SyntaxKind.ArrowExpressionClause, CS.SyntaxKind.CoalesceExpression, CS.SyntaxKind.ParenthesizedLambdaExpression, CS.SyntaxKind.SwitchExpressionArm
                            ' TODO Handle better
                            Return NothingExpression
                        Case CS.SyntaxKind.SuppressNullableWarningExpression
                            ' TODO Handle better
                            Return NothingExpression
                        Case Else
                            Stop
                    End Select
                    ' TODO Handle better
                    Return NothingExpression
                End If

                If node.IsKind(CS.SyntaxKind.CharacterLiteralExpression) Then
                    If node.Token.Text.Replace("'", "", StringComparison.InvariantCulture).Length <= 2 Then
                        Return GetLiteralExpression(node.Token.Value, node.Token, Me).WithConvertedTriviaFrom(node.Token)
                    End If
                End If
                If node.Token.ValueText.Contains("\", StringComparison.InvariantCulture) Then
                    Stop
                End If

                Return GetLiteralExpression(node.Token.Value, node.Token, Me).WithConvertedTriviaFrom(node.Token)
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitMemberAccessExpression(node As CSS.MemberAccessExpressionSyntax) As VB.VisualBasicSyntaxNode
                If node.IsKind(CS.SyntaxKind.PointerMemberAccessExpression) Then
                    Dim StatementWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                    StatementWithIssues.AddMarker(FlagUnsupportedStatements(StatementWithIssues, $"Pointer Member Access Expressions", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=False)
                End If

                Dim Expression As ExpressionSyntax = DirectCast(node.Expression.Accept(Me), ExpressionSyntax).WithConvertedTriviaFrom(node.Expression)

                If TypeOf Expression Is NewExpressionSyntax AndAlso Not TypeOf Expression Is ArrayCreationExpressionSyntax Then
                    Dim UniqueName As String = MethodBodyVisitor.GetUniqueVariableNameInScope(node, "tempVar", mSemanticModel)
                    Dim UniqueIdentifier As IdentifierNameSyntax = VBFactory.IdentifierName(VBFactory.Identifier(UniqueName))
                    Dim Names As SeparatedSyntaxList(Of ModifiedIdentifierSyntax) = VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(UniqueName))
                    Dim AsClause As AsClauseSyntax = VBFactory.AsNewClause(DirectCast(Expression.With({SpaceTrivia}, {SpaceTrivia}), NewExpressionSyntax))
                    Dim VariableDeclaration As SeparatedSyntaxList(Of VariableDeclaratorSyntax) = VBFactory.SingletonSeparatedList(VBFactory.VariableDeclarator(Names, AsClause, initializer:=Nothing))
                    Dim DimStatement As LocalDeclarationStatementSyntax = VBFactory.LocalDeclarationStatement(DimModifier, VariableDeclaration).WithLeadingTrivia(Expression.GetLeadingTrivia).WithTrailingEOL
                    Dim StatementWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                    StatementWithIssues.AddMarker(DimStatement.WithTrailingEOL, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                    Expression = UniqueIdentifier.WithLeadingTrivia(Expression.GetLeadingTrivia.Last).WithTrailingTrivia(Expression.GetTrailingTrivia)
                ElseIf TypeOf Expression Is CollectionInitializerSyntax Then
                    Dim UniqueName As String = MethodBodyVisitor.GetUniqueVariableNameInScope(node, "tempVar", mSemanticModel)
                    Dim UniqueIdentifier As IdentifierNameSyntax = VBFactory.IdentifierName(VBFactory.Identifier(UniqueName))
                    Dim Names As SeparatedSyntaxList(Of ModifiedIdentifierSyntax) = VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(UniqueName))
                    Dim AsClause As AsClauseSyntax = Nothing
                    Dim Initializer As EqualsValueSyntax = VBFactory.EqualsValue(Expression)
                    Dim VariableDeclaration As SeparatedSyntaxList(Of VariableDeclaratorSyntax) = VBFactory.SingletonSeparatedList(VBFactory.VariableDeclarator(Names, asClause:=AsClause, Initializer))
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
                    For i As Integer = 0 To OldNameLeadingTrivia.Count - 1
                        Dim t As SyntaxTrivia = OldNameLeadingTrivia(i)
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
                    OperatorTrailingTrivia.Add(VB_EOLTrivia)
                End If
                Dim OperatorToken As SyntaxToken = DotToken.With(ConvertTrivia(node.OperatorToken.LeadingTrivia).ToList, OperatorTrailingTrivia)
                Dim Name As SimpleNameSyntax = DirectCast(node.Name.Accept(Me).With(NewNameLeadingTrivia, ConvertTrivia(node.Name.GetTrailingTrivia)), SimpleNameSyntax)
                OperatorTrailingTrivia.Clear()

                If Expression.GetLastToken.ContainsEOLTrivia Then
                    Dim FoundEOL As Boolean = False
                    FoundEOL = RestructureTrivia(TriviaList:=Expression.GetTrailingTrivia, FoundEOL, OperatorTrailingTrivia)
                    FoundEOL = RestructureTrivia(TriviaList:=OperatorToken.LeadingTrivia, FoundEOL, OperatorTrailingTrivia)

                    If FoundEOL Then
                        OperatorTrailingTrivia.Add(VB_EOLTrivia)
                    End If
                    Expression = Expression.WithoutTrailingTrivia
                    OperatorToken = OperatorToken.WithoutTrivia.WithTrailingTrivia(OperatorTrailingTrivia)
                    Name = Name.WithLeadingTrivia(SpaceTrivia)
                End If
                Return WrapTypedNameIfNecessary(name:=VBFactory.MemberAccessExpression(VB.SyntaxKind.SimpleMemberAccessExpression, Expression, OperatorToken, Name), originalName:=node).WithConvertedTriviaFrom(node)
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitMemberBindingExpression(node As CSS.MemberBindingExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim name As SimpleNameSyntax = DirectCast(node.Name.Accept(Me), SimpleNameSyntax)
                Return VBFactory.SimpleMemberAccessExpression(name:=name)
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitObjectCreationExpression(node As CSS.ObjectCreationExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim type1 As TypeSyntax = DirectCast(node.Type.Accept(Me), TypeSyntax)

                Dim argumentList As ArgumentListSyntax = DirectCast(node.ArgumentList?.Accept(Me), ArgumentListSyntax)
                If argumentList IsNot Nothing Then
                    If type1.ToString.EndsWith("EventHandler", StringComparison.InvariantCulture) AndAlso
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

                    For I As Integer = 0 To argumentList.Arguments.Count - 2
                        NewArgumentList.Add(argumentList.Arguments(I).RemoveDirectiveTrivia(FoundEOL))
                        NewSeparatorList.Add(argumentList.Arguments.GetSeparator(I).RemoveDirectiveTrivia(FoundEOL))
                    Next
                    NewArgumentList.Add(argumentList.Arguments(argumentList.Arguments.Count - 1).RemoveDirectiveTrivia(FoundEOL))
                    argumentList = argumentList.WithArguments(VBFactory.SeparatedList(NewArgumentList, NewSeparatorList))
                End If
                Return VBFactory.ObjectCreationExpression(VBFactory.List(Of AttributeListSyntax)(), type1, argumentList, initializer)
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
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
                        Dim UniqueName As String = MethodBodyVisitor.GetUniqueVariableNameInScope(node, "tempVar", mSemanticModel)
                        Dim Names As SeparatedSyntaxList(Of ModifiedIdentifierSyntax) = VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(UniqueName))
                        Dim UniqueIdentifier As IdentifierNameSyntax = VBFactory.IdentifierName(VBFactory.Identifier(UniqueName))
                        If TypeOf Expression Is TernaryConditionalExpressionSyntax Then
                            Dim TExpression As TernaryConditionalExpressionSyntax = DirectCast(Expression, TernaryConditionalExpressionSyntax)
                            If TExpression.Condition.IsKind(VB.SyntaxKind.IdentifierName) Then
                                Dim IfStatement As IfStatementSyntax =
                                   VBFactory.IfStatement(IfKeyword, TExpression.Condition, ThenKeyword).WithConvertedLeadingTriviaFrom(node)
                                Dim EndIfStatement As EndBlockStatementSyntax = VBFactory.EndIfStatement(EndKeyword, IfKeyword).WithConvertedTrailingTriviaFrom(node)
                                Dim IfBlockStatements As New SyntaxList(Of StatementSyntax)
                                IfBlockStatements = IfBlockStatements.Add(VBFactory.SimpleAssignmentStatement(left:=UniqueIdentifier, right:=TExpression.WhenTrue))
                                Dim ElseBlockStatements As New SyntaxList(Of StatementSyntax)
                                ElseBlockStatements = ElseBlockStatements.Add(VBFactory.SimpleAssignmentStatement(left:=UniqueIdentifier, right:=TExpression.WhenFalse))
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

                                StatementWithIssue.AddMarker(DeclarationToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=False)
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
                        VariableDeclaration = VBFactory.VariableDeclarator(Names, asClause:=Nothing, Initializer)
                        DeclarationToBeAdded =
                                   VBFactory.LocalDeclarationStatement(
                                    DimModifier,
                                    VBFactory.SingletonSeparatedList(VariableDeclaration)
                                    ).WithPrependedLeadingTrivia(VBFactory.CommentTrivia($" ' TODO: Check, VB does not directly support MemberAccess off a Conditional If Expression")).WithTrailingEOL

                        StatementWithIssue.AddMarker(DeclarationToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=False)
                        Return UniqueIdentifier
                    ElseIf TypeOf node.Parent Is CSS.ConditionalAccessExpressionSyntax Then
                        Dim UniqueName As String = MethodBodyVisitor.GetUniqueVariableNameInScope(node, "tempVar", mSemanticModel)
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

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitParenthesizedLambdaExpression(node As CSS.ParenthesizedLambdaExpressionSyntax) As VB.VisualBasicSyntaxNode
                Return ConvertLambdaExpression(node, node.Body, node.ParameterList.Parameters, VBFactory.TokenList(node.AsyncKeyword))
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitParenthesizedVariableDesignation(node As CSS.ParenthesizedVariableDesignationSyntax) As VB.VisualBasicSyntaxNode
                Dim Variables As New List(Of ModifiedIdentifierSyntax)
                For i As Integer = 0 To node.Variables.Count - 1
                    Dim VariableDeclarator As ModifiedIdentifierSyntax = VBFactory.ModifiedIdentifier(node.Variables(i).ToString)
                    Variables.Add(VariableDeclarator)
                Next

                Dim Names As SeparatedSyntaxList(Of ModifiedIdentifierSyntax) = VBFactory.SeparatedList(Variables)
                Return VBFactory.VariableDeclarator(Names, VBFactory.SimpleAsClause(PredefinedTypeObject), initializer:=Nothing)
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitPostfixUnaryExpression(node As CSS.PostfixUnaryExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim CSExpressionKind As CS.SyntaxKind = CS.CSharpExtensions.Kind(node)
                Dim OperandExpression As ExpressionSyntax = DirectCast(node.Operand.Accept(Me), ExpressionSyntax)
                If CSExpressionKind = CS.SyntaxKind.SuppressNullableWarningExpression Then
                    Return OperandExpression
                End If
                Dim kind As VB.SyntaxKind = ConvertCSExpressionsKindToVBKind(CSExpressionKind)
                If TypeOf node.Parent Is CSS.ExpressionStatementSyntax OrElse TypeOf node.Parent Is CSS.ForStatementSyntax Then
                    Return VBFactory.AssignmentStatement(ConvertCSExpressionsKindToVBKind(CS.CSharpExtensions.Kind(node)),
                                                            OperandExpression,
                                                            ExpressionKindToOperatorToken(kind),
                                                            Expression_1)
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
                    Dim MathExpression As NameSyntax = VBFactory.ParseName("Math." & minMax)
                    Dim InterlockedExpressionName As NameSyntax = VBFactory.ParseName("System.Threading.Interlocked." & OperatorName)

                    Dim OperandArgument As SimpleArgumentSyntax = VBFactory.SimpleArgument(OperandExpression)

                    Dim OperandArgumentList As ArgumentListSyntax = VBFactory.ArgumentList(VBFactory.SingletonSeparatedList(Of ArgumentSyntax)(OperandArgument))
                    Dim ArgumentInvocationExpression As InvocationExpressionSyntax = VBFactory.InvocationExpression(InterlockedExpressionName, OperandArgumentList)
                    Dim SecondArgumentSyntax As SimpleArgumentSyntax = VBFactory.SimpleArgument(VBFactory.BinaryExpression(op,
                                                                                                                                   OperandExpression,
                                                                                                                                   ExpressionKindToOperatorToken(op),
                                                                                                                                   Expression_1)
                                                                                                                                   )
                    Return VBFactory.InvocationExpression(
                        MathExpression,
                        VBFactory.ArgumentList(VBFactory.SeparatedList((New ArgumentSyntax() {VBFactory.SimpleArgument(ArgumentInvocationExpression),
                                                                                                      SecondArgumentSyntax})))).WithConvertedTriviaFrom(node)
                End If
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitPrefixUnaryExpression(node As CSS.PrefixUnaryExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim kind As VB.SyntaxKind = ConvertCSExpressionsKindToVBKind(CS.CSharpExtensions.Kind(node))
                If kind = CS.SyntaxKind.PointerIndirectionExpression Then
                    Dim StatementWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                    StatementWithIssues.AddMarker(FlagUnsupportedStatements(StatementWithIssues, "IndirectPointer Expressions", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=False)
                    Return NothingExpression
                End If
                Dim OperandExpression As ExpressionSyntax = DirectCast(node.Operand.Accept(Me), ExpressionSyntax)
                If TypeOf node.Parent Is CSS.ExpressionStatementSyntax Then
                    Return VBFactory.AssignmentStatement(kind,
                                                             OperandExpression,
                                                             ExpressionKindToOperatorToken(kind),
                                                             Expression_1).WithConvertedTriviaFrom(node)
                End If
                If kind = VB.SyntaxKind.AddAssignmentStatement OrElse kind = VB.SyntaxKind.SubtractAssignmentStatement Then
                    If node.Parent.IsKind(CS.SyntaxKind.ForStatement) Then
                        If kind = VB.SyntaxKind.AddAssignmentStatement Then
                            Return VBFactory.AddAssignmentStatement(OperandExpression.WithTrailingTrivia(SpaceTrivia),
                                                                        ExpressionKindToOperatorToken(kind),
                                                                        Expression_1).WithConvertedTriviaFrom(node)
                        Else
                            Return VBFactory.SubtractAssignmentStatement(OperandExpression.WithTrailingTrivia(SpaceTrivia),
                                                                             ExpressionKindToOperatorToken(kind),
                                                                             Expression_1).WithConvertedTriviaFrom(node)
                        End If
                    Else
                        Dim operatorName As String = If(kind = VB.SyntaxKind.AddAssignmentStatement, "Increment", "Decrement")
                        Dim MathExpression As NameSyntax = VBFactory.ParseName("System.Threading.Interlocked." & operatorName)
                        Return VBFactory.InvocationExpression(MathExpression, VBFactory.ArgumentList(VBFactory.SeparatedList((New ArgumentSyntax() {VBFactory.SimpleArgument(OperandExpression)}))))
                    End If
                End If
                If kind = VB.SyntaxKind.AddressOfExpression Then
                    Dim SpaceTriviaList As SyntaxTriviaList
                    SpaceTriviaList = SpaceTriviaList.Add(SpaceTrivia)
                    Dim AddressOfToken As SyntaxToken = AddressOfKeyword.With(SpaceTriviaList, SpaceTriviaList)
                    Return VBFactory.AddressOfExpression(AddressOfToken, OperandExpression).WithConvertedTriviaFrom(node)
                End If
                Return VBFactory.UnaryExpression(kind,
                                                     ExpressionKindToOperatorToken(kind),
                                                     OperandExpression.WithLeadingTrivia(SpaceTrivia)
                                                     ).WithConvertedTriviaFrom(node)
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitSimpleLambdaExpression(node As CSS.SimpleLambdaExpressionSyntax) As VB.VisualBasicSyntaxNode
                Return ConvertLambdaExpression(node, node.Body, VBFactory.SingletonSeparatedList(node.Parameter), VBFactory.TokenList(node.AsyncKeyword)).WithConvertedTriviaFrom(node)
            End Function

            ''' <summary>
            ''' Maps sizeof to Len(New {Type})
            ''' </summary>
            ''' <param name="node"></param>
            ''' <returns></returns>ThrowExpressionSyntax
            ''' <remarks>Added by PC</remarks>
            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitSizeOfExpression(node As CSS.SizeOfExpressionSyntax) As VB.VisualBasicSyntaxNode
                Return VBFactory.ParseExpression($"Len(New {node.Type.ToString}()) ")
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitThisExpression(node As CSS.ThisExpressionSyntax) As VB.VisualBasicSyntaxNode
                Return VBFactory.MeExpression().WithConvertedTriviaFrom(node)
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitThrowExpression(node As CSS.ThrowExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim Expression As ExpressionSyntax = DirectCast(node.Expression.Accept(Me), ExpressionSyntax)
                Dim ThrowStatement As ThrowStatementSyntax = VBFactory.ThrowStatement(Expression).WithTrailingEOL
                Dim ParentNode As SyntaxNode = node.Parent
                Return ThrowStatement.WithTrailingEOL
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitTupleElement(node As CSS.TupleElementSyntax) As VB.VisualBasicSyntaxNode
                Try
                    If String.IsNullOrWhiteSpace(node.Identifier.ValueText) Then
                        Dim typedTupleElementSyntax1 As TypedTupleElementSyntax = VBFactory.TypedTupleElement(DirectCast(node.Type.Accept(Me), TypeSyntax))
                        Return typedTupleElementSyntax1
                    End If
                    Dim namedTupleElementSyntax1 As NamedTupleElementSyntax = VBFactory.NamedTupleElement(GenerateSafeVBToken(node.Identifier, IsQualifiedName:=False, IsTypeName:=False).WithConvertedTriviaFrom(node.Type), VBFactory.SimpleAsClause(DirectCast(node.Type.Accept(Me).WithConvertedTriviaFrom(node.Identifier), TypeSyntax)))
                    Return namedTupleElementSyntax1
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
            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitTupleExpression(node As CSS.TupleExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim lArgumentSyntax As New List(Of SimpleArgumentSyntax)
                If TypeOf node.Arguments(0).Expression IsNot CSS.DeclarationExpressionSyntax Then
                    For Each a As CSS.ArgumentSyntax In node.Arguments
                        Dim Argument As SimpleArgumentSyntax = DirectCast(a.Accept(Me), SimpleArgumentSyntax)
                        Dim AfterWhiteSpace As Boolean = False
                        Dim InitialTriviaList As List(Of SyntaxTrivia) = Argument.GetLeadingTrivia.ToList
                        Dim TriviaListUBound As Integer = InitialTriviaList.Count - 1
                        Dim FinalLeadingTriviaList As New List(Of SyntaxTrivia)
                        For i As Integer = 0 To TriviaListUBound
                            Dim Trivia As SyntaxTrivia = InitialTriviaList(i)
                            Select Case Trivia.RawKind
                                Case VB.SyntaxKind.WhitespaceTrivia
                                    AfterWhiteSpace = True
                                    FinalLeadingTriviaList.Add(Trivia)
                                Case VB.SyntaxKind.EndOfLineTrivia
                                    FinalLeadingTriviaList.Add(Trivia)
                                    AfterWhiteSpace = False
                                    If i < TriviaListUBound Then
                                        If FinalLeadingTriviaList.Count = 0 Then
                                            FinalLeadingTriviaList.Add(SpaceTrivia)
                                            FinalLeadingTriviaList.Add(LineContinuation)
                                        End If
                                    End If
                                Case VB.SyntaxKind.CommentTrivia
                                    If Not FinalLeadingTriviaList.Last.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                                        If Not AfterWhiteSpace Then
                                            FinalLeadingTriviaList.Add(SpaceTrivia)
                                        End If
                                        FinalLeadingTriviaList.Add(LineContinuation)
                                    End If

                                    FinalLeadingTriviaList.Add(Trivia)
                                    If i < TriviaListUBound AndAlso Not InitialTriviaList(i + 1).IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                        FinalLeadingTriviaList.Add(VB_EOLTrivia)
                                    End If
                                Case VB.SyntaxKind.DisableWarningDirectiveTrivia
                                    GetStatementwithIssues(node).AddMarker(VB.SyntaxFactory.EmptyStatement.WithLeadingTrivia(Trivia), StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                                Case VB.SyntaxKind.EnableWarningDirectiveTrivia
                                    GetStatementwithIssues(node).AddMarker(VB.SyntaxFactory.EmptyStatement.WithLeadingTrivia(Trivia), StatementHandlingOption.AppendEmptyStatement, AllowDuplicates:=True)
                                Case VB.SyntaxKind.LineContinuationTrivia
                                    If FinalLeadingTriviaList.Last.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                                        Continue For
                                    End If
                                    AfterWhiteSpace = False
                                    FinalLeadingTriviaList.Add(LineContinuation)
                                Case Else
                                    Stop
                            End Select
                        Next
                        lArgumentSyntax.Add(Argument.WithLeadingTrivia(FinalLeadingTriviaList))
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

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitTupleType(node As CSS.TupleTypeSyntax) As VB.VisualBasicSyntaxNode
                Dim SSList As New List(Of TupleElementSyntax)
                SSList.AddRange(node.Elements.Select(Function(a As CSS.TupleElementSyntax) DirectCast(a.Accept(Me), TupleElementSyntax)))
                Return VBFactory.TupleType(SSList.ToArray)
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
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
                    Stop
                    VBSyntaxNode = DirectCast(VBSyntaxNode, UnaryExpressionSyntax).Operand
                End If
                Return VBFactory.GetTypeExpression(DirectCast(VBSyntaxNode, TypeSyntax))
            End Function

        End Class

    End Class

End Namespace
