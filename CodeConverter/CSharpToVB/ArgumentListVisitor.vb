' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

Imports CSharpToVBCodeConverter.Util

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBCodeConverter.Visual_Basic

    Partial Public Class CSharpConverter

        Partial Friend Class NodesVisitor
            Inherits CS.CSharpSyntaxVisitor(Of VB.VisualBasicSyntaxNode)

            Private Function VisitCSArguments(CS_OpenToken As SyntaxToken, CS_Arguments As SeparatedSyntaxList(Of CSS.ArgumentSyntax), CS_CloseToken As SyntaxToken) As VB.VisualBasicSyntaxNode
                If CS_Arguments.Count = 0 Then
                    Return VBFactory.ArgumentList(VBFactory.SeparatedList(CS_Arguments.Select(Function(a As CSS.ArgumentSyntax) DirectCast(a.Accept(Me), VBS.ArgumentSyntax))))
                End If
                Dim NodeList As New List(Of VBS.ArgumentSyntax)
                Dim Separators As New List(Of SyntaxToken)
                Dim SeparatorCount As Integer = CS_Arguments.Count - 1
                For i As Integer = 0 To SeparatorCount
                    NodeList.Add(DirectCast(CS_Arguments(i).Accept(Me), VBS.ArgumentSyntax).WithModifiedNodeTrivia(SeparatorFollows:=SeparatorCount > i))
                    If SeparatorCount > i Then
                        Separators.Add(CommaToken.WithConvertedTrailingTriviaFrom(CS_Arguments.GetSeparators()(i)))
                    End If
                Next
                Dim OpenParenTokenWithTrivia As SyntaxToken = OpenParenToken.WithConvertedTriviaFrom(CS_OpenToken)
                Dim CloseParenTokenWithTrivia As SyntaxToken = CloseParenToken.WithConvertedTriviaFrom(CS_CloseToken)
                RestructureNodesAndSeparators(OpenParenTokenWithTrivia, NodeList, Separators, CloseParenTokenWithTrivia)
                Return VBFactory.ArgumentList(
                                                  OpenParenTokenWithTrivia,
                                                  VBFactory.SeparatedList(NodeList, Separators),
                                                  CloseParenTokenWithTrivia
                                                  )
            End Function

            Public Overrides Function VisitArgument(node As CSS.ArgumentSyntax) As VB.VisualBasicSyntaxNode
                Dim name As VBS.NameColonEqualsSyntax = Nothing
                Dim NodeExpression As CSS.ExpressionSyntax = node?.Expression
                Dim ArgumentWithTrivia As VBS.ExpressionSyntax = Nothing
                Dim NewLeadingTrivia As New List(Of SyntaxTrivia)
                Dim NewTrailingTrivia As New List(Of SyntaxTrivia)
                Try
                    If node.RefKindKeyword.Text = "ref" Then
                        Dim Expression As VBS.ExpressionSyntax = DirectCast(node.Expression.Accept(Me), VBS.ExpressionSyntax)
                        Dim IdentifierString As String = Expression.ToString.Replace("[", "", StringComparison.InvariantCulture).Replace("]", "", StringComparison.InvariantCulture)
                        Dim StatementWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                        StatementWithIssues.AddMarker(FlagUnsupportedStatements(StatementWithIssues, $"ref keyword, fix variables starting with 'HandleRef_' below", CommentOutOriginalStatements:=False), StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                        ArgumentWithTrivia = VBFactory.ParseExpression($"HandleRef_{IdentifierString}")
                    ElseIf NodeExpression.IsKind(CS.SyntaxKind.CoalesceExpression) Then
                        Dim CS_BinaryExpression As CSS.BinaryExpressionSyntax = DirectCast(NodeExpression, CSS.BinaryExpressionSyntax)
                        If CS_BinaryExpression.Right.IsKind(CS.SyntaxKind.ThrowExpression) Then
                            Dim TestNode As VBS.ExpressionSyntax = DirectCast(CS_BinaryExpression.Left.Accept(Me).WithConvertedTriviaFrom(CS_BinaryExpression.Left), VBS.ExpressionSyntax)
                            Dim SecondExpression As VBS.ThrowStatementSyntax = DirectCast(CS_BinaryExpression.Right.Accept(Me).WithConvertedTriviaFrom(CS_BinaryExpression.Right), VBS.ThrowStatementSyntax)
                            Dim Statements As SyntaxList(Of VBS.StatementSyntax) = VBFactory.SingletonList(Of VBS.StatementSyntax)(SecondExpression)

                            Dim Condition As VBS.ExpressionSyntax = VBFactory.IsExpression(TestNode, NothingExpression)
                            Dim IfBlock As VBS.SingleLineIfStatementSyntax = VBFactory.SingleLineIfStatement(Condition,
                                                                                                              Statements,
                                                                                                              elseClause:=Nothing)
                            Dim StatementWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                            StatementWithIssues.AddMarker(IfBlock, StatementHandlingOption.PrependStatement, AllowDuplicates:=False)
                            ArgumentWithTrivia = DirectCast(CS_BinaryExpression.Left.Accept(Me).WithConvertedTriviaFrom(CS_BinaryExpression.Left).WithModifiedNodeTrivia(SeparatorFollows:=True), VBS.ExpressionSyntax)
                        Else
                            ArgumentWithTrivia = DirectCast(NodeExpression.Accept(Me).WithModifiedNodeTrivia(SeparatorFollows:=True), VBS.ExpressionSyntax)
                        End If
                    Else
                        ArgumentWithTrivia = DirectCast(NodeExpression.Accept(Me).WithModifiedNodeTrivia(SeparatorFollows:=True), VBS.ExpressionSyntax)
                    End If

                    If TypeOf node.Parent Is CSS.BracketedArgumentListSyntax Then
                        Dim _Typeinfo As TypeInfo = ModelExtensions.GetTypeInfo(_mSemanticModel, NodeExpression)
                        If Not SymbolEqualityComparer.Default.Equals(_Typeinfo.ConvertedType, _Typeinfo.Type) Then
                            If _Typeinfo.Type?.SpecialType = SpecialType.System_Char Then '
                                ArgumentWithTrivia = VBFactory.ParseExpression($"ChrW({ArgumentWithTrivia.WithoutTrivia.ToString})").WithTriviaFrom(ArgumentWithTrivia)
                            End If
                        End If
                    End If

                    If node.NameColon IsNot Nothing Then
                        name = VBFactory.NameColonEquals(DirectCast(node.NameColon.Name.Accept(Me), VBS.IdentifierNameSyntax))
                        Dim NameWithOutColon As String = name.Name.ToString.Replace(":=", "", StringComparison.InvariantCulture)
                        If NameWithOutColon.EndsWith("_Renamed", StringComparison.InvariantCulture) Then
                            name = VBFactory.NameColonEquals(VBFactory.IdentifierName(NameWithOutColon.Replace("_Renamed", "", StringComparison.InvariantCulture)))
                        End If
                    End If

                    If ArgumentWithTrivia.HasLeadingTrivia Then
                        For Each trivia As SyntaxTrivia In ArgumentWithTrivia.GetLeadingTrivia
                            Select Case trivia.RawKind
                                Case VB.SyntaxKind.WhitespaceTrivia, VB.SyntaxKind.EndOfLineTrivia,
                                     VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.IfDirectiveTrivia,
                                     VB.SyntaxKind.DisabledTextTrivia, VB.SyntaxKind.ElseDirectiveTrivia,
                                     VB.SyntaxKind.ElseIfDirectiveTrivia, VB.SyntaxKind.EndIfDirectiveTrivia
                                    NewLeadingTrivia.Add(trivia)
                                Case VB.SyntaxKind.DisableWarningDirectiveTrivia
                                    GetStatementwithIssues(node).AddMarker(VBFactory.EmptyStatement.WithLeadingTrivia(trivia), StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                                Case VB.SyntaxKind.EnableWarningDirectiveTrivia
                                    GetStatementwithIssues(node).AddMarker(VBFactory.EmptyStatement.WithLeadingTrivia(trivia), StatementHandlingOption.AppendEmptyStatement, AllowDuplicates:=True)
                                Case VB.SyntaxKind.LineContinuationTrivia
                                    If NewLeadingTrivia.Last.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                                        Continue For
                                    End If
                                    NewLeadingTrivia.Add(LineContinuation)
                                Case Else
                                    Stop
                            End Select
                        Next
                    End If
                    NewTrailingTrivia.AddRange(ArgumentWithTrivia.GetTrailingTrivia)
                Catch ex As OperationCanceledException
                    Throw
                Catch ex As Exception
                    Stop
                End Try
                ArgumentWithTrivia = ArgumentWithTrivia.WithLeadingTrivia(NewLeadingTrivia).WithTrailingTrivia(SpaceTrivia)
                Return VBFactory.SimpleArgument(name, ArgumentWithTrivia).WithTrailingTrivia(NewTrailingTrivia)
            End Function

            Public Overrides Function VisitArgumentList(node As CSS.ArgumentListSyntax) As VB.VisualBasicSyntaxNode
                Return VisitCSArguments(node.OpenParenToken, node.Arguments, node.CloseParenToken)
            End Function

            Public Overrides Function VisitBracketedArgumentList(node As CSS.BracketedArgumentListSyntax) As VB.VisualBasicSyntaxNode
                Return VisitCSArguments(node.OpenBracketToken, node.Arguments, node.CloseBracketToken)
            End Function

            Public Overrides Function VisitOmittedTypeArgument(node As CSS.OmittedTypeArgumentSyntax) As VB.VisualBasicSyntaxNode
                Return VBFactory.ParseTypeName("").WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitTypeArgumentList(node As CSS.TypeArgumentListSyntax) As VB.VisualBasicSyntaxNode
                Dim CS_VisitorArguments As SeparatedSyntaxList(Of CSS.TypeSyntax) = node.Arguments
                Debug.Assert(CS_VisitorArguments.Any, "VisitTypeArgumentList CS_VisitorArguments.Count = 0")
                Dim CS_Separators As IEnumerable(Of SyntaxToken) = CS_VisitorArguments.GetSeparators
                Dim NodeList As New List(Of VBS.TypeSyntax)
                Dim Separators As New List(Of SyntaxToken)
                Dim SeparatorCount As Integer = CS_VisitorArguments.Count - 1
                For i As Integer = 0 To SeparatorCount
                    Dim e As CSS.TypeSyntax = CS_VisitorArguments(i)
                    Dim TypeSyntaxNode As VBS.TypeSyntax = DirectCast(e.Accept(Me), VBS.TypeSyntax)
                    NodeList.Add(TypeSyntaxNode)
                    If SeparatorCount > i Then
                        Separators.Add(CommaToken.WithConvertedTrailingTriviaFrom(CS_Separators(i)))
                    End If
                Next
                Dim OpenParenTokenWithTrivia As SyntaxToken = OpenParenToken.WithConvertedTriviaFrom(node.LessThanToken)
                Dim CloseParenTokenWithTrivia As SyntaxToken = CloseParenToken.WithConvertedTriviaFrom(node.GreaterThanToken)
                RestructureNodesAndSeparators(OpenParenTokenWithTrivia, NodeList, Separators, CloseParenTokenWithTrivia)
                Return VBFactory.TypeArgumentList(
                                                  OpenParenTokenWithTrivia,
                                                  OfKeyword.WithTrailingTrivia(SpaceTrivia),
                                                  VBFactory.SeparatedList(NodeList, Separators),
                                                  CloseParenTokenWithTrivia
                                                  )
            End Function

        End Class

    End Class

End Namespace
