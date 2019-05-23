Option Explicit On
Option Infer Off
Option Strict On

Imports IVisualBasicCode.CodeConverter.Util

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace IVisualBasicCode.CodeConverter.Visual_Basic

    Partial Public Class CSharpConverter

        Partial Protected Friend Class NodesVisitor
            Inherits CS.CSharpSyntaxVisitor(Of VB.VisualBasicSyntaxNode)

            Private Function VisitCSArguments(CS_OpenToken As SyntaxToken, CS_VisitorArguments As SeparatedSyntaxList(Of CSS.ArgumentSyntax), CS_CloseToken As SyntaxToken) As VB.VisualBasicSyntaxNode
                If CS_VisitorArguments.Count = 0 Then
                    Return VB.SyntaxFactory.ArgumentList(VB.SyntaxFactory.SeparatedList(CS_VisitorArguments.Select(Function(a As CSS.ArgumentSyntax) DirectCast(a.Accept(Me), VBS.ArgumentSyntax))))
                End If
                Dim CS_Separators As IEnumerable(Of SyntaxToken) = CS_VisitorArguments.GetSeparators
                Dim NodeList As New List(Of VBS.ArgumentSyntax)
                Dim Separators As New List(Of SyntaxToken)
                Dim SeparatorCount As Integer = CS_VisitorArguments.Count - 1
                For i As Integer = 0 To SeparatorCount
                    Dim e As CSS.ArgumentSyntax = CS_VisitorArguments(i)
                    Dim ArgumentSyntaxNode As VBS.ArgumentSyntax = DirectCast(e.Accept(Me), VBS.ArgumentSyntax)
                    Dim NewLeadingTrivia As New List(Of SyntaxTrivia)
                    Dim OldLeadingTrivia As IEnumerable(Of SyntaxTrivia) = ConvertTrivia(e.GetLeadingTrivia)
                    NewLeadingTrivia.AddRange(OldLeadingTrivia)
                    For Each t As SyntaxTrivia In OldLeadingTrivia
                        If Not t.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            Exit For
                        End If
                        NewLeadingTrivia.RemoveAt(0)
                    Next
                    NodeList.Add(ArgumentSyntaxNode.With(NewLeadingTrivia, ConvertTrivia(e.GetTrailingTrivia)))
                    If SeparatorCount > i Then
                        Separators.Add(CommaToken.WithConvertedTrailingTriviaFrom(CS_Separators(i)))
                    End If
                Next
                Dim OpenParenTokenWithTrivia As SyntaxToken = OpenParenToken.WithConvertedTriviaFrom(CS_OpenToken)
                Dim CloseParenTokenWithTrivia As SyntaxToken = CloseParenToken.WithConvertedTriviaFrom(CS_CloseToken)
                RestructureNodesAndSeparators(OpenParenTokenWithTrivia, NodeList, Separators, CloseParenTokenWithTrivia)
                Return VB.SyntaxFactory.ArgumentList(
                                                  OpenParenTokenWithTrivia,
                                                  VB.SyntaxFactory.SeparatedList(NodeList, Separators),
                                                  CloseParenTokenWithTrivia
                                                  )
            End Function

            Public Overrides Function VisitArgument(node As CSS.ArgumentSyntax) As VB.VisualBasicSyntaxNode
                Dim name As VBS.NameColonEqualsSyntax = Nothing
                Dim NodeExpression As CSS.ExpressionSyntax = node.Expression
                Dim ArgumentWithTrivia As VBS.ExpressionSyntax = Nothing
                Dim NewLeadingTrivia As New List(Of SyntaxTrivia)
                Dim NewTrailingTrivia As New List(Of SyntaxTrivia)
                Try
                    If node.RefKindKeyword.Text = "ref" Then
                        Dim Expression As VBS.ExpressionSyntax = DirectCast(node.Expression.Accept(Me), VBS.ExpressionSyntax)
                        Dim IdentifierString As String = Expression.ToString.Replace("[", "").Replace("]", "")
                        Dim StatementWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                        StatementWithIssues.AddMarker(FlagUnsupportedStatements(StatementWithIssues, $"ref keyword, fix variables starting with 'HandleRef_' below", CommentOutOriginalStatements:=False), StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                        ArgumentWithTrivia = VB.SyntaxFactory.ParseExpression($"HandleRef_{IdentifierString}")
                    ElseIf NodeExpression.IsKind(CS.SyntaxKind.CoalesceExpression) Then
                        Dim CS_BinaryExpression As CSS.BinaryExpressionSyntax = DirectCast(NodeExpression, CSS.BinaryExpressionSyntax)
                        If CS_BinaryExpression.Right.IsKind(CS.SyntaxKind.ThrowExpression) Then
                            Dim TestNode As VBS.ExpressionSyntax = DirectCast(CS_BinaryExpression.Left.Accept(Me).WithConvertedTriviaFrom(CS_BinaryExpression.Left), VBS.ExpressionSyntax)
                            Dim SecondExpression As VBS.ThrowStatementSyntax = DirectCast(CS_BinaryExpression.Right.Accept(Me).WithConvertedTriviaFrom(CS_BinaryExpression.Right), VBS.ThrowStatementSyntax)
                            Dim Statements As SyntaxList(Of VBS.StatementSyntax) = VB.SyntaxFactory.SingletonList(Of VBS.StatementSyntax)(SecondExpression)

                            Dim Condition As VBS.ExpressionSyntax = VB.SyntaxFactory.IsExpression(TestNode, NothingExpression)
                            Dim IfBlock As VBS.SingleLineIfStatementSyntax = VB.SyntaxFactory.SingleLineIfStatement(Condition,
                                                                                                              Statements,
                                                                                                              elseClause:=Nothing)
                            Dim StatementWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                            StatementWithIssues.AddMarker(IfBlock, StatementHandlingOption.PrependStatement, AllowDuplicates:=False)
                            ArgumentWithTrivia = DirectCast(CS_BinaryExpression.Left.Accept(Me).WithConvertedTriviaFrom(CS_BinaryExpression.Left), VBS.ExpressionSyntax)
                        Else
                            ArgumentWithTrivia = DirectCast(NodeExpression.Accept(Me), VBS.ExpressionSyntax)
                        End If
                    Else
                        ArgumentWithTrivia = DirectCast(NodeExpression.Accept(Me), VBS.ExpressionSyntax)
                    End If

                    If TypeOf node.Parent Is CSS.BracketedArgumentListSyntax Then
                        Dim _Typeinfo As TypeInfo = ModelExtensions.GetTypeInfo(Me.mSemanticModel, NodeExpression)
                        If _Typeinfo.ConvertedType IsNot _Typeinfo.Type Then
                            If _Typeinfo.Type?.SpecialType = SpecialType.System_Char Then '
                                ArgumentWithTrivia = VB.SyntaxFactory.ParseExpression($"ChrW({ArgumentWithTrivia.WithoutTrivia.ToString})").WithTriviaFrom(ArgumentWithTrivia)
                            End If
                        End If
                    End If

                    If node.NameColon IsNot Nothing Then
                        name = VB.SyntaxFactory.NameColonEquals(DirectCast(node.NameColon.Name.Accept(Me), VBS.IdentifierNameSyntax))
                        Dim NameWithOutColon As String = name.Name.ToString.Replace(":=", "")
                        If NameWithOutColon.EndsWith("_Renamed") Then
                            name = VB.SyntaxFactory.NameColonEquals(VB.SyntaxFactory.IdentifierName(NameWithOutColon.Replace("_Renamed", "")))
                        End If
                    End If

                    If ArgumentWithTrivia.HasLeadingTrivia Then
                        For Each trivia As SyntaxTrivia In ArgumentWithTrivia.GetLeadingTrivia
                            ' Keep separate in case they need special handling for now
                            Select Case trivia.RawKind
                                Case VB.SyntaxKind.CommentTrivia
                                    NewLeadingTrivia.Add(trivia)
                                Case VB.SyntaxKind.WhitespaceTrivia
                                    NewLeadingTrivia.Add(trivia)
                                Case VB.SyntaxKind.EndOfLineTrivia
                                    ' Possibly need to Ignore TODO
                                    NewLeadingTrivia.Add(trivia)
                                Case VB.SyntaxKind.IfDirectiveTrivia
                                    NewLeadingTrivia.Add(trivia)
                                Case VB.SyntaxKind.DisabledTextTrivia
                                    NewLeadingTrivia.Add(trivia)
                                Case VB.SyntaxKind.ElseDirectiveTrivia
                                    NewLeadingTrivia.Add(trivia)
                                Case VB.SyntaxKind.ElseIfDirectiveTrivia
                                    NewLeadingTrivia.Add(trivia)
                                Case VB.SyntaxKind.EndIfDirectiveTrivia
                                    NewLeadingTrivia.Add(trivia)
                                Case VB.SyntaxKind.DisableWarningDirectiveTrivia
                                    GetStatementwithIssues(node).AddMarker(VB.SyntaxFactory.EmptyStatement.WithLeadingTrivia(trivia), StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                                Case VB.SyntaxKind.EnableWarningDirectiveTrivia
                                    GetStatementwithIssues(node).AddMarker(VB.SyntaxFactory.EmptyStatement.WithLeadingTrivia(trivia), StatementHandlingOption.AppendEmptyStatement, AllowDuplicates:=True)
                                Case VB.SyntaxKind.LineContinuationTrivia
                                    NewLeadingTrivia.Add(trivia)
                                Case Else
                                    Stop
                            End Select
                        Next
                    End If
                    NewTrailingTrivia.AddRange(ArgumentWithTrivia.GetTrailingTrivia)
                Catch ex As Exception
                    Stop
                End Try
                ArgumentWithTrivia = ArgumentWithTrivia.WithLeadingTrivia(NewLeadingTrivia).WithTrailingTrivia(SpaceTrivia)
                Return VB.SyntaxFactory.SimpleArgument(name, ArgumentWithTrivia).WithTrailingTrivia(NewTrailingTrivia)
            End Function

            Public Overrides Function VisitArgumentList(node As CSS.ArgumentListSyntax) As VB.VisualBasicSyntaxNode
                Return Me.VisitCSArguments(node.OpenParenToken, node.Arguments, node.CloseParenToken)
            End Function

            Public Overrides Function VisitBracketedArgumentList(node As CSS.BracketedArgumentListSyntax) As VB.VisualBasicSyntaxNode
                Return Me.VisitCSArguments(node.OpenBracketToken, node.Arguments, node.CloseBracketToken)
            End Function

            Public Overrides Function VisitOmittedTypeArgument(node As CSS.OmittedTypeArgumentSyntax) As VB.VisualBasicSyntaxNode
                Return VB.SyntaxFactory.ParseTypeName("").WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitTypeArgumentList(node As CSS.TypeArgumentListSyntax) As VB.VisualBasicSyntaxNode
                Dim CS_VisitorArguments As SeparatedSyntaxList(Of CSS.TypeSyntax) = node.Arguments

                If CS_VisitorArguments.Count = 0 Then
                    Throw UnreachableException
                    Return VB.SyntaxFactory.TypeArgumentList(VB.SyntaxFactory.SeparatedList(CS_VisitorArguments.Select(Function(a As CSS.TypeSyntax) DirectCast(a.Accept(Me), VBS.TypeSyntax))))
                End If
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
                Return VB.SyntaxFactory.TypeArgumentList(
                                                  OpenParenTokenWithTrivia,
                                                  OfKeyword.WithTrailingTrivia(SpaceTrivia),
                                                  VB.SyntaxFactory.SeparatedList(NodeList, Separators),
                                                  CloseParenTokenWithTrivia
                                                  )
            End Function

        End Class

    End Class

End Namespace