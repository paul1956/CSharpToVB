' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
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

            Public Overrides Function VisitBracketedParameterList(node As CSS.BracketedParameterListSyntax) As VB.VisualBasicSyntaxNode
                Dim CS_Separators As IEnumerable(Of SyntaxToken) = node.Parameters.GetSeparators
                Dim OpenParenTokenWithTrivia As SyntaxToken = OpenParenToken.WithConvertedTriviaFrom(node.OpenBracketToken)
                Dim CloseParenTokenWithTrivia As SyntaxToken = CloseParenToken
                Dim Items As New List(Of VBS.ParameterSyntax)
                Dim Separators As New List(Of SyntaxToken)
                Dim SeparatorCount As Integer = node.Parameters.Count - 1
                For i As Integer = 0 To SeparatorCount
                    Dim e As CSS.ParameterSyntax = node.Parameters(i)
                    Dim ItemWithTrivia As VBS.ParameterSyntax = DirectCast(e.Accept(Me).WithConvertedTrailingTriviaFrom(e), VBS.ParameterSyntax)
                    Dim Item As VBS.ParameterSyntax = ItemWithTrivia.WithoutTrivia
                    Items.Add(ItemWithTrivia)
                    If SeparatorCount > i Then
                        If Items.Last.ContainsEOLTrivia Then
                            Separators.Add(CommaToken.WithConvertedTrailingTriviaFrom(CS_Separators(i)))
                        Else
                            Separators.Add(CommaToken.WithConvertedTrailingTriviaFrom(CS_Separators(i)))
                        End If
                    End If
                Next
                RestructureNodesAndSeparators(OpenParenTokenWithTrivia, Items, Separators, CloseParenTokenWithTrivia)

                Return VB.SyntaxFactory.ParameterList(OpenParenTokenWithTrivia, VB.SyntaxFactory.SeparatedList(Items, Separators), CloseParenToken).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitParameter(node As CSS.ParameterSyntax) As VB.VisualBasicSyntaxNode
                Dim returnType As VBS.TypeSyntax = DirectCast(node.Type?.Accept(Me), VBS.TypeSyntax)
                Dim EqualsLeadingTrivia As New List(Of SyntaxTrivia)
                If returnType IsNot Nothing Then
                    If returnType.ToString.StartsWith("[") Then
                        Dim TReturnType As VBS.TypeSyntax = VB.SyntaxFactory.ParseTypeName(returnType.ToString.Substring(1).Replace("]", "")).WithTriviaFrom(returnType)
                        If Not TReturnType.IsMissing Then
                            returnType = TReturnType
                        End If

                    End If
                    If returnType.GetTrailingTrivia.LastOrDefault.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                        Dim TrailingTrivia As New List(Of SyntaxTrivia)
                        TrailingTrivia.AddRange(returnType.GetTrailingTrivia)
                        Dim Index As Integer = TrailingTrivia.Count - 1
                        TrailingTrivia.InsertRange(Index, {SpaceTrivia, LineContinuation})
                        returnType = returnType.WithTrailingTrivia(TrailingTrivia)
                    End If
                    If node.Identifier.HasLeadingTrivia Then
                        If returnType IsNot Nothing Then
                            EqualsLeadingTrivia.AddRange(ConvertTrivia(node.Identifier.LeadingTrivia))
                        End If
                    End If
                    If node.Identifier.HasTrailingTrivia AndAlso node.Identifier.TrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                        returnType = returnType.WithAppendedTrailingTrivia(ConvertTrivia(node.Identifier.TrailingTrivia))
                    End If
                End If
                Dim DefaultValue As VBS.EqualsValueSyntax = Nothing
                If node.Default IsNot Nothing Then
                    DefaultValue = VB.SyntaxFactory.EqualsValue(EqualsToken.WithLeadingTrivia(EqualsLeadingTrivia), DirectCast(node.Default?.Value.Accept(Me), VBS.ExpressionSyntax))
                End If

                Dim newAttributes As VBS.AttributeListSyntax()
                Dim modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, Me.IsModule, TokenContext.Local)
                If (modifiers.Count = 0 AndAlso returnType IsNot Nothing) OrElse node.Modifiers.Any(CS.SyntaxKind.ThisKeyword) Then
                    modifiers = VB.SyntaxFactory.TokenList(ByValKeyword).ToList
                    newAttributes = Array.Empty(Of VBS.AttributeListSyntax)
                ElseIf node.Modifiers.Any(CS.SyntaxKind.OutKeyword) Then
                    newAttributes = {VB.SyntaxFactory.AttributeList(VB.SyntaxFactory.SingletonSeparatedList(VB.SyntaxFactory.Attribute(VB.SyntaxFactory.ParseTypeName("Out"))))}
                Else
                    newAttributes = Array.Empty(Of VBS.AttributeListSyntax)
                End If

                Dim ParameterTrailingTrivia As New List(Of SyntaxTrivia)
                Dim ModifierLeadingTrivia As New List(Of SyntaxTrivia)
                Dim ModifierTrailingTrivia As New List(Of SyntaxTrivia)

                Dim id As SyntaxToken = GenerateSafeVBToken(id:=node.Identifier, IsQualifiedName:=False).
                    WithTrailingTrivia(SpaceTrivia)

                Dim TypeLeadingTrivia As IEnumerable(Of SyntaxTrivia) = ConvertTrivia(node.Type?.GetLeadingTrivia)
                If DefaultValue IsNot Nothing Then
                    modifiers.Add(OptionalKeyword.WithLeadingTrivia(TypeLeadingTrivia))
                End If
                If modifiers.Count > 0 AndAlso Not modifiers(0).RawKind = VB.SyntaxKind.ByValKeyword Then
                    If modifiers(0).RawKind = VB.SyntaxKind.OptionalKeyword OrElse
                        modifiers(0).RawKind = VB.SyntaxKind.ParamArrayKeyword Then
                        modifiers(0) = modifiers(0).WithLeadingTrivia(TypeLeadingTrivia)
                    End If
                    id = id.WithLeadingTrivia(SpaceTrivia)
                Else
                    id = id.WithLeadingTrivia(TypeLeadingTrivia)
                End If
                Dim OriginalAttributeListWithTrivia As New List(Of VBS.AttributeListSyntax)
                OriginalAttributeListWithTrivia.AddRange(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))
                Dim OriginalAttributeListHasOut As Boolean = False
                For I As Integer = 0 To OriginalAttributeListWithTrivia.Count - 1
                    For k As Integer = 0 To OriginalAttributeListWithTrivia(I).Attributes.Count - 1
                        If OriginalAttributeListWithTrivia(I).Attributes(k).Name.ToString = "Out" Then
                            OriginalAttributeListHasOut = True
                            Exit For
                        End If
                    Next
                Next
                OriginalAttributeListWithTrivia = If(OriginalAttributeListHasOut, OriginalAttributeListWithTrivia, newAttributes.Concat(OriginalAttributeListWithTrivia)).ToList
                Dim ParameterLeadingTrivia As New List(Of SyntaxTrivia)
                Dim LeadingIndent As SyntaxTrivia = SpaceTrivia

                If OriginalAttributeListWithTrivia.Count > 0 Then
                    For I As Integer = 0 To OriginalAttributeListWithTrivia.Count - 1
                        Dim AttributeLeadingTrivia As New List(Of SyntaxTrivia)
                        Dim AttributeTrailingTrivia As New List(Of SyntaxTrivia)
                        Dim NeedEOL As Boolean = False
                        Dim FoundEOL As Boolean = False
                        For j As Integer = 0 To OriginalAttributeListWithTrivia(I).GetLeadingTrivia.Count - 1
                            Dim Trivia As SyntaxTrivia = OriginalAttributeListWithTrivia(I).GetLeadingTrivia(j)
                            If j = 0 And Trivia.RawKind = VB.SyntaxKind.WhitespaceTrivia Then
                                If I = 0 Then
                                    LeadingIndent = Trivia
                                End If
                                AttributeLeadingTrivia.Add(LeadingIndent)
                                Continue For
                            End If
                            Select Case Trivia.RawKind
                                Case VB.SyntaxKind.CommentTrivia
                                    If returnType IsNot Nothing Then
                                        ParameterTrailingTrivia.Add(Trivia)
                                    Else
                                        AttributeTrailingTrivia.Add(SpaceTrivia)
                                        AttributeTrailingTrivia.Add(Trivia)
                                        NeedEOL = True
                                    End If
                                Case VB.SyntaxKind.EndOfLineTrivia
                                    If NeedEOL Then
                                        AttributeTrailingTrivia.Add(Trivia)
                                    Else
                                        ParameterTrailingTrivia.Add(Trivia)
                                    End If
                                    NeedEOL = False
                                Case VB.SyntaxKind.WhitespaceTrivia
                                    If AttributeLeadingTrivia.Count > 0 AndAlso AttributeLeadingTrivia?.Last.RawKind <> VB.SyntaxKind.WhitespaceTrivia Then
                                        AttributeLeadingTrivia.Add(SpaceTrivia)
                                    End If
                                    NeedEOL = False
                                Case VB.SyntaxKind.DisableWarningDirectiveTrivia
                                    GetStatementwithIssues(node).AddMarker(VB.SyntaxFactory.EmptyStatement.WithLeadingTrivia(trivia), StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                                Case VB.SyntaxKind.EnableWarningDirectiveTrivia
                                    GetStatementwithIssues(node).AddMarker(VB.SyntaxFactory.EmptyStatement.WithLeadingTrivia(trivia), StatementHandlingOption.AppendEmptyStatement, AllowDuplicates:=True)
                                Case Else
                                    Stop
                            End Select
                        Next
                        For Each t As SyntaxTrivia In OriginalAttributeListWithTrivia(I).GetTrailingTrivia
                            Dim FoundComment As Boolean = False
                            Select Case t.RawKind
                                Case VB.SyntaxKind.CommentTrivia
                                    If returnType IsNot Nothing Then
                                        ParameterTrailingTrivia.Add(t)
                                    Else
                                        AttributeTrailingTrivia.Add(SpaceTrivia)
                                        AttributeTrailingTrivia.Add(t)
                                        FoundComment = True
                                    End If
                                Case VB.SyntaxKind.EndOfLineTrivia
                                    AttributeTrailingTrivia.Add(t)
                                Case VB.SyntaxKind.WhitespaceTrivia
                                    AttributeTrailingTrivia.Add(t)
                                    FoundComment = False
                                Case Else
                                    Stop
                            End Select
                            If FoundComment Then
                                AttributeTrailingTrivia.Add(VB_EOLTrivia)
                            End If
                        Next
                        OriginalAttributeListWithTrivia(I) = OriginalAttributeListWithTrivia(I).With(AttributeLeadingTrivia, AttributeTrailingTrivia)
                    Next
                End If
                Dim AttributeLists As SyntaxList(Of VBS.AttributeListSyntax) = VB.SyntaxFactory.List(OriginalAttributeListWithTrivia)
                Dim Identifier As VBS.ModifiedIdentifierSyntax = VB.SyntaxFactory.ModifiedIdentifier(id)

                Dim NewModifiersWithoutComments As New List(Of SyntaxToken)
                For Each m As SyntaxToken In modifiers
                    For Each t As SyntaxTrivia In m.LeadingTrivia
                        If t.IsComment Then
                            ParameterTrailingTrivia.Add(t)
                        Else
                            ModifierLeadingTrivia.Add(t)
                        End If
                    Next
                    For Each t As SyntaxTrivia In m.TrailingTrivia
                        If t.IsComment Then
                            ParameterTrailingTrivia.Add(t)
                        Else
                            ModifierTrailingTrivia.Add(t)
                        End If
                    Next
                    NewModifiersWithoutComments.Add(m.With(ModifierLeadingTrivia, ModifierTrailingTrivia))
                Next
                modifiers = VB.SyntaxFactory.TokenList(NewModifiersWithoutComments).ToList
                Dim AsClause As VBS.SimpleAsClauseSyntax = Nothing
                If returnType IsNot Nothing Then
                    For Each Trivia As SyntaxTrivia In returnType.GetLeadingTrivia
                        Select Case Trivia.RawKind
                            Case VB.SyntaxKind.WhitespaceTrivia
                                ' ignore
                            Case VB.SyntaxKind.EndOfLineTrivia
                                ' Ignore
                            Case VB.SyntaxKind.CommentTrivia
                                ParameterTrailingTrivia.Add(Trivia)
                            Case VB.SyntaxKind.EndIfDirectiveTrivia
                                ParameterTrailingTrivia.Add(Trivia)
                            Case VB.SyntaxKind.IfDirectiveTrivia
                                ' TODO Ignore for now
                            Case VB.SyntaxKind.DisabledTextTrivia
                                ' TODO Ignore for now
                            Case VB.SyntaxKind.ElseDirectiveTrivia
                                ' TODO Ignore for now
                            Case VB.SyntaxKind.DisableWarningDirectiveTrivia
                                GetStatementwithIssues(node).AddMarker(VB.SyntaxFactory.EmptyStatement.WithLeadingTrivia(Trivia), StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                            Case VB.SyntaxKind.EnableWarningDirectiveTrivia
                                GetStatementwithIssues(node).AddMarker(VB.SyntaxFactory.EmptyStatement.WithLeadingTrivia(Trivia), StatementHandlingOption.AppendEmptyStatement, AllowDuplicates:=True)
                            Case Else
                                Stop
                        End Select
                    Next
                    Dim FoundEOL As Boolean
                    Dim TrailingTrivia As New List(Of SyntaxTrivia)
                    TrailingTrivia.AddRange(returnType.GetTrailingTrivia)
                    For i As Integer = 0 To TrailingTrivia.Count - 1
                        Dim Trivia As SyntaxTrivia = TrailingTrivia(i)
                        Dim NextTrivia As SyntaxTrivia = If(i < returnType.GetTrailingTrivia.Count - 1, TrailingTrivia(i + 1), Nothing)

                        Select Case Trivia.RawKind
                            Case VB.SyntaxKind.WhitespaceTrivia
                                If NextTrivia.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                                    ParameterTrailingTrivia.Add(Trivia)
                                End If
                            Case VB.SyntaxKind.EndOfLineTrivia
                                FoundEOL = True
                            Case VB.SyntaxKind.CommentTrivia
                                ParameterTrailingTrivia.Add(Trivia)
                            Case VB.SyntaxKind.EndIfDirectiveTrivia
                                ParameterTrailingTrivia.Add(Trivia)
                            Case VB.SyntaxKind.IfDirectiveTrivia
                                ' TODO Ignore for now
                            Case VB.SyntaxKind.DisabledTextTrivia
                                ' TODO Ignore for now
                            Case VB.SyntaxKind.ElseIfDirectiveTrivia
                                ' TODO Ignore for now
                            Case VB.SyntaxKind.ElseDirectiveTrivia
                                ' TODO Ignore for now
                            Case VB.SyntaxKind.DisableWarningDirectiveTrivia
                                GetStatementwithIssues(node).AddMarker(VB.SyntaxFactory.EmptyStatement.WithLeadingTrivia(Trivia), StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                            Case VB.SyntaxKind.EnableWarningDirectiveTrivia
                                GetStatementwithIssues(node).AddMarker(VB.SyntaxFactory.EmptyStatement.WithLeadingTrivia(Trivia), StatementHandlingOption.AppendEmptyStatement, AllowDuplicates:=True)
                            Case VB.SyntaxKind.LineContinuationTrivia
                                ParameterTrailingTrivia.Add(Trivia)
                            Case Else
                                Stop
                        End Select
                    Next
                    If FoundEOL Then
                        ParameterTrailingTrivia.Add(VB_EOLTrivia)
                    End If
                    AsClause = VB.SyntaxFactory.SimpleAsClause(returnType.WithoutTrivia).WithTrailingTrivia(ParameterTrailingTrivia)
                Else
                    Identifier = Identifier.WithTrailingTrivia(ParameterTrailingTrivia)
                End If

                Dim parameterSyntax1 As VBS.ParameterSyntax = VB.SyntaxFactory.Parameter(AttributeLists,
                                                                                VB.SyntaxFactory.TokenList(modifiers),
                                                                                Identifier,
                                                                                AsClause,
                                                                                DefaultValue).WithLeadingTrivia(SpaceTrivia)
                Return parameterSyntax1
            End Function

            Public Overrides Function VisitParameterList(node As CSS.ParameterListSyntax) As VB.VisualBasicSyntaxNode
                Dim CS_Separators As IEnumerable(Of SyntaxToken) = node.Parameters.GetSeparators
                Dim OpenParenTokenWithTrivia As SyntaxToken = OpenParenToken.WithConvertedTriviaFrom(node.OpenParenToken)
                Dim CloseParenTokenWithTrivia As SyntaxToken = CloseParenToken.WithConvertedTriviaFrom(node.CloseParenToken)
                Dim Items As New List(Of VBS.ParameterSyntax)
                Dim Separators As New List(Of SyntaxToken)
                Dim SeparatorCount As Integer = node.Parameters.Count - 1
                For i As Integer = 0 To SeparatorCount
                    Dim ItemWithTrivia As VBS.ParameterSyntax = DirectCast(node.Parameters(i).Accept(Me), VBS.ParameterSyntax)
                    ItemWithTrivia = ItemWithTrivia.RemoveModifier(VB.SyntaxKind.ByValKeyword)
                    'Dim OldLeadingTrivia As IEnumerable(Of SyntaxTrivia) = ItemWithTrivia.GetLeadingTrivia
                    'Dim NewLeadingTrivia As New List(Of SyntaxTrivia)
                    'Dim LeadingEol As Boolean = True
                    'For j As Integer = 0 To OldLeadingTrivia.Count - 1
                    '    Dim Trivia As SyntaxTrivia = OldLeadingTrivia(j)
                    '    Dim NextTrivia As SyntaxTrivia = If(i < OldLeadingTrivia.Count - 1, OldLeadingTrivia(j + 1), Nothing)
                    '    If Trivia.IsKind(VB.SyntaxKind.WhitespaceTrivia) AndAlso NextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                    '        Continue For
                    '    End If

                    '    If Trivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) AndAlso (NextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) OrElse LeadingEol) Then
                    '        Continue For
                    '    End If
                    '    LeadingEol = False
                    '    NewLeadingTrivia.Add(Trivia)
                    'Next
                    Items.Add(ItemWithTrivia.WithModifiedNodeTrivia(SeparatorCount > i))
                    If SeparatorCount > i Then
                        Separators.Add(CommaToken.WithConvertedTrailingTriviaFrom(CS_Separators(i)))
                    End If
                Next
                RestructureNodesAndSeparators(OpenParenTokenWithTrivia, Items, Separators, CloseParenTokenWithTrivia)
                Return VB.SyntaxFactory.ParameterList(OpenParenTokenWithTrivia, VB.SyntaxFactory.SeparatedList(Items, Separators), CloseParenTokenWithTrivia)
            End Function

        End Class

    End Class

End Namespace