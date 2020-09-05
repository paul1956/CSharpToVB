' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports Microsoft.CodeAnalysis
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBCodeConverter.ToVisualBasic

    Partial Public Class CSharpConverter

        Partial Friend Class NodesVisitor
            Inherits CS.CSharpSyntaxVisitor(Of VB.VisualBasicSyntaxNode)

            Public Overrides Function VisitBracketedParameterList(node As CSS.BracketedParameterListSyntax) As VB.VisualBasicSyntaxNode
                If node Is Nothing Then
                    Throw New ArgumentNullException(NameOf(node))
                End If

                Dim csSeparators As IEnumerable(Of SyntaxToken) = node.Parameters.GetSeparators
                Dim OpenParenTokenWithTrivia As SyntaxToken = OpenParenToken.WithConvertedTriviaFrom(node.OpenBracketToken)
                Dim CloseParenTokenWithTrivia As SyntaxToken = CloseParenToken
                Dim Items As New List(Of VBS.ParameterSyntax)
                Dim Separators As New List(Of SyntaxToken)
                Dim SeparatorCount As Integer = node.Parameters.Count - 1
                For index As Integer = 0 To SeparatorCount
                    Dim e As CSS.ParameterSyntax = node.Parameters(index)
                    Dim ItemWithTrivia As VBS.ParameterSyntax = DirectCast(e.Accept(Me).WithConvertedTrailingTriviaFrom(e), VBS.ParameterSyntax)
                    Dim Item As VBS.ParameterSyntax = ItemWithTrivia.WithoutTrivia
                    Items.Add(ItemWithTrivia)
                    If SeparatorCount > index Then
                        If Items.Last.ContainsEOLTrivia Then
                            Separators.Add(CommaToken.WithConvertedTrailingTriviaFrom(csSeparators(index)))
                        Else
                            Separators.Add(CommaToken.WithConvertedTrailingTriviaFrom(csSeparators(index)))
                        End If
                    End If
                Next
                RestructureNodesAndSeparators(OpenParenTokenWithTrivia, Items, Separators, CloseParenTokenWithTrivia)

                Return Factory.ParameterList(OpenParenTokenWithTrivia, Factory.SeparatedList(Items, Separators), CloseParenToken).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitParameter(node As CSS.ParameterSyntax) As VB.VisualBasicSyntaxNode
                If node Is Nothing Then
                    Throw New ArgumentNullException(NameOf(node))
                End If

                Dim returnType As VBS.TypeSyntax = DirectCast(node.Type?.Accept(Me), VBS.TypeSyntax)
                Dim EqualsLeadingTrivia As New SyntaxTriviaList
                If returnType IsNot Nothing Then
                    If returnType.ToString.StartsWith("[", StringComparison.Ordinal) Then
                        Dim TReturnType As VBS.TypeSyntax = Factory.ParseTypeName(returnType.ToString.Substring(startIndex:=1) _
                                                                                                     .Replace("]", "", StringComparison.Ordinal)).WithTriviaFrom(returnType)
                        If Not TReturnType.IsMissing Then
                            returnType = TReturnType
                        End If

                    End If
                    If returnType.GetTrailingTrivia.Any AndAlso returnType.GetTrailingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                        Dim trailingTrivia As New SyntaxTriviaList
                        trailingTrivia = trailingTrivia.AddRange(returnType.GetTrailingTrivia)
                        trailingTrivia = trailingTrivia.InsertRange(trailingTrivia.Count - 1, {VBSpaceTrivia, LineContinuation})
                        returnType = returnType.WithTrailingTrivia(trailingTrivia)
                    End If
                    If returnType IsNot Nothing Then
                        EqualsLeadingTrivia = EqualsLeadingTrivia.AddRange(node.Type.GetLeadingTrivia.ConvertTriviaList())
                    End If
                    If node.Identifier.HasLeadingTrivia Then
                        EqualsLeadingTrivia = EqualsLeadingTrivia.AddRange(node.Identifier.LeadingTrivia.ConvertTriviaList())
                    End If
                    If node.Identifier.HasTrailingTrivia AndAlso node.Identifier.TrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                        returnType = returnType.WithAppendedTrailingTrivia(node.Identifier.TrailingTrivia.ConvertTriviaList())
                    End If
                End If
                Dim defaultValue As VBS.EqualsValueSyntax = Nothing
                If node.Default IsNot Nothing Then
                    defaultValue = Factory.EqualsValue(EqualsToken.WithLeadingTrivia(EqualsLeadingTrivia), DirectCast(node.Default?.Value.Accept(Me), VBS.ExpressionSyntax))
                End If

                Dim newAttributes As VBS.AttributeListSyntax()
                Dim modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, IsModule, TokenContext.Local)
                If (modifiers.Count = 0 AndAlso returnType IsNot Nothing) OrElse node.Modifiers.Contains(CS.SyntaxKind.ThisKeyword) Then
                    modifiers = Factory.TokenList(ByValKeyword).ToList
                    newAttributes = Array.Empty(Of VBS.AttributeListSyntax)
                ElseIf node.Modifiers.Contains(CS.SyntaxKind.OutKeyword) Then
                    If Not AllImports.ContainsName(InteropServices) Then
                        AllImports.Add(ImportInteropServices)
                    End If

                    newAttributes = {Factory.AttributeList(Factory.SingletonSeparatedList(Factory.Attribute(RuntimeInteropServicesOut)))}
                Else
                    newAttributes = Array.Empty(Of VBS.AttributeListSyntax)
                End If

                Dim parameterTrailingTrivia As New SyntaxTriviaList

                Dim id As SyntaxToken = GenerateSafeVBToken(node.Identifier, node, _mSemanticModel).
                    WithTrailingTrivia(VBSpaceTrivia)

                Dim typeLeadingTrivia As New SyntaxTriviaList
                If node.Type IsNot Nothing Then
                    typeLeadingTrivia = node.Type.GetLeadingTrivia.ConvertTriviaList()
                End If
                If defaultValue IsNot Nothing Then
                    modifiers.Add(OptionalKeyword.WithLeadingTrivia(typeLeadingTrivia))
                End If
                If modifiers.Any AndAlso Not modifiers(0).RawKind = VB.SyntaxKind.ByValKeyword Then
                    If modifiers(0).RawKind = VB.SyntaxKind.OptionalKeyword OrElse
                        modifiers(0).RawKind = VB.SyntaxKind.ParamArrayKeyword Then
                        typeLeadingTrivia = typeLeadingTrivia.InsertRange(0, modifiers(0).LeadingTrivia)
                        modifiers(0) = modifiers(0).WithLeadingTrivia(typeLeadingTrivia)
                    End If
                    id = id.WithLeadingTrivia(VBSpaceTrivia)
                Else
                    id = id.WithLeadingTrivia(typeLeadingTrivia)
                End If
                Dim originalAttributeListWithTrivia As New List(Of VBS.AttributeListSyntax)
                originalAttributeListWithTrivia.AddRange(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))
                Dim OriginalAttributeListHasOut As Boolean = False
                For Each e As IndexClass(Of VBS.AttributeListSyntax) In originalAttributeListWithTrivia.WithIndex
                    For Each a As IndexClass(Of VBS.AttributeSyntax) In e.Value.Attributes.WithIndex
                        If a.Value.Name.ToString = "Out" Then
                            OriginalAttributeListHasOut = True
                            Exit For
                        End If
                    Next
                Next
                originalAttributeListWithTrivia = If(OriginalAttributeListHasOut, originalAttributeListWithTrivia, newAttributes.Concat(originalAttributeListWithTrivia)).ToList
                Dim leadingIndent As SyntaxTrivia = VBSpaceTrivia

                If originalAttributeListWithTrivia.Any Then
                    For index As Integer = 0 To originalAttributeListWithTrivia.Count - 1
                        Dim attributeLeadingTrivia As SyntaxTriviaList
                        Dim attributeTrailingTrivia As SyntaxTriviaList
                        Dim needEOL As Boolean = False
                        Dim foundEOL As Boolean = False
                        For Each e As IndexClass(Of SyntaxTrivia) In originalAttributeListWithTrivia(index).GetLeadingTrivia.WithIndex
                            Dim trivia As SyntaxTrivia = e.Value
                            If e.IsFirst AndAlso trivia.RawKind = VB.SyntaxKind.WhitespaceTrivia Then
                                If index = 0 Then
                                    leadingIndent = trivia
                                End If
                                attributeLeadingTrivia = attributeLeadingTrivia.Add(leadingIndent)
                                Continue For
                            End If
                            Select Case trivia.RawKind
                                Case VB.SyntaxKind.CommentTrivia
                                    If returnType IsNot Nothing Then
                                        parameterTrailingTrivia = parameterTrailingTrivia.Add(trivia)
                                    Else
                                        attributeTrailingTrivia = attributeTrailingTrivia.Add(VBSpaceTrivia)
                                        attributeTrailingTrivia = attributeTrailingTrivia.Add(trivia)
                                        needEOL = True
                                    End If
                                Case VB.SyntaxKind.EndOfLineTrivia
                                    If needEOL Then
                                        attributeTrailingTrivia = attributeTrailingTrivia.Add(trivia)
                                    Else
                                        parameterTrailingTrivia = parameterTrailingTrivia.Add(trivia)
                                    End If
                                    needEOL = False
                                Case VB.SyntaxKind.WhitespaceTrivia
                                    If attributeLeadingTrivia.Any AndAlso attributeLeadingTrivia.Last.RawKind <> VB.SyntaxKind.WhitespaceTrivia Then
                                        attributeLeadingTrivia = attributeLeadingTrivia.Add(VBSpaceTrivia)
                                    End If
                                    needEOL = False
                                Case VB.SyntaxKind.DisableWarningDirectiveTrivia
                                    GetStatementwithIssues(node).AddMarker(Factory.EmptyStatement.WithLeadingTrivia(trivia), StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                                Case VB.SyntaxKind.EnableWarningDirectiveTrivia
                                    GetStatementwithIssues(node).AddMarker(Factory.EmptyStatement.WithLeadingTrivia(trivia), StatementHandlingOption.AppendEmptyStatement, AllowDuplicates:=True)
                                Case Else
                                    Stop
                            End Select
                        Next
                        For Each t As SyntaxTrivia In originalAttributeListWithTrivia(index).GetTrailingTrivia
                            Dim FoundComment As Boolean = False
                            Select Case t.RawKind
                                Case VB.SyntaxKind.CommentTrivia
                                    If returnType IsNot Nothing Then
                                        parameterTrailingTrivia = parameterTrailingTrivia.Add(t)
                                    Else
                                        attributeTrailingTrivia = attributeTrailingTrivia.Add(VBSpaceTrivia)
                                        attributeTrailingTrivia = attributeTrailingTrivia.Add(t)
                                        FoundComment = True
                                    End If
                                Case VB.SyntaxKind.EndOfLineTrivia
                                    attributeTrailingTrivia = attributeTrailingTrivia.Add(t)
                                Case VB.SyntaxKind.WhitespaceTrivia
                                    attributeTrailingTrivia = attributeTrailingTrivia.Add(t)
                                    FoundComment = False
                                Case VB.SyntaxKind.LineContinuationTrivia
                                    attributeTrailingTrivia = attributeTrailingTrivia.Add(t)
                                Case Else
                                    Stop
                            End Select
                            If FoundComment Then
                                attributeTrailingTrivia = attributeTrailingTrivia.Add(VBEOLTrivia)
                            End If
                        Next
                        originalAttributeListWithTrivia(index) = originalAttributeListWithTrivia(index).With(attributeLeadingTrivia, attributeTrailingTrivia)
                    Next
                End If
                Dim AttributeLists As SyntaxList(Of VBS.AttributeListSyntax) = Factory.List(originalAttributeListWithTrivia)
                Dim Identifier As VBS.ModifiedIdentifierSyntax = Factory.ModifiedIdentifier(id)

                Dim modifierLeadingTrivia As New SyntaxTriviaList
                Dim NewModifiersWithoutComments As New List(Of SyntaxToken)
                For Each m As SyntaxToken In modifiers
                    For Each t As SyntaxTrivia In m.LeadingTrivia
                        If t.IsComment Then
                            parameterTrailingTrivia = parameterTrailingTrivia.Add(t)
                        Else
                            modifierLeadingTrivia = modifierLeadingTrivia.Add(t)
                        End If
                    Next

                    Dim modifierTrailingTrivia As SyntaxTriviaList
                    For Each t As SyntaxTrivia In m.TrailingTrivia
                        If t.IsComment Then
                            parameterTrailingTrivia = parameterTrailingTrivia.Add(t)
                        Else
                            modifierTrailingTrivia = modifierTrailingTrivia.Add(t)
                        End If
                    Next
                    NewModifiersWithoutComments.Add(m.With(modifierLeadingTrivia, modifierTrailingTrivia))
                Next
                modifiers = Factory.TokenList(NewModifiersWithoutComments).ToList
                Dim ByValIndex As Integer = modifiers.FindIndex(Function(t As SyntaxToken) t.IsKind(VB.SyntaxKind.ByValKeyword))
                If ByValIndex >= 0 Then
                    modifiers.RemoveAt(ByValIndex)
                End If
                Dim AsClause As VBS.SimpleAsClauseSyntax = Nothing
                If returnType IsNot Nothing Then
                    For Each Trivia As SyntaxTrivia In returnType.GetLeadingTrivia
                        Select Case Trivia.RawKind
                            Case VB.SyntaxKind.WhitespaceTrivia
                                ' ignore
                            Case VB.SyntaxKind.EndOfLineTrivia
                                ' Ignore
                            Case VB.SyntaxKind.CommentTrivia
                                parameterTrailingTrivia = parameterTrailingTrivia.Add(Trivia)
                            Case VB.SyntaxKind.EndIfDirectiveTrivia
                                parameterTrailingTrivia = parameterTrailingTrivia.Add(Trivia)
                            Case VB.SyntaxKind.IfDirectiveTrivia
                                ' TODO Ignore for now
                            Case VB.SyntaxKind.DisabledTextTrivia
                                ' TODO Ignore for now
                            Case VB.SyntaxKind.ElseDirectiveTrivia
                                ' TODO Ignore for now
                            Case VB.SyntaxKind.DisableWarningDirectiveTrivia
                                GetStatementwithIssues(node).AddMarker(Factory.EmptyStatement.WithLeadingTrivia(Trivia), StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                            Case VB.SyntaxKind.EnableWarningDirectiveTrivia
                                GetStatementwithIssues(node).AddMarker(Factory.EmptyStatement.WithLeadingTrivia(Trivia), StatementHandlingOption.AppendEmptyStatement, AllowDuplicates:=True)
                            Case Else
                                Stop
                        End Select
                    Next
                    Dim FoundEOL As Boolean
                    Dim initialTriviaList As SyntaxTriviaList = returnType.GetTrailingTrivia
                    For index As Integer = 0 To initialTriviaList.Count - 1
                        Dim Trivia As SyntaxTrivia = initialTriviaList(index)
                        Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(initialTriviaList, index, LookaheadCount:=1)

                        Select Case Trivia.RawKind
                            Case VB.SyntaxKind.WhitespaceTrivia
                                If nextTrivia.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                                    parameterTrailingTrivia = parameterTrailingTrivia.Add(Trivia)
                                End If
                            Case VB.SyntaxKind.EndOfLineTrivia
                                FoundEOL = True
                            Case VB.SyntaxKind.CommentTrivia
                                parameterTrailingTrivia = parameterTrailingTrivia.Add(Trivia)
                            Case VB.SyntaxKind.EndIfDirectiveTrivia
                                parameterTrailingTrivia = parameterTrailingTrivia.Add(Trivia)
                            Case VB.SyntaxKind.IfDirectiveTrivia
                                ' TODO Ignore for now
                            Case VB.SyntaxKind.DisabledTextTrivia
                                ' TODO Ignore for now
                            Case VB.SyntaxKind.ElseIfDirectiveTrivia
                                ' TODO Ignore for now
                            Case VB.SyntaxKind.ElseDirectiveTrivia
                                ' TODO Ignore for now
                            Case VB.SyntaxKind.DisableWarningDirectiveTrivia
                                GetStatementwithIssues(node).AddMarker(Factory.EmptyStatement.WithLeadingTrivia(Trivia), StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                            Case VB.SyntaxKind.EnableWarningDirectiveTrivia
                                GetStatementwithIssues(node).AddMarker(Factory.EmptyStatement.WithLeadingTrivia(Trivia), StatementHandlingOption.AppendEmptyStatement, AllowDuplicates:=True)
                            Case VB.SyntaxKind.LineContinuationTrivia
                                parameterTrailingTrivia = parameterTrailingTrivia.Add(Trivia)
                            Case Else
                                Stop
                        End Select
                    Next
                    If FoundEOL Then
                        parameterTrailingTrivia = parameterTrailingTrivia.Add(VBEOLTrivia)
                    End If
                    AsClause = Factory.SimpleAsClause(returnType.WithoutTrivia).WithTrailingTrivia(parameterTrailingTrivia)
                Else
                    Identifier = Identifier.WithTrailingTrivia(parameterTrailingTrivia)
                End If

                Dim parameterSyntax1 As VBS.ParameterSyntax = Factory.Parameter(AttributeLists,
                                                                                Factory.TokenList(modifiers),
                                                                                Identifier,
                                                                                AsClause,
                                                                                defaultValue) ' .WithLeadingTrivia(VBSpaceTrivia)
                Return parameterSyntax1
            End Function

            Public Overrides Function VisitParameterList(node As CSS.ParameterListSyntax) As VB.VisualBasicSyntaxNode
                If node Is Nothing Then
                    Throw New ArgumentNullException(NameOf(node))
                End If

                Dim csSeparators As IEnumerable(Of SyntaxToken) = node.Parameters.GetSeparators
                Dim OpenParenTokenWithTrivia As SyntaxToken = OpenParenToken.WithConvertedTriviaFrom(node.OpenParenToken)
                Dim CloseParenTokenWithTrivia As SyntaxToken = CloseParenToken.WithConvertedTriviaFrom(node.CloseParenToken)
                Dim Items As New List(Of VBS.ParameterSyntax)
                Dim Separators As New List(Of SyntaxToken)
                Dim SeparatorCount As Integer = node.Parameters.Count - 1
                For index As Integer = 0 To SeparatorCount
                    Dim ItemWithTrivia As VBS.ParameterSyntax = DirectCast(node.Parameters(index).Accept(Me), VBS.ParameterSyntax)
                    ItemWithTrivia = ItemWithTrivia.RemoveModifier(VB.SyntaxKind.ByValKeyword)
                    Items.Add(ItemWithTrivia.AdjustNodeTrivia(SeparatorCount > index))
                    If SeparatorCount > index Then
                        Separators.Add(CommaToken.WithConvertedTrailingTriviaFrom(csSeparators(index)))
                    End If
                Next
                RestructureNodesAndSeparators(OpenParenTokenWithTrivia, Items, Separators, CloseParenTokenWithTrivia)
                Return Factory.ParameterList(OpenParenTokenWithTrivia, Factory.SeparatedList(Items, Separators), CloseParenTokenWithTrivia)
            End Function

        End Class

    End Class

End Namespace
