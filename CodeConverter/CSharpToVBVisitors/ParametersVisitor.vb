' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBConverter.ToVisualBasic

    Partial Public Class CSharpConverter

        Partial Friend Class NodesVisitor
            Inherits CS.CSharpSyntaxVisitor(Of VB.VisualBasicSyntaxNode)

            Public Overrides Function VisitBracketedParameterList(node As CSS.BracketedParameterListSyntax) As VB.VisualBasicSyntaxNode
                If node Is Nothing Then
                    Throw New ArgumentNullException(NameOf(node))
                End If

                Dim csSeparators As IEnumerable(Of SyntaxToken) = node.Parameters.GetSeparators
                Dim openParenTokenWithTrivia As SyntaxToken = openParenToken.WithConvertedTriviaFrom(node.OpenBracketToken)
                Dim closeParenTokenWithTrivia As SyntaxToken = CloseParenToken
                Dim items As New List(Of VBS.ParameterSyntax)
                Dim separators As New List(Of SyntaxToken)
                Dim separatorCount As Integer = node.Parameters.Count - 1
                For index As Integer = 0 To separatorCount
                    Dim e As CSS.ParameterSyntax = node.Parameters(index)
                    Dim itemWithTrivia As VBS.ParameterSyntax = DirectCast(e.Accept(Me).WithConvertedTrailingTriviaFrom(e), VBS.ParameterSyntax)
                    Dim item As VBS.ParameterSyntax = itemWithTrivia.WithoutTrivia
                    items.Add(itemWithTrivia)
                    If separatorCount > index Then
                        If items.Last.ContainsEOLTrivia Then
                            separators.Add(CommaToken.WithConvertedTrailingTriviaFrom(csSeparators(index)))
                        Else
                            separators.Add(CommaToken.WithConvertedTrailingTriviaFrom(csSeparators(index)))
                        End If
                    End If
                Next
                RestructureNodesAndSeparators(openParenTokenWithTrivia, items, separators, closeParenTokenWithTrivia)

                Return Factory.ParameterList(openParenTokenWithTrivia, Factory.SeparatedList(items, separators), CloseParenToken).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitParameter(node As CSS.ParameterSyntax) As VB.VisualBasicSyntaxNode
                If node Is Nothing Then
                    Throw New ArgumentNullException(NameOf(node))
                End If

                Dim returnType As VBS.TypeSyntax = DirectCast(node.Type?.Accept(Me), VBS.TypeSyntax)
                Dim equalsLeadingTrivia As New SyntaxTriviaList
                If returnType IsNot Nothing Then
                    If returnType.ToString.StartsWith("[", StringComparison.Ordinal) Then
                        Dim newReturnType As VBS.TypeSyntax = Factory.ParseTypeName(returnType.ToString.Substring(1) _
                                                                                                     .Replace("]", "", StringComparison.Ordinal)).WithTriviaFrom(returnType)
                        If Not newReturnType.IsMissing Then
                            returnType = newReturnType
                        End If

                    End If
                    If returnType.GetTrailingTrivia.Any AndAlso returnType.GetTrailingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                        Dim trailingTrivia As New SyntaxTriviaList
                        trailingTrivia = trailingTrivia.AddRange(returnType.GetTrailingTrivia)
                        trailingTrivia = trailingTrivia.InsertRange(trailingTrivia.Count - 1, {Factory.Space, LineContinuation})
                        returnType = returnType.WithTrailingTrivia(trailingTrivia)
                    End If
                    If returnType IsNot Nothing Then
                        equalsLeadingTrivia = equalsLeadingTrivia.AddRange(node.Type.GetLeadingTrivia.ConvertTriviaList())
                    End If
                    If node.Identifier.HasLeadingTrivia Then
                        equalsLeadingTrivia = equalsLeadingTrivia.AddRange(node.Identifier.LeadingTrivia.ConvertTriviaList())
                    End If
                    If node.Identifier.HasTrailingTrivia AndAlso node.Identifier.TrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                        returnType = returnType.WithAppendedTrailingTrivia(node.Identifier.TrailingTrivia.ConvertTriviaList())
                    End If
                End If
                Dim defaultValue As VBS.EqualsValueSyntax = Nothing
                If node.Default IsNot Nothing Then
                    defaultValue = Factory.EqualsValue(EqualsToken.WithLeadingTrivia(equalsLeadingTrivia), DirectCast(node.Default?.Value.Accept(Me), VBS.ExpressionSyntax))
                End If

                Dim newAttributes As VBS.AttributeListSyntax()
                Dim modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, Me.IsModule, TokenContext.Local).ToList
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

                Dim id As SyntaxToken = GenerateSafeVBToken(node.Identifier, node, _usedIdentifiers, _semanticModel).
                    WithTrailingTrivia(Factory.Space)

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
                    id = id.WithLeadingTrivia(Factory.Space)
                Else
                    id = id.WithLeadingTrivia(typeLeadingTrivia)
                End If
                Dim originalAttributeListWithTrivia As New List(Of VBS.AttributeListSyntax)
                originalAttributeListWithTrivia.AddRange(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))
                Dim originalAttributeListHasOut As Boolean = False
                For Each e As IndexClass(Of VBS.AttributeListSyntax) In originalAttributeListWithTrivia.WithIndex
                    For Each a As IndexClass(Of VBS.AttributeSyntax) In e.Value.Attributes.WithIndex
                        If a.Value.Name.ToString = "Out" Then
                            originalAttributeListHasOut = True
                            Exit For
                        End If
                    Next
                Next
                originalAttributeListWithTrivia = If(originalAttributeListHasOut, originalAttributeListWithTrivia, newAttributes.Concat(originalAttributeListWithTrivia)).ToList
                Dim leadingIndent As SyntaxTrivia = Factory.Space

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
                                        attributeTrailingTrivia = attributeTrailingTrivia.Add(Factory.Space)
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
                                        attributeLeadingTrivia = attributeLeadingTrivia.Add(Factory.Space)
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
                            Dim foundComment As Boolean = False
                            Select Case t.RawKind
                                Case VB.SyntaxKind.CommentTrivia
                                    If returnType IsNot Nothing Then
                                        parameterTrailingTrivia = parameterTrailingTrivia.Add(t)
                                    Else
                                        attributeTrailingTrivia = attributeTrailingTrivia.Add(Factory.Space)
                                        attributeTrailingTrivia = attributeTrailingTrivia.Add(t)
                                        foundComment = True
                                    End If
                                Case VB.SyntaxKind.EndOfLineTrivia
                                    attributeTrailingTrivia = attributeTrailingTrivia.Add(t)
                                Case VB.SyntaxKind.WhitespaceTrivia
                                    attributeTrailingTrivia = attributeTrailingTrivia.Add(t)
                                    foundComment = False
                                Case VB.SyntaxKind.LineContinuationTrivia
                                    attributeTrailingTrivia = attributeTrailingTrivia.Add(t)
                                Case Else
                                    Stop
                            End Select
                            If foundComment Then
                                attributeTrailingTrivia = attributeTrailingTrivia.Add(VBEOLTrivia)
                            End If
                        Next
                        originalAttributeListWithTrivia(index) = originalAttributeListWithTrivia(index).With(attributeLeadingTrivia, attributeTrailingTrivia)
                    Next
                End If
                Dim attributeLists As SyntaxList(Of VBS.AttributeListSyntax) = Factory.List(originalAttributeListWithTrivia)
                Dim identifier As VBS.ModifiedIdentifierSyntax = Factory.ModifiedIdentifier(id)

                Dim modifierLeadingTrivia As New SyntaxTriviaList
                Dim newModifiersWithoutComments As New List(Of SyntaxToken)
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
                    newModifiersWithoutComments.Add(m.With(modifierLeadingTrivia, modifierTrailingTrivia))
                Next
                modifiers = Factory.TokenList(newModifiersWithoutComments).ToList
                Dim byValIndex As Integer = modifiers.FindIndex(Function(t As SyntaxToken) t.IsKind(VB.SyntaxKind.ByValKeyword))
                If byValIndex >= 0 Then
                    modifiers.RemoveAt(byValIndex)
                End If
                Dim asClause As VBS.SimpleAsClauseSyntax = Nothing
                If returnType IsNot Nothing Then
                    For Each trivia As SyntaxTrivia In returnType.GetLeadingTrivia
                        Select Case trivia.RawKind
                            Case VB.SyntaxKind.WhitespaceTrivia
                                ' ignore
                            Case VB.SyntaxKind.EndOfLineTrivia
                                ' Ignore
                            Case VB.SyntaxKind.CommentTrivia
                                parameterTrailingTrivia = parameterTrailingTrivia.Add(trivia)
                            Case VB.SyntaxKind.EndIfDirectiveTrivia
                                parameterTrailingTrivia = parameterTrailingTrivia.Add(trivia)
                            Case VB.SyntaxKind.IfDirectiveTrivia
                                ' TODO Ignore for now
                            Case VB.SyntaxKind.DisabledTextTrivia
                                ' TODO Ignore for now
                            Case VB.SyntaxKind.ElseDirectiveTrivia
                                ' TODO Ignore for now
                            Case VB.SyntaxKind.DisableWarningDirectiveTrivia
                                GetStatementwithIssues(node).AddMarker(Factory.EmptyStatement.WithLeadingTrivia(trivia), StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                            Case VB.SyntaxKind.EnableWarningDirectiveTrivia
                                GetStatementwithIssues(node).AddMarker(Factory.EmptyStatement.WithLeadingTrivia(trivia), StatementHandlingOption.AppendEmptyStatement, AllowDuplicates:=True)
                            Case Else
                                Stop
                        End Select
                    Next
                    Dim foundEOL As Boolean
                    Dim initialTriviaList As SyntaxTriviaList = returnType.GetTrailingTrivia
                    For index As Integer = 0 To initialTriviaList.Count - 1
                        Dim trivia As SyntaxTrivia = initialTriviaList(index)
                        Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(initialTriviaList, index, LookaheadCount:=1)

                        Select Case trivia.RawKind
                            Case VB.SyntaxKind.WhitespaceTrivia
                                If nextTrivia.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                                    parameterTrailingTrivia = parameterTrailingTrivia.Add(trivia)
                                End If
                            Case VB.SyntaxKind.EndOfLineTrivia
                                foundEOL = True
                            Case VB.SyntaxKind.CommentTrivia
                                parameterTrailingTrivia = parameterTrailingTrivia.Add(trivia)
                            Case VB.SyntaxKind.EndIfDirectiveTrivia
                                parameterTrailingTrivia = parameterTrailingTrivia.Add(trivia)
                            Case VB.SyntaxKind.IfDirectiveTrivia
                                ' TODO Ignore for now
                            Case VB.SyntaxKind.DisabledTextTrivia
                                ' TODO Ignore for now
                            Case VB.SyntaxKind.ElseIfDirectiveTrivia
                                ' TODO Ignore for now
                            Case VB.SyntaxKind.ElseDirectiveTrivia
                                ' TODO Ignore for now
                            Case VB.SyntaxKind.DisableWarningDirectiveTrivia
                                GetStatementwithIssues(node).AddMarker(Factory.EmptyStatement.WithLeadingTrivia(trivia), StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                            Case VB.SyntaxKind.EnableWarningDirectiveTrivia
                                GetStatementwithIssues(node).AddMarker(Factory.EmptyStatement.WithLeadingTrivia(trivia), StatementHandlingOption.AppendEmptyStatement, AllowDuplicates:=True)
                            Case VB.SyntaxKind.LineContinuationTrivia
                                parameterTrailingTrivia = parameterTrailingTrivia.Add(trivia)
                            Case Else
                                Stop
                        End Select
                    Next
                    If foundEOL Then
                        parameterTrailingTrivia = parameterTrailingTrivia.Add(VBEOLTrivia)
                    End If
                    returnType = returnType.WithLeadingTrivia(If(returnType.GetLeadingTrivia.FirstOrDefault.FullWidth > 0, Nothing, Factory.Space)).WithoutTrailingTrivia
                    asClause = Factory.SimpleAsClause(returnType).WithTrailingTrivia(parameterTrailingTrivia)
                Else
                    identifier = identifier.WithTrailingTrivia(parameterTrailingTrivia)
                End If

                Dim parameterSyntax1 As VBS.ParameterSyntax = Factory.Parameter(attributeLists,
                                                                                Factory.TokenList(modifiers),
                                                                                identifier,
                                                                                asClause,
                                                                                defaultValue) ' .WithLeadingTrivia(Factory.Space)
                Return parameterSyntax1
            End Function

            Public Overrides Function VisitParameterList(node As CSS.ParameterListSyntax) As VB.VisualBasicSyntaxNode
                If node Is Nothing Then
                    Throw New ArgumentNullException(NameOf(node))
                End If

                Dim csSeparators As IEnumerable(Of SyntaxToken) = node.Parameters.GetSeparators
                Dim openParenTokenWithTrivia As SyntaxToken = openParenToken.WithConvertedTriviaFrom(node.OpenParenToken)
                Dim closeParenTokenWithTrivia As SyntaxToken = CloseParenToken.WithConvertedTriviaFrom(node.CloseParenToken)
                Dim items As New List(Of VBS.ParameterSyntax)
                Dim separators As New List(Of SyntaxToken)
                Dim separatorCount As Integer = node.Parameters.Count - 1
                For index As Integer = 0 To separatorCount
                    Dim itemWithTrivia As VBS.ParameterSyntax = DirectCast(node.Parameters(index).Accept(Me), VBS.ParameterSyntax)
                    itemWithTrivia = itemWithTrivia.RemoveModifier(VB.SyntaxKind.ByValKeyword)
                    items.Add(itemWithTrivia.AdjustNodeTrivia(separatorCount > index))
                    If separatorCount > index Then
                        separators.Add(CommaToken.WithConvertedTrailingTriviaFrom(csSeparators(index)))
                    End If
                Next
                RestructureNodesAndSeparators(openParenTokenWithTrivia, items, separators, closeParenTokenWithTrivia)
                Return Factory.ParameterList(openParenTokenWithTrivia, Factory.SeparatedList(items, separators), closeParenTokenWithTrivia)
            End Function

        End Class

    End Class

End Namespace
