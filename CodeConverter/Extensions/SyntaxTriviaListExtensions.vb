﻿' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports System.Text
Imports CSharpToVBConverter.CSharpToVBVisitors
Imports Microsoft.CodeAnalysis
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBConverter
    Friend Module SyntaxTriviaListExtensions
        Private Function RemoveLeadingSpacesAndStar(line As String) As String
            Dim newStringBuilder As New StringBuilder
            Dim skipSpace As Boolean = True
            Dim skipStar As Boolean = True
            For Each c As String In line
                Select Case c
                    Case " "
                        If skipSpace Then
                            Continue For
                        End If
                        newStringBuilder.Append(c)
                    Case "*"
                        If skipStar Then
                            skipSpace = False
                            skipStar = False
                            Continue For
                        End If
                        newStringBuilder.Append(c)
                    Case Else
                        skipSpace = False
                        skipStar = False
                        newStringBuilder.Append(c)
                End Select
            Next
            Return newStringBuilder.ToString
        End Function

        Private Function ReplaceLeadingSlashes(CommentTriviaBody As String) As String
            For charIndex As Integer = 0 To CommentTriviaBody.Length - 1
                If CommentTriviaBody.Chars(charIndex) = "/"c Then
                    CommentTriviaBody = CommentTriviaBody.Remove(charIndex, count:=1).Insert(charIndex, "'")
                Else
                    Exit For
                End If
            Next
            Return CommentTriviaBody
        End Function

        ''' <summary>
        ''' Syntax trivia in any Language
        ''' </summary>
        ''' <param name="TriviaList"></param>
        ''' <returns>True if any trivia is a Comment or a Directive</returns>
        <Extension>
        Friend Function ContainsCommentOrDirectiveTrivia(TriviaList As SyntaxTriviaList) As Boolean
            If Not TriviaList.Any Then
                Return False
            End If
            For Each t As SyntaxTrivia In TriviaList
                If t.IsWhitespaceOrEndOfLine Then
                    Continue For
                End If
                If t.IsNone Then
                    Continue For
                End If
                If t.IsCommentOrDirectiveTrivia Then
                    Return True
                End If
                If t.RawKind = VB.SyntaxKind.LineContinuationTrivia Then
                    Continue For
                End If
                If t.RawKind = VB.SyntaxKind.SkippedTokensTrivia Then
                    Continue For
                End If
                If t.RawKind = CS.SyntaxKind.SkippedTokensTrivia Then
                    Continue For
                End If
                If t.RawKind = VB.SyntaxKind.DisabledTextTrivia Then
                    Continue For
                End If
                If t.IsKind(VB.SyntaxKind.DocumentationCommentTrivia) Then
                    Return True
                End If
                Stop
            Next
            Return False
        End Function

        <Extension>
        Friend Function ContainsCommentTrivia(TriviaList As SyntaxTriviaList) As Boolean
            If TriviaList.Count = 0 Then
                Return False
            End If
            For Each t As SyntaxTrivia In TriviaList
                If t.IsWhitespaceOrEndOfLine Then
                    Continue For
                ElseIf t.IsNone Then
                    Continue For
                ElseIf t.IsDirective Then
                    Continue For
                ElseIf t.IsComment Then
                    Return True
                ElseIf t.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                    Continue For
                ElseIf t.IsSkippedOrDisabledTrivia Then
                    Continue For
                End If
                Stop
            Next
            Return False
        End Function

        ''' <summary>
        ''' Syntax trivia in any Language
        ''' </summary>
        ''' <param name="TriviaList"></param>
        ''' <returns>True if any trivia is a Comment or a Directive</returns>
        <Extension>
        Friend Function ContainsDirectiveTrivia(TriviaList As SyntaxTriviaList, ParamArray Kinds() As VB.SyntaxKind) As Boolean
            If TriviaList.Count = 0 Then Return False
            For Each t As SyntaxTrivia In TriviaList
                If t.IsDirective Then
                    If Kinds.Length = 0 Then
                        Return True
                    End If
                    For Each k As VB.SyntaxKind In Kinds
                        If t.RawKind = k Then
                            Return True
                        End If
                    Next
                    Return False
                End If
            Next
            Return False
        End Function

        ''' <summary>
        ''' Syntax trivia in any Language
        ''' </summary>
        ''' <param name="TriviaList"></param>
        ''' <returns>True if any trivia is EndIf Directive</returns>
        <Extension>
        Friend Function ContainsEndIfTrivia(TriviaList As SyntaxTriviaList) As Boolean
            If TriviaList.Count = 0 Then Return False
            For Each t As SyntaxTrivia In TriviaList
                If t.IsKind(VB.SyntaxKind.EndIfDirectiveTrivia) Then
                    Return True
                End If
            Next
            Return False
        End Function

        <Extension>
        Friend Function ContainsEOLTrivia(TriviaList As SyntaxTriviaList) As Boolean
            For Each t As SyntaxTrivia In TriviaList
                If t.IsEndOfLine Then
                    Return True
                End If
            Next
            Return False
        End Function

        <Extension>
        Friend Function ContainsLineContinueTrivia(TriviaList As SyntaxTriviaList) As Boolean
            For Each t As SyntaxTrivia In TriviaList
                If t.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                    Return True
                End If
            Next
            Return False
        End Function

        <Extension>
        Friend Function ConvertTriviaList(initialTriviaList As SyntaxTriviaList) As SyntaxTriviaList
            Dim newTriviaList As New List(Of SyntaxTrivia)
            If Not initialTriviaList.Any Then
                Return New SyntaxTriviaList
            End If
            Try
                For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                    Dim trivia As SyntaxTrivia = e.Value
                    Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(initialTriviaList, e.index, LookaheadCount:=1)
                    Dim structuredTrivia As CSS.StructuredTriviaSyntax = DirectCast(trivia.GetStructure, CSS.StructuredTriviaSyntax)
                    Select Case trivia.RawKind
                        Case CS.SyntaxKind.WhitespaceTrivia
                            newTriviaList.Add(Factory.WhitespaceTrivia(trivia.ToString))
                            Continue For
                        Case CS.SyntaxKind.EndOfLineTrivia
                            newTriviaList.Add(VBEOLTrivia)
                            Continue For
                        Case CS.SyntaxKind.None
                            Continue For
                        Case CS.SyntaxKind.SingleLineDocumentationCommentTrivia
                            Dim singleLineDocumentationComment As CSS.DocumentationCommentTriviaSyntax = CType(structuredTrivia, CSS.DocumentationCommentTriviaSyntax)
                            Dim walker As New XMLVisitor()
                            walker.Visit(singleLineDocumentationComment)

                            Dim xmlNodes As New List(Of VBS.XmlNodeSyntax)
                            For Each e1 As IndexClass(Of CSS.XmlNodeSyntax) In singleLineDocumentationComment.Content.WithIndex
                                Dim node As CSS.XmlNodeSyntax = e1.Value
                                If (Not node.IsKind(CS.SyntaxKind.XmlText)) AndAlso node.GetLeadingTrivia.Any AndAlso node.GetLeadingTrivia.First.IsKind(CS.SyntaxKind.DocumentationCommentExteriorTrivia) Then
                                    If Not e1.IsLast Then
                                        Dim nextNode As CSS.XmlNodeSyntax = singleLineDocumentationComment.Content(e1.index + 1)
                                        If (Not nextNode.IsKind(CS.SyntaxKind.XmlText)) OrElse
                                            nextNode.GetLeadingTrivia.Count = 0 OrElse
                                            Not nextNode.GetLeadingTrivia.First.IsKind(CS.SyntaxKind.DocumentationCommentExteriorTrivia) Then
                                            xmlNodes.Add(Factory.XmlText(" ").WithLeadingTrivia(Factory.DocumentationCommentExteriorTrivia("'''")))
                                        End If
                                    End If
                                    node = node.WithoutLeadingTrivia
                                End If
                                Try
                                    Dim item As VBS.XmlNodeSyntax = DirectCast(node.Accept(walker), VBS.XmlNodeSyntax)
                                    xmlNodes.Add(item)
                                Catch ex As OperationCanceledException
                                    Throw
                                Catch ex As Exception
                                    Stop
                                    Throw
                                End Try
                            Next
                            Dim documentationCommentTrivia As VBS.DocumentationCommentTriviaSyntax = Factory.DocumentationCommentTrivia(Factory.List(xmlNodes.ToArray))
                            If (Not documentationCommentTrivia.HasLeadingTrivia) OrElse (Not documentationCommentTrivia.GetLeadingTrivia(0).IsKind(VB.SyntaxKind.DocumentationCommentExteriorTrivia)) Then
                                documentationCommentTrivia = documentationCommentTrivia.WithLeadingTrivia(Factory.DocumentationCommentExteriorTrivia("''' "))
                            End If
                            newTriviaList.Add(Factory.Trivia(documentationCommentTrivia.WithTrailingTrivia(Factory.EndOfLine(""))))
                            Continue For
                        Case CS.SyntaxKind.SingleLineCommentTrivia
                            If trivia.ToFullString.EndsWith("*/", StringComparison.Ordinal) Then
                                newTriviaList.Add(Factory.CommentTrivia($"'{ReplaceLeadingSlashes(trivia.ToFullString.Substring(startIndex:=2, trivia.ToFullString.Length - 4))}"))
                            Else
                                newTriviaList.Add(Factory.CommentTrivia($"'{ReplaceLeadingSlashes(trivia.ToFullString.Substring(startIndex:=2))}"))
                            End If
                        Case CS.SyntaxKind.MultiLineCommentTrivia
                            Dim lines() As String = trivia.ToFullString.Substring(2).SplitLines
                            For Each line As String In lines
                                If line.EndsWith("*/", StringComparison.Ordinal) Then
                                    newTriviaList.Add(Factory.CommentTrivia($"' {RemoveLeadingSpacesAndStar(line.Substring(0, line.Length - 2))}"))
                                Else
                                    newTriviaList.Add(Factory.CommentTrivia($"' {RemoveLeadingSpacesAndStar(line)}"))
                                    newTriviaList.Add(VBEOLTrivia)
                                End If
                            Next
                        Case CS.SyntaxKind.DocumentationCommentExteriorTrivia
                            newTriviaList.Add(Factory.DocumentationCommentExteriorTrivia(trivia.ToString().Replace("///", "'''", StringComparison.Ordinal)))
                            Continue For
                        Case CS.SyntaxKind.MultiLineDocumentationCommentTrivia
                            For Each t1 As SyntaxNode In structuredTrivia.ChildNodes
                                Dim lines() As String = t1.ToFullString.Split(CType(vbLf, Char))
                                For Each e1 As IndexClass(Of String) In lines.WithIndex
                                    Dim line As String = e1.Value
                                    If line.StartsWith("/*", StringComparison.Ordinal) Then
                                        newTriviaList.Add(Factory.CommentTrivia($"' {RemoveLeadingSpacesAndStar(line.Substring(1, line.Length - 1))}"))
                                    Else
                                        newTriviaList.Add(Factory.CommentTrivia($"' {RemoveLeadingSpacesAndStar(line)}"))
                                    End If
                                    If Not e1.IsLast Then
                                        newTriviaList.Add(VBEOLTrivia)
                                    End If
                                Next
                            Next
                        Case CS.SyntaxKind.IfDirectiveTrivia
                            Dim classDCL As CSS.ClassDeclarationSyntax = Nothing
                            If TypeOf trivia.Token.Parent Is CSS.ClassDeclarationSyntax Then
                                classDCL = CType(trivia.Token.Parent, CSS.ClassDeclarationSyntax)
                                If classDCL.Keyword.SpanStart > trivia.SpanStart OrElse (classDCL.BaseList IsNot Nothing AndAlso classDCL.BaseList.FullSpan.End < trivia.FullSpan.End) Then
                                    classDCL = Nothing
                                End If
                            End If

                            If classDCL IsNot Nothing Then
                                IgnoredIfDepth += 1
                                newTriviaList.Add(Factory.CommentTrivia($"' TODO VB does not allow directives here, original statement {trivia.ToFullString.WithoutNewLines(" "c)}"))
                            Else
                                If trivia.Token.Parent?.AncestorsAndSelf.OfType(Of CSS.InitializerExpressionSyntax).Any Then
                                    IgnoredIfDepth += 1
                                ElseIf classDCL IsNot Nothing Then
                                    IgnoredIfDepth += 1
                                    newTriviaList.Add(Factory.CommentTrivia($"' TODO VB does not allow directives here, original statement {trivia.ToFullString.WithoutNewLines(" "c)}"))
                                End If
                                Dim ifDirective As CSS.IfDirectiveTriviaSyntax = DirectCast(structuredTrivia, CSS.IfDirectiveTriviaSyntax)
                                Dim expression1 As String = ifDirective.Condition.ConvertDirectiveCondition

                                newTriviaList.Add(Factory.Trivia(Factory.IfDirectiveTrivia(IfKeyword,
                                                                                       Factory.ParseExpression(expression1)).With(ifDirective.GetLeadingTrivia.ConvertTriviaList(),
                                                                                                                                  ifDirective.Condition.GetTrailingTrivia.ConvertTriviaList()).WithAppendedTriviaFromEndOfDirectiveToken(ifDirective.EndOfDirectiveToken)))
                            End If
                        Case CS.SyntaxKind.ElifDirectiveTrivia
                            If IgnoredIfDepth = 0 AndAlso trivia.Token.Parent.AncestorsAndSelf.OfType(Of CSS.InitializerExpressionSyntax).Any Then
                                IgnoredIfDepth = 1
                            End If
                            Dim elIfDirective As CSS.ElifDirectiveTriviaSyntax = DirectCast(structuredTrivia, CSS.ElifDirectiveTriviaSyntax)
                            Dim expression1 As String = elIfDirective.Condition.ConvertDirectiveCondition

                            Dim ifOrElseIfKeyword As SyntaxToken
                            If trivia.IsKind(CS.SyntaxKind.ElifDirectiveTrivia) Then
                                ifOrElseIfKeyword = ElseIfKeyword
                            Else
                                ifOrElseIfKeyword = IfKeyword
                            End If
                            newTriviaList.Add(Factory.Trivia(Factory.ElseIfDirectiveTrivia(ifOrElseIfKeyword, Factory.ParseExpression(expression1)).With(elIfDirective.GetLeadingTrivia.ConvertTriviaList(), elIfDirective.Condition.GetTrailingTrivia.ConvertTriviaList()).WithAppendedTriviaFromEndOfDirectiveToken(elIfDirective.EndOfDirectiveToken)))
                        Case CS.SyntaxKind.ElseDirectiveTrivia
                            If IgnoredIfDepth > 0 Then
                                newTriviaList.AddRange(SpaceLineContinue)
                                newTriviaList.Add(Factory.CommentTrivia($" ' TODO VB does not allow directives here, original statement {trivia.ToFullString.WithoutNewLines(" "c)}"))
                            Else
                                newTriviaList.Add(Factory.Trivia(Factory.ElseDirectiveTrivia.
                                                        NormalizeWhitespace.
                                                        WithConvertedTrailingTriviaFrom(DirectCast(structuredTrivia, CSS.ElseDirectiveTriviaSyntax).ElseKeyword).WithTrailingEOL))
                            End If
                        Case CS.SyntaxKind.EndIfDirectiveTrivia
                            If IgnoredIfDepth > 0 Then
                                IgnoredIfDepth -= 1
                                newTriviaList.AddRange(SpaceLineContinue)
                                newTriviaList.Add(Factory.CommentTrivia($"' TODO VB does not allow directives here, original statement {trivia.ToFullString.WithoutNewLines(" "c)}"))
                            Else
                                Dim endIfDirective As CSS.EndIfDirectiveTriviaSyntax = DirectCast(structuredTrivia, CSS.EndIfDirectiveTriviaSyntax)
                                newTriviaList.Add(Factory.Trivia(Factory.EndIfDirectiveTrivia.WithConvertedTrailingTriviaFrom(endIfDirective.EndIfKeyword).
                                                                WithAppendedTriviaFromEndOfDirectiveToken(endIfDirective.EndOfDirectiveToken))
                                        )
                            End If
                        Case CS.SyntaxKind.DisabledTextTrivia
                            If IgnoredIfDepth > 0 Then
                                Dim lines() As String = trivia.ToFullString.Substring(2).SplitLines
                                For Each line As String In lines
                                    newTriviaList.AddRange(SpaceLineContinue)
                                    newTriviaList.Add(Factory.CommentTrivia($" ' {line}"))
                                    newTriviaList.Add(VBEOLTrivia)
                                Next
                            Else
                                newTriviaList.Add(Factory.DisabledTextTrivia(trivia.ToString.Replace(vbLf, vbCrLf, StringComparison.Ordinal)))
                            End If
                        Case CS.SyntaxKind.RegionDirectiveTrivia
                            Dim regionDirective As CSS.RegionDirectiveTriviaSyntax = CType(structuredTrivia, CSS.RegionDirectiveTriviaSyntax)
                            Dim endOfDirectiveToken As SyntaxToken = regionDirective.EndOfDirectiveToken
                            Dim nameString As String = $"""{endOfDirectiveToken.LeadingTrivia.ToString.RemoveAll("""")}"""
                            Dim regionDirectiveTriviaNode As VBS.RegionDirectiveTriviaSyntax =
                            Factory.RegionDirectiveTrivia(HashToken,
                                                          RegionKeyword,
                                                          Factory.StringLiteralToken(nameString, nameString)
                                                         ).WithConvertedTrailingTriviaFrom(endOfDirectiveToken)
                            newTriviaList.Add(Factory.Trivia(regionDirectiveTriviaNode.WithTrailingEOL))
                        Case CS.SyntaxKind.EndRegionDirectiveTrivia
                            Dim endRegionDirective As CSS.EndRegionDirectiveTriviaSyntax = DirectCast(structuredTrivia, CSS.EndRegionDirectiveTriviaSyntax)
                            newTriviaList.Add(Factory.Trivia(Factory.EndRegionDirectiveTrivia(HashToken, EndKeyword, RegionKeyword).
                                        WithAppendedTriviaFromEndOfDirectiveToken(endRegionDirective.EndOfDirectiveToken)))
                        Case CS.SyntaxKind.DefineDirectiveTrivia
                            Dim defineDirective As CSS.DefineDirectiveTriviaSyntax = DirectCast(structuredTrivia, CSS.DefineDirectiveTriviaSyntax)
                            Dim name As SyntaxToken = Factory.Identifier(defineDirective.Name.ValueText)
                            Dim value As VBS.ExpressionSyntax = Factory.TrueLiteralExpression(TrueKeyword)
                            newTriviaList.Add(Factory.Trivia(Factory.ConstDirectiveTrivia(name, value) _
                                                                        .WithConvertedTriviaFrom(defineDirective) _
                                                                        .WithAppendedTriviaFromEndOfDirectiveToken(defineDirective.EndOfDirectiveToken)
                                                            )
                                              )
                        Case CS.SyntaxKind.UndefDirectiveTrivia
                            Dim undefineDirective As CSS.UndefDirectiveTriviaSyntax = DirectCast(structuredTrivia, CSS.UndefDirectiveTriviaSyntax)
                            Dim name As SyntaxToken = Factory.Identifier(undefineDirective.Name.ValueText)
                            Dim value As VBS.ExpressionSyntax = NothingExpression
                            newTriviaList.Add(Factory.Trivia(Factory.ConstDirectiveTrivia(name, value) _
                                                                    .WithConvertedTriviaFrom(undefineDirective) _
                                                                    .WithAppendedTriviaFromEndOfDirectiveToken(undefineDirective.EndOfDirectiveToken)
                                                             )
                                             )

                        Case CS.SyntaxKind.PreprocessingMessageTrivia
                            newTriviaList.Add(Factory.CommentTrivia($" ' {trivia}"))
                        Case CS.SyntaxKind.LineDirectiveTrivia
                            newTriviaList.Add(Factory.CommentTrivia($"' TODO: Check VB does not support Line Directive trivia, Original Directive {trivia}"))
                        Case CS.SyntaxKind.PragmaChecksumDirectiveTrivia
                            Dim pragmaChecksumDirective As CSS.PragmaChecksumDirectiveTriviaSyntax = DirectCast(structuredTrivia, CSS.PragmaChecksumDirectiveTriviaSyntax)
                            Dim guid1 As SyntaxToken = Factory.ParseToken(pragmaChecksumDirective.Guid.Text.ToUpperInvariant)
                            Dim bytes As SyntaxToken = Factory.ParseToken(pragmaChecksumDirective.Bytes.Text)
                            Dim externalSource As SyntaxToken = Factory.ParseToken(pragmaChecksumDirective.File.Text)
                            newTriviaList.Add(Factory.Trivia(
                            Factory.ExternalChecksumDirectiveTrivia(HashToken,
                                                                    ExternalChecksumKeyword,
                                                                    openParenToken,
                                                                    externalSource,
                                                                    CommaToken,
                                                                    guid1,
                                                                    CommaToken,
                                                                    bytes,
                                                                    CloseParenToken
                                                                    ).WithAppendedTriviaFromEndOfDirectiveToken(pragmaChecksumDirective.EndOfDirectiveToken)
                                     ))
                        Case CS.SyntaxKind.SkippedTokensTrivia
                            Dim builder As New StringBuilder
                            For Each tok As SyntaxToken In CType(structuredTrivia, CSS.SkippedTokensTriviaSyntax).Tokens
                                builder.Append(tok.ToString)
                            Next
                            newTriviaList.Add(Factory.CommentTrivia($"' TODO: Error SkippedTokensTrivia '{builder}'"))
                        Case CS.SyntaxKind.BadDirectiveTrivia
                            newTriviaList.Add(Factory.CommentTrivia($"' TODO: Skipped BadDirectiveTrivia"))
                        Case CS.SyntaxKind.NullableDirectiveTrivia
                            Dim nullableDirective As CSS.NullableDirectiveTriviaSyntax = CType(structuredTrivia, CSS.NullableDirectiveTriviaSyntax)
                            newTriviaList.Add(Factory.CommentTrivia($"' TODO: Skipped Null-able Directive {nullableDirective.SettingToken.Text} {nullableDirective.TargetToken.Text}"))
                            newTriviaList.AddRange(nullableDirective.TargetToken.TrailingTrivia.ConvertTriviaList())
                            newTriviaList.AddRange(nullableDirective.EndOfDirectiveToken.TrailingTrivia.ConvertTriviaList())
                        Case CS.SyntaxKind.ErrorDirectiveTrivia
                            Dim errorDirective As CSS.ErrorDirectiveTriviaSyntax = DirectCast(structuredTrivia, CSS.ErrorDirectiveTriviaSyntax)
                            newTriviaList.Add(Factory.CommentTrivia($"' TODO: Check VB does not support Error Directive trivia, Original Directive {errorDirective.ToFullString.WithoutNewLines(" "c)}"))
                        Case CS.SyntaxKind.ConflictMarkerTrivia,
                             CS.SyntaxKind.LoadDirectiveTrivia,
                             CS.SyntaxKind.PragmaWarningDirectiveTrivia
                            Continue For
                        Case Else
                            Debug.WriteLine(CType(trivia.RawKind, VB.SyntaxKind).ToString)
                            Stop
                            Throw New NotImplementedException($"t.Kind({CType(trivia.RawKind, VB.SyntaxKind)}) Is unknown")
                    End Select
                    If Not nextTrivia.IsKind(CS.SyntaxKind.EndOfLineTrivia) AndAlso Not newTriviaList.Last.IsEndOfLine Then
                        newTriviaList.Add(VBEOLTrivia)
                    End If
                Next
            Catch ex As OperationCanceledException
                Throw
            Catch ex As Exception
                Stop
                Throw
            End Try
            Return newTriviaList.ToSyntaxTriviaList
        End Function

        <Extension>
        Friend Function GetDocumentBanner(TriviaList As SyntaxTriviaList) As SyntaxTriviaList
            Dim banner As New SyntaxTriviaList
            For Each e As IndexClass(Of SyntaxTrivia) In TriviaList.WithIndex
                Dim t As SyntaxTrivia = e.Value
                Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(TriviaList, e.index, LookaheadCount:=1)

                If t.IsKind(CS.SyntaxKind.SingleLineCommentTrivia) Then
                    If nextTrivia.IsKind(CS.SyntaxKind.EndOfLineTrivia) Then
                        banner = banner.Add(t)
                        banner = banner.Add(nextTrivia)
                        e.MoveNext()
                        Continue For
                    End If
                End If
                Exit For
            Next
            Return banner.ConvertTriviaList
        End Function

        <Extension>
        Friend Function GetForwardTriviaOrDefault(TriviaList As SyntaxTriviaList, index As Integer, LookaheadCount As Integer) As SyntaxTrivia
            Dim triviaIndex As Integer = index + LookaheadCount
            If triviaIndex < 0 Then
                Return New SyntaxTrivia
            End If
            Return If(triviaIndex < TriviaList.Count, TriviaList(triviaIndex), New SyntaxTrivia)
        End Function

        <Extension>
        Friend Function GetRange(InitialTriviaList As SyntaxTriviaList, start As Integer, [End] As Integer) As SyntaxTriviaList
            If start < 0 OrElse start > InitialTriviaList.Count - 1 Then
                Throw New ArgumentException("Argument out of Range", NameOf(start))
            End If
            If [End] < start OrElse [End] > InitialTriviaList.Count - 1 Then
                Throw New ArgumentException("Argument out of Range", NameOf([End]))
            End If
            Dim newTriviaList As New SyntaxTriviaList
            For i As Integer = start To [End]
                newTriviaList = newTriviaList.Add(InitialTriviaList(i))
            Next
            Return newTriviaList
        End Function

        <Extension>
        Friend Function IndexOfLast(TriviaList As SyntaxTriviaList, Kind As VB.SyntaxKind) As Integer
            For i As Integer = TriviaList.Count - 1 To 0 Step -1
                If TriviaList(i).IsKind(Kind) Then
                    Return i
                End If
            Next
            Return -1
        End Function

        Friend Sub ModifyTrailingTrivia(ModifierTriviaList As SyntaxTriviaList, ByRef newTrailingTrivia As SyntaxTriviaList)
            For Each t As SyntaxTrivia In ModifierTriviaList
                Select Case t.RawKind
                    Case VB.SyntaxKind.None
                    Case VB.SyntaxKind.WhitespaceTrivia
                        newTrailingTrivia = newTrailingTrivia.Add(SpaceTrivia)
                    Case VB.SyntaxKind.EndOfLineTrivia
                        newTrailingTrivia = newTrailingTrivia.Add(SpaceTrivia)
                    Case VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.ElseDirectiveTrivia, VB.SyntaxKind.ElseIfDirectiveTrivia,
                     VB.SyntaxKind.DisabledTextTrivia, VB.SyntaxKind.EndIfDirectiveTrivia
                        newTrailingTrivia = newTrailingTrivia.Add(t)
                    Case VB.SyntaxKind.CommentTrivia
                        newTrailingTrivia = newTrailingTrivia.Add(t)
                    Case VB.SyntaxKind.DocumentationCommentTrivia
                        newTrailingTrivia = newTrailingTrivia.Add(t)
                    Case Else
                        Stop
                End Select
            Next
        End Sub

        <Extension>
        Friend Function RemoveRange(collection As SyntaxTriviaList, items As SyntaxTriviaList) As SyntaxTriviaList
            For Each item As SyntaxTrivia In items
                collection = collection.Remove(item)
            Next item
            Return collection
        End Function

        <Extension>
        Friend Function ToSyntaxTriviaList(l As IEnumerable(Of SyntaxTrivia)) As SyntaxTriviaList
            Dim newSyntaxTriviaList As New SyntaxTriviaList
            Return newSyntaxTriviaList.AddRange(l)
        End Function

        <Extension>
        Friend Function WithoutLastLineContinuation(triviaList As SyntaxTriviaList) As SyntaxTriviaList
            Dim lineContIndex As Integer = triviaList.IndexOfLast(VB.SyntaxKind.LineContinuationTrivia)
            If lineContIndex = -1 Then Return triviaList
            Dim returnList As SyntaxTriviaList = triviaList
            Return returnList.RemoveAt(lineContIndex).RemoveAt(lineContIndex - 1)
        End Function

        <Extension>
        Friend Function WithLastLineContinuation(triviaList As SyntaxTriviaList) As SyntaxTriviaList
            Dim eolIndex As Integer = triviaList.IndexOfLast(VB.SyntaxKind.EndOfLineTrivia)
            Dim lastIndex As Integer = triviaList.Count - 1
            If lastIndex = -1 OrElse eolIndex = -1 Then
                triviaList = triviaList.AddRange(SpaceLineContinueEOL)
                Return triviaList
            End If
            triviaList = triviaList.InsertRange(eolIndex, SpaceLineContinue)
            Return triviaList
        End Function

    End Module
End Namespace
