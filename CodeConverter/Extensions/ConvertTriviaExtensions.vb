' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports System.Text
Imports CSharpToVBCodeConverter.ToVisualBasic
Imports Microsoft.CodeAnalysis
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Module ConvertTriviaExtensions
    Private Function RemoveLeadingSpacesAndStar(line As String) As String
        Dim NewStringBuilder As New StringBuilder
        Dim SkipSpace As Boolean = True
        Dim SkipStar As Boolean = True
        For Each c As String In line
            Select Case c
                Case " "
                    If SkipSpace Then
                        Continue For
                    End If
                    NewStringBuilder.Append(c)
                Case "*"
                    If SkipStar Then
                        SkipSpace = False
                        SkipStar = False
                        Continue For
                    End If
                    NewStringBuilder.Append(c)
                Case Else
                    SkipSpace = False
                    SkipStar = False
                    NewStringBuilder.Append(c)
            End Select
        Next
        Return NewStringBuilder.ToString
    End Function

    Private Function ReplaceLeadingSlashes(CommentTriviaBody As String) As String
        For charIndex As Integer = 0 To CommentTriviaBody.Length - 1
            If CommentTriviaBody.Substring(charIndex, 1) = "/" Then
                CommentTriviaBody = CommentTriviaBody.Remove(charIndex, 1).Insert(charIndex, "'")
            Else
                Exit For
            End If
        Next
        Return CommentTriviaBody
    End Function

    <Extension>
    Friend Function ConvertTrivia(initialTriviaList As IReadOnlyCollection(Of SyntaxTrivia)) As IEnumerable(Of SyntaxTrivia)
        Dim TriviaList As New List(Of SyntaxTrivia)
        If initialTriviaList Is Nothing Then
            Return TriviaList
        End If
        Try
            For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                Dim Trivia As SyntaxTrivia = e.Value
                Dim NextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(initialTriviaList, e.Index)
                Select Case Trivia.RawKind
                    Case CS.SyntaxKind.MultiLineCommentTrivia
                        Dim Lines() As String = Trivia.ToFullString.Substring(2).Split(CType(vbLf, Char))
                        For Each line As String In Lines
                            If line.EndsWith("*/", StringComparison.Ordinal) Then
                                TriviaList.Add(VBFactory.CommentTrivia($"' {RemoveLeadingSpacesAndStar(line.Substring(0, line.Length - 2))}"))
                                If Trivia.ToFullString.EndsWith(vbLf, StringComparison.Ordinal) Then
                                    TriviaList.Add(VBEOLTrivia)
                                End If
                            Else
                                TriviaList.Add(VBFactory.CommentTrivia($"' {RemoveLeadingSpacesAndStar(line)}"))
                                TriviaList.Add(VBEOLTrivia)
                            End If
                            If Lines.Length = 1 AndAlso (e.IsLast OrElse Not initialTriviaList(e.Index + 1).IsEndOfLine) Then
                                TriviaList.Add(VBEOLTrivia)
                            End If
                        Next
                    Case CS.SyntaxKind.NullableDirectiveTrivia
                        Dim StructuredTrivia As CSS.StructuredTriviaSyntax = DirectCast(Trivia.GetStructure, CSS.StructuredTriviaSyntax)
                        Dim NullableDirective As CSS.NullableDirectiveTriviaSyntax = CType(StructuredTrivia, CSS.NullableDirectiveTriviaSyntax)
                        TriviaList.Add(VBFactory.CommentTrivia($"' TODO: Skipped Null-able Directive {NullableDirective.SettingToken.Text} {NullableDirective.TargetToken.Text}"))
                        TriviaList.AddRange(ConvertTrivia(NullableDirective.TargetToken.TrailingTrivia))
                        TriviaList.AddRange(ConvertTrivia(NullableDirective.EndOfDirectiveToken.TrailingTrivia))
                    Case CS.SyntaxKind.MultiLineDocumentationCommentTrivia
                        Dim sld As CSS.StructuredTriviaSyntax = DirectCast(Trivia.GetStructure, CSS.StructuredTriviaSyntax)
                        For Each t1 As SyntaxNode In sld.ChildNodes
                            Dim Lines() As String = t1.ToFullString.Split(CType(vbLf, Char))
                            For Each line As String In Lines
                                If line.StartsWith("/*", StringComparison.Ordinal) Then
                                    TriviaList.Add(VBFactory.CommentTrivia($"' {RemoveLeadingSpacesAndStar(line.Substring(1, line.Length - 1))}"))
                                    TriviaList.Add(VBEOLTrivia)
                                Else
                                    TriviaList.Add(VBFactory.CommentTrivia($"' {RemoveLeadingSpacesAndStar(line)}"))
                                    TriviaList.Add(VBEOLTrivia)
                                End If
                            Next
                        Next
                    Case Else
                        Dim ConvertedTrivia As SyntaxTrivia = ConvertTrivia(Trivia)
                        If ConvertedTrivia = Nothing Then
                            Continue For
                        End If
                        TriviaList.Add(ConvertedTrivia)
                        If Trivia.IsKind(CS.SyntaxKind.SingleLineCommentTrivia) Then
                            If Not NextTrivia.IsKind(CS.SyntaxKind.EndOfLineTrivia) Then
                                TriviaList.Add(VBEOLTrivia)
                            End If
                        End If
                End Select
            Next
        Catch ex As OperationCanceledException
            Throw
        Catch ex As Exception
            Stop
            Throw
        End Try
        Return TriviaList
    End Function

    <Extension>
    Friend Function ConvertTrivia(t As SyntaxTrivia) As SyntaxTrivia

#Region "Non-structured Trivia"

        Select Case t.RawKind
            Case CS.SyntaxKind.WhitespaceTrivia
                Return VBFactory.WhitespaceTrivia(t.ToString)
            Case CS.SyntaxKind.EndOfLineTrivia
                Return VBEOLTrivia
            Case CS.SyntaxKind.SingleLineCommentTrivia
                If t.ToFullString.EndsWith("*/", StringComparison.Ordinal) Then
                    Return VBFactory.CommentTrivia($"'{ReplaceLeadingSlashes(t.ToFullString.Substring(2, t.ToFullString.Length - 4))}")
                End If
                Return VBFactory.CommentTrivia($"'{ReplaceLeadingSlashes(t.ToFullString.Substring(2))}")
            Case CS.SyntaxKind.MultiLineCommentTrivia
                If t.ToFullString.EndsWith("*/", StringComparison.Ordinal) Then
                    Return VBFactory.CommentTrivia($"'{ReplaceLeadingSlashes(t.ToFullString.Substring(2, t.ToFullString.Length - 4)).Replace(vbLf, "", StringComparison.Ordinal)}")
                End If
                Return VBFactory.CommentTrivia($"'{ReplaceLeadingSlashes(t.ToFullString.Substring(2)).Replace(vbLf, "", StringComparison.Ordinal)}")

            Case CS.SyntaxKind.DocumentationCommentExteriorTrivia
                Return VBFactory.SyntaxTrivia(VB.SyntaxKind.CommentTrivia, "'''")
            Case CS.SyntaxKind.DisabledTextTrivia
                If IgnoredIfDepth > 0 Then
                    Return VBFactory.DisabledTextTrivia(t.ToString.WithoutNewLines(" "c))
                End If
                Return VBFactory.DisabledTextTrivia(t.ToString.Replace(vbLf, vbCrLf, StringComparison.Ordinal))
            Case CS.SyntaxKind.PreprocessingMessageTrivia
                Return VBFactory.CommentTrivia($" ' {t}")

            Case CS.SyntaxKind.None
                Return Nothing
        End Select
        If Not t.HasStructure Then
            Stop
        End If

#End Region

#Region "Start of Structured Trivia"

        Dim StructuredTrivia As CSS.StructuredTriviaSyntax = DirectCast(t.GetStructure, CSS.StructuredTriviaSyntax)
        Debug.Assert(StructuredTrivia IsNot Nothing, $"Found new type of non structured trivia {t.RawKind}")

        Select Case t.RawKind
            Case CS.SyntaxKind.DefineDirectiveTrivia
                Dim DefineDirective As CSS.DefineDirectiveTriviaSyntax = DirectCast(StructuredTrivia, CSS.DefineDirectiveTriviaSyntax)
                Dim Name As SyntaxToken = VBFactory.Identifier(DefineDirective.Name.ValueText)
                Dim value As VBS.ExpressionSyntax = VBFactory.TrueLiteralExpression(TrueKeyword)
                Return VBFactory.Trivia(VBFactory.ConstDirectiveTrivia(Name, value).WithConvertedTriviaFrom(DefineDirective).
                                                WithAppendedTriviaFromEndOfDirectiveToken(DefineDirective.EndOfDirectiveToken)
                                       )
            Case CS.SyntaxKind.UndefDirectiveTrivia
                Dim UndefineDirective As CSS.UndefDirectiveTriviaSyntax = DirectCast(StructuredTrivia, CSS.UndefDirectiveTriviaSyntax)
                Dim Name As SyntaxToken = VBFactory.Identifier(UndefineDirective.Name.ValueText)
                Dim value As VBS.ExpressionSyntax = NothingExpression
                Return VBFactory.Trivia(VBFactory.ConstDirectiveTrivia(Name, value).WithConvertedTriviaFrom(UndefineDirective).
                                                WithAppendedTriviaFromEndOfDirectiveToken(UndefineDirective.EndOfDirectiveToken)
                                        )

            Case CS.SyntaxKind.EndIfDirectiveTrivia
                If IgnoredIfDepth > 0 Then
                    IgnoredIfDepth -= 1
                    Return VBFactory.CommentTrivia($"' TODO VB does not allow directives here, original statement {t.ToFullString.WithoutNewLines(" "c)}")
                End If
                Dim EndIfDirective As CSS.EndIfDirectiveTriviaSyntax = DirectCast(StructuredTrivia, CSS.EndIfDirectiveTriviaSyntax)
                Return VBFactory.Trivia(VBFactory.EndIfDirectiveTrivia.WithConvertedTrailingTriviaFrom(EndIfDirective.EndIfKeyword).
                                                                WithAppendedTriviaFromEndOfDirectiveToken(EndIfDirective.EndOfDirectiveToken)
                                        )

            Case CS.SyntaxKind.ErrorDirectiveTrivia
                Dim ErrorDirective As CSS.ErrorDirectiveTriviaSyntax = DirectCast(StructuredTrivia, CSS.ErrorDirectiveTriviaSyntax)
                Return VBFactory.CommentTrivia($"' TODO: Check VB does not support Error Directive Trivia, Original Directive {ErrorDirective.ToFullString.WithoutNewLines(" "c)}")
            Case CS.SyntaxKind.IfDirectiveTrivia
                If t.Token.Parent?.AncestorsAndSelf.OfType(Of CSS.InitializerExpressionSyntax).Any Then
                    IgnoredIfDepth += 1
                End If
                Dim IfDirective As CSS.IfDirectiveTriviaSyntax = DirectCast(StructuredTrivia, CSS.IfDirectiveTriviaSyntax)
                Dim Expression1 As String = IfDirective.Condition.ToString.
                                    Replace("==", "=", StringComparison.Ordinal).
                                    Replace("!=", "Not ", StringComparison.Ordinal).
                                    Replace("&&", "And", StringComparison.Ordinal).
                                    Replace("||", "Or", StringComparison.Ordinal).
                                    Replace("  ", " ", StringComparison.Ordinal).
                                    Replace("!", "Not ", StringComparison.Ordinal).
                                    Replace("false", "False", StringComparison.Ordinal).
                                    Replace("true", "True", StringComparison.Ordinal)

                Return VBFactory.Trivia(VBFactory.IfDirectiveTrivia(IfKeyword, VBFactory.ParseExpression(Expression1)).
                                                                With(ConvertTrivia(IfDirective.GetLeadingTrivia),
                                                                     ConvertTrivia(IfDirective.Condition.GetTrailingTrivia)).
                                                                     WithAppendedTriviaFromEndOfDirectiveToken(IfDirective.EndOfDirectiveToken))
            Case CS.SyntaxKind.ElifDirectiveTrivia
                If t.Token.Parent.AncestorsAndSelf.OfType(Of CSS.InitializerExpressionSyntax).Any Then
                    IgnoredIfDepth += 1
                End If
                Dim ELIfDirective As CSS.ElifDirectiveTriviaSyntax = DirectCast(StructuredTrivia, CSS.ElifDirectiveTriviaSyntax)
                Dim Expression1 As String = ELIfDirective.Condition.ToString.
                                                Replace("!", "Not ", StringComparison.Ordinal).
                                                Replace("==", "=", StringComparison.Ordinal).
                                                Replace("!=", "<>", StringComparison.Ordinal).
                                                Replace("&&", "And", StringComparison.Ordinal).
                                                Replace("||", "Or", StringComparison.Ordinal).
                                                Replace("  ", " ", StringComparison.Ordinal).
                                                Replace("false", "False", StringComparison.Ordinal).
                                                Replace("true", "True", StringComparison.Ordinal)

                Dim IfOrElseIfKeyword As SyntaxToken
                If t.IsKind(CS.SyntaxKind.ElifDirectiveTrivia) Then
                    IfOrElseIfKeyword = ElseIfKeyword
                Else
                    IfOrElseIfKeyword = IfKeyword
                End If
                Return VBFactory.Trivia(VBFactory.ElseIfDirectiveTrivia(IfOrElseIfKeyword,
                                                                        VBFactory.ParseExpression(Expression1)
                                                                        ).With(ConvertTrivia(ELIfDirective.GetLeadingTrivia),
                                                                     ConvertTrivia(ELIfDirective.Condition.GetTrailingTrivia)).
                                                                     WithAppendedTriviaFromEndOfDirectiveToken(ELIfDirective.EndOfDirectiveToken))
            Case CS.SyntaxKind.LineDirectiveTrivia
                Return VBFactory.CommentTrivia($"' TODO: Check VB does not support Line Directive Trivia, Original Directive {t}")
            Case CS.SyntaxKind.ElseDirectiveTrivia
                Return VBFactory.Trivia(VBFactory.ElseDirectiveTrivia.
                                                        NormalizeWhitespace.
                                                        WithConvertedTrailingTriviaFrom(DirectCast(StructuredTrivia, CSS.ElseDirectiveTriviaSyntax).ElseKeyword).WithTrailingEOL)
            Case CS.SyntaxKind.EndRegionDirectiveTrivia
                Dim EndRegionDirective As CSS.EndRegionDirectiveTriviaSyntax = DirectCast(StructuredTrivia, CSS.EndRegionDirectiveTriviaSyntax)
                Return VBFactory.Trivia(VBFactory.EndRegionDirectiveTrivia(HashToken, EndKeyword, RegionKeyword).
                                        WithAppendedTriviaFromEndOfDirectiveToken(EndRegionDirective.EndOfDirectiveToken))
            Case CS.SyntaxKind.PragmaWarningDirectiveTrivia
                Return Nothing
            Case CS.SyntaxKind.RegionDirectiveTrivia
                Dim RegionDirective As CSS.RegionDirectiveTriviaSyntax = CType(StructuredTrivia, CSS.RegionDirectiveTriviaSyntax)
                Dim EndOfDirectiveToken As SyntaxToken = RegionDirective.EndOfDirectiveToken
                Dim NameString As String = $"""{EndOfDirectiveToken.LeadingTrivia.ToString.Replace("""", "", StringComparison.Ordinal)}"""
                Dim RegionDirectiveTriviaNode As VBS.RegionDirectiveTriviaSyntax =
                        VBFactory.RegionDirectiveTrivia(
                                                HashToken,
                                                RegionKeyword,
                                                VBFactory.StringLiteralToken(NameString, NameString)
                                                                        ).WithConvertedTrailingTriviaFrom(EndOfDirectiveToken)
                Return VBFactory.Trivia(RegionDirectiveTriviaNode.WithTrailingEOL)
            Case CS.SyntaxKind.SingleLineDocumentationCommentTrivia
                Dim SingleLineDocumentationComment As CSS.DocumentationCommentTriviaSyntax = CType(StructuredTrivia, CSS.DocumentationCommentTriviaSyntax)
                Dim walker As New XMLVisitor()
                walker.Visit(SingleLineDocumentationComment)

                Dim xmlNodes As New List(Of VBS.XmlNodeSyntax)
                For Each e As IndexClass(Of CSS.XmlNodeSyntax) In SingleLineDocumentationComment.Content.WithIndex
                    Dim node As CSS.XmlNodeSyntax = e.Value
                    If (Not node.IsKind(CS.SyntaxKind.XmlText)) AndAlso node.GetLeadingTrivia.Any AndAlso node.GetLeadingTrivia.First.IsKind(CS.SyntaxKind.DocumentationCommentExteriorTrivia) Then
                        If Not e.IsLast Then
                            Dim NextNode As CSS.XmlNodeSyntax = SingleLineDocumentationComment.Content(e.Index + 1)
                            If (Not NextNode.IsKind(CS.SyntaxKind.XmlText)) OrElse
                                NextNode.GetLeadingTrivia.Count = 0 OrElse
                                Not NextNode.GetLeadingTrivia.First.IsKind(CS.SyntaxKind.DocumentationCommentExteriorTrivia) Then
                                xmlNodes.Add(VBFactory.XmlText(" ").WithLeadingTrivia(VBFactory.DocumentationCommentExteriorTrivia("'''")))
                            End If
                        End If
                        node = node.WithoutLeadingTrivia
                    End If
                    Try
                        Dim Item As VBS.XmlNodeSyntax = DirectCast(node.Accept(walker), VBS.XmlNodeSyntax)
                        xmlNodes.Add(Item)
                    Catch ex As OperationCanceledException
                        Throw
                    Catch ex As Exception
                        Stop
                        Throw
                    End Try
                Next
                Dim DocumentationCommentTrivia As VBS.DocumentationCommentTriviaSyntax = VBFactory.DocumentationCommentTrivia(VBFactory.List(xmlNodes.ToArray))
                If (Not DocumentationCommentTrivia.HasLeadingTrivia) OrElse (Not DocumentationCommentTrivia.GetLeadingTrivia(0).IsKind(VB.SyntaxKind.DocumentationCommentExteriorTrivia)) Then
                    DocumentationCommentTrivia = DocumentationCommentTrivia.WithLeadingTrivia(VBFactory.DocumentationCommentExteriorTrivia("''' "))
                End If
                Dim _DocumentationComment As SyntaxTrivia = VBFactory.Trivia(DocumentationCommentTrivia.WithTrailingTrivia(VBFactory.EndOfLine("")))
                Return _DocumentationComment
            Case CS.SyntaxKind.PragmaChecksumDirectiveTrivia
                Dim PragmaChecksumDirective As CSS.PragmaChecksumDirectiveTriviaSyntax = DirectCast(StructuredTrivia, CSS.PragmaChecksumDirectiveTriviaSyntax)
                Dim Guid1 As SyntaxToken = VBFactory.ParseToken(PragmaChecksumDirective.Guid.Text.ToUpperInvariant)
                Dim Bytes As SyntaxToken = VBFactory.ParseToken(PragmaChecksumDirective.Bytes.Text)
                Dim ExternalSource As SyntaxToken = VBFactory.ParseToken(PragmaChecksumDirective.File.Text)
                Return VBFactory.Trivia(
                            VBFactory.ExternalChecksumDirectiveTrivia(
                                                                HashToken,
                                                                ExternalChecksumKeyword,
                                                                OpenParenToken,
                                                                ExternalSource,
                                                                CommaToken,
                                                                Guid1,
                                                                CommaToken,
                                                                Bytes,
                                                                CloseParenToken).
                                                                WithAppendedTriviaFromEndOfDirectiveToken(PragmaChecksumDirective.EndOfDirectiveToken)
                                                                )
            Case CS.SyntaxKind.SkippedTokensTrivia
                Dim Builder As New StringBuilder
                For Each tok As SyntaxToken In CType(StructuredTrivia, CSS.SkippedTokensTriviaSyntax).Tokens
                    Builder.Append(tok.ToString)
                Next
                Return VBFactory.CommentTrivia($"' TODO: Error SkippedTokensTrivia '{Builder}'")
            Case CS.SyntaxKind.BadDirectiveTrivia
                Return VBFactory.CommentTrivia($"' TODO: Skipped BadDirectiveTrivia")
            Case CS.SyntaxKind.ConflictMarkerTrivia
                Stop
            Case CS.SyntaxKind.LoadDirectiveTrivia
                Stop
                'VB.SyntaxKind.ExternalSourceDirectiveTrivia
            Case Else
                Debug.WriteLine(CType(t.RawKind, VB.SyntaxKind).ToString)
                Stop
        End Select

#End Region

        Throw New NotImplementedException($"t.Kind({CType(t.RawKind, VB.SyntaxKind)}) Is unknown")
    End Function

End Module
