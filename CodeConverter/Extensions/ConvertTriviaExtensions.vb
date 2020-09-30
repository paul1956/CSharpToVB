' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports System.Text
Imports CSharpToVBConverter.ToVisualBasic
Imports Microsoft.CodeAnalysis
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBConverter
    Module ConvertTriviaExtensions

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

        <Extension>
        Friend Function ConvertTrivia(t As SyntaxTrivia) As SyntaxTrivia

#Region "Non-structured Trivia"

            Select Case t.RawKind
                Case CS.SyntaxKind.WhitespaceTrivia
                    Return Factory.WhitespaceTrivia(t.ToString)
                Case CS.SyntaxKind.EndOfLineTrivia
                    Return VBEOLTrivia
                Case CS.SyntaxKind.None
                    Return Nothing
                Case CS.SyntaxKind.SingleLineCommentTrivia
                    If t.ToFullString.EndsWith("*/", StringComparison.Ordinal) Then
                        Return Factory.CommentTrivia($"'{ReplaceLeadingSlashes(t.ToFullString.Substring(startIndex:=2, t.ToFullString.Length - 4))}")
                    End If
                    Return Factory.CommentTrivia($"'{ReplaceLeadingSlashes(t.ToFullString.Substring(startIndex:=2))}")
                Case CS.SyntaxKind.MultiLineCommentTrivia
                    If t.ToFullString.EndsWith("*/", StringComparison.Ordinal) Then
                        Return Factory.CommentTrivia($"'{ReplaceLeadingSlashes(t.ToFullString.Substring(startIndex:=2, t.ToFullString.Length - 4)).Replace(vbLf, "", StringComparison.Ordinal)}")
                    End If
                    Return Factory.CommentTrivia($"'{ReplaceLeadingSlashes(t.ToFullString.Substring(startIndex:=2)).Replace(vbLf, newValue:="", StringComparison.Ordinal)}")

                Case CS.SyntaxKind.DocumentationCommentExteriorTrivia
                    Return Factory.SyntaxTrivia(VB.SyntaxKind.CommentTrivia, "'''")
                Case CS.SyntaxKind.DisabledTextTrivia
                    If IgnoredIfDepth > 0 Then
                        Return Factory.DisabledTextTrivia(t.ToString.WithoutNewLines(" "c))
                    End If
                    Return Factory.DisabledTextTrivia(t.ToString.Replace(vbLf, vbCrLf, StringComparison.Ordinal))
                Case CS.SyntaxKind.PreprocessingMessageTrivia
                    Return Factory.CommentTrivia($" ' {t}")

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
                    Dim Name As SyntaxToken = Factory.Identifier(DefineDirective.Name.ValueText)
                    Dim value As VBS.ExpressionSyntax = Factory.TrueLiteralExpression(TrueKeyword)
                    Return Factory.Trivia(Factory.ConstDirectiveTrivia(Name, value).WithConvertedTriviaFrom(DefineDirective).
                                                WithAppendedTriviaFromEndOfDirectiveToken(DefineDirective.EndOfDirectiveToken)
                                       )
                Case CS.SyntaxKind.UndefDirectiveTrivia
                    Dim UndefineDirective As CSS.UndefDirectiveTriviaSyntax = DirectCast(StructuredTrivia, CSS.UndefDirectiveTriviaSyntax)
                    Dim Name As SyntaxToken = Factory.Identifier(UndefineDirective.Name.ValueText)
                    Dim value As VBS.ExpressionSyntax = NothingExpression
                    Return Factory.Trivia(Factory.ConstDirectiveTrivia(Name, value).WithConvertedTriviaFrom(UndefineDirective).
                                                WithAppendedTriviaFromEndOfDirectiveToken(UndefineDirective.EndOfDirectiveToken)
                                        )

                Case CS.SyntaxKind.EndIfDirectiveTrivia
                    If IgnoredIfDepth > 0 Then
                        IgnoredIfDepth -= 1
                        Return Factory.CommentTrivia($"' TODO VB does not allow directives here, original statement {t.ToFullString.WithoutNewLines(" "c)}")
                    End If
                    Dim EndIfDirective As CSS.EndIfDirectiveTriviaSyntax = DirectCast(StructuredTrivia, CSS.EndIfDirectiveTriviaSyntax)
                    Return Factory.Trivia(Factory.EndIfDirectiveTrivia.WithConvertedTrailingTriviaFrom(EndIfDirective.EndIfKeyword).
                                                                WithAppendedTriviaFromEndOfDirectiveToken(EndIfDirective.EndOfDirectiveToken)
                                        )

                Case CS.SyntaxKind.ErrorDirectiveTrivia
                    Dim ErrorDirective As CSS.ErrorDirectiveTriviaSyntax = DirectCast(StructuredTrivia, CSS.ErrorDirectiveTriviaSyntax)
                    Return Factory.CommentTrivia($"' TODO: Check VB does not support Error Directive Trivia, Original Directive {ErrorDirective.ToFullString.WithoutNewLines(" "c)}")
                Case CS.SyntaxKind.IfDirectiveTrivia
                    If t.Token.Parent?.AncestorsAndSelf.OfType(Of CSS.InitializerExpressionSyntax).Any Then
                        IgnoredIfDepth += 1
                    End If
                    Dim IfDirective As CSS.IfDirectiveTriviaSyntax = DirectCast(StructuredTrivia, CSS.IfDirectiveTriviaSyntax)
                    Dim Expression1 As String = IfDirective.Condition.ConvertDirectiveCondition

                    Return Factory.Trivia(Factory.IfDirectiveTrivia(IfKeyword, Factory.ParseExpression(Expression1)).
                                                                With(IfDirective.GetLeadingTrivia.ConvertTriviaList(),
                                                                     IfDirective.Condition.GetTrailingTrivia.ConvertTriviaList()).
                                                                     WithAppendedTriviaFromEndOfDirectiveToken(IfDirective.EndOfDirectiveToken))
                Case CS.SyntaxKind.ElifDirectiveTrivia
                    If t.Token.Parent.AncestorsAndSelf.OfType(Of CSS.InitializerExpressionSyntax).Any Then
                        IgnoredIfDepth += 1
                    End If
                    Dim ELIfDirective As CSS.ElifDirectiveTriviaSyntax = DirectCast(StructuredTrivia, CSS.ElifDirectiveTriviaSyntax)
                    Dim Expression1 As String = ELIfDirective.Condition.ConvertDirectiveCondition

                    Dim IfOrElseIfKeyword As SyntaxToken
                    If t.IsKind(CS.SyntaxKind.ElifDirectiveTrivia) Then
                        IfOrElseIfKeyword = ElseIfKeyword
                    Else
                        IfOrElseIfKeyword = IfKeyword
                    End If
                    Return Factory.Trivia(Factory.ElseIfDirectiveTrivia(IfOrElseIfKeyword,
                                                                    Factory.ParseExpression(Expression1)
                                                                   ).With(ELIfDirective.GetLeadingTrivia.ConvertTriviaList(),
                                                                     ELIfDirective.Condition.GetTrailingTrivia.ConvertTriviaList()).
                                                                     WithAppendedTriviaFromEndOfDirectiveToken(ELIfDirective.EndOfDirectiveToken))
                Case CS.SyntaxKind.LineDirectiveTrivia
                    Return Factory.CommentTrivia($"' TODO: Check VB does not support Line Directive Trivia, Original Directive {t}")
                Case CS.SyntaxKind.ElseDirectiveTrivia
                    Return Factory.Trivia(Factory.ElseDirectiveTrivia.
                                                        NormalizeWhitespace.
                                                        WithConvertedTrailingTriviaFrom(DirectCast(StructuredTrivia, CSS.ElseDirectiveTriviaSyntax).ElseKeyword).WithTrailingEOL)
                Case CS.SyntaxKind.EndRegionDirectiveTrivia
                    Dim EndRegionDirective As CSS.EndRegionDirectiveTriviaSyntax = DirectCast(StructuredTrivia, CSS.EndRegionDirectiveTriviaSyntax)
                    Return Factory.Trivia(Factory.EndRegionDirectiveTrivia(HashToken, EndKeyword, RegionKeyword).
                                        WithAppendedTriviaFromEndOfDirectiveToken(EndRegionDirective.EndOfDirectiveToken))
                Case CS.SyntaxKind.PragmaWarningDirectiveTrivia
                    Return Nothing
                Case CS.SyntaxKind.RegionDirectiveTrivia
                    Dim RegionDirective As CSS.RegionDirectiveTriviaSyntax = CType(StructuredTrivia, CSS.RegionDirectiveTriviaSyntax)
                    Dim EndOfDirectiveToken As SyntaxToken = RegionDirective.EndOfDirectiveToken
                    Dim NameString As String = $"""{EndOfDirectiveToken.LeadingTrivia.ToString.RemoveAll("""")}"""
                    Dim RegionDirectiveTriviaNode As VBS.RegionDirectiveTriviaSyntax =
                        Factory.RegionDirectiveTrivia(HashToken,
                                                      RegionKeyword,
                                                      Factory.StringLiteralToken(NameString, NameString)
                                                     ).WithConvertedTrailingTriviaFrom(EndOfDirectiveToken)
                    Return Factory.Trivia(RegionDirectiveTriviaNode.WithTrailingEOL)
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
                                    xmlNodes.Add(Factory.XmlText(" ").WithLeadingTrivia(Factory.DocumentationCommentExteriorTrivia("'''")))
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
                    Dim DocumentationCommentTrivia As VBS.DocumentationCommentTriviaSyntax = Factory.DocumentationCommentTrivia(Factory.List(xmlNodes.ToArray))
                    If (Not DocumentationCommentTrivia.HasLeadingTrivia) OrElse (Not DocumentationCommentTrivia.GetLeadingTrivia(0).IsKind(VB.SyntaxKind.DocumentationCommentExteriorTrivia)) Then
                        DocumentationCommentTrivia = DocumentationCommentTrivia.WithLeadingTrivia(Factory.DocumentationCommentExteriorTrivia("''' "))
                    End If
                    Dim _DocumentationComment As SyntaxTrivia = Factory.Trivia(DocumentationCommentTrivia.WithTrailingTrivia(Factory.EndOfLine("")))
                    Return _DocumentationComment
                Case CS.SyntaxKind.PragmaChecksumDirectiveTrivia
                    Dim PragmaChecksumDirective As CSS.PragmaChecksumDirectiveTriviaSyntax = DirectCast(StructuredTrivia, CSS.PragmaChecksumDirectiveTriviaSyntax)
                    Dim Guid1 As SyntaxToken = Factory.ParseToken(PragmaChecksumDirective.Guid.Text.ToUpperInvariant)
                    Dim Bytes As SyntaxToken = Factory.ParseToken(PragmaChecksumDirective.Bytes.Text)
                    Dim ExternalSource As SyntaxToken = Factory.ParseToken(PragmaChecksumDirective.File.Text)
                    Return Factory.Trivia(
                            Factory.ExternalChecksumDirectiveTrivia(HashToken,
                                                                    ExternalChecksumKeyword,
                                                                    OpenParenToken,
                                                                    ExternalSource,
                                                                    CommaToken,
                                                                    Guid1,
                                                                    CommaToken,
                                                                    Bytes,
                                                                    CloseParenToken
                                                                    ).WithAppendedTriviaFromEndOfDirectiveToken(PragmaChecksumDirective.EndOfDirectiveToken)
                                     )
                Case CS.SyntaxKind.SkippedTokensTrivia
                    Dim Builder As New StringBuilder
                    For Each tok As SyntaxToken In CType(StructuredTrivia, CSS.SkippedTokensTriviaSyntax).Tokens
                        Builder.Append(tok.ToString)
                    Next
                    Return Factory.CommentTrivia($"' TODO: Error SkippedTokensTrivia '{Builder}'")
                Case CS.SyntaxKind.BadDirectiveTrivia
                    Return Factory.CommentTrivia($"' TODO: Skipped BadDirectiveTrivia")
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
End Namespace
