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
    Friend Module ConvertTriviaExtensions

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

#Region "Non-structured trivia"

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
                    Return Factory.SyntaxTrivia(VB.SyntaxKind.CommentTrivia, t.ToString.Replace("///", "'''"))
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

#Region "Start of Structured trivia"

            Dim structuredTrivia As CSS.StructuredTriviaSyntax = DirectCast(t.GetStructure, CSS.StructuredTriviaSyntax)
            Debug.Assert(structuredTrivia IsNot Nothing, $"Found new type of non structured trivia {t.RawKind}")

            Select Case t.RawKind
                Case CS.SyntaxKind.DefineDirectiveTrivia
                    Dim defineDirective As CSS.DefineDirectiveTriviaSyntax = DirectCast(structuredTrivia, CSS.DefineDirectiveTriviaSyntax)
                    Dim name As SyntaxToken = Factory.Identifier(defineDirective.Name.ValueText)
                    Dim value As VBS.ExpressionSyntax = Factory.TrueLiteralExpression(TrueKeyword)
                    Return Factory.Trivia(Factory.ConstDirectiveTrivia(name, value).WithConvertedTriviaFrom(defineDirective).
                                                WithAppendedTriviaFromEndOfDirectiveToken(defineDirective.EndOfDirectiveToken)
                                       )
                Case CS.SyntaxKind.UndefDirectiveTrivia
                    Dim undefineDirective As CSS.UndefDirectiveTriviaSyntax = DirectCast(structuredTrivia, CSS.UndefDirectiveTriviaSyntax)
                    Dim name As SyntaxToken = Factory.Identifier(undefineDirective.Name.ValueText)
                    Dim value As VBS.ExpressionSyntax = NothingExpression
                    Return Factory.Trivia(Factory.ConstDirectiveTrivia(name, value).WithConvertedTriviaFrom(undefineDirective).
                                                WithAppendedTriviaFromEndOfDirectiveToken(undefineDirective.EndOfDirectiveToken)
                                        )

                Case CS.SyntaxKind.EndIfDirectiveTrivia
                    If IgnoredIfDepth > 0 Then
                        IgnoredIfDepth -= 1
                        Return Factory.CommentTrivia($"' TODO VB does not allow directives here, original statement {t.ToFullString.WithoutNewLines(" "c)}")
                    End If
                    Dim endIfDirective As CSS.EndIfDirectiveTriviaSyntax = DirectCast(structuredTrivia, CSS.EndIfDirectiveTriviaSyntax)
                    Return Factory.Trivia(Factory.EndIfDirectiveTrivia.WithConvertedTrailingTriviaFrom(endIfDirective.EndIfKeyword).
                                                                WithAppendedTriviaFromEndOfDirectiveToken(endIfDirective.EndOfDirectiveToken)
                                        )

                Case CS.SyntaxKind.ErrorDirectiveTrivia
                    Dim errorDirective As CSS.ErrorDirectiveTriviaSyntax = DirectCast(structuredTrivia, CSS.ErrorDirectiveTriviaSyntax)
                    Return Factory.CommentTrivia($"' TODO: Check VB does not support Error Directive trivia, Original Directive {errorDirective.ToFullString.WithoutNewLines(" "c)}")
                Case CS.SyntaxKind.IfDirectiveTrivia
                    If t.Token.Parent?.AncestorsAndSelf.OfType(Of CSS.InitializerExpressionSyntax).Any Then
                        IgnoredIfDepth += 1
                    End If
                    Dim ifDirective As CSS.IfDirectiveTriviaSyntax = DirectCast(structuredTrivia, CSS.IfDirectiveTriviaSyntax)
                    Dim expression1 As String = ifDirective.Condition.ConvertDirectiveCondition

                    Return Factory.Trivia(Factory.IfDirectiveTrivia(IfKeyword, Factory.ParseExpression(expression1)).
                                                                With(ifDirective.GetLeadingTrivia.ConvertTriviaList(),
                                                                     ifDirective.Condition.GetTrailingTrivia.ConvertTriviaList()).
                                                                     WithAppendedTriviaFromEndOfDirectiveToken(ifDirective.EndOfDirectiveToken))
                Case CS.SyntaxKind.ElifDirectiveTrivia
                    If t.Token.Parent.AncestorsAndSelf.OfType(Of CSS.InitializerExpressionSyntax).Any Then
                        IgnoredIfDepth += 1
                    End If
                    Dim elIfDirective As CSS.ElifDirectiveTriviaSyntax = DirectCast(structuredTrivia, CSS.ElifDirectiveTriviaSyntax)
                    Dim expression1 As String = elIfDirective.Condition.ConvertDirectiveCondition

                    Dim ifOrElseIfKeyword As SyntaxToken
                    If t.IsKind(CS.SyntaxKind.ElifDirectiveTrivia) Then
                        ifOrElseIfKeyword = ElseIfKeyword
                    Else
                        ifOrElseIfKeyword = IfKeyword
                    End If
                    Return Factory.Trivia(Factory.ElseIfDirectiveTrivia(ifOrElseIfKeyword,
                                                                    Factory.ParseExpression(expression1)
                                                                   ).With(elIfDirective.GetLeadingTrivia.ConvertTriviaList(),
                                                                     elIfDirective.Condition.GetTrailingTrivia.ConvertTriviaList()).
                                                                     WithAppendedTriviaFromEndOfDirectiveToken(elIfDirective.EndOfDirectiveToken))
                Case CS.SyntaxKind.LineDirectiveTrivia
                    Return Factory.CommentTrivia($"' TODO: Check VB does not support Line Directive trivia, Original Directive {t}")
                Case CS.SyntaxKind.ElseDirectiveTrivia
                    Return Factory.Trivia(Factory.ElseDirectiveTrivia.
                                                        NormalizeWhitespace.
                                                        WithConvertedTrailingTriviaFrom(DirectCast(structuredTrivia, CSS.ElseDirectiveTriviaSyntax).ElseKeyword).WithTrailingEOL)
                Case CS.SyntaxKind.EndRegionDirectiveTrivia
                    Dim endRegionDirective As CSS.EndRegionDirectiveTriviaSyntax = DirectCast(structuredTrivia, CSS.EndRegionDirectiveTriviaSyntax)
                    Return Factory.Trivia(Factory.EndRegionDirectiveTrivia(HashToken, EndKeyword, RegionKeyword).
                                        WithAppendedTriviaFromEndOfDirectiveToken(endRegionDirective.EndOfDirectiveToken))
                Case CS.SyntaxKind.PragmaWarningDirectiveTrivia
                    Return Nothing
                Case CS.SyntaxKind.RegionDirectiveTrivia
                    Dim regionDirective As CSS.RegionDirectiveTriviaSyntax = CType(structuredTrivia, CSS.RegionDirectiveTriviaSyntax)
                    Dim endOfDirectiveToken As SyntaxToken = regionDirective.EndOfDirectiveToken
                    Dim nameString As String = $"""{endOfDirectiveToken.LeadingTrivia.ToString.RemoveAll("""")}"""
                    Dim regionDirectiveTriviaNode As VBS.RegionDirectiveTriviaSyntax =
                        Factory.RegionDirectiveTrivia(HashToken,
                                                      RegionKeyword,
                                                      Factory.StringLiteralToken(nameString, nameString)
                                                     ).WithConvertedTrailingTriviaFrom(endOfDirectiveToken)
                    Return Factory.Trivia(regionDirectiveTriviaNode.WithTrailingEOL)
                Case CS.SyntaxKind.SingleLineDocumentationCommentTrivia
                    Dim singleLineDocumentationComment As CSS.DocumentationCommentTriviaSyntax = CType(structuredTrivia, CSS.DocumentationCommentTriviaSyntax)
                    Dim walker As New XMLVisitor()
                    walker.Visit(singleLineDocumentationComment)

                    Dim xmlNodes As New List(Of VBS.XmlNodeSyntax)
                    For Each e As IndexClass(Of CSS.XmlNodeSyntax) In singleLineDocumentationComment.Content.WithIndex
                        Dim node As CSS.XmlNodeSyntax = e.Value
                        If (Not node.IsKind(CS.SyntaxKind.XmlText)) AndAlso node.GetLeadingTrivia.Any AndAlso node.GetLeadingTrivia.First.IsKind(CS.SyntaxKind.DocumentationCommentExteriorTrivia) Then
                            If Not e.IsLast Then
                                Dim nextNode As CSS.XmlNodeSyntax = singleLineDocumentationComment.Content(e.index + 1)
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
                    Dim documentationComment As SyntaxTrivia = Factory.Trivia(documentationCommentTrivia.WithTrailingTrivia(Factory.EndOfLine("")))
                    Return documentationComment
                Case CS.SyntaxKind.PragmaChecksumDirectiveTrivia
                    Dim pragmaChecksumDirective As CSS.PragmaChecksumDirectiveTriviaSyntax = DirectCast(structuredTrivia, CSS.PragmaChecksumDirectiveTriviaSyntax)
                    Dim guid1 As SyntaxToken = Factory.ParseToken(pragmaChecksumDirective.Guid.Text.ToUpperInvariant)
                    Dim bytes As SyntaxToken = Factory.ParseToken(pragmaChecksumDirective.Bytes.Text)
                    Dim externalSource As SyntaxToken = Factory.ParseToken(pragmaChecksumDirective.File.Text)
                    Return Factory.Trivia(
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
                                     )
                Case CS.SyntaxKind.SkippedTokensTrivia
                    Dim builder As New StringBuilder
                    For Each tok As SyntaxToken In CType(structuredTrivia, CSS.SkippedTokensTriviaSyntax).Tokens
                        builder.Append(tok.ToString)
                    Next
                    Return Factory.CommentTrivia($"' TODO: Error SkippedTokensTrivia '{builder}'")
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
