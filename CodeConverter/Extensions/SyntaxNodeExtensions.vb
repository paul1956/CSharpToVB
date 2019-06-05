Option Explicit On
Option Infer Off
Option Strict On

Imports System.Diagnostics.CodeAnalysis
Imports System.Runtime.CompilerServices
Imports System.Text

Imports IVisualBasicCode.CodeConverter.Visual_Basic

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory

Namespace IVisualBasicCode.CodeConverter.Util
    Public Module SyntaxNodeExtensions

        Private Function RemoveLeadingSpacesStar(line As String) As String
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
            For i As Integer = 0 To CommentTriviaBody.Length - 1
                If CommentTriviaBody.Substring(i, 1) = "/" Then
                    CommentTriviaBody = CommentTriviaBody.Remove(i, 1).Insert(i, "'")
                Else
                    Exit For
                End If
            Next
            Return CommentTriviaBody
        End Function

        <Extension()>
        Public Function ConvertDirectiveTrivia(OriginalText As String) As List(Of SyntaxTrivia)
            Dim Text As String = OriginalText.Trim(" "c)
            Dim ResultTrivia As New List(Of SyntaxTrivia)
            Debug.Assert(Text.StartsWith("#"), "All directives must start with #")

            If Text.StartsWith("#if") OrElse Text.StartsWith("#elif") Then
                Dim Expression1 As String = Text.Replace("#if ", "").Replace("#elif ", "").
                        Replace("!", "Not ").
                        Replace("==", "=").
                        Replace("!=", "<>").
                        Replace("&&", "And").
                        Replace("||", "Or").
                        Replace("  ", " ").
                        Replace("false", "False").
                        Replace("true", "True").
                        Replace("//", " ' ").
                        Replace("  ", " ")

                Dim Kind As VB.SyntaxKind = If(Text.StartsWith("#if"), VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.ElseIfDirectiveTrivia)
                Dim IfOrElseIfKeyword As SyntaxToken = If(Text.StartsWith("#if"), IfKeyword, ElseIfKeyword)
                Dim Expr As VB.Syntax.ExpressionSyntax = VBFactory.ParseExpression(Expression1)
                Dim IfDirectiveTrivia As VB.Syntax.IfDirectiveTriviaSyntax = VBFactory.IfDirectiveTrivia(IfOrElseIfKeyword, Expr)
                ResultTrivia.Add(VBFactory.Trivia(IfDirectiveTrivia))
                Return ResultTrivia
            End If
            If Text.StartsWith("#region") OrElse Text.StartsWith("# region") Then
                ResultTrivia.AddRange(ConvertTrivia(CS.SyntaxFactory.ParseLeadingTrivia(Text)))
                Return ResultTrivia
            End If
            If Text.StartsWith("#endregion") Then
                ResultTrivia.Add(VBFactory.Trivia(VBFactory.EndRegionDirectiveTrivia()))
                Text = Text.Replace("#endregion", "")
                If Text.Length > 0 Then
                    Stop
                End If
                Return ResultTrivia
            End If
            If Text.StartsWith("#else") Then
                Dim ElseKeywordWithTrailingTrivia As SyntaxToken = ElseKeyword.WithTrailingTrivia(ConvertTrivia(CS.SyntaxFactory.ParseTrailingTrivia(Text.Replace("#else", ""))))
                ResultTrivia.Add(VBFactory.Trivia(VBFactory.ElseDirectiveTrivia(HashToken, ElseKeywordWithTrailingTrivia)))
                Return ResultTrivia
            End If
            If Text.StartsWith("#endif") Then
                Text = Text.Replace("#endif", "")
                Dim IfKeywordWithTrailingTrivia As SyntaxToken = IfKeyword.WithTrailingTrivia(ConvertTrivia(CS.SyntaxFactory.ParseTrailingTrivia(Text.Replace("#endif", ""))))
                ResultTrivia.Add(VBFactory.Trivia(VBFactory.EndIfDirectiveTrivia(HashToken, EndKeyword, IfKeywordWithTrailingTrivia)))
                Return ResultTrivia
            End If
            If Text.StartsWith("#pragma warning") Then
                ResultTrivia.AddRange(ConvertTrivia(CS.SyntaxFactory.ParseLeadingTrivia(Text)))
                Return ResultTrivia
            Else
                Throw New NotImplementedException($"Directive ""{Text}"" Is unknown")
            End If
        End Function

        <Extension>
        Public Function ConvertTrivia(t As SyntaxTrivia) As SyntaxTrivia

#Region "Non-structured Trivia"

            Select Case t.RawKind
                Case CS.SyntaxKind.WhitespaceTrivia
                    Return VBFactory.WhitespaceTrivia(t.ToString)
                    'Return GetWhitespaceTrivia(t)
                Case CS.SyntaxKind.EndOfLineTrivia
                    Return VB_EOLTrivia
                Case CS.SyntaxKind.SingleLineCommentTrivia
                    If t.ToFullString.EndsWith("*/") Then
                        Return VBFactory.CommentTrivia($"'{ReplaceLeadingSlashes(t.ToFullString.Substring(2, t.ToFullString.Length - 4))}")
                    End If
                    Return VBFactory.CommentTrivia($"'{ReplaceLeadingSlashes(t.ToFullString.Substring(2))}")
                Case CS.SyntaxKind.MultiLineCommentTrivia
                    If t.ToFullString.EndsWith("*/") Then
                        Return VBFactory.CommentTrivia($"'{ReplaceLeadingSlashes(t.ToFullString.Substring(2, t.ToFullString.Length - 4)).Replace(vbLf, "")}")
                    End If
                    Return VBFactory.CommentTrivia($"'{ReplaceLeadingSlashes(t.ToFullString.Substring(2)).Replace(vbLf, "")}")

                Case CS.SyntaxKind.DocumentationCommentExteriorTrivia
                    Return VBFactory.SyntaxTrivia(VB.SyntaxKind.CommentTrivia, "'''")
                Case CS.SyntaxKind.DisabledTextTrivia
                    If IgnoredIfDepth > 0 Then
                        Return VBFactory.DisabledTextTrivia(t.ToString.WithoutNewLines(" "c))
                    End If
                    Return VBFactory.DisabledTextTrivia(t.ToString.Replace(vbLf, vbCrLf))
                Case CS.SyntaxKind.PreprocessingMessageTrivia
                    Return VBFactory.CommentTrivia($" ' {t.ToString}")

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
                    Dim value As VB.Syntax.ExpressionSyntax = VBFactory.TrueLiteralExpression(TrueKeyword)
                    Return VBFactory.Trivia(VBFactory.ConstDirectiveTrivia(Name, value).WithConvertedTriviaFrom(DefineDirective).
                                                    WithAppendedTriviaFromEndOfDirectiveToken(DefineDirective.EndOfDirectiveToken)
                                           )
                Case CS.SyntaxKind.UndefDirectiveTrivia
                    Dim UndefineDirective As CSS.UndefDirectiveTriviaSyntax = DirectCast(StructuredTrivia, CSS.UndefDirectiveTriviaSyntax)
                    Dim Name As SyntaxToken = VBFactory.Identifier(UndefineDirective.Name.ValueText)
                    Dim value As VB.Syntax.ExpressionSyntax = NothingExpression
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
                    Return VBFactory.CommentTrivia($"' TODO: Check VB does not support Error Directive Trivia, Original Directive {t.ToString}")

                Case CS.SyntaxKind.IfDirectiveTrivia
                    If t.Token.Parent?.AncestorsAndSelf.OfType(Of CSS.InitializerExpressionSyntax).Any Then
                        IgnoredIfDepth += 1
                    End If
                    Dim IfDirective As CSS.IfDirectiveTriviaSyntax = DirectCast(StructuredTrivia, CSS.IfDirectiveTriviaSyntax)
                    Dim Expression1 As String = IfDirective.Condition.ToString.
                        Replace("!", "Not ").Replace("==", "=").
                        Replace("!=", "<>").Replace("&&", "And").
                        Replace("||", "Or").Replace("  ", " ").
                        Replace("false", "False").Replace("true", "True")

                    Dim condition As VB.Syntax.ExpressionSyntax = VBFactory.ParseExpression(Expression1)
                    Return VBFactory.Trivia(VBFactory.IfDirectiveTrivia(IfKeyword, condition).
                                                                    With(ConvertTrivia(IfDirective.GetLeadingTrivia),
                                                                         ConvertTrivia(IfDirective.Condition.GetTrailingTrivia)).
                                                                         WithAppendedTriviaFromEndOfDirectiveToken(IfDirective.EndOfDirectiveToken))
                Case CS.SyntaxKind.ElifDirectiveTrivia
                    If t.Token.Parent.AncestorsAndSelf.OfType(Of CSS.InitializerExpressionSyntax).Any Then
                        IgnoredIfDepth += 1
                    End If
                    Dim ELIfDirective As CSS.ElifDirectiveTriviaSyntax = DirectCast(StructuredTrivia, CSS.ElifDirectiveTriviaSyntax)
                    Dim Expression1 As String = ELIfDirective.Condition.ToString.
                            Replace("!", "Not ").Replace("==", "=").
                            Replace("!=", "<>").Replace("&&", "And").
                            Replace("||", "Or").Replace("  ", " ").
                            Replace("false", "False").Replace("true", "True")

                    Dim condition As VB.Syntax.ExpressionSyntax = VBFactory.ParseExpression(Expression1)

                    Dim IfOrElseIfKeyword As SyntaxToken
                    If t.IsKind(CS.SyntaxKind.ElifDirectiveTrivia) Then
                        IfOrElseIfKeyword = ElseIfKeyword
                    Else
                        IfOrElseIfKeyword = IfKeyword
                    End If
                    Return VBFactory.Trivia(VBFactory.ElseIfDirectiveTrivia(IfOrElseIfKeyword, condition).
                                                                    With(ConvertTrivia(ELIfDirective.GetLeadingTrivia),
                                                                         ConvertTrivia(ELIfDirective.Condition.GetTrailingTrivia)).
                                                                         WithAppendedTriviaFromEndOfDirectiveToken(ELIfDirective.EndOfDirectiveToken))
                Case CS.SyntaxKind.LineDirectiveTrivia
                    Return VBFactory.CommentTrivia($"' TODO: Check VB does not support Line Directive Trivia, Original Directive {t.ToString}")
                Case CS.SyntaxKind.ElseDirectiveTrivia
                    Return VBFactory.Trivia(VBFactory.ElseDirectiveTrivia.
                                                            NormalizeWhitespace.
                                                            WithConvertedTrailingTriviaFrom(DirectCast(StructuredTrivia, CSS.ElseDirectiveTriviaSyntax).ElseKeyword).WithTrailingEOL)
                Case CS.SyntaxKind.EndRegionDirectiveTrivia
                    Dim EndRegionDirective As CSS.EndRegionDirectiveTriviaSyntax = DirectCast(StructuredTrivia, CSS.EndRegionDirectiveTriviaSyntax)
                    Return VBFactory.Trivia(VBFactory.EndRegionDirectiveTrivia(HashToken, EndKeyword, RegionKeyword).
                                            WithAppendedTriviaFromEndOfDirectiveToken(EndRegionDirective.EndOfDirectiveToken))
                Case CS.SyntaxKind.MultiLineDocumentationCommentTrivia
                    Return VBFactory.CommentTrivia($"' TODO: Check VB does not support MultiLine Document Comment Trivia, Original Directive {t.ToString}")
                Case CS.SyntaxKind.PragmaWarningDirectiveTrivia
                    Dim PragmaWarningDirectiveTrivia As CSS.PragmaWarningDirectiveTriviaSyntax = DirectCast(StructuredTrivia, CSS.PragmaWarningDirectiveTriviaSyntax)
                    Dim ErrorList As New List(Of VB.Syntax.IdentifierNameSyntax)
                    Dim TrailingTrivia As New List(Of SyntaxTrivia)
                    For Each i As CSS.ExpressionSyntax In PragmaWarningDirectiveTrivia.ErrorCodes
                        Dim ErrorCode As String = i.ToString
                        If ErrorCode.IsInteger Then
                            ErrorCode = $"CS_{ErrorCode}"
                        End If
                        ErrorList.Add(VBFactory.IdentifierName(ErrorCode))
                        TrailingTrivia.AddRange(ConvertTrivia(i.GetTrailingTrivia))
                    Next
                    TrailingTrivia.AddRange(ConvertTrivia(PragmaWarningDirectiveTrivia.EndOfDirectiveToken.LeadingTrivia))
                    TrailingTrivia.AddRange(ConvertTrivia(PragmaWarningDirectiveTrivia.EndOfDirectiveToken.TrailingTrivia))
                    Dim WarningDirectiveTrivia As VB.Syntax.DirectiveTriviaSyntax
                    If PragmaWarningDirectiveTrivia.DisableOrRestoreKeyword.IsKind(CS.SyntaxKind.DisableKeyword) Then
                        WarningDirectiveTrivia = VBFactory.DisableWarningDirectiveTrivia(ErrorList.ToArray)
                    Else
                        WarningDirectiveTrivia = VBFactory.EnableWarningDirectiveTrivia(ErrorList.ToArray)
                    End If
                    Return VBFactory.Trivia(WarningDirectiveTrivia.
                                                            WithPrependedLeadingTrivia(VBFactory.CommentTrivia($"' TODO The value of the warning(s) needs to be manually translated after removing the 'CS' and optional '_'"), VB_EOLTrivia).
                                                            WithTrailingTrivia(TrailingTrivia).WithTrailingEOL)
                Case CS.SyntaxKind.RegionDirectiveTrivia
                    Dim RegionDirective As CSS.RegionDirectiveTriviaSyntax = CType(StructuredTrivia, CSS.RegionDirectiveTriviaSyntax)
                    Dim EndOfDirectiveToken As SyntaxToken = RegionDirective.EndOfDirectiveToken
                    Dim NameString As String = $"""{EndOfDirectiveToken.LeadingTrivia.ToString.Replace("""", "")}"""
                    Dim RegionDirectiveTriviaNode As VB.Syntax.RegionDirectiveTriviaSyntax =
                            VBFactory.RegionDirectiveTrivia(
                                                    HashToken,
                                                    RegionKeyword,
                                                    VBFactory.StringLiteralToken(NameString, NameString)
                                                                            ).WithConvertedTrailingTriviaFrom(EndOfDirectiveToken)
                    Return VBFactory.Trivia(RegionDirectiveTriviaNode.WithTrailingEOL)
                Case CS.SyntaxKind.SingleLineDocumentationCommentTrivia
                    Dim SingleLineDocumentationComment As CSS.DocumentationCommentTriviaSyntax = CType(StructuredTrivia, CSS.DocumentationCommentTriviaSyntax)
                    Dim VBSingleLineDocumentationCommentTrivia As VB.Syntax.DocumentationCommentTriviaSyntax = CreateVBDocumentCommentFromCSharpComment(SingleLineDocumentationComment)
                    Return VBFactory.Trivia(VBSingleLineDocumentationCommentTrivia)
                Case CS.SyntaxKind.PragmaChecksumDirectiveTrivia
                    Dim PragmaChecksumDirective As CSS.PragmaChecksumDirectiveTriviaSyntax = DirectCast(StructuredTrivia, CSS.PragmaChecksumDirectiveTriviaSyntax)
                    Dim Guid1 As SyntaxToken = VBFactory.ParseToken(PragmaChecksumDirective.Guid.Text.ToUpper)
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
                    Dim CSSyntaxKinds As Dictionary(Of CS.SyntaxKind, VB.SyntaxKind) =
                        New Dictionary(Of CS.SyntaxKind, VB.SyntaxKind) From {
                            {CS.SyntaxKind.SkippedTokensTrivia, VB.SyntaxKind.SkippedTokensTrivia},
                            {CS.SyntaxKind.WarningDirectiveTrivia, VB.SyntaxKind.DisableWarningDirectiveTrivia},
                            {CS.SyntaxKind.ReferenceDirectiveTrivia, VB.SyntaxKind.ReferenceDirectiveTrivia},
                            {CS.SyntaxKind.BadDirectiveTrivia, VB.SyntaxKind.BadDirectiveTrivia},
                            {CS.SyntaxKind.ConflictMarkerTrivia, VB.SyntaxKind.ConflictMarkerTrivia},
                            {CS.SyntaxKind.LoadDirectiveTrivia, VB.SyntaxKind.ExternalSourceDirectiveTrivia}
                            }
                    Dim ConvertedKind As KeyValuePair(Of CS.SyntaxKind, VB.SyntaxKind)? = CSSyntaxKinds.FirstOrNullable(Function(kvp As KeyValuePair(Of CS.SyntaxKind, VB.SyntaxKind)) t.IsKind(kvp.Key))
                    Return If(ConvertedKind.HasValue, VBFactory.CommentTrivia($"' TODO: Error Skipped {ConvertedKind.Value.Key}"), Nothing)
                Case CS.SyntaxKind.BadDirectiveTrivia
                    Return VBFactory.CommentTrivia($"' TODO: Skipped BadDirectiveTrivia")
                Case CS.SyntaxKind.NullableDirectiveTrivia
                    Return VBFactory.CommentTrivia($"' TODO: Skipped Nullable Directive Trivia")
                Case Else
                    Debug.WriteLine(CType(t.RawKind, VB.SyntaxKind).ToString)
                    Stop
            End Select

#End Region

            Throw New NotImplementedException($"t.Kind({CType(t.RawKind, VB.SyntaxKind)}) Is unknown")
        End Function

        <Extension>
        Function WithAppendedTriviaFromEndOfDirectiveToken(Of T As SyntaxNode)(node As T, Token As SyntaxToken) As T
            Dim NewTrailingTrivia As New List(Of SyntaxTrivia)
            If Token.HasLeadingTrivia Then
                NewTrailingTrivia.AddRange(ConvertTrivia(Token.LeadingTrivia))
            End If
            If Token.HasTrailingTrivia Then
                NewTrailingTrivia.AddRange(ConvertTrivia(Token.TrailingTrivia))
            End If

            Return node.WithAppendedTrailingTrivia(NewTrailingTrivia).WithTrailingEOL
        End Function

        <Extension>
        Public Function ConvertTrivia(TriviaToConvert As IReadOnlyCollection(Of SyntaxTrivia)) As IEnumerable(Of SyntaxTrivia)
            OriginalRequest.MyDoEvents().Invoke
            Dim TriviaList As New List(Of SyntaxTrivia)
            If TriviaToConvert Is Nothing Then
                Return TriviaList
            End If
            Try
                Dim TriviaCount As Integer = TriviaToConvert.Count - 1
                For i As Integer = 0 To TriviaCount
                    Dim t As SyntaxTrivia = TriviaToConvert(i)
                    Select Case t.RawKind
                        Case CS.SyntaxKind.MultiLineCommentTrivia
                            Dim Lines() As String = t.ToFullString.Substring(2).Split(CType(vbLf, Char))
                            For Each line As String In Lines
                                If line.EndsWith("*/") Then
                                    TriviaList.Add(VBFactory.CommentTrivia($"' {RemoveLeadingSpacesStar(line.Substring(0, line.Length - 2))}"))
                                    If t.ToFullString.EndsWith(vbLf) Then
                                        TriviaList.Add(VB_EOLTrivia)
                                    End If
                                Else
                                    TriviaList.Add(VBFactory.CommentTrivia($"' {RemoveLeadingSpacesStar(line)}"))
                                    TriviaList.Add(VB_EOLTrivia)
                                End If
                                If Lines.Count = 1 AndAlso (i = TriviaCount OrElse Not TriviaToConvert(i + 1).IsEndOfLine) Then
                                    TriviaList.Add(VB_EOLTrivia)
                                End If
                            Next
                        Case CS.SyntaxKind.MultiLineDocumentationCommentTrivia
                            Dim sld As CSS.StructuredTriviaSyntax = DirectCast(t.GetStructure, CSS.StructuredTriviaSyntax)
                            For Each t1 As SyntaxNode In sld.ChildNodes
                                Dim Lines() As String = t1.ToFullString.Split(CType(vbLf, Char))
                                For Each line As String In Lines
                                    If line.StartsWith("/*") Then
                                        TriviaList.Add(VBFactory.CommentTrivia($"' {RemoveLeadingSpacesStar(line.Substring(1, line.Length - 1))}"))
                                        TriviaList.Add(VB_EOLTrivia)
                                    Else
                                        TriviaList.Add(VBFactory.CommentTrivia($"' {RemoveLeadingSpacesStar(line)}"))
                                        TriviaList.Add(VB_EOLTrivia)
                                    End If
                                Next
                            Next
                        Case Else
                            Dim ConvertedTrivia As SyntaxTrivia = ConvertTrivia(t)
                            If ConvertedTrivia = Nothing Then
                                Continue For
                            End If
                            TriviaList.Add(ConvertedTrivia)
                            If t.IsKind(CS.SyntaxKind.SingleLineCommentTrivia) Then
                                If i < TriviaCount AndAlso TriviaToConvert(i + 1).RawKind <> CS.SyntaxKind.EndOfLineTrivia Then
                                    TriviaList.Add(VB_EOLTrivia)
                                End If
                            End If
                    End Select
                Next
                OriginalRequest.MyDoEvents().Invoke
            Catch ex As Exception
                Stop
                Throw
            End Try
            Return TriviaList
        End Function

        <Extension()>
        Public Function GetAncestor(Of TNode As SyntaxNode)(node As SyntaxNode) As TNode
            If node Is Nothing Then
                Return Nothing
            End If

            Return node.GetAncestors(Of TNode)().FirstOrDefault()
        End Function

        <Extension()>
        Public Iterator Function GetAncestors(Of TNode As SyntaxNode)(node As SyntaxNode) As IEnumerable(Of TNode)
            Dim current As SyntaxNode = node.Parent
            While current IsNot Nothing
                If TypeOf current Is TNode Then
                    Yield DirectCast(current, TNode)
                End If

                current = If(TypeOf current Is IStructuredTriviaSyntax, (DirectCast(current, IStructuredTriviaSyntax)).ParentTrivia.Token.Parent, current.Parent)
            End While
        End Function

        <Extension()>
        Public Iterator Function GetAncestorsOrThis(Of TNode As SyntaxNode)(node As SyntaxNode) As IEnumerable(Of TNode)
            Dim current As SyntaxNode = node
            While current IsNot Nothing
                If TypeOf current Is TNode Then
                    Yield DirectCast(current, TNode)
                End If

                current = If(TypeOf current Is IStructuredTriviaSyntax, (DirectCast(current, IStructuredTriviaSyntax)).ParentTrivia.Token.Parent, current.Parent)
            End While
        End Function

        <Extension()>
        Public Function GetBraces(node As SyntaxNode) As ValueTuple(Of SyntaxToken, SyntaxToken)
            Dim namespaceNode As CSS.NamespaceDeclarationSyntax = TryCast(node, CSS.NamespaceDeclarationSyntax)
            If namespaceNode IsNot Nothing Then
                Return ValueTuple.Create(namespaceNode.OpenBraceToken, namespaceNode.CloseBraceToken)
            End If

            Dim baseTypeNode As CSS.BaseTypeDeclarationSyntax = TryCast(node, CSS.BaseTypeDeclarationSyntax)
            If baseTypeNode IsNot Nothing Then
                Return ValueTuple.Create(baseTypeNode.OpenBraceToken, baseTypeNode.CloseBraceToken)
            End If

            Dim accessorListNode As CSS.AccessorListSyntax = TryCast(node, CSS.AccessorListSyntax)
            If accessorListNode IsNot Nothing Then
                Return ValueTuple.Create(accessorListNode.OpenBraceToken, accessorListNode.CloseBraceToken)
            End If

            Dim blockNode As CSS.BlockSyntax = TryCast(node, CSS.BlockSyntax)
            If blockNode IsNot Nothing Then
                Return ValueTuple.Create(blockNode.OpenBraceToken, blockNode.CloseBraceToken)
            End If

            Dim switchStatementNode As CSS.SwitchStatementSyntax = TryCast(node, CSS.SwitchStatementSyntax)
            If switchStatementNode IsNot Nothing Then
                Return ValueTuple.Create(switchStatementNode.OpenBraceToken, switchStatementNode.CloseBraceToken)
            End If

            Dim anonymousObjectCreationExpression As CSS.AnonymousObjectCreationExpressionSyntax = TryCast(node, CSS.AnonymousObjectCreationExpressionSyntax)
            If anonymousObjectCreationExpression IsNot Nothing Then
                Return ValueTuple.Create(anonymousObjectCreationExpression.OpenBraceToken, anonymousObjectCreationExpression.CloseBraceToken)
            End If

            Dim initializeExpressionNode As CSS.InitializerExpressionSyntax = TryCast(node, CSS.InitializerExpressionSyntax)
            If initializeExpressionNode IsNot Nothing Then
                Return ValueTuple.Create(initializeExpressionNode.OpenBraceToken, initializeExpressionNode.CloseBraceToken)
            End If

            Return New ValueTuple(Of SyntaxToken, SyntaxToken)()
        End Function

        Public Sub GetPartsOfParenthesizedExpression(node As SyntaxNode, ByRef openParen As SyntaxToken, ByRef expression As SyntaxNode, ByRef closeParen As SyntaxToken)

            Dim parenthesizedExpression As VB.Syntax.ParenthesizedExpressionSyntax = DirectCast(node, VB.Syntax.ParenthesizedExpressionSyntax)
            openParen = parenthesizedExpression.OpenParenToken
            expression = parenthesizedExpression.Expression
            closeParen = parenthesizedExpression.CloseParenToken
        End Sub

        <Extension()>
        Public Function IsKind(node As SyntaxNode, ParamArray kind1() As CS.SyntaxKind) As Boolean
            If node Is Nothing Then
                Return False
            End If

            For Each k As CS.SyntaxKind In kind1
                If node.IsKind(k) Then
                    Return True
                End If
            Next
            Return False
        End Function

        <Extension()>
        Public Function IsKind(node As SyntaxNode, ParamArray kind1() As VB.SyntaxKind) As Boolean
            If node Is Nothing Then
                Return False
            End If

            For Each k As VB.SyntaxKind In kind1
                If CType(node.RawKind(), VB.SyntaxKind) = k Then
                    Return True
                End If
            Next
            Return False
        End Function

        <Extension>
        Public Function ParentHasSameTrailingTrivia(otherNode As SyntaxNode) As Boolean
            If otherNode.Parent Is Nothing Then
                Return False
            End If
            Return otherNode.Parent.GetLastToken() = otherNode.GetLastToken()
        End Function

        <Extension()>
        Public Function RemoveLeadingEOL(Of T As SyntaxNode)(node As T) As T
            Dim LeadingTrivia As List(Of SyntaxTrivia) = node.GetLeadingTrivia.ToList
            Select Case LeadingTrivia.Count
                Case 0
                    Return node
                Case 1
                    If LeadingTrivia.First.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                        Return node.WithoutLeadingTrivia
                    End If
                Case 2
                    Select Case LeadingTrivia.First.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            If LeadingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                Return node.WithoutLeadingTrivia
                            End If
                            Return node
                        Case VB.SyntaxKind.EndOfLineTrivia
                            If LeadingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                Return node.WithoutLeadingTrivia
                            End If
                            Return node.WithLeadingTrivia(LeadingTrivia.Last)
                        Case Else
                    End Select
                Case Else
            End Select
            Dim NewLeadingTrivia As New List(Of SyntaxTrivia)
            Dim FirstTrivia As Boolean = True
            For i As Integer = 0 To node.GetLeadingTrivia.Count - 1
                Dim Trivia As SyntaxTrivia = node.GetLeadingTrivia(i)
                Dim NextTrivia As SyntaxTrivia = If(i < node.GetLeadingTrivia.Count - 2, node.GetLeadingTrivia(i + 1), Nothing)
                If Trivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) AndAlso (FirstTrivia OrElse NextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia)) Then
                    Continue For
                End If
                If Trivia.IsKind(VB.SyntaxKind.WhitespaceTrivia) AndAlso NextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                    Continue For
                End If

                FirstTrivia = False
                NewLeadingTrivia.Add(Trivia)
            Next

            Return node.WithLeadingTrivia(NewLeadingTrivia)
        End Function

        <Extension>
        Public Function Unparenthesize(node As SyntaxNode) As SyntaxNode
            If TypeOf node IsNot VB.Syntax.ParenthesizedExpressionSyntax Then
                Return node
            End If
            Dim parenthesizedExpression As VB.Syntax.ParenthesizedExpressionSyntax = DirectCast(node, VB.Syntax.ParenthesizedExpressionSyntax)
            GetPartsOfParenthesizedExpression(node, parenthesizedExpression.OpenParenToken, parenthesizedExpression.Expression, parenthesizedExpression.CloseParenToken)

            Dim LeadingTrivia As New List(Of SyntaxTrivia)
            LeadingTrivia.AddRange(parenthesizedExpression.OpenParenToken.LeadingTrivia.Concat(parenthesizedExpression.OpenParenToken.TrailingTrivia).Where(Function(t As SyntaxTrivia) Not t.IsElastic).Concat(parenthesizedExpression.Expression.GetLeadingTrivia()))

            Dim TrailingTrivia As New List(Of SyntaxTrivia)
            TrailingTrivia.AddRange(parenthesizedExpression.Expression.GetTrailingTrivia().Concat(parenthesizedExpression.CloseParenToken.LeadingTrivia).Where(Function(t As SyntaxTrivia) Not t.IsElastic).Concat(parenthesizedExpression.CloseParenToken.TrailingTrivia))

            Dim resultNode As SyntaxNode = parenthesizedExpression.Expression.WithLeadingTrivia(LeadingTrivia).WithTrailingTrivia(TrailingTrivia)
            Dim VBSyntaxFacts As VB.SyntaxFacts = (New VB.SyntaxFacts)

            ' If there's no trivia between the original node and the tokens around it, then add
            ' elastic markers so the formatting engine will spaces if necessary to keep things
            ' parse-able.
            If resultNode.GetLeadingTrivia().Count = 0 Then
                Dim previousToken As SyntaxToken = node.GetFirstToken().GetPreviousToken()

                If previousToken.TrailingTrivia.Count = 0 AndAlso IsWordOrNumber(previousToken) AndAlso IsWordOrNumber(resultNode.GetFirstToken()) Then
                    resultNode = resultNode.WithPrependedLeadingTrivia(ElasticMarker)
                End If
            End If

            If resultNode.GetTrailingTrivia().Count = 0 Then
                Dim nextToken As SyntaxToken = node.GetLastToken().GetNextToken()
                If nextToken.LeadingTrivia.Count = 0 AndAlso IsWordOrNumber(nextToken) AndAlso IsWordOrNumber(resultNode.GetLastToken()) Then
                    resultNode = resultNode.WithAppendedTrailingTrivia(ElasticMarker)
                End If
            End If

            Return resultNode
        End Function

        <Extension()>
        Public Function [With](Of T As SyntaxNode)(node As T, leadingTrivia As IEnumerable(Of SyntaxTrivia), trailingTrivia As IEnumerable(Of SyntaxTrivia)) As T
            Return node.WithLeadingTrivia(leadingTrivia).WithTrailingTrivia(trailingTrivia)
        End Function

        <Extension()>
        Public Function WithAppendedTrailingTrivia(Of T As SyntaxNode)(node As T, ParamArray trivia As SyntaxTrivia()) As T
            If trivia.Length = 0 Then
                Return node
            End If

            Return node.WithAppendedTrailingTrivia(DirectCast(trivia, IEnumerable(Of SyntaxTrivia)))
        End Function

        <Extension()>
        Public Function WithAppendedTrailingTrivia(Of T As SyntaxNode)(node As T, trivia As SyntaxTriviaList) As T
            If trivia.Count = 0 Then
                Return node
            End If
            If node Is Nothing Then
                Return Nothing
            End If

            Return node.WithTrailingTrivia(node.GetTrailingTrivia().Concat(trivia))
        End Function

        <Extension()>
        Public Function WithAppendedTrailingTrivia(Of T As SyntaxNode)(node As T, trivia As IEnumerable(Of SyntaxTrivia)) As T
            If node Is Nothing Then
                Return Nothing
            End If
            Return node.WithAppendedTrailingTrivia(trivia.ToSyntaxTriviaList())
        End Function

        <Extension()>
        Public Function WithMergedTrailingTrivia(Of T As SyntaxNode)(node As T, TriviaListToMerge As IEnumerable(Of SyntaxTrivia)) As T
            If node Is Nothing Then
                Return Nothing
            End If
            If TriviaListToMerge.Count = 0 Then
                Return node
            End If
            Dim NodeTrailingTrivia As List(Of SyntaxTrivia) = node.GetTrailingTrivia.ToList
            If NodeTrailingTrivia.Count = 0 Then
                Return node.WithTrailingTrivia(TriviaListToMerge)
            End If
            ' Both nodes have trivia
            Dim FoundWhiteSpace As Boolean = False
            Dim NewTrailingTrivia As New List(Of SyntaxTrivia)
            For i As Integer = 0 To NodeTrailingTrivia.Count - 1
                Dim Trivia As SyntaxTrivia = NodeTrailingTrivia(i)
                Dim NextTrivia As SyntaxTrivia = If(i < NodeTrailingTrivia.Count - 2, NodeTrailingTrivia(i + 1), Nothing)
                Select Case Trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        If Not FoundWhiteSpace AndAlso Not NextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            NewTrailingTrivia.Add(Trivia)
                            FoundWhiteSpace = True
                        End If
                    Case VB.SyntaxKind.EndOfLineTrivia
                        NewTrailingTrivia.Add(Trivia)
                    Case VB.SyntaxKind.CommentTrivia
                        NewTrailingTrivia.Add(Trivia)
                        If Not NextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            NewTrailingTrivia.Add(VB_EOLTrivia)
                        End If
                    Case VB.SyntaxKind.EnableWarningDirectiveTrivia
                        NewTrailingTrivia.Add(VB_EOLTrivia)
                        NewTrailingTrivia.Add(Trivia)
                    Case Else
                        Stop
                End Select
            Next
            Dim FoundEOL As Boolean = False
            For i As Integer = 0 To TriviaListToMerge.Count - 1
                Dim Trivia As SyntaxTrivia = TriviaListToMerge(i)
                Dim NextTrivia As SyntaxTrivia = If(i < TriviaListToMerge.Count - 2, TriviaListToMerge(i + 1), Nothing)
                Select Case Trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        If Not FoundWhiteSpace AndAlso Not NextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            NewTrailingTrivia.Add(Trivia)
                            FoundWhiteSpace = True
                        End If
                    Case VB.SyntaxKind.EndOfLineTrivia
                        NewTrailingTrivia.Add(Trivia)
                        FoundEOL = True
                    Case VB.SyntaxKind.CommentTrivia
                        NewTrailingTrivia.Add(Trivia)
                        If Not NextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            NewTrailingTrivia.Add(VB_EOLTrivia)
                            FoundEOL = True
                        End If
                    Case VB.SyntaxKind.EndRegionDirectiveTrivia

                        If FoundEOL Then
                            NewTrailingTrivia.Add(VB_EOLTrivia)
                        End If
                        NewTrailingTrivia.Add(Trivia)
                    Case Else
                        Stop
                End Select
            Next
            Return node.WithTrailingTrivia(NewTrailingTrivia)
        End Function

        ''' <summary>
        ''' This function is used where a Token is Followed by a Node followed by a Token
        ''' </summary>
        ''' <param name="Node"></param>
        ''' <returns>New Node with valid Trivia</returns>
        <Extension>
        Public Function WithModifiedNodeTrivia(Node As VB.VisualBasicSyntaxNode, SeparatorFollows As Boolean) As VB.VisualBasicSyntaxNode
            Dim AfterWhiteSpace As Boolean = False
            Dim FinalLeadingTriviaList As New List(Of SyntaxTrivia)
            Dim InitialTriviaList As List(Of SyntaxTrivia) = Node.GetLeadingTrivia.ToList
            Dim InitialTriviaListUBound As Integer = InitialTriviaList.Count - 1
            For i As Integer = 0 To InitialTriviaListUBound
                Dim Trivia As SyntaxTrivia = InitialTriviaList(i)
                Dim NextTrivia As SyntaxTrivia = If(i < InitialTriviaList.Count - 1, InitialTriviaList(i + 1), Nothing)
                Select Case Trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        AfterWhiteSpace = True
                        FinalLeadingTriviaList.Add(Trivia)
                    Case VB.SyntaxKind.EndOfLineTrivia
                        FinalLeadingTriviaList.Add(Trivia)
                        AfterWhiteSpace = False
                        If FinalLeadingTriviaList.Count = 0 Then
                            FinalLeadingTriviaList.Add(SpaceTrivia)
                            FinalLeadingTriviaList.Add(LineContinuation)
                        End If
                    Case VB.SyntaxKind.CommentTrivia
                        If Not AfterWhiteSpace Then
                            FinalLeadingTriviaList.Add(SpaceTrivia)
                        End If
                        FinalLeadingTriviaList.Add(LineContinuation)
                        FinalLeadingTriviaList.Add(Trivia)
                        If Not NextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            FinalLeadingTriviaList.Add(VB_EOLTrivia)
                        End If
                    Case VB.SyntaxKind.DisableWarningDirectiveTrivia, VB.SyntaxKind.EnableWarningDirectiveTrivia,
                         VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.DisabledTextTrivia,
                       VB.SyntaxKind.ElseDirectiveTrivia, VB.SyntaxKind.EndIfDirectiveTrivia
                        FinalLeadingTriviaList.AddRange(DirectiveNotAllowedHere(Trivia))
                        If NextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) OrElse NextTrivia.IsNone Then
                            Continue For
                        End If
                        FinalLeadingTriviaList.Add(VB_EOLTrivia)
                    Case VB.SyntaxKind.LineContinuationTrivia
                        ' ignore handled elsewhere
                    Case Else
                        Stop
                End Select
            Next
            InitialTriviaList.Clear()
            InitialTriviaList.AddRange(Node.GetTrailingTrivia)
            InitialTriviaListUBound = InitialTriviaList.Count - 1
            AfterWhiteSpace = False
            Dim AfterLineContinuation As Boolean = False
            Dim AfterLinefeed As Boolean = False
            Dim AfterComment As Boolean = False
            Dim FinalTrailingTriviaList As New List(Of SyntaxTrivia)
            For i As Integer = 0 To InitialTriviaListUBound
                Dim Trivia As SyntaxTrivia = InitialTriviaList(i)
                Dim NextTrivia As SyntaxTrivia = If(i < InitialTriviaListUBound, InitialTriviaList(i + 1), VBFactory.ElasticMarker)
                Select Case Trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        If NextTrivia.IsKind(VB.SyntaxKind.CommentTrivia) OrElse
                                NextTrivia.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                            FinalTrailingTriviaList.Add(Trivia)
                            AfterLinefeed = False
                            AfterComment = False
                            AfterWhiteSpace = True
                        End If
                    Case VB.SyntaxKind.EndOfLineTrivia
                        ' There is a Token after this node
                        If Not AfterLinefeed Then
                            If AfterComment Then
                                FinalTrailingTriviaList.Add(Trivia)
                            Else
                                If SeparatorFollows Then
                                    FinalTrailingTriviaList.Add(SpaceTrivia)
                                    FinalTrailingTriviaList.Add(LineContinuation)
                                    FinalTrailingTriviaList.Add(Trivia)
                                End If
                            End If
                            AfterComment = False
                            AfterLinefeed = True
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
                        If Not NextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            FinalTrailingTriviaList.Add(VB_EOLTrivia)
                            AfterLineContinuation = False
                            AfterLinefeed = True
                        End If
                        AfterComment = True
                        AfterWhiteSpace = False
                    Case VB.SyntaxKind.LineContinuationTrivia
                        If FinalTrailingTriviaList.Last.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                            Continue For
                        End If
                        AfterWhiteSpace = False
                        AfterLineContinuation = True
                        FinalTrailingTriviaList.Add(LineContinuation)
                    Case Else
                        Stop
                End Select
            Next
            Return Node.With(FinalLeadingTriviaList, FinalTrailingTriviaList)
        End Function

        <Extension()>
        Public Function WithPrependedLeadingTrivia(Of T As SyntaxNode)(node As T, ParamArray trivia As SyntaxTrivia()) As T
            If trivia.Length = 0 Then
                Return node
            End If
            Dim TriviaList As List(Of SyntaxTrivia) = trivia.ToList
            If TriviaList.Last.IsKind(VB.SyntaxKind.CommentTrivia) Then
                TriviaList.Add(VB_EOLTrivia)
            End If
            Return node.WithPrependedLeadingTrivia(TriviaList)
        End Function

        <Extension()>
        Public Function WithPrependedLeadingTrivia(Of T As SyntaxNode)(node As T, trivia As SyntaxTriviaList) As T
            If trivia.Count = 0 Then
                Return node
            End If
            If trivia.Last.IsKind(VB.SyntaxKind.CommentTrivia) Then
                trivia = trivia.Add(VB_EOLTrivia)
            End If
            Return node.WithLeadingTrivia(trivia.Concat(node.GetLeadingTrivia()))
        End Function

        <Extension()>
        Public Function WithPrependedLeadingTrivia(Of T As SyntaxNode)(node As T, trivia As IEnumerable(Of SyntaxTrivia)) As T
            Return node.WithPrependedLeadingTrivia(trivia.ToSyntaxTriviaList())
        End Function


        <Extension>
        Public Function WithRestructuredingEOLTrivia(Of T As SyntaxNode)(node As T) As T
            If Not node.HasTrailingTrivia Then
                Return node
            End If

            Dim NodeTrailingTrivia As SyntaxTriviaList = node.GetTrailingTrivia
            If NodeTrailingTrivia.ContainsEOLTrivia Then
                Dim NewTriviaList As New List(Of SyntaxTrivia)
                For Each Trivia As SyntaxTrivia In NodeTrailingTrivia
                    If Trivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                        Continue For
                    End If
                    NewTriviaList.Add(Trivia)
                Next
                Return node.WithTrailingTrivia(NewTriviaList)
            Else
                Return node
            End If
        End Function

        <Extension()>
        Public Function WithTrailingEOL(Of T As SyntaxNode)(node As T) As T
            Dim TrailingTrivia As List(Of SyntaxTrivia) = node.GetTrailingTrivia.ToList
            Dim Count As Integer = TrailingTrivia.Count
            If Count = 0 Then
                Return node.WithTrailingTrivia(VB_EOLTrivia)
            End If
            Select Case Count
                Case 1
                    Select Case TrailingTrivia.Last.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia, VB.SyntaxKind.EndOfLineTrivia
                            Return node.WithTrailingTrivia(VB_EOLTrivia)
                        Case Else
                            TrailingTrivia.Add(VB_EOLTrivia)
                            Return node.WithTrailingTrivia(TrailingTrivia)
                    End Select
                Case 2
                    Select Case TrailingTrivia.First.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            Select Case TrailingTrivia.Last.RawKind
                                Case VB.SyntaxKind.WhitespaceTrivia, VB.SyntaxKind.EndOfLineTrivia
                                    ' Replace Whitespaces, Whitespaces and Whitespaces, EOL with just EOL
                                    TrailingTrivia = New List(Of SyntaxTrivia)
                                    ' EOL added below
                                Case VB.SyntaxKind.CommentTrivia
                                    ' nothing to do EOL added below
                                Case Else
                                    Stop
                            End Select
                        Case VB.SyntaxKind.EndOfLineTrivia
                            If TrailingTrivia.Last.IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                                Return node
                            ElseIf TrailingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                Return node.WithTrailingTrivia(VB_EOLTrivia)
                            End If
                            Stop
                        Case VB.SyntaxKind.CommentTrivia
                            If TrailingTrivia.Last.IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                                TrailingTrivia.RemoveAt(1)
                                TrailingTrivia.Insert(0, SpaceTrivia)
                                ' EOL added below
                                Stop
                            End If
                        Case Else
                            Stop
                    End Select
                    TrailingTrivia.Add(VB_EOLTrivia)
                    Return node.WithTrailingTrivia(TrailingTrivia)
                Case Else
                    Count -= 1 ' Last index
                    Select Case TrailingTrivia.Last.RawKind
                        Case VB.SyntaxKind.EndOfLineTrivia
                            If TrailingTrivia(Count - 1).IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                TrailingTrivia.RemoveAt(Count)
                                Return node.WithTrailingTrivia(TrailingTrivia).WithTrailingEOL
                            End If
                            Return node
                        Case VB.SyntaxKind.WhitespaceTrivia
                            If TrailingTrivia(Count - 1).IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                                TrailingTrivia.RemoveAt(Count - 1)
                                Return node.WithTrailingTrivia(TrailingTrivia).WithTrailingEOL
                            ElseIf TrailingTrivia(Count - 1).IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                Return node
                            ElseIf TrailingTrivia(Count - 1).IsCommentOrDirectiveTrivia Then
                                TrailingTrivia.Insert(Count, VB_EOLTrivia)
                                Return node.WithTrailingTrivia(TrailingTrivia)
                            End If
                            Return node.WithTrailingTrivia(TrailingTrivia)
                        Case Else
                            Stop
                    End Select
                    Stop
            End Select
            Return node
        End Function

#Region "WithConvertedTriviaFrom"

        <ExcludeFromCodeCoverage>
        <Extension>
        Public Function WithConvertedTriviaFrom(Of TSyntax As CSS.XmlNodeSyntax)(node As TSyntax, otherNode As CSS.XmlNodeSyntax) As TSyntax
            If otherNode Is Nothing Then
                Return node
            End If
            If otherNode.HasLeadingTrivia Then
                node = node.WithLeadingTrivia(ConvertTrivia(otherNode.GetLeadingTrivia()))
            End If
            If Not otherNode.HasTrailingTrivia OrElse ParentHasSameTrailingTrivia(otherNode) Then
                Return node
            End If
            Return node.WithTrailingTrivia(ConvertTrivia(otherNode.GetTrailingTrivia()))
        End Function

        <Extension>
        Public Function WithConvertedTriviaFrom(Of T As SyntaxNode)(node As T, otherNode As SyntaxNode) As T
            If otherNode Is Nothing Then
                Return node
            End If
            If otherNode.HasLeadingTrivia Then
                node = node.WithLeadingTrivia(ConvertTrivia(otherNode.GetLeadingTrivia()))
            End If
            If Not otherNode.HasTrailingTrivia OrElse ParentHasSameTrailingTrivia(otherNode) Then
                Return node
            End If
            Return node.WithTrailingTrivia(ConvertTrivia(otherNode.GetTrailingTrivia()))
        End Function

        <Extension>
        Public Function WithConvertedTriviaFrom(Of T As SyntaxNode)(node As T, otherToken As SyntaxToken) As T
            If otherToken.HasLeadingTrivia Then
                node = node.WithLeadingTrivia(ConvertTrivia(otherToken.LeadingTrivia))
            End If
            If Not otherToken.HasTrailingTrivia Then
                Return node
            End If
            Return node.WithTrailingTrivia(ConvertTrivia(otherToken.TrailingTrivia()))
        End Function

        <Extension>
        Public Function WithConvertedTriviaFrom(Token As SyntaxToken, otherNode As SyntaxNode) As SyntaxToken
            If otherNode.HasLeadingTrivia Then
                Token = Token.WithLeadingTrivia(ConvertTrivia(otherNode.GetLeadingTrivia))
            End If
            If Not otherNode.HasTrailingTrivia OrElse ParentHasSameTrailingTrivia(otherNode) Then
                Return Token
            End If
            Return Token.WithTrailingTrivia(ConvertTrivia(otherNode.GetTrailingTrivia()))
        End Function

        <Extension>
        Public Function WithConvertedTriviaFrom(Token As SyntaxToken, otherToken As SyntaxToken) As SyntaxToken
            Try

                If otherToken.HasLeadingTrivia Then
                    Token = Token.WithLeadingTrivia(ConvertTrivia(otherToken.LeadingTrivia).ToList())
                End If
                Return Token.WithTrailingTrivia(ConvertTrivia(otherToken.TrailingTrivia))
            Catch ex As Exception
                Stop
            End Try
        End Function

        ''' <summary>
        ''' Allows for swapping trivia usually for cast or declarations
        ''' </summary>
        ''' <typeparam name="T"></typeparam>
        ''' <param name="node"></param>
        ''' <param name="LeadingNode"></param>
        ''' <param name="TrailingNode"></param>
        ''' <returns></returns>
        <ExcludeFromCodeCoverage>
        <Extension>
        Public Function WithConvertedTriviaFrom(Of T As SyntaxNode)(node As T, LeadingNode As SyntaxNode, TrailingNode As SyntaxNode) As T
            Return node.WithConvertedLeadingTriviaFrom(LeadingNode).WithConvertedTrailingTriviaFrom(TrailingNode)
        End Function

#End Region

#Region "WithConvertedLeadingTriviaFrom"

        <Extension>
        Public Function WithConvertedLeadingTriviaFrom(Of T As SyntaxNode)(node As T, otherNode As SyntaxNode) As T
            If otherNode Is Nothing OrElse Not otherNode.HasLeadingTrivia Then
                Return node
            End If
            Return node.WithLeadingTrivia(ConvertTrivia(otherNode.GetLeadingTrivia()))
        End Function

        <Extension>
        Public Function WithConvertedLeadingTriviaFrom(Of T As SyntaxNode)(node As T, otherToken As SyntaxToken) As T
            If Not otherToken.HasLeadingTrivia Then
                Return node
            End If
            Return node.WithLeadingTrivia(ConvertTrivia(otherToken.LeadingTrivia()))
        End Function

        <Extension>
        Public Function WithConvertedLeadingTriviaFrom(node As SyntaxToken, otherToken As SyntaxToken) As SyntaxToken
            If Not otherToken.HasLeadingTrivia Then
                Return node
            End If
            Return node.WithLeadingTrivia(ConvertTrivia(otherToken.LeadingTrivia()))
        End Function

#End Region

#Region "WithConvertedTrailingTriviaFrom"

        <Extension>
        Public Function WithConvertedTrailingTriviaFrom(Of T As SyntaxNode)(node As T, otherNode As SyntaxNode) As T
            If otherNode Is Nothing OrElse Not otherNode.HasTrailingTrivia Then
                Return node
            End If
            If ParentHasSameTrailingTrivia(otherNode) Then
                Return node
            End If
            Return node.WithTrailingTrivia(ConvertTrivia(otherNode.GetTrailingTrivia()))
        End Function

        <Extension>
        Public Function WithConvertedTrailingTriviaFrom(Of T As SyntaxNode)(node As T, otherToken As SyntaxToken) As T
            If Not otherToken.HasTrailingTrivia Then
                Return node
            End If
            Return node.WithTrailingTrivia(ConvertTrivia(otherToken.TrailingTrivia()))
        End Function

        <Extension>
        Public Function WithConvertedTrailingTriviaFrom(Token As SyntaxToken, otherToken As SyntaxToken) As SyntaxToken
            Return Token.WithTrailingTrivia(ConvertTrivia(otherToken.TrailingTrivia()))
        End Function

#End Region

    End Module
End Namespace