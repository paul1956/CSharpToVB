' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Diagnostics.CodeAnalysis
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory

Namespace CSharpToVBConverter

    Friend Class SyntaxNormalizer
        Inherits VisualBasicSyntaxRewriter

        Private ReadOnly _consideredSpan As TextSpan
        Private ReadOnly _eolTrivia As SyntaxTrivia
        Private ReadOnly _indentWhitespace As String
        Private ReadOnly _lastStatementsInBlocks As HashSet(Of SyntaxNode) = New HashSet(Of SyntaxNode)()
        Private ReadOnly _lineBreaksAfterToken As Dictionary(Of SyntaxToken, Integer) = New Dictionary(Of SyntaxToken, Integer)()
        Private ReadOnly _useDefaultCasing As Boolean
        Private ReadOnly _useElasticTrivia As Boolean
        Private ReadOnly _usePreserveCRLF As Boolean
        Private _afterIndentation As Boolean
        Private _afterLineBreak As Boolean
        Private _eolLeadingTriviaCount As Integer
        Private _eolTraiingTriviaCount As Integer
        Private _indentationDepth As Integer
        Private _indentations As List(Of SyntaxTrivia)
        Private _isInStructuredTrivia As Boolean

        Private _previousToken As SyntaxToken

        ''' <summary>
        ''' Creates a syntax trivia normalizing visitor
        ''' </summary>
        ''' <param name="indentWhitespace">The whitespace to indent with</param>
        ''' <param name="eolWhitespace">The whitespace to use for end of line</param>
        ''' <param name="useElasticTrivia">Whether to use elastic trivia or not</param>
        ''' <param name="useDefaultCasing">Whether to rewrite keywords in default casing or not</param>
        ''' <param name="usePreserveCRLF">Whether to remove extra CRLF (beyond 2) or not</param>
        ''' <remarks></remarks>
        Private Sub New(consideredSpan As TextSpan, indentWhitespace As String, eolWhitespace As String, useElasticTrivia As Boolean, useDefaultCasing As Boolean, usePreserveCRLF As Boolean)
            MyBase.New(visitIntoStructuredTrivia:=True)

            _consideredSpan = consideredSpan
            _indentWhitespace = indentWhitespace
            _useElasticTrivia = useElasticTrivia
            _eolTrivia = If(useElasticTrivia, SyntaxFactory.ElasticEndOfLine(eolWhitespace), SyntaxFactory.EndOfLine(eolWhitespace))
            _useDefaultCasing = useDefaultCasing
            _usePreserveCRLF = usePreserveCRLF
            _afterLineBreak = True
        End Sub

        Private Shared Function EndsInLineBreak(trivia As SyntaxTrivia) As Boolean
            If trivia.IsKind(SyntaxKind.EndOfLineTrivia) Then
                Return True
            End If

            If trivia.IsKind(SyntaxKind.DisabledTextTrivia) Then
                Dim text As String = trivia.ToFullString()
                Return text.Any AndAlso IsNewLineChar(text.Last())
            End If

            If trivia.HasStructure Then
                If trivia.GetStructure.GetLastToken.HasTrailingTrivia AndAlso
                    trivia.GetStructure.GetLastToken.TrailingTrivia.Last.IsKind(SyntaxKind.EndOfLineTrivia) Then

                    Return True
                End If
            End If

            Return False
        End Function

        Private Shared Function EndsWithColonSeparator(token As SyntaxToken) As Boolean
            Return token.HasTrailingTrivia AndAlso
                    token.TrailingTrivia.Last.IsKind(SyntaxKind.ColonTrivia)
        End Function

        Private Shared Function ExtraIndentNeeded(previousToken As SyntaxToken, CurrentToken As SyntaxToken) As Boolean
            If previousToken.ContainsEOLTrivia Then
                If previousToken.IsKind(
                                SyntaxKind.AmpersandToken,
                                SyntaxKind.CommaToken,
                                SyntaxKind.DotToken,
                                SyntaxKind.OpenBraceToken,
                                SyntaxKind.OpenParenToken,
                                SyntaxKind.WithKeyword) Then
                    Return True
                End If
                If CurrentToken.IsKind(SyntaxKind.SelectKeyword) AndAlso CurrentToken.Parent.IsKind(SyntaxKind.SelectClause) Then
                    Return True
                End If
                If CurrentToken.IsKind(SyntaxKind.WhereKeyword) AndAlso CurrentToken.Parent.IsKind(SyntaxKind.WhereClause) Then
                    Return True
                End If
            End If
            Return False
        End Function

        Private Shared Function IsLastTokenOnLine(token As SyntaxToken) As Boolean
            Return (token.HasTrailingTrivia AndAlso token.TrailingTrivia.Last.IsKind(SyntaxKind.ColonTrivia)) OrElse
                (token.Parent IsNot Nothing AndAlso token.Parent.GetLastToken() = token)
        End Function

        Private Shared Function IsNewLineChar(ch As Char) As Boolean
            ' new-line-character:
            '   Carriage return character (U+000D)
            '   Line feed character (U+000A)
            '   Next line character (U+0085)
            '   Line separator character (U+2028)
            '   Paragraph separator character (U+2029)

            Return ch = vbLf _
                OrElse ch = vbCr _
                OrElse ch = "\u0085" _
                OrElse ch = "\u2028" _
                OrElse ch = "\u2029"
        End Function

        Private Shared Function NeedsIndentAfterLineBreak(trivia As SyntaxTrivia) As Boolean
            Select Case trivia.Kind
                Case SyntaxKind.CommentTrivia,
                        SyntaxKind.DocumentationCommentExteriorTrivia,
                        SyntaxKind.DocumentationCommentTrivia
                    Return True

                Case Else
                    Return False
            End Select
        End Function

        Private Shared Function NeedsLineBreakAfter(trivia As SyntaxTrivia) As Boolean
            Return trivia.IsKind(SyntaxKind.CommentTrivia)
        End Function

        Private Shared Function NeedsLineBreakBefore(trivia As SyntaxTrivia) As Boolean
            Select Case trivia.Kind
                Case SyntaxKind.DocumentationCommentExteriorTrivia
                    Return True

                Case Else
                    Return False
            End Select
        End Function

        Private Shared Function NeedsLineBreakBetween(trivia As SyntaxTrivia, nextTrivia As SyntaxTrivia, isTrailingTrivia As Boolean) As Boolean
            If EndsInLineBreak(trivia) Then
                Return False
            End If

            Select Case nextTrivia.Kind
                Case SyntaxKind.CommentTrivia
                    Return False
                Case SyntaxKind.DocumentationCommentExteriorTrivia, SyntaxKind.EmptyStatement,
                SyntaxKind.IfDirectiveTrivia,
                SyntaxKind.ElseIfDirectiveTrivia,
                SyntaxKind.ElseDirectiveTrivia,
                SyntaxKind.EndIfDirectiveTrivia,
                SyntaxKind.RegionDirectiveTrivia,
                SyntaxKind.EndRegionDirectiveTrivia,
                SyntaxKind.ConstDirectiveTrivia,
                SyntaxKind.ExternalSourceDirectiveTrivia,
                SyntaxKind.EndExternalSourceDirectiveTrivia,
                SyntaxKind.ExternalChecksumDirectiveTrivia,
                SyntaxKind.EnableWarningDirectiveTrivia,
                SyntaxKind.DisableWarningDirectiveTrivia,
                SyntaxKind.ReferenceDirectiveTrivia,
                SyntaxKind.BadDirectiveTrivia

                    Return Not isTrailingTrivia

                Case Else
                    Return False
            End Select
        End Function

        Private Shared Function NeedsSeparator(token As SyntaxToken, nextToken As SyntaxToken) As Boolean
            If token.IsKind(SyntaxKind.EndOfFileToken) Then
                Return False
            End If

            If token.Parent Is Nothing OrElse nextToken.IsKind(SyntaxKind.None) Then
                Return False
            End If

            If nextToken.Parent.Kind = SyntaxKind.SingleLineFunctionLambdaExpression Then
                Return True
            End If

            If nextToken.Kind = SyntaxKind.EndOfFileToken Then
                Return False
            End If

            ' +1 instead of + 1
            ' but not a instead of nota ...
            If TypeOf token.Parent Is UnaryExpressionSyntax AndAlso
                token.Kind <> SyntaxKind.NotKeyword AndAlso
                token.Kind <> SyntaxKind.AddressOfKeyword Then
                Return False
            End If

            ' generally a + b, needs to go here to make it b + (a + b) instead of b +(a + b
            If TypeOf token.Parent Is BinaryExpressionSyntax OrElse
                TypeOf nextToken.Parent Is BinaryExpressionSyntax Then
                Return True
            End If

            ' (a instead of ( a
            If token.Kind = SyntaxKind.OpenParenToken Then
                Return False
            End If

            ' a) instead of a )
            If nextToken.Kind = SyntaxKind.CloseParenToken Then
                Return False
            End If

            ' = ( instead of =(
            If token.Kind = SyntaxKind.EqualsToken AndAlso nextToken.Kind = SyntaxKind.OpenParenToken Then
                Return True
            End If

            ' m( instead of m (
            If Not token.IsKind(SyntaxKind.CommaToken, SyntaxKind.AsKeyword) AndAlso nextToken.IsKind(SyntaxKind.OpenParenToken) Then
                Return False
            End If

            ' (,,,) instead of ( , , ,) or (a As Char, b as Char) instead of (a As Char , b as Char)
            ' $"{e,1:C}" instead of $"{e,1:C}"
            If (token.IsKind(SyntaxKind.CommaToken) AndAlso (nextToken.IsKind(SyntaxKind.EmptyToken) OrElse token.Parent.IsKind(SyntaxKind.InterpolationAlignmentClause))) OrElse
               nextToken.IsKind(SyntaxKind.CommaToken) Then
                Return False
            End If

            ' a.b and .b instead of a . b, but keep with {key .field}
            If token.IsKind(SyntaxKind.DotToken) Then
                Return False
            ElseIf nextToken.IsKind(SyntaxKind.DotToken) AndAlso nextToken.Parent.Kind <> SyntaxKind.NamedFieldInitializer Then
                Return False
            End If

            If nextToken.IsKind(SyntaxKind.ColonToken) Then
                If token.Parent.IsKind(SyntaxKind.LabelStatement) Then
                    ' label: instead of label :
                    Return False

                ElseIf nextToken.Parent.IsKind(SyntaxKind.InterpolationFormatClause) Then
                    ' $"{e,1:C}" instead of $"{e,1 :C}"
                    Return False

                End If
            End If

            ' {1 instead of { 1 and 1} instead of 1 }
            If token.IsKind(SyntaxKind.OpenBraceToken) OrElse nextToken.IsKind(SyntaxKind.CloseBraceToken) Then
                Return False
            End If

            ' s1(p1:=23, p2:=12) instead of s1(p1 := 23, p2 := 12)
            If token.IsKind(SyntaxKind.ColonEqualsToken) OrElse nextToken.IsKind(SyntaxKind.ColonEqualsToken) Then
                Return False
            End If

            ' case > 100 should keep separator
            ' need to test before XML analysis below
            If SyntaxFacts.IsRelationalCaseClause(token.Parent.Kind()) OrElse
                SyntaxFacts.IsRelationalCaseClause(nextToken.Parent.Kind()) Then
                Return True
            End If

            ' handle closing attribute before XML tokens
            ' sub goo(<obsolete()> ByRef i as Integer) instead of sub goo(<obsolete()>ByRef i as Integer)
            If token.IsKind(SyntaxKind.GreaterThanToken) AndAlso
                token.Parent.IsKind(SyntaxKind.AttributeList) Then
                Return True
            End If

            ' needs to be checked after binary operators
            ' Imports <goo instead of Imports < goo
            If token.IsKind(SyntaxKind.LessThanToken, SyntaxKind.LessThanSlashToken, SyntaxKind.GreaterThanToken) OrElse
               nextToken.IsKind(SyntaxKind.GreaterThanToken, SyntaxKind.LessThanSlashToken) Then
                Return False
            End If

            ' <xmlns:goo instead of <xmlns : goo
            If (token.IsKind(SyntaxKind.ColonToken) AndAlso token.Parent.IsKind(SyntaxKind.XmlPrefix)) OrElse
                (nextToken.IsKind(SyntaxKind.ColonToken) AndAlso nextToken.Parent.IsKind(SyntaxKind.XmlPrefix)) Then
                Return False
            End If

            ' <e/> instead of <e />
            If nextToken.IsKind(SyntaxKind.SlashGreaterThanToken) Then
                Return False
            End If

            ' <!--a--> instead of <!-- a -->
            If token.IsKind(SyntaxKind.LessThanExclamationMinusMinusToken) OrElse
                nextToken.IsKind(SyntaxKind.MinusMinusGreaterThanToken) Then
                Return False
            End If

            ' <?xml ?> instead of <? xml ?>
            If token.IsKind(SyntaxKind.LessThanQuestionToken) Then
                Return False
            End If

            ' <![CDATA[goo]]> instead of <![CDATA[ goo ]]>
            If token.IsKind(SyntaxKind.BeginCDataToken) OrElse
                nextToken.IsKind(SyntaxKind.EndCDataToken) Then
                Return False
            End If

            ' <Assembly: System.Copyright("(C) 2009")> instead of <Assembly: System.Copyright("(C) 2009")>
            If token.IsKind(SyntaxKind.ColonToken) AndAlso token.Parent.IsKind(SyntaxKind.AttributeTarget) Then
                Return True
            End If
            ' <Assembly: System.Copyright("(C) 2009")> instead of <Assembly :System.Copyright("(C) 2009")>
            If nextToken.IsKind(SyntaxKind.ColonToken) AndAlso nextToken.Parent.IsKind(SyntaxKind.AttributeTarget) Then
                Return False
            End If

            ' <goo="bar" instead of <goo = "bar"
            If (token.IsKind(SyntaxKind.EqualsToken) AndAlso
                token.Parent.IsKind(SyntaxKind.XmlAttribute, SyntaxKind.XmlCrefAttribute, SyntaxKind.XmlNameAttribute, SyntaxKind.XmlDeclaration)) OrElse
                (nextToken.IsKind(SyntaxKind.EqualsToken) AndAlso
                nextToken.Parent.IsKind(SyntaxKind.XmlAttribute, SyntaxKind.XmlCrefAttribute, SyntaxKind.XmlNameAttribute, SyntaxKind.XmlDeclaration)) Then
                Return False
            End If

            ' needs to be below binary expression checks
            ' <attrib="goo" instead of <attrib=" goo "
            If token.IsKind(SyntaxKind.DoubleQuoteToken) OrElse
                nextToken.IsKind(SyntaxKind.DoubleQuoteToken) Then
                Return False
            End If

            ' <x/>@a instead of <x/>@ a
            If token.IsKind(SyntaxKind.AtToken) AndAlso token.Parent.IsKind(SyntaxKind.XmlAttributeAccessExpression) Then
                Return False
            End If

            ' 'e' instead of ' e '
            If token.IsKind(SyntaxKind.SingleQuoteToken) OrElse
                nextToken.IsKind(SyntaxKind.SingleQuoteToken) Then
                Return False
            End If

            ' Integer? instead of Integer ?
            If nextToken.IsKind(SyntaxKind.QuestionToken) Then
                Return False
            End If

            ' #if instead of # if
            If token.IsKind(SyntaxKind.HashToken) AndAlso TypeOf token.Parent Is DirectiveTriviaSyntax Then
                Return False
            End If

            ' "#region" instead of "#region "
            If token.Parent.IsKind(SyntaxKind.RegionDirectiveTrivia) AndAlso
                nextToken.IsKind(SyntaxKind.StringLiteralToken) AndAlso
                String.IsNullOrEmpty(nextToken.ValueText) Then
                Return False
            End If

            If token.IsKind(SyntaxKind.XmlTextLiteralToken, SyntaxKind.DocumentationCommentLineBreakToken) Then
                Return False
            End If

            If token.IsKind(SyntaxKind.XmlTextLiteralToken, SyntaxKind.DocumentationCommentExteriorTrivia) Then
                Return False
            End If

            If token.IsKind(SyntaxKind.XmlEntityLiteralToken) Then
                Return False
            End If

            If token.IsKind(SyntaxKind.DollarSignDoubleQuoteToken) Then
                Return False
            End If

            If token.IsKind(SyntaxKind.InterpolatedStringTextToken) OrElse nextToken.IsKind(SyntaxKind.InterpolatedStringTextToken) Then
                Return False
            End If
            If token.IsKind(SyntaxKind.CloseBraceToken) AndAlso token.Parent.IsKind(SyntaxKind.Interpolation) Then
                Return False
            End If
            Return True
        End Function

        Private Shared Function NeedsSeparatorBetween(trivia As SyntaxTrivia) As Boolean
            Select Case trivia.Kind
                Case SyntaxKind.None,
                        SyntaxKind.WhitespaceTrivia,
                        SyntaxKind.DocumentationCommentExteriorTrivia,
                        SyntaxKind.EndOfLineTrivia
                    Return False
                Case SyntaxKind.LineContinuationTrivia
                    Return True
                Case Else
                    Return Not SyntaxFacts.IsPreprocessorDirective(trivia.Kind)
            End Select
        End Function

        Private Sub AddLinebreaksAfterElementsIfNeeded(Of TNode As SyntaxNode)(
                                                    list As SyntaxList(Of TNode),
            linebreaksBetweenElements As Integer,
            linebreaksAfterLastElement As Integer
        )
            Dim lastElementIndex As Integer = list.Count - 1
            For elementIndex As Integer = 0 To lastElementIndex
                Dim listElement As TNode = list(elementIndex)
                If listElement.IsKind(SyntaxKind.LabelStatement) Then
                    ' always add line breaks after label
                    _lineBreaksAfterToken(listElement.GetLastToken()) = 1
                Else
                    Me.AddLinebreaksAfterTokenIfNeeded(listElement.GetLastToken(),
                                                       If(elementIndex = lastElementIndex,
                                                            linebreaksAfterLastElement,
                                                            linebreaksBetweenElements)
                                                        )
                End If
            Next
        End Sub

        Private Sub AddLinebreaksAfterTokenIfNeeded(node As SyntaxToken, linebreaksAfterToken As Integer)
            If Not EndsWithColonSeparator(node) Then
                _lineBreaksAfterToken(node) = linebreaksAfterToken
            End If
        End Sub

        Private Sub Free()
            If _indentations IsNot Nothing Then
                _indentations.Clear()
            End If
        End Sub

        Private Function GetEndOfLine() As SyntaxTrivia
            Return _eolTrivia
        End Function

        Private Function GetIndentation(count As Integer, CurrentToken As SyntaxToken) As SyntaxTrivia
            If ExtraIndentNeeded(_previousToken, CurrentToken) Then
                count += 1
            End If
            Dim capacity As Integer = count + 1
            If _indentations Is Nothing Then
                _indentations = New List(Of SyntaxTrivia)(capacity)
            End If

            For index As Integer = _indentations.Count To count
                Dim text As String = If(index = 0, "", _indentations(index - 1).ToString() & _indentWhitespace)
                _indentations.Add(If(_useElasticTrivia, SyntaxFactory.ElasticWhitespace(text), SyntaxFactory.Whitespace(text)))
            Next

            Return _indentations(count)
        End Function

        ''' <summary>
        ''' indentation depth is the declaration depth for statements within the block. for start/end statements
        ''' of these blocks (e.g. the if statement), it is a level less
        ''' </summary>
        Private Function GetIndentationDepth() As Integer
            Debug.Assert(_indentationDepth >= 0)
            Return _indentationDepth
        End Function

        Private Function GetIndentationDepth(trivia As SyntaxTrivia) As Integer
            If SyntaxFacts.IsPreprocessorDirective(trivia.Kind) Then
                Return 0
            End If

            Return Me.GetIndentationDepth()
        End Function

        Private Function GetNextRelevantToken(token As SyntaxToken) As SyntaxToken
            Dim nextToken As SyntaxToken = token.GetNextToken(Function(t As SyntaxToken)
                                                                  Return Not t.IsKind(SyntaxKind.None)
                                                              End Function, Function(t As SyntaxTrivia) False)

            If _consideredSpan.Contains(nextToken.FullSpan) Then
                Return nextToken
            Else
                Return Nothing
            End If

        End Function

        Private Function GetSpace() As SyntaxTrivia
            Return If(_useElasticTrivia, Factory.ElasticSpace(), Factory.Space)
        End Function

        Private Function LineBreaksBetween(currentToken As SyntaxToken, nextToken As SyntaxToken) As Integer
            ' First and last token may be of kind none
            If currentToken.IsKind(SyntaxKind.None) OrElse nextToken.IsKind(SyntaxKind.None) Then
                Return 0
            End If

            Dim numLineBreaks As Integer = 0
            If _lineBreaksAfterToken.TryGetValue(currentToken, numLineBreaks) Then
                Return Math.Max(1, numLineBreaks)
            End If
            'Structured trivia may end in  NewLine so don't need to add another
            If currentToken.ToFullString.IsNewLine Then
                Return 0
            End If
            Return If(currentToken.ContainsEOLTrivia, 1, 0)
        End Function

        Private Sub MarkLastStatementIfNeeded(Of TNode As SyntaxNode)(list As SyntaxList(Of TNode))
            If list.Any Then
                _lastStatementsInBlocks.Add(list.Last)
            End If
        End Sub

        ''' <summary>
        '''
        ''' </summary>
        ''' <param name="CurrentToken">Original Token</param>
        ''' <param name="triviaList">List of trivia From Original Token</param>
        ''' <param name="depth">How many indents are required</param>
        ''' <param name="isTrailing">True if the triviaList leading</param>
        ''' <param name="mustBeIndented">True if indent is required</param>
        ''' <param name="mustHaveSeparator">True if a space is needed</param>
        ''' <param name="lineBreaksAfter">Number of Line Breaks required after this token (0 or 1)</param>
        ''' <param name="lineBreaksBefore">Number of Line Breaks required before this token</param>
        ''' <returns></returns>
        Private Function RewriteTrivia(CurrentToken As SyntaxToken,
                                       triviaList As SyntaxTriviaList,
                                       depth As Integer,
                                       isTrailing As Boolean,
                                       mustBeIndented As Boolean,
                                       mustHaveSeparator As Boolean,
                                       lineBreaksAfter As Integer,
                                       lineBreaksBefore As Integer) As SyntaxTriviaList

            Dim currentTriviaList As New SyntaxTriviaList
            Try
                For index As Integer = 1 To lineBreaksBefore
                    If _eolLeadingTriviaCount < 2 Then
                        currentTriviaList = currentTriviaList.Add(Me.GetEndOfLine())
                        _eolLeadingTriviaCount += 1
                    End If
                    _afterLineBreak = True
                    _afterIndentation = False
                Next

                Dim minLeadingSpaces As Integer = 0
                For Each e As IndexClass(Of SyntaxTrivia) In triviaList.WithIndex
                    Dim trivia As SyntaxTrivia = e.Value
                    ' just keep non whitespace trivia
                    If trivia.IsKind(SyntaxKind.WhitespaceTrivia) Then
                        If _usePreserveCRLF AndAlso Not _afterIndentation Then
                            minLeadingSpaces = trivia.ToString.Length
                        End If
                        Continue For
                    ElseIf trivia.FullWidth = 0 OrElse
                             (trivia.IsKind(SyntaxKind.EndOfLineTrivia) And Not _usePreserveCRLF) Then
                        Continue For
                    End If
                    If trivia.IsKind(SyntaxKind.EndOfLineTrivia) Then
                        If isTrailing Then
                            If _eolTraiingTriviaCount = 0 Then
                                currentTriviaList = currentTriviaList.Add(Me.GetEndOfLine())
                                _eolTraiingTriviaCount += 1
                            Else
                                If currentTriviaList.Last.IsComment AndAlso Not e.IsLast Then
                                    currentTriviaList = currentTriviaList.Add(VBEOLTrivia)
                                End If
                                Continue For
                            End If
                        Else
                            If _eolLeadingTriviaCount < 1 Then
                                currentTriviaList = currentTriviaList.Add(Me.GetEndOfLine())
                                _eolLeadingTriviaCount += 1
                            Else
                                Continue For
                            End If
                        End If
                        _afterIndentation = False
                        _afterLineBreak = True
                    End If
                    If trivia.IsKind(SyntaxKind.LineContinuationTrivia) Then
                        If Not _afterIndentation Then
                            currentTriviaList = currentTriviaList.Add(SyntaxFactory.WhitespaceTrivia(Space(minLeadingSpaces)))
                        End If
                        currentTriviaList = currentTriviaList.Add(LineContinuation)
                        If isTrailing Then
                            _afterIndentation = False
                        Else
                            currentTriviaList = currentTriviaList.Add(Factory.Space)
                            _afterIndentation = True
                        End If
                        _afterLineBreak = False
                        Continue For
                    End If
                    ' check if there's a separator or a line break needed between the trivia itself
                    Dim tokenParent As SyntaxNode = trivia.Token.Parent
                    Dim needsSeparator As Boolean =
                            Not (trivia.IsKind(SyntaxKind.ColonTrivia) AndAlso tokenParent IsNot Nothing AndAlso tokenParent.IsKind(SyntaxKind.LabelStatement)) AndAlso
                            Not (tokenParent IsNot Nothing AndAlso tokenParent.Parent IsNot Nothing AndAlso tokenParent.Parent.IsKind(SyntaxKind.CrefReference)) AndAlso
                            (
                                (currentTriviaList.Any AndAlso NeedsSeparatorBetween(currentTriviaList.Last()) AndAlso Not EndsInLineBreak(currentTriviaList.Last())) OrElse
                                (currentTriviaList.Count = 0 AndAlso isTrailing)
                            )

                    Dim needsLineBreak As Boolean = NeedsLineBreakBefore(trivia) OrElse
                            (currentTriviaList.Any AndAlso NeedsLineBreakBetween(currentTriviaList.Last(), trivia, isTrailing))

                    If needsLineBreak AndAlso Not _afterLineBreak Then
                        If _eolTraiingTriviaCount = 0 Then
                            currentTriviaList = currentTriviaList.Add(Me.GetEndOfLine())
                            _eolTraiingTriviaCount += 1
                        End If
                        _afterLineBreak = True
                        _afterIndentation = False
                    End If

                    If _afterLineBreak And Not isTrailing Then
                        If Not _afterIndentation AndAlso NeedsIndentAfterLineBreak(trivia) Then
                            currentTriviaList = currentTriviaList.Add(Me.GetIndentation(Me.GetIndentationDepth(trivia), CurrentToken))
                            _afterIndentation = True
                        End If

                    ElseIf needsSeparator Then
                        currentTriviaList = currentTriviaList.Add(Me.GetSpace())
                        If minLeadingSpaces > 0 Then
                            minLeadingSpaces -= 1
                        End If
                        _afterLineBreak = False
                        _afterIndentation = False
                    End If
                    Dim needExtraSpace As Boolean = _isInStructuredTrivia AndAlso
                                SyntaxFactory.DocumentationCommentExteriorTrivia(SyntaxFacts.GetText(SyntaxKind.DocumentationCommentExteriorTrivia)).ToString = trivia.ToString
                    If trivia.HasStructure Then
                        Dim structuredTrivia As SyntaxTrivia = Me.VisitStructuredTrivia(trivia)
                        currentTriviaList = currentTriviaList.Add(structuredTrivia)
                    Else
                        ' in structured trivia, the XML doc ''' token contains leading whitespace as text
                        If trivia.IsKind(SyntaxKind.DocumentationCommentExteriorTrivia) OrElse needExtraSpace Then
                            trivia = SyntaxFactory.DocumentationCommentExteriorTrivia(SyntaxFacts.GetText(SyntaxKind.DocumentationCommentExteriorTrivia))
                        End If
                        If trivia.IsKind(SyntaxKind.EndOfLineTrivia) Then
                            ' Skip it if was already handled
                        Else
                            If isTrailing Then
                                currentTriviaList = currentTriviaList.Add(SyntaxFactory.WhitespaceTrivia(Space(minLeadingSpaces)))
                                minLeadingSpaces = 0
                            End If
                            currentTriviaList = currentTriviaList.Add(trivia)
                            ' Allow one return after this trivia
                            _eolTraiingTriviaCount = 0
                        End If
                    End If

                    If NeedsLineBreakAfter(trivia) Then
                        If Not isTrailing Then
                            currentTriviaList = currentTriviaList.Add(Me.GetEndOfLine())
                            _eolLeadingTriviaCount += 1
                        End If
                        _afterLineBreak = True
                        _afterIndentation = False
                    Else
                        _afterLineBreak = EndsInLineBreak(trivia)
                    End If
                Next

                If lineBreaksAfter > 0 Then
                    If currentTriviaList.Any AndAlso EndsInLineBreak(currentTriviaList.Last()) Then
                        lineBreaksAfter -= 1
                    End If

                    For index As Integer = 0 To lineBreaksAfter - 1
                        If Not isTrailing Then
                            Throw UnexpectedValue("IsTrailing")
                        End If
                        If _eolTraiingTriviaCount = 0 Then
                            currentTriviaList = currentTriviaList.Add(Me.GetEndOfLine())
                        End If
                        _eolLeadingTriviaCount = 0
                        _eolTraiingTriviaCount += 1
                        _afterLineBreak = True
                        _afterIndentation = False
                    Next index

                ElseIf mustHaveSeparator Then
                    currentTriviaList = currentTriviaList.Add(Me.GetSpace())
                    _afterLineBreak = False
                    _afterIndentation = False
                End If

                If mustBeIndented Then
                    currentTriviaList = currentTriviaList.Add(Me.GetIndentation(depth, CurrentToken))
                    _afterIndentation = True
                    _afterLineBreak = False
                End If

                If currentTriviaList.Count = 0 Then
                    Return If(_useElasticTrivia, SyntaxFactory.TriviaList(SyntaxFactory.ElasticMarker), Nothing)
                ElseIf currentTriviaList.Count = 1 Then
                    Return SyntaxFactory.TriviaList(currentTriviaList.First())
                Else
                    Return SyntaxFactory.TriviaList(currentTriviaList)
                End If
            Finally
            End Try
        End Function

        Private Sub VisitForOrForEachBlock(node As ForOrForEachBlockSyntax)
            Me.AddLinebreaksAfterTokenIfNeeded(node.ForOrForEachStatement.GetLastToken(), 1)

            Me.AddLinebreaksAfterElementsIfNeeded(node.Statements, 1, 1)

            Me.MarkLastStatementIfNeeded(node.Statements)

            If node.NextStatement IsNot Nothing Then
                If Not _lastStatementsInBlocks.Contains(node) Then
                    Me.AddLinebreaksAfterTokenIfNeeded(node.NextStatement.GetLastToken(), 2)
                Else
                    Me.AddLinebreaksAfterTokenIfNeeded(node.NextStatement.GetLastToken(), 1)
                End If
            End If
        End Sub

        Private Overloads Function VisitStructuredTrivia(trivia As SyntaxTrivia) As SyntaxTrivia
            Dim oldIsInStructuredTrivia As Boolean = _isInStructuredTrivia
            _isInStructuredTrivia = True

            Dim oldPreviousToken As SyntaxToken = _previousToken
            _previousToken = Nothing

            Dim result As SyntaxTrivia = Me.VisitTrivia(trivia)

            _isInStructuredTrivia = oldIsInStructuredTrivia
            _previousToken = oldPreviousToken

            Return result
        End Function

        ''' <summary>
        ''' We want to display type blocks (Modules, Classes, Structures and Interfaces) like follows
        ''' Class Goo
        '''   implements IBar1, IBar2
        '''   implements IBar3
        '''   inherits Bar1
        '''
        ''' Public Sub Boo()
        '''    End Sub
        ''' End Class
        '''
        ''' or
        '''
        ''' Class Goo
        '''
        ''' Public Sub Boo()
        '''   End Sub
        ''' End Class
        '''
        ''' Basically it's an empty line between implements and inherits and between each member. If there are no
        ''' inherits or implements, add an empty line before the first member.
        ''' </summary>
        Private Sub VisitTypeBlockSyntax(node As TypeBlockSyntax)

            Dim hasImplements As Boolean = node.Implements.Any
            Dim hasInherits As Boolean = node.Inherits.Any

            ' add a line break between begin statement and the ones from the statement list
            If Not hasInherits AndAlso Not hasImplements AndAlso node.members.Any Then
                Me.AddLinebreaksAfterTokenIfNeeded(node.BlockStatement.GetLastToken(), 2)
            Else
                Me.AddLinebreaksAfterTokenIfNeeded(node.BlockStatement.GetLastToken(), 1)
            End If

            If hasImplements Then
                Me.AddLinebreaksAfterElementsIfNeeded(node.Inherits, 1, 1)
            Else
                Me.AddLinebreaksAfterElementsIfNeeded(node.Inherits, 1, 2)
            End If

            Me.AddLinebreaksAfterElementsIfNeeded(node.Implements, 1, 2)

            Select Case node.Kind
                Case SyntaxKind.InterfaceBlock
                    Me.AddLinebreaksAfterElementsIfNeeded(node.members, 1, 2)
                Case SyntaxKind.StructureBlock
                    Me.AddLinebreaksAfterElementsIfNeeded(node.members, 1, 2)
                Case SyntaxKind.ModuleBlock
                    Me.AddLinebreaksAfterElementsIfNeeded(node.members, 1, 2)
                Case Else
                    Me.AddLinebreaksAfterElementsIfNeeded(node.members, 2, 1)
            End Select
        End Sub

        Friend Shared Function Normalize(Of TNode As SyntaxNode)(node As TNode, indentWhitespace As String, eolWhitespace As String, useElasticTrivia As Boolean, useDefaultCasing As Boolean, usePreserveCRLF As Boolean) As SyntaxNode
            Dim normalizer As New SyntaxNormalizer(node.FullSpan, indentWhitespace, eolWhitespace, useElasticTrivia, useDefaultCasing, usePreserveCRLF)
            Dim result As TNode = DirectCast(normalizer.Visit(node), TNode)
            normalizer.Free()
            Return result
        End Function

        Public Overrides Function VisitAccessorBlock(node As AccessorBlockSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.BlockStatement.GetLastToken(), 1)

            Me.AddLinebreaksAfterElementsIfNeeded(node.Statements, 1, 1)

            Me.MarkLastStatementIfNeeded(node.Statements)

            Return MyBase.VisitAccessorBlock(node)
        End Function

        Public Overrides Function VisitAccessorStatement(node As AccessorStatementSyntax) As SyntaxNode
            Dim result As SyntaxNode = MyBase.VisitAccessorStatement(node)
            _indentationDepth += 1

            Return result
        End Function

        Public Overrides Function VisitAttributeList(node As AttributeListSyntax) As SyntaxNode
            ' do not add line breaks for attributes of parameters or return types
            If node.Parent Is Nothing OrElse
                (node.Parent.Kind <> SyntaxKind.Parameter AndAlso node.Parent.Kind <> SyntaxKind.SimpleAsClause) Then

                Me.AddLinebreaksAfterTokenIfNeeded(node.GetLastToken(), 1)
            End If

            Return MyBase.VisitAttributeList(node)
        End Function

        <ExcludeFromCodeCoverage>
        Public Overrides Function VisitBadDirectiveTrivia(node As BadDirectiveTriviaSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.GetLastToken(), 1)

            Return MyBase.VisitBadDirectiveTrivia(node)
        End Function

        Public Overrides Function VisitCaseBlock(node As CaseBlockSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.CaseStatement.GetLastToken(), 1)

            Me.AddLinebreaksAfterElementsIfNeeded(node.Statements, 1, 1)

            Dim result As SyntaxNode = MyBase.VisitCaseBlock(node)
            _indentationDepth -= 1

            Return result
        End Function

        Public Overrides Function VisitCaseStatement(node As CaseStatementSyntax) As SyntaxNode
            Dim result As SyntaxNode = MyBase.VisitCaseStatement(node)
            _indentationDepth += 1

            Return result
        End Function

        Public Overrides Function VisitCatchBlock(node As CatchBlockSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.CatchStatement.GetLastToken(), 1)

            Me.AddLinebreaksAfterElementsIfNeeded(node.Statements, 1, 1)

            Return MyBase.VisitCatchBlock(node)
        End Function

        Public Overrides Function VisitCatchStatement(node As CatchStatementSyntax) As SyntaxNode
            _indentationDepth -= 1
            Dim result As SyntaxNode = MyBase.VisitCatchStatement(node)
            _indentationDepth += 1

            Return result
        End Function

        Public Overrides Function VisitClassBlock(node As ClassBlockSyntax) As SyntaxNode
            Me.VisitTypeBlockSyntax(node)

            Return MyBase.VisitClassBlock(node)
        End Function

        Public Overrides Function VisitClassStatement(node As ClassStatementSyntax) As SyntaxNode
            Dim result As SyntaxNode = MyBase.VisitClassStatement(node)
            _indentationDepth += 1

            Return result
        End Function

        ''' <summary>
        ''' We each element of option, imports and attributes on a separate line, where the last element of this the list if
        ''' followed by an empty line:
        ''' Option Strict On
        '''
        ''' Imports System
        ''' Imports Goo
        '''
        ''' [...]
        '''
        ''' Namespace
        ''' [...]
        ''' </summary>
        Public Overrides Function VisitCompilationUnit(node As CompilationUnitSyntax) As SyntaxNode
            Dim hasImports As Boolean = node.Imports.Any
            Dim hasMembers As Boolean = node.members.Any
            Dim hasAttributes As Boolean = node.attributes.Any

            If hasImports OrElse hasAttributes OrElse hasMembers Then
                Me.AddLinebreaksAfterElementsIfNeeded(node.Options, 1, 2)
            Else
                Me.AddLinebreaksAfterElementsIfNeeded(node.Options, 1, 1)
            End If

            If hasAttributes OrElse hasMembers Then
                Me.AddLinebreaksAfterElementsIfNeeded(node.Imports, 1, 2)
            Else
                Me.AddLinebreaksAfterElementsIfNeeded(node.Imports, 1, 1)
            End If

            If hasMembers Then
                Me.AddLinebreaksAfterElementsIfNeeded(node.attributes, 1, 2)
            Else
                Me.AddLinebreaksAfterElementsIfNeeded(node.attributes, 1, 1)
            End If

            Me.AddLinebreaksAfterElementsIfNeeded(node.members, 2, 1)

            Return MyBase.VisitCompilationUnit(node)
        End Function

        <ExcludeFromCodeCoverage>
        Public Overrides Function VisitConstDirectiveTrivia(node As ConstDirectiveTriviaSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.GetLastToken(), 1)

            Return MyBase.VisitConstDirectiveTrivia(node)
        End Function

        Public Overrides Function VisitConstructorBlock(node As ConstructorBlockSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.BlockStatement.GetLastToken(), 1)

            Me.AddLinebreaksAfterElementsIfNeeded(node.Statements, 1, 1)

            Me.MarkLastStatementIfNeeded(node.Statements)

            Return MyBase.VisitConstructorBlock(node)
        End Function

        <ExcludeFromCodeCoverage>
        Public Overrides Function VisitDisableWarningDirectiveTrivia(node As DisableWarningDirectiveTriviaSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.GetLastToken(), 1)

            Return MyBase.VisitDisableWarningDirectiveTrivia(node)
        End Function

        Public Overrides Function VisitDoLoopBlock(node As DoLoopBlockSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.DoStatement.GetLastToken(), 1)

            Me.AddLinebreaksAfterElementsIfNeeded(node.Statements, 1, 1)

            Me.MarkLastStatementIfNeeded(node.Statements)

            If _lastStatementsInBlocks.Contains(node) Then
                Me.AddLinebreaksAfterTokenIfNeeded(node.LoopStatement.GetLastToken(), 1)
            Else
                Me.AddLinebreaksAfterTokenIfNeeded(node.LoopStatement.GetLastToken(), 2)
            End If

            Return MyBase.VisitDoLoopBlock(node)
        End Function

        Public Overrides Function VisitDoStatement(node As DoStatementSyntax) As SyntaxNode
            Dim result As SyntaxNode = MyBase.VisitDoStatement(node)
            _indentationDepth += 1

            Return result
        End Function

        Public Overrides Function VisitElseBlock(node As ElseBlockSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.ElseStatement.GetLastToken(), 1)

            Me.AddLinebreaksAfterElementsIfNeeded(node.Statements, 1, 1)

            Me.MarkLastStatementIfNeeded(node.Statements)

            Return MyBase.VisitElseBlock(node)
        End Function

        <ExcludeFromCodeCoverage>
        Public Overrides Function VisitElseDirectiveTrivia(node As ElseDirectiveTriviaSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.GetLastToken(), 1)

            Return MyBase.VisitElseDirectiveTrivia(node)
        End Function

        Public Overrides Function VisitElseIfBlock(node As ElseIfBlockSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.ElseIfStatement.GetLastToken(), 1)

            Me.AddLinebreaksAfterElementsIfNeeded(node.Statements, 1, 1)

            Me.MarkLastStatementIfNeeded(node.Statements)

            Return MyBase.VisitElseIfBlock(node)
        End Function

        Public Overrides Function VisitElseIfStatement(node As ElseIfStatementSyntax) As SyntaxNode
            _indentationDepth -= 1
            Dim result As SyntaxNode = MyBase.VisitElseIfStatement(node)
            _indentationDepth += 1

            Return result
        End Function

        Public Overrides Function VisitElseStatement(node As ElseStatementSyntax) As SyntaxNode
            _indentationDepth -= 1
            Dim result As SyntaxNode = MyBase.VisitElseStatement(node)
            _indentationDepth += 1

            Return result
        End Function

        <ExcludeFromCodeCoverage>
        Public Overrides Function VisitEnableWarningDirectiveTrivia(node As EnableWarningDirectiveTriviaSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.GetLastToken(), 1)

            Return MyBase.VisitEnableWarningDirectiveTrivia(node)
        End Function

        Public Overrides Function VisitEndBlockStatement(node As EndBlockStatementSyntax) As SyntaxNode
            _indentationDepth -= 1

            Return MyBase.VisitEndBlockStatement(node)
        End Function

        <ExcludeFromCodeCoverage>
        Public Overrides Function VisitEndExternalSourceDirectiveTrivia(node As EndExternalSourceDirectiveTriviaSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.GetLastToken(), 1)

            Return MyBase.VisitEndExternalSourceDirectiveTrivia(node)
        End Function

        <ExcludeFromCodeCoverage>
        Public Overrides Function VisitEndIfDirectiveTrivia(node As EndIfDirectiveTriviaSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.GetLastToken(), 1)

            Return MyBase.VisitEndIfDirectiveTrivia(node)
        End Function

        <ExcludeFromCodeCoverage>
        Public Overrides Function VisitEndRegionDirectiveTrivia(node As EndRegionDirectiveTriviaSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.GetLastToken(), 1)

            Return MyBase.VisitEndRegionDirectiveTrivia(node)
        End Function

        ''' <summary>
        ''' Each statement and the begin will be displayed on a separate line. No empty lines.
        ''' </summary>
        Public Overrides Function VisitEnumBlock(node As EnumBlockSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.EnumStatement.GetLastToken(), 1)
            Me.AddLinebreaksAfterElementsIfNeeded(node.members, 1, 1)

            Return MyBase.VisitEnumBlock(node)
        End Function

        Public Overrides Function VisitEnumStatement(node As EnumStatementSyntax) As SyntaxNode
            Dim result As SyntaxNode = MyBase.VisitEnumStatement(node)
            _indentationDepth += 1

            Return result
        End Function

        Public Overrides Function VisitEventBlock(node As EventBlockSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.EventStatement.GetLastToken, 1)
            Me.AddLinebreaksAfterElementsIfNeeded(node.Accessors, 2, 1)
            Return MyBase.VisitEventBlock(node)
        End Function

        Public Overrides Function VisitEventStatement(node As EventStatementSyntax) As SyntaxNode
            Dim result As SyntaxNode = MyBase.VisitEventStatement(node)

            ' only indent if this is a block
            If node.Parent IsNot Nothing AndAlso node.Parent.IsKind(SyntaxKind.EventBlock) Then
                _indentationDepth += 1
            End If

            Return result
        End Function

        <ExcludeFromCodeCoverage>
        Public Overrides Function VisitExternalChecksumDirectiveTrivia(node As ExternalChecksumDirectiveTriviaSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.GetLastToken(), 1)

            Return MyBase.VisitExternalChecksumDirectiveTrivia(node)
        End Function

        <ExcludeFromCodeCoverage>
        Public Overrides Function VisitExternalSourceDirectiveTrivia(node As ExternalSourceDirectiveTriviaSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.GetLastToken(), 1)

            Return MyBase.VisitExternalSourceDirectiveTrivia(node)
        End Function

        Public Overrides Function VisitFinallyBlock(node As FinallyBlockSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.FinallyStatement.GetLastToken(), 1)
            Me.AddLinebreaksAfterElementsIfNeeded(node.Statements, 1, 1)
            Me.MarkLastStatementIfNeeded(node.Statements)
            Return MyBase.VisitFinallyBlock(node)
        End Function

        Public Overrides Function VisitFinallyStatement(node As FinallyStatementSyntax) As SyntaxNode
            _indentationDepth -= 1
            Dim result As SyntaxNode = MyBase.VisitFinallyStatement(node)
            _indentationDepth += 1

            Return result
        End Function

        Public Overrides Function VisitForBlock(node As ForBlockSyntax) As SyntaxNode
            Me.VisitForOrForEachBlock(node)

            Return MyBase.VisitForBlock(node)
        End Function

        Public Overrides Function VisitForEachBlock(node As ForEachBlockSyntax) As SyntaxNode
            Me.VisitForOrForEachBlock(node)

            Return MyBase.VisitForEachBlock(node)
        End Function

        Public Overrides Function VisitForEachStatement(node As ForEachStatementSyntax) As SyntaxNode
            Dim result As SyntaxNode = MyBase.VisitForEachStatement(node)
            _indentationDepth += 1

            Return result
        End Function

        Public Overrides Function VisitForStatement(node As ForStatementSyntax) As SyntaxNode
            Dim result As SyntaxNode = MyBase.VisitForStatement(node)
            _indentationDepth += 1

            Return result
        End Function

        <ExcludeFromCodeCoverage>
        Public Overrides Function VisitIfDirectiveTrivia(node As IfDirectiveTriviaSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.GetLastToken(), 1)

            Return MyBase.VisitIfDirectiveTrivia(node)
        End Function

        Public Overrides Function VisitIfStatement(node As IfStatementSyntax) As SyntaxNode
            Dim result As SyntaxNode = MyBase.VisitIfStatement(node)
            _indentationDepth += 1

            Return result
        End Function

        Public Overrides Function VisitInheritsStatement(node As InheritsStatementSyntax) As SyntaxNode
            Return MyBase.VisitInheritsStatement(node)
        End Function

        Public Overrides Function VisitInterfaceBlock(node As InterfaceBlockSyntax) As SyntaxNode
            Me.VisitTypeBlockSyntax(node)

            Return MyBase.VisitInterfaceBlock(node)
        End Function

        Public Overrides Function VisitInterfaceStatement(node As InterfaceStatementSyntax) As SyntaxNode
            Dim result As SyntaxNode = MyBase.VisitInterfaceStatement(node)
            _indentationDepth += 1

            Return result
        End Function

        Public Overrides Function VisitLabelStatement(node As LabelStatementSyntax) As SyntaxNode
            ' labels are never indented.
            Dim previousIndentationDepth As Integer = _indentationDepth
            _indentationDepth = 0
            Dim result As SyntaxNode = MyBase.VisitLabelStatement(node)
            _indentationDepth = previousIndentationDepth

            Return result
        End Function

        Public Overrides Function VisitLoopStatement(node As LoopStatementSyntax) As SyntaxNode
            _indentationDepth -= 1

            Return MyBase.VisitLoopStatement(node)
        End Function

        Public Overrides Function VisitMethodBlock(node As MethodBlockSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.BlockStatement.GetLastToken(), 1)

            Me.AddLinebreaksAfterElementsIfNeeded(node.Statements, 1, 1)

            Me.MarkLastStatementIfNeeded(node.Statements)

            Return MyBase.VisitMethodBlock(node)
        End Function

        Public Overrides Function VisitMethodStatement(node As MethodStatementSyntax) As SyntaxNode
            Dim result As SyntaxNode = MyBase.VisitMethodStatement(node)

            ' only indent if this is a block
            If node.Parent IsNot Nothing AndAlso
                node.Parent.IsKind(SyntaxKind.SubBlock, SyntaxKind.FunctionBlock) Then
                _indentationDepth += 1
            End If

            Return result
        End Function

        Public Overrides Function VisitModuleBlock(node As ModuleBlockSyntax) As SyntaxNode
            Me.VisitTypeBlockSyntax(node)

            Return MyBase.VisitModuleBlock(node)
        End Function

        Public Overrides Function VisitModuleStatement(node As ModuleStatementSyntax) As SyntaxNode
            Dim result As SyntaxNode = MyBase.VisitModuleStatement(node)
            _indentationDepth += 1

            Return result
        End Function

        Public Overrides Function VisitMultiLineIfBlock(node As MultiLineIfBlockSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.IfStatement.GetLastToken(), 1)
            Me.AddLinebreaksAfterElementsIfNeeded(node.Statements, 1, 1)
            Me.MarkLastStatementIfNeeded(node.Statements)

            Dim previousNode As VisualBasicSyntaxNode

            If node.Statements.Any() Then
                previousNode = node.Statements.Last()
            Else
                previousNode = node.IfStatement
            End If

            For Each elseIfBlock As ElseIfBlockSyntax In node.ElseIfBlocks
                Me.AddLinebreaksAfterTokenIfNeeded(previousNode.GetLastToken(), 1)
                previousNode = elseIfBlock
            Next

            If node.ElseBlock IsNot Nothing Then
                Me.AddLinebreaksAfterTokenIfNeeded(previousNode.GetLastToken(), 1)
            End If

            If Not _lastStatementsInBlocks.Contains(node) Then
                Me.AddLinebreaksAfterTokenIfNeeded(node.EndIfStatement.GetLastToken(), 2)
            Else
                Me.AddLinebreaksAfterTokenIfNeeded(node.EndIfStatement.GetLastToken(), 1)
            End If

            Return MyBase.VisitMultiLineIfBlock(node)
        End Function

        Public Overrides Function VisitMultiLineLambdaExpression(node As MultiLineLambdaExpressionSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.SubOrFunctionHeader.GetLastToken(), 1)
            ' one statement per line
            Me.AddLinebreaksAfterElementsIfNeeded(node.Statements, 1, 1)
            Me.MarkLastStatementIfNeeded(node.Statements)
            _indentationDepth += 1
            Dim result As SyntaxNode = MyBase.VisitMultiLineLambdaExpression(node)

            Return result
        End Function

        ''' <summary>
        ''' Add an empty line after the begin, except the first member is a nested namespace.
        ''' Separate each member of a namespace with an empty line.
        ''' </summary>
        Public Overrides Function VisitNamespaceBlock(node As NamespaceBlockSyntax) As SyntaxNode
            If node.members.Any Then
                ' Add an empty line after the namespace begin if there
                ' is not a namespace declaration as first member
                If node.members(0).Kind <> SyntaxKind.NamespaceBlock Then
                    Me.AddLinebreaksAfterTokenIfNeeded(node.NamespaceStatement.GetLastToken(), 2)
                Else
                    Me.AddLinebreaksAfterTokenIfNeeded(node.NamespaceStatement.GetLastToken(), 1)
                End If

                Me.AddLinebreaksAfterElementsIfNeeded(node.members, 2, 1)
            Else
                Me.AddLinebreaksAfterTokenIfNeeded(node.NamespaceStatement.GetLastToken(), 1)
            End If

            Return MyBase.VisitNamespaceBlock(node)
        End Function

        Public Overrides Function VisitNamespaceStatement(node As NamespaceStatementSyntax) As SyntaxNode
            Dim result As SyntaxNode = MyBase.VisitNamespaceStatement(node)
            _indentationDepth += 1

            Return result
        End Function

        Public Overrides Function VisitNextStatement(node As NextStatementSyntax) As SyntaxNode
            ' next statements with multiple control variables are attached to the inner most for statement,
            ' but it should be indented as it is attached to the outer most one.
            Dim variableCount As Integer = node.ControlVariables.Count
            If variableCount = 0 Then
                variableCount = 1
            End If
            _indentationDepth -= variableCount

            Return MyBase.VisitNextStatement(node)
        End Function

        <ExcludeFromCodeCoverage>
        Public Overrides Function VisitOperatorBlock(node As OperatorBlockSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.BlockStatement.GetLastToken(), 1)
            Me.AddLinebreaksAfterElementsIfNeeded(node.Statements, 1, 1)
            Me.MarkLastStatementIfNeeded(node.Statements)
            Return MyBase.VisitOperatorBlock(node)
        End Function

        <ExcludeFromCodeCoverage>
        Public Overrides Function VisitOperatorStatement(node As OperatorStatementSyntax) As SyntaxNode
            Dim result As SyntaxNode = MyBase.VisitOperatorStatement(node)
            _indentationDepth += 1

            Return result
        End Function

        Public Overrides Function VisitPropertyBlock(node As PropertyBlockSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.PropertyStatement.GetLastToken(), 1)
            Me.AddLinebreaksAfterElementsIfNeeded(node.Accessors, 2, 1)
            Return MyBase.VisitPropertyBlock(node)
        End Function

        Public Overrides Function VisitPropertyStatement(node As PropertyStatementSyntax) As SyntaxNode
            Dim result As SyntaxNode = MyBase.VisitPropertyStatement(node)

            ' only indent if this is a block
            If node.Parent IsNot Nothing AndAlso node.Parent.IsKind(SyntaxKind.PropertyBlock) Then
                _indentationDepth += 1
            End If

            Return result
        End Function

        <ExcludeFromCodeCoverage>
        Public Overrides Function VisitReferenceDirectiveTrivia(node As ReferenceDirectiveTriviaSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.GetLastToken(), 1)

            Return MyBase.VisitReferenceDirectiveTrivia(node)
        End Function

        <ExcludeFromCodeCoverage>
        Public Overrides Function VisitRegionDirectiveTrivia(node As RegionDirectiveTriviaSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.GetLastToken(), 1)

            Return MyBase.VisitRegionDirectiveTrivia(node)
        End Function

        Public Overrides Function VisitSelectBlock(node As SelectBlockSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.SelectStatement.GetLastToken(), 1)

            If Not _lastStatementsInBlocks.Contains(node) Then
                Me.AddLinebreaksAfterTokenIfNeeded(node.EndSelectStatement.GetLastToken(), 2)
            Else
                Me.AddLinebreaksAfterTokenIfNeeded(node.EndSelectStatement.GetLastToken(), 1)
            End If

            Return MyBase.VisitSelectBlock(node)
        End Function

        Public Overrides Function VisitSelectStatement(node As SelectStatementSyntax) As SyntaxNode
            Dim result As SyntaxNode = MyBase.VisitSelectStatement(node)
            _indentationDepth += 1

            Return result
        End Function

        Public Overrides Function VisitStructureBlock(node As StructureBlockSyntax) As SyntaxNode
            Me.VisitTypeBlockSyntax(node)

            Return MyBase.VisitStructureBlock(node)
        End Function

        Public Overrides Function VisitStructureStatement(node As StructureStatementSyntax) As SyntaxNode
            Dim result As SyntaxNode = MyBase.VisitStructureStatement(node)
            _indentationDepth += 1

            Return result
        End Function

        Public Overrides Function VisitSubNewStatement(node As SubNewStatementSyntax) As SyntaxNode
            Dim result As SyntaxNode = MyBase.VisitSubNewStatement(node)
            _indentationDepth += 1

            Return result
        End Function

        Public Overrides Function VisitSyncLockBlock(node As SyncLockBlockSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.SyncLockStatement.GetLastToken(), 1)

            Me.AddLinebreaksAfterElementsIfNeeded(node.Statements, 1, 1)

            If _lastStatementsInBlocks.Contains(node) Then
                Me.AddLinebreaksAfterTokenIfNeeded(node.EndSyncLockStatement.GetLastToken(), 1)
            Else
                Me.AddLinebreaksAfterTokenIfNeeded(node.EndSyncLockStatement.GetLastToken(), 2)
            End If

            Return MyBase.VisitSyncLockBlock(node)
        End Function

        Public Overrides Function VisitSyncLockStatement(node As SyncLockStatementSyntax) As SyntaxNode
            Dim result As SyntaxNode = MyBase.VisitSyncLockStatement(node)
            _indentationDepth += 1

            Return result
        End Function

        ' use leadingTrivia as indentation
        ' use trailingTrivia as separation & newlines if needed
        Public Overrides Function VisitToken(token As SyntaxToken) As SyntaxToken

            ' ignore tokens with no content
            If token.IsKind(SyntaxKind.None) Then
                Return token
            End If

            Try
                Dim newToken As SyntaxToken
                If _useDefaultCasing AndAlso token.IsKeyword() Then
                    newToken = SyntaxFactory.Token(token.Kind)
                Else
                    newToken = token
                End If

                Dim indentationDepth As Integer = Me.GetIndentationDepth()

                ' check if this token is first on this line
                Dim numLineBreaksBefore As Integer = Me.LineBreaksBetween(_previousToken, token)

                Dim needsIndentation As Boolean = numLineBreaksBefore > 0

                ' all line breaks except the first will be leading trivia of this token. The first line break
                ' is trailing trivia of the previous token.
                If numLineBreaksBefore > 0 AndAlso (IsLastTokenOnLine(_previousToken) OrElse _previousToken.ContainsEOLTrivia) Then
                    numLineBreaksBefore -= 1
                End If
                _eolLeadingTriviaCount = 0

                Dim leadingTrivia As SyntaxTriviaList = token.leadingTrivia
                If _previousToken.IsKind(SyntaxKind.GreaterThanToken) AndAlso token.IsKind(SyntaxKind.LessThanToken) Then
                    Do While leadingTrivia.Any AndAlso leadingTrivia(0).IsKind(SyntaxKind.EndOfLineTrivia)
                        leadingTrivia = leadingTrivia.RemoveAt(0)
                    Loop

                End If
                newToken = newToken.WithLeadingTrivia(
                            Me.RewriteTrivia(token,
                                leadingTrivia,
                                indentationDepth,
                                isTrailing:=False,
                                mustBeIndented:=needsIndentation,
                                mustHaveSeparator:=False,
                                lineBreaksAfter:=0,
                                lineBreaksBefore:=numLineBreaksBefore))

                Dim nextToken As SyntaxToken = Me.GetNextRelevantToken(token)

                _afterIndentation = False

                ' we only add one of the line breaks to trivia of this token. The remaining ones will be leading trivia
                ' for the next token
                Dim numLineBreaksAfter As Integer = If(_usePreserveCRLF,
                                                       Me.LineBreaksBetween(token, nextToken),
                                                       If(Me.LineBreaksBetween(token, nextToken) > 0, 1, 0))
                Dim needsSeparatorAfter As Boolean = numLineBreaksAfter <= 0 AndAlso NeedsSeparator(token, nextToken)

                _eolTraiingTriviaCount = 0
                newToken = newToken.WithTrailingTrivia(
                            Me.RewriteTrivia(token,
                                token.TrailingTrivia,
                                depth:=0,
                                isTrailing:=True,
                                mustBeIndented:=False,
                                mustHaveSeparator:=needsSeparatorAfter,
                                lineBreaksAfter:=numLineBreaksAfter,
                                lineBreaksBefore:=0))

                If newToken.IsKind(SyntaxKind.DocumentationCommentLineBreakToken) Then
                    _afterLineBreak = True

                ElseIf newToken.IsKind(SyntaxKind.XmlTextLiteralToken) Then
                    If newToken.TrailingTrivia.Count = 0 AndAlso IsNewLineChar(newToken.ValueText.Last) Then
                        _afterLineBreak = True
                    End If
                End If

                Return newToken
            Finally
                _previousToken = token
            End Try

            Return token
        End Function

        Public Overrides Function VisitTryBlock(node As TryBlockSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.TryStatement.GetLastToken(), 1)
            Me.AddLinebreaksAfterElementsIfNeeded(node.Statements, 1, 1)
            Me.MarkLastStatementIfNeeded(node.Statements)
            If Not _lastStatementsInBlocks.Contains(node) Then
                Me.AddLinebreaksAfterTokenIfNeeded(node.EndTryStatement.GetLastToken(), 2)
            Else
                Me.AddLinebreaksAfterTokenIfNeeded(node.EndTryStatement.GetLastToken(), 1)
            End If

            Return MyBase.VisitTryBlock(node)
        End Function

        Public Overrides Function VisitTryStatement(node As TryStatementSyntax) As SyntaxNode
            Dim result As SyntaxNode = MyBase.VisitTryStatement(node)
            _indentationDepth += 1

            Return result
        End Function

        <ExcludeFromCodeCoverage>
        Public Overrides Function VisitUsingBlock(node As UsingBlockSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.UsingStatement.GetLastToken(), 1)
            Me.AddLinebreaksAfterElementsIfNeeded(node.Statements, 1, 1)
            Me.MarkLastStatementIfNeeded(node.Statements)
            If Not _lastStatementsInBlocks.Contains(node) Then
                Me.AddLinebreaksAfterTokenIfNeeded(node.EndUsingStatement.GetLastToken(), 2)
            Else
                Me.AddLinebreaksAfterTokenIfNeeded(node.EndUsingStatement.GetLastToken(), 1)
            End If

            Return MyBase.VisitUsingBlock(node)
        End Function

        <ExcludeFromCodeCoverage>
        Public Overrides Function VisitUsingStatement(node As UsingStatementSyntax) As SyntaxNode
            Dim result As SyntaxNode = MyBase.VisitUsingStatement(node)
            _indentationDepth += 1

            Return result
        End Function

        Public Overrides Function VisitWhileBlock(node As WhileBlockSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.WhileStatement.GetLastToken(), 1)
            Me.AddLinebreaksAfterElementsIfNeeded(node.Statements, 1, 1)
            Me.MarkLastStatementIfNeeded(node.Statements)
            If Not _lastStatementsInBlocks.Contains(node) Then
                Me.AddLinebreaksAfterTokenIfNeeded(node.EndWhileStatement.GetLastToken(), 2)
            End If

            Return MyBase.VisitWhileBlock(node)
        End Function

        Public Overrides Function VisitWhileStatement(node As WhileStatementSyntax) As SyntaxNode
            Dim result As SyntaxNode = MyBase.VisitWhileStatement(node)
            _indentationDepth += 1

            Return result
        End Function

        <ExcludeFromCodeCoverage>
        Public Overrides Function VisitWithBlock(node As WithBlockSyntax) As SyntaxNode
            Me.AddLinebreaksAfterTokenIfNeeded(node.WithStatement.GetLastToken(), 1)
            Me.AddLinebreaksAfterElementsIfNeeded(node.Statements, 1, 1)
            Me.MarkLastStatementIfNeeded(node.Statements)
            Return MyBase.VisitWithBlock(node)
        End Function

        <ExcludeFromCodeCoverage>
        Public Overrides Function VisitWithStatement(node As WithStatementSyntax) As SyntaxNode
            Dim result As SyntaxNode = MyBase.VisitWithStatement(node)
            _indentationDepth += 1

            Return result
        End Function

    End Class

End Namespace
