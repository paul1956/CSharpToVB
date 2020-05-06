' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.Runtime.CompilerServices
Imports System.Text

Imports CSharpToVBCodeConverter.DestVisualBasic

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBCodeConverter.Util
    Public Module SyntaxNodeExtensions

        ''' <summary>
        ''' Used at the end of a statement to adjust trivia from two items (like semicolon) the second
        ''' of which will be removed. Directives are allowed.
        ''' </summary>
        ''' <param name="TriviaList"></param>
        ''' <param name="NewTrailingTrivia"></param>
        ''' <param name="FoundEOL"></param>
        ''' <param name="FoundWhiteSpace"></param>
        Private Sub AdjustTrailingTrivia(TriviaList As IEnumerable(Of SyntaxTrivia), NewTrailingTrivia As List(Of SyntaxTrivia), ByRef FoundEOL As Boolean, ByRef FoundWhiteSpace As Boolean)
            For Each e As IndexClass(Of SyntaxTrivia) In TriviaList.WithIndex
                Dim Trivia As SyntaxTrivia = e.Value
                Dim NextTrivia As SyntaxTrivia = If(Not e.IsLast, TriviaList(e.Index + 1), Nothing)
                Select Case Trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        If Not FoundWhiteSpace AndAlso Not NextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            NewTrailingTrivia.Add(Trivia)
                            FoundEOL = False
                            FoundWhiteSpace = True
                        End If
                    Case VB.SyntaxKind.EndOfLineTrivia
                        If Not FoundEOL Then
                            NewTrailingTrivia.Add(Trivia)
                            FoundEOL = True
                        End If
                        FoundWhiteSpace = False
                    Case VB.SyntaxKind.CommentTrivia
                        NewTrailingTrivia.Add(Trivia)
                        If Not NextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            NewTrailingTrivia.Add(VBEOLTrivia)
                            FoundEOL = True
                        End If
                        FoundWhiteSpace = False
                    Case VB.SyntaxKind.EndRegionDirectiveTrivia
                        If Not FoundEOL Then
                            NewTrailingTrivia.Add(VBEOLTrivia)
                        End If
                        NewTrailingTrivia.Add(Trivia)
                        FoundEOL = False
                        FoundWhiteSpace = False
                    Case Else
                        If Trivia.IsDirective Then
                            If Not FoundEOL Then
                                NewTrailingTrivia.Add(VBEOLTrivia)
                            End If
                            NewTrailingTrivia.Add(Trivia)
                            FoundEOL = False
                            FoundWhiteSpace = False
                        Else
                            Stop
                        End If
                End Select
            Next
        End Sub

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
            For charIndex As Integer = 0 To CommentTriviaBody.Length - 1
                If CommentTriviaBody.Substring(charIndex, 1) = "/" Then
                    CommentTriviaBody = CommentTriviaBody.Remove(charIndex, 1).Insert(charIndex, "'")
                Else
                    Exit For
                End If
            Next
            Return CommentTriviaBody
        End Function

        <Extension()>
        Friend Function [With](Of T As SyntaxNode)(node As T, leadingTrivia As IEnumerable(Of SyntaxTrivia), trailingTrivia As IEnumerable(Of SyntaxTrivia)) As T
            Return node.WithLeadingTrivia(leadingTrivia).WithTrailingTrivia(trailingTrivia)
        End Function

        <Extension()>
        Friend Function ConvertDirectiveTrivia(OriginalText As String) As List(Of SyntaxTrivia)
            Dim Text As String = OriginalText.Trim(" "c)
            Dim ResultTrivia As New List(Of SyntaxTrivia)
            Debug.Assert(Text.StartsWith("#", StringComparison.Ordinal), "All directives must start with #")

            If Text.StartsWith("#if", StringComparison.Ordinal) OrElse Text.StartsWith("#elif", StringComparison.Ordinal) Then
                Dim Expression1 As String = Text.Replace("#if ", "", StringComparison.Ordinal).
                        Replace("#elif ", "", StringComparison.Ordinal).
                        Replace("!", "Not ", StringComparison.Ordinal).
                        Replace("==", "=", StringComparison.Ordinal).
                        Replace("!=", "<>", StringComparison.Ordinal).
                        Replace("&&", "And", StringComparison.Ordinal).
                        Replace("||", "Or", StringComparison.Ordinal).
                        Replace("  ", " ", StringComparison.Ordinal).
                        Replace("false", "False", StringComparison.Ordinal).
                        Replace("true", "True", StringComparison.Ordinal).
                        Replace("//", " ' ", StringComparison.Ordinal).
                        Replace("  ", " ", StringComparison.Ordinal)

                Dim Kind As VB.SyntaxKind = If(Text.StartsWith("#if", StringComparison.Ordinal), VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.ElseIfDirectiveTrivia)
                Dim IfOrElseIfKeyword As SyntaxToken = If(Text.StartsWith("#if", StringComparison.Ordinal), IfKeyword, ElseIfKeyword)
                Dim Expr As VBS.ExpressionSyntax = VBFactory.ParseExpression(Expression1)
                Dim IfDirectiveTrivia As VBS.IfDirectiveTriviaSyntax = VBFactory.IfDirectiveTrivia(IfOrElseIfKeyword, Expr)
                ResultTrivia.Add(VBFactory.Trivia(IfDirectiveTrivia))
                Return ResultTrivia
            End If
            If Text.StartsWith("#region", StringComparison.Ordinal) OrElse Text.StartsWith("# region", StringComparison.Ordinal) Then
                ResultTrivia.AddRange(ConvertTrivia(CS.SyntaxFactory.ParseLeadingTrivia(Text)))
                Return ResultTrivia
            End If
            If Text.StartsWith("#endregion", StringComparison.Ordinal) Then
                ResultTrivia.Add(VBFactory.Trivia(VBFactory.EndRegionDirectiveTrivia()))
                Text = Text.Replace("#endregion", "", StringComparison.Ordinal)
                If Text.Length > 0 Then
                    Stop
                End If
                Return ResultTrivia
            End If
            If Text.StartsWith("#else", StringComparison.Ordinal) Then
                Dim ElseKeywordWithTrailingTrivia As SyntaxToken = ElseKeyword.WithTrailingTrivia(ConvertTrivia(CS.SyntaxFactory.ParseTrailingTrivia(Text.Replace("#else", "", StringComparison.Ordinal))))
                ResultTrivia.Add(VBFactory.Trivia(VBFactory.ElseDirectiveTrivia(HashToken, ElseKeywordWithTrailingTrivia)))
                Return ResultTrivia
            End If
            If Text.StartsWith("#endif", StringComparison.Ordinal) Then
                Text = Text.Replace("#endif", "", StringComparison.Ordinal)
                Dim IfKeywordWithTrailingTrivia As SyntaxToken = IfKeyword.WithTrailingTrivia(ConvertTrivia(CS.SyntaxFactory.ParseTrailingTrivia(Text.Replace("#endif", "", StringComparison.Ordinal))))
                ResultTrivia.Add(VBFactory.Trivia(VBFactory.EndIfDirectiveTrivia(HashToken, EndKeyword, IfKeywordWithTrailingTrivia)))
                Return ResultTrivia
            End If
            If Text.StartsWith("#pragma warning", StringComparison.Ordinal) Then
                ResultTrivia.AddRange(ConvertTrivia(CS.SyntaxFactory.ParseLeadingTrivia(Text)))
                Return ResultTrivia
            Else
                Throw New NotImplementedException($"Directive ""{Text}"" Is unknown")
            End If
        End Function

        <Extension()>
        Friend Iterator Function GetAncestors(Of TNode As SyntaxNode)(node As SyntaxNode) As IEnumerable(Of TNode)
            Dim current As SyntaxNode = node.Parent
            While current IsNot Nothing
                If TypeOf current Is TNode Then
                    Yield DirectCast(current, TNode)
                End If

                current = If(TypeOf current Is IStructuredTriviaSyntax, (DirectCast(current, IStructuredTriviaSyntax)).ParentTrivia.Token.Parent, current.Parent)
            End While
        End Function

        <Extension()>
        Friend Function IsKind(node As SyntaxNode, ParamArray kind1() As CS.SyntaxKind) As Boolean
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

        <Extension>
        Friend Function ParentHasSameTrailingTrivia(otherNode As SyntaxNode) As Boolean
            If otherNode.Parent Is Nothing Then
                Return False
            End If
            Return otherNode.Parent.GetLastToken() = otherNode.GetLastToken()
        End Function

        <Extension>
        Friend Function RelocateDirectivesInLeadingTrivia(Of T As VB.VisualBasicSyntaxNode)(Statement As T) As T
            Dim NewLeadingTrivia As New List(Of SyntaxTrivia)
            Dim NewTrailingTrivia As New List(Of SyntaxTrivia)
            NewLeadingTrivia.AddRange(Statement.GetLeadingTrivia)
            For Each Trivia As SyntaxTrivia In Statement.GetTrailingTrivia
                Select Case Trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia, VB.SyntaxKind.EndOfLineTrivia, VB.SyntaxKind.CommentTrivia
                        NewTrailingTrivia.Add(Trivia)
                    Case VB.SyntaxKind.IfDirectiveTrivia
                        NewLeadingTrivia.Add(Trivia)
                    Case Else
                        Stop
                End Select
            Next
            Return Statement.With(NewLeadingTrivia, NewTrailingTrivia).WithTrailingEOL
        End Function

        ''' <summary>
        ''' Remove Leading EOL and convert multiple EOL's to one.
        ''' This is used in statements that are more then 1 line
        ''' </summary>
        ''' <typeparam name="T"></typeparam>
        ''' <param name="node"></param>
        ''' <returns>Node with extra EOL trivia removed</returns>
        <Extension()>
        Friend Function RemoveExtraLeadingEOL(Of T As SyntaxNode)(node As T) As T
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
            For Each e As IndexClass(Of SyntaxTrivia) In node.GetLeadingTrivia.WithIndex
                Dim Trivia As SyntaxTrivia = e.Value
                Dim NextTrivia As SyntaxTrivia = If(Not e.IsLast, node.GetLeadingTrivia(e.Index + 1), Nothing)
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

        <Extension()>
        Friend Function WithAppendedTrailingTrivia(Of T As SyntaxNode)(node As T, ParamArray trivia As SyntaxTrivia()) As T
            If trivia.Length = 0 Then
                Return node
            End If

            Return node.WithAppendedTrailingTrivia(DirectCast(trivia, IEnumerable(Of SyntaxTrivia)))
        End Function

        <Extension>
        Friend Function WithAppendedTriviaFromEndOfDirectiveToken(Of T As SyntaxNode)(node As T, Token As SyntaxToken) As T
            Dim NewTrailingTrivia As New List(Of SyntaxTrivia)
            If Token.HasLeadingTrivia Then
                NewTrailingTrivia.AddRange(ConvertTrivia(Token.LeadingTrivia))
            End If
            If Token.HasTrailingTrivia Then
                NewTrailingTrivia.AddRange(ConvertTrivia(Token.TrailingTrivia))
            End If

            Return node.WithAppendedTrailingTrivia(NewTrailingTrivia).WithTrailingEOL
        End Function

        ''' <summary>
        ''' Merge trailing trivia
        ''' </summary>
        ''' <typeparam name="T"></typeparam>
        ''' <param name="node"></param>
        ''' <param name="TriviaListToMerge"></param>
        ''' <returns></returns>
        <Extension()>
        Friend Function WithMergedTrailingTrivia(Of T As SyntaxNode)(node As T, TriviaListToMerge As IEnumerable(Of SyntaxTrivia)) As T
            If node Is Nothing Then
                Return Nothing
            End If
            If Not TriviaListToMerge?.Any Then
                Return node
            End If
            Dim NodeTrailingTrivia As List(Of SyntaxTrivia) = node.GetTrailingTrivia.ToList
            If Not NodeTrailingTrivia.Any Then
                Return node.WithTrailingTrivia(TriviaListToMerge)
            End If
            Dim NewTrailingTrivia As New List(Of SyntaxTrivia)
            ' Both nodes have trivia
            Dim FoundEOL As Boolean = False
            Dim FoundWhiteSpace As Boolean = False
            AdjustTrailingTrivia(NodeTrailingTrivia, NewTrailingTrivia, FoundEOL, FoundWhiteSpace)
            AdjustTrailingTrivia(TriviaListToMerge, NewTrailingTrivia, FoundEOL, FoundWhiteSpace)
            Return node.WithTrailingTrivia(NewTrailingTrivia)
        End Function

        <Extension>
        Friend Function WithModifiedNodeTrailingTrivia(Of T As VB.VisualBasicSyntaxNode)(Node As T, SeparatorFollows As Boolean) As T
            Dim AfterLineContinuation As Boolean = False
            Dim AfterWhiteSpace As Boolean = False
            Dim FinalLeadingTriviaList As New List(Of SyntaxTrivia)
            Dim InitialTriviaList As List(Of SyntaxTrivia) = Node.GetTrailingTrivia.ToList
            Dim AfterComment As Boolean = False
            Dim AfterLinefeed As Boolean = False
            Dim FinalTrailingTriviaList As New List(Of SyntaxTrivia)
            For Each e As IndexClass(Of SyntaxTrivia) In InitialTriviaList.WithIndex
                Dim Trivia As SyntaxTrivia = e.Value
                Dim NextTrivia As SyntaxTrivia = If(Not e.IsLast, InitialTriviaList(e.Index + 1), VBFactory.ElasticMarker)
                Select Case Trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        If NextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            Continue For
                        End If

                        If NextTrivia.IsKind(VB.SyntaxKind.CommentTrivia) OrElse
                                NextTrivia.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                            FinalTrailingTriviaList.Add(Trivia)
                            AfterLinefeed = False
                            AfterComment = False
                            AfterWhiteSpace = True
                        End If
                    Case VB.SyntaxKind.EndOfLineTrivia
                        If NextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            Continue For
                        End If
                        If Not AfterLinefeed Then
                            If AfterComment OrElse AfterLineContinuation Then
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
                            AfterLineContinuation = False
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
                            FinalTrailingTriviaList.Add(VBEOLTrivia)
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

        ''' <summary>
        ''' This function is used where a Token is Followed by a Node followed by a Token
        ''' in the middle of a statement where VB does not allow Directives
        ''' </summary>
        ''' <param name="Node"></param>
        ''' <returns>New Node with valid Trivia</returns>
        <Extension>
        Friend Function WithModifiedNodeTrivia(Of T As VB.VisualBasicSyntaxNode)(Node As T, SeparatorFollows As Boolean) As T
            Dim AfterFirstTrivia As Boolean = False
            Dim AfterEOL As Boolean = False
            Dim AfterLineContinuation As Boolean = False
            Dim AfterWhiteSpace As Boolean = False
            Dim FinalLeadingTriviaList As New List(Of SyntaxTrivia)
            Dim InitialTriviaList As List(Of SyntaxTrivia) = Node.GetLeadingTrivia.ToList
            For Each e As IndexClass(Of SyntaxTrivia) In InitialTriviaList.WithIndex
                Dim Trivia As SyntaxTrivia = e.Value
                Dim NextTrivia As SyntaxTrivia = If(Not e.IsLast, InitialTriviaList(e.Index + 1), Nothing)
                Select Case Trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        If NextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) OrElse AfterLineContinuation Then
                            Continue For
                        ElseIf NextTrivia.IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                            If Trivia.FullSpan.Length = 0 OrElse Trivia.FullSpan.Length <= NextTrivia.FullSpan.Length Then
                                Continue For
                            Else
                                If NextTrivia.FullSpan.Length = 0 Then
                                    e.MoveNext()
                                End If
                            End If
                        End If
                        AfterFirstTrivia = True
                        AfterEOL = False
                        AfterWhiteSpace = True
                        FinalLeadingTriviaList.Add(Trivia)
                    Case VB.SyntaxKind.EndOfLineTrivia
                        If Not AfterFirstTrivia Then
                            AfterFirstTrivia = True
                            Continue For
                        End If
                        FinalLeadingTriviaList.Add(Trivia)
                        AfterWhiteSpace = False
                        If FinalLeadingTriviaList.Count = 1 OrElse NextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            FinalLeadingTriviaList.Add(SpaceTrivia)
                            FinalLeadingTriviaList.Add(LineContinuation)
                            AfterLineContinuation = True
                        Else
                            AfterLineContinuation = False
                        End If
                        AfterEOL = True
                    Case VB.SyntaxKind.CommentTrivia
                        AfterFirstTrivia = True
                        If Not AfterLineContinuation OrElse AfterEOL Then
                            If Not AfterWhiteSpace Then
                                FinalLeadingTriviaList.Add(SpaceTrivia)
                            End If
                            FinalLeadingTriviaList.Add(LineContinuation)
                            AfterLineContinuation = False
                        End If
                        FinalLeadingTriviaList.Add(Trivia)
                        If Not NextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            FinalLeadingTriviaList.Add(VBEOLTrivia)
                            AfterEOL = True
                        End If
                    Case VB.SyntaxKind.DisableWarningDirectiveTrivia, VB.SyntaxKind.EnableWarningDirectiveTrivia,
                         VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.DisabledTextTrivia,
                       VB.SyntaxKind.ElseDirectiveTrivia, VB.SyntaxKind.EndIfDirectiveTrivia
                        FinalLeadingTriviaList.AddRange(DirectiveNotAllowedHere(Trivia))
                        AfterFirstTrivia = True
                        AfterEOL = False
                        AfterLineContinuation = False
                        If NextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) OrElse NextTrivia.IsNone Then
                            Continue For
                        End If
                        FinalLeadingTriviaList.Add(VBEOLTrivia)
                    Case VB.SyntaxKind.LineContinuationTrivia
                        If Not AfterLineContinuation Then
                            FinalLeadingTriviaList.Add(Trivia)
                        End If
                        AfterEOL = False
                        AfterLineContinuation = True
                    Case VB.SyntaxKind.RegionDirectiveTrivia, VB.SyntaxKind.EndRegionDirectiveTrivia
                        AfterFirstTrivia = True
                        AfterEOL = False
                        AfterLineContinuation = False
                        FinalLeadingTriviaList.Add(Trivia)
                        If NextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) OrElse NextTrivia.IsNone Then
                            Continue For
                        End If
                        FinalLeadingTriviaList.Add(VBEOLTrivia)
                    Case Else
                        Stop
                End Select
            Next
            InitialTriviaList.Clear()
            InitialTriviaList.AddRange(Node.GetTrailingTrivia)
            AfterWhiteSpace = False
            Dim AfterComment As Boolean = False
            AfterLineContinuation = False
            Dim AfterLinefeed As Boolean = False
            Dim FinalTrailingTriviaList As New List(Of SyntaxTrivia)
            For Each e As IndexClass(Of SyntaxTrivia) In InitialTriviaList.WithIndex
                Dim Trivia As SyntaxTrivia = e.Value
                Dim NextTrivia As SyntaxTrivia = If(Not e.IsLast, InitialTriviaList(e.Index + 1), VBFactory.ElasticMarker)
                Select Case Trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        If NextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            Continue For
                        End If

                        If NextTrivia.IsKind(VB.SyntaxKind.CommentTrivia) OrElse
                                NextTrivia.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                            FinalTrailingTriviaList.Add(Trivia)
                            AfterLinefeed = False
                            AfterComment = False
                            AfterWhiteSpace = True
                        End If
                    Case VB.SyntaxKind.EndOfLineTrivia
                        If NextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            Continue For
                        End If
                        If Not AfterLinefeed Then
                            If AfterComment OrElse AfterLineContinuation Then
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
                            AfterLineContinuation = False
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
                            FinalTrailingTriviaList.Add(VBEOLTrivia)
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
        Friend Function WithPrependedLeadingTrivia(Of T As SyntaxNode)(node As T, ParamArray trivia As SyntaxTrivia()) As T
            If trivia.Length = 0 Then
                Return node
            End If
            Dim TriviaList As List(Of SyntaxTrivia) = trivia.ToList
            If TriviaList.Last.IsKind(VB.SyntaxKind.CommentTrivia) Then
                TriviaList.Add(VBEOLTrivia)
            End If
            Return node.WithPrependedLeadingTrivia(TriviaList)
        End Function

        <Extension()>
        Friend Function WithPrependedLeadingTrivia(Of T As SyntaxNode)(node As T, trivia As SyntaxTriviaList) As T
            If trivia.Count = 0 Then
                Return node
            End If
            If trivia.Last.IsKind(VB.SyntaxKind.CommentTrivia) Then
                trivia = trivia.Add(VBEOLTrivia)
            End If
            Return node.WithLeadingTrivia(trivia.Concat(node.GetLeadingTrivia()))
        End Function

        <Extension()>
        Friend Function WithPrependedLeadingTrivia(Of T As SyntaxNode)(node As T, trivia As IEnumerable(Of SyntaxTrivia)) As T
            If trivia Is Nothing Then
                Return node
            End If
            Return node.WithPrependedLeadingTrivia(trivia.ToSyntaxTriviaList())
        End Function

        <Extension>
        Friend Function WithRestructuredingEOLTrivia(Of T As SyntaxNode)(node As T) As T
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

        ''' <summary>
        ''' Make sure the node (usually a statement) ends with an EOL and possibly whitespace
        ''' </summary>
        ''' <typeparam name="T"></typeparam>
        ''' <param name="node"></param>
        ''' <returns></returns>
        <Extension()>
        Friend Function WithTrailingEOL(Of T As SyntaxNode)(node As T) As T
            Dim TrailingTrivia As List(Of SyntaxTrivia) = node.GetTrailingTrivia.ToList
            Dim Count As Integer = TrailingTrivia.Count
            If Count = 0 Then
                Return node.WithTrailingTrivia(VBEOLTrivia)
            End If
            Select Case Count
                Case 1
                    Select Case TrailingTrivia.Last.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia, VB.SyntaxKind.EndOfLineTrivia
                            Return node.WithTrailingTrivia(VBEOLTrivia)
                        Case Else
                            TrailingTrivia.Add(VBEOLTrivia)
                            Return node.WithTrailingTrivia(TrailingTrivia)
                    End Select
                Case 2
                    Select Case TrailingTrivia.First.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            Select Case TrailingTrivia.Last.RawKind
                                Case VB.SyntaxKind.WhitespaceTrivia, VB.SyntaxKind.EndOfLineTrivia
                                    ' Replace Whitespace, Whitespace and Whitespace, EOL with just EOL
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
                                Return node.WithTrailingTrivia(VBEOLTrivia)
                            ElseIf TrailingTrivia.Last.IsDirective Then
                                Return node
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
                    TrailingTrivia.Add(VBEOLTrivia)
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
                                TrailingTrivia.Insert(Count, VBEOLTrivia)
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

        <Extension>
        Public Function ConvertTrivia(t As SyntaxTrivia) As SyntaxTrivia

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
                    'Dim PragmaWarningDirectiveTrivia As CSS.PragmaWarningDirectiveTriviaSyntax = DirectCast(StructuredTrivia, CSS.PragmaWarningDirectiveTriviaSyntax)
                    'Dim ErrorList As New List(Of VBS.IdentifierNameSyntax)
                    'Dim TrailingTriviaStringBuilder As New StringBuilder
                    'For Each i As CSS.ExpressionSyntax In PragmaWarningDirectiveTrivia.ErrorCodes
                    '    Dim ErrorCode As String = i.ToString
                    '    If ErrorCode.IsInteger Then
                    '        ErrorCode = $"cs{ErrorCode}"
                    '    End If
                    '    ErrorList.Add(VBFactory.IdentifierName(ErrorCode))
                    '    For Each Trivial As SyntaxTrivia In i.GetTrailingTrivia
                    '        TrailingTriviaStringBuilder.Append(Trivial.ToString)
                    '    Next
                    'Next
                    'Dim WarningDirectiveTrivia As VBS.DirectiveTriviaSyntax
                    'If PragmaWarningDirectiveTrivia.DisableOrRestoreKeyword.IsKind(CS.SyntaxKind.DisableKeyword) Then
                    '    WarningDirectiveTrivia = VBFactory.DisableWarningDirectiveTrivia(ErrorList.ToArray)
                    'Else
                    '    WarningDirectiveTrivia = VBFactory.EnableWarningDirectiveTrivia(ErrorList.ToArray)
                    'End If
                    'Return VBFactory.CommentTrivia($" ' TODO {WarningDirectiveTrivia.NormalizeWhitespace}{TrailingTriviaStringBuilder.ToString}")
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

        <Extension>
        Public Function ConvertTrivia(TriviaToConvert As IReadOnlyCollection(Of SyntaxTrivia)) As IEnumerable(Of SyntaxTrivia)
            Dim TriviaList As New List(Of SyntaxTrivia)
            If TriviaToConvert Is Nothing Then
                Return TriviaList
            End If
            Try
                For Each e As IndexClass(Of SyntaxTrivia) In TriviaToConvert.WithIndex
                    Dim Trivia As SyntaxTrivia = e.Value
                    Dim NextTrivia As SyntaxTrivia = If(Not e.IsLast, TriviaToConvert(e.Index + 1), Nothing)
                    Select Case Trivia.RawKind
                        Case CS.SyntaxKind.MultiLineCommentTrivia
                            Dim Lines() As String = Trivia.ToFullString.Substring(2).Split(CType(vbLf, Char))
                            For Each line As String In Lines
                                If line.EndsWith("*/", StringComparison.Ordinal) Then
                                    TriviaList.Add(VBFactory.CommentTrivia($"' {RemoveLeadingSpacesStar(line.Substring(0, line.Length - 2))}"))
                                    If Trivia.ToFullString.EndsWith(vbLf, StringComparison.Ordinal) Then
                                        TriviaList.Add(VBEOLTrivia)
                                    End If
                                Else
                                    TriviaList.Add(VBFactory.CommentTrivia($"' {RemoveLeadingSpacesStar(line)}"))
                                    TriviaList.Add(VBEOLTrivia)
                                End If
                                If Lines.Length = 1 AndAlso (e.IsLast OrElse Not TriviaToConvert(e.Index + 1).IsEndOfLine) Then
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
                                        TriviaList.Add(VBFactory.CommentTrivia($"' {RemoveLeadingSpacesStar(line.Substring(1, line.Length - 1))}"))
                                        TriviaList.Add(VBEOLTrivia)
                                    Else
                                        TriviaList.Add(VBFactory.CommentTrivia($"' {RemoveLeadingSpacesStar(line)}"))
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

        <Extension()>
        Public Function GetAncestor(Of TNode As SyntaxNode)(node As SyntaxNode) As TNode
            If node Is Nothing Then
                Return Nothing
            End If

            Return node.GetAncestors(Of TNode)().FirstOrDefault()
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
            If trivia Is Nothing Then
                Return node
            End If
            Return node.WithAppendedTrailingTrivia(trivia.ToSyntaxTriviaList())
        End Function

#Region "WithConvertedTriviaFrom"

        <Extension>
        Friend Function WithConvertedTriviaFrom(Of T As SyntaxNode)(node As T, otherNode As SyntaxNode) As T
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
        Friend Function WithConvertedTriviaFrom(Token As SyntaxToken, otherNode As SyntaxNode) As SyntaxToken
            If otherNode.HasLeadingTrivia Then
                Token = Token.WithLeadingTrivia(ConvertTrivia(otherNode.GetLeadingTrivia))
            End If
            If Not otherNode.HasTrailingTrivia OrElse ParentHasSameTrailingTrivia(otherNode) Then
                Return Token
            End If
            Return Token.WithTrailingTrivia(ConvertTrivia(otherNode.GetTrailingTrivia()))
        End Function

        <Extension>
        Friend Function WithConvertedTriviaFrom(Token As SyntaxToken, otherToken As SyntaxToken) As SyntaxToken
            Try
                If otherToken.HasLeadingTrivia Then
                    Token = Token.WithLeadingTrivia(ConvertTrivia(otherToken.LeadingTrivia).ToList())
                End If
                Return Token.WithTrailingTrivia(ConvertTrivia(otherToken.TrailingTrivia))
            Catch ex As OperationCanceledException
                Throw
            Catch ex As Exception
                Stop
            End Try
        End Function

        <Extension>
        Public Function WithConvertedTriviaFrom(Of T As SyntaxNode)(node As T, otherToken As SyntaxToken) As T
            If node Is Nothing Then
                Throw New ArgumentException($"Parameter {NameOf(node)} Is Nothing")
            End If
            If otherToken.HasLeadingTrivia Then
                node = node.WithLeadingTrivia(ConvertTrivia(otherToken.LeadingTrivia))
            End If
            If Not otherToken.HasTrailingTrivia Then
                Return node
            End If
            Return node.WithTrailingTrivia(ConvertTrivia(otherToken.TrailingTrivia()))
        End Function

#End Region

#Region "WithConvertedLeadingTriviaFrom"

        <Extension>
        Friend Function WithConvertedLeadingTriviaFrom(Of T As SyntaxNode)(node As T, otherNode As SyntaxNode) As T
            If otherNode Is Nothing OrElse Not otherNode.HasLeadingTrivia Then
                Return node
            End If
            Return node.WithLeadingTrivia(ConvertTrivia(otherNode.GetLeadingTrivia()))
        End Function

        <Extension>
        Friend Function WithConvertedLeadingTriviaFrom(Of T As SyntaxNode)(node As T, otherToken As SyntaxToken) As T
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
        Friend Function WithConvertedTrailingTriviaFrom(Of T As SyntaxNode)(node As T, otherNode As SyntaxNode) As T
            If otherNode Is Nothing OrElse Not otherNode.HasTrailingTrivia Then
                Return node
            End If
            If ParentHasSameTrailingTrivia(otherNode) Then
                Return node
            End If
            Return node.WithTrailingTrivia(ConvertTrivia(otherNode.GetTrailingTrivia()))
        End Function

        <Extension>
        Friend Function WithConvertedTrailingTriviaFrom(Of T As SyntaxNode)(node As T, otherToken As SyntaxToken) As T
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
