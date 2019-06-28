Option Explicit On
Option Infer Off
Option Strict On

Imports System.Diagnostics.CodeAnalysis
Imports System.Runtime.CompilerServices
Imports System.Text
Imports System.Threading

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory

Namespace IVisualBasicCode.CodeConverter.Util
    Public Module SyntaxTriviaExtensions

        <Extension()>
        Public Function [Do](Of T)(source As IEnumerable(Of T), action As Action(Of T)) As IEnumerable(Of T)
            If source Is Nothing Then
                Throw New ArgumentNullException(NameOf(source))
            End If

            If action Is Nothing Then
                Throw New ArgumentNullException(NameOf(action))
            End If

            ' perf optimization. try to not use enumerator if possible
            Dim list As IList(Of T) = TryCast(source, IList(Of T))
            If list IsNot Nothing Then
                Dim i As Integer = 0
                Dim count As Integer = list.Count
                Do While i < count
                    action(list(i))
                    i += 1
                Loop
            Else
                For Each value As T In source
                    action(value)
                Next value
            End If

            Return source
        End Function

        <ExcludeFromCodeCoverage>
        <Extension>
        Public Function AsString(trivia As IEnumerable(Of SyntaxTrivia)) As String
            If trivia.Any() Then
                Dim sb As New StringBuilder()
                trivia.Select(Function(t As SyntaxTrivia) t.ToFullString()).Do(Function(s As String) sb.Append(s))
                Return sb.ToString()
            Else
                Return String.Empty
            End If
        End Function

        <Extension>
        Public Function ContainsCommentOrDirectiveTrivia(TriviaList As List(Of SyntaxTrivia)) As Boolean
            If TriviaList.Count = 0 Then
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
                If t.RawKind = VB.SyntaxKind.DocumentationCommentTrivia Then
                    Return True
                End If
                If t.RawKind = VB.SyntaxKind.DocumentationCommentExteriorTrivia Then
                    Return True
                End If

                Stop
            Next
            Return False
        End Function

        ''' <summary>
        ''' Syntax Trivia in any Language
        ''' </summary>
        ''' <param name="TriviaList"></param>
        ''' <returns>True if any Trivia is a Comment or a Directive</returns>
        <Extension>
        Public Function ContainsCommentOrDirectiveTrivia(TriviaList As SyntaxTriviaList) As Boolean
            If TriviaList.Count = 0 Then Return False
            For Each t As SyntaxTrivia In TriviaList
                If t.IsWhitespaceOrEndOfLine Then
                    Continue For
                End If
                If t.RawKind = 0 Then
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
                If t.RawKind = VB.SyntaxKind.DisabledTextTrivia Then
                    Continue For
                End If
                Stop
            Next
            Return False
        End Function

        ''' <summary>
        '''
        ''' </summary>
        ''' <param name="node"></param>
        ''' <returns>True if any Trivia is a Comment or a Directive</returns>
        <Extension>
        Public Function ContainsCommentOrDirectiveTrivia(node As VB.VisualBasicSyntaxNode) As Boolean
            Dim CurrentToken As SyntaxToken = node.GetFirstToken
            While CurrentToken <> Nothing
                If CurrentToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia OrElse CurrentToken.TrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                    Return True
                End If
                CurrentToken = CurrentToken.GetNextToken
            End While

            Return False
        End Function

        <Extension>
        Public Function ContainsCommentTrivia(TriviaList As SyntaxTriviaList) As Boolean
            If TriviaList.Count = 0 Then
                Return False
            End If
            For Each t As SyntaxTrivia In TriviaList
                If t.IsWhitespaceOrEndOfLine Then
                    Continue For
                End If
                If t.IsNone Then
                    Continue For
                End If
                If t.IsComment Then
                    Return True
                End If
                Stop
            Next
            Return False
        End Function

        ''' <summary>
        ''' Syntax Trivia in any Language
        ''' </summary>
        ''' <param name="TriviaList"></param>
        ''' <returns>True if any Trivia is EndIf Directive</returns>
        <Extension>
        Public Function ContainsEndIfTrivia(TriviaList As SyntaxTriviaList) As Boolean
            If TriviaList.Count = 0 Then Return False
            For Each t As SyntaxTrivia In TriviaList
                If t.IsKind(VB.SyntaxKind.EndIfDirectiveTrivia) Then
                    Return True
                End If
            Next
            Return False
        End Function

        <Extension>
        Public Function ContainsEOLTrivia(node As VB.VisualBasicSyntaxNode) As Boolean
            If Not node.HasTrailingTrivia Then
                Return False
            End If
            Dim TriviaList As SyntaxTriviaList = node.GetTrailingTrivia
            For Each t As SyntaxTrivia In TriviaList
                If t.IsEndOfLine Then
                    Return True
                End If
            Next
            Return False
        End Function

        <Extension>
        Public Function ContainsEOLTrivia(Token As SyntaxToken) As Boolean
            If Not Token.HasTrailingTrivia Then
                Return False
            End If
            Dim TriviaList As SyntaxTriviaList = Token.TrailingTrivia
            For Each t As SyntaxTrivia In TriviaList
                If t.IsEndOfLine Then
                    Return True
                End If
            Next
            Return False
        End Function

        <Extension>
        Public Function ContainsEOLTrivia(TriviaList As SyntaxTriviaList) As Boolean
            For Each t As SyntaxTrivia In TriviaList
                If t.IsEndOfLine Then
                    Return True
                End If
            Next
            Return False
        End Function

        <Extension>
        Public Function ContainsLineContinuation(TriviaList As SyntaxTriviaList) As Boolean
            If TriviaList.Count = 0 Then
                Return False
            End If
            For Each t As SyntaxTrivia In TriviaList
                If t.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                    Return True
                End If
            Next
            Return False
        End Function

        Public Function DirectiveNotAllowedHere(Trivia As SyntaxTrivia) As List(Of SyntaxTrivia)
            Dim NewTriviaList As New List(Of SyntaxTrivia)
            Dim LeadingTriviaList As New List(Of SyntaxTrivia) From {
                SpaceTrivia,
                LineContinuation,
                SpaceTrivia
            }
            Dim TriviaAsString As String = ""

            If Trivia.IsKind(VB.SyntaxKind.DisabledTextTrivia) Then
                NewTriviaList.AddRange(LeadingTriviaList)
                NewTriviaList.Add(VBFactory.CommentTrivia($" ' TODO VB does not allow Disabled Text here, original text:"))
                NewTriviaList.Add(VB_EOLTrivia)
                Dim TextStrings() As String = Trivia.ToFullString.Split({vbCrLf}, StringSplitOptions.RemoveEmptyEntries)
                For Each TriviaAsString In TextStrings
                    NewTriviaList.AddRange(LeadingTriviaList)
                    NewTriviaList.Add(VBFactory.CommentTrivia($" ' {TriviaAsString}".Replace("  ", " ").TrimEnd))
                    NewTriviaList.Add(VB_EOLTrivia)
                Next
                If NewTriviaList.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                    NewTriviaList.RemoveAt(NewTriviaList.Count - 1)
                End If
                Return NewTriviaList
            End If

            Select Case Trivia.RawKind
                Case VB.SyntaxKind.IfDirectiveTrivia
                    TriviaAsString = $"#If {Trivia.ToFullString.Substring("#if".Length).Trim.WithoutNewLines(" "c)}"
                Case VB.SyntaxKind.ElseDirectiveTrivia
                    TriviaAsString = $"#Else {Trivia.ToFullString.Substring("#Else".Length).Trim.WithoutNewLines(" "c)}"
                Case VB.SyntaxKind.ElseIfDirectiveTrivia
                    TriviaAsString = $"#ElseIf {Trivia.ToFullString.Substring("#Else If".Length).Trim.WithoutNewLines(" "c)}"
                Case VB.SyntaxKind.EndIfDirectiveTrivia
                    TriviaAsString = $"#EndIf {Trivia.ToFullString.Substring("#End if".Length).Trim.WithoutNewLines(" "c)}"
                Case VB.SyntaxKind.DisableWarningDirectiveTrivia
                    TriviaAsString = $"#Disable Warning Directive {Trivia.ToFullString.Substring("#Disable Warning".Length).Trim.WithoutNewLines(" "c)}"
                Case VB.SyntaxKind.EnableWarningDirectiveTrivia
                    TriviaAsString = $"#Enable Warning Directive {Trivia.ToFullString.Substring("#Enable Warning".Length).Trim.WithoutNewLines(" "c)}"
                Case Else
                    Stop
            End Select
            Dim Msg As String = " ' TODO VB does not allow directives here, original directive: "
            NewTriviaList = New List(Of SyntaxTrivia) From {
                SpaceTrivia,
                LineContinuation,
                VB_EOLTrivia,
                SpaceTrivia,
                LineContinuation,
                SpaceTrivia,
                VBFactory.CommentTrivia($"{Msg}{TriviaAsString}".Replace("  ", " ").TrimEnd)
                }
            Return NewTriviaList
        End Function

        <Extension>
        Public Function FullWidth(trivia As SyntaxTrivia) As Integer
            Return trivia.FullSpan.Length
        End Function

        <Extension>
        Public Function GetLeadingDirective(node As SyntaxToken) As SyntaxTrivia
            If node.HasLeadingTrivia Then
                For Each t As SyntaxTrivia In node.LeadingTrivia
                    If t.IsDirective Then
                        Return t
                    End If
                Next
            End If
            Return New SyntaxTrivia
        End Function

        <Extension>
        Public Function GetPreviousTrivia(trivia As SyntaxTrivia, syntaxTree As SyntaxTree, cancellationToken As CancellationToken, Optional FindInsideTrivia As Boolean = False) As SyntaxTrivia
            Dim span As Text.TextSpan = trivia.FullSpan
            If span.Start = 0 Then
                Return Nothing
            End If

            Return syntaxTree.GetRoot(cancellationToken).FindTrivia(position:=span.Start - 1, findInsideTrivia:=FindInsideTrivia)
        End Function

        <Extension>
        Public Function GetTrailingDirective(node As SyntaxToken) As SyntaxTrivia
            For Each t As SyntaxTrivia In node.TrailingTrivia
                If t.IsDirective Then
                    Return t
                End If
            Next
            Return New SyntaxTrivia
        End Function

        <Extension>
        Public Function IsComment(trivia As SyntaxTrivia) As Boolean
            Return trivia.IsSingleLineComment OrElse trivia.IsMultiLineComment
        End Function

        <Extension>
        Public Function IsCommentOrDirectiveTrivia(t As SyntaxTrivia) As Boolean
            If t.IsSingleLineComment Then
                Return True
            End If
            If t.IsMultiLineComment Then
                Return True
            End If
            If t.IsDirective Then
                Return True
            End If
            Return False
        End Function

        <Extension>
        Public Function IsCompleteMultiLineComment(trivia As SyntaxTrivia) As Boolean
            If trivia.IsKind(CS.SyntaxKind.MultiLineCommentTrivia) Then
                Return False
            End If

            Dim text As String = trivia.ToFullString()
            Return text.Length >= 4 AndAlso text(text.Length - 1) = "/"c AndAlso text(text.Length - 2) = "*"c
        End Function

        <Extension>
        Public Function IsDocComment(trivia As SyntaxTrivia) As Boolean
            Return trivia.IsSingleLineDocComment() OrElse trivia.IsMultiLineDocComment()
        End Function

        <Extension>
        Public Function IsDocumentationCommentTrivia(t As SyntaxTrivia) As Boolean
            If t.IsKind(VisualBasic.SyntaxKind.DocumentationCommentTrivia) Then
                Return True
            End If
            Return False
        End Function

        <Extension>
        Public Function IsElastic(trivia As SyntaxTrivia) As Boolean
            Return trivia.HasAnnotation(SyntaxAnnotation.ElasticAnnotation)
        End Function

        <Extension>
        Public Function IsEndOfLine(trivia As SyntaxTrivia) As Boolean
            Return trivia.IsKind(CS.SyntaxKind.EndOfLineTrivia) OrElse
                trivia.IsKind(VisualBasic.SyntaxKind.EndOfLineTrivia)
        End Function

        <Extension>
        Public Function IsMultiLineComment(trivia As SyntaxTrivia) As Boolean
            Return trivia.IsKind(CS.SyntaxKind.MultiLineCommentTrivia) OrElse
                trivia.IsKind(CS.SyntaxKind.DocumentationCommentExteriorTrivia) OrElse
                trivia.IsKind(CS.SyntaxKind.MultiLineDocumentationCommentTrivia)
        End Function

        <Extension>
        Public Function IsMultiLineDocComment(trivia As SyntaxTrivia) As Boolean
            Return trivia.IsKind(CS.SyntaxKind.MultiLineDocumentationCommentTrivia)
        End Function

        <Extension>
        Public Function IsNone(trivia As SyntaxTrivia) As Boolean
            Return trivia.RawKind = 0
        End Function

        <Extension>
        Public Function IsRegularComment(trivia As SyntaxTrivia) As Boolean
            Return trivia.IsSingleLineComment() OrElse trivia.IsMultiLineComment()
        End Function

        <Extension>
        Public Function IsRegularOrDocComment(trivia As SyntaxTrivia) As Boolean
            Return trivia.IsSingleLineComment() OrElse trivia.IsMultiLineComment() OrElse trivia.IsDocComment()
        End Function

        <Extension>
        Public Function IsSingleLineComment(trivia As SyntaxTrivia) As Boolean
            Return trivia.IsKind(CS.SyntaxKind.SingleLineCommentTrivia) OrElse
                trivia.IsKind(CS.SyntaxKind.SingleLineDocumentationCommentTrivia) OrElse
                trivia.IsKind(VB.SyntaxKind.CommentTrivia)
        End Function

        <Extension>
        Public Function IsSingleLineDocComment(trivia As SyntaxTrivia) As Boolean
            Return trivia.IsKind(CS.SyntaxKind.SingleLineDocumentationCommentTrivia)
        End Function

        <Extension>
        Public Function IsSkippedTokensTrivia(t As SyntaxTrivia) As Boolean
            If t.IsKind(VisualBasic.SyntaxKind.DocumentationCommentTrivia) Then
                Return True
            End If
            Return False
        End Function

        <Extension>
        Public Function IsWhitespace(trivia As SyntaxTrivia) As Boolean
            Return trivia.IsKind(CS.SyntaxKind.WhitespaceTrivia) OrElse
                trivia.IsKind(CS.SyntaxKind.EndOfLineTrivia) OrElse
                trivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) OrElse
                trivia.IsKind(VB.SyntaxKind.WhitespaceTrivia)
        End Function

        <Extension>
        Public Function IsWhitespaceOrEndOfLine(trivia As SyntaxTrivia) As Boolean
            Return trivia.IsKind(CS.SyntaxKind.WhitespaceTrivia) OrElse
                trivia.IsKind(CS.SyntaxKind.EndOfLineTrivia) OrElse
                trivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) OrElse
                trivia.IsKind(VB.SyntaxKind.WhitespaceTrivia)
        End Function

        ' VB
        <ExcludeFromCodeCoverage>
        <Extension>
        Public Function MatchesKind(trivia As SyntaxTrivia, kind As VB.SyntaxKind) As Boolean
            Return trivia.IsKind(kind)
        End Function

        ' VB
        <ExcludeFromCodeCoverage>
        <Extension>
        Public Function MatchesKind(trivia As SyntaxTrivia, kind1 As VB.SyntaxKind, kind2 As VB.SyntaxKind) As Boolean
            Return trivia.IsKind(kind1) OrElse trivia.IsKind(kind2)
        End Function

        ' VB
        <ExcludeFromCodeCoverage>
        <Extension>
        Public Function MatchesKind(trivia As SyntaxTrivia, ParamArray kinds() As VB.SyntaxKind) As Boolean
            For Each kind As VB.SyntaxKind In kinds
                If trivia.IsKind(kind) Then
                    Return True
                End If
            Next
            Return False
        End Function

        ' C#
        <Extension>
        Public Function MatchesKind(trivia As SyntaxTrivia, kind As CS.SyntaxKind) As Boolean
            Return trivia.IsKind(kind)
        End Function

        ' C#
        <Extension>
        Public Function MatchesKind(trivia As SyntaxTrivia, kind1 As CS.SyntaxKind, kind2 As CS.SyntaxKind) As Boolean
            Return trivia.IsKind(kind1) OrElse trivia.IsKind(kind2)
        End Function

        ' C#
        <ExcludeFromCodeCoverage>
        <Extension>
        Public Function MatchesKind(trivia As SyntaxTrivia, ParamArray kinds() As CS.SyntaxKind) As Boolean
            For Each kind As CS.SyntaxKind In kinds
                If trivia.IsKind(kind) Then
                    Return True
                End If
            Next
            Return False
        End Function

        ''' <summary>
        ''' Remove directive trivia
        ''' </summary>
        ''' <param name="node"></param>
        ''' <returns></returns>
        <Extension>
        Public Function RemoveDirectiveTrivia(Of T As VB.Syntax.ArgumentSyntax)(node As T, ByRef FoundEOL As Boolean) As T
            Dim NewLeadingTrivia As New List(Of SyntaxTrivia)
            Dim NewTrailingTrivia As New List(Of SyntaxTrivia)
            For Each trivia As SyntaxTrivia In node.GetLeadingTrivia
                Select Case trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia, VB.SyntaxKind.CommentTrivia
                        NewLeadingTrivia.Add(trivia)
                        FoundEOL = False
                    Case VB.SyntaxKind.EndOfLineTrivia
                        If Not FoundEOL Then
                            NewLeadingTrivia.Add(trivia)
                        End If
                        FoundEOL = True
                    Case VB.SyntaxKind.DisabledTextTrivia,
                         VB.SyntaxKind.IfDirectiveTrivia,
                         VB.SyntaxKind.ElseDirectiveTrivia,
                         VB.SyntaxKind.ElseIfDirectiveTrivia,
                         VB.SyntaxKind.EndIfDirectiveTrivia
                        ' skip
                    Case Else
                        Stop
                End Select
            Next
            FoundEOL = False
            For Each trivia As SyntaxTrivia In node.GetTrailingTrivia
                Select Case trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia, VB.SyntaxKind.CommentTrivia
                        NewTrailingTrivia.Add(trivia)
                        FoundEOL = False
                    Case VB.SyntaxKind.EndOfLineTrivia
                        If Not FoundEOL Then
                            NewTrailingTrivia.Add(trivia)
                            FoundEOL = True
                        End If
                    Case VB.SyntaxKind.DisableWarningDirectiveTrivia,
                         VB.SyntaxKind.IfDirectiveTrivia,
                         VB.SyntaxKind.ElseDirectiveTrivia,
                         VB.SyntaxKind.ElseIfDirectiveTrivia,
                         VB.SyntaxKind.EndIfDirectiveTrivia
                        ' skip
                    Case Else
                        Stop
                End Select
            Next

            Return node.With(NewLeadingTrivia, NewTrailingTrivia)
        End Function

        ''' <summary>
        ''' Remove directive trivia
        ''' </summary>
        ''' <param name="Token"></param>
        ''' <returns></returns>
        <Extension>
        Public Function RemoveDirectiveTrivia(Token As SyntaxToken, ByRef FoundEOL As Boolean) As SyntaxToken
            Dim NewLeadingTrivia As New List(Of SyntaxTrivia)
            Dim NewTrailingTrivia As New List(Of SyntaxTrivia)

            For Each trivia As SyntaxTrivia In Token.LeadingTrivia
                Select Case trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia, VB.SyntaxKind.CommentTrivia
                        NewLeadingTrivia.Add(trivia)
                        FoundEOL = False
                    Case VB.SyntaxKind.EndOfLineTrivia
                        If Not FoundEOL Then
                            NewLeadingTrivia.Add(trivia)
                            FoundEOL = True
                        End If
                    Case VB.SyntaxKind.DisabledTextTrivia,
                        VB.SyntaxKind.IfDirectiveTrivia,
                        VB.SyntaxKind.ElseDirectiveTrivia,
                        VB.SyntaxKind.ElseIfDirectiveTrivia,
                        VB.SyntaxKind.EndIfDirectiveTrivia
                        ' skip
                    Case Else
                        Stop
                End Select
            Next

            For Each trivia As SyntaxTrivia In Token.TrailingTrivia
                Select Case trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia, VB.SyntaxKind.CommentTrivia
                        NewTrailingTrivia.Add(trivia)
                        FoundEOL = False
                    Case VB.SyntaxKind.EndOfLineTrivia
                        If Not FoundEOL Then
                            NewTrailingTrivia.Add(trivia)
                            FoundEOL = True
                        End If
                    Case VB.SyntaxKind.DisableWarningDirectiveTrivia,
                         VB.SyntaxKind.IfDirectiveTrivia,
                         VB.SyntaxKind.ElseDirectiveTrivia,
                         VB.SyntaxKind.ElseIfDirectiveTrivia,
                         VB.SyntaxKind.EndIfDirectiveTrivia
                        ' skip
                    Case Else
                        Stop
                End Select
            Next

            Return Token.With(NewLeadingTrivia, NewTrailingTrivia)
        End Function

        <ExcludeFromCodeCoverage>
        <Extension>
        Public Function ToSyntaxTriviaList(l As IEnumerable(Of SyntaxTrivia)) As SyntaxTriviaList
            Dim NewSyntaxTriviaList As New SyntaxTriviaList
            Return NewSyntaxTriviaList.AddRange(l)
        End Function

        <ExcludeFromCodeCoverage>
        <Extension>
        Public Function ToSyntaxTriviaList(l As List(Of SyntaxTrivia)) As SyntaxTriviaList
            Dim NewSyntaxTriviaList As New SyntaxTriviaList
            Return NewSyntaxTriviaList.AddRange(l)
        End Function

        <Extension>
        Public Function WithoutDirective(node As SyntaxToken) As SyntaxToken
            Dim NewLeadingTrivia As New List(Of SyntaxTrivia)
            For Each t As SyntaxTrivia In node.TrailingTrivia
                If t.IsDirective Then
                    Continue For
                End If
                NewLeadingTrivia.Add(t)
            Next
            Dim NewTrailingTrivia As New List(Of SyntaxTrivia)
            For Each t As SyntaxTrivia In node.TrailingTrivia
                If t.IsDirective Then
                    Continue For
                End If
                NewTrailingTrivia.Add(t)
            Next
            Return node.With(NewLeadingTrivia, NewTrailingTrivia)
        End Function

    End Module
End Namespace