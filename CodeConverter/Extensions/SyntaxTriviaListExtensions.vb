' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports VB = Microsoft.CodeAnalysis.VisualBasic

Module SyntaxTriviaListExtensions

    <Extension>
    Friend Function IndexOf(TriviaList As List(Of SyntaxTrivia), kind As VB.SyntaxKind) As Integer
        If TriviaList Is Nothing Then
            Throw New ArgumentNullException(NameOf(TriviaList))
        End If

        For i As Integer = 0 To TriviaList.Count - 1
            If TriviaList(i).IsKind(kind) Then
                Return i
            End If
        Next
        Return -1
    End Function

    <Extension>
    Friend Function RemoveLineContinuation(triviaList As SyntaxTriviaList) As SyntaxTriviaList
        Return triviaList.ToList.RemoveLineContinuation.ToSyntaxTriviaList
    End Function

    <Extension>
    Friend Function RemoveLineContinuation(triviaList As List(Of SyntaxTrivia)) As List(Of SyntaxTrivia)
        Dim lineContIndex As Integer = triviaList.IndexOf(VB.SyntaxKind.LineContinuationTrivia)
        If lineContIndex = -1 Then Return triviaList
        triviaList.RemoveRange(lineContIndex - 1, 2)
        Return triviaList
    End Function

    ''' <summary>
    ''' Syntax Trivia in any Language
    ''' </summary>
    ''' <param name="TriviaList"></param>
    ''' <returns>True if any Trivia is a Comment or a Directive</returns>
    <Extension>
    Friend Function ContainsCommentOrDirectiveTrivia(TriviaList As IEnumerable(Of SyntaxTrivia)) As Boolean
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
            If t.IsDirective Then
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
    ''' <returns>True if any Trivia is a Comment or a Directive</returns>
    <Extension>
    Public Function ContainsDirectiveTrivia(TriviaList As SyntaxTriviaList, ParamArray Kinds() As VB.SyntaxKind) As Boolean
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
    Public Function ContainsEOLTrivia(TriviaList As SyntaxTriviaList) As Boolean
        For Each t As SyntaxTrivia In TriviaList
            If t.IsEndOfLine Then
                Return True
            End If
        Next
        Return False
    End Function

    <Extension>
    Friend Sub ModifyTrailingTrivia(VB_ModifierTrivia As SyntaxTriviaList, ByRef TrailingTrivia As List(Of SyntaxTrivia))
        For Each t As SyntaxTrivia In VB_ModifierTrivia
            Select Case t.RawKind
                Case VB.SyntaxKind.None
                Case VB.SyntaxKind.WhitespaceTrivia
                    TrailingTrivia.Add(SpaceTrivia)
                Case VB.SyntaxKind.EndOfLineTrivia
                    TrailingTrivia.Add(SpaceTrivia)
                Case VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.ElseDirectiveTrivia, VB.SyntaxKind.ElseIfDirectiveTrivia,
                     VB.SyntaxKind.DisabledTextTrivia, VB.SyntaxKind.EndIfDirectiveTrivia
                    TrailingTrivia.Add(t)
                Case VB.SyntaxKind.CommentTrivia
                    TrailingTrivia.Add(t)
                Case VB.SyntaxKind.DocumentationCommentTrivia
                    TrailingTrivia.Add(t)
                Case Else
                    Stop
            End Select
        Next
    End Sub

    <Extension>
    Public Function ToSyntaxTriviaList(l As IEnumerable(Of SyntaxTrivia)) As SyntaxTriviaList
        Dim NewSyntaxTriviaList As New SyntaxTriviaList
        Return NewSyntaxTriviaList.AddRange(l)
    End Function

    <Extension>
    Public Function ToSyntaxTriviaList(l As List(Of SyntaxTrivia)) As SyntaxTriviaList
        Dim NewSyntaxTriviaList As New SyntaxTriviaList
        Return NewSyntaxTriviaList.AddRange(l)
    End Function

End Module
