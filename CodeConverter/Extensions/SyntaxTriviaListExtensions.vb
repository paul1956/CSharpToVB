' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports System.Text
Imports Microsoft.CodeAnalysis
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic

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
        Friend Function ConvertTriviaList(initialTriviaList As SyntaxTriviaList) As SyntaxTriviaList
            Dim newTriviaList As New List(Of SyntaxTrivia)
            If Not initialTriviaList.Any Then
                Return New SyntaxTriviaList
            End If
            Try
                For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                    Dim trivia As SyntaxTrivia = e.Value
                    Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(initialTriviaList, e.index, LookaheadCount:=1)
                    Select Case trivia.RawKind
                        Case CS.SyntaxKind.MultiLineCommentTrivia
                            Dim lines() As String = trivia.ToFullString.Substring(2).Split(CType(vbLf, Char))
                            For Each line As String In lines
                                If line.EndsWith("*/", StringComparison.Ordinal) Then
                                    newTriviaList.Add(Factory.CommentTrivia($"' {RemoveLeadingSpacesAndStar(line.Substring(0, line.Length - 2))}"))
                                    If trivia.ToFullString.EndsWith(vbLf, StringComparison.Ordinal) Then
                                        newTriviaList.Add(VBEOLTrivia)
                                    End If
                                Else
                                    newTriviaList.Add(Factory.CommentTrivia($"' {RemoveLeadingSpacesAndStar(line)}"))
                                    newTriviaList.Add(VBEOLTrivia)
                                End If
                                If lines.Length = 1 AndAlso (e.IsLast OrElse Not initialTriviaList(e.index + 1).IsEndOfLine) Then
                                    newTriviaList.Add(VBEOLTrivia)
                                End If
                            Next
                        Case CS.SyntaxKind.NullableDirectiveTrivia
                            Dim structuredTrivia As CSS.StructuredTriviaSyntax = DirectCast(trivia.GetStructure, CSS.StructuredTriviaSyntax)
                            Dim nullableDirective As CSS.NullableDirectiveTriviaSyntax = CType(structuredTrivia, CSS.NullableDirectiveTriviaSyntax)
                            newTriviaList.Add(Factory.CommentTrivia($"' TODO: Skipped Null-able Directive {nullableDirective.SettingToken.Text} {nullableDirective.TargetToken.Text}"))
                            newTriviaList.AddRange(nullableDirective.TargetToken.TrailingTrivia.ConvertTriviaList())
                            newTriviaList.AddRange(nullableDirective.EndOfDirectiveToken.TrailingTrivia.ConvertTriviaList())
                        Case CS.SyntaxKind.MultiLineDocumentationCommentTrivia
                            Dim structuredTrivia As CSS.StructuredTriviaSyntax = DirectCast(trivia.GetStructure, CSS.StructuredTriviaSyntax)
                            For Each t1 As SyntaxNode In structuredTrivia.ChildNodes
                                Dim lines() As String = t1.ToFullString.Split(CType(vbLf, Char))
                                For Each line As String In lines
                                    If line.StartsWith("/*", StringComparison.Ordinal) Then
                                        newTriviaList.Add(Factory.CommentTrivia($"' {RemoveLeadingSpacesAndStar(line.Substring(1, line.Length - 1))}"))
                                        newTriviaList.Add(VBEOLTrivia)
                                    Else
                                        newTriviaList.Add(Factory.CommentTrivia($"' {RemoveLeadingSpacesAndStar(line)}"))
                                        newTriviaList.Add(VBEOLTrivia)
                                    End If
                                Next
                            Next

                        Case CS.SyntaxKind.DocumentationCommentExteriorTrivia
                            newTriviaList.Add(Factory.DocumentationCommentExteriorTrivia(trivia.ToString().Replace("///", "'''", StringComparison.Ordinal)))
                        Case Else
                            Dim convertedTrivia As SyntaxTrivia = trivia.ConvertTrivia
                            If convertedTrivia = Nothing Then
                                Continue For
                            End If
                            newTriviaList.Add(convertedTrivia)
                            If trivia.IsKind(CS.SyntaxKind.SingleLineCommentTrivia) Then
                                If Not nextTrivia.IsKind(CS.SyntaxKind.EndOfLineTrivia) Then
                                    newTriviaList.Add(VBEOLTrivia)
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
                        newTrailingTrivia = newTrailingTrivia.Add(Factory.Space)
                    Case VB.SyntaxKind.EndOfLineTrivia
                        newTrailingTrivia = newTrailingTrivia.Add(Factory.Space)
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

    End Module
End Namespace
