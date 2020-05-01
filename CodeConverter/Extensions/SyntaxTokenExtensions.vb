' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices

Imports CSharpToVBCodeConverter.Util

Imports Microsoft.CodeAnalysis

Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory

Namespace CSharpToVBCodeConverter.Util
    Public Module SyntaxTokenExtensions

        <Extension>
        Friend Function Contains(tokens As SyntaxTokenList, Kind As CSharp.SyntaxKind, ByRef Result As Boolean) As Boolean
            Result = tokens.Contains(Kind)
            Return Result
        End Function

        <Extension>
        Friend Function Contains(tokens As SyntaxTokenList, Kind As CSharp.SyntaxKind) As Boolean
            Return tokens.Contains(Function(m As SyntaxToken) m.IsKind(Kind))
        End Function

        <Extension>
        Friend Function WithPrependedLeadingTrivia(token As SyntaxToken, trivia As IEnumerable(Of SyntaxTrivia)) As SyntaxToken
            Return token.WithPrependedLeadingTrivia(trivia.ToSyntaxTriviaList())
        End Function

        <Extension>
        Public Function [With](token As SyntaxToken, leading As List(Of SyntaxTrivia), trailing As List(Of SyntaxTrivia)) As SyntaxToken
            Return token.WithLeadingTrivia(leading).WithTrailingTrivia(trailing)
        End Function

        <Extension>
        Public Function [With](token As SyntaxToken, leading As SyntaxTriviaList, trailing As SyntaxTriviaList) As SyntaxToken
            Return token.WithLeadingTrivia(leading).WithTrailingTrivia(trailing)
        End Function

        ''' <summary>
        ''' Returns the token after this token in the syntax tree.
        ''' </summary>
        ''' <param name="predicate">Delegate applied to each token.  The token is returned if the predicate returns
        ''' true.</param>
        ''' <param name="stepInto">Delegate applied to trivia.  If this delegate is present then trailing trivia is
        ''' included in the search.</param>
        <Extension>
        Public Function GetNextToken(Node As SyntaxToken, predicate As Func(Of SyntaxToken, Boolean), Optional stepInto As Func(Of SyntaxTrivia, Boolean) = Nothing) As SyntaxToken
            If Node = Nothing Then
                Return Nothing
            End If

            Return SyntaxNavigator.s_instance.GetNextToken(Node, predicate, stepInto)
        End Function

        <Extension>
        Public Function IsKind(token As SyntaxToken, ParamArray kinds() As CSharp.SyntaxKind) As Boolean
            Return kinds.Contains(CType(token.RawKind, CSharp.SyntaxKind))
        End Function

        <Extension>
        Public Function IsKind(token As SyntaxToken, ParamArray kinds() As VB.SyntaxKind) As Boolean
            Return kinds.Contains(CType(token.RawKind, VB.SyntaxKind))
        End Function

        <Extension()>
        Public Function RemoveExtraEOL(Token As SyntaxToken) As SyntaxToken
            Dim LeadingTrivia As New List(Of SyntaxTrivia)
            LeadingTrivia.AddRange(Token.LeadingTrivia())
            Select Case LeadingTrivia.Count
                Case 0
                    Return Token
                Case 1
                    If LeadingTrivia.First.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                        Return Token.WithLeadingTrivia(New SyntaxTriviaList)
                    End If
                Case 2
                    Select Case LeadingTrivia.First.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            If LeadingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                Return Token.WithLeadingTrivia(New SyntaxTriviaList)
                            End If
                            Return Token
                        Case VB.SyntaxKind.EndOfLineTrivia
                            If LeadingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                Return Token.WithLeadingTrivia(New SyntaxTriviaList)
                            End If
                            Return Token.WithLeadingTrivia(LeadingTrivia.Last)
                        Case Else
                    End Select
                Case Else
            End Select
            Dim NewLeadingTrivia As New List(Of SyntaxTrivia)
            For Each e As IndexClass(Of SyntaxTrivia) In Token.LeadingTrivia.WithIndex
                Dim trivia As SyntaxTrivia = e.Value
                Dim nextTrivia As SyntaxTrivia = If(Not e.IsLast, Token.LeadingTrivia(e.Index + 1), Nothing)
                If trivia.IsKind(VB.SyntaxKind.WhitespaceTrivia) AndAlso nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                    Continue For
                End If
                If trivia.IsKind(VB.SyntaxKind.CommentTrivia) AndAlso nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                    NewLeadingTrivia.Add(trivia)
                    Continue For
                End If

                If trivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) AndAlso nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                    Continue For
                End If
                NewLeadingTrivia.Add(trivia)
            Next

            Return Token.WithLeadingTrivia(NewLeadingTrivia)
        End Function

        <Extension>
        Public Function WithAppendedTrailingTrivia(token As SyntaxToken, trivia As IEnumerable(Of SyntaxTrivia)) As SyntaxToken
            Return token.WithTrailingTrivia(token.TrailingTrivia.Concat(trivia))
        End Function

        ''' <summary>
        ''' Used for parameters and arguments where blank lines and
        ''' most directives are not allowed
        ''' </summary>
        ''' <param name="Token"></param>
        ''' <param name="LeadingToken"></param>
        ''' <param name="AfterEOL"></param>
        ''' <returns></returns>
        <Extension>
        Public Function WithModifiedTokenTrivia(Token As SyntaxToken, LeadingToken As Boolean, AfterEOL As Boolean) As SyntaxToken
            Dim afterWhiteSpace As Boolean = False
            Dim afterLineContinuation As Boolean = LeadingToken
            Dim initialTriviaList As New List(Of SyntaxTrivia)
            Dim triviaListUBound As Integer
            Dim finalLeadingTriviaList As New List(Of SyntaxTrivia)
            Dim finalTrailingTriviaList As New List(Of SyntaxTrivia)
            If LeadingToken Then
                finalLeadingTriviaList.AddRange(Token.LeadingTrivia)
            Else
                initialTriviaList.AddRange(Token.LeadingTrivia)
                triviaListUBound = initialTriviaList.Count - 1
                For index As Integer = 0 To triviaListUBound
                    Dim trivia As SyntaxTrivia = initialTriviaList(index)
                    Dim NextTrivia As SyntaxTrivia = If(index < triviaListUBound, initialTriviaList(index + 1), Nothing)
                    Select Case trivia.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            AfterEOL = False
                            afterLineContinuation = False
                            afterWhiteSpace = True
                            finalLeadingTriviaList.Add(trivia)
                        Case VB.SyntaxKind.EndOfLineTrivia
                            afterLineContinuation = False
                            afterWhiteSpace = False
                            If AfterEOL Then
                                Continue For
                            End If
                            finalLeadingTriviaList.Add(trivia)
                            ' What I do depends on whats next
                            If index < triviaListUBound Then
                                Dim j As Integer
                                Dim NewWhiteSpaceString As String = ""
                                For j = index + 1 To triviaListUBound
                                    If initialTriviaList(j).IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                                        NewWhiteSpaceString &= initialTriviaList(j).ToString
                                        index += 1
                                    Else
                                        Exit For
                                    End If
                                Next
                                If j < triviaListUBound AndAlso initialTriviaList(j).IsKind(VB.SyntaxKind.CommentTrivia) Then
                                    If String.IsNullOrWhiteSpace(NewWhiteSpaceString) Then
                                        finalLeadingTriviaList.Add(SpaceTrivia)
                                    Else
                                        finalLeadingTriviaList.Add(VBFactory.WhitespaceTrivia(NewWhiteSpaceString))
                                    End If
                                    finalLeadingTriviaList.Add(LineContinuation)
                                    afterLineContinuation = True
                                Else
                                    If Not String.IsNullOrWhiteSpace(NewWhiteSpaceString) Then
                                        finalLeadingTriviaList.Add(VBFactory.WhitespaceTrivia(NewWhiteSpaceString))
                                    End If
                                End If
                            End If
                        Case VB.SyntaxKind.CommentTrivia
                            AfterEOL = False
                            If Not afterWhiteSpace Then
                                finalLeadingTriviaList.Add(SpaceTrivia)
                            End If
                            If Not afterLineContinuation Then
                                finalLeadingTriviaList.Add(LineContinuation)
                                finalLeadingTriviaList.Add(SpaceTrivia)
                            End If
                            finalLeadingTriviaList.Add(trivia)
                            afterLineContinuation = False
                            afterWhiteSpace = False
                        Case VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.DisabledTextTrivia
                            AfterEOL = False
                            finalLeadingTriviaList.AddRange(DirectiveNotAllowedHere(trivia))
                            Select Case NextTrivia.RawKind
                                Case VB.SyntaxKind.None
                                    finalLeadingTriviaList.Add(VBEOLTrivia)
                                Case VB.SyntaxKind.WhitespaceTrivia
                                    finalLeadingTriviaList.Add(VBEOLTrivia)
                                Case VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.DisabledTextTrivia, VB.SyntaxKind.EndIfDirectiveTrivia
                                    finalLeadingTriviaList.Add(VBEOLTrivia)
                                Case Else
                                    Stop
                            End Select
                        Case VB.SyntaxKind.EndIfDirectiveTrivia
                            If Token.LeadingTrivia.ContainsDirectiveTrivia(VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.ElseIfDirectiveTrivia) Then
                                finalLeadingTriviaList.AddRange(DirectiveNotAllowedHere(trivia))
                                Select Case NextTrivia.RawKind
                                    Case VB.SyntaxKind.None
                                        finalLeadingTriviaList.Add(VBEOLTrivia)
                                    Case VB.SyntaxKind.WhitespaceTrivia
                                        finalLeadingTriviaList.Add(VBEOLTrivia)
                                    Case VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.DisabledTextTrivia, VB.SyntaxKind.EndIfDirectiveTrivia
                                        finalLeadingTriviaList.Add(VBEOLTrivia)
                                    Case Else
                                        Stop
                                End Select
                                Continue For
                            End If
                            AfterEOL = False
                            finalTrailingTriviaList.Add(VBEOLTrivia)
                            finalTrailingTriviaList.Add(trivia)
                        Case Else
                            Stop
                    End Select
                Next
            End If
            initialTriviaList.Clear()
            initialTriviaList.AddRange(Token.TrailingTrivia)
            triviaListUBound = initialTriviaList.Count - 1
            afterWhiteSpace = False
            afterLineContinuation = False

            If LeadingToken Then
                For index As Integer = 0 To triviaListUBound
                    Dim trivia As SyntaxTrivia = initialTriviaList(index)
                    Dim nextTrivia As SyntaxTrivia = If(index < triviaListUBound, initialTriviaList(index + 1), New SyntaxTrivia)
                    Select Case trivia.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            If nextTrivia.IsKind(VB.SyntaxKind.CommentTrivia) OrElse
                                nextTrivia.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                                finalTrailingTriviaList.Add(trivia)
                            End If
                        Case VB.SyntaxKind.EndOfLineTrivia
                            ' If leading there is a node after this Token
                            Dim j As Integer
                            Dim NewWhiteSpaceString As String = ""
                            If index < triviaListUBound Then
                                For j = index + 1 To triviaListUBound
                                    If initialTriviaList(j).IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                                        NewWhiteSpaceString &= initialTriviaList(j).ToString
                                        index += 1
                                    Else
                                        Exit For
                                    End If
                                Next
                            End If
                            If j = 0 OrElse j < triviaListUBound AndAlso initialTriviaList(j).IsKind(VB.SyntaxKind.CommentTrivia) Then
                                If Not afterLineContinuation Then
                                    If String.IsNullOrWhiteSpace(NewWhiteSpaceString) Then
                                        finalTrailingTriviaList.Add(SpaceTrivia)
                                    Else
                                        finalTrailingTriviaList.Add(VBFactory.WhitespaceTrivia(NewWhiteSpaceString))
                                    End If
                                    finalTrailingTriviaList.Add(LineContinuation)
                                End If
                                finalTrailingTriviaList.Add(trivia)
                                afterLineContinuation = True
                            Else
                                finalTrailingTriviaList.Add(trivia)
                                If Not String.IsNullOrWhiteSpace(NewWhiteSpaceString) Then
                                    finalTrailingTriviaList.Add(VBFactory.WhitespaceTrivia(NewWhiteSpaceString))
                                End If
                            End If
                        Case VB.SyntaxKind.CommentTrivia
                            If Not afterWhiteSpace Then
                                finalTrailingTriviaList.Add(SpaceTrivia)
                            End If
                            If Not afterLineContinuation Then
                                finalTrailingTriviaList.Add(LineContinuation)
                                finalTrailingTriviaList.Add(SpaceTrivia)
                            End If
                            finalTrailingTriviaList.Add(trivia)
                            afterLineContinuation = False
                            afterWhiteSpace = False
                        Case VB.SyntaxKind.LineContinuationTrivia
                            If finalTrailingTriviaList.Last.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                                Continue For
                            End If
                            afterWhiteSpace = False
                            afterLineContinuation = True
                            finalTrailingTriviaList.Add(LineContinuation)
                        Case Else
                            Stop
                    End Select
                Next
            Else
                finalTrailingTriviaList.AddRange(Token.TrailingTrivia)
            End If
            Return Token.With(finalLeadingTriviaList, finalTrailingTriviaList)
        End Function

        <Extension>
        Public Function WithPrependedLeadingTrivia(token As SyntaxToken, trivia As SyntaxTriviaList) As SyntaxToken
            If trivia.Count = 0 Then
                Return token
            End If

            Return token.WithLeadingTrivia(trivia.Concat(token.LeadingTrivia))
        End Function

    End Module
End Namespace
