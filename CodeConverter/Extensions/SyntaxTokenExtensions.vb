' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

Imports System.Runtime.CompilerServices

Imports Microsoft.CodeAnalysis

Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory

Namespace CSharpToVBCodeConverter.Util
    Public Module SyntaxTokenExtensions

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

            Return SyntaxNavigator.Instance.GetNextToken(Node, predicate, stepInto)
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
            For i As Integer = 0 To Token.LeadingTrivia.Count - 1
                Dim Trivia As SyntaxTrivia = Token.LeadingTrivia(i)
                Dim NextTrivia As SyntaxTrivia = If(i < Token.LeadingTrivia.Count - 1, Token.LeadingTrivia(i + 1), Nothing)
                If Trivia.IsKind(VB.SyntaxKind.WhitespaceTrivia) AndAlso NextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                    Continue For
                End If
                If Trivia.IsKind(VB.SyntaxKind.CommentTrivia) AndAlso NextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                    NewLeadingTrivia.Add(Trivia)
                    Continue For
                End If

                If Trivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) AndAlso NextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                    Continue For
                End If
                NewLeadingTrivia.Add(Trivia)
            Next

            Return Token.WithLeadingTrivia(NewLeadingTrivia)
        End Function

        <Extension>
        Public Function [With](token As SyntaxToken, leading As List(Of SyntaxTrivia), trailing As List(Of SyntaxTrivia)) As SyntaxToken
            Return token.WithLeadingTrivia(leading).WithTrailingTrivia(trailing)
        End Function

        <Extension>
        Public Function [With](token As SyntaxToken, leading As SyntaxTriviaList, trailing As SyntaxTriviaList) As SyntaxToken
            Return token.WithLeadingTrivia(leading).WithTrailingTrivia(trailing)
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
            Dim FinalLeadingTriviaList As New List(Of SyntaxTrivia)
            Dim AfterWhiteSpace As Boolean = False
            Dim AfterLineContinuation As Boolean = LeadingToken
            Dim InitialTriviaList As New List(Of SyntaxTrivia)
            Dim TriviaListUBound As Integer
            If LeadingToken Then
                FinalLeadingTriviaList.AddRange(Token.LeadingTrivia)
            Else
                InitialTriviaList.AddRange(Token.LeadingTrivia)
                TriviaListUBound = InitialTriviaList.Count - 1
                For i As Integer = 0 To TriviaListUBound
                    Dim Trivia As SyntaxTrivia = InitialTriviaList(i)
                    Dim NextTrivia As SyntaxTrivia = If(i < TriviaListUBound, InitialTriviaList(i + 1), Nothing)
                    Select Case Trivia.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            AfterEOL = False
                            AfterLineContinuation = False
                            AfterWhiteSpace = True
                            FinalLeadingTriviaList.Add(Trivia)
                        Case VB.SyntaxKind.EndOfLineTrivia
                            AfterLineContinuation = False
                            AfterWhiteSpace = False
                            If AfterEOL Then
                                Continue For
                            End If
                            FinalLeadingTriviaList.Add(Trivia)
                            ' What I do depends on whats next
                            If i < TriviaListUBound Then
                                Dim j As Integer
                                Dim NewWhiteSpaceString As String = ""
                                For j = i + 1 To TriviaListUBound
                                    If InitialTriviaList(j).IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                                        NewWhiteSpaceString &= InitialTriviaList(j).ToString
                                        i += 1
                                    Else
                                        Exit For
                                    End If
                                Next
                                If j < TriviaListUBound AndAlso InitialTriviaList(j).IsKind(VB.SyntaxKind.CommentTrivia) Then
                                    If NewWhiteSpaceString.IsNotEmptyNullOrWhitespace Then
                                        FinalLeadingTriviaList.Add(VBFactory.WhitespaceTrivia(NewWhiteSpaceString))
                                    Else
                                        FinalLeadingTriviaList.Add(SpaceTrivia)
                                    End If
                                    FinalLeadingTriviaList.Add(LineContinuation)
                                    AfterLineContinuation = True
                                Else
                                    If NewWhiteSpaceString.IsNotEmptyNullOrWhitespace Then
                                        FinalLeadingTriviaList.Add(VBFactory.WhitespaceTrivia(NewWhiteSpaceString))
                                    End If
                                End If
                            End If
                        Case VB.SyntaxKind.CommentTrivia
                            AfterEOL = False
                            If Not AfterWhiteSpace Then
                                FinalLeadingTriviaList.Add(SpaceTrivia)
                            End If
                            If Not AfterLineContinuation Then
                                FinalLeadingTriviaList.Add(LineContinuation)
                                FinalLeadingTriviaList.Add(SpaceTrivia)
                            End If
                            FinalLeadingTriviaList.Add(Trivia)
                            AfterLineContinuation = False
                            AfterWhiteSpace = False
                        Case VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.DisabledTextTrivia, VB.SyntaxKind.EndIfDirectiveTrivia
                            AfterEOL = False
                            FinalLeadingTriviaList.AddRange(DirectiveNotAllowedHere(Trivia))
                            Select Case NextTrivia.RawKind
                                Case VB.SyntaxKind.None
                                    FinalLeadingTriviaList.Add(VB_EOLTrivia)
                                Case VB.SyntaxKind.WhitespaceTrivia
                                    FinalLeadingTriviaList.Add(VB_EOLTrivia)
                                Case VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.DisabledTextTrivia, VB.SyntaxKind.EndIfDirectiveTrivia
                                    FinalLeadingTriviaList.Add(VB_EOLTrivia)
                                Case Else
                                    Stop
                            End Select
                        Case Else
                            Stop
                    End Select
                Next
            End If
            InitialTriviaList.Clear()
            InitialTriviaList.AddRange(Token.TrailingTrivia)
            TriviaListUBound = InitialTriviaList.Count - 1
            AfterWhiteSpace = False
            AfterLineContinuation = False

            Dim FinalTrailingTriviaList As New List(Of SyntaxTrivia)
            If LeadingToken Then
                For i As Integer = 0 To TriviaListUBound
                    Dim Trivia As SyntaxTrivia = InitialTriviaList(i)
                    Dim NextTrivia As SyntaxTrivia = If(i < TriviaListUBound, InitialTriviaList(i + 1), New SyntaxTrivia)
                    Select Case Trivia.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            If NextTrivia.IsKind(VB.SyntaxKind.CommentTrivia) OrElse
                                NextTrivia.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                                FinalTrailingTriviaList.Add(Trivia)
                            End If
                        Case VB.SyntaxKind.EndOfLineTrivia
                            ' If leading there is a node after this Token
                            Dim j As Integer
                            Dim NewWhiteSpaceString As String = ""
                            If i < TriviaListUBound Then
                                For j = i + 1 To TriviaListUBound
                                    If InitialTriviaList(j).IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                                        NewWhiteSpaceString &= InitialTriviaList(j).ToString
                                        i += 1
                                    Else
                                        Exit For
                                    End If
                                Next
                            End If
                            If j = 0 OrElse j < TriviaListUBound AndAlso InitialTriviaList(j).IsKind(VB.SyntaxKind.CommentTrivia) Then
                                If Not AfterLineContinuation Then
                                    If NewWhiteSpaceString.IsNotEmptyNullOrWhitespace Then
                                        FinalTrailingTriviaList.Add(VBFactory.WhitespaceTrivia(NewWhiteSpaceString))
                                    Else
                                        FinalTrailingTriviaList.Add(SpaceTrivia)
                                    End If
                                    FinalTrailingTriviaList.Add(LineContinuation)
                                End If
                                FinalTrailingTriviaList.Add(Trivia)
                                AfterLineContinuation = True
                            Else
                                FinalTrailingTriviaList.Add(Trivia)
                                If NewWhiteSpaceString.IsNotEmptyNullOrWhitespace Then
                                    FinalTrailingTriviaList.Add(VBFactory.WhitespaceTrivia(NewWhiteSpaceString))
                                End If
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
                            AfterLineContinuation = False
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
            Else
                FinalTrailingTriviaList.AddRange(Token.TrailingTrivia)
            End If
            Return Token.With(FinalLeadingTriviaList, FinalTrailingTriviaList)
        End Function

        <Extension>
        Public Function WithPrependedLeadingTrivia(token As SyntaxToken, trivia As SyntaxTriviaList) As SyntaxToken
            If trivia.Count = 0 Then
                Return token
            End If

            Return token.WithLeadingTrivia(trivia.Concat(token.LeadingTrivia))
        End Function

        <Extension>
        Public Function WithPrependedLeadingTrivia(token As SyntaxToken, trivia As IEnumerable(Of SyntaxTrivia)) As SyntaxToken
            Contracts.Contract.Requires(trivia IsNot Nothing)
            Return token.WithPrependedLeadingTrivia(trivia.ToSyntaxTriviaList())
        End Function

    End Module
End Namespace
