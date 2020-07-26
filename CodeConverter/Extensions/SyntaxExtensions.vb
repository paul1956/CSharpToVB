' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports Microsoft.CodeAnalysis
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Public Module SyntaxExtensions

    <Extension>
    Public Function ConvertAndModifyTokenTrivia(Token As SyntaxToken, NodesOrTokens As List(Of SyntaxNodeOrToken), Index As Integer) As SyntaxToken
        If NodesOrTokens Is Nothing Then
            Throw New ArgumentNullException(NameOf(NodesOrTokens))
        End If
        Dim initialTriviaList As List(Of SyntaxTrivia) = ConvertTrivia(NodesOrTokens(Index).GetLeadingTrivia).ToList
        Dim initialTriviaListUBound As Integer = initialTriviaList.Count - 1
        Dim afterWhiteSpace As Boolean = False
        Dim afterLineContinuation As Boolean = False
        Dim finalLeadingTriviaList As New List(Of SyntaxTrivia)
        For initialTriviaIndex As Integer = 0 To initialTriviaListUBound
            Dim Trivia As SyntaxTrivia = initialTriviaList(initialTriviaIndex)
            Select Case Trivia.RawKind
                Case VB.SyntaxKind.WhitespaceTrivia
                    afterLineContinuation = False
                    afterWhiteSpace = True
                    finalLeadingTriviaList.Add(Trivia)
                Case VB.SyntaxKind.EndOfLineTrivia
                    finalLeadingTriviaList.Add(Trivia)
                    afterLineContinuation = False
                    afterWhiteSpace = False
                    ' What I do depends on whats next
                    If initialTriviaIndex < initialTriviaListUBound Then
                        Dim j As Integer
                        Dim newWhiteSpaceString As String = ""
                        For j = initialTriviaIndex + 1 To initialTriviaListUBound
                            If initialTriviaList(j).RawKind = VB.SyntaxKind.WhitespaceTrivia Then
                                newWhiteSpaceString &= initialTriviaList(j).ToString
                                initialTriviaIndex += 1
                            Else
                                Exit For
                            End If
                        Next
                        If j < initialTriviaListUBound AndAlso initialTriviaList(j).IsComment Then
                            If String.IsNullOrWhiteSpace(newWhiteSpaceString) Then
                                finalLeadingTriviaList.Add(SpaceTrivia)
                            Else
                                finalLeadingTriviaList.Add(VBFactory.WhitespaceTrivia(newWhiteSpaceString))
                            End If
                            finalLeadingTriviaList.Add(LineContinuation)
                            afterLineContinuation = True
                        Else
                            If Not String.IsNullOrWhiteSpace(newWhiteSpaceString) Then
                                finalLeadingTriviaList.Add(VBFactory.WhitespaceTrivia(newWhiteSpaceString))
                            End If
                        End If
                    End If
                Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                    If Not afterWhiteSpace Then
                        finalLeadingTriviaList.Add(SpaceTrivia)
                    End If
                    If Not afterLineContinuation Then
                        finalLeadingTriviaList.Add(LineContinuation)
                        finalLeadingTriviaList.Add(SpaceTrivia)
                    End If
                    finalLeadingTriviaList.Add(Trivia)
                    afterLineContinuation = False
                    afterWhiteSpace = False
                Case VB.SyntaxKind.EndIfDirectiveTrivia
                    finalLeadingTriviaList.AddRange(DirectiveNotAllowedHere(Trivia))
                    finalLeadingTriviaList.Add(VBEOLTrivia)
                    afterLineContinuation = False
                    afterWhiteSpace = False
                Case Else
                    Stop
            End Select
        Next
        initialTriviaList.Clear()
        initialTriviaList.AddRange(ConvertTrivia(NodesOrTokens(Index).GetTrailingTrivia))
        Dim FinalTrailingTriviaList As New List(Of SyntaxTrivia)
        For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
            Dim Trivia As SyntaxTrivia = e.Value
            Select Case Trivia.RawKind
                Case VB.SyntaxKind.WhitespaceTrivia
                    FinalTrailingTriviaList.Add(Trivia)
                    afterWhiteSpace = True
                Case VB.SyntaxKind.EndOfLineTrivia
                    FinalTrailingTriviaList.Add(Trivia)
                    afterWhiteSpace = False
                Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                    If Not afterWhiteSpace = True Then
                        FinalTrailingTriviaList.Add(SpaceTrivia)
                    End If
                    FinalTrailingTriviaList.Add(LineContinuation)
                    FinalTrailingTriviaList.Add(Trivia)
                    afterWhiteSpace = False
                Case Else
                    Stop
            End Select
        Next
        Return Token.With(finalLeadingTriviaList, FinalTrailingTriviaList)
    End Function

    <Extension()>
    Public Function IsParentKind(node As SyntaxNode, kind As CS.SyntaxKind) As Boolean
        Return node IsNot Nothing AndAlso node.Parent.IsKind(kind)
    End Function

    ''' <summary>
    ''' Creates a new syntax node with all whitespace and end of line trivia replaced with
    ''' regularly formatted trivia.        ''' </summary>
    ''' <typeparam name="TNode">>The type of the node.</typeparam>
    ''' <param name="node">The node to format.</param>
    ''' <param name="useDefaultCasing"></param>
    ''' <param name="indentation">An optional sequence of whitespace characters that defines a single level of indentation.</param>
    ''' <param name="elasticTrivia"></param>
    ''' <param name="PreserveCRLF"></param>
    ''' <returns></returns>
    <Extension()>
    Public Function NormalizeWhitespaceEx(Of TNode As SyntaxNode)(node As TNode,
                                                                  useDefaultCasing As Boolean,
                                                                  Optional indentation As String = "    ",
                                                                  Optional eol As String = vbCrLf,
                                                                  Optional PreserveCRLF As Boolean = False
                                                                  ) As TNode
        Contracts.Contract.Requires(node IsNot Nothing)
        Return DirectCast(VBS.SyntaxNormalizer.Normalize(node, indentation, eol, useElasticTrivia:=False, useDefaultCasing, PreserveCRLF), TNode)
    End Function

End Module
