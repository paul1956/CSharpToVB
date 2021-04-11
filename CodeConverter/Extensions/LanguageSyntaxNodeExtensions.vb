' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports System.Threading
Imports CSharpToVBConverter.CSharpToVBVisitors.CSharpConverter
Imports Microsoft.CodeAnalysis
Imports ProgressReportLibrary
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Public Module LanguageSyntaxNodeExtensions
    Public Property IgnoredIfDepth As Integer

    <Extension>
    Private Function GetWhiteSpaceTrivia(length As Integer) As SyntaxTrivia
        Return If(length = 0, SpaceTrivia, Factory.WhitespaceTrivia(StrDup(length, " "c)))
    End Function

    ''' <summary>
    ''' This function is used where a Token is Followed by a Node followed by a Token
    ''' in the middle of a statement where VB does not allow Directives
    ''' </summary>
    ''' <param name="node"></param>
    ''' <returns>New Node with valid trivia</returns>
    <Extension>
    Friend Function AdjustNodeTrivia(Of T As VB.VisualBasicSyntaxNode)(node As T, separatorFollows As Boolean) As T
        Dim afterFirstTrivia As Boolean = False
        Dim afterEol As Boolean = False
        Dim afterLineContinuation As Boolean = False
        Dim afterWhiteSpace As Boolean = False
        Dim finalLeadingTrivia As SyntaxTriviaList
        Dim initialTriviaList As SyntaxTriviaList = node.GetLeadingTrivia
        For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
            Dim trivia As SyntaxTrivia = e.Value
            Dim nextTrivia As SyntaxTrivia = initialTriviaList.GetForwardTriviaOrDefault(e.Index, lookaheadCount:=1)
            Select Case trivia.RawKind
                Case VB.SyntaxKind.WhitespaceTrivia
                    If nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) OrElse afterLineContinuation Then
                        Continue For
                    ElseIf nextTrivia.IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                        If trivia.Span.IsEmpty OrElse trivia.Span.Length <= nextTrivia.Span.Length Then
                            Continue For
                        Else
                            If nextTrivia.Span.IsEmpty Then
                                e.MoveNext()
                            End If
                        End If
                    End If
                    afterFirstTrivia = True
                    afterEol = False
                    afterWhiteSpace = True
                    finalLeadingTrivia = finalLeadingTrivia.Add(trivia)
                Case VB.SyntaxKind.EndOfLineTrivia
                    If Not afterFirstTrivia Then
                        afterFirstTrivia = True
                        Continue For
                    End If
                    If Not afterEol Then
                        finalLeadingTrivia = finalLeadingTrivia.Add(trivia)
                    End If
                    afterWhiteSpace = False
                    If finalLeadingTrivia.Count = 1 OrElse nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                        finalLeadingTrivia = finalLeadingTrivia.Add(SpaceTrivia)
                        finalLeadingTrivia = finalLeadingTrivia.Add(LineContinuation)
                        afterLineContinuation = True
                        afterEol = False
                    Else
                        afterLineContinuation = False
                        afterEol = True
                    End If
                Case VB.SyntaxKind.CommentTrivia
                    afterFirstTrivia = True
                    If Not afterLineContinuation OrElse afterEol Then
                        If Not afterWhiteSpace Then
                            finalLeadingTrivia = finalLeadingTrivia.Add(SpaceTrivia)
                        End If
                        finalLeadingTrivia = finalLeadingTrivia.Add(LineContinuation)
                        afterLineContinuation = False
                    End If
                    finalLeadingTrivia = finalLeadingTrivia.Add(trivia)
                    If nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                        afterEol = False
                    Else
                        finalLeadingTrivia = finalLeadingTrivia.Add(VbEolTrivia)
                        afterEol = True
                    End If
                Case VB.SyntaxKind.DisableWarningDirectiveTrivia, VB.SyntaxKind.EnableWarningDirectiveTrivia,
                 VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.DisabledTextTrivia,
               VB.SyntaxKind.ElseDirectiveTrivia, VB.SyntaxKind.EndIfDirectiveTrivia
                    finalLeadingTrivia = finalLeadingTrivia.AddRange(trivia.DirectiveNotAllowedHere(afterEol))
                    afterFirstTrivia = True
                    afterLineContinuation = False
                    If nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) OrElse afterEol Then
                        Continue For
                    End If
                    finalLeadingTrivia = finalLeadingTrivia.Add(VbEolTrivia)
                    afterEol = True
                Case VB.SyntaxKind.LineContinuationTrivia
                    If Not afterLineContinuation Then
                        finalLeadingTrivia = finalLeadingTrivia.Add(trivia)
                    End If
                    afterEol = False
                    afterLineContinuation = True
                Case VB.SyntaxKind.RegionDirectiveTrivia, VB.SyntaxKind.EndRegionDirectiveTrivia
                    afterFirstTrivia = True
                    afterEol = False
                    afterLineContinuation = False
                    finalLeadingTrivia = finalLeadingTrivia.Add(trivia)
                    If nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) OrElse nextTrivia.IsNone Then
                        Continue For
                    End If
                    finalLeadingTrivia = finalLeadingTrivia.Add(VbEolTrivia)
            End Select
        Next
        initialTriviaList = node.GetTrailingTrivia
        afterWhiteSpace = False
        Dim afterComment As Boolean = False
        afterLineContinuation = False
        Dim afterLinefeed As Boolean = False
        Dim finalTrailingTriviaList As New SyntaxTriviaList
        For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
            Dim trivia As SyntaxTrivia = e.Value
            Dim nextTrivia As SyntaxTrivia = initialTriviaList.GetForwardTriviaOrDefault(e.Index, lookaheadCount:=1)
            Select Case trivia.RawKind
                Case VB.SyntaxKind.WhitespaceTrivia
                    If nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                        Continue For
                    End If

                    If nextTrivia.IsKind(VB.SyntaxKind.CommentTrivia) OrElse
                        nextTrivia.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                        finalTrailingTriviaList = finalTrailingTriviaList.Add(trivia)
                        afterLinefeed = False
                        afterComment = False
                        afterWhiteSpace = True
                    End If
                Case VB.SyntaxKind.EndOfLineTrivia
                    If nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                        Continue For
                    End If
                    If Not afterLinefeed Then
                        If afterComment OrElse afterLineContinuation Then
                            finalTrailingTriviaList = finalTrailingTriviaList.Add(trivia)
                        Else
                            If separatorFollows Then
                                finalTrailingTriviaList = finalTrailingTriviaList.Add(SpaceTrivia)
                                finalTrailingTriviaList = finalTrailingTriviaList.Add(LineContinuation)
                                finalTrailingTriviaList = finalTrailingTriviaList.Add(trivia)
                            Else
                                finalTrailingTriviaList = finalTrailingTriviaList.Add(trivia)
                            End If
                        End If
                        afterComment = False
                        afterLinefeed = True
                        afterWhiteSpace = False
                        afterLineContinuation = False
                    End If
                Case VB.SyntaxKind.CommentTrivia
                    If Not afterWhiteSpace Then
                        finalTrailingTriviaList = finalTrailingTriviaList.Add(SpaceTrivia)
                    End If
                    If Not afterLineContinuation Then
                        finalTrailingTriviaList = finalTrailingTriviaList.Add(LineContinuation)
                        finalTrailingTriviaList = finalTrailingTriviaList.Add(SpaceTrivia)
                    End If
                    finalTrailingTriviaList = finalTrailingTriviaList.Add(trivia)
                    If Not nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                        finalTrailingTriviaList = finalTrailingTriviaList.Add(VbEolTrivia)
                        afterLineContinuation = False
                        afterLinefeed = True
                    End If
                    afterComment = True
                    afterWhiteSpace = False
                Case VB.SyntaxKind.LineContinuationTrivia
                    If finalTrailingTriviaList.Last.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                        Continue For
                    End If
                    afterWhiteSpace = False
                    afterLineContinuation = True
                    finalTrailingTriviaList = finalTrailingTriviaList.Add(LineContinuation)
                Case Else
                    Stop
            End Select
        Next
        Return node.With(finalLeadingTrivia, finalTrailingTriviaList)
    End Function

    ''' <summary>
    ''' Does node contain any Comments or Directives
    ''' </summary>
    ''' <param name="node"></param>
    ''' <returns>True if any trivia is a Comment or a Directive</returns>
    <Extension>
    Friend Function ContainsCommentOrDirectiveTrivia(node As VB.VisualBasicSyntaxNode) As Boolean
        Dim currentToken As SyntaxToken = node.GetFirstToken
        While currentToken <> Nothing
            If currentToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia OrElse currentToken.TrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                Return True
            End If
            currentToken = currentToken.GetNextToken
        End While

        Return False
    End Function

    <Extension>
    Friend Function ContainsEolTrivia(node As VB.VisualBasicSyntaxNode) As Boolean
        If Not node.HasTrailingTrivia Then
            Return False
        End If

        For Each t As SyntaxTrivia In node.GetTrailingTrivia
            If t.IsEndOfLine Then
                Return True
            End If
        Next
        Return False
    End Function

    <Extension>
    Friend Function ConvertAndModifyNodeTrivia(Of T As VB.VisualBasicSyntaxNode)(node As T, nodesOrTokens As List(Of SyntaxNodeOrToken), index As Integer, isStatement As Boolean) As T
        If nodesOrTokens Is Nothing Then
            Throw New ArgumentNullException(NameOf(nodesOrTokens))
        End If
        Dim afterWhiteSpace As Boolean = False
        Dim afterLineContinuation As Boolean = False
        Dim finalLeadingTrivia As New SyntaxTriviaList
        Dim initialTriviaList As SyntaxTriviaList = nodesOrTokens(index).GetLeadingTrivia.ConvertTriviaList()
        Dim afterEol As Boolean
        For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
            Dim nextTrivia As SyntaxTrivia = initialTriviaList.GetForwardTriviaOrDefault(e.Index, lookaheadCount:=1)

            Select Case e.Value.RawKind
                Case VB.SyntaxKind.WhitespaceTrivia
                    afterLineContinuation = False
                    afterWhiteSpace = True
                    finalLeadingTrivia = finalLeadingTrivia.Add(e.Value)
                Case VB.SyntaxKind.EndOfLineTrivia
                    ' we want to skip any leading trivia
                    If Not e.IsFirst Then
                        finalLeadingTrivia = finalLeadingTrivia.Add(e.Value)
                        afterLineContinuation = False
                        afterWhiteSpace = False
                        If index < nodesOrTokens.Count - 1 Then
                            If finalLeadingTrivia.Count = 0 Then
                                finalLeadingTrivia = finalLeadingTrivia.AddRange(SpaceLineContinue)
                            End If
                        End If
                    End If
                Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                    If Not afterWhiteSpace Then
                        finalLeadingTrivia = finalLeadingTrivia.Add(SpaceTrivia)
                    End If
                    finalLeadingTrivia = finalLeadingTrivia.AddRange({LineContinuation, e.Value})
                    If Not nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                        finalLeadingTrivia = finalLeadingTrivia.Add(VbEolTrivia)
                    End If
                Case VB.SyntaxKind.DisableWarningDirectiveTrivia, VB.SyntaxKind.EnableWarningDirectiveTrivia
                    Stop
                Case VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.ElseDirectiveTrivia, VB.SyntaxKind.EndIfDirectiveTrivia, VB.SyntaxKind.DisabledTextTrivia
                    finalLeadingTrivia = finalLeadingTrivia.AddRange(e.Value.DirectiveNotAllowedHere(afterEol))
                Case Else
                    Stop
            End Select
        Next
        initialTriviaList = nodesOrTokens(index).GetTrailingTrivia.ConvertTriviaList()

        Dim finalTrailingTrivia As SyntaxTriviaList
        For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
            Select Case e.Value.RawKind
                Case VB.SyntaxKind.WhitespaceTrivia
                    finalTrailingTrivia = finalTrailingTrivia.Add(e.Value)
                Case VB.SyntaxKind.EndOfLineTrivia
                    ' What to do depends on whats next
                    If index < nodesOrTokens.Count - 1 Then
                        Dim j As Integer
                        Dim newWhiteSpaceString As String = ""
                        If Not e.IsLast Then
                            For j = e.Index + 1 To initialTriviaList.Count - 1
                                If initialTriviaList(j).RawKind = VB.SyntaxKind.WhitespaceTrivia Then
                                    newWhiteSpaceString &= initialTriviaList(j).ToString
                                    e.MoveNext()
                                Else
                                    Exit For
                                End If
                            Next
                        End If
                        If (Not isStatement) OrElse ((Not e.IsLast) AndAlso e.Value.IsComment) Then
                            finalTrailingTrivia = finalTrailingTrivia.AddRange(SpaceLineContinue)
                            finalTrailingTrivia = finalTrailingTrivia.AddRange({newWhiteSpaceString.Length.GetWhiteSpaceTrivia(),
                                                                               e.Value})
                            afterLineContinuation = True
                        Else
                            finalTrailingTrivia = finalTrailingTrivia.Add(e.Value)
                            If Not String.IsNullOrWhiteSpace(newWhiteSpaceString) Then
                                finalTrailingTrivia = finalTrailingTrivia.Add(Factory.WhitespaceTrivia(newWhiteSpaceString))
                            End If
                        End If
                    Else
                        finalTrailingTrivia = finalTrailingTrivia.Add(e.Value)
                        afterLineContinuation = False
                        afterWhiteSpace = False
                    End If
                Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                    If Not afterWhiteSpace Then
                        finalTrailingTrivia = finalTrailingTrivia.Add(SpaceTrivia)
                    End If
                    If Not afterLineContinuation Then
                        finalTrailingTrivia = finalTrailingTrivia.Add(LineContinuation)
                        finalTrailingTrivia = finalTrailingTrivia.Add(SpaceTrivia)
                    End If
                    finalTrailingTrivia = finalTrailingTrivia.Add(e.Value)
                    afterLineContinuation = False
                    afterWhiteSpace = False
                Case Else
                    Stop
            End Select
        Next
        If node Is Nothing Then
            Throw New ArgumentNullException(NameOf(node))
        End If
        Return node.With(finalLeadingTrivia, finalTrailingTrivia)
    End Function

    <Extension>
    Friend Function DedupLeadingTrivia(node As CS.CSharpSyntaxNode, keyword As SyntaxToken, attributes As List(Of VBS.AttributeListSyntax), modifiers As IEnumerable(Of SyntaxToken)) As SyntaxTriviaList
        Dim nodeLeadingTrivia As SyntaxTriviaList = node.GetLeadingTrivia.ConvertTriviaList()
        If attributes.Any Then
            If EndsWithSimilarTrivia(nodeLeadingTrivia, attributes(0).GetLeadingTrivia) Then
                nodeLeadingTrivia = New SyntaxTriviaList
            End If
        ElseIf modifiers.Any Then
            If EndsWithSimilarTrivia(nodeLeadingTrivia, modifiers(0).LeadingTrivia) Then
                nodeLeadingTrivia = New SyntaxTriviaList
            End If
        ElseIf EndsWithSimilarTrivia(nodeLeadingTrivia, keyword.LeadingTrivia) Then
            nodeLeadingTrivia = New SyntaxTriviaList
        End If

        Return nodeLeadingTrivia
    End Function

    <Extension>
    Friend Function IsNotInStructure(node As CS.CSharpSyntaxNode) As Boolean
        Dim stmtWithIssues As CS.CSharpSyntaxNode = node
        While stmtWithIssues IsNot Nothing
            If TypeOf stmtWithIssues Is CSS.StructDeclarationSyntax Then
                Exit While
            End If

            stmtWithIssues = CType(stmtWithIssues.Parent, CS.CSharpSyntaxNode)
        End While
        If stmtWithIssues Is Nothing Then
            Return True
        End If

        Return False
    End Function

    <Extension>
    Friend Function RelocateDirectivesInLeadingTrivia(Of T As VB.VisualBasicSyntaxNode)(statement As T) As T
        Dim newLeadingTrivia As SyntaxTriviaList = newLeadingTrivia.AddRange(statement.GetLeadingTrivia)
        Dim newTrailingTrivia As SyntaxTriviaList
        For Each trivia As SyntaxTrivia In statement.GetTrailingTrivia
            Select Case trivia.RawKind
                Case VB.SyntaxKind.WhitespaceTrivia, VB.SyntaxKind.EndOfLineTrivia, VB.SyntaxKind.CommentTrivia
                    newTrailingTrivia = newTrailingTrivia.Add(trivia)
                Case VB.SyntaxKind.IfDirectiveTrivia
                    newLeadingTrivia = newLeadingTrivia.Add(trivia)
                Case VB.SyntaxKind.LineContinuationTrivia

                Case Else
                    Stop
            End Select
        Next
        Return statement.With(newLeadingTrivia, newTrailingTrivia).WithTrailingEol
    End Function

    Friend Sub RestructureNodesAndSeparators(Of T As VB.VisualBasicSyntaxNode)(ByRef openToken As SyntaxToken, ByRef items As List(Of T), ByRef separators As List(Of SyntaxToken), ByRef closeToken As SyntaxToken)
        openToken = openToken.WithModifiedTokenTrivia(leadingToken:=True, afterEol:=False, requireTrailingSpace:=False, finalLeadingDirectiveNotAllowed:=False)
        For index As Integer = 0 To items.Count - 2
            Dim newItem As T = items(index).AdjustNodeTrivia(separatorFollows:=True)
            items(index) = newItem
            Dim newSeparators As SyntaxToken = separators(index).WithModifiedTokenTrivia(leadingToken:=False, afterEol:=False, requireTrailingSpace:=False, finalLeadingDirectiveNotAllowed:=False)
            separators(index) = newSeparators
        Next
        Dim lastItemEndsWithEol As Boolean = False
        If items.Any Then
            Dim newItem As T = items.Last.AdjustNodeTrivia(separatorFollows:=False)
            items(items.Count - 1) = newItem
            lastItemEndsWithEol = items.Last.HasTrailingTrivia AndAlso items.Last.GetTrailingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia)
        End If
        Dim newCloseToken As SyntaxToken = closeToken.WithModifiedTokenTrivia(leadingToken:=False, afterEol:=lastItemEndsWithEol, requireTrailingSpace:=False, finalLeadingDirectiveNotAllowed:=True)
        closeToken = newCloseToken.RemoveExtraEol
    End Sub

    ''' <summary>
    ''' Entry Point for converting source and new applications
    ''' </summary>
    ''' <param name="sourceTree"></param>
    ''' <param name="skipAutoGenerated"></param>
    ''' <param name="defaultVbOptions"></param>
    ''' <param name="pSemanticModel"></param>
    ''' <returns></returns>
    <Extension>
    Public Function DoConversion(sourceTree As CS.CSharpSyntaxNode, pSemanticModel As SemanticModel, defaultVbOptions As DefaultVbOptions, skipAutoGenerated As Boolean, reportException As Action(Of Exception), progress As IProgress(Of ProgressReport), cancelToken As CancellationToken) As VB.VisualBasicSyntaxNode
        IgnoredIfDepth = 0
        Dim visualBasicSyntaxNode1 As VB.VisualBasicSyntaxNode
        SyncLock _thisLock
            ClearMarker()
            visualBasicSyntaxNode1 = sourceTree?.Accept(New NodesVisitor(New ConvertRequest(skipAutoGenerated, progress, cancelToken), pSemanticModel, defaultVbOptions, reportException))
        End SyncLock
        Return visualBasicSyntaxNode1
    End Function

End Module
