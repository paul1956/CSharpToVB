' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports System.Threading
Imports CSharpToVBConverter.ToVisualBasic.CSharpConverter
Imports Microsoft.CodeAnalysis
Imports ProgressReportLibrary
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBConverter.ToVisualBasic

    Public Module LanguageSyntaxNodeExtensions
        Public Property IgnoredIfDepth As Integer

        ''' <summary>
        ''' This function is used where a Token is Followed by a Node followed by a Token
        ''' in the middle of a statement where VB does not allow Directives
        ''' </summary>
        ''' <param name="Node"></param>
        ''' <returns>New Node with valid trivia</returns>
        <Extension>
        Friend Function AdjustNodeTrivia(Of T As VB.VisualBasicSyntaxNode)(Node As T, SeparatorFollows As Boolean) As T
            Dim afterFirstTrivia As Boolean = False
            Dim afterEOL As Boolean = False
            Dim afterLineContinuation As Boolean = False
            Dim afterWhiteSpace As Boolean = False
            Dim finalLeadingTrivia As SyntaxTriviaList
            Dim initialTriviaList As SyntaxTriviaList = Node.GetLeadingTrivia
            For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                Dim trivia As SyntaxTrivia = e.Value
                Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(initialTriviaList, e.index, LookaheadCount:=1)
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
                        afterEOL = False
                        afterWhiteSpace = True
                        finalLeadingTrivia = finalLeadingTrivia.Add(trivia)
                    Case VB.SyntaxKind.EndOfLineTrivia
                        If Not afterFirstTrivia Then
                            afterFirstTrivia = True
                            Continue For
                        End If
                        finalLeadingTrivia = finalLeadingTrivia.Add(trivia)
                        afterWhiteSpace = False
                        If finalLeadingTrivia.Count = 1 OrElse nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            finalLeadingTrivia = finalLeadingTrivia.Add(Factory.Space)
                            finalLeadingTrivia = finalLeadingTrivia.Add(LineContinuation)
                            afterLineContinuation = True
                        Else
                            afterLineContinuation = False
                        End If
                        afterEOL = True
                    Case VB.SyntaxKind.CommentTrivia
                        afterFirstTrivia = True
                        If Not afterLineContinuation OrElse afterEOL Then
                            If Not afterWhiteSpace Then
                                finalLeadingTrivia = finalLeadingTrivia.Add(Factory.Space)
                            End If
                            finalLeadingTrivia = finalLeadingTrivia.Add(LineContinuation)
                            afterLineContinuation = False
                        End If
                        finalLeadingTrivia = finalLeadingTrivia.Add(trivia)
                        If Not nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            finalLeadingTrivia = finalLeadingTrivia.Add(VBEOLTrivia)
                            afterEOL = True
                        End If
                    Case VB.SyntaxKind.DisableWarningDirectiveTrivia, VB.SyntaxKind.EnableWarningDirectiveTrivia,
                     VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.DisabledTextTrivia,
                   VB.SyntaxKind.ElseDirectiveTrivia, VB.SyntaxKind.EndIfDirectiveTrivia
                        finalLeadingTrivia = finalLeadingTrivia.AddRange(DirectiveNotAllowedHere(trivia))
                        afterFirstTrivia = True
                        afterEOL = False
                        afterLineContinuation = False
                        If nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) OrElse nextTrivia.IsNone Then
                            Continue For
                        End If
                        finalLeadingTrivia = finalLeadingTrivia.Add(VBEOLTrivia)
                    Case VB.SyntaxKind.LineContinuationTrivia
                        If Not afterLineContinuation Then
                            finalLeadingTrivia = finalLeadingTrivia.Add(trivia)
                        End If
                        afterEOL = False
                        afterLineContinuation = True
                    Case VB.SyntaxKind.RegionDirectiveTrivia, VB.SyntaxKind.EndRegionDirectiveTrivia
                        afterFirstTrivia = True
                        afterEOL = False
                        afterLineContinuation = False
                        finalLeadingTrivia = finalLeadingTrivia.Add(trivia)
                        If nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) OrElse nextTrivia.IsNone Then
                            Continue For
                        End If
                        finalLeadingTrivia = finalLeadingTrivia.Add(VBEOLTrivia)
                    Case Else
                        Stop
                End Select
            Next
            initialTriviaList = Node.GetTrailingTrivia
            afterWhiteSpace = False
            Dim afterComment As Boolean = False
            afterLineContinuation = False
            Dim afterLinefeed As Boolean = False
            Dim finalTrailingTriviaList As New SyntaxTriviaList
            For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                Dim trivia As SyntaxTrivia = e.Value
                Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(initialTriviaList, e.index, LookaheadCount:=1)
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
                                If SeparatorFollows Then
                                    finalTrailingTriviaList = finalTrailingTriviaList.Add(Factory.Space)
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
                            finalTrailingTriviaList = finalTrailingTriviaList.Add(Factory.Space)
                        End If
                        If Not afterLineContinuation Then
                            finalTrailingTriviaList = finalTrailingTriviaList.Add(LineContinuation)
                            finalTrailingTriviaList = finalTrailingTriviaList.Add(Factory.Space)
                        End If
                        finalTrailingTriviaList = finalTrailingTriviaList.Add(trivia)
                        If Not nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            finalTrailingTriviaList = finalTrailingTriviaList.Add(VBEOLTrivia)
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
            Return Node.With(finalLeadingTrivia, finalTrailingTriviaList)
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
        Friend Function ContainsEOLTrivia(node As VB.VisualBasicSyntaxNode) As Boolean
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
        Friend Function ConvertAndModifyNodeTrivia(Of T As VB.VisualBasicSyntaxNode)(Node As T, NodesOrTokens As List(Of SyntaxNodeOrToken), index As Integer, IsStatement As Boolean) As T
            If NodesOrTokens Is Nothing Then
                Throw New ArgumentNullException(NameOf(NodesOrTokens))
            End If
            Dim afterWhiteSpace As Boolean = False
            Dim afterLineContinuation As Boolean = False
            Dim finalLeadingTrivia As New SyntaxTriviaList
            Dim initialTriviaList As SyntaxTriviaList = NodesOrTokens(index).GetLeadingTrivia.ConvertTriviaList()
            For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(initialTriviaList, e.index, LookaheadCount:=1)

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
                            If index < NodesOrTokens.Count - 1 Then
                                If finalLeadingTrivia.Count = 0 Then
                                    finalLeadingTrivia = finalLeadingTrivia.AddRange({Factory.Space, LineContinuation})
                                End If
                            End If
                        End If
                    Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                        If Not afterWhiteSpace Then
                            finalLeadingTrivia = finalLeadingTrivia.Add(Factory.Space)
                        End If
                        finalLeadingTrivia = finalLeadingTrivia.AddRange({LineContinuation, e.Value})
                        If Not nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            finalLeadingTrivia = finalLeadingTrivia.Add(VBEOLTrivia)
                        End If
                    Case VB.SyntaxKind.DisableWarningDirectiveTrivia, VB.SyntaxKind.EnableWarningDirectiveTrivia
                        Stop
                    Case VB.SyntaxKind.IfDirectiveTrivia
                        finalLeadingTrivia = finalLeadingTrivia.AddRange(DirectiveNotAllowedHere(e.Value))
                    Case VB.SyntaxKind.DisabledTextTrivia
                        finalLeadingTrivia = finalLeadingTrivia.AddRange(DirectiveNotAllowedHere(e.Value))
                    Case VB.SyntaxKind.ElseDirectiveTrivia
                        finalLeadingTrivia = finalLeadingTrivia.AddRange(DirectiveNotAllowedHere(e.Value))
                    Case VB.SyntaxKind.EndIfDirectiveTrivia
                        finalLeadingTrivia = finalLeadingTrivia.AddRange(DirectiveNotAllowedHere(e.Value))
                    Case Else
                        Stop
                End Select
            Next
            initialTriviaList = NodesOrTokens(index).GetTrailingTrivia.ConvertTriviaList()

            Dim finalTrailingTrivia As SyntaxTriviaList
            For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                Select Case e.Value.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        finalTrailingTrivia = finalTrailingTrivia.Add(e.Value)
                    Case VB.SyntaxKind.EndOfLineTrivia
                        ' What to do depends on whats next
                        If index < NodesOrTokens.Count - 1 Then
                            Dim j As Integer
                            Dim newWhiteSpaceString As String = ""
                            If Not e.IsLast Then
                                For j = e.index + 1 To initialTriviaList.Count - 1
                                    If initialTriviaList(j).RawKind = VB.SyntaxKind.WhitespaceTrivia Then
                                        newWhiteSpaceString &= initialTriviaList(j).ToString
                                        e.MoveNext()
                                    Else
                                        Exit For
                                    End If
                                Next
                            End If
                            If (Not IsStatement) OrElse ((Not e.IsLast) AndAlso e.Value.IsComment) Then
                                Dim whitespaceTrivia As SyntaxTrivia = If(newWhiteSpaceString.Length = 0, Factory.Space, Factory.WhitespaceTrivia(newWhiteSpaceString))
                                finalTrailingTrivia = finalTrailingTrivia.AddRange({Factory.Space, LineContinuation, whitespaceTrivia, e.Value})
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
                            finalTrailingTrivia = finalTrailingTrivia.Add(Factory.Space)
                        End If
                        If Not afterLineContinuation Then
                            finalTrailingTrivia = finalTrailingTrivia.Add(LineContinuation)
                            finalTrailingTrivia = finalTrailingTrivia.Add(Factory.Space)
                        End If
                        finalTrailingTrivia = finalTrailingTrivia.Add(e.Value)
                        afterLineContinuation = False
                        afterWhiteSpace = False
                    Case Else
                        Stop
                End Select
            Next
            If Node Is Nothing Then
                Throw New ArgumentNullException(NameOf(Node))
            End If
            Return Node.With(finalLeadingTrivia, finalTrailingTrivia)
        End Function

        <Extension>
        Friend Function DedupLeadingTrivia(node As CS.CSharpSyntaxNode, Keyword As SyntaxToken, attributes As List(Of VBS.AttributeListSyntax), modifiers As IEnumerable(Of SyntaxToken)) As SyntaxTriviaList
            Dim nodeLeadingTrivia As SyntaxTriviaList = node.GetLeadingTrivia.ConvertTriviaList()
            If attributes.Any Then
                If EndsWithSimilarTrivia(nodeLeadingTrivia, attributes(0).GetLeadingTrivia) Then
                    nodeLeadingTrivia = New SyntaxTriviaList
                End If
            ElseIf modifiers.Any Then
                If EndsWithSimilarTrivia(nodeLeadingTrivia, modifiers(0).LeadingTrivia) Then
                    nodeLeadingTrivia = New SyntaxTriviaList
                End If
            ElseIf EndsWithSimilarTrivia(nodeLeadingTrivia, Keyword.LeadingTrivia) Then
                nodeLeadingTrivia = New SyntaxTriviaList
            End If

            Return nodeLeadingTrivia
        End Function

        <Extension>
        Friend Function GetScopingBlock(Node As CS.CSharpSyntaxNode) As CS.CSharpSyntaxNode
            Dim blocknode As CS.CSharpSyntaxNode = Node
            While blocknode IsNot Nothing

                If TypeOf blocknode Is CSS.BlockSyntax Then
                    Return CType(blocknode.Parent, CS.CSharpSyntaxNode)
                End If
                If TypeOf blocknode Is CSS.EventDeclarationSyntax Then
                    Return blocknode
                End If

                If TypeOf blocknode Is CSS.PropertyDeclarationSyntax Then
                    Return blocknode
                End If

                If TypeOf blocknode Is CSS.MethodDeclarationSyntax Then
                    Return blocknode
                End If

                If TypeOf blocknode Is CSS.ClassDeclarationSyntax Then
                    Return blocknode
                End If

                If TypeOf blocknode Is CSS.ConversionOperatorDeclarationSyntax Then
                    Return blocknode
                End If

                If TypeOf blocknode Is CSS.ConstructorDeclarationSyntax Then
                    Return blocknode
                End If

                If TypeOf blocknode Is CSS.EnumDeclarationSyntax Then
                    Return blocknode
                End If

                If TypeOf blocknode Is CSS.StructDeclarationSyntax Then
                    Return blocknode
                End If

                If TypeOf blocknode Is CSS.UsingDirectiveSyntax Then
                    Return blocknode
                End If

                If TypeOf blocknode Is CSS.UsingStatementSyntax Then
                    Return CType(blocknode.Parent, CS.CSharpSyntaxNode)
                End If
                blocknode = CType(blocknode.Parent, CS.CSharpSyntaxNode)

            End While
            Return GetStatementwithIssues(Node)
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
            Return statement.With(newLeadingTrivia, newTrailingTrivia).WithTrailingEOL
        End Function

        Friend Sub RestructureNodesAndSeparators(Of T As VB.VisualBasicSyntaxNode)(ByRef _OpenToken As SyntaxToken, ByRef items As List(Of T), ByRef separators As List(Of SyntaxToken), ByRef _CloseToken As SyntaxToken)
            _OpenToken = _OpenToken.WithModifiedTokenTrivia(LeadingToken:=True, AfterEOL:=False, RequireTrailingSpace:=False)
            For index As Integer = 0 To items.Count - 2
                Dim newItem As T = items(index).AdjustNodeTrivia(SeparatorFollows:=True)
                items(index) = newItem
                Dim newSeparators As SyntaxToken = separators(index).WithModifiedTokenTrivia(LeadingToken:=False, AfterEOL:=False, RequireTrailingSpace:=False)
                separators(index) = newSeparators
            Next
            Dim lastItemEndsWithEOL As Boolean = False
            If items.Any Then
                Dim newItem As T = items.Last.AdjustNodeTrivia(SeparatorFollows:=False)
                items(items.Count - 1) = newItem
                lastItemEndsWithEOL = items.Last.HasTrailingTrivia AndAlso items.Last.GetTrailingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia)
            End If
            Dim newCloseToken As SyntaxToken = _CloseToken.WithModifiedTokenTrivia(LeadingToken:=False, AfterEOL:=lastItemEndsWithEOL, RequireTrailingSpace:=False)
            _CloseToken = newCloseToken.RemoveExtraEOL
        End Sub

        <Extension>
        Friend Function WithModifiedNodeTrailingTrivia(Of T As VB.VisualBasicSyntaxNode)(Node As T, SeparatorFollows As Boolean) As T
            Dim afterComment As Boolean = False
            Dim afterLinefeed As Boolean = False
            Dim afterWhiteSpace As Boolean = False
            Dim finalTrailingTrivia As SyntaxTriviaList
            Dim initialTriviaList As SyntaxTriviaList = Node.GetTrailingTrivia
            For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                Dim trivia As SyntaxTrivia = e.Value
                Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(initialTriviaList, e.index, LookaheadCount:=1)
                Dim afterLineContinuation As Boolean = False
                Select Case trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        If nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            Continue For
                        End If

                        If nextTrivia.IsKind(VB.SyntaxKind.CommentTrivia) OrElse
                            nextTrivia.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                            finalTrailingTrivia = finalTrailingTrivia.Add(trivia)
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
                                finalTrailingTrivia = finalTrailingTrivia.Add(trivia)
                            Else
                                If SeparatorFollows Then
                                    finalTrailingTrivia = finalTrailingTrivia.AddRange(
                                        {Factory.Space, LineContinuation, trivia}
                                        )
                                End If
                            End If
                            afterComment = False
                            afterLinefeed = True
                            afterWhiteSpace = False
                            afterLineContinuation = False
                        End If
                    Case VB.SyntaxKind.CommentTrivia
                        If Not afterWhiteSpace Then
                            finalTrailingTrivia = finalTrailingTrivia.Add(Factory.Space)
                        End If
                        If Not afterLineContinuation Then
                            finalTrailingTrivia = finalTrailingTrivia.AddRange({LineContinuation, Factory.Space})
                        End If
                        finalTrailingTrivia = finalTrailingTrivia.Add(trivia)
                        If Not nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            finalTrailingTrivia = finalTrailingTrivia.Add(VBEOLTrivia)
                            afterLineContinuation = False
                            afterLinefeed = True
                        End If
                        afterComment = True
                        afterWhiteSpace = False
                    Case VB.SyntaxKind.LineContinuationTrivia
                        If finalTrailingTrivia.Last.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                            Continue For
                        End If
                        afterWhiteSpace = False
                        afterLineContinuation = True
                        finalTrailingTrivia = finalTrailingTrivia.Add(LineContinuation)
                    Case Else
                        Stop
                End Select
            Next
            Return Node.WithTrailingTrivia(finalTrailingTrivia)
        End Function

        ''' <summary>
        ''' Entry Point for converting source and new applications
        ''' </summary>
        ''' <param name="SourceTree"></param>
        ''' <param name="SkipAutoGenerated"></param>
        ''' <param name="DefaultVBOptions"></param>
        ''' <param name="pSemanticModel"></param>
        ''' <returns></returns>
        <Extension>
        Public Function DoConversion(SourceTree As CS.CSharpSyntaxNode, pSemanticModel As SemanticModel, DefaultVBOptions As DefaultVBOptions, SkipAutoGenerated As Boolean, ReportException As Action(Of Exception), Progress As IProgress(Of ProgressReport), CancelToken As CancellationToken) As VB.VisualBasicSyntaxNode
            IgnoredIfDepth = 0
            Dim visualBasicSyntaxNode1 As VB.VisualBasicSyntaxNode
            SyncLock s_thisLock
                ClearMarker()
                visualBasicSyntaxNode1 = SourceTree?.Accept(New NodesVisitor(New ConvertRequest(SkipAutoGenerated, Progress, CancelToken), pSemanticModel, DefaultVBOptions, ReportException))
            End SyncLock
            Return visualBasicSyntaxNode1
        End Function

    End Module
End Namespace
