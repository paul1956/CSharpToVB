' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports System.Threading
Imports CSharpToVBCodeConverter.ToVisualBasic.CSharpConverter
Imports Microsoft.CodeAnalysis
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBCodeConverter.ToVisualBasic

    Public Module LanguageSyntaxNodeExtensions
        Public Property IgnoredIfDepth As Integer = 0

        ''' <summary>
        '''
        ''' </summary>
        ''' <param name="node"></param>
        ''' <returns>True if any Trivia is a Comment or a Directive</returns>
        <Extension>
        Friend Function ContainsCommentOrDirectiveTrivia(node As VB.VisualBasicSyntaxNode) As Boolean
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
        Friend Function ContainsEOLTrivia(node As VB.VisualBasicSyntaxNode) As Boolean
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
        Friend Function ConvertAndModifyNodeTrivia(Of T As VB.VisualBasicSyntaxNode)(Node As T, NodesOrTokens As List(Of SyntaxNodeOrToken), Index As Integer, IsStatement As Boolean) As T
            If NodesOrTokens Is Nothing Then
                Throw New ArgumentNullException(NameOf(NodesOrTokens))
            End If
            Dim afterWhiteSpace As Boolean = False
            Dim afterLineContinuation As Boolean = False
            Dim finalLeadingTriviaList As New List(Of SyntaxTrivia)
            Dim initialTriviaList As List(Of SyntaxTrivia) = ConvertTrivia(NodesOrTokens(Index).GetLeadingTrivia).ToList
            For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(initialTriviaList, e.Index)

                Select Case e.Value.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        afterLineContinuation = False
                        afterWhiteSpace = True
                        finalLeadingTriviaList.Add(e.Value)
                    Case VB.SyntaxKind.EndOfLineTrivia
                        ' we want to skip any leading trivia
                        If Not e.IsFirst Then
                            finalLeadingTriviaList.Add(e.Value)
                            afterLineContinuation = False
                            afterWhiteSpace = False
                            If Index < NodesOrTokens.Count - 1 Then
                                If finalLeadingTriviaList.Count = 0 Then
                                    finalLeadingTriviaList.Add(SpaceTrivia)
                                    finalLeadingTriviaList.Add(LineContinuation)
                                End If
                            End If
                        End If
                    Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                        If Not afterWhiteSpace Then
                            finalLeadingTriviaList.Add(SpaceTrivia)
                        End If
                        finalLeadingTriviaList.Add(LineContinuation)
                        finalLeadingTriviaList.Add(e.Value)
                        If Not nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            finalLeadingTriviaList.Add(VBEOLTrivia)
                        End If
                    Case VB.SyntaxKind.DisableWarningDirectiveTrivia, VB.SyntaxKind.EnableWarningDirectiveTrivia
                        Stop
                    Case VB.SyntaxKind.IfDirectiveTrivia
                        finalLeadingTriviaList.AddRange(DirectiveNotAllowedHere(e.Value))
                    Case VB.SyntaxKind.DisabledTextTrivia
                        finalLeadingTriviaList.AddRange(DirectiveNotAllowedHere(e.Value))
                    Case VB.SyntaxKind.ElseDirectiveTrivia
                        finalLeadingTriviaList.AddRange(DirectiveNotAllowedHere(e.Value))
                    Case VB.SyntaxKind.EndIfDirectiveTrivia
                        finalLeadingTriviaList.AddRange(DirectiveNotAllowedHere(e.Value))
                    Case Else
                        Stop
                End Select
            Next
            initialTriviaList.Clear()
            initialTriviaList.AddRange(ConvertTrivia(NodesOrTokens(Index).GetTrailingTrivia))

            Dim finalTrailingTriviaList As New List(Of SyntaxTrivia)
            For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                Select Case e.Value.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        finalTrailingTriviaList.Add(e.Value)
                    Case VB.SyntaxKind.EndOfLineTrivia
                        ' What to do depends on whats next
                        If Index < NodesOrTokens.Count - 1 Then
                            Dim j As Integer
                            Dim NewWhiteSpaceString As String = ""
                            If Not e.IsLast Then
                                For j = e.Index + 1 To initialTriviaList.Count - 1
                                    If initialTriviaList(j).RawKind = VB.SyntaxKind.WhitespaceTrivia Then
                                        NewWhiteSpaceString &= initialTriviaList(j).ToString
                                        e.MoveNext()
                                    Else
                                        Exit For
                                    End If
                                Next
                            End If
                            If (Not IsStatement) OrElse ((Not e.IsLast) AndAlso e.Value.IsComment) Then
                                Dim whitespaceTrivia As SyntaxTrivia = If(NewWhiteSpaceString.Length = 0, SpaceTrivia, VBFactory.WhitespaceTrivia(NewWhiteSpaceString))
                                finalTrailingTriviaList.Add(SpaceTrivia)
                                finalTrailingTriviaList.Add(LineContinuation)
                                finalTrailingTriviaList.Add(whitespaceTrivia)
                                finalTrailingTriviaList.Add(e.Value)
                                afterLineContinuation = True
                            Else
                                finalTrailingTriviaList.Add(e.Value)
                                If Not String.IsNullOrWhiteSpace(NewWhiteSpaceString) Then
                                    finalTrailingTriviaList.Add(VBFactory.WhitespaceTrivia(NewWhiteSpaceString))
                                End If
                            End If
                        Else
                            finalTrailingTriviaList.Add(e.Value)
                            afterLineContinuation = False
                            afterWhiteSpace = False
                        End If
                    Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                        If Not afterWhiteSpace Then
                            finalTrailingTriviaList.Add(SpaceTrivia)
                        End If
                        If Not afterLineContinuation Then
                            finalTrailingTriviaList.Add(LineContinuation)
                            finalTrailingTriviaList.Add(SpaceTrivia)
                        End If
                        finalTrailingTriviaList.Add(e.Value)
                        afterLineContinuation = False
                        afterWhiteSpace = False
                    Case Else
                        Stop
                End Select
            Next
            If Node Is Nothing Then
                Throw New ArgumentNullException(NameOf(Node))
            End If
            Return Node.With(finalLeadingTriviaList, finalTrailingTriviaList)
        End Function

        <Extension>
        Friend Function DedupLeadingTrivia(node As CS.CSharpSyntaxNode, Keyword As SyntaxToken, Attributes As List(Of VBS.AttributeListSyntax), Modifiers As List(Of SyntaxToken)) As List(Of SyntaxTrivia)
            Dim NodeLeadingTrivia As New List(Of SyntaxTrivia)
            NodeLeadingTrivia.AddRange(ConvertTrivia(node.GetLeadingTrivia))
            If Attributes.Any Then
                If TriviaIsIdentical(Attributes(0).GetLeadingTrivia, NodeLeadingTrivia) Then
                    NodeLeadingTrivia.Clear()
                End If
            ElseIf Modifiers.Any Then
                If TriviaIsIdentical(Modifiers(0).LeadingTrivia, NodeLeadingTrivia) Then
                    NodeLeadingTrivia.Clear()
                End If
            ElseIf TriviaIsIdentical(Keyword.LeadingTrivia, NodeLeadingTrivia) Then
                NodeLeadingTrivia.Clear()
            End If

            Return NodeLeadingTrivia
        End Function

        <Extension>
        Friend Function IsNotInStructure(node As CS.CSharpSyntaxNode) As Boolean
            Dim StatementWithIssues As CS.CSharpSyntaxNode = node
            While StatementWithIssues IsNot Nothing
                If TypeOf StatementWithIssues Is CSS.StructDeclarationSyntax Then
                    Exit While
                End If

                StatementWithIssues = CType(StatementWithIssues.Parent, CS.CSharpSyntaxNode)
            End While
            If StatementWithIssues Is Nothing Then
                Return True
            End If

            Return False
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

        Friend Sub RestructureNodesAndSeparators(Of T As VB.VisualBasicSyntaxNode)(ByRef _OpenToken As SyntaxToken, ByRef Items As List(Of T), ByRef Separators As List(Of SyntaxToken), ByRef _CloseToken As SyntaxToken)
            Dim TokenLeadingTrivia As New List(Of SyntaxTrivia)
            Dim NewOpenToken As SyntaxToken = _OpenToken.WithModifiedTokenTrivia(LeadingToken:=True, AfterEOL:=False)
            _OpenToken = NewOpenToken
            For index As Integer = 0 To Items.Count - 2
                Dim NewItem As T = Items(index).WithModifiedNodeTrivia(SeparatorFollows:=True)
                Items(index) = NewItem
                Dim newSeparators As SyntaxToken = Separators(index).WithModifiedTokenTrivia(LeadingToken:=False, AfterEOL:=False)
                Separators(index) = newSeparators
            Next
            Dim LastItemEndsWithEOL As Boolean = False
            If Items.Any Then
                Dim NewItem As T = Items.Last.WithModifiedNodeTrivia(SeparatorFollows:=False)
                Items(Items.Count - 1) = NewItem
                LastItemEndsWithEOL = Items.Last.HasTrailingTrivia AndAlso Items.Last.GetTrailingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia)
            End If
            Dim newCloseToken As SyntaxToken = _CloseToken.WithModifiedTokenTrivia(LeadingToken:=False, LastItemEndsWithEOL)
            _CloseToken = newCloseToken.RemoveExtraEOL
        End Sub

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
                Dim NextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(InitialTriviaList, e.Index)
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
                Dim NextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(InitialTriviaList, e.Index)
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
                Dim NextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(InitialTriviaList, e.Index)
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
        ''' Entry Point for converting source and new applications
        ''' </summary>
        ''' <param name="SourceTree"></param>
        ''' <param name="SkipAutoGenerated"></param>
        ''' <param name="DefaultVBOptions"></param>
        ''' <param name="pSemanticModel"></param>
        ''' <returns></returns>
        <Extension>
        Public Function DoConversion(SourceTree As CS.CSharpSyntaxNode, SkipAutoGenerated As Boolean, DefaultVBOptions As DefaultVBOptions, pSemanticModel As SemanticModel, ReportException As Action(Of Exception), Progress As IProgress(Of ProgressReport), CancelToken As CancellationToken) As VB.VisualBasicSyntaxNode
            IgnoredIfDepth = 0
            Dim visualBasicSyntaxNode1 As VB.VisualBasicSyntaxNode
            s_originalRequest = New ConvertRequest(SkipAutoGenerated, Progress, CancelToken)
            SyncLock s_thisLock
                ClearMarker()
                If s_usedIdentifiers.Any Then
                    s_usedStacks.Push(s_usedIdentifiers)
                    s_usedIdentifiers.Clear()
                End If
                visualBasicSyntaxNode1 = SourceTree?.Accept(New NodesVisitor(pSemanticModel, DefaultVBOptions, ReportException))
                If s_usedStacks.Count > 0 Then
                    s_usedIdentifiers = DirectCast(s_usedStacks.Pop, Dictionary(Of String, SymbolTableEntry))
                End If
            End SyncLock
            Return visualBasicSyntaxNode1
        End Function

    End Module
End Namespace
