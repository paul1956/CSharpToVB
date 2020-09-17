' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports System.Threading
Imports CSharpToVBConverter.ToVisualBasic.CSharpConverter
Imports Microsoft.CodeAnalysis
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
        ''' <returns>New Node with valid Trivia</returns>
        <Extension>
        Friend Function AdjustNodeTrivia(Of T As VB.VisualBasicSyntaxNode)(Node As T, SeparatorFollows As Boolean) As T
            Dim AfterFirstTrivia As Boolean = False
            Dim AfterEOL As Boolean = False
            Dim AfterLineContinuation As Boolean = False
            Dim AfterWhiteSpace As Boolean = False
            Dim FinalLeadingTrivia As SyntaxTriviaList
            Dim InitialTriviaList As SyntaxTriviaList = Node.GetLeadingTrivia
            For Each e As IndexClass(Of SyntaxTrivia) In InitialTriviaList.WithIndex
                Dim Trivia As SyntaxTrivia = e.Value
                Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(InitialTriviaList, e.Index, LookaheadCount:=1)
                Select Case Trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        If nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) OrElse AfterLineContinuation Then
                            Continue For
                        ElseIf nextTrivia.IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                            If Trivia.Span.IsEmpty OrElse Trivia.Span.Length <= nextTrivia.Span.Length Then
                                Continue For
                            Else
                                If nextTrivia.Span.IsEmpty Then
                                    e.MoveNext()
                                End If
                            End If
                        End If
                        AfterFirstTrivia = True
                        AfterEOL = False
                        AfterWhiteSpace = True
                        FinalLeadingTrivia = FinalLeadingTrivia.Add(Trivia)
                    Case VB.SyntaxKind.EndOfLineTrivia
                        If Not AfterFirstTrivia Then
                            AfterFirstTrivia = True
                            Continue For
                        End If
                        FinalLeadingTrivia = FinalLeadingTrivia.Add(Trivia)
                        AfterWhiteSpace = False
                        If FinalLeadingTrivia.Count = 1 OrElse nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            FinalLeadingTrivia = FinalLeadingTrivia.Add(VBSpaceTrivia)
                            FinalLeadingTrivia = FinalLeadingTrivia.Add(LineContinuation)
                            AfterLineContinuation = True
                        Else
                            AfterLineContinuation = False
                        End If
                        AfterEOL = True
                    Case VB.SyntaxKind.CommentTrivia
                        AfterFirstTrivia = True
                        If Not AfterLineContinuation OrElse AfterEOL Then
                            If Not AfterWhiteSpace Then
                                FinalLeadingTrivia = FinalLeadingTrivia.Add(VBSpaceTrivia)
                            End If
                            FinalLeadingTrivia = FinalLeadingTrivia.Add(LineContinuation)
                            AfterLineContinuation = False
                        End If
                        FinalLeadingTrivia = FinalLeadingTrivia.Add(Trivia)
                        If Not nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            FinalLeadingTrivia = FinalLeadingTrivia.Add(VBEOLTrivia)
                            AfterEOL = True
                        End If
                    Case VB.SyntaxKind.DisableWarningDirectiveTrivia, VB.SyntaxKind.EnableWarningDirectiveTrivia,
                     VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.DisabledTextTrivia,
                   VB.SyntaxKind.ElseDirectiveTrivia, VB.SyntaxKind.EndIfDirectiveTrivia
                        FinalLeadingTrivia = FinalLeadingTrivia.AddRange(DirectiveNotAllowedHere(Trivia))
                        AfterFirstTrivia = True
                        AfterEOL = False
                        AfterLineContinuation = False
                        If nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) OrElse nextTrivia.IsNone Then
                            Continue For
                        End If
                        FinalLeadingTrivia = FinalLeadingTrivia.Add(VBEOLTrivia)
                    Case VB.SyntaxKind.LineContinuationTrivia
                        If Not AfterLineContinuation Then
                            FinalLeadingTrivia = FinalLeadingTrivia.Add(Trivia)
                        End If
                        AfterEOL = False
                        AfterLineContinuation = True
                    Case VB.SyntaxKind.RegionDirectiveTrivia, VB.SyntaxKind.EndRegionDirectiveTrivia
                        AfterFirstTrivia = True
                        AfterEOL = False
                        AfterLineContinuation = False
                        FinalLeadingTrivia = FinalLeadingTrivia.Add(Trivia)
                        If nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) OrElse nextTrivia.IsNone Then
                            Continue For
                        End If
                        FinalLeadingTrivia = FinalLeadingTrivia.Add(VBEOLTrivia)
                    Case Else
                        Stop
                End Select
            Next
            InitialTriviaList = Node.GetTrailingTrivia
            AfterWhiteSpace = False
            Dim AfterComment As Boolean = False
            AfterLineContinuation = False
            Dim AfterLinefeed As Boolean = False
            Dim FinalTrailingTriviaList As New SyntaxTriviaList
            For Each e As IndexClass(Of SyntaxTrivia) In InitialTriviaList.WithIndex
                Dim Trivia As SyntaxTrivia = e.Value
                Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(InitialTriviaList, e.Index, LookaheadCount:=1)
                Select Case Trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        If nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            Continue For
                        End If

                        If nextTrivia.IsKind(VB.SyntaxKind.CommentTrivia) OrElse
                            nextTrivia.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                            FinalTrailingTriviaList = FinalTrailingTriviaList.Add(Trivia)
                            AfterLinefeed = False
                            AfterComment = False
                            AfterWhiteSpace = True
                        End If
                    Case VB.SyntaxKind.EndOfLineTrivia
                        If nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            Continue For
                        End If
                        If Not AfterLinefeed Then
                            If AfterComment OrElse AfterLineContinuation Then
                                FinalTrailingTriviaList = FinalTrailingTriviaList.Add(Trivia)
                            Else
                                If SeparatorFollows Then
                                    FinalTrailingTriviaList = FinalTrailingTriviaList.Add(VBSpaceTrivia)
                                    FinalTrailingTriviaList = FinalTrailingTriviaList.Add(LineContinuation)
                                    FinalTrailingTriviaList = FinalTrailingTriviaList.Add(Trivia)
                                End If
                            End If
                            AfterComment = False
                            AfterLinefeed = True
                            AfterWhiteSpace = False
                            AfterLineContinuation = False
                        End If
                    Case VB.SyntaxKind.CommentTrivia
                        If Not AfterWhiteSpace Then
                            FinalTrailingTriviaList = FinalTrailingTriviaList.Add(VBSpaceTrivia)
                        End If
                        If Not AfterLineContinuation Then
                            FinalTrailingTriviaList = FinalTrailingTriviaList.Add(LineContinuation)
                            FinalTrailingTriviaList = FinalTrailingTriviaList.Add(VBSpaceTrivia)
                        End If
                        FinalTrailingTriviaList = FinalTrailingTriviaList.Add(Trivia)
                        If Not nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            FinalTrailingTriviaList = FinalTrailingTriviaList.Add(VBEOLTrivia)
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
                        FinalTrailingTriviaList = FinalTrailingTriviaList.Add(LineContinuation)
                    Case Else
                        Stop
                End Select
            Next
            Return Node.With(FinalLeadingTrivia, FinalTrailingTriviaList)
        End Function

        ''' <summary>
        ''' Does node contain any Comments or Directives
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

            For Each t As SyntaxTrivia In node.GetTrailingTrivia
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
            Dim finalLeadingTrivia As New SyntaxTriviaList
            Dim initialTriviaList As SyntaxTriviaList = NodesOrTokens(Index).GetLeadingTrivia.ConvertTriviaList()
            For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(initialTriviaList, e.Index, LookaheadCount:=1)

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
                            If Index < NodesOrTokens.Count - 1 Then
                                If finalLeadingTrivia.Count = 0 Then
                                    finalLeadingTrivia = finalLeadingTrivia.AddRange({VBSpaceTrivia, LineContinuation})
                                End If
                            End If
                        End If
                    Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                        If Not afterWhiteSpace Then
                            finalLeadingTrivia = finalLeadingTrivia.Add(VBSpaceTrivia)
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
            initialTriviaList = NodesOrTokens(Index).GetTrailingTrivia.ConvertTriviaList()

            Dim finalTrailingTrivia As SyntaxTriviaList
            For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                Select Case e.Value.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        finalTrailingTrivia = finalTrailingTrivia.Add(e.Value)
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
                                Dim whitespaceTrivia As SyntaxTrivia = If(NewWhiteSpaceString.Length = 0, VBSpaceTrivia, Factory.WhitespaceTrivia(NewWhiteSpaceString))
                                finalTrailingTrivia = finalTrailingTrivia.AddRange({VBSpaceTrivia, LineContinuation, whitespaceTrivia, e.Value})
                                afterLineContinuation = True
                            Else
                                finalTrailingTrivia = finalTrailingTrivia.Add(e.Value)
                                If Not String.IsNullOrWhiteSpace(NewWhiteSpaceString) Then
                                    finalTrailingTrivia = finalTrailingTrivia.Add(Factory.WhitespaceTrivia(NewWhiteSpaceString))
                                End If
                            End If
                        Else
                            finalTrailingTrivia = finalTrailingTrivia.Add(e.Value)
                            afterLineContinuation = False
                            afterWhiteSpace = False
                        End If
                    Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                        If Not afterWhiteSpace Then
                            finalTrailingTrivia = finalTrailingTrivia.Add(VBSpaceTrivia)
                        End If
                        If Not afterLineContinuation Then
                            finalTrailingTrivia = finalTrailingTrivia.Add(LineContinuation)
                            finalTrailingTrivia = finalTrailingTrivia.Add(VBSpaceTrivia)
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
        Friend Function DedupLeadingTrivia(node As CS.CSharpSyntaxNode, Keyword As SyntaxToken, Attributes As List(Of VBS.AttributeListSyntax), Modifiers As IEnumerable(Of SyntaxToken)) As SyntaxTriviaList
            Dim NodeLeadingTrivia As SyntaxTriviaList = node.GetLeadingTrivia.ConvertTriviaList()
            If Attributes.Any Then
                If EndsWithSimilarTrivia(NodeLeadingTrivia, Attributes(0).GetLeadingTrivia) Then
                    NodeLeadingTrivia = New SyntaxTriviaList
                End If
            ElseIf Modifiers.Any Then
                If EndsWithSimilarTrivia(NodeLeadingTrivia, Modifiers(0).LeadingTrivia) Then
                    NodeLeadingTrivia = New SyntaxTriviaList
                End If
            ElseIf EndsWithSimilarTrivia(NodeLeadingTrivia, Keyword.LeadingTrivia) Then
                NodeLeadingTrivia = New SyntaxTriviaList
            End If

            Return NodeLeadingTrivia
        End Function

        <Extension>
        Friend Function GetScopingBlock(Node As CS.CSharpSyntaxNode) As CS.CSharpSyntaxNode
            Dim Blocknode As CS.CSharpSyntaxNode = Node
            While Blocknode IsNot Nothing

                If TypeOf Blocknode Is CSS.BlockSyntax Then
                    Return CType(Blocknode.Parent, CS.CSharpSyntaxNode)
                End If
                If TypeOf Blocknode Is CSS.EventDeclarationSyntax Then
                    Return Blocknode
                End If

                If TypeOf Blocknode Is CSS.PropertyDeclarationSyntax Then
                    Return Blocknode
                End If

                If TypeOf Blocknode Is CSS.MethodDeclarationSyntax Then
                    Return Blocknode
                End If

                If TypeOf Blocknode Is CSS.ClassDeclarationSyntax Then
                    Return Blocknode
                End If

                If TypeOf Blocknode Is CSS.ConversionOperatorDeclarationSyntax Then
                    Return Blocknode
                End If

                If TypeOf Blocknode Is CSS.ConstructorDeclarationSyntax Then
                    Return Blocknode
                End If

                If TypeOf Blocknode Is CSS.EnumDeclarationSyntax Then
                    Return Blocknode
                End If

                If TypeOf Blocknode Is CSS.StructDeclarationSyntax Then
                    Return Blocknode
                End If

                If TypeOf Blocknode Is CSS.UsingDirectiveSyntax Then
                    Return Blocknode
                End If

                If TypeOf Blocknode Is CSS.UsingStatementSyntax Then
                    Return CType(Blocknode.Parent, CS.CSharpSyntaxNode)
                End If
                Blocknode = CType(Blocknode.Parent, CS.CSharpSyntaxNode)

            End While
            Return GetStatementwithIssues(Node)
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
            Dim newLeadingTrivia As SyntaxTriviaList = newLeadingTrivia.AddRange(Statement.GetLeadingTrivia)
            Dim NewTrailingTrivia As SyntaxTriviaList
            For Each Trivia As SyntaxTrivia In Statement.GetTrailingTrivia
                Select Case Trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia, VB.SyntaxKind.EndOfLineTrivia, VB.SyntaxKind.CommentTrivia
                        NewTrailingTrivia = NewTrailingTrivia.Add(Trivia)
                    Case VB.SyntaxKind.IfDirectiveTrivia
                        newLeadingTrivia = newLeadingTrivia.Add(Trivia)
                    Case Else
                        Stop
                End Select
            Next
            Return Statement.With(newLeadingTrivia, NewTrailingTrivia).WithTrailingEOL(RemoveLastLineContinuation:=True)
        End Function

        Friend Sub RestructureNodesAndSeparators(Of T As VB.VisualBasicSyntaxNode)(ByRef _OpenToken As SyntaxToken, ByRef Items As List(Of T), ByRef Separators As List(Of SyntaxToken), ByRef _CloseToken As SyntaxToken)
            _OpenToken = _OpenToken.WithModifiedTokenTrivia(LeadingToken:=True, AfterEOL:=False)
            For index As Integer = 0 To Items.Count - 2
                Dim NewItem As T = Items(index).AdjustNodeTrivia(SeparatorFollows:=True)
                Items(index) = NewItem
                Dim newSeparators As SyntaxToken = Separators(index).WithModifiedTokenTrivia(LeadingToken:=False, AfterEOL:=False)
                Separators(index) = newSeparators
            Next
            Dim LastItemEndsWithEOL As Boolean = False
            If Items.Any Then
                Dim NewItem As T = Items.Last.AdjustNodeTrivia(SeparatorFollows:=False)
                Items(Items.Count - 1) = NewItem
                LastItemEndsWithEOL = Items.Last.HasTrailingTrivia AndAlso Items.Last.GetTrailingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia)
            End If
            Dim newCloseToken As SyntaxToken = _CloseToken.WithModifiedTokenTrivia(LeadingToken:=False, LastItemEndsWithEOL)
            _CloseToken = newCloseToken.RemoveExtraEOL
        End Sub

        <Extension>
        Friend Function WithModifiedNodeTrailingTrivia(Of T As VB.VisualBasicSyntaxNode)(Node As T, SeparatorFollows As Boolean) As T
            Dim AfterComment As Boolean = False
            Dim AfterLineContinuation As Boolean = False
            Dim AfterLinefeed As Boolean = False
            Dim AfterWhiteSpace As Boolean = False
            Dim finalTrailingTrivia As SyntaxTriviaList
            Dim InitialTriviaList As SyntaxTriviaList = Node.GetTrailingTrivia
            For Each e As IndexClass(Of SyntaxTrivia) In InitialTriviaList.WithIndex
                Dim Trivia As SyntaxTrivia = e.Value
                Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(InitialTriviaList, e.Index, LookaheadCount:=1)
                Select Case Trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        If nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            Continue For
                        End If

                        If nextTrivia.IsKind(VB.SyntaxKind.CommentTrivia) OrElse
                            nextTrivia.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                            finalTrailingTrivia = finalTrailingTrivia.Add(Trivia)
                            AfterLinefeed = False
                            AfterComment = False
                            AfterWhiteSpace = True
                        End If
                    Case VB.SyntaxKind.EndOfLineTrivia
                        If nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            Continue For
                        End If
                        If Not AfterLinefeed Then
                            If AfterComment OrElse AfterLineContinuation Then
                                finalTrailingTrivia = finalTrailingTrivia.Add(Trivia)
                            Else
                                If SeparatorFollows Then
                                    finalTrailingTrivia = finalTrailingTrivia.AddRange(
                                        {VBSpaceTrivia, LineContinuation, Trivia}
                                        )
                                End If
                            End If
                            AfterComment = False
                            AfterLinefeed = True
                            AfterWhiteSpace = False
                            AfterLineContinuation = False
                        End If
                    Case VB.SyntaxKind.CommentTrivia
                        If Not AfterWhiteSpace Then
                            finalTrailingTrivia = finalTrailingTrivia.Add(VBSpaceTrivia)
                        End If
                        If Not AfterLineContinuation Then
                            finalTrailingTrivia = finalTrailingTrivia.AddRange({LineContinuation, VBSpaceTrivia})
                        End If
                        finalTrailingTrivia = finalTrailingTrivia.Add(Trivia)
                        If Not nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            finalTrailingTrivia = finalTrailingTrivia.Add(VBEOLTrivia)
                            AfterLineContinuation = False
                            AfterLinefeed = True
                        End If
                        AfterComment = True
                        AfterWhiteSpace = False
                    Case VB.SyntaxKind.LineContinuationTrivia
                        If finalTrailingTrivia.Last.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                            Continue For
                        End If
                        AfterWhiteSpace = False
                        AfterLineContinuation = True
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
