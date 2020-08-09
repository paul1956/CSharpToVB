' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices

Imports CSharpToVBCodeConverter.ToVisualBasic

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VB = Microsoft.CodeAnalysis.VisualBasic

Public Module SyntaxNodeExtensions

    ''' <summary>
    ''' Used at the end of a statement to adjust trivia from two items (like semicolon) the second
    ''' of which will be removed. Directives are allowed.
    ''' </summary>
    ''' <param name="InitialTriviaList"></param>
    ''' <param name="NewTrailingTrivia"></param>
    ''' <param name="FoundEOL"></param>
    ''' <param name="FoundWhiteSpace"></param>
    Private Sub AdjustTrailingTrivia(InitialTriviaList As IEnumerable(Of SyntaxTrivia), NewTrailingTrivia As List(Of SyntaxTrivia), ByRef FoundEOL As Boolean, ByRef FoundWhiteSpace As Boolean)
        For Each e As IndexClass(Of SyntaxTrivia) In InitialTriviaList.WithIndex
            Dim Trivia As SyntaxTrivia = e.Value
            Dim NextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(InitialTriviaList, e.Index)
            Select Case Trivia.RawKind
                Case VB.SyntaxKind.WhitespaceTrivia
                    If Not FoundWhiteSpace AndAlso Not NextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                        NewTrailingTrivia.Add(Trivia)
                        FoundEOL = False
                        FoundWhiteSpace = True
                    End If
                Case VB.SyntaxKind.EndOfLineTrivia
                    If Not FoundEOL Then
                        NewTrailingTrivia.Add(Trivia)
                        FoundEOL = True
                    End If
                    FoundWhiteSpace = False
                Case VB.SyntaxKind.CommentTrivia
                    NewTrailingTrivia.Add(Trivia)
                    If Not NextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                        NewTrailingTrivia.Add(VBEOLTrivia)
                        FoundEOL = True
                    End If
                    FoundWhiteSpace = False
                Case VB.SyntaxKind.EndRegionDirectiveTrivia
                    If Not FoundEOL Then
                        NewTrailingTrivia.Add(VBEOLTrivia)
                    End If
                    NewTrailingTrivia.Add(Trivia)
                    FoundEOL = False
                    FoundWhiteSpace = False
                Case Else
                    If Trivia.IsDirective Then
                        If Not FoundEOL Then
                            NewTrailingTrivia.Add(VBEOLTrivia)
                        End If
                        NewTrailingTrivia.Add(Trivia)
                        FoundEOL = False
                        FoundWhiteSpace = False
                    Else
                        Stop
                    End If
            End Select
        Next
    End Sub

    <Extension()>
    Friend Function [With](Of T As SyntaxNode)(node As T, leadingTrivia As IEnumerable(Of SyntaxTrivia), trailingTrivia As IEnumerable(Of SyntaxTrivia)) As T
        Return node.WithLeadingTrivia(leadingTrivia).WithTrailingTrivia(trailingTrivia)
    End Function

    <Extension()>
    Friend Iterator Function GetAncestors(Of TNode As SyntaxNode)(node As SyntaxNode) As IEnumerable(Of TNode)
        Dim current As SyntaxNode = node.Parent
        While current IsNot Nothing
            If TypeOf current Is TNode Then
                Yield DirectCast(current, TNode)
            End If

            current = If(TypeOf current Is IStructuredTriviaSyntax, DirectCast(current, IStructuredTriviaSyntax).ParentTrivia.Token.Parent, current.Parent)
        End While
    End Function

    <Extension()>
    Friend Function IsKind(node As SyntaxNode, ParamArray kind1() As CS.SyntaxKind) As Boolean
        If node Is Nothing Then
            Return False
        End If

        For Each k As CS.SyntaxKind In kind1
            If node.IsKind(k) Then
                Return True
            End If
        Next
        Return False
    End Function

    <Extension>
    Friend Function ParentHasSameTrailingTrivia(otherNode As SyntaxNode) As Boolean
        If otherNode.Parent Is Nothing Then
            Return False
        End If
        Return otherNode.Parent.GetLastToken() = otherNode.GetLastToken()
    End Function

    ''' <summary>
    ''' Remove Leading EOL and convert multiple EOL's to one.
    ''' This is used in statements that are more then 1 line
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="node"></param>
    ''' <returns>Node with extra EOL trivia removed</returns>
    <Extension()>
    Friend Function RemoveExtraLeadingEOL(Of T As SyntaxNode)(node As T) As T
        Dim LeadingTrivia As List(Of SyntaxTrivia) = node.GetLeadingTrivia.ToList
        Select Case LeadingTrivia.Count
            Case 0
                Return node
            Case 1
                If LeadingTrivia.First.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                    Return node.WithoutLeadingTrivia
                End If
            Case 2
                Select Case LeadingTrivia.First.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        If LeadingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            Return node.WithoutLeadingTrivia
                        End If
                        Return node
                    Case VB.SyntaxKind.EndOfLineTrivia
                        If LeadingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            Return node.WithoutLeadingTrivia
                        End If
                        Return node.WithLeadingTrivia(LeadingTrivia.Last)
                    Case Else
                End Select
            Case Else
        End Select
        Dim NewLeadingTrivia As New List(Of SyntaxTrivia)
        Dim FirstTrivia As Boolean = True
        Dim intialTriviaList As SyntaxTriviaList = node.GetLeadingTrivia
        For Each e As IndexClass(Of SyntaxTrivia) In intialTriviaList.WithIndex
            Dim Trivia As SyntaxTrivia = e.Value
            Dim NextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(intialTriviaList, e.Index)
            If Trivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) AndAlso (FirstTrivia OrElse NextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia)) Then
                Continue For
            End If
            If Trivia.IsKind(VB.SyntaxKind.WhitespaceTrivia) AndAlso NextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                Continue For
            End If

            FirstTrivia = False
            NewLeadingTrivia.Add(Trivia)
        Next

        Return node.WithLeadingTrivia(NewLeadingTrivia)
    End Function

    <Extension()>
    Friend Function WithAppendedTrailingTrivia(Of T As SyntaxNode)(node As T, ParamArray trivia As SyntaxTrivia()) As T
        If trivia.Length = 0 Then
            Return node
        End If

        Return node.WithAppendedTrailingTrivia(DirectCast(trivia, IEnumerable(Of SyntaxTrivia)))
    End Function

    <Extension>
    Friend Function WithAppendedTriviaFromEndOfDirectiveToken(Of T As SyntaxNode)(node As T, Token As SyntaxToken) As T
        Dim NewTrailingTrivia As New List(Of SyntaxTrivia)
        If Token.HasLeadingTrivia Then
            NewTrailingTrivia.AddRange(ConvertTrivia(Token.LeadingTrivia))
        End If
        If Token.HasTrailingTrivia Then
            NewTrailingTrivia.AddRange(ConvertTrivia(Token.TrailingTrivia))
        End If

        Return node.WithAppendedTrailingTrivia(NewTrailingTrivia).WithTrailingEOL
    End Function

    ''' <summary>
    ''' Merge trailing trivia
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="node"></param>
    ''' <param name="TriviaListToMerge"></param>
    ''' <returns></returns>
    <Extension()>
    Friend Function WithMergedTrailingTrivia(Of T As SyntaxNode)(node As T, TriviaListToMerge As IEnumerable(Of SyntaxTrivia)) As T
        If node Is Nothing Then
            Return Nothing
        End If
        If Not TriviaListToMerge?.Any Then
            Return node
        End If
        Dim NodeTrailingTrivia As List(Of SyntaxTrivia) = node.GetTrailingTrivia.ToList
        If Not NodeTrailingTrivia.Any Then
            Return node.WithTrailingTrivia(TriviaListToMerge)
        End If
        Dim NewTrailingTrivia As New List(Of SyntaxTrivia)
        ' Both nodes have trivia
        Dim FoundEOL As Boolean = False
        Dim FoundWhiteSpace As Boolean = False
        AdjustTrailingTrivia(NodeTrailingTrivia, NewTrailingTrivia, FoundEOL, FoundWhiteSpace)
        AdjustTrailingTrivia(TriviaListToMerge, NewTrailingTrivia, FoundEOL, FoundWhiteSpace)
        Return node.WithTrailingTrivia(NewTrailingTrivia)
    End Function

    <Extension()>
    Friend Function WithPrependedLeadingTrivia(Of T As SyntaxNode)(node As T, ParamArray trivia As SyntaxTrivia()) As T
        If trivia.Length = 0 Then
            Return node
        End If
        Dim TriviaList As List(Of SyntaxTrivia) = trivia.ToList
        If TriviaList.Last.IsKind(VB.SyntaxKind.CommentTrivia) Then
            TriviaList.Add(VBEOLTrivia)
        End If
        Return node.WithPrependedLeadingTrivia(TriviaList)
    End Function

    <Extension()>
    Friend Function WithPrependedLeadingTrivia(Of T As SyntaxNode)(node As T, trivia As SyntaxTriviaList) As T
        If trivia.Count = 0 Then
            Return node
        End If
        If trivia.Last.IsKind(VB.SyntaxKind.CommentTrivia) Then
            trivia = trivia.Add(VBEOLTrivia)
        End If
        Return node.WithLeadingTrivia(trivia.Concat(node.GetLeadingTrivia()))
    End Function

    <Extension()>
    Friend Function WithPrependedLeadingTrivia(Of T As SyntaxNode)(node As T, trivia As IEnumerable(Of SyntaxTrivia)) As T
        If trivia Is Nothing Then
            Return node
        End If
        Return node.WithPrependedLeadingTrivia(trivia.ToSyntaxTriviaList())
    End Function

    <Extension>
    Friend Function WithRestructuredingEOLTrivia(Of T As SyntaxNode)(node As T) As T
        If Not node.HasTrailingTrivia Then
            Return node
        End If

        Dim NodeTrailingTrivia As SyntaxTriviaList = node.GetTrailingTrivia
        If NodeTrailingTrivia.ContainsEOLTrivia Then
            Dim NewTriviaList As New List(Of SyntaxTrivia)
            For Each Trivia As SyntaxTrivia In NodeTrailingTrivia
                If Trivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                    Continue For
                End If
                NewTriviaList.Add(Trivia)
            Next
            Return node.WithTrailingTrivia(NewTriviaList)
        Else
            Return node
        End If
    End Function

    ''' <summary>
    ''' Make sure the node (usually a statement) ends with an EOL and possibly whitespace
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="node"></param>
    ''' <returns></returns>
    <Extension()>
    Friend Function WithTrailingEOL(Of T As SyntaxNode)(node As T) As T
        Dim TrailingTrivia As List(Of SyntaxTrivia) = node.GetTrailingTrivia.ToList
        Dim Count As Integer = TrailingTrivia.Count
        If Count = 0 Then
            Return node.WithTrailingTrivia(VBEOLTrivia)
        End If
        If Count = 1 Then
            Select Case TrailingTrivia(0).RawKind
                Case VB.SyntaxKind.EndOfLineTrivia
                    Return node
                Case VB.SyntaxKind.WhitespaceTrivia
                    Return node.WithTrailingTrivia(VBEOLTrivia)
            End Select
            TrailingTrivia.Add(VBEOLTrivia)
            Return node.WithTrailingTrivia(TrailingTrivia)
        End If
        If Count = 2 Then
            Select Case TrailingTrivia(0).RawKind
                Case VB.SyntaxKind.WhitespaceTrivia
                    Select Case TrailingTrivia(1).RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia, VB.SyntaxKind.EndOfLineTrivia
                            ' Replace Whitespace, Whitespace and Whitespace, EOL with just EOL
                            TrailingTrivia = New List(Of SyntaxTrivia)
                                    ' EOL added below
                        Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.LineContinuationTrivia
                            ' nothing to do EOL added below
                        Case Else
                            Stop
                    End Select
                Case VB.SyntaxKind.EndOfLineTrivia
                    If TrailingTrivia(1).IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                        Return node
                    ElseIf TrailingTrivia(1).IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                        Return node.WithTrailingTrivia(VBEOLTrivia)
                    ElseIf TrailingTrivia(1).IsDirective Then
                        Return node
                    End If
                    Stop
                Case VB.SyntaxKind.CommentTrivia
                    If TrailingTrivia(1).IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                        TrailingTrivia.RemoveAt(1)
                        TrailingTrivia.Insert(0, SpaceTrivia)
                        ' EOL added below
                    End If
                Case Else
                    Stop
            End Select
            TrailingTrivia.Add(VBEOLTrivia)
            Return node.WithTrailingTrivia(TrailingTrivia)
        End If
        Dim newTrailingTrivia As New List(Of SyntaxTrivia)
        For Each e As IndexClass(Of SyntaxTrivia) In TrailingTrivia.WithIndex
            Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(TrailingTrivia, e.Index)
            Select Case e.Value.RawKind
                Case VB.SyntaxKind.WhitespaceTrivia
                    Select Case nextTrivia.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            newTrailingTrivia.Add(AdjustWhitespace(e.Value, nextTrivia, False))
                            e.MoveNext()
                            If e.IsLast Then
                                newTrailingTrivia.Add(VBEOLTrivia)
                            End If
                        Case VB.SyntaxKind.EndOfLineTrivia
                            ' Ignore white space before EOL
                        Case VB.SyntaxKind.CommentTrivia
                            newTrailingTrivia.Add(e.Value)
                        Case VB.SyntaxKind.LineContinuationTrivia
                            newTrailingTrivia.Add(e.Value)
                        Case VB.SyntaxKind.None
                            newTrailingTrivia.Add(e.Value)
                        Case VB.SyntaxKind.RegionDirectiveTrivia,
                             VB.SyntaxKind.EndRegionDirectiveTrivia
                            newTrailingTrivia.Add(e.Value)
                        Case Else
                            Stop
                    End Select
                Case VB.SyntaxKind.EndOfLineTrivia
                    If Not (nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) OrElse newTrailingTrivia.LastOrDefault.IsKind(VB.SyntaxKind.EndOfLineTrivia)) Then
                        newTrailingTrivia.Add(e.Value)
                    End If
                Case VB.SyntaxKind.LineContinuationTrivia
                    Select Case nextTrivia.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            newTrailingTrivia.Add(e.Value)
                        Case VB.SyntaxKind.EndOfLineTrivia
                            newTrailingTrivia.Add(e.Value)
                        Case VB.SyntaxKind.CommentTrivia
                            newTrailingTrivia.Add(e.Value)
                        Case VB.SyntaxKind.LineContinuationTrivia
                            ' ignore
                        Case Else
                            Stop
                    End Select
                Case VB.SyntaxKind.CommentTrivia,
                     VB.SyntaxKind.DocumentationCommentExteriorTrivia,
                     VB.SyntaxKind.DocumentationCommentTrivia
                    Select Case nextTrivia.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            newTrailingTrivia.Add(e.Value)
                            If GetForwardTriviaOrDefault(TrailingTrivia, e.Index, lookaheadCount:=2).IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                                newTrailingTrivia.Add(VBEOLTrivia)
                            Else
                                e.MoveNext()
                                If e.IsLast Then
                                    newTrailingTrivia.Add(VBEOLTrivia)
                                End If
                            End If
                        Case VB.SyntaxKind.EndOfLineTrivia
                            newTrailingTrivia.Add(e.Value)
                        Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentExteriorTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                            newTrailingTrivia.Add(e.Value)
                            newTrailingTrivia.Add(VBEOLTrivia)
                        Case Else
                            Stop
                    End Select
                Case VB.SyntaxKind.IfDirectiveTrivia,
                     VB.SyntaxKind.DisabledTextTrivia,
                     VB.SyntaxKind.ElseIfDirectiveTrivia,
                     VB.SyntaxKind.ElseDirectiveTrivia,
                     VB.SyntaxKind.EndIfDirectiveTrivia,
                     VB.SyntaxKind.RegionDirectiveTrivia,
                     VB.SyntaxKind.EndRegionDirectiveTrivia
                    newTrailingTrivia.Add(e.Value)
                    If e.IsLast Then
                        newTrailingTrivia.Add(VBEOLTrivia)
                    End If
                Case Else
                    Stop
            End Select
        Next
        Return node.WithTrailingTrivia(newTrailingTrivia)
    End Function

    <Extension()>
    Public Function GetAncestor(Of TNode As SyntaxNode)(node As SyntaxNode) As TNode
        If node Is Nothing Then
            Return Nothing
        End If

        Return node.GetAncestors(Of TNode)().FirstOrDefault()
    End Function

    <Extension()>
    Public Function GetBraces(node As SyntaxNode) As ValueTuple(Of SyntaxToken, SyntaxToken)
        Dim namespaceNode As CSS.NamespaceDeclarationSyntax = TryCast(node, CSS.NamespaceDeclarationSyntax)
        If namespaceNode IsNot Nothing Then
            Return ValueTuple.Create(namespaceNode.OpenBraceToken, namespaceNode.CloseBraceToken)
        End If

        Dim baseTypeNode As CSS.BaseTypeDeclarationSyntax = TryCast(node, CSS.BaseTypeDeclarationSyntax)
        If baseTypeNode IsNot Nothing Then
            Return ValueTuple.Create(baseTypeNode.OpenBraceToken, baseTypeNode.CloseBraceToken)
        End If

        Dim accessorListNode As CSS.AccessorListSyntax = TryCast(node, CSS.AccessorListSyntax)
        If accessorListNode IsNot Nothing Then
            Return ValueTuple.Create(accessorListNode.OpenBraceToken, accessorListNode.CloseBraceToken)
        End If

        Dim blockNode As CSS.BlockSyntax = TryCast(node, CSS.BlockSyntax)
        If blockNode IsNot Nothing Then
            Return ValueTuple.Create(blockNode.OpenBraceToken, blockNode.CloseBraceToken)
        End If

        Dim switchStatementNode As CSS.SwitchStatementSyntax = TryCast(node, CSS.SwitchStatementSyntax)
        If switchStatementNode IsNot Nothing Then
            Return ValueTuple.Create(switchStatementNode.OpenBraceToken, switchStatementNode.CloseBraceToken)
        End If

        Dim anonymousObjectCreationExpression As CSS.AnonymousObjectCreationExpressionSyntax = TryCast(node, CSS.AnonymousObjectCreationExpressionSyntax)
        If anonymousObjectCreationExpression IsNot Nothing Then
            Return ValueTuple.Create(anonymousObjectCreationExpression.OpenBraceToken, anonymousObjectCreationExpression.CloseBraceToken)
        End If

        Dim initializeExpressionNode As CSS.InitializerExpressionSyntax = TryCast(node, CSS.InitializerExpressionSyntax)
        If initializeExpressionNode IsNot Nothing Then
            Return ValueTuple.Create(initializeExpressionNode.OpenBraceToken, initializeExpressionNode.CloseBraceToken)
        End If

        Return New ValueTuple(Of SyntaxToken, SyntaxToken)()
    End Function

    <Extension()>
    Public Function IsKind(node As SyntaxNode, ParamArray kind1() As VB.SyntaxKind) As Boolean
        If node Is Nothing Then
            Return False
        End If

        For Each k As VB.SyntaxKind In kind1
            If CType(node.RawKind(), VB.SyntaxKind) = k Then
                Return True
            End If
        Next
        Return False
    End Function

    <Extension()>
    Public Function WithAppendedTrailingTrivia(Of T As SyntaxNode)(node As T, trivia As SyntaxTriviaList) As T
        If trivia.Count = 0 Then
            Return node
        End If
        If node Is Nothing Then
            Return Nothing
        End If

        Return node.WithTrailingTrivia(node.GetTrailingTrivia().Concat(trivia))
    End Function

    <Extension()>
    Public Function WithAppendedTrailingTrivia(Of T As SyntaxNode)(node As T, trivia As IEnumerable(Of SyntaxTrivia)) As T
        If node Is Nothing Then
            Return Nothing
        End If
        If trivia Is Nothing Then
            Return node
        End If
        Return node.WithAppendedTrailingTrivia(trivia.ToSyntaxTriviaList())
    End Function

#Region "WithConvertedTriviaFrom"

    <Extension>
    Friend Function WithConvertedTriviaFrom(Of T As SyntaxNode)(node As T, otherNode As SyntaxNode) As T
        If otherNode Is Nothing Then
            Return node
        End If
        If otherNode.HasLeadingTrivia Then
            node = node.WithLeadingTrivia(ConvertTrivia(otherNode.GetLeadingTrivia()))
        End If
        If Not otherNode.HasTrailingTrivia OrElse ParentHasSameTrailingTrivia(otherNode) Then
            Return node
        End If
        Return node.WithTrailingTrivia(ConvertTrivia(otherNode.GetTrailingTrivia()))
    End Function

    <Extension>
    Public Function WithConvertedTriviaFrom(Of T As SyntaxNode)(node As T, otherToken As SyntaxToken) As T
        If node Is Nothing Then
            Throw New ArgumentException($"Parameter {NameOf(node)} Is Nothing")
        End If
        If otherToken.HasLeadingTrivia Then
            node = node.WithLeadingTrivia(ConvertTrivia(otherToken.LeadingTrivia))
        End If
        If Not otherToken.HasTrailingTrivia Then
            Return node
        End If
        Return node.WithTrailingTrivia(ConvertTrivia(otherToken.TrailingTrivia()))
    End Function

#End Region

#Region "WithConvertedLeadingTriviaFrom"

    <Extension>
    Friend Function WithConvertedLeadingTriviaFrom(Of T As SyntaxNode)(node As T, otherNode As SyntaxNode) As T
        If otherNode Is Nothing OrElse Not otherNode.HasLeadingTrivia Then
            Return node
        End If
        Return node.WithLeadingTrivia(ConvertTrivia(otherNode.GetLeadingTrivia()))
    End Function

    <Extension>
    Friend Function WithConvertedLeadingTriviaFrom(Of T As SyntaxNode)(node As T, otherToken As SyntaxToken) As T
        If Not otherToken.HasLeadingTrivia Then
            Return node
        End If
        Return node.WithLeadingTrivia(ConvertTrivia(otherToken.LeadingTrivia()))
    End Function

    <Extension>
    Public Function WithConvertedLeadingTriviaFrom(node As SyntaxToken, otherToken As SyntaxToken) As SyntaxToken
        If Not otherToken.HasLeadingTrivia Then
            Return node
        End If
        Return node.WithLeadingTrivia(ConvertTrivia(otherToken.LeadingTrivia()))
    End Function

#End Region

#Region "WithConvertedTrailingTriviaFrom"

    <Extension>
    Friend Function WithConvertedTrailingTriviaFrom(Of T As SyntaxNode)(node As T, otherNode As SyntaxNode) As T
        If otherNode Is Nothing OrElse Not otherNode.HasTrailingTrivia Then
            Return node
        End If
        If ParentHasSameTrailingTrivia(otherNode) Then
            Return node
        End If
        Return node.WithTrailingTrivia(ConvertTrivia(otherNode.GetTrailingTrivia()))
    End Function

    <Extension>
    Friend Function WithConvertedTrailingTriviaFrom(Of T As SyntaxNode)(node As T, otherToken As SyntaxToken) As T
        If Not otherToken.HasTrailingTrivia Then
            Return node
        End If
        Return node.WithTrailingTrivia(ConvertTrivia(otherToken.TrailingTrivia()))
    End Function

#End Region

End Module
