' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports System.Runtime.InteropServices
Imports System.Threading
Imports CSharpToVBConverter.ToVisualBasic

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VB = Microsoft.CodeAnalysis.VisualBasic

Namespace CSharpToVBConverter

    Public Module SyntaxNodeExtensions

        ''' <summary>
        ''' Used at the end of a statement to adjust trivia from two items (like semicolon) the second
        ''' of which will be removed. Directives are allowed.
        ''' </summary>
        ''' <param name="InitialTriviaList"></param>
        ''' <param name="TriviaListToMerge"></param>
        ''' <returns></returns>
        Private Function AdjustMergedStatementTrailingTrivia(InitialTriviaList As SyntaxTriviaList, TriviaListToMerge As SyntaxTriviaList) As SyntaxTriviaList
            Dim inputTrivia As SyntaxTriviaList = InitialTriviaList
            inputTrivia = inputTrivia.AddRange(TriviaListToMerge)
            Dim FoundEOL As Boolean = False
            Dim FoundWhiteSpace As Boolean = False
            Dim NewTrailingTrivia As SyntaxTriviaList
            For Each e As IndexClass(Of SyntaxTrivia) In inputTrivia.WithIndex
                Dim Trivia As SyntaxTrivia = e.Value
                Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(inputTrivia, e.Index, LookaheadCount:=1)
                Select Case Trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        If Not FoundWhiteSpace AndAlso Not nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            NewTrailingTrivia = NewTrailingTrivia.Add(Trivia)
                            FoundEOL = False
                            FoundWhiteSpace = True
                        End If
                    Case VB.SyntaxKind.EndOfLineTrivia
                        If Not FoundEOL Then
                            NewTrailingTrivia = NewTrailingTrivia.Add(Trivia)
                            FoundEOL = True
                        End If
                        FoundWhiteSpace = False
                    Case VB.SyntaxKind.CommentTrivia
                        NewTrailingTrivia = NewTrailingTrivia.Add(Trivia)
                        If Not nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            NewTrailingTrivia = NewTrailingTrivia.Add(VBEOLTrivia)
                            FoundEOL = True
                        End If
                        FoundWhiteSpace = False
                    Case VB.SyntaxKind.EndRegionDirectiveTrivia
                        If Not FoundEOL Then
                            NewTrailingTrivia = NewTrailingTrivia.Add(VBEOLTrivia)
                        End If
                        NewTrailingTrivia = NewTrailingTrivia.Add(Trivia)
                        FoundEOL = False
                        FoundWhiteSpace = False
                    Case Else
                        If Trivia.IsDirective Then
                            If Not FoundEOL Then
                                NewTrailingTrivia = NewTrailingTrivia.Add(VBEOLTrivia)
                            End If
                            NewTrailingTrivia = NewTrailingTrivia.Add(Trivia)
                            FoundEOL = False
                            FoundWhiteSpace = False
                        Else
                            Stop
                        End If
                End Select
            Next
            Return NewTrailingTrivia
        End Function

        ''' <summary>
        ''' Used at the beginning of a statement to adjust trivia from two statement, the first
        ''' of which will be removed. Directives are allowed.
        ''' </summary>
        ''' <param name="InitialTriviaList"></param>
        ''' <param name="TriviaListToMerge"></param>
        ''' <returns>The merged Trivia</returns>
        Private Function AdjustStatementLeadingTrivia(InitialTriviaList As SyntaxTriviaList, TriviaListToMerge As SyntaxTriviaList) As SyntaxTriviaList
            Dim inputTrivia As SyntaxTriviaList = inputTrivia.AddRange(InitialTriviaList)
            inputTrivia = inputTrivia.AddRange(TriviaListToMerge)
            Dim newLeadingTrivia As New SyntaxTriviaList
            Dim FoundEOL As Boolean = True
            Dim FoundWhiteSpace As Boolean = False
            For Each e As IndexClass(Of SyntaxTrivia) In inputTrivia.WithIndex
                Dim Trivia As SyntaxTrivia = e.Value
                Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(InitialTriviaList, e.Index, LookaheadCount:=1)
                Select Case Trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        If Not (FoundWhiteSpace OrElse nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia)) Then
                            newLeadingTrivia = newLeadingTrivia.Add(Trivia)
                            FoundEOL = False
                            FoundWhiteSpace = True
                        End If
                    Case VB.SyntaxKind.EndOfLineTrivia
                        If Not FoundEOL Then
                            newLeadingTrivia = newLeadingTrivia.Add(Trivia)
                            FoundEOL = True
                        End If
                        FoundWhiteSpace = False
                    Case VB.SyntaxKind.CommentTrivia
                        newLeadingTrivia = newLeadingTrivia.Add(Trivia)
                        If Not nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            newLeadingTrivia = newLeadingTrivia.Add(VBEOLTrivia)
                            FoundEOL = True
                        End If
                        FoundWhiteSpace = False
                    Case Else
                        If Trivia.IsDirective Then
                            If Not FoundEOL Then
                                newLeadingTrivia = newLeadingTrivia.Add(VBEOLTrivia)
                            End If
                            newLeadingTrivia = newLeadingTrivia.Add(Trivia)
                            If Not nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                newLeadingTrivia = newLeadingTrivia.Add(VBEOLTrivia)
                                FoundEOL = True
                            Else
                                FoundEOL = False
                            End If
                            FoundWhiteSpace = False
                        Else
                            Stop
                        End If
                End Select
            Next
            Return newLeadingTrivia
        End Function

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
            Dim intialLeadingTrivia As SyntaxTriviaList = node.GetLeadingTrivia
            Select Case intialLeadingTrivia.Count
                Case 0
                    Return node
                Case 1
                    If intialLeadingTrivia.First.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                        Return node.WithoutLeadingTrivia
                    End If
                Case 2
                    Select Case intialLeadingTrivia.First.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            If intialLeadingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                Return node.WithoutLeadingTrivia
                            End If
                            Return node
                        Case VB.SyntaxKind.EndOfLineTrivia
                            If intialLeadingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                Return node.WithoutLeadingTrivia
                            End If
                            Return node.WithLeadingTrivia(intialLeadingTrivia.Last)
                        Case Else
                    End Select
                Case Else
            End Select
            Dim firstTrivia As Boolean = True
            Dim newLeadingTrivia As New SyntaxTriviaList
            For Each e As IndexClass(Of SyntaxTrivia) In intialLeadingTrivia.WithIndex
                Dim Trivia As SyntaxTrivia = e.Value
                Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(intialLeadingTrivia, e.Index, LookaheadCount:=1)
                If Trivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) AndAlso (firstTrivia OrElse nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia)) Then
                    Continue For
                End If
                If Trivia.IsKind(VB.SyntaxKind.WhitespaceTrivia) AndAlso nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                    Continue For
                End If

                firstTrivia = False
                newLeadingTrivia = newLeadingTrivia.Add(Trivia)
            Next

            Return node.WithLeadingTrivia(newLeadingTrivia)
        End Function

        <Extension>
        Friend Function TryInitializeState(node As SyntaxNode,
                                       model As SemanticModel,
                                       <Out()> ByRef classOrStructDecl As SyntaxNode,
                                       <Out()> ByRef classOrStructType As INamedTypeSymbol,
                                       <Out()> ByRef interfaceTypes As IEnumerable(Of INamedTypeSymbol),
                                       CancelToken As CancellationToken) As Boolean
            Dim NodeTypeIsTypeSyntax As Boolean = TypeOf node Is CSS.TypeSyntax
            Dim interfaceNode As CSS.TypeSyntax = If(NodeTypeIsTypeSyntax, CType(node, CSS.TypeSyntax), Nothing)
            If NodeTypeIsTypeSyntax AndAlso TypeOf interfaceNode.Parent Is CSS.BaseTypeSyntax AndAlso interfaceNode.Parent.IsParentKind(CS.SyntaxKind.BaseList) AndAlso CType(interfaceNode.Parent, CSS.BaseTypeSyntax).Type Is interfaceNode Then
                If interfaceNode.Parent.Parent.IsParentKind(CS.SyntaxKind.ClassDeclaration) OrElse interfaceNode.Parent.Parent.IsParentKind(CS.SyntaxKind.StructDeclaration) Then
                    Dim interfaceSymbolInfo As SymbolInfo = model.GetSymbolInfo(interfaceNode, CancelToken)
                    If interfaceSymbolInfo.CandidateReason <> CandidateReason.WrongArity Then
                        Dim interfaceType As INamedTypeSymbol = TryCast(interfaceSymbolInfo.GetAnySymbol(), INamedTypeSymbol)
                        If interfaceType IsNot Nothing AndAlso interfaceType.TypeKind = Microsoft.CodeAnalysis.TypeKind.Interface Then
                            classOrStructDecl = TryCast(interfaceNode.Parent.Parent.Parent, CSS.TypeDeclarationSyntax)
                            classOrStructType = TryCast(model.GetDeclaredSymbol(classOrStructDecl, CancelToken), INamedTypeSymbol)
                            interfaceTypes = New SingletonList(Of INamedTypeSymbol)(interfaceType)
                            Return interfaceTypes IsNot Nothing AndAlso classOrStructType IsNot Nothing
                        End If
                    End If
                End If
            End If

            classOrStructDecl = Nothing
            classOrStructType = Nothing
            interfaceTypes = Nothing
            Return False
        End Function

        <Extension()>
        Friend Function WithAppendedEOL(Of T As SyntaxNode)(node As T) As T
            Dim TrailingTrivia As SyntaxTriviaList = node.GetTrailingTrivia()
            TrailingTrivia = TrailingTrivia.Add(VBEOLTrivia)
            Return node.WithTrailingTrivia(TrailingTrivia)
        End Function

        <Extension>
        Friend Function WithAppendedTriviaFromEndOfDirectiveToken(Of T As SyntaxNode)(node As T, Token As SyntaxToken) As T
            Dim NewTrailingTrivia As New SyntaxTriviaList
            If Token.HasLeadingTrivia Then
                NewTrailingTrivia = Token.LeadingTrivia.ConvertTriviaList()
            End If
            If Token.HasTrailingTrivia Then
                NewTrailingTrivia = NewTrailingTrivia.AddRange(Token.TrailingTrivia.ConvertTriviaList())
            End If

            Return node.WithAppendedTrailingTrivia(NewTrailingTrivia).WithTrailingEOL(RemoveLastLineContinuation:=True)
        End Function

        ''' <summary>
        ''' Merge leading trivia
        ''' </summary>
        ''' <typeparam name="T"></typeparam>
        ''' <param name="node"></param>
        ''' <param name="TriviaListToMerge"></param>
        ''' <returns></returns>
        <Extension()>
        Friend Function WithMergedLeadingTrivia(Of T As SyntaxNode)(node As T, TriviaListToMerge As SyntaxTriviaList) As T
            If node Is Nothing Then
                Return Nothing
            End If
            If Not TriviaListToMerge.Any Then
                Return node
            End If
            Dim nodeLeadingTrivia As SyntaxTriviaList = node.GetLeadingTrivia
            If Not nodeLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                Return node.WithLeadingTrivia(TriviaListToMerge)
            End If
            ' Both nodes have trivia
            Return node.WithLeadingTrivia(AdjustStatementLeadingTrivia(nodeLeadingTrivia, TriviaListToMerge))
        End Function

        ''' <summary>
        ''' Merge trailing trivia
        ''' </summary>
        ''' <typeparam name="T"></typeparam>
        ''' <param name="node"></param>
        ''' <param name="TriviaListToMerge"></param>
        ''' <returns></returns>
        <Extension()>
        Friend Function WithMergedTrailingTrivia(Of T As SyntaxNode)(node As T, TriviaListToMerge As SyntaxTriviaList) As T
            If node Is Nothing Then
                Return Nothing
            End If
            If Not TriviaListToMerge.Any Then
                Return node
            End If
            Dim NodeTrailingTrivia As SyntaxTriviaList = node.GetTrailingTrivia
            If Not NodeTrailingTrivia.Any Then
                Return node.WithTrailingTrivia(TriviaListToMerge)
            End If
            ' Both nodes have trivia
            Return node.WithTrailingTrivia(AdjustMergedStatementTrailingTrivia(NodeTrailingTrivia, TriviaListToMerge))
        End Function

        <Extension()>
        Friend Function WithPrependedLeadingTrivia(Of T As SyntaxNode)(node As T, ParamArray triviaArray As SyntaxTrivia()) As T
            If triviaArray.Length = 0 Then
                Return node
            End If
            Dim newTriviaList As SyntaxTriviaList = triviaArray.ToSyntaxTriviaList
            If newTriviaList.Last.IsKind(VB.SyntaxKind.CommentTrivia) Then
                newTriviaList = newTriviaList.Add(VBEOLTrivia)
            End If
            Return node.WithPrependedLeadingTrivia(newTriviaList)
        End Function

        <Extension()>
        Friend Function WithPrependedLeadingTrivia(Of T As SyntaxNode)(node As T, trivia As SyntaxTriviaList) As T
            If Not trivia.Any Then
                Return node
            End If
            If trivia.Last.IsKind(VB.SyntaxKind.CommentTrivia) Then
                trivia = trivia.Add(VBEOLTrivia)
            End If
            Return node.WithLeadingTrivia(trivia.Concat(node.GetLeadingTrivia()))
        End Function

        <Extension>
        Friend Function WithRestructuredingEOLTrivia(Of T As SyntaxNode)(node As T) As T
            If Not node.HasTrailingTrivia Then
                Return node
            End If

            Dim NodeTrailingTrivia As SyntaxTriviaList = node.GetTrailingTrivia
            If NodeTrailingTrivia.ContainsEOLTrivia Then
                Dim newTriviaList As SyntaxTriviaList
                For Each Trivia As SyntaxTrivia In NodeTrailingTrivia
                    If Trivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                        Continue For
                    End If
                    newTriviaList = newTriviaList.Add(Trivia)
                Next
                Return node.WithTrailingTrivia(newTriviaList)
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
        ''' <param name="RemoveLastLineContinuation"></param>
        <Extension()>
        Friend Function WithTrailingEOL(Of T As SyntaxNode)(node As T, RemoveLastLineContinuation As Boolean) As T
            Dim TrailingTrivia As SyntaxTriviaList = node.GetTrailingTrivia
            Dim newTrailingTrivia As New SyntaxTriviaList
            Dim Count As Integer = TrailingTrivia.Count
            'RemoveLastLineContinuation = True
            Select Case TrailingTrivia.Count
                Case 0
                    Return node.WithTrailingTrivia(VBEOLTrivia)
                Case 1
                    Select Case TrailingTrivia(0).RawKind
                        Case VB.SyntaxKind.EndOfLineTrivia
                            Return node
                        Case VB.SyntaxKind.WhitespaceTrivia
                            Return node.WithTrailingTrivia(VBEOLTrivia)
                    End Select
                    Throw UnexpectedValue($"TrailingTrivia(0).RawKind = {TrailingTrivia(0).RawKind}")
                Case 2
                    Select Case TrailingTrivia(0).RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            Select Case TrailingTrivia(1).RawKind
                                Case VB.SyntaxKind.WhitespaceTrivia, VB.SyntaxKind.EndOfLineTrivia
                                ' Replace Whitespace, Whitespace and Whitespace, EOL with just EOL
                                Case VB.SyntaxKind.CommentTrivia,
                             VB.SyntaxKind.EndRegionDirectiveTrivia
                                    newTrailingTrivia = newTrailingTrivia.Add(TrailingTrivia(0))
                                    newTrailingTrivia = newTrailingTrivia.Add(TrailingTrivia(1))
                                Case VB.SyntaxKind.LineContinuationTrivia
                                    If RemoveLastLineContinuation Then
                                        newTrailingTrivia = newTrailingTrivia.Add(TrailingTrivia(0))
                                        newTrailingTrivia = newTrailingTrivia.Add(TrailingTrivia(1))
                                    End If
                                Case Else
                                    Stop
                                    Throw UnexpectedValue($"TrailingTrivia(1).RawKind = {TrailingTrivia(1).RawKind}")
                            End Select
                        Case VB.SyntaxKind.EndOfLineTrivia
                            If TrailingTrivia(1).IsKind(VB.SyntaxKind.WhitespaceTrivia, VB.SyntaxKind.EndOfLineTrivia) Then
                                Return node.WithTrailingTrivia(VBEOLTrivia)
                            ElseIf TrailingTrivia(1).IsCommentOrDirectiveTrivia Then
                                Return node.WithAppendedEOL
                            End If
                            Stop
                            Throw UnexpectedValue($"{TrailingTrivia(1).RawKind}")
                        Case VB.SyntaxKind.CommentTrivia
                            If TrailingTrivia(1).IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                                newTrailingTrivia = newTrailingTrivia.Add(VBSpaceTrivia)
                            End If
                            newTrailingTrivia = newTrailingTrivia.Add(TrailingTrivia(0))
                            ' EOL added below
                        Case Else
                            Stop
                            Throw UnexpectedValue($"{TrailingTrivia(0).RawKind}")
                    End Select

                    Return node.WithTrailingTrivia(newTrailingTrivia.Add(VBEOLTrivia))
                Case Else
                    For Each e As IndexClass(Of SyntaxTrivia) In TrailingTrivia.WithIndex
                        Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(TrailingTrivia, e.Index, LookaheadCount:=1)
                        Select Case e.Value.RawKind
                            Case VB.SyntaxKind.WhitespaceTrivia
                                Select Case nextTrivia.RawKind
                                    Case VB.SyntaxKind.WhitespaceTrivia
                                        Dim MaxTrivia As SyntaxTrivia
                                        While nextTrivia.IsKind(VB.SyntaxKind.WhitespaceTrivia)
                                            MaxTrivia = AdjustWhitespace(e.Value, nextTrivia, afterLineContinue:=False)
                                            e.MoveNext()
                                            nextTrivia = GetForwardTriviaOrDefault(TrailingTrivia, e.Index, LookaheadCount:=1)
                                        End While
                                        If e.IsLast Then
                                            newTrailingTrivia = newTrailingTrivia.Add(VBEOLTrivia)
                                        Else
                                            newTrailingTrivia = newTrailingTrivia.Add(MaxTrivia)
                                        End If
                                    Case VB.SyntaxKind.EndOfLineTrivia
                           ' Ignore white space before EOL
                                    Case VB.SyntaxKind.LineContinuationTrivia
                                        newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                                    Case VB.SyntaxKind.CommentTrivia
                                        newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                                    Case VB.SyntaxKind.None
                                        newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                                    Case VB.SyntaxKind.RegionDirectiveTrivia,
                             VB.SyntaxKind.EndRegionDirectiveTrivia
                                        newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                                    Case Else
                                        Stop
                                        Throw UnexpectedValue($"{nextTrivia.RawKind}")
                                End Select
                            Case VB.SyntaxKind.EndOfLineTrivia
                                If Not (nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) OrElse newTrailingTrivia.LastOrDefault.IsKind(VB.SyntaxKind.EndOfLineTrivia)) Then
                                    newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                                End If
                            Case VB.SyntaxKind.LineContinuationTrivia
                                Select Case nextTrivia.RawKind
                                    Case VB.SyntaxKind.WhitespaceTrivia
                                        newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                                    Case VB.SyntaxKind.EndOfLineTrivia
                                        newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                                    Case VB.SyntaxKind.CommentTrivia
                                        newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                                    Case VB.SyntaxKind.LineContinuationTrivia
                                        ' ignore
                                    Case Else
                                        Stop
                                        Throw UnexpectedValue($"{nextTrivia.RawKind}")
                                End Select
                            Case VB.SyntaxKind.CommentTrivia,
                     VB.SyntaxKind.DocumentationCommentExteriorTrivia,
                     VB.SyntaxKind.DocumentationCommentTrivia
                                Select Case nextTrivia.RawKind
                                    Case VB.SyntaxKind.WhitespaceTrivia
                                        newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                                        If GetForwardTriviaOrDefault(TrailingTrivia, e.Index, LookaheadCount:=2).IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                                            newTrailingTrivia = newTrailingTrivia.Add(VBEOLTrivia)
                                        Else
                                            e.MoveNext()
                                            If e.IsLast Then
                                                newTrailingTrivia = newTrailingTrivia.Add(VBEOLTrivia)
                                            End If
                                        End If
                                    Case VB.SyntaxKind.EndOfLineTrivia
                                        newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                                    Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentExteriorTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                                        newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                                        newTrailingTrivia = newTrailingTrivia.Add(VBEOLTrivia)
                                    Case VB.SyntaxKind.None
                                        newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                                    Case Else
                                        Stop
                                        Throw UnexpectedValue($"{nextTrivia.RawKind}")
                                End Select
                            Case VB.SyntaxKind.IfDirectiveTrivia,
                     VB.SyntaxKind.DisabledTextTrivia,
                     VB.SyntaxKind.ElseIfDirectiveTrivia,
                     VB.SyntaxKind.ElseDirectiveTrivia,
                     VB.SyntaxKind.EndIfDirectiveTrivia,
                     VB.SyntaxKind.RegionDirectiveTrivia,
                     VB.SyntaxKind.EndRegionDirectiveTrivia
                                newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                                If Not nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                    newTrailingTrivia = newTrailingTrivia.Add(VBEOLTrivia)
                                End If
                            Case Else
                                Stop
                                Throw UnexpectedValue($"{nextTrivia.RawKind}")
                        End Select
                    Next
                    If RemoveLastLineContinuation Then
                        Return node.WithTrailingTrivia(newTrailingTrivia.WithoutLastLineContinuation)
                    End If
                    Return node.WithTrailingTrivia(newTrailingTrivia)
            End Select
        End Function

        <Extension>
        Friend Function WithUniqueLeadingTrivia(Of T As VB.VisualBasicSyntaxNode)(Node As T, HeaderLeadingTrivia As SyntaxTriviaList) As T
            Dim NodeLeadingTrivia As SyntaxTriviaList = Node.GetLeadingTrivia
            If NodeLeadingTrivia.Count = 0 Then
                Return Node
            End If
            If NodeLeadingTrivia.First.Language = "C#" Then
                NodeLeadingTrivia = NodeLeadingTrivia.ConvertTriviaList
            End If
            If HeaderLeadingTrivia.Count = 0 Then
                Return Node
            End If

            If Not NodeLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                Return Node
            End If
            Dim index As Integer
            For index = 0 To HeaderLeadingTrivia.Count - 1
                If HeaderLeadingTrivia(index).RawKind <> NodeLeadingTrivia(index).RawKind Then
                    Exit For
                End If
                If HeaderLeadingTrivia(index).ToString <> NodeLeadingTrivia(index).ToString Then
                    Exit For
                End If
            Next
            Dim newLeadingTrivia As New SyntaxTriviaList
            For i As Integer = index To NodeLeadingTrivia.Count - 1
                If i <> 0 AndAlso i = index AndAlso NodeLeadingTrivia(i).IsKind(CS.SyntaxKind.EndOfLineTrivia) Then
                    Continue For
                End If
                newLeadingTrivia = newLeadingTrivia.Add(NodeLeadingTrivia(i))
            Next
            Return Node.WithLeadingTrivia(newLeadingTrivia)
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

            Dim trailingtrivia As SyntaxTriviaList = node.GetTrailingTrivia().Concat(trivia).ToSyntaxTriviaList
            If trailingtrivia.ContainsEOLTrivia Then
                Return node.WithTrailingTrivia(trailingtrivia).WithTrailingEOL(RemoveLastLineContinuation:=True)
            End If
            Return node.WithTrailingTrivia(trailingtrivia)
        End Function

#Region "WithConvertedTriviaFrom"

        <Extension>
        Friend Function WithConvertedTriviaFrom(Of T As SyntaxNode)(node As T, otherNode As SyntaxNode) As T
            If otherNode Is Nothing Then
                Return node
            End If
            If otherNode.HasLeadingTrivia Then
                node = node.WithLeadingTrivia(otherNode.GetLeadingTrivia.ConvertTriviaList())
            End If
            If Not otherNode.HasTrailingTrivia OrElse ParentHasSameTrailingTrivia(otherNode) Then
                Return node
            End If
            Dim trailingTrivia As SyntaxTriviaList = otherNode.GetTrailingTrivia.ConvertTriviaList()
            If trailingTrivia.ContainsEOLTrivia Then
                Return node.WithTrailingTrivia(trailingTrivia).WithTrailingEOL(RemoveLastLineContinuation:=True)
            End If
            Return node.WithTrailingTrivia(trailingTrivia)
        End Function

        <Extension>
        Public Function WithConvertedTriviaFrom(Of T As SyntaxNode)(node As T, otherToken As SyntaxToken) As T
            If node Is Nothing Then
                Throw New ArgumentException($"Parameter {NameOf(node)} Is Nothing")
            End If
            If otherToken.HasLeadingTrivia Then
                node = node.WithLeadingTrivia(otherToken.LeadingTrivia.ConvertTriviaList())
            End If
            If Not otherToken.HasTrailingTrivia Then
                Return node
            End If
            Dim trailingTrivia As SyntaxTriviaList = otherToken.TrailingTrivia.ConvertTriviaList()
            If trailingTrivia.ContainsEOLTrivia Then
                Return node.WithTrailingTrivia(trailingTrivia).WithTrailingEOL(RemoveLastLineContinuation:=True)
            End If
            Return node.WithTrailingTrivia(trailingTrivia)
        End Function

#End Region

#Region "WithConvertedLeadingTriviaFrom"

        <Extension>
        Friend Function WithConvertedLeadingTriviaFrom(Of T As SyntaxNode)(node As T, otherNode As SyntaxNode) As T
            If otherNode Is Nothing OrElse Not otherNode.HasLeadingTrivia Then
                Return node
            End If
            Return node.WithLeadingTrivia(otherNode.GetLeadingTrivia.ConvertTriviaList())
        End Function

        <Extension>
        Friend Function WithConvertedLeadingTriviaFrom(Of T As SyntaxNode)(node As T, otherToken As SyntaxToken) As T
            If Not otherToken.HasLeadingTrivia Then
                Return node
            End If
            Return node.WithLeadingTrivia(otherToken.LeadingTrivia.ConvertTriviaList())
        End Function

        <Extension>
        Public Function WithConvertedLeadingTriviaFrom(node As SyntaxToken, otherToken As SyntaxToken) As SyntaxToken
            If Not otherToken.HasLeadingTrivia Then
                Return node
            End If
            Return node.WithLeadingTrivia(otherToken.LeadingTrivia.ConvertTriviaList())
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
            Dim trailingTrivia As SyntaxTriviaList = otherNode.GetTrailingTrivia.ConvertTriviaList().ToSyntaxTriviaList
            If trailingTrivia.ContainsEOLTrivia Then
                Return node.WithTrailingTrivia(trailingTrivia).WithTrailingEOL(RemoveLastLineContinuation:=True)
            End If
            Return node.WithTrailingTrivia(trailingTrivia)
        End Function

        <Extension>
        Friend Function WithConvertedTrailingTriviaFrom(Of T As SyntaxNode)(node As T, otherToken As SyntaxToken) As T
            If Not otherToken.HasTrailingTrivia Then
                Return node
            End If
            Dim Trailingtrivia As SyntaxTriviaList = otherToken.TrailingTrivia.ConvertTriviaList()
            If Trailingtrivia.ContainsEOLTrivia Then
                Return node.WithTrailingTrivia(Trailingtrivia).WithTrailingEOL(RemoveLastLineContinuation:=True)
            End If
            Return node.WithTrailingTrivia(Trailingtrivia)
        End Function

#End Region

    End Module
End Namespace
