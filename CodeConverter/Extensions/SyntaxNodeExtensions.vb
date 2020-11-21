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
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
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
        Friend Function AdjustMergedStatementTrailingTrivia(InitialTriviaList As SyntaxTriviaList, TriviaListToMerge As SyntaxTriviaList) As SyntaxTriviaList
            Dim inputTrivia As SyntaxTriviaList = InitialTriviaList
            inputTrivia = inputTrivia.AddRange(TriviaListToMerge)
            Dim foundEOL As Boolean = False
            Dim foundWhiteSpace As Boolean = False
            Dim newTrailingTrivia As SyntaxTriviaList
            For Each e As IndexClass(Of SyntaxTrivia) In inputTrivia.WithIndex
                Dim trivia As SyntaxTrivia = e.Value
                Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(inputTrivia, e.index, LookaheadCount:=1)
                Select Case trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        If Not foundWhiteSpace AndAlso Not nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            newTrailingTrivia = newTrailingTrivia.Add(trivia)
                            foundEOL = False
                            foundWhiteSpace = True
                        End If
                    Case VB.SyntaxKind.EndOfLineTrivia
                        If Not foundEOL Then
                            newTrailingTrivia = newTrailingTrivia.Add(trivia)
                            foundEOL = True
                        End If
                        foundWhiteSpace = False
                    Case VB.SyntaxKind.CommentTrivia
                        newTrailingTrivia = newTrailingTrivia.Add(trivia)
                        If Not nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            newTrailingTrivia = newTrailingTrivia.Add(VBEOLTrivia)
                            foundEOL = True
                        End If
                        foundWhiteSpace = False
                    Case VB.SyntaxKind.EndRegionDirectiveTrivia
                        If Not foundEOL Then
                            newTrailingTrivia = newTrailingTrivia.Add(VBEOLTrivia)
                        End If
                        newTrailingTrivia = newTrailingTrivia.Add(trivia)
                        foundEOL = False
                        foundWhiteSpace = False
                    Case Else
                        If trivia.IsDirective Then
                            If Not foundEOL Then
                                newTrailingTrivia = newTrailingTrivia.Add(VBEOLTrivia)
                            End If
                            newTrailingTrivia = newTrailingTrivia.Add(trivia)
                            foundEOL = False
                            foundWhiteSpace = False
                        Else
                            Stop
                        End If
                End Select
            Next
            Return newTrailingTrivia
        End Function

        ''' <summary>
        ''' Used at the beginning of a statement to adjust trivia from two statement, the first
        ''' of which will be removed. Directives are allowed.
        ''' </summary>
        ''' <param name="InitialTriviaList"></param>
        ''' <param name="TriviaListToMerge"></param>
        ''' <returns>The merged trivia</returns>
        Private Function AdjustStatementLeadingTrivia(InitialTriviaList As SyntaxTriviaList, TriviaListToMerge As SyntaxTriviaList) As SyntaxTriviaList
            Dim inputTrivia As SyntaxTriviaList = inputTrivia.AddRange(InitialTriviaList)
            inputTrivia = inputTrivia.AddRange(TriviaListToMerge)
            Dim newLeadingTrivia As New SyntaxTriviaList
            Dim foundEOL As Boolean = True
            Dim foundWhiteSpace As Boolean = False
            For Each e As IndexClass(Of SyntaxTrivia) In inputTrivia.WithIndex
                Dim trivia As SyntaxTrivia = e.Value
                Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(InitialTriviaList, e.index, LookaheadCount:=1)
                Select Case trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        If Not (foundWhiteSpace OrElse nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia)) Then
                            newLeadingTrivia = newLeadingTrivia.Add(trivia)
                            foundEOL = False
                            foundWhiteSpace = True
                        End If
                    Case VB.SyntaxKind.EndOfLineTrivia
                        If Not foundEOL Then
                            newLeadingTrivia = newLeadingTrivia.Add(trivia)
                            foundEOL = True
                        End If
                        foundWhiteSpace = False
                    Case VB.SyntaxKind.CommentTrivia
                        newLeadingTrivia = newLeadingTrivia.Add(trivia)
                        If Not nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            newLeadingTrivia = newLeadingTrivia.Add(VBEOLTrivia)
                            foundEOL = True
                        End If
                        foundWhiteSpace = False
                    Case Else
                        If trivia.IsDirective Then
                            If Not foundEOL Then
                                newLeadingTrivia = newLeadingTrivia.Add(VBEOLTrivia)
                            End If
                            newLeadingTrivia = newLeadingTrivia.Add(trivia)
                            If Not nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                newLeadingTrivia = newLeadingTrivia.Add(VBEOLTrivia)
                                foundEOL = True
                            Else
                                foundEOL = False
                            End If
                            foundWhiteSpace = False
                        Else
                            Stop
                        End If
                End Select
            Next
            Return newLeadingTrivia
        End Function

        <Extension>
        Friend Function GetAncestor(Of TNode As SyntaxNode)(node As SyntaxNode) As TNode
            If node Is Nothing Then
                Return Nothing
            End If

            Return node.GetAncestors(Of TNode)().FirstOrDefault()
        End Function

        <Extension>
        Friend Iterator Function GetAncestors(Of TNode As SyntaxNode)(node As SyntaxNode) As IEnumerable(Of TNode)
            Dim current As SyntaxNode = node.Parent
            While current IsNot Nothing
                If TypeOf current Is TNode Then
                    Yield DirectCast(current, TNode)
                End If

                current = If(TypeOf current Is IStructuredTriviaSyntax, DirectCast(current, IStructuredTriviaSyntax).ParentTrivia.Token.Parent, current.Parent)
            End While
        End Function

        <Extension>
        Friend Function GetBraces(node As SyntaxNode) As ValueTuple(Of SyntaxToken, SyntaxToken)
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

        <Extension>
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
        Friend Function IsKind(node As SyntaxNode, ParamArray kind1() As VB.SyntaxKind) As Boolean
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
        <Extension>
        Friend Function RemoveExtraLeadingEOL(Of T As SyntaxNode)(node As T) As T
            Dim intialLeadingTrivia As SyntaxTriviaList = node.GetLeadingTrivia
            Select Case intialLeadingTrivia.Count
                Case 0
                    Return node
                Case 1
                    Select Case intialLeadingTrivia.First.RawKind
                        Case VB.SyntaxKind.EndOfLineTrivia
                            Return node.WithoutLeadingTrivia
                        Case VB.SyntaxKind.WhitespaceTrivia
                            Return node
                    End Select
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
                    End Select
                Case Else
            End Select
            Dim firstTrivia As Boolean = True
            Dim newLeadingTrivia As New SyntaxTriviaList
            For Each e As IndexClass(Of SyntaxTrivia) In intialLeadingTrivia.WithIndex
                Dim trivia As SyntaxTrivia = e.Value
                Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(intialLeadingTrivia, e.index, LookaheadCount:=1)
                If trivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) AndAlso (firstTrivia OrElse nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia)) Then
                    Continue For
                End If
                If trivia.IsKind(VB.SyntaxKind.WhitespaceTrivia) AndAlso nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                    Continue For
                End If

                firstTrivia = False
                newLeadingTrivia = newLeadingTrivia.Add(trivia)
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
            Dim nodeTypeIsTypeSyntax As Boolean = TypeOf node Is CSS.TypeSyntax
            Dim interfaceNode As CSS.TypeSyntax = If(nodeTypeIsTypeSyntax, CType(node, CSS.TypeSyntax), Nothing)
            If nodeTypeIsTypeSyntax AndAlso TypeOf interfaceNode.Parent Is CSS.BaseTypeSyntax AndAlso interfaceNode.Parent.IsParentKind(CS.SyntaxKind.BaseList) AndAlso CType(interfaceNode.Parent, CSS.BaseTypeSyntax).Type Is interfaceNode Then
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

        <Extension>
        Friend Function WithAppendedEOL(Of T As SyntaxNode)(node As T) As T
            Dim trailingTrivia As SyntaxTriviaList = node.GetTrailingTrivia()
            trailingTrivia = trailingTrivia.Add(VBEOLTrivia)
            Return node.WithTrailingTrivia(trailingTrivia)
        End Function

        <Extension>
        Friend Function WithAppendedTrailingTrivia(Of T As SyntaxNode)(node As T, trivia As SyntaxTriviaList) As T
            If trivia.Count = 0 Then
                Return node
            End If
            If node Is Nothing Then
                Return Nothing
            End If

            Dim trailingtrivia As SyntaxTriviaList = node.GetTrailingTrivia().Concat(trivia).ToSyntaxTriviaList
            If trailingtrivia.ContainsEOLTrivia Then
                Return node.WithTrailingTrivia(trailingtrivia).WithTrailingEOL
            End If
            Return node.WithTrailingTrivia(trailingtrivia)
        End Function

        <Extension>
        Friend Function WithAppendedTriviaFromEndOfDirectiveToken(Of T As SyntaxNode)(node As T, Token As SyntaxToken) As T
            Dim newTrailingTrivia As New SyntaxTriviaList
            If Token.HasLeadingTrivia Then
                newTrailingTrivia = Token.LeadingTrivia.ConvertTriviaList()
            End If
            If Token.HasTrailingTrivia Then
                newTrailingTrivia = newTrailingTrivia.AddRange(Token.TrailingTrivia.ConvertTriviaList())
            End If

            Return node.WithAppendedTrailingTrivia(newTrailingTrivia).WithTrailingEOL
        End Function

        ''' <summary>
        ''' Merge leading trivia
        ''' </summary>
        ''' <typeparam name="T"></typeparam>
        ''' <param name="node"></param>
        ''' <param name="TriviaListToMerge"></param>
        ''' <returns></returns>
        <Extension>
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
        <Extension>
        Friend Function WithMergedTrailingTrivia(Of T As SyntaxNode)(node As T, TriviaListToMerge As SyntaxTriviaList) As T
            If node Is Nothing Then
                Return Nothing
            End If
            If Not TriviaListToMerge.Any Then
                Return node
            End If
            Dim nodeTrailingTrivia As SyntaxTriviaList = node.GetTrailingTrivia
            If Not nodeTrailingTrivia.Any Then
                Return node.WithTrailingTrivia(TriviaListToMerge)
            End If
            ' Both nodes have trivia
            Return node.WithTrailingTrivia(AdjustMergedStatementTrailingTrivia(nodeTrailingTrivia, TriviaListToMerge))
        End Function

        <Extension>
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

        <Extension>
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

            Dim nodeTrailingTrivia As SyntaxTriviaList = node.GetTrailingTrivia
            If nodeTrailingTrivia.ContainsEOLTrivia Then
                Dim newTriviaList As SyntaxTriviaList
                For Each trivia As SyntaxTrivia In nodeTrailingTrivia
                    If trivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                        Continue For
                    End If
                    newTriviaList = newTriviaList.Add(trivia)
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
        <Extension>
        Friend Function WithTrailingEOL(Of T As SyntaxNode)(node As T, Optional RemoveLastLineContinuation As Boolean = True) As T
            If node Is Nothing Then
                Return Nothing
            End If
            Dim trailingTrivia As SyntaxTriviaList = node.GetTrailingTrivia
            Dim newTrailingTrivia As New SyntaxTriviaList
            Dim count As Integer = trailingTrivia.Count
            'RemoveLastLineContinuation = True
            Select Case trailingTrivia.Count
                Case 0
                    Return node.WithTrailingTrivia(VBEOLTrivia)
                Case 1
                    Select Case trailingTrivia(0).RawKind
                        Case VB.SyntaxKind.EndOfLineTrivia
                            Return node
                        Case VB.SyntaxKind.WhitespaceTrivia
                            Return node.WithTrailingTrivia(VBEOLTrivia)
                    End Select
                    Throw UnexpectedValue($"TrailingTrivia(0).RawKind = {trailingTrivia(0).RawKind}")
                Case 2
                    Select Case trailingTrivia(0).RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            Select Case trailingTrivia(1).RawKind
                                Case VB.SyntaxKind.WhitespaceTrivia, VB.SyntaxKind.EndOfLineTrivia
                                ' Replace Whitespace, Whitespace and Whitespace, EOL with just EOL
                                Case VB.SyntaxKind.CommentTrivia,
                             VB.SyntaxKind.EndRegionDirectiveTrivia
                                    newTrailingTrivia = newTrailingTrivia.Add(trailingTrivia(0))
                                    newTrailingTrivia = newTrailingTrivia.Add(trailingTrivia(1))
                                Case VB.SyntaxKind.LineContinuationTrivia
                                    If RemoveLastLineContinuation Then
                                        newTrailingTrivia = newTrailingTrivia.Add(trailingTrivia(0))
                                        newTrailingTrivia = newTrailingTrivia.Add(trailingTrivia(1))
                                    End If
                                Case Else
                                    Stop
                                    Throw UnexpectedValue($"TrailingTrivia(1).RawKind = {trailingTrivia(1).RawKind}")
                            End Select
                        Case VB.SyntaxKind.EndOfLineTrivia
                            If trailingTrivia(1).IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                                Return node.WithTrailingTrivia(VBEOLTrivia)
                            ElseIf trailingTrivia(1).IsEndOfLine Then
                                Return node
                            ElseIf trailingTrivia(1).IsCommentOrDirectiveTrivia Then
                                Return node.WithAppendedEOL
                            End If
                            Stop
                            Throw UnexpectedValue($"{trailingTrivia(1).RawKind}")
                        Case VB.SyntaxKind.CommentTrivia
                            If trailingTrivia(1).IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                                newTrailingTrivia = newTrailingTrivia.Add(Factory.Space)
                            End If
                            newTrailingTrivia = newTrailingTrivia.Add(trailingTrivia(0))
                            ' EOL added below
                        Case Else
                            Stop
                            Throw UnexpectedValue($"{trailingTrivia(0).RawKind}")
                    End Select

                    Return node.WithTrailingTrivia(newTrailingTrivia.Add(VBEOLTrivia))
                Case Else
                    For Each e As IndexClass(Of SyntaxTrivia) In trailingTrivia.WithIndex
                        Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(trailingTrivia, e.index, LookaheadCount:=1)
                        Select Case e.Value.RawKind
                            Case VB.SyntaxKind.WhitespaceTrivia
                                Select Case nextTrivia.RawKind
                                    Case VB.SyntaxKind.WhitespaceTrivia
                                        Dim maxTrivia As SyntaxTrivia
                                        While nextTrivia.IsKind(VB.SyntaxKind.WhitespaceTrivia)
                                            maxTrivia = AdjustWhitespace(e.Value, nextTrivia, afterLineContinue:=False)
                                            e.MoveNext()
                                            nextTrivia = GetForwardTriviaOrDefault(trailingTrivia, e.index, LookaheadCount:=1)
                                        End While
                                        If e.IsLast Then
                                            newTrailingTrivia = newTrailingTrivia.Add(VBEOLTrivia)
                                        Else
                                            newTrailingTrivia = newTrailingTrivia.Add(maxTrivia)
                                        End If
                                    Case VB.SyntaxKind.EndOfLineTrivia
                                        ' Ignore white space before EOL
                                    Case VB.SyntaxKind.LineContinuationTrivia
                                        newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                                    Case VB.SyntaxKind.CommentTrivia
                                        newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                                    Case VB.SyntaxKind.None
                                        ' Skip don't end with whitespace
                                    Case VB.SyntaxKind.RegionDirectiveTrivia,
                                         VB.SyntaxKind.EndRegionDirectiveTrivia,
                                         VB.SyntaxKind.ElseIfDirectiveTrivia,
                                         VB.SyntaxKind.ElseDirectiveTrivia,
                                         VB.SyntaxKind.EndIfDirectiveTrivia,
                                         VB.SyntaxKind.IfDirectiveTrivia
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
                                        If GetForwardTriviaOrDefault(trailingTrivia, e.index, LookaheadCount:=2).IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
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
                                    Case VB.SyntaxKind.EndIfDirectiveTrivia
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
                Return node.WithTrailingTrivia(trailingTrivia).WithTrailingEOL
            End If
            Return node.WithTrailingTrivia(trailingTrivia)
        End Function

        <Extension>
        Friend Function WithConvertedTriviaFrom(Of T As SyntaxNode)(node As T, otherToken As SyntaxToken) As T
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
                Return node.WithTrailingTrivia(trailingTrivia).WithTrailingEOL
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
                Return node.WithTrailingTrivia(trailingTrivia).WithTrailingEOL
            End If
            Return node.WithTrailingTrivia(trailingTrivia)
        End Function

        <Extension>
        Friend Function WithConvertedTrailingTriviaFrom(Of T As SyntaxNode)(node As T, otherToken As SyntaxToken) As T
            If Not otherToken.HasTrailingTrivia Then
                Return node
            End If
            Dim trailingtrivia As SyntaxTriviaList = otherToken.TrailingTrivia.ConvertTriviaList()
            If trailingtrivia.ContainsEOLTrivia Then
                Return node.WithTrailingTrivia(trailingtrivia).WithTrailingEOL
            End If
            Return node.WithTrailingTrivia(trailingtrivia)
        End Function

#End Region

    End Module
End Namespace
