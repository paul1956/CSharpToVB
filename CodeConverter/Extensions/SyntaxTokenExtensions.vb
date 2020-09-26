' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports CSharpToVBConverter.ToVisualBasic
Imports CSharpToVBConverter.ToVisualBasic.AttributeAndModifierSupport
Imports Microsoft.CodeAnalysis
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic

Namespace CSharpToVBConverter
    Public Module SyntaxTokenExtensions

        <Extension>
        Private Function RestructureModifierLeadingTrivia(Modifier As SyntaxToken, i As Integer, ByRef LeadingTriviaNotHandled As Boolean, ByRef StatementLeadingTrivia As SyntaxTriviaList, ByRef StatementTrailingTrivia As SyntaxTriviaList) As SyntaxTriviaList
            Dim NewModifierLeadingTrivia As New SyntaxTriviaList
            If LeadingTriviaNotHandled Then
                If i = 0 Then
                    StatementLeadingTrivia = StatementLeadingTrivia.AddRange(Modifier.LeadingTrivia)
                Else
                    NewModifierLeadingTrivia = RelocateDirectiveDisabledTrivia(Modifier.LeadingTrivia, StatementLeadingTrivia, RemoveEOL:=True)
                End If
            Else
                If i = 0 Then
                    If Modifier.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        StatementTrailingTrivia = StatementTrailingTrivia.Add(VBEOLTrivia)
                    End If
                    StatementTrailingTrivia = StatementTrailingTrivia.AddRange(Modifier.LeadingTrivia)
                Else
                    NewModifierLeadingTrivia = RelocateDirectiveDisabledTrivia(Modifier.LeadingTrivia, StatementTrailingTrivia, RemoveEOL:=True)
                End If

            End If
            LeadingTriviaNotHandled = StatementLeadingTrivia.Count = 0
            Return NewModifierLeadingTrivia
        End Function

        <Extension>
        Friend Function AdjustTokenLeadingTrivia(Token As SyntaxToken) As SyntaxToken
            Dim newLeadingTrivia As New SyntaxTriviaList
            Dim initialTrivia As SyntaxTriviaList = Token.LeadingTrivia
            'Debug.WriteLine($"Leading  Token({sourceLineNumber}) In :{Token.ToFullString}")

            For Each e As IndexClass(Of SyntaxTrivia) In initialTrivia.WithIndex
                Dim nextTrivia As SyntaxTrivia = initialTrivia.GetForwardTriviaOrDefault(e.Index, LookaheadCount:=1)
                Select Case e.Value.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        Select Case nextTrivia.RawKind
                            Case VB.SyntaxKind.CommentTrivia,
                             VB.SyntaxKind.DocumentationCommentTrivia
                                newLeadingTrivia = newLeadingTrivia.AddRange({VBSpaceTrivia,
                                                                    LineContinuation,
                                                                    e.Value}
                                                                   )
                            Case VB.SyntaxKind.WhitespaceTrivia
                            Case VB.SyntaxKind.None
                                If e.IsFirst Then
                                    newLeadingTrivia = newLeadingTrivia.Add(e.Value)
                                End If
                            Case Else
                                Stop
                        End Select
                    Case VB.SyntaxKind.CommentTrivia
                        If e.IsFirst Then
                            newLeadingTrivia = newLeadingTrivia.AddRange({VBSpaceTrivia, LineContinuation})
                        End If
                        newLeadingTrivia = newLeadingTrivia.Add(e.Value)
                    Case VB.SyntaxKind.EndOfLineTrivia
                        If e.IsFirst Then
                            newLeadingTrivia = newLeadingTrivia.Add(VBSpaceTrivia)
                            newLeadingTrivia = newLeadingTrivia.Add(LineContinuation)
                        End If
                        If Not nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            newLeadingTrivia = newLeadingTrivia.Add(e.Value)
                        End If
                    Case VB.SyntaxKind.LineContinuationTrivia
                        newLeadingTrivia = newLeadingTrivia.Add(e.Value)
                    Case Else
                        Stop
                End Select
            Next
            'Debug.WriteLine($"Leading  Token({sourceLineNumber}) Out:{Token.ToFullString}")
            Return Token.WithLeadingTrivia(newLeadingTrivia)
        End Function

        <Extension>
        Friend Function AdjustTokenTrailingTrivia(Token As SyntaxToken, RemoveTrailingLineContinuation As Boolean) As SyntaxToken
            Dim newTrailingTrivia As New SyntaxTriviaList
            Dim initialTrivia As SyntaxTriviaList = Token.TrailingTrivia

            'Debug.WriteLine($"Trailing Token({sourceLineNumber}) In :{Token.ToFullString}")
            For Each e As IndexClass(Of SyntaxTrivia) In initialTrivia.WithIndex
                Dim nextTrivia As SyntaxTrivia = initialTrivia.GetForwardTriviaOrDefault(e.Index, LookaheadCount:=1)
                Select Case e.Value.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        If nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            Continue For
                        End If
                        newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                    Case VB.SyntaxKind.CommentTrivia
                        newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                        If Not nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            newTrailingTrivia = newTrailingTrivia.Add(VBEOLTrivia)
                        End If
                    Case VB.SyntaxKind.EndOfLineTrivia
                        If e.IsFirst Then
                            newTrailingTrivia = newTrailingTrivia.Add(VBSpaceTrivia)
                            newTrailingTrivia = newTrailingTrivia.Add(LineContinuation)
                        End If
                        If Not nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                        End If
                    Case VB.SyntaxKind.LineContinuationTrivia
                        newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                    Case Else
                        Stop
                End Select
            Next
            'Debug.WriteLine($"Trailing Token({sourceLineNumber}) Out:{Token.ToFullString}")
            If RemoveTrailingLineContinuation Then
                Return Token.WithTrailingTrivia(newTrailingTrivia.WithoutLastLineContinuation)
            End If
            Return Token.WithTrailingTrivia(newTrailingTrivia)
        End Function

        ''' <summary>
        ''' Combines leading and trailing trivia for a Token
        ''' </summary>
        ''' <param name="Token"></param>
        ''' <param name="GetLeading"></param>
        ''' <param name="GetTrailing"></param>
        ''' <returns>New SyntaxTriviaList</returns>
        <Extension>
        Friend Function CollectConvertedTokenTrivia(Token As SyntaxToken, GetLeading As Boolean, GetTrailing As Boolean) As SyntaxTriviaList
            Dim CombinedTrivia As New SyntaxTriviaList
            Dim leadingTrivia As SyntaxTriviaList = Token.LeadingTrivia
            If GetLeading Then
                If leadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    CombinedTrivia = CombinedTrivia.AddRange(Token.LeadingTrivia.ConvertTriviaList())
                ElseIf leadingTrivia.ContainsEOLTrivia Then
                    CombinedTrivia = CombinedTrivia.Add(VBEOLTrivia)
                End If
            End If
            If GetTrailing Then
                Dim trailingTrivia As SyntaxTriviaList = Token.TrailingTrivia
                If trailingTrivia.ContainsCommentOrDirectiveTrivia Then
                    CombinedTrivia = CombinedTrivia.AddRange(trailingTrivia.ConvertTriviaList())
                ElseIf trailingTrivia.ContainsEOLTrivia AndAlso Not CombinedTrivia.ContainsEOLTrivia Then
                    CombinedTrivia = CombinedTrivia.Add(VBEOLTrivia)
                End If
            End If
            Return CombinedTrivia
        End Function

        <Extension>
        Friend Function ContainsEOLTrivia(Token As SyntaxToken) As Boolean
            If Not Token.HasTrailingTrivia Then
                Return False
            End If
            For Each t As SyntaxTrivia In Token.TrailingTrivia
                If t.IsEndOfLine Then
                    Return True
                End If
            Next
            Return False
        End Function

        <Extension>
        Friend Function ConvertAndModifyTokenTrivia(Token As SyntaxToken, NodesOrTokens As List(Of SyntaxNodeOrToken), Index As Integer) As SyntaxToken
            If NodesOrTokens Is Nothing Then
                Throw New ArgumentNullException(NameOf(NodesOrTokens))
            End If
            Dim initialTriviaList As SyntaxTriviaList = NodesOrTokens(Index).GetLeadingTrivia.ConvertTriviaList()
            Dim initialTriviaListUBound As Integer = initialTriviaList.Count - 1
            Dim afterWhiteSpace As Boolean = False
            Dim afterLineContinuation As Boolean = False
            Dim finalLeadingTrivia As New SyntaxTriviaList
            For initialTriviaIndex As Integer = 0 To initialTriviaListUBound
                Dim Trivia As SyntaxTrivia = initialTriviaList(initialTriviaIndex)
                Select Case Trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        afterLineContinuation = False
                        afterWhiteSpace = True
                        finalLeadingTrivia = finalLeadingTrivia.Add(Trivia)
                    Case VB.SyntaxKind.EndOfLineTrivia
                        finalLeadingTrivia = finalLeadingTrivia.Add(Trivia)
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
                                    finalLeadingTrivia = finalLeadingTrivia.Add(VBSpaceTrivia)
                                Else
                                    finalLeadingTrivia = finalLeadingTrivia.Add(Factory.WhitespaceTrivia(newWhiteSpaceString))
                                End If
                                finalLeadingTrivia = finalLeadingTrivia.Add(LineContinuation)
                                afterLineContinuation = True
                            Else
                                If Not String.IsNullOrWhiteSpace(newWhiteSpaceString) Then
                                    finalLeadingTrivia = finalLeadingTrivia.Add(Factory.WhitespaceTrivia(newWhiteSpaceString))
                                End If
                            End If
                        End If
                    Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                        If Not afterWhiteSpace Then
                            finalLeadingTrivia = finalLeadingTrivia.Add(VBSpaceTrivia)
                        End If
                        If Not afterLineContinuation Then
                            finalLeadingTrivia = finalLeadingTrivia.Add(LineContinuation)
                            finalLeadingTrivia = finalLeadingTrivia.Add(VBSpaceTrivia)
                        End If
                        finalLeadingTrivia = finalLeadingTrivia.Add(Trivia)
                        afterLineContinuation = False
                        afterWhiteSpace = False
                    Case VB.SyntaxKind.EndIfDirectiveTrivia
                        finalLeadingTrivia = finalLeadingTrivia.AddRange(DirectiveNotAllowedHere(Trivia))
                        finalLeadingTrivia = finalLeadingTrivia.Add(VBEOLTrivia)
                        afterLineContinuation = False
                        afterWhiteSpace = False
                    Case Else
                        Stop
                End Select
            Next
            initialTriviaList = NodesOrTokens(Index).GetTrailingTrivia.ConvertTriviaList()
            Dim finalTrailingTrivia As SyntaxTriviaList
            For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                Dim Trivia As SyntaxTrivia = e.Value
                Select Case Trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        finalTrailingTrivia = finalTrailingTrivia.Add(Trivia)
                        afterWhiteSpace = True
                    Case VB.SyntaxKind.EndOfLineTrivia
                        finalTrailingTrivia = finalTrailingTrivia.Add(Trivia)
                        afterWhiteSpace = False
                    Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                        If Not afterWhiteSpace = True Then
                            finalTrailingTrivia = finalTrailingTrivia.Add(VBSpaceTrivia)
                        End If
                        finalTrailingTrivia = finalTrailingTrivia.AddRange({LineContinuation, Trivia})
                        afterWhiteSpace = False
                    Case Else
                        Stop
                End Select
            Next
            Return Token.With(finalLeadingTrivia, finalTrailingTrivia)
        End Function

        <Extension>
        Friend Function ConvertModifier(CSToken As SyntaxToken, IsModule As Boolean, context As TokenContext, ByRef FoundVisibility As Boolean) As SyntaxToken
            If CSToken.Language <> "C#" Then
                Throw New ArgumentException($"Invalid language {CSToken.Language}, for parameter,", NameOf(CSToken))
            End If
            Dim Token As SyntaxToken = CS.CSharpExtensions.Kind(CSToken).GetVisibilityKeyword(IsModule, context, FoundVisibility)
            If Token.IsKind(VB.SyntaxKind.EmptyToken) Then
                Return EmptyToken.WithConvertedLeadingTriviaFrom(CSToken)
            End If
            Return Token.WithConvertedTriviaFrom(CSToken)
        End Function

        <Extension>
        Friend Function ConvertToInterpolatedStringTextToken(Token As SyntaxToken) As SyntaxToken
            If Token.Language <> "C#" Then
                Throw New ArgumentException($"Invalid language {Token.Language}, for parameter", NameOf(Token))
            End If
            Dim TokenString As String = ConvertCSharpEscapes(Token.ValueText)
            Return Factory.InterpolatedStringTextToken(TokenString, TokenString)
        End Function

        ''' <summary>
        ''' Returns the token after this token in the syntax tree.
        ''' </summary>
        ''' <param name="predicate">Delegate applied to each token.  The token is returned if the predicate returns
        ''' true.</param>
        ''' <param name="stepInto">Delegate applied to trivia.  If this delegate is present then trailing trivia is
        ''' included in the search.</param>
        <Extension>
        Friend Function GetNextToken(Token As SyntaxToken, predicate As Func(Of SyntaxToken, Boolean), Optional stepInto As Func(Of SyntaxTrivia, Boolean) = Nothing) As SyntaxToken
            If Token = Nothing Then
                Return Nothing
            End If

            Return SyntaxNavigator.s_instance.GetNextToken(Token, predicate, stepInto)
        End Function

        Friend Function GetSymbolTableEntry(csIdentifier As SyntaxToken, BaseVBIdent As String, Node As CS.CSharpSyntaxNode, Model As SemanticModel, IsQualifiedNameOrTypeName As Boolean, isField As Boolean) As (IdentToken As SyntaxToken, MeNeeded As Boolean)
            If s_usedIdentifiers.ContainsKey(BaseVBIdent) Then
                Dim symbolTableEntry As SymbolTableEntry = s_usedIdentifiers(BaseVBIdent)
                Return (Factory.Identifier(symbolTableEntry.Name).WithConvertedTriviaFrom(csIdentifier), symbolTableEntry.isProperty)
            End If
            For Each ident As KeyValuePair(Of String, SymbolTableEntry) In s_usedIdentifiers
                If String.Compare(ident.Key, BaseVBIdent, ignoreCase:=False, Globalization.CultureInfo.InvariantCulture) = 0 Then
                    ' We have an exact match keep looking
                    Return (Factory.Identifier(ident.Key), ident.Value.isProperty)
                End If
                If String.Compare(ident.Key, BaseVBIdent, ignoreCase:=True, Globalization.CultureInfo.InvariantCulture) = 0 Then
                    ' If we are here we have seen the variable in a different case so fix it
                    Dim NewUniqueName As String = BaseVBIdent.GetNewUniqueName(IsQualifiedNameOrTypeName, Node.GetScopingBlock, Model)
                    If s_usedIdentifiers(ident.Key).IsType Then
                        s_usedIdentifiers.Add(BaseVBIdent, New SymbolTableEntry(Name:=NewUniqueName, IsType:=False, isField))
                    Else
                        s_usedIdentifiers.Add(BaseVBIdent, New SymbolTableEntry(Name:=NewUniqueName, IsQualifiedNameOrTypeName, isField))
                    End If
                    Dim symbolTableEntry As SymbolTableEntry = s_usedIdentifiers(BaseVBIdent)
                    Return (Factory.Identifier(symbolTableEntry.Name).WithConvertedTriviaFrom(csIdentifier), symbolTableEntry.isProperty)
                End If
            Next
            Dim newIdentifier As String = BaseVBIdent
            s_usedIdentifiers.Add(BaseVBIdent, New SymbolTableEntry(newIdentifier, IsQualifiedNameOrTypeName, isField))
            Return (Factory.Identifier(newIdentifier), False)
        End Function

        <Extension>
        Friend Function IsKind(Token As SyntaxToken, ParamArray kinds() As CS.SyntaxKind) As Boolean
            Return kinds.Contains(CType(Token.RawKind, CS.SyntaxKind))
        End Function

        <Extension>
        Friend Function IsKind(Token As SyntaxToken, ParamArray kinds() As VB.SyntaxKind) As Boolean
            Return kinds.Contains(CType(Token.RawKind, VB.SyntaxKind))
        End Function

        <Extension>
        Friend Function MakeIdentifierUnique(csIdentifier As SyntaxToken, Node As CS.CSharpSyntaxNode, Model As SemanticModel, IsBracketNeeded As Boolean, IsQualifiedNameOrTypeName As Boolean) As SyntaxToken
            Dim isField As Boolean = Node.AncestorsAndSelf().OfType(Of CSS.FieldDeclarationSyntax).Any And Not IsQualifiedNameOrTypeName
            Dim BaseVBIdent As String = If(IsBracketNeeded, $"[{csIdentifier.ValueText}]", csIdentifier.ValueText)
            If BaseVBIdent = "_" Then
                BaseVBIdent = "__"
            End If
            ' Don't Change Qualified Names
            If IsQualifiedNameOrTypeName Then
                If Not s_usedIdentifiers.ContainsKey(BaseVBIdent) Then
                    s_usedIdentifiers.Add(BaseVBIdent, New SymbolTableEntry(BaseVBIdent, IsType:=True, isField))
                End If
                Return Factory.Identifier(BaseVBIdent).WithConvertedTriviaFrom(csIdentifier)
            End If

            Return GetSymbolTableEntry(csIdentifier, BaseVBIdent, Node, Model, IsQualifiedNameOrTypeName, isField).IdentToken
        End Function

        ''' <summary>
        ''' Remove directive trivia
        ''' </summary>
        ''' <param name="Token"></param>
        ''' <returns></returns>
        <Extension>
        Friend Function RemoveDirectiveTrivia(Token As SyntaxToken, ByRef FoundEOL As Boolean) As SyntaxToken
            Dim newLeadingTrivia As SyntaxTriviaList
            Dim NewTrailingTrivia As SyntaxTriviaList

            For Each trivia As SyntaxTrivia In Token.LeadingTrivia
                Select Case trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia, VB.SyntaxKind.CommentTrivia
                        newLeadingTrivia = newLeadingTrivia.Add(trivia)
                        FoundEOL = False
                    Case VB.SyntaxKind.EndOfLineTrivia
                        If Not FoundEOL Then
                            newLeadingTrivia = newLeadingTrivia.Add(trivia)
                            FoundEOL = True
                        End If
                    Case VB.SyntaxKind.DisabledTextTrivia,
                    VB.SyntaxKind.IfDirectiveTrivia,
                    VB.SyntaxKind.ElseDirectiveTrivia,
                    VB.SyntaxKind.ElseIfDirectiveTrivia,
                    VB.SyntaxKind.EndIfDirectiveTrivia
                        ' skip
                    Case Else
                        Stop
                End Select
            Next

            For Each trivia As SyntaxTrivia In Token.TrailingTrivia
                Select Case trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia, VB.SyntaxKind.CommentTrivia
                        NewTrailingTrivia = NewTrailingTrivia.Add(trivia)
                        FoundEOL = False
                    Case VB.SyntaxKind.EndOfLineTrivia
                        If Not FoundEOL Then
                            NewTrailingTrivia = NewTrailingTrivia.Add(trivia)
                            FoundEOL = True
                        End If
                    Case VB.SyntaxKind.DisableWarningDirectiveTrivia,
                     VB.SyntaxKind.IfDirectiveTrivia,
                     VB.SyntaxKind.ElseDirectiveTrivia,
                     VB.SyntaxKind.ElseIfDirectiveTrivia,
                     VB.SyntaxKind.EndIfDirectiveTrivia
                        ' skip
                    Case Else
                        Stop
                End Select
            Next

            Return Token.With(newLeadingTrivia, NewTrailingTrivia)
        End Function

        <Extension>
        Friend Function RemoveExtraEOL(Token As SyntaxToken) As SyntaxToken
            Dim initialTriviaList As SyntaxTriviaList = Token.LeadingTrivia
            Select Case initialTriviaList.Count
                Case 0
                    Return Token
                Case 1
                    If initialTriviaList.First.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                        Return Token.WithLeadingTrivia(New SyntaxTriviaList)
                    End If
                Case 2
                    Select Case initialTriviaList.First.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            If initialTriviaList.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                Return Token.WithLeadingTrivia(New SyntaxTriviaList)
                            End If
                            Return Token
                        Case VB.SyntaxKind.EndOfLineTrivia
                            If initialTriviaList.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                Return Token.WithLeadingTrivia(VBEOLTrivia)
                            End If
                            Return Token.WithLeadingTrivia(initialTriviaList.Last)
                        Case Else
                            Stop
                    End Select
                    Stop
                Case Else
            End Select
            Dim newLeadingTrivia As New SyntaxTriviaList
            For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                Dim trivia As SyntaxTrivia = e.Value
                Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(initialTriviaList, e.Index, LookaheadCount:=1)
                If trivia.IsKind(VB.SyntaxKind.WhitespaceTrivia) AndAlso nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                    Continue For
                End If
                If trivia.IsKind(VB.SyntaxKind.CommentTrivia) AndAlso nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                    newLeadingTrivia = newLeadingTrivia.Add(trivia)
                    Continue For
                End If

                If trivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) AndAlso
                (nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) OrElse nextTrivia.IsDirective) Then
                    Continue For
                End If
                newLeadingTrivia = newLeadingTrivia.Add(trivia)
            Next

            Return Token.WithLeadingTrivia(newLeadingTrivia)
        End Function

        <Extension>
        Friend Function RestructureModifier(NodeModifier As SyntaxToken, i As Integer, ByRef AttributesNotFound As Boolean, ByRef StatementLeadingTrivia As SyntaxTriviaList, ByRef StatementTrailingTrivia As SyntaxTriviaList) As SyntaxToken
            If (Not AttributesNotFound) AndAlso NodeModifier.RawKind = VB.SyntaxKind.EmptyToken AndAlso NodeModifier.HasLeadingTrivia AndAlso NodeModifier.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                StatementTrailingTrivia = StatementTrailingTrivia.Add(VBEOLTrivia)
                StatementTrailingTrivia = StatementTrailingTrivia.AddRange(NodeModifier.LeadingTrivia)
                NodeModifier = NodeModifier.WithLeadingTrivia(VBSpaceTrivia)
            Else
                NodeModifier = NodeModifier.WithLeadingTrivia(NodeModifier.RestructureModifierLeadingTrivia(i, AttributesNotFound, StatementLeadingTrivia, StatementTrailingTrivia))
            End If
            If NodeModifier.TrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                StatementLeadingTrivia = StatementLeadingTrivia.AddRange(RelocateDirectiveDisabledTrivia(NodeModifier.TrailingTrivia, StatementTrailingTrivia, RemoveEOL:=False))
                If StatementLeadingTrivia.Any AndAlso StatementLeadingTrivia.Last.RawKind <> VB.SyntaxKind.EndOfLineTrivia Then
                    StatementLeadingTrivia = StatementLeadingTrivia.Add(VBEOLTrivia)
                End If
                Return NodeModifier.WithTrailingTrivia(VBSpaceTrivia)
            End If
            Return NodeModifier.WithTrailingTrivia(RelocateDirectiveDisabledTrivia(NodeModifier.TrailingTrivia, StatementTrailingTrivia, RemoveEOL:=True))
        End Function

#Region "With Extensions"

        <Extension>
        Friend Function [With](token As SyntaxToken, leading As SyntaxTrivia, trailing As SyntaxTrivia) As SyntaxToken
            Return token.WithLeadingTrivia(leading).WithTrailingTrivia(trailing)
        End Function

        <Extension>
        Friend Function [With](token As SyntaxToken, leading As SyntaxTriviaList, trailing As SyntaxTriviaList) As SyntaxToken
            Return token.WithLeadingTrivia(leading).WithTrailingTrivia(trailing)
        End Function

        <Extension>
        Friend Function WithAppendedTrailingTrivia(Token As SyntaxToken, TriviaList As IEnumerable(Of SyntaxTrivia)) As SyntaxToken
            Return Token.WithTrailingTrivia(Token.TrailingTrivia.Concat(TriviaList))
        End Function

        <Extension>
        Friend Function WithConvertedLeadingTriviaFrom(node As SyntaxToken, otherToken As SyntaxToken) As SyntaxToken
            If Not otherToken.HasLeadingTrivia Then
                Return node
            End If
            Return node.WithLeadingTrivia(otherToken.LeadingTrivia.ConvertTriviaList())
        End Function

        <Extension>
        Friend Function WithConvertedTrailingTriviaFrom(Token As SyntaxToken, otherToken As SyntaxToken) As SyntaxToken
            Return Token.WithTrailingTrivia(otherToken.TrailingTrivia.ConvertTriviaList())
        End Function

        <Extension>
        Friend Function WithConvertedTriviaFrom(Token As SyntaxToken, otherNode As SyntaxNode) As SyntaxToken
            If otherNode.HasLeadingTrivia Then
                Token = Token.WithLeadingTrivia(otherNode.GetLeadingTrivia.ConvertTriviaList())
            End If
            If Not otherNode.HasTrailingTrivia OrElse ParentHasSameTrailingTrivia(otherNode) Then
                Return Token
            End If
            Return Token.WithTrailingTrivia(otherNode.GetTrailingTrivia.ConvertTriviaList())
        End Function

        <Extension>
        Friend Function WithConvertedTriviaFrom(Token As SyntaxToken, otherToken As SyntaxToken) As SyntaxToken
            Try
                If otherToken.HasLeadingTrivia Then
                    Token = Token.WithLeadingTrivia(otherToken.LeadingTrivia.ConvertTriviaList())
                End If
                Return Token.WithTrailingTrivia(otherToken.TrailingTrivia.ConvertTriviaList())
            Catch ex As OperationCanceledException
                Throw
            Catch ex As Exception
                Stop
            End Try
        End Function

        ''' <summary>
        ''' Used for parameters and arguments where blank lines and
        ''' most directives are not allowed
        ''' </summary>
        ''' <param name="Token"></param>
        ''' <param name="LeadingToken"></param>
        ''' <param name="AfterEOL"></param>
        ''' <returns></returns>
        ''' <param name="RequireTrailingSpace"></param>
        <Extension>
        Friend Function WithModifiedTokenTrivia(Token As SyntaxToken, LeadingToken As Boolean, AfterEOL As Boolean, RequireTrailingSpace As Boolean) As SyntaxToken
            Dim afterWhiteSpace As Boolean = False
            Dim afterLineContinuation As Boolean = LeadingToken
            Dim initialTriviaList As SyntaxTriviaList
            Dim triviaListUBound As Integer
            Dim finalLeadingTrivia As New SyntaxTriviaList
            Dim finalTrailingTrivia As New SyntaxTriviaList
            If LeadingToken Then
                finalLeadingTrivia = finalLeadingTrivia.AddRange(Token.LeadingTrivia)
            Else
                initialTriviaList = Token.LeadingTrivia
                triviaListUBound = initialTriviaList.Count - 1
                For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                    Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(initialTriviaList, e.Index, LookaheadCount:=1)
                    Select Case e.Value.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            AfterEOL = False
                            afterLineContinuation = False
                            afterWhiteSpace = True
                            finalLeadingTrivia = finalLeadingTrivia.Add(e.Value)
                        Case VB.SyntaxKind.EndOfLineTrivia
                            afterLineContinuation = False
                            afterWhiteSpace = False
                            If AfterEOL Then
                                Continue For
                            End If
                            finalLeadingTrivia = finalLeadingTrivia.Add(e.Value)
                            ' What I do depends on whats next
                            If Not e.IsLast Then
                                Dim j As Integer
                                Dim NewWhiteSpaceString As String = ""
                                For j = e.Index + 1 To triviaListUBound
                                    If initialTriviaList(j).IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                                        NewWhiteSpaceString &= initialTriviaList(j).ToString
                                        e.MoveNext()
                                    Else
                                        Exit For
                                    End If
                                Next
                                If j < triviaListUBound AndAlso initialTriviaList(j).IsKind(VB.SyntaxKind.CommentTrivia) Then
                                    If String.IsNullOrWhiteSpace(NewWhiteSpaceString) Then
                                        finalLeadingTrivia = finalLeadingTrivia.Add(VBSpaceTrivia)
                                    Else
                                        finalLeadingTrivia = finalLeadingTrivia.Add(Factory.WhitespaceTrivia(NewWhiteSpaceString))
                                    End If
                                    finalLeadingTrivia = finalLeadingTrivia.Add(LineContinuation)
                                    afterLineContinuation = True
                                Else
                                    If Not String.IsNullOrWhiteSpace(NewWhiteSpaceString) Then
                                        finalLeadingTrivia = finalLeadingTrivia.Add(Factory.WhitespaceTrivia(NewWhiteSpaceString))
                                    End If
                                End If
                            End If
                        Case VB.SyntaxKind.CommentTrivia
                            AfterEOL = False
                            If Not afterWhiteSpace Then
                                finalLeadingTrivia = finalLeadingTrivia.Add(VBSpaceTrivia)
                            End If
                            If Not afterLineContinuation Then
                                finalLeadingTrivia = finalLeadingTrivia.Add(LineContinuation)
                                finalLeadingTrivia = finalLeadingTrivia.Add(VBSpaceTrivia)
                            End If
                            finalLeadingTrivia = finalLeadingTrivia.Add(e.Value)
                            afterLineContinuation = False
                            afterWhiteSpace = False
                        Case VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.DisabledTextTrivia
                            AfterEOL = False
                            finalLeadingTrivia = finalLeadingTrivia.AddRange(DirectiveNotAllowedHere(e.Value))
                            Select Case nextTrivia.RawKind
                                Case VB.SyntaxKind.None
                                    finalLeadingTrivia = finalLeadingTrivia.Add(VBEOLTrivia)
                                Case VB.SyntaxKind.WhitespaceTrivia
                                    finalLeadingTrivia = finalLeadingTrivia.Add(VBEOLTrivia)
                                Case VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.DisabledTextTrivia, VB.SyntaxKind.EndIfDirectiveTrivia
                                    finalLeadingTrivia = finalLeadingTrivia.Add(VBEOLTrivia)
                                Case Else
                                    Stop
                            End Select
                        Case VB.SyntaxKind.EndIfDirectiveTrivia
                            If Token.LeadingTrivia.ContainsDirectiveTrivia(VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.ElseIfDirectiveTrivia) Then
                                finalLeadingTrivia = finalLeadingTrivia.AddRange(DirectiveNotAllowedHere(e.Value))
                                Select Case nextTrivia.RawKind
                                    Case VB.SyntaxKind.None
                                        finalLeadingTrivia = finalLeadingTrivia.Add(VBEOLTrivia)
                                    Case VB.SyntaxKind.WhitespaceTrivia
                                        finalLeadingTrivia = finalLeadingTrivia.Add(VBEOLTrivia)
                                    Case VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.DisabledTextTrivia, VB.SyntaxKind.EndIfDirectiveTrivia
                                        finalLeadingTrivia = finalLeadingTrivia.Add(VBEOLTrivia)
                                    Case Else
                                        Stop
                                End Select
                                Continue For
                            End If
                            AfterEOL = False
                            finalTrailingTrivia = finalTrailingTrivia.Add(VBEOLTrivia)
                            finalTrailingTrivia = finalTrailingTrivia.Add(e.Value)
                        Case Else
                            Stop
                    End Select
                Next
            End If
            initialTriviaList = Token.TrailingTrivia
            triviaListUBound = initialTriviaList.Count - 1
            afterWhiteSpace = False
            afterLineContinuation = False

            If LeadingToken Then
                For index As Integer = 0 To triviaListUBound
                    Dim trivia As SyntaxTrivia = initialTriviaList(index)
                    Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(initialTriviaList, index, LookaheadCount:=1)
                    Select Case trivia.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            If nextTrivia.IsKind(VB.SyntaxKind.CommentTrivia) OrElse
                            nextTrivia.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                                finalTrailingTrivia = finalTrailingTrivia.Add(trivia)
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
                            If j = 0 OrElse (j < triviaListUBound AndAlso initialTriviaList(j).IsKind(VB.SyntaxKind.CommentTrivia)) Then
                                If Not afterLineContinuation Then
                                    If String.IsNullOrWhiteSpace(NewWhiteSpaceString) Then
                                        finalTrailingTrivia = finalTrailingTrivia.Add(VBSpaceTrivia)
                                    Else
                                        finalTrailingTrivia = finalTrailingTrivia.Add(Factory.WhitespaceTrivia(NewWhiteSpaceString))
                                    End If
                                    finalTrailingTrivia = finalTrailingTrivia.Add(LineContinuation)
                                End If
                                finalTrailingTrivia = finalTrailingTrivia.Add(trivia)
                                afterLineContinuation = True
                            Else
                                finalTrailingTrivia = finalTrailingTrivia.Add(trivia)
                                If Not String.IsNullOrWhiteSpace(NewWhiteSpaceString) Then
                                    finalTrailingTrivia = finalTrailingTrivia.Add(Factory.WhitespaceTrivia(NewWhiteSpaceString))
                                End If
                            End If
                        Case VB.SyntaxKind.CommentTrivia
                            If Not afterWhiteSpace Then
                                finalTrailingTrivia = finalTrailingTrivia.Add(VBSpaceTrivia)
                            End If
                            If Not afterLineContinuation Then
                                finalTrailingTrivia = finalTrailingTrivia.Add(LineContinuation)
                                finalTrailingTrivia = finalTrailingTrivia.Add(VBSpaceTrivia)
                            End If
                            finalTrailingTrivia = finalTrailingTrivia.Add(trivia)
                            afterLineContinuation = False
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
            Else
                finalTrailingTrivia = finalTrailingTrivia.AddRange(Token.TrailingTrivia)
            End If
            If RequireTrailingSpace AndAlso Not finalTrailingTrivia.FirstOrDefault.IsWhitespaceOrEndOfLine Then
                finalTrailingTrivia = finalTrailingTrivia.Insert(0, VBSpaceTrivia)
            End If
            Return Token.With(finalLeadingTrivia, finalTrailingTrivia)

#End Region

        End Function

        <Extension>
        Friend Function WithPrependedLeadingTrivia(Token As SyntaxToken, TriviaList As SyntaxTriviaList) As SyntaxToken
            If TriviaList.Count = 0 Then
                Return Token
            End If

            Return Token.WithLeadingTrivia(TriviaList.Concat(Token.LeadingTrivia))
        End Function

    End Module
End Namespace
