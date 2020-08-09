' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports CSharpToVBCodeConverter
Imports CSharpToVBCodeConverter.ToVisualBasic
Imports CSharpToVBCodeConverter.ToVisualBasic.AttributeAndModifierSupport
Imports Microsoft.CodeAnalysis
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory

Public Module TokenExtensions

    <Extension>
    Private Function RestructureModifierLeadingTrivia(Modifier As SyntaxToken, i As Integer, ByRef LeadingTriviaNotHandled As Boolean, ByRef StatementLeadingTrivia As List(Of SyntaxTrivia), ByRef StatementTrailingTrivia As List(Of SyntaxTrivia)) As SyntaxTriviaList
        Dim NewModifierLeadingTrivia As New SyntaxTriviaList
        If LeadingTriviaNotHandled Then
            If i = 0 Then
                StatementLeadingTrivia.AddRange(Modifier.LeadingTrivia)
            Else
                NewModifierLeadingTrivia = RelocateDirectiveDisabledTrivia(Modifier.LeadingTrivia, StatementLeadingTrivia, RemoveEOL:=True)
            End If
        Else
            If i = 0 Then
                If Modifier.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    StatementTrailingTrivia.Add(VBEOLTrivia)
                End If
                StatementTrailingTrivia.AddRange(Modifier.LeadingTrivia)
            Else
                NewModifierLeadingTrivia = RelocateDirectiveDisabledTrivia(Modifier.LeadingTrivia, StatementTrailingTrivia, RemoveEOL:=True)
            End If

        End If
        LeadingTriviaNotHandled = StatementLeadingTrivia.Count = 0
        Return NewModifierLeadingTrivia
    End Function

    <Extension>
    Friend Function AdjustTokenTriviaWithLineContuations(Token As SyntaxToken) As SyntaxToken
        Dim leadingTrivia As New List(Of SyntaxTrivia)
        For Each e As IndexClass(Of SyntaxTrivia) In Token.LeadingTrivia.WithIndex
            If e.Value.IsComment Then
                If e.IsFirst Then
                    leadingTrivia.InsertRange(0, {SpaceTrivia, LineContinuation})
                ElseIf Token.LeadingTrivia(e.Index - 1).IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                    leadingTrivia(e.Index - 1) = leadingTrivia(e.Index - 1).AdjustWhitespace(2)
                    leadingTrivia.InsertRange(leadingTrivia.Count - 1, {SpaceTrivia, LineContinuation})
                Else
                    Stop
                End If
            End If
            leadingTrivia.Add(e.Value)
        Next
        Return Token.WithLeadingTrivia(leadingTrivia)
    End Function

    ''' <summary>
    ''' Returns new leading trivia for first Item in a List
    ''' This Trivia consists of all the OpenBrace Leading Trivia and the trailing Trivia only of does not just contain a CRLF
    ''' </summary>
    ''' <param name="Token"></param>
    ''' <returns>List(Of SyntaxTrivia) from OpenBrace</returns>
    ''' <param name="IsLeading"></param>
    <Extension>
    Friend Function CollectTokenTrivia(Token As SyntaxToken, IsLeading As Boolean) As List(Of SyntaxTrivia)
        Dim leadingTrivia As New List(Of SyntaxTrivia)
        If Token.HasLeadingTrivia Then
            leadingTrivia.AddRange(ConvertTrivia(Token.LeadingTrivia))
        End If
        Dim trailingTrivia As SyntaxTriviaList = Token.TrailingTrivia
        If Token.HasTrailingTrivia Then
            If Not IsLeading OrElse trailingTrivia.Count <> 1 OrElse Not trailingTrivia(0).IsEndOfLine Then
                leadingTrivia.AddRange(ConvertTrivia(trailingTrivia))
            End If
        End If
        Return leadingTrivia
    End Function

    <Extension>
    Friend Function ConvertModifier(CSToken As SyntaxToken, IsModule As Boolean, context As TokenContext, ByRef FoundVisibility As Boolean) As SyntaxToken
        If CSToken.Language <> "C#" Then
            Throw New ArgumentException($"Invalid language {CSToken.Language}, for parameter,", NameOf(CSToken))
        End If
        Dim Token As SyntaxToken = CS.CSharpExtensions.Kind(CSToken).ConvertModifierKindToVBKeyword(IsModule, context, FoundVisibility)
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
        Return VBFactory.InterpolatedStringTextToken(TokenString, TokenString)
    End Function

    <Extension>
    Friend Function MakeIdentifierUnique(IdentifierToken As SyntaxToken, IsBracketNeeded As Boolean, IsQualifiedNameOrTypeName As Boolean) As SyntaxToken
        Dim ConvertedIdentifier As String = If(IsBracketNeeded, $"[{IdentifierToken.ValueText}]", IdentifierToken.ValueText)
        If ConvertedIdentifier = "_" Then
            ConvertedIdentifier = "underscore"
        End If
        ' Don't Change Qualified Names
        If IsQualifiedNameOrTypeName Then
            If Not s_usedIdentifiers.ContainsKey(ConvertedIdentifier) Then
                s_usedIdentifiers.Add(ConvertedIdentifier, New SymbolTableEntry(ConvertedIdentifier, True))
            End If
            Return VBFactory.Identifier(s_usedIdentifiers(ConvertedIdentifier).Name).WithConvertedTriviaFrom(IdentifierToken)
        End If
        If s_usedIdentifiers.ContainsKey(ConvertedIdentifier) Then
            ' We have a case sensitive exact match so just return it
            Return VBFactory.Identifier(s_usedIdentifiers(ConvertedIdentifier).Name).WithConvertedTriviaFrom(IdentifierToken)
        End If
        Dim IsFieldIdentifier As Boolean = False
        If TypeOf IdentifierToken.Parent Is CSS.VariableDeclaratorSyntax Then
            Dim VarDecl As CSS.VariableDeclaratorSyntax = CType(IdentifierToken.Parent, CSS.VariableDeclaratorSyntax)
            Dim FieldDeclatationOrNothing As CSS.FieldDeclarationSyntax = VarDecl.FirstAncestorOrSelf(Of CSS.FieldDeclarationSyntax)
            If Char.IsLower(ConvertedIdentifier.Chars(0)) AndAlso FieldDeclatationOrNothing IsNot Nothing Then
                If FieldDeclatationOrNothing.Modifiers.Any AndAlso FieldDeclatationOrNothing.Modifiers.Contains(CS.SyntaxKind.PrivateKeyword) Then
                    IsFieldIdentifier = True
                End If
            End If
        End If
        For Each ident As KeyValuePair(Of String, SymbolTableEntry) In s_usedIdentifiers
            If String.Compare(ident.Key, ConvertedIdentifier, ignoreCase:=False, Globalization.CultureInfo.InvariantCulture) = 0 Then
                ' We have an exact match keep looking
                Continue For
            End If
            If String.Compare(ident.Key, ConvertedIdentifier, ignoreCase:=True, Globalization.CultureInfo.InvariantCulture) = 0 Then
                ' If we are here we have seen the variable in a different case so fix it
                Dim NewUniqueName As String = ConvertedIdentifier.GetNewUniqueName(ident)
                If s_usedIdentifiers(ident.Key).IsType Then
                    s_usedIdentifiers.Add(ConvertedIdentifier, New SymbolTableEntry(Name:=NewUniqueName, IsType:=False))
                Else
                    s_usedIdentifiers.Add(ConvertedIdentifier, New SymbolTableEntry(Name:=NewUniqueName, IsType:=IsQualifiedNameOrTypeName))
                End If
                Return VBFactory.Identifier(s_usedIdentifiers(ConvertedIdentifier).Name).WithConvertedTriviaFrom(IdentifierToken)
            End If
        Next
        Dim _ConvertedIdentifier As String = $"{If(IsFieldIdentifier, "_", "")}{ConvertedIdentifier}"
        s_usedIdentifiers.Add(ConvertedIdentifier, New SymbolTableEntry(_ConvertedIdentifier, IsQualifiedNameOrTypeName))
        Return VBFactory.Identifier(_ConvertedIdentifier)
    End Function

    <Extension>
    Friend Function RestructureModifier(NodeModifier As SyntaxToken, i As Integer, ByRef AttributesNotFound As Boolean, ByRef StatementLeadingTrivia As List(Of SyntaxTrivia), ByRef StatementTrailingTrivia As List(Of SyntaxTrivia)) As SyntaxToken
        If (Not AttributesNotFound) AndAlso NodeModifier.RawKind = VB.SyntaxKind.EmptyToken AndAlso NodeModifier.HasLeadingTrivia AndAlso NodeModifier.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
            StatementTrailingTrivia.Add(VBEOLTrivia)
            StatementTrailingTrivia.AddRange(NodeModifier.LeadingTrivia)
            NodeModifier = NodeModifier.WithLeadingTrivia(SpaceTrivia)
        Else
            NodeModifier = NodeModifier.WithLeadingTrivia(NodeModifier.RestructureModifierLeadingTrivia(i, AttributesNotFound, StatementLeadingTrivia, StatementTrailingTrivia))
        End If
        If NodeModifier.TrailingTrivia.ContainsCommentOrDirectiveTrivia Then
            StatementLeadingTrivia.AddRange(RelocateDirectiveDisabledTrivia(NodeModifier.TrailingTrivia, StatementTrailingTrivia, RemoveEOL:=False))
            If StatementLeadingTrivia.Any AndAlso StatementLeadingTrivia.Last.RawKind <> VB.SyntaxKind.EndOfLineTrivia Then
                StatementLeadingTrivia.Add(VBEOLTrivia)
            End If
            Return NodeModifier.WithTrailingTrivia(SpaceTrivia)
        End If
        Return NodeModifier.WithTrailingTrivia(RelocateDirectiveDisabledTrivia(NodeModifier.TrailingTrivia, StatementTrailingTrivia, RemoveEOL:=True))
    End Function

    <Extension>
    Friend Function WithConvertedTriviaFrom(Token As SyntaxToken, otherNode As SyntaxNode) As SyntaxToken
        If otherNode.HasLeadingTrivia Then
            Token = Token.WithLeadingTrivia(ConvertTrivia(otherNode.GetLeadingTrivia))
        End If
        If Not otherNode.HasTrailingTrivia OrElse ParentHasSameTrailingTrivia(otherNode) Then
            Return Token
        End If
        Return Token.WithTrailingTrivia(ConvertTrivia(otherNode.GetTrailingTrivia()))
    End Function

    <Extension>
    Friend Function WithConvertedTriviaFrom(Token As SyntaxToken, otherToken As SyntaxToken) As SyntaxToken
        Try
            If otherToken.HasLeadingTrivia Then
                Token = Token.WithLeadingTrivia(ConvertTrivia(otherToken.LeadingTrivia).ToList())
            End If
            Return Token.WithTrailingTrivia(ConvertTrivia(otherToken.TrailingTrivia))
        Catch ex As OperationCanceledException
            Throw
        Catch ex As Exception
            Stop
        End Try
    End Function

    <Extension>
    Friend Function WithPrependedLeadingTrivia(Token As SyntaxToken, Trivia As IEnumerable(Of SyntaxTrivia)) As SyntaxToken
        Return Token.WithPrependedLeadingTrivia(Trivia.ToSyntaxTriviaList())
    End Function

    <Extension>
    Public Function [With](token As SyntaxToken, leading As IEnumerable(Of SyntaxTrivia), trailing As IEnumerable(Of SyntaxTrivia)) As SyntaxToken
        Return token.WithLeadingTrivia(leading).WithTrailingTrivia(trailing)
    End Function

    <Extension>
    Public Function ContainsEOLTrivia(Token As SyntaxToken) As Boolean
        If Not Token.HasTrailingTrivia Then
            Return False
        End If
        Dim TriviaList As SyntaxTriviaList = Token.TrailingTrivia
        For Each t As SyntaxTrivia In TriviaList
            If t.IsEndOfLine Then
                Return True
            End If
        Next
        Return False
    End Function

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

    ''' <summary>
    ''' Returns the token after this token in the syntax tree.
    ''' </summary>
    ''' <param name="predicate">Delegate applied to each token.  The token is returned if the predicate returns
    ''' true.</param>
    ''' <param name="stepInto">Delegate applied to trivia.  If this delegate is present then trailing trivia is
    ''' included in the search.</param>
    <Extension>
    Public Function GetNextToken(Token As SyntaxToken, predicate As Func(Of SyntaxToken, Boolean), Optional stepInto As Func(Of SyntaxTrivia, Boolean) = Nothing) As SyntaxToken
        If Token = Nothing Then
            Return Nothing
        End If

        Return SyntaxNavigator.s_instance.GetNextToken(Token, predicate, stepInto)
    End Function

    <Extension>
    Public Function IsKind(Token As SyntaxToken, ParamArray kinds() As CS.SyntaxKind) As Boolean
        Return kinds.Contains(CType(Token.RawKind, CS.SyntaxKind))
    End Function

    <Extension>
    Public Function IsKind(Token As SyntaxToken, ParamArray kinds() As VB.SyntaxKind) As Boolean
        Return kinds.Contains(CType(Token.RawKind, VB.SyntaxKind))
    End Function

    ''' <summary>
    ''' Remove directive trivia
    ''' </summary>
    ''' <param name="Token"></param>
    ''' <returns></returns>
    <Extension>
    Public Function RemoveDirectiveTrivia(Token As SyntaxToken, ByRef FoundEOL As Boolean) As SyntaxToken
        Dim NewLeadingTrivia As New List(Of SyntaxTrivia)
        Dim NewTrailingTrivia As New List(Of SyntaxTrivia)

        For Each trivia As SyntaxTrivia In Token.LeadingTrivia
            Select Case trivia.RawKind
                Case VB.SyntaxKind.WhitespaceTrivia, VB.SyntaxKind.CommentTrivia
                    NewLeadingTrivia.Add(trivia)
                    FoundEOL = False
                Case VB.SyntaxKind.EndOfLineTrivia
                    If Not FoundEOL Then
                        NewLeadingTrivia.Add(trivia)
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
                    NewTrailingTrivia.Add(trivia)
                    FoundEOL = False
                Case VB.SyntaxKind.EndOfLineTrivia
                    If Not FoundEOL Then
                        NewTrailingTrivia.Add(trivia)
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

        Return Token.With(NewLeadingTrivia, NewTrailingTrivia)
    End Function

    <Extension()>
    Public Function RemoveExtraEOL(Token As SyntaxToken) As SyntaxToken
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
        Dim NewLeadingTrivia As New List(Of SyntaxTrivia)
        For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
            Dim trivia As SyntaxTrivia = e.Value
            Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(initialTriviaList, e.Index)
            If trivia.IsKind(VB.SyntaxKind.WhitespaceTrivia) AndAlso nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                Continue For
            End If
            If trivia.IsKind(VB.SyntaxKind.CommentTrivia) AndAlso nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                NewLeadingTrivia.Add(trivia)
                Continue For
            End If

            If trivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) AndAlso nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                Continue For
            End If
            NewLeadingTrivia.Add(trivia)
        Next

        Return Token.WithLeadingTrivia(NewLeadingTrivia)
    End Function

    <Extension>
    Public Function WithAppendedTrailingTrivia(Token As SyntaxToken, TriviaList As IEnumerable(Of SyntaxTrivia)) As SyntaxToken
        Return Token.WithTrailingTrivia(Token.TrailingTrivia.Concat(TriviaList))
    End Function

    <Extension>
    Public Function WithConvertedTrailingTriviaFrom(Token As SyntaxToken, otherToken As SyntaxToken) As SyntaxToken
        Return Token.WithTrailingTrivia(ConvertTrivia(otherToken.TrailingTrivia()))
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
        Dim afterWhiteSpace As Boolean = False
        Dim afterLineContinuation As Boolean = LeadingToken
        Dim initialTriviaList As New List(Of SyntaxTrivia)
        Dim triviaListUBound As Integer
        Dim finalLeadingTriviaList As New List(Of SyntaxTrivia)
        Dim finalTrailingTriviaList As New List(Of SyntaxTrivia)
        If LeadingToken Then
            finalLeadingTriviaList.AddRange(Token.LeadingTrivia)
        Else
            initialTriviaList.AddRange(Token.LeadingTrivia)
            triviaListUBound = initialTriviaList.Count - 1
            For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                Dim NextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(initialTriviaList, e.Index)
                Select Case e.Value.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        AfterEOL = False
                        afterLineContinuation = False
                        afterWhiteSpace = True
                        finalLeadingTriviaList.Add(e.Value)
                    Case VB.SyntaxKind.EndOfLineTrivia
                        afterLineContinuation = False
                        afterWhiteSpace = False
                        If AfterEOL Then
                            Continue For
                        End If
                        finalLeadingTriviaList.Add(e.Value)
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
                                    finalLeadingTriviaList.Add(SpaceTrivia)
                                Else
                                    finalLeadingTriviaList.Add(VBFactory.WhitespaceTrivia(NewWhiteSpaceString))
                                End If
                                finalLeadingTriviaList.Add(LineContinuation)
                                afterLineContinuation = True
                            Else
                                If Not String.IsNullOrWhiteSpace(NewWhiteSpaceString) Then
                                    finalLeadingTriviaList.Add(VBFactory.WhitespaceTrivia(NewWhiteSpaceString))
                                End If
                            End If
                        End If
                    Case VB.SyntaxKind.CommentTrivia
                        AfterEOL = False
                        If Not afterWhiteSpace Then
                            finalLeadingTriviaList.Add(SpaceTrivia)
                        End If
                        If Not afterLineContinuation Then
                            finalLeadingTriviaList.Add(LineContinuation)
                            finalLeadingTriviaList.Add(SpaceTrivia)
                        End If
                        finalLeadingTriviaList.Add(e.Value)
                        afterLineContinuation = False
                        afterWhiteSpace = False
                    Case VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.DisabledTextTrivia
                        AfterEOL = False
                        finalLeadingTriviaList.AddRange(DirectiveNotAllowedHere(e.Value))
                        Select Case NextTrivia.RawKind
                            Case VB.SyntaxKind.None
                                finalLeadingTriviaList.Add(VBEOLTrivia)
                            Case VB.SyntaxKind.WhitespaceTrivia
                                finalLeadingTriviaList.Add(VBEOLTrivia)
                            Case VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.DisabledTextTrivia, VB.SyntaxKind.EndIfDirectiveTrivia
                                finalLeadingTriviaList.Add(VBEOLTrivia)
                            Case Else
                                Stop
                        End Select
                    Case VB.SyntaxKind.EndIfDirectiveTrivia
                        If Token.LeadingTrivia.ContainsDirectiveTrivia(VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.ElseIfDirectiveTrivia) Then
                            finalLeadingTriviaList.AddRange(DirectiveNotAllowedHere(e.Value))
                            Select Case NextTrivia.RawKind
                                Case VB.SyntaxKind.None
                                    finalLeadingTriviaList.Add(VBEOLTrivia)
                                Case VB.SyntaxKind.WhitespaceTrivia
                                    finalLeadingTriviaList.Add(VBEOLTrivia)
                                Case VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.DisabledTextTrivia, VB.SyntaxKind.EndIfDirectiveTrivia
                                    finalLeadingTriviaList.Add(VBEOLTrivia)
                                Case Else
                                    Stop
                            End Select
                            Continue For
                        End If
                        AfterEOL = False
                        finalTrailingTriviaList.Add(VBEOLTrivia)
                        finalTrailingTriviaList.Add(e.Value)
                    Case Else
                        Stop
                End Select
            Next
        End If
        initialTriviaList.Clear()
        initialTriviaList.AddRange(Token.TrailingTrivia)
        triviaListUBound = initialTriviaList.Count - 1
        afterWhiteSpace = False
        afterLineContinuation = False

        If LeadingToken Then
            For index As Integer = 0 To triviaListUBound
                Dim trivia As SyntaxTrivia = initialTriviaList(index)
                Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(initialTriviaList, index)
                Select Case trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        If nextTrivia.IsKind(VB.SyntaxKind.CommentTrivia) OrElse
                            nextTrivia.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                            finalTrailingTriviaList.Add(trivia)
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
                                    finalTrailingTriviaList.Add(SpaceTrivia)
                                Else
                                    finalTrailingTriviaList.Add(VBFactory.WhitespaceTrivia(NewWhiteSpaceString))
                                End If
                                finalTrailingTriviaList.Add(LineContinuation)
                            End If
                            finalTrailingTriviaList.Add(trivia)
                            afterLineContinuation = True
                        Else
                            finalTrailingTriviaList.Add(trivia)
                            If Not String.IsNullOrWhiteSpace(NewWhiteSpaceString) Then
                                finalTrailingTriviaList.Add(VBFactory.WhitespaceTrivia(NewWhiteSpaceString))
                            End If
                        End If
                    Case VB.SyntaxKind.CommentTrivia
                        If Not afterWhiteSpace Then
                            finalTrailingTriviaList.Add(SpaceTrivia)
                        End If
                        If Not afterLineContinuation Then
                            finalTrailingTriviaList.Add(LineContinuation)
                            finalTrailingTriviaList.Add(SpaceTrivia)
                        End If
                        finalTrailingTriviaList.Add(trivia)
                        afterLineContinuation = False
                        afterWhiteSpace = False
                    Case VB.SyntaxKind.LineContinuationTrivia
                        If finalTrailingTriviaList.Last.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                            Continue For
                        End If
                        afterWhiteSpace = False
                        afterLineContinuation = True
                        finalTrailingTriviaList.Add(LineContinuation)
                    Case Else
                        Stop
                End Select
            Next
        Else
            finalTrailingTriviaList.AddRange(Token.TrailingTrivia)
        End If
        Return Token.With(finalLeadingTriviaList, finalTrailingTriviaList)
    End Function

    <Extension>
    Public Function WithPrependedLeadingTrivia(Token As SyntaxToken, TriviaList As SyntaxTriviaList) As SyntaxToken
        If TriviaList.Count = 0 Then
            Return Token
        End If

        Return Token.WithLeadingTrivia(TriviaList.Concat(Token.LeadingTrivia))
    End Function

End Module
