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

Public Module SyntaxTokenExtensions

    <Extension>
    Friend Function AdjustTokenTriviaWithLineContuations(OperatorToken As SyntaxToken) As SyntaxToken
        Dim operatorLeadingTrivia As New List(Of SyntaxTrivia)
        For Each e As IndexClass(Of SyntaxTrivia) In OperatorToken.LeadingTrivia.WithIndex
            If e.Value.IsComment Then
                If e.IsFirst Then
                    operatorLeadingTrivia.InsertRange(0, {SpaceTrivia, LineContinuation})
                ElseIf OperatorToken.LeadingTrivia(e.Index - 1).IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                    operatorLeadingTrivia(e.Index - 1) = operatorLeadingTrivia(e.Index - 1).AdjustWhitespace(2)
                    operatorLeadingTrivia.InsertRange(operatorLeadingTrivia.Count - 1, {SpaceTrivia, LineContinuation})
                Else
                    Stop
                End If
            End If
            operatorLeadingTrivia.Add(e.Value)
        Next
        Return OperatorToken.WithLeadingTrivia(operatorLeadingTrivia)
    End Function

    ''' <summary>
    ''' Returns new leading trivia for first Item in a List
    ''' This Trivia consists of all the OpenBrace Leading Trivia and the trailing Trivia only of does not just contain a CRLF
    ''' </summary>
    ''' <param name="Token"></param>
    ''' <returns>List(Of SyntaxTrivia) from OpenBrace</returns>
    ''' <param name="Leading"></param>
    <Extension>
    Friend Function CollectTokenTrivia(Token As SyntaxToken, Leading As Boolean) As List(Of SyntaxTrivia)
        Dim leadingTrivia As New List(Of SyntaxTrivia)
        If Token.HasLeadingTrivia Then
            leadingTrivia.AddRange(ConvertTrivia(Token.LeadingTrivia))
        End If
        Dim trailingTrivia As SyntaxTriviaList = Token.TrailingTrivia
        If Token.HasTrailingTrivia Then
            If Not Leading OrElse trailingTrivia.Count <> 1 OrElse Not trailingTrivia(0).IsEndOfLine Then
                leadingTrivia.AddRange(ConvertTrivia(trailingTrivia))
            End If
        End If
        Return leadingTrivia
    End Function

    <Extension>
    Friend Function ConvertModifier(m As SyntaxToken, IsModule As Boolean, context As TokenContext, ByRef FoundVisibility As Boolean) As SyntaxToken
        Dim Token As SyntaxToken = CS.CSharpExtensions.Kind(m).ConvertModifierKindToVBKeyword(IsModule, context, FoundVisibility)
        If Token.IsKind(VB.SyntaxKind.EmptyToken) Then
            Return EmptyToken.WithConvertedLeadingTriviaFrom(m)
        End If
        Return Token.WithConvertedTriviaFrom(m)
    End Function

    <Extension>
    Friend Function ConvertToInterpolatedStringTextToken(CSharpToken As SyntaxToken) As SyntaxToken
        Dim TokenString As String = ConvertCSharpEscapes(CSharpToken.ValueText)
        Return VBFactory.InterpolatedStringTextToken(TokenString, TokenString)
    End Function

    <Extension>
    Friend Function MakeIdentifierUnique(id As SyntaxToken, BracketNeeded As Boolean, QualifiedNameOrTypeName As Boolean) As SyntaxToken
        Dim ConvertedIdentifier As String = If(BracketNeeded, $"[{id.ValueText}]", id.ValueText)
        If ConvertedIdentifier = "_" Then
            ConvertedIdentifier = "underscore"
        End If
        ' Don't Change Qualified Names
        If QualifiedNameOrTypeName Then
            If Not s_usedIdentifiers.ContainsKey(ConvertedIdentifier) Then
                s_usedIdentifiers.Add(ConvertedIdentifier, New SymbolTableEntry(ConvertedIdentifier, True))
            End If
            Return VBFactory.Identifier(s_usedIdentifiers(ConvertedIdentifier).Name).WithConvertedTriviaFrom(id)
        End If
        If s_usedIdentifiers.ContainsKey(ConvertedIdentifier) Then
            ' We have a case sensitive exact match so just return it
            Return VBFactory.Identifier(s_usedIdentifiers(ConvertedIdentifier).Name).WithConvertedTriviaFrom(id)
        End If
        Dim IsFieldIdentifier As Boolean = False
        If TypeOf id.Parent Is CSS.VariableDeclaratorSyntax Then
            Dim VarDecl As CSS.VariableDeclaratorSyntax = CType(id.Parent, CSS.VariableDeclaratorSyntax)
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
                    s_usedIdentifiers.Add(ConvertedIdentifier, New SymbolTableEntry(Name:=NewUniqueName, IsType:=QualifiedNameOrTypeName))
                End If
                Return VBFactory.Identifier(s_usedIdentifiers(ConvertedIdentifier).Name).WithConvertedTriviaFrom(id)
            End If
        Next
        Dim _ConvertedIdentifier As String = $"{If(IsFieldIdentifier, "_", "")}{ConvertedIdentifier}"
        s_usedIdentifiers.Add(ConvertedIdentifier, New SymbolTableEntry(_ConvertedIdentifier, QualifiedNameOrTypeName))
        Return VBFactory.Identifier(_ConvertedIdentifier)
    End Function

    <Extension>
    Friend Function WithPrependedLeadingTrivia(Token As SyntaxToken, Trivia As IEnumerable(Of SyntaxTrivia)) As SyntaxToken
        Return Token.WithPrependedLeadingTrivia(Trivia.ToSyntaxTriviaList())
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

    ''' <summary>
    ''' Returns the token after this token in the syntax tree.
    ''' </summary>
    ''' <param name="predicate">Delegate applied to each token.  The token is returned if the predicate returns
    ''' true.</param>
    ''' <param name="stepInto">Delegate applied to trivia.  If this delegate is present then trailing trivia is
    ''' included in the search.</param>
    <Extension>
    Public Function GetNextToken(Node As SyntaxToken, predicate As Func(Of SyntaxToken, Boolean), Optional stepInto As Func(Of SyntaxTrivia, Boolean) = Nothing) As SyntaxToken
        If Node = Nothing Then
            Return Nothing
        End If

        Return SyntaxNavigator.s_instance.GetNextToken(Node, predicate, stepInto)
    End Function

    <Extension>
    Public Function IsKind(token As SyntaxToken, ParamArray kinds() As CSharp.SyntaxKind) As Boolean
        Return kinds.Contains(CType(token.RawKind, CSharp.SyntaxKind))
    End Function

    <Extension>
    Public Function IsKind(token As SyntaxToken, ParamArray kinds() As VB.SyntaxKind) As Boolean
        Return kinds.Contains(CType(token.RawKind, VB.SyntaxKind))
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
    Public Function WithAppendedTrailingTrivia(token As SyntaxToken, trivia As IEnumerable(Of SyntaxTrivia)) As SyntaxToken
        Return token.WithTrailingTrivia(token.TrailingTrivia.Concat(trivia))
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
    Public Function WithPrependedLeadingTrivia(token As SyntaxToken, trivia As SyntaxTriviaList) As SyntaxToken
        If trivia.Count = 0 Then
            Return token
        End If

        Return token.WithLeadingTrivia(trivia.Concat(token.LeadingTrivia))
    End Function

End Module
