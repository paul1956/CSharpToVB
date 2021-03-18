' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports Microsoft.CodeAnalysis
Imports SupportClasses
Imports Utilities
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic

Namespace Extensions
    Public Module SyntaxTokenExtensions

        <Extension>
        Private Function GetNewUniqueName(convertedIdentifier As String, usedIdentifiers As Dictionary(Of String, SymbolTableEntry), isType As Boolean, node As CS.CSharpSyntaxNode, model As SemanticModel) As String
            If isType Then
                Return convertedIdentifier
            End If

            convertedIdentifier = convertedIdentifier.RemoveBrackets

            Dim uniqueId As String = node.GetUniqueVariableNameInScope(convertedIdentifier, usedIdentifiers, model)
            If VB.SyntaxFacts.GetKeywordKind(uniqueId) = VB.SyntaxKind.None Then
                Return uniqueId
            End If

            Return $"[{uniqueId}]"
        End Function

        <Extension>
        Private Function GetScopingBlock(node As CS.CSharpSyntaxNode) As CS.CSharpSyntaxNode
            Dim blockNode As CS.CSharpSyntaxNode = node
            While blockNode IsNot Nothing

                If TypeOf blockNode Is CSS.BlockSyntax Then
                    Return CType(blockNode.Parent, CS.CSharpSyntaxNode)
                End If
                If TypeOf blockNode Is CSS.EventDeclarationSyntax Then
                    Return blockNode
                End If

                If TypeOf blockNode Is CSS.NamespaceDeclarationSyntax Then
                    Return blockNode
                End If

                If TypeOf blockNode Is CSS.PropertyDeclarationSyntax Then
                    Return blockNode
                End If

                If TypeOf blockNode Is CSS.MethodDeclarationSyntax Then
                    Return blockNode
                End If

                If TypeOf blockNode Is CSS.ClassDeclarationSyntax Then
                    Return blockNode
                End If

                If TypeOf blockNode Is CSS.ConversionOperatorDeclarationSyntax Then
                    Return blockNode
                End If

                If TypeOf blockNode Is CSS.ConstructorDeclarationSyntax Then
                    Return blockNode
                End If

                If TypeOf blockNode Is CSS.EnumDeclarationSyntax Then
                    Return blockNode
                End If

                If TypeOf blockNode Is CSS.StructDeclarationSyntax Then
                    Return blockNode
                End If

                If TypeOf blockNode Is CSS.UsingDirectiveSyntax Then
                    Return blockNode
                End If

                If TypeOf blockNode Is CSS.UsingStatementSyntax Then
                    Return CType(blockNode.Parent, CS.CSharpSyntaxNode)
                End If
                blockNode = CType(blockNode.Parent, CS.CSharpSyntaxNode)

            End While
            Return GetStatementWithIssues(node)
        End Function

        Private Function GetSymbolTableEntry(csIdentifier As SyntaxToken, baseVbIdent As String, usedIdentifiers As Dictionary(Of String, SymbolTableEntry), node As CS.CSharpSyntaxNode, model As SemanticModel, isQualifiedNameOrTypeName As Boolean, isField As Boolean) As (IdentToken As SyntaxToken, MeNeeded As Boolean)
            If usedIdentifiers.ContainsKey(baseVbIdent) Then
                Dim symbolTableEntry As SymbolTableEntry = usedIdentifiers(baseVbIdent)
                Return (Factory.Identifier(symbolTableEntry.Name).WithConvertedTriviaFrom(csIdentifier), symbolTableEntry.IsProperty)
            End If
            For Each ident As KeyValuePair(Of String, SymbolTableEntry) In usedIdentifiers
                If String.Compare(ident.Key, baseVbIdent, ignoreCase:=False, Globalization.CultureInfo.InvariantCulture) = 0 Then
                    ' We have an exact match keep looking
                    Return (Factory.Identifier(ident.Key), ident.Value.IsProperty)
                End If
                If String.Compare(ident.Key, baseVbIdent, ignoreCase:=True, Globalization.CultureInfo.InvariantCulture) = 0 Then
                    ' If we are here we have seen the variable in a different case so fix it
                    Dim uniqueName As String = baseVbIdent.GetNewUniqueName(usedIdentifiers, isQualifiedNameOrTypeName, node.GetScopingBlock, model)
                    If usedIdentifiers(ident.Key).IsType Then
                        usedIdentifiers.Add(baseVbIdent, New SymbolTableEntry(uniqueName, isType:=False, isField))
                    Else
                        usedIdentifiers.Add(baseVbIdent, New SymbolTableEntry(uniqueName, isQualifiedNameOrTypeName, isField))
                    End If
                    Dim symbolTableEntry As SymbolTableEntry = usedIdentifiers(baseVbIdent)
                    Return (Factory.Identifier(symbolTableEntry.Name).WithConvertedTriviaFrom(csIdentifier), symbolTableEntry.IsProperty)
                End If
            Next
            Dim newIdentifier As String = baseVbIdent
            usedIdentifiers.Add(baseVbIdent, New SymbolTableEntry(newIdentifier, isQualifiedNameOrTypeName, isField))
            Return (Factory.Identifier(newIdentifier), False)
        End Function

        <Extension>
        Private Function RestructureModifierLeadingTrivia(modifier As SyntaxToken, i As Integer, ByRef leadingTriviaNotHandled As Boolean, ByRef statementLeadingTrivia As SyntaxTriviaList, ByRef statementTrailingTrivia As SyntaxTriviaList) As SyntaxTriviaList
            Dim newModifierLeadingTrivia As New SyntaxTriviaList
            If leadingTriviaNotHandled Then
                If i = 0 Then
                    statementLeadingTrivia = statementLeadingTrivia.AddRange(modifier.LeadingTrivia)
                Else
                    newModifierLeadingTrivia = RelocateDirectiveDisabledTrivia(modifier.LeadingTrivia, statementLeadingTrivia, removeEol:=True)
                End If
            Else
                If i = 0 Then
                    If modifier.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        statementTrailingTrivia = statementTrailingTrivia.Add(VbEolTrivia)
                    End If
                    statementTrailingTrivia = statementTrailingTrivia.AddRange(modifier.LeadingTrivia)
                Else
                    newModifierLeadingTrivia = RelocateDirectiveDisabledTrivia(modifier.LeadingTrivia, statementTrailingTrivia, removeEol:=True)
                End If

            End If
            leadingTriviaNotHandled = statementLeadingTrivia.Count = 0
            Return newModifierLeadingTrivia
        End Function

        <Extension>
        Friend Function AdjustTokenLeadingTrivia(token As SyntaxToken) As SyntaxToken
            Dim newLeadingTrivia As New SyntaxTriviaList
            Dim initialTrivia As SyntaxTriviaList = token.LeadingTrivia
            Dim newTrailingTrivia As New SyntaxTriviaList

            For Each e As IndexClass(Of SyntaxTrivia) In initialTrivia.WithIndex
                Dim nextTrivia As SyntaxTrivia = initialTrivia.GetForwardTriviaOrDefault(e.Index, lookaheadCount:=1)
                Select Case e.Value.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        Select Case nextTrivia.RawKind
                            Case VB.SyntaxKind.CommentTrivia,
                             VB.SyntaxKind.DocumentationCommentTrivia
                                newLeadingTrivia = newLeadingTrivia.AddRange(SpaceLineContinue)
                                newLeadingTrivia = newLeadingTrivia.Add(e.Value)
                            Case VB.SyntaxKind.LineContinuationTrivia
                                newLeadingTrivia = newLeadingTrivia.Add(e.Value)
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
                            newLeadingTrivia = newLeadingTrivia.AddRange(SpaceLineContinue)
                        End If
                        newLeadingTrivia = newLeadingTrivia.Add(e.Value)
                    Case VB.SyntaxKind.EndOfLineTrivia
                        If e.IsFirst Then
                            newLeadingTrivia = newLeadingTrivia.Add(SpaceTrivia)
                            newLeadingTrivia = newLeadingTrivia.Add(LineContinuation)
                        End If
                        If Not nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            newLeadingTrivia = newLeadingTrivia.Add(e.Value)
                        End If
                    Case VB.SyntaxKind.LineContinuationTrivia
                        newLeadingTrivia = newLeadingTrivia.Add(e.Value)
                    Case VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.ElseDirectiveTrivia, VB.SyntaxKind.ElseIfDirectiveTrivia, VB.SyntaxKind.DisabledTextTrivia
                        newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                    Case Else
                        Stop
                End Select
            Next
            'Debug.WriteLine($"Leading  Token({sourceLineNumber}) Out:{Token.ToFullString}")
            newTrailingTrivia = newTrailingTrivia.AddRange(token.TrailingTrivia)
            Return token.With(newLeadingTrivia, newTrailingTrivia)
        End Function

        <Extension>
        Friend Function AdjustTokenTrailingTrivia(token As SyntaxToken, removeTrailingLineContinuation As Boolean) As SyntaxToken
            Dim newTrailingTrivia As New SyntaxTriviaList
            Dim initialTrivia As SyntaxTriviaList = token.TrailingTrivia

            'Debug.WriteLine($"Trailing Token({sourceLineNumber}) In :{Token.ToFullString}")
            For Each e As IndexClass(Of SyntaxTrivia) In initialTrivia.WithIndex
                Dim nextTrivia As SyntaxTrivia = initialTrivia.GetForwardTriviaOrDefault(e.Index, lookaheadCount:=1)
                Select Case e.Value.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        If nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            Continue For
                        End If
                        newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                    Case VB.SyntaxKind.CommentTrivia
                        newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                        If Not nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            newTrailingTrivia = newTrailingTrivia.Add(VbEolTrivia)
                        End If
                    Case VB.SyntaxKind.EndOfLineTrivia
                        If e.IsFirst Then
                            newTrailingTrivia = newTrailingTrivia.Add(SpaceTrivia)
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
            If removeTrailingLineContinuation Then
                Return token.WithTrailingTrivia(newTrailingTrivia.WithoutLastLineContinuation)
            End If
            If newTrailingTrivia.Count = 0 Then
                newTrailingTrivia = newTrailingTrivia.Add(Factory.ElasticSpace)
            End If
            Return token.WithTrailingTrivia(newTrailingTrivia)
        End Function

        ''' <summary>
        ''' Combines leading and trailing trivia for a Token
        ''' </summary>
        ''' <param name="token"></param>
        ''' <param name="getLeading"></param>
        ''' <param name="getTrailing"></param>
        ''' <returns>New SyntaxTriviaList</returns>
        <Extension>
        Friend Function CollectConvertedTokenTrivia(token As SyntaxToken, getLeading As Boolean, getTrailing As Boolean) As SyntaxTriviaList
            Dim combinedTrivia As New SyntaxTriviaList
            Dim leadingTrivia As SyntaxTriviaList = token.LeadingTrivia
            If getLeading Then
                If leadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    combinedTrivia = combinedTrivia.AddRange(token.LeadingTrivia.ConvertTriviaList())
                ElseIf leadingTrivia.ContainsEolTrivia Then
                    combinedTrivia = combinedTrivia.Add(VbEolTrivia)
                End If
            End If
            If getTrailing Then
                Dim trailingTrivia As SyntaxTriviaList = token.TrailingTrivia
                If trailingTrivia.ContainsCommentOrDirectiveTrivia Then
                    combinedTrivia = combinedTrivia.AddRange(trailingTrivia.ConvertTriviaList())
                ElseIf trailingTrivia.ContainsEolTrivia AndAlso Not combinedTrivia.ContainsEolTrivia Then
                    combinedTrivia = combinedTrivia.Add(VbEolTrivia)
                End If
            End If
            Return combinedTrivia
        End Function

        <Extension>
        Friend Function ContainsEolTrivia(token As SyntaxToken) As Boolean
            If Not token.HasTrailingTrivia Then
                Return False
            End If
            For Each t As SyntaxTrivia In token.TrailingTrivia
                If t.IsEndOfLine Then
                    Return True
                End If
            Next
            Return False
        End Function

        <Extension>
        Friend Function ConvertAndModifyTokenTrivia(token As SyntaxToken, nodesOrTokens As List(Of SyntaxNodeOrToken), index As Integer) As SyntaxToken
            If nodesOrTokens Is Nothing Then
                Throw New ArgumentNullException(NameOf(nodesOrTokens))
            End If
            Dim initialTriviaList As SyntaxTriviaList = nodesOrTokens(index).GetLeadingTrivia.ConvertTriviaList()
            Dim initialTriviaListUBound As Integer = initialTriviaList.Count - 1
            Dim afterWhiteSpace As Boolean = False
            Dim afterLineContinuation As Boolean = False
            Dim finalLeadingTrivia As New SyntaxTriviaList
            For initialTriviaIndex As Integer = 0 To initialTriviaListUBound
                Dim trivia As SyntaxTrivia = initialTriviaList(initialTriviaIndex)
                Select Case trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        afterLineContinuation = False
                        afterWhiteSpace = True
                        finalLeadingTrivia = finalLeadingTrivia.Add(trivia)
                    Case VB.SyntaxKind.EndOfLineTrivia
                        finalLeadingTrivia = finalLeadingTrivia.Add(trivia)
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
                                    finalLeadingTrivia = finalLeadingTrivia.Add(SpaceTrivia)
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
                            finalLeadingTrivia = finalLeadingTrivia.Add(SpaceTrivia)
                        End If
                        If Not afterLineContinuation Then
                            finalLeadingTrivia = finalLeadingTrivia.Add(LineContinuation)
                            finalLeadingTrivia = finalLeadingTrivia.Add(SpaceTrivia)
                        End If
                        finalLeadingTrivia = finalLeadingTrivia.Add(trivia)
                        afterLineContinuation = False
                        afterWhiteSpace = False
                    Case VB.SyntaxKind.EndIfDirectiveTrivia
                        Dim afterEol As Boolean
                        finalLeadingTrivia = finalLeadingTrivia.AddRange(trivia.DirectiveNotAllowedHere(afterEol))
                        If Not afterEol Then
                            finalLeadingTrivia = finalLeadingTrivia.Add(VbEolTrivia)
                        End If
                        afterLineContinuation = False
                        afterWhiteSpace = False
                    Case Else
                        Stop
                End Select
            Next
            initialTriviaList = nodesOrTokens(index).GetTrailingTrivia.ConvertTriviaList()
            Dim finalTrailingTrivia As SyntaxTriviaList
            For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                Dim trivia As SyntaxTrivia = e.Value
                Select Case trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        finalTrailingTrivia = finalTrailingTrivia.Add(trivia)
                        afterWhiteSpace = True
                    Case VB.SyntaxKind.EndOfLineTrivia
                        If e.IsLast AndAlso index < nodesOrTokens.Count - 1 AndAlso nodesOrTokens(index + 1).GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                            finalTrailingTrivia = finalTrailingTrivia.Add(SpaceTrivia)
                            finalTrailingTrivia = finalTrailingTrivia.Add(LineContinuation)
                        End If
                        finalTrailingTrivia = finalTrailingTrivia.Add(trivia)
                        afterWhiteSpace = False
                    Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                        If Not afterWhiteSpace = True Then
                            finalTrailingTrivia = finalTrailingTrivia.Add(SpaceTrivia)
                        End If
                        finalTrailingTrivia = finalTrailingTrivia.AddRange({LineContinuation, trivia})
                        afterWhiteSpace = False
                    Case Else
                        Stop
                End Select
            Next
            Return token.With(finalLeadingTrivia, finalTrailingTrivia)
        End Function

        <Extension>
        Friend Function ConvertToInterpolatedStringTextToken(token As SyntaxToken) As SyntaxToken
            If token.Language <> "C#" Then
                Throw New ArgumentException($"Invalid language {token.Language}, for parameter", NameOf(token))
            End If
            Dim tokenStr As String = ConvertCSharpEscapes(token.ValueText)
            Return Factory.InterpolatedStringTextToken(tokenStr, tokenStr)
        End Function

        <Extension>
        Friend Function IsKind(token As SyntaxToken, ParamArray kinds() As CS.SyntaxKind) As Boolean
            Return kinds.Contains(CType(token.RawKind, CS.SyntaxKind))
        End Function

        <Extension>
        Friend Function IsKind(token As SyntaxToken, ParamArray kinds() As VB.SyntaxKind) As Boolean
            Return kinds.Contains(CType(token.RawKind, VB.SyntaxKind))
        End Function

        <Extension>
        Friend Function MakeIdentifierUnique(csIdentifier As SyntaxToken, node As CS.CSharpSyntaxNode, usedIdentifiers As Dictionary(Of String, SymbolTableEntry), model As SemanticModel, isBracketNeeded As Boolean, isQualifiedNameOrTypeName As Boolean) As SyntaxToken
            Dim isField As Boolean = node.AncestorsAndSelf().OfType(Of CSS.FieldDeclarationSyntax).Any And Not isQualifiedNameOrTypeName
            Dim baseIdent As String = If(isBracketNeeded, $"[{csIdentifier.ValueText}]", csIdentifier.ValueText)
            If baseIdent = "_" Then
                baseIdent = "__"
            End If
            ' Don't Change Qualified Names
            If isQualifiedNameOrTypeName Then
                If Not usedIdentifiers.ContainsKey(baseIdent) Then
                    usedIdentifiers.Add(baseIdent, New SymbolTableEntry(baseIdent, isType:=True, isField))
                End If
                Return Factory.Identifier(baseIdent).WithConvertedTriviaFrom(csIdentifier)
            End If

            Return GetSymbolTableEntry(csIdentifier, baseIdent, usedIdentifiers, node, model, isQualifiedNameOrTypeName, isField).IdentToken
        End Function

        ''' <summary>
        ''' Remove directive trivia
        ''' </summary>
        ''' <param name="token"></param>
        ''' <returns></returns>
        <Extension>
        Friend Function RemoveDirectiveTrivia(token As SyntaxToken, ByRef foundEol As Boolean) As SyntaxToken
            Dim newLeadingTrivia As SyntaxTriviaList
            Dim newTrailingTrivia As SyntaxTriviaList

            For Each trivia As SyntaxTrivia In token.LeadingTrivia
                Select Case trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia, VB.SyntaxKind.CommentTrivia
                        newLeadingTrivia = newLeadingTrivia.Add(trivia)
                        foundEol = False
                    Case VB.SyntaxKind.EndOfLineTrivia
                        If Not foundEol Then
                            newLeadingTrivia = newLeadingTrivia.Add(trivia)
                            foundEol = True
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

            For Each trivia As SyntaxTrivia In token.TrailingTrivia
                Select Case trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia, VB.SyntaxKind.CommentTrivia
                        newTrailingTrivia = newTrailingTrivia.Add(trivia)
                        foundEol = False
                    Case VB.SyntaxKind.EndOfLineTrivia
                        If Not foundEol Then
                            newTrailingTrivia = newTrailingTrivia.Add(trivia)
                            foundEol = True
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

            Return token.With(newLeadingTrivia, newTrailingTrivia)
        End Function

        <Extension>
        Friend Function RemoveExtraEol(token As SyntaxToken) As SyntaxToken
            Dim initialTriviaList As SyntaxTriviaList = token.LeadingTrivia
            Select Case initialTriviaList.Count
                Case 0
                    Return token
                Case 1
                    If initialTriviaList.First.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                        Return token.WithLeadingTrivia(New SyntaxTriviaList)
                    End If
                Case 2
                    Select Case initialTriviaList.First.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            If initialTriviaList.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                Return token.WithLeadingTrivia(New SyntaxTriviaList)
                            End If
                            Return token
                        Case VB.SyntaxKind.EndOfLineTrivia
                            If initialTriviaList.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                Return token.WithLeadingTrivia(VbEolTrivia)
                            End If
                            Return token.WithLeadingTrivia(initialTriviaList.Last)
                        Case Else
                            Stop
                    End Select
            End Select
            Dim newLeadingTrivia As New SyntaxTriviaList
            For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                Dim trivia As SyntaxTrivia = e.Value
                Dim nextTrivia As SyntaxTrivia = initialTriviaList.GetForwardTriviaOrDefault(e.Index, lookaheadCount:=1)
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

            Return token.WithLeadingTrivia(newLeadingTrivia)
        End Function

        <Extension>
        Friend Function RestructureModifier(nodeModifier As SyntaxToken, i As Integer, ByRef attributesNotFound As Boolean, ByRef statementLeadingTrivia As SyntaxTriviaList, ByRef statementTrailingTrivia As SyntaxTriviaList) As SyntaxToken
            If (Not attributesNotFound) AndAlso nodeModifier.RawKind = VB.SyntaxKind.EmptyToken AndAlso nodeModifier.HasLeadingTrivia AndAlso nodeModifier.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                statementTrailingTrivia = statementTrailingTrivia.Add(VbEolTrivia)
                statementTrailingTrivia = statementTrailingTrivia.AddRange(nodeModifier.LeadingTrivia)
                nodeModifier = nodeModifier.WithLeadingTrivia(SpaceTrivia)
            Else
                nodeModifier = nodeModifier.WithLeadingTrivia(nodeModifier.RestructureModifierLeadingTrivia(i, attributesNotFound, statementLeadingTrivia, statementTrailingTrivia))
            End If
            If nodeModifier.TrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                statementLeadingTrivia = statementLeadingTrivia.AddRange(RelocateDirectiveDisabledTrivia(nodeModifier.TrailingTrivia, statementTrailingTrivia, removeEol:=False))
                If statementLeadingTrivia.Any AndAlso statementLeadingTrivia.Last.RawKind <> VB.SyntaxKind.EndOfLineTrivia Then
                    statementLeadingTrivia = statementLeadingTrivia.Add(VbEolTrivia)
                End If
                Return nodeModifier.WithTrailingTrivia(SpaceTrivia)
            End If
            Return nodeModifier.WithTrailingTrivia(RelocateDirectiveDisabledTrivia(nodeModifier.TrailingTrivia, statementTrailingTrivia, removeEol:=True))
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
        Friend Function WithAppendedTrailingTrivia(token As SyntaxToken, triviaList As IEnumerable(Of SyntaxTrivia)) As SyntaxToken
            Return token.WithTrailingTrivia(token.TrailingTrivia.Concat(triviaList))
        End Function

        <Extension>
        Friend Function WithConvertedLeadingTriviaFrom(node As SyntaxToken, otherToken As SyntaxToken) As SyntaxToken
            If Not otherToken.HasLeadingTrivia Then
                Return node
            End If
            Return node.WithLeadingTrivia(otherToken.LeadingTrivia.ConvertTriviaList())
        End Function

        <Extension>
        Friend Function WithConvertedTrailingTriviaFrom(token As SyntaxToken, otherToken As SyntaxToken) As SyntaxToken
            Return token.WithTrailingTrivia(otherToken.TrailingTrivia.ConvertTriviaList())
        End Function

        <Extension>
        Friend Function WithConvertedTriviaFrom(token As SyntaxToken, otherNode As SyntaxNode) As SyntaxToken
            If otherNode.HasLeadingTrivia Then
                token = token.WithLeadingTrivia(otherNode.GetLeadingTrivia.ConvertTriviaList())
            End If
            If Not otherNode.HasTrailingTrivia OrElse otherNode.ParentHasSameTrailingTrivia() Then
                Return token
            End If
            Return token.WithTrailingTrivia(otherNode.GetTrailingTrivia.ConvertTriviaList())
        End Function

        <Extension>
        Friend Function WithConvertedTriviaFrom(token As SyntaxToken, otherToken As SyntaxToken) As SyntaxToken
            Try
                If otherToken.HasLeadingTrivia Then
                    token = token.WithLeadingTrivia(otherToken.LeadingTrivia.ConvertTriviaList())
                End If
                Return token.WithTrailingTrivia(otherToken.TrailingTrivia.ConvertTriviaList())
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
        ''' <param name="token"></param>
        ''' <param name="leadingToken"></param>
        ''' <param name="afterEol"></param>
        ''' <param name="requireTrailingSpace"></param>
        ''' <param name="finalLeadingDirectiveNotAllowed"></param>
        ''' <returns></returns>
        <Extension>
        Friend Function WithModifiedTokenTrivia(token As SyntaxToken, leadingToken As Boolean, afterEol As Boolean, requireTrailingSpace As Boolean, finalLeadingDirectiveNotAllowed As Boolean) As SyntaxToken
            Dim afterWhiteSpace As Boolean = False
            Dim afterLineContinuation As Boolean = leadingToken
            Dim initialTriviaList As SyntaxTriviaList
            Dim triviaListUBound As Integer
            Dim finalLeadingTrivia As New SyntaxTriviaList
            Dim finalTrailingTrivia As New SyntaxTriviaList
            If leadingToken Then
                finalLeadingTrivia = finalLeadingTrivia.AddRange(token.LeadingTrivia)
            Else
                initialTriviaList = token.LeadingTrivia
                triviaListUBound = initialTriviaList.Count - 1
                For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                    Dim nextTrivia As SyntaxTrivia = initialTriviaList.GetForwardTriviaOrDefault(e.Index, lookaheadCount:=1)
                    Select Case e.Value.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            afterEol = False
                            afterLineContinuation = False
                            afterWhiteSpace = True
                            finalLeadingTrivia = finalLeadingTrivia.Add(e.Value)
                        Case VB.SyntaxKind.EndOfLineTrivia
                            afterLineContinuation = False
                            afterWhiteSpace = False
                            If afterEol Then
                                Continue For
                            End If
                            finalLeadingTrivia = finalLeadingTrivia.Add(e.Value)
                            ' What I do depends on whats next
                            If Not e.IsLast Then
                                Dim j As Integer
                                Dim newWhiteSpaceStr As String = ""
                                For j = e.Index + 1 To triviaListUBound
                                    If initialTriviaList(j).IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                                        newWhiteSpaceStr &= initialTriviaList(j).ToString
                                        e.MoveNext()
                                    Else
                                        Exit For
                                    End If
                                Next
                                If j < triviaListUBound AndAlso initialTriviaList(j).IsKind(VB.SyntaxKind.CommentTrivia) Then
                                    If String.IsNullOrWhiteSpace(newWhiteSpaceStr) Then
                                        finalLeadingTrivia = finalLeadingTrivia.Add(SpaceTrivia)
                                    Else
                                        finalLeadingTrivia = finalLeadingTrivia.Add(Factory.WhitespaceTrivia(newWhiteSpaceStr))
                                    End If
                                    finalLeadingTrivia = finalLeadingTrivia.Add(LineContinuation)
                                    afterLineContinuation = True
                                Else
                                    If Not String.IsNullOrWhiteSpace(newWhiteSpaceStr) Then
                                        finalLeadingTrivia = finalLeadingTrivia.Add(Factory.WhitespaceTrivia(newWhiteSpaceStr))
                                    End If
                                End If
                            End If
                        Case VB.SyntaxKind.CommentTrivia
                            afterEol = False
                            If Not afterWhiteSpace Then
                                finalLeadingTrivia = finalLeadingTrivia.Add(SpaceTrivia)
                            End If
                            If Not afterLineContinuation Then
                                finalLeadingTrivia = finalLeadingTrivia.Add(LineContinuation)
                                finalLeadingTrivia = finalLeadingTrivia.Add(SpaceTrivia)
                            End If
                            finalLeadingTrivia = finalLeadingTrivia.Add(e.Value)
                            afterLineContinuation = False
                            afterWhiteSpace = False
                        Case VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.DisabledTextTrivia
                            afterEol = False
                            finalLeadingTrivia = finalLeadingTrivia.AddRange(e.Value.DirectiveNotAllowedHere(afterEol))
                            Select Case nextTrivia.RawKind
                                Case VB.SyntaxKind.None
                                    finalLeadingTrivia = finalLeadingTrivia.Add(VbEolTrivia)
                                Case VB.SyntaxKind.WhitespaceTrivia
                                    finalLeadingTrivia = finalLeadingTrivia.Add(VbEolTrivia)
                                Case VB.SyntaxKind.EndOfLineTrivia,
                                     VB.SyntaxKind.IfDirectiveTrivia,
                                     VB.SyntaxKind.DisabledTextTrivia,
                                     VB.SyntaxKind.EndIfDirectiveTrivia
                                    finalLeadingTrivia = finalLeadingTrivia.Add(VbEolTrivia)
                                Case Else
                                    Stop
                            End Select
                        Case VB.SyntaxKind.EndIfDirectiveTrivia
                            If token.LeadingTrivia.ContainsDirectiveTrivia(VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.ElseIfDirectiveTrivia) Then
                                finalLeadingTrivia = finalLeadingTrivia.AddRange(e.Value.DirectiveNotAllowedHere(afterEol))
                                Select Case nextTrivia.RawKind
                                    Case VB.SyntaxKind.None,
                                         VB.SyntaxKind.EndOfLineTrivia,
                                         VB.SyntaxKind.DisabledTextTrivia,
                                         VB.SyntaxKind.EndIfDirectiveTrivia,
                                         VB.SyntaxKind.IfDirectiveTrivia,
                                         VB.SyntaxKind.WhitespaceTrivia
                                        finalLeadingTrivia = finalLeadingTrivia.Add(VbEolTrivia)
                                    Case Else
                                        Stop
                                End Select
                                Continue For
                            End If
                            If finalLeadingDirectiveNotAllowed Then
                                finalLeadingTrivia = finalLeadingTrivia.AddRange(e.Value.DirectiveNotAllowedHere(afterEol))
                            Else
                                finalLeadingTrivia = finalLeadingTrivia.Add(e.Value)
                                afterEol = False
                            End If
                        Case Else
                            Stop
                    End Select
                Next
            End If
            initialTriviaList = token.TrailingTrivia
            triviaListUBound = initialTriviaList.Count - 1
            afterWhiteSpace = False
            afterLineContinuation = False

            If leadingToken Then
                For index As Integer = 0 To triviaListUBound
                    Dim trivia As SyntaxTrivia = initialTriviaList(index)
                    Dim nextTrivia As SyntaxTrivia = initialTriviaList.GetForwardTriviaOrDefault(index, lookaheadCount:=1)
                    Select Case trivia.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            If nextTrivia.IsKind(VB.SyntaxKind.CommentTrivia) OrElse
                            nextTrivia.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                                finalTrailingTrivia = finalTrailingTrivia.Add(trivia)
                            End If
                        Case VB.SyntaxKind.EndOfLineTrivia
                            ' If leading there is a node after this Token
                            Dim j As Integer
                            Dim newWhiteSpaceStr As String = ""
                            If index < triviaListUBound Then
                                For j = index + 1 To triviaListUBound
                                    If initialTriviaList(j).IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                                        newWhiteSpaceStr &= initialTriviaList(j).ToString
                                        index += 1
                                    Else
                                        Exit For
                                    End If
                                Next
                            End If
                            If j = 0 OrElse (j < triviaListUBound AndAlso initialTriviaList(j).IsKind(VB.SyntaxKind.CommentTrivia)) Then
                                If Not afterLineContinuation Then
                                    If String.IsNullOrWhiteSpace(newWhiteSpaceStr) Then
                                        finalTrailingTrivia = finalTrailingTrivia.Add(SpaceTrivia)
                                    Else
                                        finalTrailingTrivia = finalTrailingTrivia.Add(Factory.WhitespaceTrivia(newWhiteSpaceStr))
                                    End If
                                    finalTrailingTrivia = finalTrailingTrivia.Add(LineContinuation)
                                End If
                                finalTrailingTrivia = finalTrailingTrivia.Add(trivia)
                                afterLineContinuation = True
                            Else
                                finalTrailingTrivia = finalTrailingTrivia.Add(trivia)
                                If Not String.IsNullOrWhiteSpace(newWhiteSpaceStr) Then
                                    finalTrailingTrivia = finalTrailingTrivia.Add(Factory.WhitespaceTrivia(newWhiteSpaceStr))
                                End If
                            End If
                        Case VB.SyntaxKind.CommentTrivia
                            If Not afterWhiteSpace Then
                                finalTrailingTrivia = finalTrailingTrivia.Add(SpaceTrivia)
                            End If
                            If Not afterLineContinuation Then
                                finalTrailingTrivia = finalTrailingTrivia.Add(LineContinuation)
                                finalTrailingTrivia = finalTrailingTrivia.Add(SpaceTrivia)
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
                finalTrailingTrivia = finalTrailingTrivia.AddRange(token.TrailingTrivia)
            End If

            If requireTrailingSpace AndAlso Not finalTrailingTrivia.FirstOrDefault.IsWhitespaceOrEndOfLine Then
                finalTrailingTrivia = finalTrailingTrivia.Insert(0, SpaceTrivia)
            End If
            Return token.With(finalLeadingTrivia, finalTrailingTrivia)

#End Region

        End Function

        <Extension>
        Friend Function WithPrependedLeadingTrivia(token As SyntaxToken, triviaList As SyntaxTriviaList) As SyntaxToken
            If triviaList.Count = 0 Then
                Return token
            End If

            Return token.WithLeadingTrivia(triviaList.Concat(token.LeadingTrivia))
        End Function

    End Module
End Namespace
