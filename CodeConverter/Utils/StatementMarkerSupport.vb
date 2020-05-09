' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports System.Text

Imports CSharpToVBCodeConverter.DestVisualBasic
Imports CSharpToVBCodeConverter.Util

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory

Public Module StatementMarker
    Private ReadOnly s_statementDictionary As New Dictionary(Of CS.CSharpSyntaxNode, Integer)
    Private ReadOnly s_statementSupportTupleList As New List(Of (Index As Integer, Statement As VisualBasic.VisualBasicSyntaxNode, RemoveStatement As StatementHandlingOption))
    Private s_nextIndex As Integer = 0

    Public Enum StatementHandlingOption
        PrependStatement ' Perpend original statement
        ReplaceStatement ' Replace original statement with new statement
        AppendEmptyStatement ' Append statement with empty statement which contains Directives or Comment
    End Enum

    Private Function CompareWithoutTrivia(Statement1 As VisualBasic.VisualBasicSyntaxNode, Statement2 As VisualBasic.VisualBasicSyntaxNode) As Boolean
        Return Statement1.
                    WithoutTrivia.
                    ToFullString.
                    Replace(vbCrLf, "", StringComparison.Ordinal).
                    Replace(" ", "", StringComparison.Ordinal) =
                Statement2.
                    WithoutTrivia.
                    ToFullString.Replace(vbCrLf, "", StringComparison.Ordinal).
                    Replace(" ", "", StringComparison.Ordinal)
    End Function

    ''' <summary>
    '''
    ''' </summary>
    ''' <param name="FullString"></param>
    ''' <returns></returns>
    ''' <remarks>Added by PC</remarks>
    Private Function ConvertSourceTextToTriviaList(FullString As String, Optional LeadingComment As String = "") As SyntaxTriviaList
        Dim NewTrivia As New SyntaxTriviaList
        If Not String.IsNullOrWhiteSpace(LeadingComment) Then
            Dim LeadingCommentLines() As String = LeadingComment.SplitLines
            For Each Line As String In LeadingCommentLines
                NewTrivia = NewTrivia.Add(VBFactory.CommentTrivia($"' {Line.Trim}"))
                NewTrivia = NewTrivia.Add(VBEOLTrivia)
            Next
        End If
        Dim strBuilder As New StringBuilder
        For Each chr As String In FullString
            If chr.IsNewLine Then
                If strBuilder.Length > 0 Then
                    NewTrivia = NewTrivia.Add(VBFactory.CommentTrivia($"' {strBuilder.ToString.Trim}"))
                    NewTrivia = NewTrivia.Add(VBEOLTrivia)
                    strBuilder.Clear()
                End If
            ElseIf chr = vbTab Then
                strBuilder.Append("    ")
            Else
                strBuilder.Append(chr)
            End If
        Next
        If strBuilder.Length > 0 Then
            NewTrivia = NewTrivia.Add(VBFactory.CommentTrivia($"' {strBuilder}"))
            NewTrivia = NewTrivia.Add(VBEOLTrivia)
        End If

        Return NewTrivia
    End Function

    Friend Function AddFinalTriviaToField(node As CSS.FieldDeclarationSyntax) As List(Of StatementSyntax)
        Dim StatementList As New List(Of StatementSyntax)
        If Not s_statementDictionary.ContainsKey(node) Then
            Return StatementList
        End If
        Dim Index As Integer = s_statementDictionary(node)
        For Each StatementTuple As (Index As Integer, Statement As StatementSyntax, StatementHandling As StatementHandlingOption) In s_statementSupportTupleList
            If StatementTuple.Index = Index AndAlso StatementTuple.StatementHandling = StatementHandlingOption.AppendEmptyStatement Then
                StatementList.Add(StatementTuple.Statement)
                s_statementDictionary.Remove(node)
            End If
        Next
        If StatementDictionaryEmpty() Then
            s_statementSupportTupleList.Clear()
        End If
        Return StatementList
    End Function

    ''' <summary>
    ''' Add a marker so we can add a statement higher up in the result tree
    ''' </summary>
    ''' <param name="Node">The C# statement above which we can add the statements we need</param>
    ''' <param name="Statement">The Statement we want to add above the Node</param>
    ''' <param name="StatementHandling">If True we will replace the Node Statement with the new statement(s)
    ''' otherwise we just add the statement BEFORE the node</param>
    ''' <param name="AllowDuplicates">True if we can put do multiple replacements</param>
    <Extension>
    Friend Sub AddMarker(Node As CS.CSharpSyntaxNode, Statement As VisualBasic.VisualBasicSyntaxNode, StatementHandling As StatementHandlingOption, AllowDuplicates As Boolean)
        If s_statementDictionary.ContainsKey(Node) Then
            If Not AllowDuplicates Then
                Return
            End If
        Else
            ' Need to ignore duplicate declarations
            s_statementDictionary.Add(Node, s_nextIndex)
            s_nextIndex += 1
        End If
        Dim Index As Integer = s_statementDictionary(Node)
        Dim IdenticalTrivia As Boolean = False
        For Each t As (Index As Integer, Statement As VisualBasic.VisualBasicSyntaxNode, StatementHandlingOption As StatementHandlingOption) In s_statementSupportTupleList
            If t.Index = Index AndAlso TypeOf Statement IsNot EmptyStatementSyntax AndAlso CompareWithoutTrivia(Statement, t.Statement) AndAlso t.StatementHandlingOption = StatementHandling Then
                Return
            End If
            If t.Index = Index AndAlso TriviaIsIdentical(t.Statement.GetLeadingTrivia, Statement.GetLeadingTrivia.ToList) Then
                IdenticalTrivia = True
            End If
        Next
        If IdenticalTrivia Then
            Statement = Statement.WithoutLeadingTrivia()
        End If
        s_statementSupportTupleList.Add((Index, Statement, StatementHandling))
    End Sub

    Friend Function AddSpecialCommentToField(node As CSS.FieldDeclarationSyntax, FieldDeclaration As FieldDeclarationSyntax) As FieldDeclarationSyntax
        If Not s_statementDictionary.ContainsKey(node) Then
            Return FieldDeclaration
        End If
        Dim LeadingTrivia As New List(Of SyntaxTrivia)
        Dim Index As Integer = s_statementDictionary(node)
        For Each StatementTuple As (Index As Integer, Statement As StatementSyntax, StatementHandling As StatementHandlingOption) In s_statementSupportTupleList
            If StatementTuple.Index = Index AndAlso StatementTuple.StatementHandling <> StatementHandlingOption.AppendEmptyStatement Then
                LeadingTrivia.AddRange(StatementTuple.Statement.GetLeadingTrivia)
                s_statementDictionary.Remove(node)
            End If
        Next
        If s_statementDictionary.Count = 0 Then
            s_statementSupportTupleList.Clear()
        End If
        LeadingTrivia.AddRange(FieldDeclaration.GetLeadingTrivia)
        Return FieldDeclaration.WithLeadingTrivia(LeadingTrivia)
    End Function

    <Extension>
    Friend Function CheckCorrectnessLeadingTrivia(Of T As SyntaxNode)(NodeWithIssue As T, AttemptToPortMade As Boolean, Optional MessageFragment As String = "") As SyntaxTriviaList
        Dim LeadingTrivia As New List(Of SyntaxTrivia) From {
            VBFactory.CommentTrivia($"' TODO TASK: {MessageFragment}:")
        }
        If NodeWithIssue IsNot Nothing Then
            LeadingTrivia.Add(VBFactory.CommentTrivia($"' Original Statement:"))
            LeadingTrivia.Add(VBEOLTrivia)
            LeadingTrivia.AddRange(ConvertSourceTextToTriviaList(NodeWithIssue.ToFullString))
        End If
        If AttemptToPortMade Then
            LeadingTrivia.Add(VBFactory.CommentTrivia($"' An attempt was made to correctly port the code, check the code below for correctness"))
        End If
        LeadingTrivia.Add(VBEOLTrivia)
        Return LeadingTrivia.ToSyntaxTriviaList
    End Function

    Friend Sub ClearMarker()
        s_nextIndex = 0
        s_statementDictionary.Clear()
        s_statementSupportTupleList.Clear()
    End Sub

    Friend Function FlagUnsupportedStatements(node As CS.CSharpSyntaxNode, UnsupportedFeature As String, CommentOutOriginalStatements As Boolean) As EmptyStatementSyntax
        Dim newLeadingTrivia As New List(Of SyntaxTrivia)
        Dim newTrailingTrivia As New List(Of SyntaxTrivia)
        If CommentOutOriginalStatements Then
            newLeadingTrivia.AddRange(ConvertTrivia(node.GetLeadingTrivia))
            newTrailingTrivia.AddRange(ConvertTrivia(node.GetTrailingTrivia))
        Else
            Dim csLeadingTrivia As SyntaxTriviaList = node.GetLeadingTrivia
            If csLeadingTrivia.Any AndAlso csLeadingTrivia.First.IsKind(CS.SyntaxKind.WhitespaceTrivia) Then
                newTrailingTrivia.AddRange(ConvertTrivia({csLeadingTrivia(0)}))
            End If
            ' NewTrailingTrivia.Add(VB_EOLTrivia)
        End If
        newLeadingTrivia.Add(VBFactory.CommentTrivia($"' TODO: Visual Basic does not support {UnsupportedFeature}."))
        newLeadingTrivia.Add(VBEOLTrivia)
        If CommentOutOriginalStatements Then
            newLeadingTrivia.Add(VBEOLTrivia)
            newLeadingTrivia.Add(VBFactory.CommentTrivia($"' Original Statement:"))
            newLeadingTrivia.Add(VBEOLTrivia)
            ' Match #
            For Each e As IndexClass(Of String) In node.ToString.SplitLines().WithIndex
                If e.Value.TrimStart(" "c).StartsWith("#", StringComparison.Ordinal) Then
                    newLeadingTrivia.AddRange(ConvertDirectiveTrivia(e.Value))
                Else
                    newLeadingTrivia.Add(VBFactory.CommentTrivia($"' {e.Value}"))
                End If
                newLeadingTrivia.Add(VBEOLTrivia)
            Next
        End If
        Return VBFactory.EmptyStatement.With(newLeadingTrivia, newTrailingTrivia)
    End Function

    Friend Function GetMarkerErrorMessage() As String
        Dim builder As New StringBuilder()
        builder.Append($" Marker Error StatementDictionary.Count = {s_statementDictionary.Count}{vbCrLf}")
        For Each statement As CS.CSharpSyntaxNode In s_statementDictionary.Keys
            builder.Append(statement.ToFullString)
        Next
        Return builder.ToString()
    End Function

    Friend Function GetStatementwithIssues(node As CS.CSharpSyntaxNode, Optional ReportErrors As Boolean = True) As CS.CSharpSyntaxNode
        Dim StatementWithIssues As CS.CSharpSyntaxNode = node
        While StatementWithIssues IsNot Nothing
            If TypeOf StatementWithIssues Is CSS.FieldDeclarationSyntax Then
                Exit While
            End If

            If TypeOf StatementWithIssues Is CSS.ArrowExpressionClauseSyntax Then
                Exit While
            End If

            If TypeOf StatementWithIssues Is CSS.ExpressionStatementSyntax Then
                Exit While
            End If

            If TypeOf StatementWithIssues Is CSS.StatementSyntax Then
                Dim StatementWithIssueParent As SyntaxNode = StatementWithIssues.Parent
                While StatementWithIssueParent.IsKind(CS.SyntaxKind.ElseClause)
                    StatementWithIssues = StatementWithIssues.Parent.FirstAncestorOrSelf(Of CSS.StatementSyntax)
                    StatementWithIssueParent = StatementWithIssues.Parent
                End While
                Exit While
            End If

            If TypeOf StatementWithIssues Is CSS.PropertyDeclarationSyntax Then
                Exit While
            End If

            If TypeOf StatementWithIssues Is CSS.MethodDeclarationSyntax Then
                Exit While
            End If

            If TypeOf StatementWithIssues Is CSS.ClassDeclarationSyntax Then
                Exit While
            End If

            If TypeOf StatementWithIssues Is CSS.ConversionOperatorDeclarationSyntax Then
                Exit While
            End If

            If TypeOf StatementWithIssues Is CSS.ConstructorDeclarationSyntax Then
                Exit While
            End If

            If TypeOf StatementWithIssues Is CSS.EnumDeclarationSyntax Then
                Exit While
            End If

            If TypeOf StatementWithIssues Is CSS.StructDeclarationSyntax Then
                Exit While
            End If

            If TypeOf StatementWithIssues Is CSS.UsingDirectiveSyntax Then
                Exit While
            End If

            StatementWithIssues = CType(StatementWithIssues.Parent, CS.CSharpSyntaxNode)
        End While
        If ReportErrors AndAlso StatementWithIssues Is Nothing Then
            Throw UnexpectedValue($"Can't find parent 'statement' of {node}")
        End If

        Return StatementWithIssues
    End Function

    ''' <summary>
    ''' Allow access to Marker Errors with exposing implementation
    ''' </summary>
    ''' <returns>True if there are statements left out of translation</returns>
    Friend Function HasMarkerError() As Boolean
        If s_statementDictionary.Any Then
            Return True
        End If
        Return False
    End Function

    Friend Function IsDecedentOfAsyncMethod(node As CS.CSharpSyntaxNode) As Boolean
        Dim StatementWithIssues As CS.CSharpSyntaxNode = node
        While StatementWithIssues IsNot Nothing
            If TypeOf StatementWithIssues Is CSS.MethodDeclarationSyntax Then
                Dim MethodStatement As CSS.MethodDeclarationSyntax = CType(StatementWithIssues, CSS.MethodDeclarationSyntax)
                For Each Modifier As SyntaxToken In MethodStatement.Modifiers
                    If Modifier.IsKind(CS.SyntaxKind.AsyncKeyword) Then
                        Return True
                    End If
                Next
                Return False
            End If

            If TypeOf StatementWithIssues Is CSS.FieldDeclarationSyntax Then
                Return False
            End If

            If TypeOf StatementWithIssues Is CSS.PropertyDeclarationSyntax Then
                Return False
            End If

            If TypeOf StatementWithIssues Is CSS.ClassDeclarationSyntax Then
                Return False
            End If

            If TypeOf StatementWithIssues Is CSS.ConversionOperatorDeclarationSyntax Then
                Return False
            End If

            If TypeOf StatementWithIssues Is CSS.ConstructorDeclarationSyntax Then
                Return False
            End If

            If TypeOf StatementWithIssues Is CSS.EnumDeclarationSyntax Then
                Exit While
            End If

            If TypeOf StatementWithIssues Is CSS.StructDeclarationSyntax Then
                Return False
            End If

            If TypeOf StatementWithIssues Is CSS.UsingDirectiveSyntax Then
                Return False
            End If

            StatementWithIssues = CType(StatementWithIssues.Parent, CS.CSharpSyntaxNode)
        End While
        If StatementWithIssues Is Nothing Then
            Throw UnexpectedValue($"Can't find parent 'statement' of {node}")
        End If

        Return False
    End Function

    Friend Function PrependStatementWithMarkedStatementTrivia(node As CS.CSharpSyntaxNode, Statement As StatementSyntax) As StatementSyntax
        Dim NewNodesList As New SyntaxList(Of StatementSyntax)
        Dim RemoveStatement As Boolean = False
        If Not s_statementDictionary.ContainsKey(node) Then
            Return Statement
        End If
        Dim Index As Integer = s_statementDictionary(node)

        For Each StatementTuple As (Index As Integer, Statement As StatementSyntax, RemoveStatement As Boolean) In s_statementSupportTupleList
            If StatementTuple.Index = Index Then
                NewNodesList = NewNodesList.Add(StatementTuple.Statement)
                RemoveStatement = RemoveStatement Or StatementTuple.RemoveStatement
            End If
        Next
        s_statementDictionary.Remove(node)
        If s_statementDictionary.Count = 0 Then
            s_statementSupportTupleList.Clear()
        End If
        Return Statement.WithPrependedLeadingTrivia(NewNodesList(0).GetLeadingTrivia)
    End Function

    Friend Function ReplaceOneStatementWithMarkedStatements(node As CS.CSharpSyntaxNode, Statement As StatementSyntax, Optional RemoveStatement As Boolean = False) As SyntaxList(Of StatementSyntax)
        Return ReplaceStatementsWithMarkedStatements(node, VBFactory.SingletonList(Statement), RemoveStatement)
    End Function

    Friend Function ReplaceStatementsWithMarkedStatements(node As CS.CSharpSyntaxNode, Statements As SyntaxList(Of StatementSyntax), Optional RemoveStatement As Boolean = False) As SyntaxList(Of StatementSyntax)
        If node Is Nothing Then
            Return Statements
        End If
        If s_statementDictionary.Count = 0 Then
            Return Statements
        End If
        Dim NewNodesList As New List(Of StatementSyntax)
        Dim TrailingNodesList As New List(Of StatementSyntax)
        If Not s_statementDictionary.ContainsKey(node) Then
            Return Statements
        End If
        Dim Index As Integer = s_statementDictionary(node)

        For Each StatementTuple As (Index As Integer, Statement As StatementSyntax, StatementHandling As StatementHandlingOption) In s_statementSupportTupleList
            If StatementTuple.Index = Index Then
                If StatementTuple.StatementHandling = StatementHandlingOption.AppendEmptyStatement Then
                    TrailingNodesList.Add(StatementTuple.Statement)
                Else
                    NewNodesList.Add(StatementTuple.Statement)
                    RemoveStatement = RemoveStatement Or StatementTuple.StatementHandling = StatementHandlingOption.ReplaceStatement
                End If
            End If
        Next
        s_statementDictionary.Remove(node)
        If StatementDictionaryEmpty() Then
            s_statementSupportTupleList.Clear()
        End If
        If Not RemoveStatement Then
            If NewNodesList.Any Then
                If NewNodesList(0).IsKind(VisualBasic.SyntaxKind.EmptyStatement) Then
                    Dim TempStatement As StatementSyntax = NewNodesList(0)
                    NewNodesList.RemoveAt(0)
                    Statements = Statements.Replace(Statements(0), Statements(0).WithPrependedLeadingTrivia(TempStatement.GetLeadingTrivia))
                Else
                    Statements = Statements.Replace(Statements(0), Statements(0).WithoutLeadingTrivia)
                End If
            End If
            NewNodesList.AddRange(Statements)
        End If
        NewNodesList.AddRange(TrailingNodesList)
        Return VBFactory.List(NewNodesList)
    End Function

    Friend Function StatementDictionaryEmpty() As Boolean
        Return Not s_statementDictionary.Any
    End Function

    ''' <summary>
    '''
    ''' </summary>
    ''' <param name="nodes"></param>
    ''' <param name="comment"></param>
    ''' <returns></returns>
    Friend Function WrapInComment(nodes As SyntaxList(Of StatementSyntax), NodeWithComments As CSS.StatementSyntax, comment As String) As SyntaxList(Of StatementSyntax)
        If nodes.Any Then
            nodes = nodes.Replace(nodes(0), nodes(0).WithConvertedTriviaFrom(NodeWithComments).WithPrependedLeadingTrivia(VBFactory.CommentTrivia($"' BEGIN TODO: {comment}")).WithTrailingEOL)
            nodes = nodes.Add(VBFactory.EmptyStatement.WithLeadingTrivia(VBEOLTrivia, VBFactory.CommentTrivia($"' END TODO: {comment}")))
        End If
        Return nodes
    End Function

End Module
