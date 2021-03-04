' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports System.Text
Imports Extensions
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax
Imports SupportClasses
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic

Namespace Utilities

    Public Module StatementMarker
        Friend ReadOnly s_statementDictionary As New Dictionary(Of CS.CSharpSyntaxNode, Integer)
        Friend ReadOnly s_statementHandlingList As New List(Of StatementHandling)
        Private _nextIndex As Integer

        Public Enum StatementHandlingOption
            PrependStatement ' Prepend original statement
            ReplaceStatement ' Replace original statement with new statement
            AppendEmptyStatement ' Append statement with empty statement which contains Directives or Comment
            AddMethod ' Add a Method to a class
        End Enum

        Private Function CompareWithoutTrivia(statement1 As VB.VisualBasicSyntaxNode, statement2 As VB.VisualBasicSyntaxNode) As Boolean
            Return statement1.
                    WithoutTrivia.
                    ToFullString.
                    RemoveAll(vbCrLf, " ") =
                statement2.
                    WithoutTrivia.
                    ToFullString.
                    RemoveAll(vbCrLf, " ")
        End Function

        Private Function ConvertDirectiveTrivia(originalText As String) As SyntaxTriviaList
            Dim text As String = originalText.Trim(" "c)
            Debug.Assert(text.StartsWith("#", StringComparison.Ordinal), "All directives must start with #")
            Dim resultTrivia As New SyntaxTriviaList
            If text.StartsWith("#if", StringComparison.Ordinal) OrElse text.StartsWith("#elif", StringComparison.Ordinal) Then
                Dim expr1 As String = text.
                    RemoveAll("#if ", "#elif ").
                    ConvertCondition

                Dim ifOrElseIfKeyword As SyntaxToken = If(text.StartsWith("#if", StringComparison.Ordinal), IfKeyword, ElseIfKeyword)
                Dim expr As ExpressionSyntax = Factory.ParseExpression(expr1)
                Dim ifDirectiveTrivia As IfDirectiveTriviaSyntax = Factory.IfDirectiveTrivia(ifOrElseIfKeyword, expr)
                resultTrivia = resultTrivia.Add(Factory.Trivia(ifDirectiveTrivia))
                Return resultTrivia
            End If
            If text.StartsWith("#region", StringComparison.Ordinal) OrElse text.StartsWith("# region", StringComparison.Ordinal) Then
                resultTrivia = resultTrivia.AddRange(CS.SyntaxFactory.ParseLeadingTrivia(text).ConvertTriviaList())
                Return resultTrivia
            End If
            If text.StartsWith("#endregion", StringComparison.Ordinal) Then
                resultTrivia = resultTrivia.Add(Factory.Trivia(Factory.EndRegionDirectiveTrivia()))
                text = text.RemoveAll("#endregion")
                If text.Any Then
                    Stop
                End If
                Return resultTrivia
            End If
            If text.StartsWith("#else", StringComparison.Ordinal) Then
                Dim elseKeywordWithTrailingTrivia As SyntaxToken = ElseKeyword.WithTrailingTrivia(CS.SyntaxFactory.ParseTrailingTrivia(text.RemoveAll("#else")).ConvertTriviaList())
                resultTrivia = resultTrivia.Add(Factory.Trivia(Factory.ElseDirectiveTrivia(HashToken, elseKeywordWithTrailingTrivia)))
                Return resultTrivia
            End If
            If text.StartsWith("#endif", StringComparison.Ordinal) Then
                text = text.RemoveAll("#endif")
                Dim ifKeywordWithTrailingTrivia As SyntaxToken = IfKeyword.WithTrailingTrivia(CS.SyntaxFactory.ParseTrailingTrivia(text.RemoveAll("#endif")).ConvertTriviaList())
                resultTrivia = resultTrivia.Add(Factory.Trivia(Factory.EndIfDirectiveTrivia(HashToken, EndKeyword, ifKeywordWithTrailingTrivia)))
                Return resultTrivia
            End If
            If text.StartsWith("#pragma warning", StringComparison.Ordinal) Then
                resultTrivia = resultTrivia.AddRange(CS.SyntaxFactory.ParseLeadingTrivia(text).ConvertTriviaList())
                Return resultTrivia
            Else
                Throw New NotImplementedException($"Directive ""{text}"" Is unknown")
            End If
        End Function

        ''' <summary>
        '''
        ''' </summary>
        ''' <param name="fullString"></param>
        ''' <returns></returns>
        ''' <remarks>Added by PC</remarks>
        Private Function ConvertSourceTextToTriviaList(fullString As String, Optional leadingComment As String = "") As SyntaxTriviaList
            Dim newTrivia As New List(Of SyntaxTrivia)
            If Not String.IsNullOrWhiteSpace(leadingComment) Then
                Dim leadingCommentLines() As String = leadingComment.SplitLines
                For Each line As String In leadingCommentLines
                    newTrivia.Add(Factory.CommentTrivia($"' {line.Trim}"))
                    newTrivia.Add(VbEolTrivia)
                Next
            End If
            Dim strBuilder As New StringBuilder
            For Each chr As String In fullString
                If chr.IsNewLine Then
                    If strBuilder.Length > 0 Then
                        newTrivia.Add(Factory.CommentTrivia($"' {strBuilder.ToString.Trim}"))
                        newTrivia.Add(VbEolTrivia)
                        strBuilder.Clear()
                    End If
                ElseIf chr = vbTab Then
                    strBuilder.Append("    ")
                Else
                    strBuilder.Append(chr)
                End If
            Next
            If strBuilder.Length > 0 Then
                newTrivia.Add(Factory.CommentTrivia($"' {strBuilder}"))
                newTrivia.Add(VbEolTrivia)
            End If

            Return newTrivia.ToSyntaxTriviaList
        End Function

        Private Function IsStatementDictionaryEmpty() As Boolean
            Return Not s_statementDictionary.Any
        End Function

        Friend Function AddFinalTriviaToField(node As CSS.FieldDeclarationSyntax) As List(Of StatementSyntax)
            Dim statementList As New List(Of StatementSyntax)
            If Not s_statementDictionary.ContainsKey(node) Then
                Return statementList
            End If
            Dim index As Integer = s_statementDictionary(node)
            For Each statementTuple As StatementHandling In s_statementHandlingList
                If statementTuple.Index = index AndAlso statementTuple.HandlingOption = StatementHandlingOption.AppendEmptyStatement Then
                    statementList.Add(statementTuple.Statement)
                    s_statementDictionary.Remove(node)
                End If
            Next
            If IsStatementDictionaryEmpty() Then
                s_statementHandlingList.Clear()
            End If
            Return statementList
        End Function

        ''' <summary>
        ''' Add a marker so we can add a statement higher up in the result tree
        ''' </summary>
        ''' <param name="node">The C# statement above which we can add the statements we need</param>
        ''' <param name="statement">The statement we want to add above the Node</param>
        ''' <param name="statementHandling">If True we will replace the Node statement with the new statement(s)
        ''' otherwise we just add the statement BEFORE the node</param>
        ''' <param name="allowDuplicates">True if we can put do multiple replacements</param>
        <Extension>
        Friend Sub AddMarker(node As CS.CSharpSyntaxNode, statement As VB.VisualBasicSyntaxNode, statementHandling As StatementHandlingOption, allowDuplicates As Boolean)
            If s_statementDictionary.ContainsKey(node) Then
                If Not allowDuplicates Then
                    Exit Sub
                End If
            Else
                ' Need to ignore duplicate declarations
                s_statementDictionary.Add(node, _nextIndex)
                _nextIndex += 1
            End If
            Dim index As Integer = s_statementDictionary(node)
            Dim identicalTrivia As Boolean = False
            For Each t As StatementHandling In s_statementHandlingList
                If t.Index = index AndAlso TypeOf statement IsNot EmptyStatementSyntax AndAlso CompareWithoutTrivia(statement, t.VbNode) AndAlso t.HandlingOption = statementHandling Then
                    Exit Sub
                End If
                If t.Index = index AndAlso (statementHandling <> StatementHandlingOption.AddMethod) AndAlso EndsWithSimilarTrivia(statement.GetLeadingTrivia, t.Statement.GetLeadingTrivia) Then
                    identicalTrivia = True
                End If
            Next
            If identicalTrivia Then
                statement = statement.WithoutLeadingTrivia()
            End If
            s_statementHandlingList.Add(New StatementHandling(index, statement, statementHandling))
        End Sub

        Friend Function AddSpecialCommentToField(node As CSS.FieldDeclarationSyntax, fieldDeclaration As FieldDeclarationSyntax) As FieldDeclarationSyntax
            If Not s_statementDictionary.ContainsKey(node) Then
                Return fieldDeclaration
            End If
            Dim index As Integer = s_statementDictionary(node)
            Dim newLeadingTrivia As New List(Of SyntaxTrivia)
            For Each stmtTuple As StatementHandling In s_statementHandlingList
                If stmtTuple.Index = index AndAlso stmtTuple.HandlingOption <> StatementHandlingOption.AppendEmptyStatement Then
                    newLeadingTrivia.AddRange(stmtTuple.Statement.GetLeadingTrivia)
                    s_statementDictionary.Remove(node)
                End If
            Next
            If s_statementDictionary.Count = 0 Then
                s_statementHandlingList.Clear()
            End If
            newLeadingTrivia.AddRange(fieldDeclaration.GetLeadingTrivia)
            Return fieldDeclaration.WithLeadingTrivia(newLeadingTrivia)
        End Function

        <Extension>
        Friend Function CheckCorrectnessLeadingTrivia(Of T As SyntaxNode)(nodeWithIssue As T, attemptToPortMade As Boolean, Optional messageFragment As String = "") As SyntaxTriviaList
            Dim newLeadingTrivia As New List(Of SyntaxTrivia) From {
                Factory.CommentTrivia($"' TODO TASK: {messageFragment}:")}

            If nodeWithIssue IsNot Nothing Then
                newLeadingTrivia.Add(Factory.CommentTrivia($"' Original statement:"))
                newLeadingTrivia.Add(VbEolTrivia)
                newLeadingTrivia.AddRange(ConvertSourceTextToTriviaList(nodeWithIssue.ToFullString))
            End If
            If attemptToPortMade Then
                newLeadingTrivia.Add(Factory.CommentTrivia($"' An attempt was made to correctly port the code, check the code below for correctness"))
            End If
            newLeadingTrivia.Add(VbEolTrivia)
            Return newLeadingTrivia.ToSyntaxTriviaList
        End Function

        Friend Sub ClearMarker()
            _nextIndex = 0
            s_statementDictionary.Clear()
            s_statementHandlingList.Clear()
        End Sub

        Friend Function FlagUnsupportedStatements(node As CS.CSharpSyntaxNode, unsupportedFeature As String, commentOutOriginalStatements As Boolean) As EmptyStatementSyntax
            Dim newLeadingTrivia As New List(Of SyntaxTrivia)
            Dim newTrailingTrivia As New List(Of SyntaxTrivia)
            If commentOutOriginalStatements Then
                newLeadingTrivia = node.GetLeadingTrivia.ConvertTriviaList().ToList
                newTrailingTrivia = node.GetTrailingTrivia.ConvertTriviaList().ToList
            Else
                Dim csLeadingTrivia As SyntaxTriviaList = node.GetLeadingTrivia
                If csLeadingTrivia.Any AndAlso csLeadingTrivia.First.IsKind(CS.SyntaxKind.WhitespaceTrivia) Then
                    newTrailingTrivia.Add(Factory.Whitespace(csLeadingTrivia.Last.ToString))
                End If
            End If
            Dim leadingSpace As SyntaxTrivia = New SyntaxTrivia
            If newLeadingTrivia.LastOrDefault.IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                leadingSpace = newLeadingTrivia.Last
            End If
            newLeadingTrivia.Add(Factory.CommentTrivia($"' TODO: Visual Basic does not support {unsupportedFeature}."))
            newLeadingTrivia.Add(VbEolTrivia)
            If commentOutOriginalStatements Then
                If leadingSpace.IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                    newLeadingTrivia.Add(leadingSpace)
                End If
                newLeadingTrivia.Add(Factory.CommentTrivia($"' Original statement:"))
                newLeadingTrivia.Add(VbEolTrivia)
                ' Match #
                For Each e As IndexClass(Of String) In node.ToString.SplitLines().WithIndex
                    If e.Value.TrimStart(" "c).StartsWith("#", StringComparison.Ordinal) Then
                        newLeadingTrivia.AddRange(ConvertDirectiveTrivia(e.Value))
                    Else
                        If leadingSpace.IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                            newLeadingTrivia.Add(leadingSpace)
                        End If
                        newLeadingTrivia.Add(Factory.CommentTrivia($"' {e.Value}"))
                    End If
                    newLeadingTrivia.Add(VbEolTrivia)
                Next
            End If
            Return Factory.EmptyStatement.With(newLeadingTrivia, newTrailingTrivia)
        End Function

        Friend Function GetMarkerErrorMessage() As String
            Dim builder As New StringBuilder()
            builder.Append($" Marker Error StatementDictionary.Count = {s_statementDictionary.Count}{vbCrLf}")
            For Each statement As CS.CSharpSyntaxNode In s_statementDictionary.Keys
                builder.Append(statement.ToFullString)
            Next
            Return builder.ToString()
        End Function

        <Extension>
        Friend Function GetStatementsForNode(node As CS.CSharpSyntaxNode, kind As VB.SyntaxKind) As List(Of (Integer, StatementSyntax))
            Dim statements As New List(Of (Integer, StatementSyntax))

            If node Is Nothing Then
                Return statements
            End If

            If s_statementDictionary.Count = 0 Then
                Return statements
            End If

            If Not s_statementDictionary.ContainsKey(node) Then
                Return statements
            End If
            Dim index As Integer = s_statementDictionary(node)

            For Each stmtTuple As StatementHandling In s_statementHandlingList
                If stmtTuple.Index = index AndAlso stmtTuple.Statement.Kind = kind Then
                    statements.Add((index, stmtTuple.Statement))
                End If
            Next
            Return statements
        End Function

        Friend Function GetStatementWithIssues(node As CS.CSharpSyntaxNode, Optional reportErrors As Boolean = True) As CS.CSharpSyntaxNode
            Dim stmtWithIssues As CS.CSharpSyntaxNode = node

            While stmtWithIssues IsNot Nothing
                If TypeOf stmtWithIssues Is CSS.FieldDeclarationSyntax Then
                    Return stmtWithIssues
                End If

                If TypeOf stmtWithIssues Is CSS.ExpressionStatementSyntax Then
                    Return stmtWithIssues
                End If

                If TypeOf stmtWithIssues Is CSS.ArrowExpressionClauseSyntax Then
                    Return stmtWithIssues
                End If

                If TypeOf stmtWithIssues Is CSS.StatementSyntax Then
                    Dim stmtWithIssueParent As SyntaxNode = stmtWithIssues.Parent
                    While stmtWithIssueParent.IsKind(CS.SyntaxKind.ElseClause)
                        stmtWithIssues = stmtWithIssues.Parent.FirstAncestorOrSelf(Of CSS.StatementSyntax)
                        stmtWithIssueParent = stmtWithIssues.Parent
                    End While
                    Return stmtWithIssues
                End If

                If TypeOf stmtWithIssues Is CSS.PropertyDeclarationSyntax Then
                    Return stmtWithIssues
                End If

                If TypeOf stmtWithIssues Is CSS.MethodDeclarationSyntax Then
                    Return stmtWithIssues
                End If

                If TypeOf stmtWithIssues Is CSS.ClassDeclarationSyntax Then
                    Return stmtWithIssues
                End If

                If TypeOf stmtWithIssues Is CSS.ConversionOperatorDeclarationSyntax Then
                    Return stmtWithIssues
                End If

                If TypeOf stmtWithIssues Is CSS.ConstructorDeclarationSyntax Then
                    Return stmtWithIssues
                End If

                If TypeOf stmtWithIssues Is CSS.EnumDeclarationSyntax Then
                    Return stmtWithIssues
                End If

                If TypeOf stmtWithIssues Is CSS.StructDeclarationSyntax Then
                    Return stmtWithIssues
                End If

                If TypeOf stmtWithIssues Is CSS.UsingDirectiveSyntax Then
                    Return stmtWithIssues
                End If

                If TypeOf stmtWithIssues Is CSS.DelegateDeclarationSyntax Then
                    Return stmtWithIssues
                End If

                stmtWithIssues = CType(stmtWithIssues.Parent, CS.CSharpSyntaxNode)
            End While
            If reportErrors AndAlso stmtWithIssues Is Nothing Then
                Throw UnexpectedValue($"Can't find parent 'statement' of {node}")
            End If

            Return stmtWithIssues
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
            Dim stmtWithIssues As CS.CSharpSyntaxNode = node
            While stmtWithIssues IsNot Nothing
                Dim methodStmt As CSS.MethodDeclarationSyntax = TryCast(stmtWithIssues, CSS.MethodDeclarationSyntax)
                If methodStmt IsNot Nothing Then
                    For Each modifier As SyntaxToken In methodStmt.Modifiers
                        If modifier.IsKind(CS.SyntaxKind.AsyncKeyword) Then
                            Return True
                        End If
                    Next
                    Return False
                End If

                If TypeOf stmtWithIssues Is CSS.FieldDeclarationSyntax Then
                    Return False
                End If

                If TypeOf stmtWithIssues Is CSS.PropertyDeclarationSyntax Then
                    Return False
                End If

                If TypeOf stmtWithIssues Is CSS.ClassDeclarationSyntax Then
                    Return False
                End If

                If TypeOf stmtWithIssues Is CSS.ConversionOperatorDeclarationSyntax Then
                    Return False
                End If

                If TypeOf stmtWithIssues Is CSS.ConstructorDeclarationSyntax Then
                    Return False
                End If

                If TypeOf stmtWithIssues Is CSS.EnumDeclarationSyntax Then
                    Exit While
                End If

                If TypeOf stmtWithIssues Is CSS.StructDeclarationSyntax Then
                    Return False
                End If

                If TypeOf stmtWithIssues Is CSS.UsingDirectiveSyntax Then
                    Return False
                End If

                stmtWithIssues = CType(stmtWithIssues.Parent, CS.CSharpSyntaxNode)
            End While
            If stmtWithIssues Is Nothing Then
                Throw UnexpectedValue($"Can't find parent 'statement' of {node}")
            End If

            Return False
        End Function

        Friend Function PrependStatementWithMarkedStatementTrivia(node As CS.CSharpSyntaxNode, statement As StatementSyntax) As StatementSyntax
            Dim newNodesList As New SyntaxList(Of StatementSyntax)
            If Not s_statementDictionary.ContainsKey(node) Then
                Return statement
            End If
            Dim index As Integer = s_statementDictionary(node)

            For Each stmtHandling As StatementHandling In s_statementHandlingList
                If stmtHandling.Index = index Then
                    newNodesList = newNodesList.Add(stmtHandling.Statement)
                End If
            Next
            s_statementDictionary.Remove(node)
            If s_statementDictionary.Count = 0 Then
                s_statementHandlingList.Clear()
            End If
            Return statement.WithPrependedLeadingTrivia(newNodesList(0).GetLeadingTrivia)
        End Function

        Friend Sub RemoveMarkedStatement(node As CS.CSharpSyntaxNode, statementToBeRemoved As VB.VisualBasicSyntaxNode)
            Dim index As Integer = s_statementDictionary(node)
            Dim nodeToBeRemoved As Integer = 0
            Dim totalStatementForNode As Integer = 0
            For i As Integer = 0 To s_statementHandlingList.Count - 1
                Dim stmtTuple As StatementHandling = s_statementHandlingList(i)
                If stmtTuple.Index = index Then
                    totalStatementForNode += 1
                    If stmtTuple.VbNode.ToString = statementToBeRemoved.ToString Then
                        nodeToBeRemoved = i
                    End If
                End If
            Next
            s_statementHandlingList.RemoveAt(nodeToBeRemoved)
            If totalStatementForNode = 1 Then
                s_statementDictionary.Remove(node)
            End If

        End Sub

        Friend Function ReplaceOneStatementWithMarkedStatements(node As CS.CSharpSyntaxNode, statement As StatementSyntax, Optional removeStatement As Boolean = False) As SyntaxList(Of StatementSyntax)
            Return ReplaceStatementsWithMarkedStatements(node, Factory.SingletonList(statement), removeStatement)
        End Function

        Friend Function ReplaceStatementsWithMarkedStatements(node As CS.CSharpSyntaxNode, statements As List(Of StatementSyntax), Optional removeStatement As Boolean = False) As SyntaxList(Of StatementSyntax)
            Return ReplaceStatementsWithMarkedStatements(node, Factory.List(statements), removeStatement)
        End Function

        Friend Function ReplaceStatementsWithMarkedStatements(node As CS.CSharpSyntaxNode, statements As SyntaxList(Of StatementSyntax), Optional removeStatement As Boolean = False) As SyntaxList(Of StatementSyntax)
            If node Is Nothing Then
                Return statements
            End If
            If s_statementDictionary.Count = 0 Then
                Return statements
            End If
            Dim newNodesList As New List(Of StatementSyntax)
            If Not s_statementDictionary.ContainsKey(node) Then
                Return statements
            End If
            Dim index As Integer = s_statementDictionary(node)

            For Each stmtTuple As StatementHandling In s_statementHandlingList
                If stmtTuple.Index = index Then
                    If stmtTuple.HandlingOption = StatementHandlingOption.AppendEmptyStatement Then
                        Call New List(Of StatementSyntax)().Add(stmtTuple.Statement)
                    Else
                        newNodesList.Add(stmtTuple.Statement)
                        removeStatement = removeStatement Or stmtTuple.HandlingOption = StatementHandlingOption.ReplaceStatement
                    End If
                End If
            Next
            s_statementDictionary.Remove(node)
            If IsStatementDictionaryEmpty() Then
                s_statementHandlingList.Clear()
            End If
            If Not removeStatement Then
                If newNodesList.Any Then
                    If newNodesList(0).IsKind(VB.SyntaxKind.EmptyStatement) Then
                        Dim tempStmt As StatementSyntax = newNodesList(0)
                        newNodesList.RemoveAt(0)
                        statements = statements.Replace(statements(0), statements(0).WithPrependedLeadingTrivia(tempStmt.GetLeadingTrivia))
                    Else
                        statements = statements.Replace(statements(0), statements(0).WithoutLeadingTrivia)
                    End If
                End If
                newNodesList.AddRange(statements)
            End If
            newNodesList.AddRange(New List(Of StatementSyntax))
            Return Factory.List(newNodesList)
        End Function

        ''' <summary>
        '''
        ''' </summary>
        ''' <param name="nodes"></param>
        ''' <param name="comment"></param>
        ''' <returns></returns>
        Friend Function WrapInComment(nodes As SyntaxList(Of StatementSyntax), nodeWithComments As CSS.StatementSyntax, comment As String) As SyntaxList(Of StatementSyntax)
            If nodes.Any Then
                nodes = nodes.Replace(nodes(0), nodes(0).WithConvertedTriviaFrom(nodeWithComments).WithPrependedLeadingTrivia(Factory.CommentTrivia($"' BEGIN TODO: {comment}")).WithTrailingEol)
                nodes = nodes.Add(Factory.EmptyStatement.WithLeadingTrivia(VbEolTrivia, Factory.CommentTrivia($"' END TODO: {comment}")))
            End If
            Return nodes
        End Function

    End Module
End Namespace
