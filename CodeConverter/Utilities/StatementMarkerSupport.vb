' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports System.Text

Imports CSharpToVBConverter.ToVisualBasic
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic

Namespace CSharpToVBConverter

    Public Module StatementMarker
        Private ReadOnly s_statementDictionary As New Dictionary(Of CS.CSharpSyntaxNode, Integer)
        Private ReadOnly s_statementSupportTupleList As New List(Of (index As Integer, statement As VB.VisualBasicSyntaxNode, RemoveStatement As StatementHandlingOption))
        Private s_nextIndex As Integer

        Public Enum StatementHandlingOption
            PrependStatement ' Perpend original statement
            ReplaceStatement ' Replace original statement with new statement
            AppendEmptyStatement ' Append statement with empty statement which contains Directives or Comment
        End Enum

        Private Function CompareWithoutTrivia(Statement1 As VB.VisualBasicSyntaxNode, Statement2 As VB.VisualBasicSyntaxNode) As Boolean
            Return Statement1.
                    WithoutTrivia.
                    ToFullString.
                    RemoveAll(vbCrLf, " ") =
                Statement2.
                    WithoutTrivia.
                    ToFullString.
                    RemoveAll(vbCrLf, " ")
        End Function

        Private Function ConvertDirectiveTrivia(OriginalText As String) As SyntaxTriviaList
            Dim text As String = OriginalText.Trim(" "c)
            Debug.Assert(text.StartsWith("#", StringComparison.Ordinal), "All directives must start with #")
            Dim resultTrivia As New SyntaxTriviaList
            If text.StartsWith("#if", StringComparison.Ordinal) OrElse text.StartsWith("#elif", StringComparison.Ordinal) Then
                Dim expr1 As String = text.
                    RemoveAll("#if ", "#elif ").
                    ConvertCondition

                Dim kind As VB.SyntaxKind = If(text.StartsWith("#if", StringComparison.Ordinal), VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.ElseIfDirectiveTrivia)
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
        ''' <param name="FullString"></param>
        ''' <returns></returns>
        ''' <remarks>Added by PC</remarks>
        Private Function ConvertSourceTextToTriviaList(FullString As String, Optional LeadingComment As String = "") As SyntaxTriviaList
            Dim newTrivia As New List(Of SyntaxTrivia)
            If Not String.IsNullOrWhiteSpace(LeadingComment) Then
                Dim leadingCommentLines() As String = LeadingComment.SplitLines
                For Each line As String In leadingCommentLines
                    newTrivia.Add(Factory.CommentTrivia($"' {line.Trim}"))
                    newTrivia.Add(VBEOLTrivia)
                Next
            End If
            Dim strBuilder As New StringBuilder
            For Each chr As String In FullString
                If chr.IsNewLine Then
                    If strBuilder.Length > 0 Then
                        newTrivia.Add(Factory.CommentTrivia($"' {strBuilder.ToString.Trim}"))
                        newTrivia.Add(VBEOLTrivia)
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
                newTrivia.Add(VBEOLTrivia)
            End If

            Return newTrivia.ToSyntaxTriviaList
        End Function

        Friend Function AddFinalTriviaToField(node As CSS.FieldDeclarationSyntax) As List(Of StatementSyntax)
            Dim statementList As New List(Of StatementSyntax)
            If Not s_statementDictionary.ContainsKey(node) Then
                Return statementList
            End If
            Dim index As Integer = s_statementDictionary(node)
            For Each stmtTuple As (index As Integer, statement As StatementSyntax, StatementHandling As StatementHandlingOption) In s_statementSupportTupleList
                If stmtTuple.index = index AndAlso stmtTuple.StatementHandling = StatementHandlingOption.AppendEmptyStatement Then
                    statementList.Add(stmtTuple.statement)
                    s_statementDictionary.Remove(node)
                End If
            Next
            If StatementDictionaryEmpty() Then
                s_statementSupportTupleList.Clear()
            End If
            Return statementList
        End Function

        ''' <summary>
        ''' Add a marker so we can add a statement higher up in the result tree
        ''' </summary>
        ''' <param name="Node">The C# statement above which we can add the statements we need</param>
        ''' <param name="statement">The statement we want to add above the Node</param>
        ''' <param name="StatementHandling">If True we will replace the Node statement with the new statement(s)
        ''' otherwise we just add the statement BEFORE the node</param>
        ''' <param name="AllowDuplicates">True if we can put do multiple replacements</param>
        <Extension>
        Friend Sub AddMarker(Node As CS.CSharpSyntaxNode, statement As VB.VisualBasicSyntaxNode, StatementHandling As StatementHandlingOption, AllowDuplicates As Boolean)
            If s_statementDictionary.ContainsKey(Node) Then
                If Not AllowDuplicates Then
                    Exit Sub
                End If
            Else
                ' Need to ignore duplicate declarations
                s_statementDictionary.Add(Node, s_nextIndex)
                s_nextIndex += 1
            End If
            Dim index As Integer = s_statementDictionary(Node)
            Dim identicalTrivia As Boolean = False
            For Each t As (index As Integer, statement As VB.VisualBasicSyntaxNode, StatementHandlingOption As StatementHandlingOption) In s_statementSupportTupleList
                If t.index = index AndAlso TypeOf statement IsNot EmptyStatementSyntax AndAlso CompareWithoutTrivia(statement, t.statement) AndAlso t.StatementHandlingOption = StatementHandling Then
                    Exit Sub
                End If
                If t.index = index AndAlso EndsWithSimilarTrivia(statement.GetLeadingTrivia, t.statement.GetLeadingTrivia) Then
                    identicalTrivia = True
                End If
            Next
            If identicalTrivia Then
                statement = statement.WithoutLeadingTrivia()
            End If
            s_statementSupportTupleList.Add((index, statement, StatementHandling))
        End Sub

        Friend Function AddSpecialCommentToField(node As CSS.FieldDeclarationSyntax, FieldDeclaration As FieldDeclarationSyntax) As FieldDeclarationSyntax
            If Not s_statementDictionary.ContainsKey(node) Then
                Return FieldDeclaration
            End If
            Dim index As Integer = s_statementDictionary(node)
            Dim newLeadingTrivia As New List(Of SyntaxTrivia)
            For Each stmtTuple As (index As Integer, statement As StatementSyntax, StatementHandling As StatementHandlingOption) In s_statementSupportTupleList
                If stmtTuple.index = index AndAlso stmtTuple.StatementHandling <> StatementHandlingOption.AppendEmptyStatement Then
                    newLeadingTrivia.AddRange(stmtTuple.statement.GetLeadingTrivia)
                    s_statementDictionary.Remove(node)
                End If
            Next
            If s_statementDictionary.Count = 0 Then
                s_statementSupportTupleList.Clear()
            End If
            newLeadingTrivia.AddRange(FieldDeclaration.GetLeadingTrivia)
            Return FieldDeclaration.WithLeadingTrivia(newLeadingTrivia)
        End Function

        <Extension>
        Friend Function CheckCorrectnessLeadingTrivia(Of T As SyntaxNode)(NodeWithIssue As T, AttemptToPortMade As Boolean, Optional MessageFragment As String = "") As SyntaxTriviaList
            Dim newLeadingTrivia As New List(Of SyntaxTrivia) From {
                Factory.CommentTrivia($"' TODO TASK: {MessageFragment}:")}

            If NodeWithIssue IsNot Nothing Then
                newLeadingTrivia.Add(Factory.CommentTrivia($"' Original statement:"))
                newLeadingTrivia.Add(VBEOLTrivia)
                newLeadingTrivia.AddRange(ConvertSourceTextToTriviaList(NodeWithIssue.ToFullString))
            End If
            If AttemptToPortMade Then
                newLeadingTrivia.Add(Factory.CommentTrivia($"' An attempt was made to correctly port the code, check the code below for correctness"))
            End If
            newLeadingTrivia.Add(VBEOLTrivia)
            Return newLeadingTrivia.ToSyntaxTriviaList
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
                node.GetLeadingTrivia.ConvertTriviaList()
                node.GetTrailingTrivia.ConvertTriviaList()
            Else
                Dim csLeadingTrivia As SyntaxTriviaList = node.GetLeadingTrivia
                If csLeadingTrivia.Any AndAlso csLeadingTrivia.First.IsKind(CS.SyntaxKind.WhitespaceTrivia) Then
                    newTrailingTrivia.Add(csLeadingTrivia(0).ConvertTrivia())
                End If
            End If
            Dim leadingSpace As SyntaxTrivia = New SyntaxTrivia
            If newLeadingTrivia.LastOrDefault.IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                leadingSpace = newLeadingTrivia.Last
            End If
            newLeadingTrivia.Add(Factory.CommentTrivia($"' TODO: Visual Basic does not support {UnsupportedFeature}."))
            newLeadingTrivia.Add(VBEOLTrivia)
            If CommentOutOriginalStatements Then
                If leadingSpace.IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                    newLeadingTrivia.Add(leadingSpace)
                End If
                newLeadingTrivia.Add(Factory.CommentTrivia($"' Original statement:"))
                newLeadingTrivia.Add(VBEOLTrivia)
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
                    newLeadingTrivia.Add(VBEOLTrivia)
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

        Friend Function GetStatementwithIssues(node As CS.CSharpSyntaxNode, Optional ReportErrors As Boolean = True) As CS.CSharpSyntaxNode
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
            If ReportErrors AndAlso stmtWithIssues Is Nothing Then
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
                If TypeOf stmtWithIssues Is CSS.MethodDeclarationSyntax Then
                    Dim methodStmt As CSS.MethodDeclarationSyntax = CType(stmtWithIssues, CSS.MethodDeclarationSyntax)
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
            Dim removeStmt As Boolean = False
            If Not s_statementDictionary.ContainsKey(node) Then
                Return statement
            End If
            Dim index As Integer = s_statementDictionary(node)

            For Each stmtTuple As (index As Integer, statement As StatementSyntax, RemoveStatement As Boolean) In s_statementSupportTupleList
                If stmtTuple.index = index Then
                    newNodesList = newNodesList.Add(stmtTuple.statement)
                    removeStmt = removeStmt Or stmtTuple.RemoveStatement
                End If
            Next
            s_statementDictionary.Remove(node)
            If s_statementDictionary.Count = 0 Then
                s_statementSupportTupleList.Clear()
            End If
            Return statement.WithPrependedLeadingTrivia(newNodesList(0).GetLeadingTrivia)
        End Function

        Friend Function ReplaceOneStatementWithMarkedStatements(node As CS.CSharpSyntaxNode, statement As StatementSyntax, Optional RemoveStatement As Boolean = False) As SyntaxList(Of StatementSyntax)
            Return ReplaceStatementsWithMarkedStatements(node, Factory.SingletonList(statement), RemoveStatement)
        End Function

        Friend Function ReplaceStatementsWithMarkedStatements(node As CS.CSharpSyntaxNode, statements As List(Of StatementSyntax), Optional RemoveStatement As Boolean = False) As SyntaxList(Of StatementSyntax)
            Return ReplaceStatementsWithMarkedStatements(node, Factory.List(statements), RemoveStatement)
        End Function

        Friend Function ReplaceStatementsWithMarkedStatements(node As CS.CSharpSyntaxNode, statements As SyntaxList(Of StatementSyntax), Optional RemoveStatement As Boolean = False) As SyntaxList(Of StatementSyntax)
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

            For Each stmtTuple As (index As Integer, statement As StatementSyntax, StatementHandling As StatementHandlingOption) In s_statementSupportTupleList
                If stmtTuple.index = index Then
                    If stmtTuple.StatementHandling = StatementHandlingOption.AppendEmptyStatement Then
                        Call New List(Of StatementSyntax)().Add(stmtTuple.statement)
                    Else
                        newNodesList.Add(stmtTuple.statement)
                        RemoveStatement = RemoveStatement Or stmtTuple.StatementHandling = StatementHandlingOption.ReplaceStatement
                    End If
                End If
            Next
            s_statementDictionary.Remove(node)
            If StatementDictionaryEmpty() Then
                s_statementSupportTupleList.Clear()
            End If
            If Not RemoveStatement Then
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
                nodes = nodes.Replace(nodes(0), nodes(0).WithConvertedTriviaFrom(NodeWithComments).WithPrependedLeadingTrivia(Factory.CommentTrivia($"' BEGIN TODO: {comment}")).WithTrailingEOL)
                nodes = nodes.Add(Factory.EmptyStatement.WithLeadingTrivia(VBEOLTrivia, Factory.CommentTrivia($"' END TODO: {comment}")))
            End If
            Return nodes
        End Function

    End Module
End Namespace
