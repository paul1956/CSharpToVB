' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports CSharpToVB.PooledObjects

Namespace Microsoft.CodeAnalysis

    Friend NotInheritable Class SyntaxNavigator

        Private Const None As Integer = 0

        Private Shared ReadOnly s_childEnumeratorStackPool As New ObjectPool(Of Stack(Of ChildSyntaxList.Enumerator))(Function() New Stack(Of ChildSyntaxList.Enumerator)(), 10)
        Public Shared ReadOnly s_instance As SyntaxNavigator = New SyntaxNavigator()

        Private Sub New()
        End Sub

        Private Shared Function Matches(predicate As Func(Of SyntaxToken, Boolean), token As SyntaxToken) As Boolean
            Return predicate Is Nothing OrElse ReferenceEquals(predicate, Function(t As SyntaxToken) True) OrElse predicate(token)
        End Function

        Private Function GetFirstToken(triviaList As SyntaxTriviaList, predicate As Func(Of SyntaxToken, Boolean), stepInto As Func(Of SyntaxTrivia, Boolean)) As SyntaxToken
            Debug.Assert(stepInto IsNot Nothing)
            For Each trivia As SyntaxTrivia In triviaList
                If trivia.HasStructure AndAlso stepInto(trivia) Then
                    Dim [structure] As SyntaxNode = trivia.GetStructure()
                    Dim token As SyntaxToken = GetFirstToken([structure], predicate, stepInto)
                    If token.RawKind <> None Then
                        Return token
                    End If
                End If
            Next

            Return Nothing
        End Function

        Private Function GetFirstToken(token As SyntaxToken, predicate As Func(Of SyntaxToken, Boolean), stepInto As Func(Of SyntaxTrivia, Boolean)) As SyntaxToken
            ' find first token that matches (either specified token or token inside related trivia)
            If stepInto IsNot Nothing Then
                ' search in leading trivia
                Dim firstToken As SyntaxToken = GetFirstToken(token.LeadingTrivia, predicate, stepInto)
                If firstToken.RawKind <> None Then
                    Return firstToken
                End If
            End If

            If Matches(predicate, token) Then
                Return token
            End If

            If stepInto IsNot Nothing Then
                ' search in trailing trivia
                Dim firstToken As SyntaxToken = GetFirstToken(token.TrailingTrivia, predicate, stepInto)
                If firstToken.RawKind <> None Then
                    Return firstToken
                End If
            End If

            Return Nothing
        End Function

        Private Function GetFirstToken(current As SyntaxNode, predicate As Func(Of SyntaxToken, Boolean), stepInto As Func(Of SyntaxTrivia, Boolean)) As SyntaxToken
            Dim stack As Stack(Of ChildSyntaxList.Enumerator) = s_childEnumeratorStackPool.Allocate()
            Try
                stack.Push(current.ChildNodesAndTokens().GetEnumerator())
                While stack.Any
                    Dim en As ChildSyntaxList.Enumerator = stack.Pop()
                    If en.MoveNext() Then
                        Dim child As SyntaxNodeOrToken = en.Current
                        If child.IsToken Then
                            Dim token As SyntaxToken = GetFirstToken(child.AsToken(), predicate, stepInto)
                            If token.RawKind <> None Then
                                Return token
                            End If
                        End If

                        ' push this enumerator back, not done yet
                        stack.Push(en)
                        If child.IsNode Then
                            stack.Push(child.AsNode().ChildNodesAndTokens().GetEnumerator())
                        End If
                    End If
                End While

                Return Nothing
            Finally
                stack.Clear()
                s_childEnumeratorStackPool.Free(stack)
            End Try
        End Function

        Private Function GetNextToken(current As SyntaxTrivia, list As SyntaxTriviaList, predicate As Func(Of SyntaxToken, Boolean), stepInto As Func(Of SyntaxTrivia, Boolean), ByRef returnNext As Boolean) As SyntaxToken
            For Each trivia As SyntaxTrivia In list
                If returnNext Then
                    If trivia.HasStructure AndAlso stepInto IsNot Nothing AndAlso stepInto(trivia) Then
                        Dim [structure] As SyntaxNode = trivia.GetStructure()
                        Dim token As SyntaxToken = GetFirstToken([structure], predicate, stepInto)
                        'BC30518: Overload resolution failed because no accessible 'GetFirstToken' can be called with these arguments:
                        If token.RawKind <> None Then
                            Return token
                        End If
                    End If
                ElseIf trivia = current Then
                    returnNext = True
                End If
            Next

            Return Nothing
        End Function

        Friend Function GetNextToken(current As SyntaxToken, predicate As Func(Of SyntaxToken, Boolean), stepInto As Func(Of SyntaxTrivia, Boolean)) As SyntaxToken
            Return GetNextToken(current, predicate, stepInto IsNot Nothing, stepInto)
        End Function

        Friend Function GetNextToken(current As SyntaxTrivia, predicate As Func(Of SyntaxToken, Boolean), stepInto As Func(Of SyntaxTrivia, Boolean)) As SyntaxToken
            Dim returnNext As Boolean = False
            ' look inside leading trivia for current & next
            Dim token As SyntaxToken = GetNextToken(current, current.Token.LeadingTrivia, predicate, stepInto, returnNext)
            If token.RawKind <> None Then
                Return token
            End If

            ' consider containing token if current trivia was in the leading trivia
            If returnNext AndAlso (predicate Is Nothing OrElse predicate = Function(t As SyntaxToken) True OrElse predicate(current.Token)) Then
                Return current.Token
            End If

            ' look inside trailing trivia for current & next (or just next)
            token = GetNextToken(current, current.Token.TrailingTrivia, predicate, stepInto, returnNext)
            If token.RawKind <> None Then
                Return token
            End If

            Return GetNextToken(current.Token, predicate, False, stepInto)
        End Function

        Friend Function GetNextToken(node As SyntaxNode, predicate As Func(Of SyntaxToken, Boolean), stepInto As Func(Of SyntaxTrivia, Boolean)) As SyntaxToken
            While node.Parent IsNot Nothing
                ' walk forward in parent's child list until we find ourselves and then return the
                ' next token
                Dim returnNext As Boolean = False
                For Each child As SyntaxNodeOrToken In node.Parent.ChildNodesAndTokens()
                    If returnNext Then
                        If child.IsToken Then
                            Dim token As SyntaxToken = GetFirstToken(child.AsToken(), predicate, stepInto)
                            'BC30518: Overload resolution failed because no accessible 'GetFirstToken' can be called with these arguments:
                            If token.RawKind <> None Then
                                Return token
                            End If
                        Else
                            Dim token As SyntaxToken = GetFirstToken(child.AsNode(), predicate, stepInto)
                            'BC30518: Overload resolution failed because no accessible 'GetFirstToken' can be called with these arguments:
                            If token.RawKind <> None Then
                                Return token
                            End If
                        End If
                    ElseIf child.IsNode AndAlso child.AsNode() Is node Then
                        returnNext = True
                    End If
                Next

                ' didn't find the next token in my parent's children, look up the tree
                node = node.Parent
            End While

            If node.IsStructuredTrivia Then
                Return GetNextToken(CType(node, IStructuredTriviaSyntax).ParentTrivia, predicate, stepInto)
            End If

            Return Nothing
        End Function

        Friend Function GetNextToken(current As SyntaxToken, predicate As Func(Of SyntaxToken, Boolean), searchInsideCurrentTokenTrailingTrivia As Boolean, stepInto As Func(Of SyntaxTrivia, Boolean)) As SyntaxToken
            Debug.Assert(searchInsideCurrentTokenTrailingTrivia = False OrElse stepInto IsNot Nothing)
            If current.Parent IsNot Nothing Then
                ' look inside trailing trivia for structure
                If searchInsideCurrentTokenTrailingTrivia Then
                    Dim firstToken As SyntaxToken = GetFirstToken(current.TrailingTrivia, predicate, stepInto)
                    If firstToken.RawKind <> None Then
                        Return firstToken
                    End If
                End If

                ' walk forward in parent's child list until we find ourself
                ' and then return the next token
                Dim returnNext As Boolean = False
                For Each child As SyntaxNodeOrToken In current.Parent.ChildNodesAndTokens()
                    If returnNext Then
                        If child.IsToken Then
                            Dim token As SyntaxToken = GetFirstToken(child.AsToken(), predicate, stepInto)
                            If token.RawKind <> None Then
                                Return token
                            End If
                        Else
                            Dim token As SyntaxToken = GetFirstToken(child.AsNode(), predicate, stepInto)
                            If token.RawKind <> None Then
                                Return token
                            End If
                        End If
                    ElseIf child.IsToken AndAlso child.AsToken() = current Then
                        returnNext = True
                    End If
                Next

                Return GetNextToken(current.Parent, predicate, stepInto)
            End If

            Return Nothing
        End Function

    End Class

End Namespace
