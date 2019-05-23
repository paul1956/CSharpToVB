Imports System.Runtime.CompilerServices
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Xunit
Public Module TestUtilities
    <Extension()>
    Public Function VerifyOccurrenceCount(tree As SyntaxTree, kind As SyntaxKind, expectedCount As Integer) As SyntaxTree
        Dim actualCount As Integer = 0
        GetOccurrenceCount(kind, tree.GetRoot(), actualCount)
        Assert.Equal(expectedCount, actualCount)
        Return tree
    End Function

    Private Sub GetOccurrenceCount(kind As SyntaxKind, node As SyntaxNodeOrToken,
                                      ByRef actualCount As Integer)
        If node.IsKind(kind) Then
            actualCount += 1
        End If
        If node.IsToken Then
            Dim tk As SyntaxNodeOrToken = node
            For Each leadingTrivia As SyntaxTrivia In tk.GetLeadingTrivia()
                If leadingTrivia.Kind = kind Then
                    actualCount += 1
                End If
                If leadingTrivia.HasStructure Then
                    Dim leadingTriviaStructure As SyntaxNode = leadingTrivia.GetStructure
                    GetOccurrenceCount(kind, leadingTriviaStructure, actualCount)
                End If
            Next
            For Each trailingTrivia As SyntaxTrivia In tk.GetTrailingTrivia()
                If trailingTrivia.Kind = kind Then
                    actualCount += 1
                End If
                If trailingTrivia.HasStructure Then
                    Dim trailingTriviaStructure As SyntaxNode = trailingTrivia.GetStructure
                    GetOccurrenceCount(kind, trailingTriviaStructure, actualCount)
                End If
            Next
        End If
        For Each child As SyntaxNodeOrToken In node.ChildNodesAndTokens()
            GetOccurrenceCount(kind, child, actualCount)
        Next
    End Sub

End Module
