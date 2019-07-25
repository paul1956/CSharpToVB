' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.Runtime.CompilerServices
Imports System.Text

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic

Imports Xunit

Public Module TestUtilities

    Private _roslynRootDirectory As String = ""

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

    Friend Function HomogenizeEol(str As String) As String
        Dim sb As New StringBuilder()
        For i As Integer = 0 To str.Length - 1
            Dim ch As Char = str.Chars(i)
            Dim possibleNewline As Integer = NewLine.GetDelimiterLength(ch, If(i + 1 < str.Length, str.Chars(i + 1), ControlChars.NullChar))
            If possibleNewline > 0 Then
                sb.AppendLine()
                If possibleNewline = 2 Then
                    i += 1
                End If
            Else
                sb.Append(ch)
            End If
        Next i
        Return sb.ToString()
    End Function

    Public Function GetRoslynRootDirectory() As String
        If _roslynRootDirectory.IsEmptyNullOrWhitespace Then
            _roslynRootDirectory = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile) & "\Source\Repos\roslyn"
        End If
        Return _roslynRootDirectory
    End Function

    <Extension()>
    Public Function VerifyOccurrenceCount(tree As SyntaxTree, kind As SyntaxKind, expectedCount As Integer) As SyntaxTree
        Dim actualCount As Integer = 0
        GetOccurrenceCount(kind, tree.GetRoot(), actualCount)
        Assert.Equal(expectedCount, actualCount)
        Return tree
    End Function
End Module