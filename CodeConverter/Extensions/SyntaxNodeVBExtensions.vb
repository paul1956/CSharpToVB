' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports CSharpToVBConverter.ToVisualBasic

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports VB = Microsoft.CodeAnalysis.VisualBasic

Namespace CSharpToVBConverter

    Module SyntaxNodeVBExtensions

        <Extension>
        Friend Function [With](Of T As VB.VisualBasicSyntaxNode)(node As T, leadingTrivia As IEnumerable(Of SyntaxTrivia), trailingTrivia As IEnumerable(Of SyntaxTrivia)) As T
            Return node.WithLeadingTrivia(leadingTrivia).WithTrailingTrivia(trailingTrivia)
        End Function

        <Extension>
        Friend Function WithUniqueLeadingTrivia(Of T As VB.VisualBasicSyntaxNode)(Node As T, HeaderLeadingTrivia As SyntaxTriviaList) As T
            Dim NodeLeadingTrivia As SyntaxTriviaList = Node.GetLeadingTrivia
            If NodeLeadingTrivia.Count = 0 Then
                Return Node
            End If
            If NodeLeadingTrivia.First.Language = "C#" Then
                NodeLeadingTrivia = NodeLeadingTrivia.ConvertTriviaList
            End If
            If HeaderLeadingTrivia.Count = 0 Then
                Return Node
            End If

            If Not NodeLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                Return Node
            End If
            Dim index As Integer
            For index = 0 To HeaderLeadingTrivia.Count - 1
                If HeaderLeadingTrivia(index).RawKind <> NodeLeadingTrivia(index).RawKind Then
                    Exit For
                End If
                If HeaderLeadingTrivia(index).ToString <> NodeLeadingTrivia(index).ToString Then
                    Exit For
                End If
            Next
            Dim newLeadingTrivia As New SyntaxTriviaList
            For i As Integer = index To NodeLeadingTrivia.Count - 1
                If i <> 0 AndAlso i = index AndAlso NodeLeadingTrivia(i).IsKind(CS.SyntaxKind.EndOfLineTrivia) Then
                    Continue For
                End If
                newLeadingTrivia = newLeadingTrivia.Add(NodeLeadingTrivia(i))
            Next
            Return Node.WithLeadingTrivia(newLeadingTrivia)
        End Function

    End Module
End Namespace
