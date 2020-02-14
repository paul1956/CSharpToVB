' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports CSharpToVBCodeConverter.Util

Imports Microsoft.CodeAnalysis

Imports VB = Microsoft.CodeAnalysis.VisualBasic

Public Module RestuructureSeparatedLists
    Public Property IgnoredIfDepth As Integer = 0

    Friend Sub RestructureNodesAndSeparators(Of T As VB.VisualBasicSyntaxNode)(ByRef _OpenToken As SyntaxToken, ByRef Items As List(Of T), ByRef Separators As List(Of SyntaxToken), ByRef _CloseToken As SyntaxToken)
        Dim TokenLeadingTrivia As New List(Of SyntaxTrivia)
        Dim NewOpenToken As SyntaxToken = _OpenToken.WithModifiedTokenTrivia(LeadingToken:=True, AfterEOL:=False)
        _OpenToken = NewOpenToken
        For i As Integer = 0 To Items.Count - 2
            Dim NewItem As T = Items(i).WithModifiedNodeTrivia(SeparatorFollows:=True)
            Items(i) = NewItem
            Dim newSeparators As SyntaxToken = Separators(i).WithModifiedTokenTrivia(LeadingToken:=False, AfterEOL:=False)
            Separators(i) = newSeparators
        Next
        Dim LastItemEndsWithEOL As Boolean = False
        If Items.Count > 0 Then
            Dim NewItem As T = Items.Last.WithModifiedNodeTrivia(SeparatorFollows:=False)
            Items(Items.Count - 1) = NewItem
            LastItemEndsWithEOL = Items.Last.HasTrailingTrivia AndAlso Items.Last.GetTrailingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia)
        End If
        Dim newCloseToken As SyntaxToken = _CloseToken.WithModifiedTokenTrivia(LeadingToken:=False, LastItemEndsWithEOL)
        _CloseToken = newCloseToken
    End Sub

End Module
