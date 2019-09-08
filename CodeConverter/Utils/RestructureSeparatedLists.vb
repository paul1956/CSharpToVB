﻿' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

Imports CSharpToVBCodeConverter.Util

Imports Microsoft.CodeAnalysis

Imports VB = Microsoft.CodeAnalysis.VisualBasic

Public Module RestuructureSeparatedLists
    Public Property IgnoredIfDepth As Integer = 0

    Friend Sub RestructureNodesAndSeparators(Of T As VB.VisualBasicSyntaxNode)(ByRef _OpenToken As SyntaxToken, ByRef Items As List(Of T), ByRef Separators As List(Of SyntaxToken), ByRef _CloseToken As SyntaxToken)
        Dim TokenLeadingTrivia As New List(Of SyntaxTrivia)
        _OpenToken = _OpenToken.WithModifiedTokenTrivia(LeadingToken:=True, AfterEOL:=False)
        For i As Integer = 0 To Items.Count - 2
            Items(i) = Items(i).WithModifiedNodeTrivia(SeparatorFollows:=True)
            Separators(i) = Separators(i).WithModifiedTokenTrivia(LeadingToken:=False, AfterEOL:=False)
        Next
        Dim LastItemEndsWithEOL As Boolean = False
        If Items.Count > 0 Then
            Items(Items.Count - 1) = Items.Last.WithModifiedNodeTrivia(SeparatorFollows:=False)
            LastItemEndsWithEOL = Items.Last.HasTrailingTrivia AndAlso Items.Last.GetTrailingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia)
        End If

        _CloseToken = _CloseToken.WithModifiedTokenTrivia(LeadingToken:=False, LastItemEndsWithEOL)
    End Sub

End Module
