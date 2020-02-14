' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports Microsoft.CodeAnalysis
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Public Module StatementTriviaExtensions

    <Extension>
    Public Function RemoveLineContinuation(Statement As VBS.StatementSyntax) As VBS.StatementSyntax
        If Statement Is Nothing Then
            Throw New ArgumentNullException(NameOf(Statement))
        End If

        Dim TrailingTrivia As New List(Of SyntaxTrivia)
        TrailingTrivia.AddRange(Statement.GetTrailingTrivia)
        If TrailingTrivia.Count > 2 Then
            If TrailingTrivia(1).IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                Statement = Statement.WithTrailingTrivia(TrailingTrivia.GetRange(2, TrailingTrivia.Count - 2))
            End If
        End If
        Return Statement
    End Function

End Module