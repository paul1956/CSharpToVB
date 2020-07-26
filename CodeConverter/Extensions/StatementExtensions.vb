' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports System.Runtime.InteropServices

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

Public Module StatementExtensions

    <Extension>
    Friend Function TryUnpackExpression(statementSyntax As StatementSyntax, <Out> ByRef expression As ExpressionSyntax) As Boolean
        If TypeOf statementSyntax Is ReturnStatementSyntax Then
            expression = DirectCast(statementSyntax, ReturnStatementSyntax).Expression
        ElseIf TypeOf statementSyntax Is YieldStatementSyntax Then
            expression = DirectCast(statementSyntax, YieldStatementSyntax).Expression
        Else
            expression = Nothing
        End If

        Return expression IsNot Nothing
    End Function

    <Extension>
    Public Function RemoveLineContinuation(Statement As StatementSyntax) As StatementSyntax
        If Statement Is Nothing Then
            Throw New ArgumentNullException(NameOf(Statement))
        End If
        Return Statement.WithTrailingTrivia(Statement.GetTrailingTrivia.RemoveLineContinuation)
    End Function

End Module
