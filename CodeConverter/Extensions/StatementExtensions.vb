' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports CSharpToVBCodeConverter.Utilities
Imports Microsoft.CodeAnalysis
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Public Module StatementExtensions

    <Extension>
    Public Function RemoveLineContinuation(Statement As VBS.StatementSyntax) As VBS.StatementSyntax
        If Statement Is Nothing Then
            Throw New ArgumentNullException(NameOf(Statement))
        End If
        Return Statement.WithTrailingTrivia(Statement.GetTrailingTrivia.RemoveLineContinuation)
    End Function

End Module
