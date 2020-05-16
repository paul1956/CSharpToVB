' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.Diagnostics.CodeAnalysis
Imports System.Runtime.CompilerServices

<ExcludeFromCodeCoverage>
Public Module ExceptionUtilities

    <ExcludeFromCodeCoverage>
    Public ReadOnly Property UnreachableException(<CallerMemberName> Optional memberName As String = Nothing,
                <CallerLineNumber()> Optional sourceLineNumber As Integer = 0) As Exception
        Get
            Return New InvalidOperationException($"The program location {memberName} line {sourceLineNumber} is thought to be unreachable.")
        End Get
    End Property

    <ExcludeFromCodeCoverage>
    Public Function UnexpectedValue(o As Object, <CallerMemberName> Optional memberName As String = Nothing,
                <CallerLineNumber()> Optional sourceLineNumber As Integer = 0) As Exception

        ' We do not throw from here because we don't want all Watson reports to be bucketed to this call.
        Return New InvalidOperationException($"Unexpected value '{o}' of type '{If(o Is Nothing, "<unknown>", o.GetType().FullName)}' at {memberName} line {sourceLineNumber}")
    End Function

End Module
