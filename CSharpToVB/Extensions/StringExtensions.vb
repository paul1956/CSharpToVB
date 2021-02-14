' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices

Imports Microsoft.CodeAnalysis

Public Module StringExtensions

    <Extension()>
    Public Function Count(value As String, ch As Char) As Integer
        If value Is Nothing Then
            Return 0
        End If
        Return value.Count(Function(c As Char) c = ch)
    End Function

    <Extension>
    Public Function Join(source As IEnumerable(Of String), separator As String) As String
        If source Is Nothing Then
            Throw New ArgumentNullException(NameOf(source))
        End If

        If separator Is Nothing Then
            Throw New ArgumentNullException(NameOf(separator))
        End If

        Return String.Join(separator, source)
    End Function

End Module
