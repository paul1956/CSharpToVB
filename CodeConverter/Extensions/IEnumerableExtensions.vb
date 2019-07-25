' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.Runtime.CompilerServices

Public Module IEnumerableExtensions

    <Extension>
    Public Function Concat(Of T)(ByVal source As IEnumerable(Of T), ByVal value As T) As IEnumerable(Of T)
        If source Is Nothing Then
            Throw New ArgumentNullException(NameOf(source))
        End If

        Return source.ConcatWorker(value)
    End Function

    <Extension>
    Public Iterator Function ConcatWorker(Of T)(ByVal source As IEnumerable(Of T), ByVal value As T) As IEnumerable(Of T)
        For Each v As T In source
            Yield v
        Next v

        Yield value
    End Function

End Module