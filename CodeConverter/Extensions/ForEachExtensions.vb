' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices

Public Module ForEachExtensions
    <Extension>
    Public Iterator Function WithIndex(Of T)(
                                         source As IEnumerable(Of T),
                                         Optional minIndex As Integer = 0                      ' optionally skip elements at begin
                                         ) As IEnumerable(Of IndexStruct(Of T))
        If source Is Nothing Then
            Throw New ArgumentNullException(NameOf(source))
        End If

        Using enumerator As IEnumerator(Of T) = source.GetEnumerator
            Dim isFirst As Boolean = True
            Dim hasNext As Boolean = enumerator.MoveNext
            Dim i As Integer = 0
            While hasNext
                Dim current As T = enumerator.Current
                hasNext = enumerator.MoveNext
                If i >= minIndex Then
                    Yield New IndexStruct(Of T) With {.Index = i, .Value = current, .IsFirst = isFirst, .IsLast = Not hasNext, .MinIndex = minIndex}
                    isFirst = False
                End If
                Threading.Interlocked.Increment(i) ' index += 1, may be called with .AsParallel
            End While
        End Using
    End Function
End Module
