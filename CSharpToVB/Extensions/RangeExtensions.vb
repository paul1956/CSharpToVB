' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices

Friend Module RangeExtensions

    <Extension>
    Public Function Last(Of T)(list As IReadOnlyList(Of T)) As T
        Return list.ItemFromEnd(1)
    End Function

    <Extension>
    Public Function ItemFromEnd(Of T)(list As IReadOnlyList(Of T), index As Integer) As T
        Return list(list.Count - index)
    End Function

End Module
