' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Public Class IndexClass(Of T)
    Public Property Value As T
    Public Property Index As Integer                                                           ' first element has index = 0

    Public ReadOnly Property IsFirst As Boolean
        Get
            Return Index = 0
        End Get
    End Property

    Public Property IsLast As Boolean
    Public Property Enumerator As IEnumerator(Of T)

    Public Sub MoveNext()
        Value = Enumerator.Current
        IsLast = Not Enumerator.MoveNext()
        ' may be called with .AsParallel
        Threading.Interlocked.Increment(Index)
    End Sub

End Class
