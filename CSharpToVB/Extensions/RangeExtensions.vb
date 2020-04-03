Imports System.Runtime.CompilerServices

Module RangeExtensions
    <Extension>
    Function Last(Of T)(list As IReadOnlyList(Of T)) As T
        Return list.ItemFromEnd(1)
    End Function

    <Extension>
    Function ItemFromEnd(Of T)(list As IReadOnlyList(Of T), index As Integer) As T
        Return list(list.Count - index)
    End Function
End Module