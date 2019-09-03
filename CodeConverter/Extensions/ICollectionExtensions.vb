Imports System.Runtime.CompilerServices

Public Module ICollectionExtensions
    <Extension>
    Public Sub RemoveRange(Of T)(collection As ICollection(Of T), items As IEnumerable(Of T))
        If collection Is Nothing Then
            Throw New ArgumentNullException(NameOf(collection))
        End If

        If items IsNot Nothing Then
            For Each item As T In items
                collection.Remove(item)
            Next item
        End If
    End Sub

End Module
