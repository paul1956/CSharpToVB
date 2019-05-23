Imports System.Runtime.CompilerServices

Partial Public Module SpecializedCollection
    <Extension>
    Public Function Last(ByVal _StringCollection As Specialized.StringCollection) As String

        Return If(_StringCollection Is Nothing OrElse _StringCollection.Count = 0, "", _StringCollection.Item(_StringCollection.Count - 1))
    End Function

    Public Function SingletonEnumerable(Of T)(value As T) As IEnumerable(Of T)
        Return New Singleton.List(Of T)(value)
    End Function

End Module
