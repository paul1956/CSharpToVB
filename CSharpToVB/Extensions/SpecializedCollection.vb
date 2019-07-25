' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
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
