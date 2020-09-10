' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices

Namespace CSharpToVBConverter
    Public Module ICollectionExtensions

        <Extension>
        Friend Sub RemoveRange(Of T)(collection As ICollection(Of T), items As IEnumerable(Of T))
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
End Namespace
