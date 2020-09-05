' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices

Namespace CSharpToVBConverter

    Public Module EnumerableExtensions

        <Extension>
        Friend Function Contains(Of T)(sequence As IEnumerable(Of T), predicate As Func(Of T, Boolean)) As Boolean
            Return sequence.Any(predicate)
        End Function

        <Extension>
        Friend Iterator Function IndexedSelect(Of T, TReturn)(source As IEnumerable(Of T), transform As Func(Of Integer, T, TReturn)) As IEnumerable(Of TReturn)
            Dim i As Integer = 0
            For Each item As T In source
                Yield transform(i, item)
                i += 1
            Next item
        End Function

        <Extension>
        Public Iterator Function [Yield](Of T)(singleElement As T) As IEnumerable(Of T)
            Yield singleElement
        End Function

    End Module
End Namespace
