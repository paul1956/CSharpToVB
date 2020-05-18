' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.Collections.Immutable
Imports System.Runtime.CompilerServices

Public Module ImmutableArrayExtensions

    ''' <summary>
    ''' Maps an immutable array to another immutable array.
    ''' </summary>
    ''' <typeparam name="TItem"></typeparam>
    ''' <typeparam name="TResult"></typeparam>
    ''' <param name="items">The array to map</param>
    ''' <param name="map">The mapping delegate</param>
    ''' <returns>If the items's length is 0, this will return an empty immutable array</returns>
    <Extension>
    Public Function SelectAsArray(Of TItem, TResult)(items As ImmutableArray(Of TItem), map As Func(Of TItem, TResult)) As ImmutableArray(Of TResult)
        Return ImmutableArray.CreateRange(items, map)
    End Function

    ''' <summary>
    ''' Creates a new immutable array based on filtered elements by the predicate. The array must not be null.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="array">The array to process</param>
    ''' <param name="predicate">The delegate that defines the conditions of the element to search for.</param>
    ''' <returns></returns>
    <Extension>
    Public Function WhereAsArray(Of T)(array As ImmutableArray(Of T), predicate As Func(Of T, Boolean)) As ImmutableArray(Of T)
        Debug.Assert(Not array.IsDefault)

        Dim builder As List(Of T) = Nothing
        Dim none As Boolean = True
        Dim all As Boolean = True

        Dim n As Integer = array.Length
        For index As Integer = 0 To n - 1
            Dim a As T = array(index)
            If predicate(a) Then
                none = False
                If all Then
                    Continue For
                End If

                Debug.Assert(index > 0)
                If builder Is Nothing Then
                    builder = New List(Of T)
                End If

                builder.Add(a)
            Else
                If none Then
                    all = False
                    Continue For
                End If

                Debug.Assert(index > 0)
                If all Then
                    Debug.Assert(builder Is Nothing)
                    all = False
                    builder = New List(Of T)
                    For j As Integer = 0 To index - 1
                        builder.Add(array(j))
                    Next j
                End If
            End If
        Next index

        If builder IsNot Nothing Then
            Debug.Assert(Not all)
            Debug.Assert(Not none)
            Return ImmutableArray.Create(builder.ToArray)
        ElseIf all Then
            Return array
        Else
            Debug.Assert(none)
            Return ImmutableArray(Of T).Empty
        End If
    End Function

End Module
