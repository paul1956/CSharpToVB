' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

Imports System.Threading

Namespace Microsoft.CodeAnalysis.PooledObjects

    ''' <summary>
    ''' Generic implementation of object pooling pattern with predefined pool size limit. The main
    ''' purpose is that limited number of frequently used objects can be kept in the pool for
    ''' further recycling.
    '''
    ''' Notes:
    ''' 1) it is not the goal to keep all returned objects. Pool is not meant for storage. If there
    '''    is no space in the pool, extra returned objects will be dropped.
    '''
    ''' 2) it is implied that if object was obtained from a pool, the caller will return it back in
    '''    a relatively short time. Keeping checked out objects for long durations is OK, but
    '''    reduces usefulness of pooling. Just new up your own.
    '''
    ''' Not returning objects to the pool in not detrimental to the pool's work, but is a bad practice.
    ''' Rationale:
    '''    If there is no intent for reusing the object, do not use pool - just use "new".
    ''' </summary>
    Friend Class ObjectPool(Of T As Class)

        ' factory is stored for the lifetime of the pool. We will call this only when pool needs to
        ' expand. compared to "new T()", Func gives more flexibility to implementers and faster
        ' than "new T()".
        Private ReadOnly _factory As Factory

        Private ReadOnly _items() As Element

        ' Storage for the pool objects. The first item is stored in a dedicated field because we
        ' expect to be able to satisfy most requests from it.
        Private _firstItem As T

        Friend Sub New(factory As Factory, size As Integer)
            Debug.Assert(size >= 1)
            _factory = factory
            _items = New Element(size - 2) {}
        End Sub

        ''' <remarks>
        ''' Not using System.Func{T} because this file is linked into the (debugger) Formatter,
        ''' which does not have that type (since it compiles against .NET 2.0).
        ''' </remarks>
        Friend Delegate Function Factory() As T

        Private Function AllocateSlow() As T
            Dim items() As Element = _items

            For i As Integer = 0 To items.Length - 1
                ' Note that the initial read is optimistically not synchronized. That is intentional.
                ' We will interlock only when we have a candidate. in a worst case we may miss some
                ' recently returned objects. Not a big deal.
                Dim inst As T = items(i).Value
                If inst IsNot Nothing Then
                    If inst Is Interlocked.CompareExchange(items(i).Value, Nothing, inst) Then
                        Return inst
                    End If
                End If
            Next i

            Return CreateInstance()
        End Function

        Private Function CreateInstance() As T
            Dim inst As T = _factory()
            Return inst
        End Function

        Private Sub FreeSlow(obj As T)
            Dim items() As Element = _items
            For i As Integer = 0 To items.Length - 1
                If items(i).Value Is Nothing Then
                    ' Intentionally not using interlocked here.
                    ' In a worst case scenario two objects may be stored into same slot.
                    ' It is very unlikely to happen and will only mean that one of the objects will get collected.
                    items(i).Value = obj
                    Exit For
                End If
            Next i
        End Sub

        <Conditional("DEBUG")>
        Private Sub Validate(obj As Object)
            Dim items() As Element = _items
            For i As Integer = 0 To items.Length - 1
                Dim value As T = items(i).Value
                If value Is Nothing Then
                    Return
                End If
            Next i
        End Sub

        ''' <summary>
        ''' Produces an instance.
        ''' </summary>
        ''' <remarks>
        ''' Search strategy is a simple linear probing which is chosen for it cache-friendliness.
        ''' Note that Free will try to store recycled objects close to the start thus statistically
        ''' reducing how far we will typically search.
        ''' </remarks>
        Friend Function Allocate() As T
            ' PERF: Examine the first element. If that fails, AllocateSlow will look at the remaining elements.
            ' Note that the initial read is optimistically not synchronized. That is intentional.
            ' We will interlock only when we have a candidate. in a worst case we may miss some
            ' recently returned objects. Not a big deal.
            Dim inst As T = _firstItem
            If inst Is Nothing OrElse inst IsNot Interlocked.CompareExchange(_firstItem, Nothing, inst) Then
                inst = AllocateSlow()
            End If
            Return inst
        End Function

        ''' <summary>
        ''' Returns objects to the pool.
        ''' </summary>
        ''' <remarks>
        ''' Search strategy is a simple linear probing which is chosen for it cache-friendliness.
        ''' Note that Free will try to store recycled objects close to the start thus statistically
        ''' reducing how far we will typically search in Allocate.
        ''' </remarks>
        Friend Sub Free(obj As T)
            Validate(obj)
            ' ForgetTrackedObject(obj)

            If _firstItem Is Nothing Then
                ' Intentionally not using interlocked here.
                ' In a worst case scenario two objects may be stored into same slot.
                ' It is very unlikely to happen and will only mean that one of the objects will get collected.
                _firstItem = obj
            Else
                FreeSlow(obj)
            End If
        End Sub

        <DebuggerDisplay("{Value,nq}")>
        Private Structure Element
            Friend Value As T
        End Structure
    End Class

End Namespace
