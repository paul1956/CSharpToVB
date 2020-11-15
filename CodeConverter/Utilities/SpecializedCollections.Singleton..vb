' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Diagnostics.CodeAnalysis

Namespace CSharpToVBConverter
    <ExcludeFromCodeCoverage>
    Partial Public Module SpecializedCollections

        Private Class Enumerator(Of T)
            Implements IEnumerator(Of T)

            Private _disposedValue As Boolean
            Private _moveNextCalled As Boolean

            Friend Sub New(value As T)
                Me.Current = value
                _moveNextCalled = False
            End Sub

            Private ReadOnly Property IEnumerator_Current() As Object Implements IEnumerator.Current
                Get
                    Return Me.Current
                End Get
            End Property

            Friend ReadOnly Property Current() As T Implements IEnumerator(Of T).Current

            ' IDisposable
            Protected Overridable Sub Dispose(disposing As Boolean)
                If Not _disposedValue Then
                    If disposing Then
                        ' TODO: dispose managed state (managed objects).
                    End If

                    ' TODO: free unmanaged resources (unmanaged objects) and override Finalize() below.
                    ' TODO: set large fields to null.
                End If
                _disposedValue = True
            End Sub

            ' To detect redundant calls
            ' This code added by Visual Basic to correctly implement the disposable pattern.
            Friend Sub Dispose() Implements IDisposable.Dispose
                ' Do not change this code.  Put cleanup code in Dispose(disposing As Boolean) above.
                Me.Dispose(True)
                ' TODO: uncomment the following line if Finalize() is overridden above.
                ' GC.SuppressFinalize(Me)
            End Sub

            Friend Function MoveNext() As Boolean Implements IEnumerator(Of T).MoveNext
                If Not _moveNextCalled Then
                    _moveNextCalled = True
                    Return True
                End If

                Return False
            End Function

            Friend Sub Reset() Implements IEnumerator(Of T).Reset
                _moveNextCalled = False
            End Sub

        End Class

        Friend NotInheritable Class SingletonList(Of T)
            Implements IList(Of T), IReadOnlyCollection(Of T)

            Private ReadOnly _loneValue As T

            Friend Sub New(value As T)
                _loneValue = value
            End Sub

            Friend ReadOnly Property Count() As Integer Implements ICollection(Of T).Count, IReadOnlyCollection(Of T).Count
                Get
                    Return 1
                End Get
            End Property

            Friend ReadOnly Property IsReadOnly() As Boolean Implements ICollection(Of T).IsReadOnly
                Get
                    Return True
                End Get
            End Property

            Default Friend Property item(index As Integer) As T Implements IList(Of T).item
                Get
                    If index <> 0 Then
                        Throw New IndexOutOfRangeException()
                    End If

                    Return _loneValue
                End Get

                Set(value As T)
                    Throw New NotSupportedException()
                End Set
            End Property

            Private Function IEnumerable_GetEnumerator() As IEnumerator Implements IEnumerable.GetEnumerator
                Return Me.GetEnumerator()
            End Function

            Friend Sub Add(item As T) Implements ICollection(Of T).Add
                Throw New NotSupportedException()
            End Sub

            Friend Sub Clear() Implements ICollection(Of T).Clear
                Throw New NotSupportedException()
            End Sub

            Friend Function Contains(item As T) As Boolean Implements ICollection(Of T).Contains
                Return EqualityComparer(Of T).Default.Equals(_loneValue, item)
            End Function

            Friend Sub CopyTo(array() As T, arrayIndex As Integer) Implements ICollection(Of T).CopyTo
                array(arrayIndex) = _loneValue
            End Sub

            Friend Function GetEnumerator() As IEnumerator(Of T) Implements IEnumerable(Of T).GetEnumerator
                Return New Enumerator(Of T)(_loneValue)
            End Function

            Friend Function IndexOf(item As T) As Integer Implements IList(Of T).IndexOf
                If Equals(_loneValue, item) Then
                    Return 0
                End If

                Return -1
            End Function

            Friend Sub Insert(index As Integer, item As T) Implements IList(Of T).Insert
                Throw New NotSupportedException()
            End Sub

            Friend Function Remove(item As T) As Boolean Implements ICollection(Of T).Remove
                Throw New NotSupportedException()
            End Function

            Friend Sub RemoveAt(index As Integer) Implements IList(Of T).RemoveAt
                Throw New NotSupportedException()
            End Sub

        End Class

    End Module
End Namespace
