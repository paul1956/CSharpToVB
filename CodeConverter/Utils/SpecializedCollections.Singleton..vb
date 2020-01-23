' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Partial Public Module SpecializedCollections

    Friend NotInheritable Class SingletonList(Of T)
        Implements IList(Of T), IReadOnlyCollection(Of T)

        Private ReadOnly _loneValue As T

        Public Sub New(value As T)
            _loneValue = value
        End Sub

        Public Sub Add(item As T) Implements ICollection(Of T).Add
            Throw New NotSupportedException()
        End Sub

        Public Sub Clear() Implements ICollection(Of T).Clear
            Throw New NotSupportedException()
        End Sub

        Public Function Contains(item As T) As Boolean Implements ICollection(Of T).Contains
            Return EqualityComparer(Of T).Default.Equals(_loneValue, item)
        End Function

        Public Sub CopyTo(array() As T, arrayIndex As Integer) Implements ICollection(Of T).CopyTo
            array(arrayIndex) = _loneValue
        End Sub

        Public ReadOnly Property Count() As Integer Implements ICollection(Of T).Count, IReadOnlyCollection(Of T).Count
            Get
                Return 1
            End Get
        End Property

        Public ReadOnly Property IsReadOnly() As Boolean Implements ICollection(Of T).IsReadOnly
            Get
                Return True
            End Get
        End Property

        Public Function Remove(item As T) As Boolean Implements ICollection(Of T).Remove
            Throw New NotSupportedException()
        End Function

        Public Function GetEnumerator() As IEnumerator(Of T) Implements IEnumerable(Of T).GetEnumerator
            Return New Enumerator(Of T)(_loneValue)
        End Function

        Private Function IEnumerable_GetEnumerator() As IEnumerator Implements IEnumerable.GetEnumerator
            Return GetEnumerator()
        End Function

        Default Public Property Item(index As Integer) As T Implements IList(Of T).Item
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

        Public Function IndexOf(item As T) As Integer Implements IList(Of T).IndexOf
            If Equals(_loneValue, item) Then
                Return 0
            End If

            Return -1
        End Function

        Public Sub Insert(index As Integer, item As T) Implements IList(Of T).Insert
            Throw New NotSupportedException()
        End Sub

        Public Sub RemoveAt(index As Integer) Implements IList(Of T).RemoveAt
            Throw New NotSupportedException()
        End Sub

    End Class

    Friend Class Enumerator(Of T)
        Implements IEnumerator(Of T)

        Private _moveNextCalled As Boolean

        Public Sub New(value As T)
            Current = value
            _moveNextCalled = False
        End Sub

        Public ReadOnly Property Current() As T Implements IEnumerator(Of T).Current

        Private ReadOnly Property IEnumerator_Current() As Object Implements IEnumerator.Current
            Get
                Return Current
            End Get
        End Property

        Public Function MoveNext() As Boolean Implements IEnumerator(Of T).MoveNext
            If Not _moveNextCalled Then
                _moveNextCalled = True
                Return True
            End If

            Return False
        End Function

        Public Sub Reset() Implements IEnumerator(Of T).Reset
            _moveNextCalled = False
        End Sub

#Region "IDisposable Support"

        Private _disposedValue As Boolean ' To detect redundant calls

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

        ' This code added by Visual Basic to correctly implement the disposable pattern.
        Public Sub Dispose() Implements IDisposable.Dispose
            ' Do not change this code.  Put cleanup code in Dispose(disposing As Boolean) above.
            Dispose(True)
            ' TODO: uncomment the following line if Finalize() is overridden above.
            ' GC.SuppressFinalize(Me)
        End Sub

#End Region

    End Class

End Module
