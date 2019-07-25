﻿' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Partial Module SpecializedCollections

    Partial Friend NotInheritable Class Singleton

        Friend Class Enumerator(Of T)
            Implements IEnumerator(Of T)

            Private _moveNextCalled As Boolean

            Public Sub New(value As T)
                Me.Current = value
                Me._moveNextCalled = False
            End Sub

            Public ReadOnly Property Current() As T Implements IEnumerator(Of T).Current

            Private ReadOnly Property IEnumerator_Current() As Object Implements IEnumerator.Current
                Get
                    Return Me.Current
                End Get
            End Property

            Public Function MoveNext() As Boolean Implements IEnumerator(Of T).MoveNext
                If Not Me._moveNextCalled Then
                    Me._moveNextCalled = True
                    Return True
                End If

                Return False
            End Function

            Public Sub Reset() Implements IEnumerator(Of T).Reset
                Me._moveNextCalled = False
            End Sub

#Region "IDisposable Support"

            Private disposedValue As Boolean ' To detect redundant calls

            ' IDisposable
            Protected Overridable Sub Dispose(disposing As Boolean)
                If Not Me.disposedValue Then
                    If disposing Then
                        ' TODO: dispose managed state (managed objects).
                    End If

                    ' TODO: free unmanaged resources (unmanaged objects) and override Finalize() below.
                    ' TODO: set large fields to null.
                End If
                Me.disposedValue = True
            End Sub

            ' This code added by Visual Basic to correctly implement the disposable pattern.
            Public Sub Dispose() Implements IDisposable.Dispose
                ' Do not change this code.  Put cleanup code in Dispose(disposing As Boolean) above.
                Me.Dispose(True)
                ' TODO: uncomment the following line if Finalize() is overridden above.
                ' GC.SuppressFinalize(Me)
            End Sub

#End Region

        End Class

    End Class

End Module