Option Explicit On
Option Infer Off
Option Strict On

Public Class SymbolTableEntry
    Public Sub New(_Name As String, _IsType As Boolean)
        Me.Name = _Name
        Me.IsType = _IsType
    End Sub

    Public ReadOnly Property IsType As Boolean
    Public ReadOnly Property Name As String
End Class
