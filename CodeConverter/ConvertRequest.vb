Option Explicit On
Option Infer Off
Option Strict On

#If NETCOREAPP3_0 Then
Imports ManageProgressBar
#Else
Imports ReportProgress = System.Object
#End If

Public Class ConvertRequest
    Sub New(_RequestedConversion As String, _DoEvents As DoEvents, _ProgressBar As ReportProgress)
        Me.MyDoEvents = _DoEvents
        Me.ProgressBar = _ProgressBar
        Me.RequestedConversion = _RequestedConversion

    End Sub

    Private Property RequestedConversion As String

    Public Delegate Sub DoEvents()
    Public ProgressBar As ReportProgress
    Public Property SourceCode As String
    Public ReadOnly Property MyDoEvents As DoEvents

    Public Function GetSourceExtension() As String
        Return Me.RequestedConversion.ToString.Left(2)
    End Function

    Public Function GetTargetExtension() As String
        Return Me.RequestedConversion.ToString.Right(2)
    End Function

End Class

Public NotInheritable Class ConversionOperation
    Public Const CS_To_VB As String = "cs2vb"
    Public Const VB_To_CS As String = "vb2CS"
End Class
