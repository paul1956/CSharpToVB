' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.IO
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Text

Public NotInheritable Class AnalyzerAdditionalFile
    Inherits AdditionalText

    Private ReadOnly _path As String

    Public Sub New(path As String)
        _path = path
    End Sub
    Public Overrides ReadOnly Property Path As String
        Get
            Return _path
        End Get
    End Property

    Public Overrides Function GetText(Optional cancellationToken As Threading.CancellationToken = Nothing) As Text.SourceText
        Dim info As FileInfo = New FileInfo(Me.Path)
        Return SourceText.From(File.ReadAllText(Me.Path))
    End Function
End Class
