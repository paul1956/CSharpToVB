' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.IO
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Text

Public NotInheritable Class AnalyzerAdditionalFile
    Inherits AdditionalText

    Public Sub New(path As String)
        Me.Path = path
    End Sub

    Public Overrides ReadOnly Property Path As String

    Public Overrides Function GetText(Optional cancellationToken As Threading.CancellationToken = Nothing) As Text.SourceText
        Return SourceText.From(File.ReadAllText(Me.Path))
    End Function
End Class
