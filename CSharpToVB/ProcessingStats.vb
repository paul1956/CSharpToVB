' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Public NotInheritable Class ProcessingStats
    Public Sub New(LastFileNameWithPath As String)
        Me.LastFileNameWithPath = LastFileNameWithPath
    End Sub

    Public Property LastFileNameWithPath As String
    Public Property FilesProcessed As Long
    Public Property TotalFilesToProcess As Long
End Class
