' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Public Class NumberedListItem

    Public Sub New(display As String, value As String)
        Me.DisplayItem = display
        Me.ValueItem = value
    End Sub

    Private Property DisplayItem As String
    Public Property ValueItem As String

    Public Function SourceFileWithPath() As String
        If String.IsNullOrEmpty(Me.DisplayItem) Then
            Return String.Empty
        End If
        Return Me.DisplayItem.Split(" ", StringSplitOptions.RemoveEmptyEntries)(1)
    End Function

    Public Overrides Function ToString() As String
        Return Me.DisplayItem.ToString(Globalization.CultureInfo.CurrentCulture)
    End Function

End Class
