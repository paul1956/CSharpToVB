' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Public Class NumberedListItem
    Public Sub New(display As String, value As String)
        Me._displayItem = display
        Me._valueItem = value
    End Sub

    Private Property _displayItem As String

    Private Property _valueItem As String

    Public Property ValueItem As String
        Get
            Return Me._valueItem
        End Get
        Set(value As String)
            Me._valueItem = value
        End Set
    End Property

    Public Function SourceFileWithPath() As String
        If String.IsNullOrEmpty(Me._displayItem) Then
            Return String.Empty
        End If
        Return Me._displayItem.Split(" ", StringSplitOptions.RemoveEmptyEntries)(1)
    End Function

    Public Overrides Function ToString() As String
        Return Me._displayItem.ToString(Globalization.CultureInfo.CurrentCulture)
    End Function

End Class
