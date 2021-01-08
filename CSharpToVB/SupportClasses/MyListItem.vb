' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Public Class MyListItem

    Public Sub New(pText As String, pValue As String)
        _Text = pText
        _Value = pValue
    End Sub

    Public ReadOnly Property Text() As String

    Public ReadOnly Property Value() As String

    Public Overrides Function ToString() As String
        Return Me.Text
    End Function

End Class
