﻿' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices

Friend Module RichTextBoxExtensions

    <Extension>
    Public Function FindIndexOfAny(rtb As RichTextBox, ParamArray strings() As String) As Integer
        For Each s As String In strings
            Dim keywordIndex As Integer = rtb.Find(s)
            If keywordIndex >= 0 Then
                Return keywordIndex
            End If
        Next
        Return -1
    End Function

End Module
