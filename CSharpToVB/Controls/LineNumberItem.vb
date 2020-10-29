' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Partial Public Class LineNumbersForRichTextBox

    Private Class LineNumberItem
        Friend _lineNumber As Integer
        Friend _rectangle As Rectangle

        Friend Sub New(zLineNumber As Integer, zRectangle As Rectangle)
            _lineNumber = zLineNumber
            _rectangle = zRectangle
        End Sub

    End Class

End Class
