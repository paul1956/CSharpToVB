' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices

Module ToolStripExtensions

    <Extension>
    Public Function IndexOf(ContextMenu As ContextMenuStrip, Text As String, Optional searchAllChildren As Boolean = False) As Integer
        Return ContextMenu.Items.IndexOf(ContextMenu.Items.Find(Text, searchAllChildren)(0))
    End Function

End Module
