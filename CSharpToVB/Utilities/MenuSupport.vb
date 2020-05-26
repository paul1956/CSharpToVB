' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Public Module MenuSupport

    Friend Sub AddDropDownMenuItem(DropDownItems As ToolStripItemCollection, ItemName As String)
        DropDownItems.Add(New ToolStripMenuItem With {
            .AutoSize = True,
            .CheckOnClick = True,
            .ImageScaling = ToolStripItemImageScaling.None,
            .Name = $"{ItemName}ToolStripMenuItem",
            .Text = ItemName
        })
    End Sub

    Friend Sub mnuAddToMRU(mru_Data As Specialized.StringCollection, Path As String)
        ' remove the item from the collection if exists so that we can
        ' re-add it to the beginning...
        If mru_Data.Contains(Path) Then
            mru_Data.Remove(Path)
        End If
        ' add to MRU list..
        mru_Data.Add(Path)
        ' make sure there are only ever 5 items...
        While mru_Data.Count > 5
            mru_Data.RemoveAt(0)
        End While
    End Sub

    Friend Sub mnuMRUList_MouseDown(sender As Object, e As MouseEventArgs)
        If e.Button = Windows.Forms.MouseButtons.Right Then
            Clipboard.SetText(text:=CType(sender, ToolStripMenuItem).Text)
        End If
    End Sub

End Module
