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

    Friend Sub MRU_UpdateUI(dropDownItems As ToolStripItemCollection, ClickEvent As EventHandler, IncludeMouseDownEvent As Boolean)
        ' clear MRU menu items...
        Dim MRUToolStripItems As New List(Of ToolStripItem)
        ' create a temporary collection containing every MRU menu item
        ' (identified by the tag text when added to the list)...
        For Each FileMenuItem As ToolStripItem In dropDownItems
            If Not FileMenuItem.Tag Is Nothing Then
                If FileMenuItem.Tag.ToString().StartsWith("MRU:", StringComparison.Ordinal) Then
                    MRUToolStripItems.Add(FileMenuItem)
                End If
            End If
        Next
        ' iterate through list and remove each from menu...
        For Each MRUToolStripItem As ToolStripItem In MRUToolStripItems
            RemoveHandler MRUToolStripItem.Click, ClickEvent
            If IncludeMouseDownEvent Then
                RemoveHandler MRUToolStripItem.MouseDown, AddressOf mnuMRUList_MouseDown
            End If
            dropDownItems.Remove(MRUToolStripItem)
        Next
        ' display items (in reverse order so the most recent is on top)...
        For iCounter As Integer = My.Settings.MRU_Data.Count - 1 To 0 Step -1
            Dim sPath As String = My.Settings.MRU_Data(iCounter)
            ' create new ToolStripItem, displaying the name of the file...
            ' set the tag - identifies the ToolStripItem as an MRU item and
            ' contains the full path so it can be opened later...
#Disable Warning CA2000 ' Dispose objects before losing scope
            Dim clsItem As New ToolStripMenuItem(sPath) With {
                .Tag = "MRU:" & sPath
            }
#Enable Warning CA2000 ' Dispose objects before losing scope
            ' hook into the click event handler so we can open the file later...
            AddHandler clsItem.Click, ClickEvent
            If IncludeMouseDownEvent Then
                AddHandler clsItem.MouseDown, AddressOf mnuMRUList_MouseDown
            End If
            ' insert into DropDownItems list...
            dropDownItems.Insert(dropDownItems.Count - 10, clsItem)
        Next

    End Sub

End Module
