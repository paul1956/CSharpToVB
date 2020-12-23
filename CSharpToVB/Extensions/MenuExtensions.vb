' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.IO
Imports System.Runtime.CompilerServices

Friend Module MenuExtensions

    Private Sub FileMenuMRUList_MouseDown(sender As Object, e As MouseEventArgs)
        If e.Button = Windows.Forms.MouseButtons.Right Then
            Clipboard.SetText(text:=CType(sender, ToolStripMenuItem).Text)
        End If
    End Sub

    <Extension>
    Friend Sub FileMenuMRUUpdateUI(dropDownItems As ToolStripItemCollection, ClickEvent As EventHandler)
        ' clear MRU menu items...
        Dim mruToolStripItems As New List(Of ToolStripItem)
        ' create a temporary collection containing every MRU menu item
        ' (identified by the tag text when added to the list)...
        For Each fileMenuItem As ToolStripItem In dropDownItems
            If fileMenuItem.Tag IsNot Nothing Then
                If fileMenuItem.Tag.ToString().StartsWith("MRU:", StringComparison.Ordinal) Then
                    mruToolStripItems.Add(fileMenuItem)
                End If
            End If
        Next
        ' iterate through list and remove each from menu...
        For Each mruToolStripItem As ToolStripItem In mruToolStripItems
            RemoveHandler mruToolStripItem.Click, ClickEvent
            RemoveHandler mruToolStripItem.MouseDown, AddressOf FileMenuMRUList_MouseDown
            dropDownItems.Remove(mruToolStripItem)
            mruToolStripItem.Dispose()
        Next
        ' display items (in reverse order so the most recent is on top)...
        For iCounter As Integer = My.Settings.MRU_Data.Count - 1 To 0 Step -1
            Dim sPath As String = My.Settings.MRU_Data(iCounter)
            ' create new ToolStripItem, displaying the name of the file...
            ' set the tag - identifies the ToolStripItem as an MRU item and
            ' contains the full path so it can be opened later...
            Dim clsItem As New ToolStripMenuItem(sPath) With {
                .Tag = "MRU:" & sPath
            }
            ' hook into the click event handler so we can open the file later...
            AddHandler clsItem.Click, ClickEvent
            AddHandler clsItem.MouseDown, AddressOf FileMenuMRUList_MouseDown
            ' insert into DropDownItems list...
            dropDownItems.Insert(dropDownItems.Count - 10, clsItem)
        Next

    End Sub

    <Extension>
    Friend Function IndexOf(ContextMenu As ContextMenuStrip, Text As String, Optional searchAllChildren As Boolean = False) As Integer
        Return ContextMenu.Items.IndexOf(ContextMenu.Items.Find(Text, searchAllChildren)(0))
    End Function

    <Extension>
    Friend Sub mnuAddToMRU(mru_Data As Specialized.StringCollection, Text As String)
        ' remove the item from the collection if exists so that we can
        ' re-add it to the beginning...
        If mru_Data.Contains(Text) Then
            mru_Data.Remove(Text)
        End If
        ' add to MRU list..
        mru_Data.Add(Text)
        ' make sure there are only ever 5 items...
        While mru_Data.Count > 5
            mru_Data.RemoveAt(0)
        End While
    End Sub

    <Extension>
    Friend Sub TSFindWhatMRUUpdateUI(dropDownItems As ToolStripComboBox)
        ' clear MRU menu items...
        Dim mruToolStripItems As New List(Of ToolStripItem)
        dropDownItems.Items.Clear()
        ' display items (in reverse order so the most recent is on top)...
        For iCounter As Integer = My.Settings.TSFindMRU_Data.Count - 1 To 0 Step -1
            dropDownItems.Items.Add(My.Settings.TSFindMRU_Data(iCounter))
        Next
    End Sub

    <Extension>
    Friend Sub UpdateLastFileMenu(MainForm As Form1)
        My.Settings.Save()
        ' show separator...
        If My.Settings.MRU_Data.Count > 0 Then
            MainForm.mnuFileLastFolder.Text = Path.GetDirectoryName(My.Settings.MRU_Data.Last)
            MainForm.mnuFileLastFolder.Visible = True
            MainForm.mnuFileSep1.Visible = True
            MainForm.mnuFileSep2.Visible = True
        Else
            MainForm.mnuFileLastFolder.Visible = False
            MainForm.mnuFileSep1.Visible = False
            MainForm.mnuFileSep2.Visible = False
        End If
    End Sub

End Module
