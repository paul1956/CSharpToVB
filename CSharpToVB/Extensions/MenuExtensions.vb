﻿' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.IO
Imports System.Runtime.CompilerServices

Friend Module MenuExtensions

    Private Sub FileMenuMRUList_MouseDown(sender As Object, e As MouseEventArgs)
        If e.Button = MouseButtons.Right Then
            Clipboard.SetText(text:=CType(sender, ToolStripMenuItem).Text)
        End If
    End Sub

    <Extension>
    Friend Sub FileMenuMruUpdateUi(dropDownItems As ToolStripItemCollection, clickEvent As EventHandler)
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
            RemoveHandler mruToolStripItem.Click, clickEvent
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
            AddHandler clsItem.Click, clickEvent
            AddHandler clsItem.MouseDown, AddressOf FileMenuMRUList_MouseDown
            ' insert into DropDownItems list...
            dropDownItems.Insert(dropDownItems.Count - 10, clsItem)
        Next

    End Sub

    <Extension>
    Friend Function IndexOf(contextMenu As ContextMenuStrip, text As String, Optional searchAllChildren As Boolean = False) As Integer
        Return contextMenu.Items.IndexOf(contextMenu.Items.Find(text, searchAllChildren)(0))
    End Function

    <Extension>
    Friend Sub MenuAddToMru(mruData As Specialized.StringCollection, text As String)
        ' remove the item from the collection if exists so that we can
        ' re-add it to the beginning...
        If mruData.Contains(text) Then
            mruData.Remove(text)
        End If
        ' add to MRU list..
        mruData.Add(text)
        ' make sure there are only ever 5 items...
        While mruData.Count > 5
            mruData.RemoveAt(0)
        End While
    End Sub

    <Extension>
    Friend Sub TsFindWhatMruUpdateUi(dropDownItems As ToolStripComboBox)
        If My.Settings.TSFindWhatMRU_Data Is Nothing Then
            My.Settings.TSFindWhatMRU_Data = New Specialized.StringCollection
            My.Settings.Save()
        End If
        ' clear MRU menu items...
        dropDownItems.Items.Clear()
        ' display items (in reverse order so the most recent is on top)...
        For iCounter As Integer = My.Settings.TSFindWhatMRU_Data.Count - 1 To 0 Step -1
            dropDownItems.Items.Add(My.Settings.TSFindWhatMRU_Data(iCounter))
        Next
    End Sub

    <Extension>
    Friend Sub UpdateLastFileMenu(mainForm As Form1)
        ' load MRU...
        If My.Settings.MRU_Data Is Nothing Then
            My.Settings.MRU_Data = New Specialized.StringCollection
        End If

        My.Settings.Save()

        ' show separator...
        If My.Settings.MRU_Data.Count > 0 Then
            mainForm.MenuFileLastFolder.Text = Path.GetDirectoryName(My.Settings.MRU_Data.Last)
            mainForm.MenuFileLastFolder.Visible = True
            mainForm.MenuFileSep1.Visible = True
            mainForm.MenuFileSep2.Visible = True
        Else
            mainForm.MenuFileLastFolder.Visible = False
            mainForm.MenuFileSep1.Visible = False
            mainForm.MenuFileSep2.Visible = False
        End If
    End Sub

End Module
