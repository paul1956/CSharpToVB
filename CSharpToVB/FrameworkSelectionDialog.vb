' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Public Class FrameworkSelectionDialog
    Private _frameworkList As New List(Of String)
    Public ReadOnly Property CurrentFramework As String

    Private Sub Cancel_Button_Click(ByVal sender As Object, ByVal e As EventArgs) Handles Cancel_Button.Click
        DialogResult = DialogResult.Cancel
        Close()
    End Sub

    Private Sub CheckedListBox1_Click(sender As Object, e As EventArgs) Handles CheckedListBox1.Click
        Dim chkListBox As CheckedListBox = CType(sender, CheckedListBox)
        Dim CheckedItems As CheckedListBox.CheckedIndexCollection = chkListBox.CheckedIndices
        Dim selectedIndex As Integer = chkListBox.SelectedIndex
        If CheckedItems.Count > 0 Then
            chkListBox.SetItemChecked(CheckedItems(0), False)
        End If
        chkListBox.SetItemChecked(selectedIndex, True)
    End Sub

    Private Sub FrameworkSelectionDialog_Load(sender As Object, e As EventArgs) Handles Me.Load
        Debug.Assert(_frameworkList IsNot Nothing)
        Dim items As CheckedListBox.ObjectCollection = CheckedListBox1.Items
        For Each framework As String In _frameworkList
            items.Add(framework, False)
        Next
    End Sub

    Private Sub OK_Button_Click(ByVal sender As Object, ByVal e As EventArgs) Handles OK_Button.Click
        Dim CheckedItems As CheckedListBox.CheckedItemCollection = CheckedListBox1.CheckedItems
        If CheckedItems.Count = 1 Then
            'Return OK to the calling form
            _CurrentFramework = CheckedItems(0).ToString
            DialogResult = DialogResult.OK
            Close()
        Else
            'Show an error message, but keep the form open
            MessageBox.Show("You must select one framework.")
        End If
    End Sub

    Public Sub SetFrameworkList(Value As List(Of String))
        _frameworkList = Value
    End Sub
End Class
