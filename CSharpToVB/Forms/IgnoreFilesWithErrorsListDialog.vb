' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Public Class IgnoreFilesWithErrorsListDialog
    Private ReadOnly _list_string As New List(Of String)()
    Private _fileToLoad As String = ""

    Public ReadOnly Property FileToLoad As String
        Get
            Return _fileToLoad
        End Get
    End Property

    Private Sub btlClearErrorFileList_Click_1(sender As Object, e As EventArgs) Handles btlClearErrorFileList.Click
        My.Settings.IgnoreFileList.Clear()
        Me.UpdateGrid()
    End Sub

    Private Sub Cancel_Button_Click(sender As Object, e As EventArgs) Handles Cancel_Button.Click
        Me.DialogResult = DialogResult.Cancel
        Me.Close()
    End Sub

    Private Sub dgvIgnoredFilesList_CellClick(sender As Object, e As DataGridViewCellEventArgs) Handles dgvIgnoredFilesList.CellClick
        ' Ignore clicks that are not on button cells.
        If e.RowIndex < 0 Then
            Exit Sub
        End If
        Select Case e.ColumnIndex
            Case Me.dgvIgnoredFilesList.Columns("Delete").Index
                My.Settings.IgnoreFileList.RemoveAt(e.RowIndex)
                Me.UpdateGrid()
            Case Me.dgvIgnoredFilesList.Columns("Load").Index
                _fileToLoad = My.Settings.IgnoreFileList(e.RowIndex)
                Me.OK_Button.PerformClick()
            Case Else
                Exit Sub
        End Select
    End Sub

    Private Sub IgnoreFilesWithErrorsListDialog_Load(sender As Object, e As EventArgs) Handles Me.Load
        Me.UpdateGrid()
        If Me.dgvIgnoredFilesList.Columns.Count > 1 Then
            Exit Sub
        End If
        ' Initialize the button column.
        _fileToLoad = ""
    End Sub

    Private Sub OK_Button_Click(sender As Object, e As EventArgs) Handles OK_Button.Click
        Me.DialogResult = DialogResult.OK
        My.Settings.Save()
        Me.Close()
    End Sub

    Private Sub UpdateGrid()
        _list_string.Clear()
        For Each s As String In My.Settings.IgnoreFileList
            _list_string.Add(s)
        Next
        Me.dgvIgnoredFilesList.DataSource = _list_string.Select(Function(x As String) New With {Key .Value = x}).ToList()
    End Sub

End Class
