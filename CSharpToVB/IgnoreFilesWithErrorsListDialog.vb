' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Public Class IgnoreFilesWithErrorsList
    Private ReadOnly _list_string As IList(Of String) = New List(Of String)()
    Private _fileToLoad As String = ""

    Public ReadOnly Property FileToLoad As String
        Get
            Return _fileToLoad
        End Get
    End Property

    Private Sub btlClearErrorFileList_Click_1(sender As Object, e As EventArgs) Handles btlClearErrorFileList.Click
        My.Settings.IgnoreFileList.Clear()
        UpdateGrid()
    End Sub

    Private Sub Cancel_Button_Click(sender As Object, e As EventArgs) Handles Cancel_Button.Click
        DialogResult = DialogResult.Cancel
        Close()
    End Sub

    Private Sub dgvIgnoredFilesList_CellClick(sender As Object, e As DataGridViewCellEventArgs) Handles dgvIgnoredFilesList.CellClick
        ' Ignore clicks that are not on button cells.
        If e.RowIndex < 0 Then Return
        Select Case e.ColumnIndex
            Case dgvIgnoredFilesList.Columns("Delete").Index
                My.Settings.IgnoreFileList.RemoveAt(e.RowIndex)
                UpdateGrid()
            Case dgvIgnoredFilesList.Columns("Load").Index
                _fileToLoad = My.Settings.IgnoreFileList(e.RowIndex)
            Case Else
                Return
        End Select
    End Sub

    Private Sub IgnoreFilesWithErrorsListDialog_Load(sender As Object, e As EventArgs) Handles Me.Load
        UpdateGrid()
        If dgvIgnoredFilesList.Columns.Count > 1 Then
            Exit Sub
        End If
        ' Initialize the button column.
        _fileToLoad = ""
#Disable Warning IDE0067 ' Dispose objects before losing scope
        Dim buttonDeleteEntry As New DataGridViewButtonColumn
        With buttonDeleteEntry
            .HeaderText = "Delete"
            .Name = "Delete"
            .Text = "Delete Entry"
            ' Use the Text property for the button text for all cells rather
            ' than using each cell's value as the text for its own button.
            .UseColumnTextForButtonValue = True
        End With
        ' Add the button column to the control.
        dgvIgnoredFilesList.Columns.Insert(0, buttonDeleteEntry)

        Dim buttonLoadFile As New DataGridViewButtonColumn
        With buttonLoadFile
            .HeaderText = "Load File"
            .Name = "Load"
            .Text = "Load File"
            ' Use the Text property for the button text for all cells rather
            ' than using each cell's value as the text for its own button.
            .UseColumnTextForButtonValue = True
        End With
        ' Add the button column to the control.
        dgvIgnoredFilesList.Columns.Insert(1, buttonLoadFile)
#Enable Warning IDE0067 ' Dispose objects before losing scope
    End Sub

    Private Sub OK_Button_Click(sender As Object, e As EventArgs) Handles OK_Button.Click
        DialogResult = DialogResult.OK
        My.Settings.Save()
        Close()
    End Sub

    Private Sub UpdateGrid()
        _list_string.Clear()
        For Each s As String In My.Settings.IgnoreFileList
            _list_string.Add(s)
        Next
        dgvIgnoredFilesList.DataSource = _list_string.Select(Function(x As String) New With {Key .Value = x}).ToList()
    End Sub

End Class
