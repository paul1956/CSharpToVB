Public Class IgnoreFilesWithErrorsList
    Dim _FileToLoad = ""
    Private ReadOnly list_string As IList(Of String) = New List(Of String)()

    Public ReadOnly Property FileToLoad As String
        Get
            Return Me._FileToLoad
        End Get
    End Property

    Private Sub btlClearErrorFileList_Click_1(sender As Object, e As EventArgs) Handles btlClearErrorFileList.Click
        My.Settings.IgnoreFileList.Clear()
        Me.UpdateGrid()
    End Sub

    Private Sub Cancel_Button_Click(ByVal sender As Object, ByVal e As EventArgs) Handles Cancel_Button.Click
        Me.DialogResult = DialogResult.Cancel
        Me.Close()
    End Sub

    Private Sub dgvIgnoredFilesList_CellClick(sender As Object, e As DataGridViewCellEventArgs) Handles dgvIgnoredFilesList.CellClick
        ' Ignore clicks that are not on button cells.
        If e.RowIndex < 0 Then Return
        Select Case e.ColumnIndex
            Case Me.dgvIgnoredFilesList.Columns("Delete").Index
                My.Settings.IgnoreFileList.RemoveAt(e.RowIndex)
                Me.UpdateGrid()
            Case Me.dgvIgnoredFilesList.Columns("Load").Index
                Me._FileToLoad = My.Settings.IgnoreFileList(e.RowIndex)
            Case Else
                Return
        End Select
    End Sub

    Private Sub IgnoreFilesWithErrorsListDialog_Load(sender As Object, e As EventArgs) Handles Me.Load
        Me.UpdateGrid()
        If Me.dgvIgnoredFilesList.Columns.Count > 1 Then
            Exit Sub
        End If
        ' Initialize the button column.
        Me._FileToLoad = ""
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
        Me.dgvIgnoredFilesList.Columns.Insert(0, buttonDeleteEntry)

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
        Me.dgvIgnoredFilesList.Columns.Insert(1, buttonLoadFile)

    End Sub

    Private Sub OK_Button_Click(ByVal sender As Object, ByVal e As EventArgs) Handles OK_Button.Click
        Me.DialogResult = DialogResult.OK
        My.Settings.Save()
        Me.Close()
    End Sub

    Private Sub UpdateGrid()
        Me.list_string.Clear()
        For Each s As String In My.Settings.IgnoreFileList
            Me.list_string.Add(s)
        Next
        Me.dgvIgnoredFilesList.DataSource = Me.list_string.Select(Function(x As String) New With {Key .Value = x}).ToList()
    End Sub

End Class