<CompilerServices.DesignerGenerated()>
Partial Class IgnoreFilesWithErrorsListDialog
    Inherits Form

    'Form overrides dispose to clean up the component list.
    <DebuggerNonUserCode()>
    Protected Overrides Sub Dispose(disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Required by the Windows Form Designer
    Private components As ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <DebuggerStepThrough()>
    Private Sub InitializeComponent()
        Me.TableLayoutPanel1 = New TableLayoutPanel()
        Me.OK_Button = New Button()
        Me.Cancel_Button = New Button()
        Me.dgvIgnoredFilesList = New DataGridView()
        Me.btlClearErrorFileList = New Button()
        Me.Delete = New DataGridViewButtonColumn()
        Me.LoadFile = New DataGridViewButtonColumn()
        Me.TableLayoutPanel1.SuspendLayout()
        CType(Me.dgvIgnoredFilesList, ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'TableLayoutPanel1
        '
        Me.TableLayoutPanel1.Anchor = CType((AnchorStyles.Bottom Or AnchorStyles.Right), AnchorStyles)
        Me.TableLayoutPanel1.ColumnCount = 2
        Me.TableLayoutPanel1.ColumnStyles.Add(New ColumnStyle(SizeType.Percent, 50.0!))
        Me.TableLayoutPanel1.ColumnStyles.Add(New ColumnStyle(SizeType.Percent, 50.0!))
        Me.TableLayoutPanel1.Controls.Add(Me.OK_Button, 0, 0)
        Me.TableLayoutPanel1.Controls.Add(Me.Cancel_Button, 1, 0)
        Me.TableLayoutPanel1.Location = New Point(1135, 316)
        Me.TableLayoutPanel1.Margin = New Padding(4, 3, 4, 3)
        Me.TableLayoutPanel1.Name = "TableLayoutPanel1"
        Me.TableLayoutPanel1.RowCount = 1
        Me.TableLayoutPanel1.RowStyles.Add(New RowStyle(SizeType.Percent, 50.0!))
        Me.TableLayoutPanel1.Size = New Size(170, 33)
        Me.TableLayoutPanel1.TabIndex = 0
        '
        'OK_Button
        '
        Me.OK_Button.Anchor = AnchorStyles.None
        Me.OK_Button.Location = New Point(4, 3)
        Me.OK_Button.Margin = New Padding(4, 3, 4, 3)
        Me.OK_Button.Name = "OK_Button"
        Me.OK_Button.Size = New Size(77, 27)
        Me.OK_Button.TabIndex = 0
        Me.OK_Button.Text = "OK"
        '
        'Cancel_Button
        '
        Me.Cancel_Button.Anchor = AnchorStyles.None
        Me.Cancel_Button.DialogResult = DialogResult.Cancel
        Me.Cancel_Button.Location = New Point(89, 3)
        Me.Cancel_Button.Margin = New Padding(4, 3, 4, 3)
        Me.Cancel_Button.Name = "Cancel_Button"
        Me.Cancel_Button.Size = New Size(77, 27)
        Me.Cancel_Button.TabIndex = 1
        Me.Cancel_Button.Text = "Cancel"
        '
        'dgvIgnoredFilesList
        '
        Me.dgvIgnoredFilesList.AllowUserToAddRows = False
        Me.dgvIgnoredFilesList.AllowUserToResizeRows = False
        Me.dgvIgnoredFilesList.Anchor = CType(((AnchorStyles.Top Or AnchorStyles.Left) _
            Or AnchorStyles.Right), AnchorStyles)
        Me.dgvIgnoredFilesList.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.AllCells
        Me.dgvIgnoredFilesList.AutoSizeRowsMode = DataGridViewAutoSizeRowsMode.DisplayedCells
        Me.dgvIgnoredFilesList.ColumnHeadersHeightSizeMode = DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.dgvIgnoredFilesList.ColumnHeadersVisible = False
        Me.dgvIgnoredFilesList.Columns.AddRange(New DataGridViewColumn() {Me.Delete, Me.LoadFile})
        Me.dgvIgnoredFilesList.EditMode = DataGridViewEditMode.EditProgrammatically
        Me.dgvIgnoredFilesList.Location = New Point(0, 0)
        Me.dgvIgnoredFilesList.Margin = New Padding(4, 3, 4, 3)
        Me.dgvIgnoredFilesList.MultiSelect = False
        Me.dgvIgnoredFilesList.Name = "dgvIgnoredFilesList"
        Me.dgvIgnoredFilesList.ReadOnly = True
        Me.dgvIgnoredFilesList.SelectionMode = DataGridViewSelectionMode.FullRowSelect
        Me.dgvIgnoredFilesList.Size = New Size(1320, 309)
        Me.dgvIgnoredFilesList.TabIndex = 1
        '
        'btlClearErrorFileList
        '
        Me.btlClearErrorFileList.Location = New Point(94, 323)
        Me.btlClearErrorFileList.Margin = New Padding(4, 3, 4, 3)
        Me.btlClearErrorFileList.Name = "btlClearErrorFileList"
        Me.btlClearErrorFileList.Size = New Size(150, 27)
        Me.btlClearErrorFileList.TabIndex = 7
        Me.btlClearErrorFileList.Text = "Clear Error File List"
        Me.btlClearErrorFileList.UseVisualStyleBackColor = True
        '
        'Delete
        '
        Me.Delete.HeaderText = "Delete"
        Me.Delete.Name = "Delete"
        Me.Delete.Text = "Delete Entry"
        Me.Delete.ReadOnly = True
        Me.Delete.UseColumnTextForButtonValue = True
        '
        'Load
        '
        Me.LoadFile.HeaderText = "Load File"
        Me.LoadFile.Name = "Load"
        Me.LoadFile.ReadOnly = True
        Me.LoadFile.Text = "Load File"
        Me.LoadFile.UseColumnTextForButtonValue = True
        '
        'IgnoreFilesWithErrorsList
        '
        Me.AcceptButton = Me.OK_Button
        Me.AutoScaleDimensions = New SizeF(7.0!, 15.0!)
        Me.AutoScaleMode = AutoScaleMode.Font
        Me.CancelButton = Me.Cancel_Button
        Me.ClientSize = New Size(1320, 363)
        Me.Controls.Add(Me.btlClearErrorFileList)
        Me.Controls.Add(Me.dgvIgnoredFilesList)
        Me.Controls.Add(Me.TableLayoutPanel1)
        Me.FormBorderStyle = FormBorderStyle.FixedDialog
        Me.Margin = New Padding(4, 3, 4, 3)
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "IgnoreFilesWithErrorsList"
        Me.ShowInTaskbar = False
        Me.StartPosition = FormStartPosition.CenterParent
        Me.Text = "List of files With errors to ignore during automated conversion"
        Me.TableLayoutPanel1.ResumeLayout(False)
        CType(Me.dgvIgnoredFilesList, ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents TableLayoutPanel1 As TableLayoutPanel
    Friend WithEvents OK_Button As Button
    Friend WithEvents Cancel_Button As Button
    Friend WithEvents dgvIgnoredFilesList As DataGridView
    Friend WithEvents btlClearErrorFileList As Button
    Friend WithEvents Delete As DataGridViewButtonColumn
    Friend WithEvents LoadFile As DataGridViewButtonColumn
End Class
