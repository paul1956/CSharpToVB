<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class IgnoreFilesWithErrorsListDialog
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()>
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
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()>
    Private Sub InitializeComponent()
        Me.TableLayoutPanel1 = New System.Windows.Forms.TableLayoutPanel()
        Me.OK_Button = New System.Windows.Forms.Button()
        Me.Cancel_Button = New System.Windows.Forms.Button()
        Me.dgvIgnoredFilesList = New System.Windows.Forms.DataGridView()
        Me.btlClearErrorFileList = New System.Windows.Forms.Button()
        Me.Delete = New System.Windows.Forms.DataGridViewButtonColumn()
        Me.LoadFile = New System.Windows.Forms.DataGridViewButtonColumn()
        Me.TableLayoutPanel1.SuspendLayout()
        CType(Me.dgvIgnoredFilesList, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'TableLayoutPanel1
        '
        Me.TableLayoutPanel1.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.TableLayoutPanel1.ColumnCount = 2
        Me.TableLayoutPanel1.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 50.0!))
        Me.TableLayoutPanel1.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 50.0!))
        Me.TableLayoutPanel1.Controls.Add(Me.OK_Button, 0, 0)
        Me.TableLayoutPanel1.Controls.Add(Me.Cancel_Button, 1, 0)
        Me.TableLayoutPanel1.Location = New System.Drawing.Point(1135, 316)
        Me.TableLayoutPanel1.Margin = New System.Windows.Forms.Padding(4, 3, 4, 3)
        Me.TableLayoutPanel1.Name = "TableLayoutPanel1"
        Me.TableLayoutPanel1.RowCount = 1
        Me.TableLayoutPanel1.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 50.0!))
        Me.TableLayoutPanel1.Size = New System.Drawing.Size(170, 33)
        Me.TableLayoutPanel1.TabIndex = 0
        '
        'OK_Button
        '
        Me.OK_Button.Anchor = System.Windows.Forms.AnchorStyles.None
        Me.OK_Button.Location = New System.Drawing.Point(4, 3)
        Me.OK_Button.Margin = New System.Windows.Forms.Padding(4, 3, 4, 3)
        Me.OK_Button.Name = "OK_Button"
        Me.OK_Button.Size = New System.Drawing.Size(77, 27)
        Me.OK_Button.TabIndex = 0
        Me.OK_Button.Text = "OK"
        '
        'Cancel_Button
        '
        Me.Cancel_Button.Anchor = System.Windows.Forms.AnchorStyles.None
        Me.Cancel_Button.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.Cancel_Button.Location = New System.Drawing.Point(89, 3)
        Me.Cancel_Button.Margin = New System.Windows.Forms.Padding(4, 3, 4, 3)
        Me.Cancel_Button.Name = "Cancel_Button"
        Me.Cancel_Button.Size = New System.Drawing.Size(77, 27)
        Me.Cancel_Button.TabIndex = 1
        Me.Cancel_Button.Text = "Cancel"
        '
        'dgvIgnoredFilesList
        '
        Me.dgvIgnoredFilesList.AllowUserToAddRows = False
        Me.dgvIgnoredFilesList.AllowUserToResizeRows = False
        Me.dgvIgnoredFilesList.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.dgvIgnoredFilesList.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.AllCells
        Me.dgvIgnoredFilesList.AutoSizeRowsMode = System.Windows.Forms.DataGridViewAutoSizeRowsMode.DisplayedCells
        Me.dgvIgnoredFilesList.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.dgvIgnoredFilesList.ColumnHeadersVisible = False
        Me.dgvIgnoredFilesList.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.Delete, Me.LoadFile})
        Me.dgvIgnoredFilesList.EditMode = System.Windows.Forms.DataGridViewEditMode.EditProgrammatically
        Me.dgvIgnoredFilesList.Location = New System.Drawing.Point(0, 0)
        Me.dgvIgnoredFilesList.Margin = New System.Windows.Forms.Padding(4, 3, 4, 3)
        Me.dgvIgnoredFilesList.MultiSelect = False
        Me.dgvIgnoredFilesList.Name = "dgvIgnoredFilesList"
        Me.dgvIgnoredFilesList.ReadOnly = True
        Me.dgvIgnoredFilesList.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.FullRowSelect
        Me.dgvIgnoredFilesList.Size = New System.Drawing.Size(1320, 309)
        Me.dgvIgnoredFilesList.TabIndex = 1
        '
        'btlClearErrorFileList
        '
        Me.btlClearErrorFileList.Location = New System.Drawing.Point(94, 323)
        Me.btlClearErrorFileList.Margin = New System.Windows.Forms.Padding(4, 3, 4, 3)
        Me.btlClearErrorFileList.Name = "btlClearErrorFileList"
        Me.btlClearErrorFileList.Size = New System.Drawing.Size(150, 27)
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
        Me.AutoScaleDimensions = New System.Drawing.SizeF(7.0!, 15.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.CancelButton = Me.Cancel_Button
        Me.ClientSize = New System.Drawing.Size(1320, 363)
        Me.Controls.Add(Me.btlClearErrorFileList)
        Me.Controls.Add(Me.dgvIgnoredFilesList)
        Me.Controls.Add(Me.TableLayoutPanel1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog
        Me.Margin = New System.Windows.Forms.Padding(4, 3, 4, 3)
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "IgnoreFilesWithErrorsList"
        Me.ShowInTaskbar = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent
        Me.Text = "List of files With errors to ignore during automated conversion"
        Me.TableLayoutPanel1.ResumeLayout(False)
        CType(Me.dgvIgnoredFilesList, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents TableLayoutPanel1 As System.Windows.Forms.TableLayoutPanel
    Friend WithEvents OK_Button As System.Windows.Forms.Button
    Friend WithEvents Cancel_Button As System.Windows.Forms.Button
    Friend WithEvents dgvIgnoredFilesList As DataGridView
    Friend WithEvents btlClearErrorFileList As Button
    Friend WithEvents Delete As DataGridViewButtonColumn
    Friend WithEvents LoadFile As DataGridViewButtonColumn
End Class
