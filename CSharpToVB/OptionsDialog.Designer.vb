<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class OptionsDialog
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
        Me.ProjectDirectoryList = New System.Windows.Forms.ComboBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.ColorDialog1 = New System.Windows.Forms.ColorDialog()
        Me.ItemColor_ComboBox = New System.Windows.Forms.ComboBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.UpdateColor_Button = New System.Windows.Forms.Button()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.CSharpHeaderTextBox = New System.Windows.Forms.TextBox()
        Me.CSharpFooterTextBox = New System.Windows.Forms.TextBox()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.LabelHorizontalLine = New System.Windows.Forms.Label()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.TableLayoutPanel1.SuspendLayout()
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
        Me.TableLayoutPanel1.Location = New System.Drawing.Point(363, 455)
        Me.TableLayoutPanel1.Name = "TableLayoutPanel1"
        Me.TableLayoutPanel1.RowCount = 1
        Me.TableLayoutPanel1.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 50.0!))
        Me.TableLayoutPanel1.Size = New System.Drawing.Size(170, 33)
        Me.TableLayoutPanel1.TabIndex = 0
        '
        'OK_Button
        '
        Me.OK_Button.Anchor = System.Windows.Forms.AnchorStyles.None
        Me.OK_Button.Location = New System.Drawing.Point(9, 5)
        Me.OK_Button.Name = "OK_Button"
        Me.OK_Button.Size = New System.Drawing.Size(66, 23)
        Me.OK_Button.TabIndex = 0
        Me.OK_Button.Text = "OK"
        '
        'Cancel_Button
        '
        Me.Cancel_Button.Anchor = System.Windows.Forms.AnchorStyles.None
        Me.Cancel_Button.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.Cancel_Button.Location = New System.Drawing.Point(94, 5)
        Me.Cancel_Button.Name = "Cancel_Button"
        Me.Cancel_Button.Size = New System.Drawing.Size(66, 23)
        Me.Cancel_Button.TabIndex = 1
        Me.Cancel_Button.Text = "Cancel"
        '
        'ProjectDirectoryList
        '
        Me.ProjectDirectoryList.FormattingEnabled = True
        Me.ProjectDirectoryList.Location = New System.Drawing.Point(205, 17)
        Me.ProjectDirectoryList.Name = "ProjectDirectoryList"
        Me.ProjectDirectoryList.Size = New System.Drawing.Size(328, 23)
        Me.ProjectDirectoryList.TabIndex = 1
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(13, 21)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(170, 15)
        Me.Label1.TabIndex = 2
        Me.Label1.Text = "Directory to Start Folder Search"
        '
        'ColorDialog1
        '
        Me.ColorDialog1.AnyColor = True
        Me.ColorDialog1.FullOpen = True
        '
        'ItemColor_ComboBox
        '
        '
        Me.ItemColor_ComboBox.DrawMode = System.Windows.Forms.DrawMode.OwnerDrawFixed
        Me.ItemColor_ComboBox.DropDownHeight = 400
        Me.ItemColor_ComboBox.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.ItemColor_ComboBox.DropDownWidth = 300
        Me.ItemColor_ComboBox.FormattingEnabled = True
        Me.ItemColor_ComboBox.IntegralHeight = False
        Me.ItemColor_ComboBox.Location = New System.Drawing.Point(13, 95)
        Me.ItemColor_ComboBox.MaxDropDownItems = 20
        Me.ItemColor_ComboBox.Name = "ItemColor_ComboBox"
        Me.ItemColor_ComboBox.Size = New System.Drawing.Size(520, 24)
        Me.ItemColor_ComboBox.TabIndex = 3
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(13, 73)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(104, 15)
        Me.Label2.TabIndex = 4
        Me.Label2.Text = "Display Item Color"
        '
        'UpdateColor_Button
        '
        Me.UpdateColor_Button.Location = New System.Drawing.Point(395, 61)
        Me.UpdateColor_Button.Name = "UpdateColor_Button"
        Me.UpdateColor_Button.Size = New System.Drawing.Size(138, 27)
        Me.UpdateColor_Button.TabIndex = 5
        Me.UpdateColor_Button.Text = "Update Color"
        Me.UpdateColor_Button.UseVisualStyleBackColor = True
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Location = New System.Drawing.Point(13, 139)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(114, 15)
        Me.Label3.TabIndex = 6
        Me.Label3.Text = "C# Template Header"
        '
        'CSharpHeaderTextBox
        '
        Me.CSharpHeaderTextBox.Location = New System.Drawing.Point(13, 157)
        Me.CSharpHeaderTextBox.Multiline = True
        Me.CSharpHeaderTextBox.Name = "CSharpHeaderTextBox"
        Me.CSharpHeaderTextBox.Size = New System.Drawing.Size(520, 183)
        Me.CSharpHeaderTextBox.TabIndex = 7
        '
        'CSharpFooterTextBox
        '
        Me.CSharpFooterTextBox.Location = New System.Drawing.Point(13, 360)
        Me.CSharpFooterTextBox.Multiline = True
        Me.CSharpFooterTextBox.Name = "CSharpFooterTextBox"
        Me.CSharpFooterTextBox.Size = New System.Drawing.Size(520, 67)
        Me.CSharpFooterTextBox.TabIndex = 7
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.Location = New System.Drawing.Point(13, 228)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(110, 15)
        Me.Label4.TabIndex = 6
        Me.Label4.Text = "C# Template Footer"
        Me.Label4.Text = "C# Template Footer"
        '
        'LabelHorizontalLine
        '
        Me.LabelHorizontalLine.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.LabelHorizontalLine.BackColor = System.Drawing.Color.Black
        Me.LabelHorizontalLine.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.LabelHorizontalLine.Location = New System.Drawing.Point(13, 50)
        Me.LabelHorizontalLine.Name = "LabelHorizontalLine"
        Me.LabelHorizontalLine.Size = New System.Drawing.Size(520, 2)
        Me.LabelHorizontalLine.TabIndex = 8
        '
        'Label5
        '
        Me.Label5.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label5.BackColor = System.Drawing.Color.Black
        Me.Label5.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.Label5.Location = New System.Drawing.Point(13, 132)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(520, 2)
        Me.Label5.TabIndex = 9
        '
        'OptionsDialog
        '
        Me.AcceptButton = Me.OK_Button
        Me.AutoScaleDimensions = New System.Drawing.SizeF(7.0!, 15.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.CancelButton = Me.Cancel_Button
        Me.ClientSize = New System.Drawing.Size(546, 502)
        Me.Controls.Add(Me.Label5)
        Me.Controls.Add(Me.LabelHorizontalLine)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.ProjectDirectoryList)
        Me.Controls.Add(Me.CSharpHeaderTextBox)
        Me.Controls.Add(Me.UpdateColor_Button)
        Me.Controls.Add(Me.ItemColor_ComboBox)
        Me.Controls.Add(Me.TableLayoutPanel1)
        Me.Controls.Add(Me.CSharpFooterTextBox)
        Me.Controls.Add(Me.Label4)
        Me.Font = New System.Drawing.Font("Segoe UI", 9.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "OptionsDialog"
        Me.ShowInTaskbar = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent
        Me.Text = "Options"
        Me.TableLayoutPanel1.ResumeLayout(False)
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents TableLayoutPanel1 As System.Windows.Forms.TableLayoutPanel
    Friend WithEvents OK_Button As System.Windows.Forms.Button
    Friend WithEvents Cancel_Button As System.Windows.Forms.Button
    Friend WithEvents ProjectDirectoryList As ComboBox
    Friend WithEvents Label1 As Label
    Friend WithEvents ColorDialog1 As ColorDialog
    Friend WithEvents ItemColor_ComboBox As ComboBox
    Friend WithEvents Label2 As Label
    Friend WithEvents UpdateColor_Button As Button
    Friend WithEvents Label3 As Label
    Friend WithEvents CSharpHeaderTextBox As TextBox
    Friend WithEvents CSharpFooterTextBox As TextBox
    Friend WithEvents Label4 As Label
    Friend WithEvents LabelHorizontalLine As Label
    Friend WithEvents Label5 As Label
End Class
