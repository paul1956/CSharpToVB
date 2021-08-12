Imports Microsoft.VisualBasic
<CompilerServices.DesignerGenerated()>
Partial Class OptionsDialog
    Inherits Form

    'Form overrides dispose to clean up the component list.
    <Diagnostics.DebuggerNonUserCode()>
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
    <Diagnostics.DebuggerStepThrough()>
    Private Sub InitializeComponent()
        Me.TableLayoutPanel1 = New TableLayoutPanel()
        Me.OK_Button = New Button()
        Me.Cancel_Button = New Button()
        Me.ProjectDirectoryList = New ComboBox()
        Me.Label1 = New Label()
        Me.ColorDialog1 = New ColorDialog()
        Me.ItemColor_ComboBox = New ComboBox()
        Me.Label2 = New Label()
        Me.UpdateForeground_Button = New Button()
        Me.LabelHorizontalLine = New Label()
        Me.Label5 = New Label()
        Me.CheckBoxCompare = New CheckBox()
        Me.ComboBoxCompare = New ComboBox()
        Me.ComboBoxExplicit = New ComboBox()
        Me.CheckBoxExplicit = New CheckBox()
        Me.ComboBoxStrict = New ComboBox()
        Me.CheckBoxStrict = New CheckBox()
        Me.ComboBoxInfer = New ComboBox()
        Me.CheckBoxInfer = New CheckBox()
        Me.Label3 = New Label()
        Me.SelectEditorFontButton = New Button()
        Me.FontDialog1 = New FontDialog()
        Me.ModeTextBox = New TextBox()
        Me.SampleTextBox = New TextBox()
        Me.UpdateBackground_Button = New Button()
        Me.ResetThemeButton = New Button()
        Me.CheckBoxTopLevelStatements = New CheckBox()
        Me.TableLayoutPanel1.SuspendLayout()
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
        Me.TableLayoutPanel1.Location = New Point(303, 335)
        Me.TableLayoutPanel1.Name = "TableLayoutPanel1"
        Me.TableLayoutPanel1.RowCount = 1
        Me.TableLayoutPanel1.RowStyles.Add(New RowStyle(SizeType.Percent, 50.0!))
        Me.TableLayoutPanel1.Size = New Size(170, 33)
        Me.TableLayoutPanel1.TabIndex = 0
        '
        'OK_Button
        '
        Me.OK_Button.Anchor = AnchorStyles.None
        Me.OK_Button.Location = New Point(9, 5)
        Me.OK_Button.Name = "OK_Button"
        Me.OK_Button.Size = New Size(66, 23)
        Me.OK_Button.TabIndex = 0
        Me.OK_Button.Text = "OK"
        '
        'Cancel_Button
        '
        Me.Cancel_Button.Anchor = AnchorStyles.None
        Me.Cancel_Button.DialogResult = DialogResult.Cancel
        Me.Cancel_Button.Location = New Point(94, 5)
        Me.Cancel_Button.Name = "Cancel_Button"
        Me.Cancel_Button.Size = New Size(66, 23)
        Me.Cancel_Button.TabIndex = 1
        Me.Cancel_Button.Text = "Cancel"
        '
        'ProjectDirectoryList
        '
        Me.ProjectDirectoryList.FormattingEnabled = True
        Me.ProjectDirectoryList.Location = New Point(205, 17)
        Me.ProjectDirectoryList.Name = "ProjectDirectoryList"
        Me.ProjectDirectoryList.Size = New Size(268, 23)
        Me.ProjectDirectoryList.TabIndex = 1
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New Point(13, 21)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New Size(170, 15)
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
        Me.ItemColor_ComboBox.DrawMode = DrawMode.OwnerDrawFixed
        Me.ItemColor_ComboBox.DropDownHeight = 400
        Me.ItemColor_ComboBox.DropDownStyle = ComboBoxStyle.DropDownList
        Me.ItemColor_ComboBox.DropDownWidth = 300
        Me.ItemColor_ComboBox.FormattingEnabled = True
        Me.ItemColor_ComboBox.IntegralHeight = False
        Me.ItemColor_ComboBox.Location = New Point(10, 293)
        Me.ItemColor_ComboBox.MaxDropDownItems = 20
        Me.ItemColor_ComboBox.Name = "ItemColor_ComboBox"
        Me.ItemColor_ComboBox.Size = New Size(463, 24)
        Me.ItemColor_ComboBox.TabIndex = 3
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Location = New Point(189, 177)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New Size(109, 15)
        Me.Label2.TabIndex = 4
        Me.Label2.Text = "Theme Color Editor"
        '
        'UpdateForeground_Button
        '
        Me.UpdateForeground_Button.Location = New Point(148, 249)
        Me.UpdateForeground_Button.Name = "UpdateForeground_Button"
        Me.UpdateForeground_Button.Size = New Size(159, 27)
        Me.UpdateForeground_Button.TabIndex = 5
        Me.UpdateForeground_Button.Text = "Update Foreground Color"
        Me.UpdateForeground_Button.UseVisualStyleBackColor = True
        '
        'LabelHorizontalLine
        '
        Me.LabelHorizontalLine.Anchor = CType(((AnchorStyles.Top Or AnchorStyles.Left) _
            Or AnchorStyles.Right), AnchorStyles)
        Me.LabelHorizontalLine.BackColor = Color.Black
        Me.LabelHorizontalLine.BorderStyle = BorderStyle.FixedSingle
        Me.LabelHorizontalLine.Location = New Point(13, 50)
        Me.LabelHorizontalLine.Name = "LabelHorizontalLine"
        Me.LabelHorizontalLine.Size = New Size(460, 2)
        Me.LabelHorizontalLine.TabIndex = 8
        '
        'Label5
        '
        Me.Label5.Anchor = CType(((AnchorStyles.Top Or AnchorStyles.Left) _
            Or AnchorStyles.Right), AnchorStyles)
        Me.Label5.BackColor = Color.Black
        Me.Label5.BorderStyle = BorderStyle.FixedSingle
        Me.Label5.Location = New Point(13, 165)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New Size(460, 2)
        Me.Label5.TabIndex = 9
        '
        'CheckBoxCompare
        '
        Me.CheckBoxCompare.AutoSize = True
        Me.CheckBoxCompare.Checked = True
        Me.CheckBoxCompare.CheckState = CheckState.Checked
        Me.CheckBoxCompare.Location = New Point(13, 95)
        Me.CheckBoxCompare.Name = "CheckBoxCompare"
        Me.CheckBoxCompare.Size = New Size(75, 19)
        Me.CheckBoxCompare.TabIndex = 11
        Me.CheckBoxCompare.Text = "Compare"
        Me.CheckBoxCompare.UseVisualStyleBackColor = True
        '
        'ComboBoxCompare
        '
        Me.ComboBoxCompare.DropDownStyle = ComboBoxStyle.DropDownList
        Me.ComboBoxCompare.FormattingEnabled = True
        Me.ComboBoxCompare.Items.AddRange(New Object() {"Binary", "Text"})
        Me.ComboBoxCompare.Location = New Point(88, 93)
        Me.ComboBoxCompare.Name = "ComboBoxCompare"
        Me.ComboBoxCompare.Size = New Size(66, 23)
        Me.ComboBoxCompare.TabIndex = 12
        '
        'ComboBoxExplicit
        '
        Me.ComboBoxExplicit.DropDownStyle = ComboBoxStyle.DropDownList
        Me.ComboBoxExplicit.FormattingEnabled = True
        Me.ComboBoxExplicit.Items.AddRange(New Object() {"Off", "On"})
        Me.ComboBoxExplicit.Location = New Point(88, 129)
        Me.ComboBoxExplicit.Name = "ComboBoxExplicit"
        Me.ComboBoxExplicit.Size = New Size(66, 23)
        Me.ComboBoxExplicit.TabIndex = 14
        '
        'CheckBoxExplicit
        '
        Me.CheckBoxExplicit.AutoSize = True
        Me.CheckBoxExplicit.Checked = True
        Me.CheckBoxExplicit.CheckState = CheckState.Checked
        Me.CheckBoxExplicit.Location = New Point(13, 131)
        Me.CheckBoxExplicit.Name = "CheckBoxExplicit"
        Me.CheckBoxExplicit.Size = New Size(64, 19)
        Me.CheckBoxExplicit.TabIndex = 13
        Me.CheckBoxExplicit.Text = "Explicit"
        Me.CheckBoxExplicit.UseVisualStyleBackColor = True
        '
        'ComboBoxStrict
        '
        Me.ComboBoxStrict.DropDownStyle = ComboBoxStyle.DropDownList
        Me.ComboBoxStrict.FormattingEnabled = True
        Me.ComboBoxStrict.Items.AddRange(New Object() {"Off", "On"})
        Me.ComboBoxStrict.Location = New Point(235, 129)
        Me.ComboBoxStrict.Name = "ComboBoxStrict"
        Me.ComboBoxStrict.Size = New Size(66, 23)
        Me.ComboBoxStrict.TabIndex = 18
        '
        'CheckBoxStrict
        '
        Me.CheckBoxStrict.AutoSize = True
        Me.CheckBoxStrict.Checked = True
        Me.CheckBoxStrict.CheckState = CheckState.Checked
        Me.CheckBoxStrict.Location = New Point(182, 131)
        Me.CheckBoxStrict.Name = "CheckBoxStrict"
        Me.CheckBoxStrict.Size = New Size(53, 19)
        Me.CheckBoxStrict.TabIndex = 17
        Me.CheckBoxStrict.Text = "Strict"
        Me.CheckBoxStrict.UseVisualStyleBackColor = True
        '
        'ComboBoxInfer
        '
        Me.ComboBoxInfer.DropDownStyle = ComboBoxStyle.DropDownList
        Me.ComboBoxInfer.FormattingEnabled = True
        Me.ComboBoxInfer.Items.AddRange(New Object() {"Off", "On"})
        Me.ComboBoxInfer.Location = New Point(235, 93)
        Me.ComboBoxInfer.Name = "ComboBoxInfer"
        Me.ComboBoxInfer.Size = New Size(66, 23)
        Me.ComboBoxInfer.TabIndex = 16
        '
        'CheckBoxInfer
        '
        Me.CheckBoxInfer.AutoSize = True
        Me.CheckBoxInfer.Checked = True
        Me.CheckBoxInfer.CheckState = CheckState.Checked
        Me.CheckBoxInfer.Location = New Point(182, 95)
        Me.CheckBoxInfer.Name = "CheckBoxInfer"
        Me.CheckBoxInfer.Size = New Size(50, 19)
        Me.CheckBoxInfer.TabIndex = 15
        Me.CheckBoxInfer.Text = "Infer"
        Me.CheckBoxInfer.UseVisualStyleBackColor = True
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Location = New Point(29, 65)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New Size(428, 15)
        Me.Label3.TabIndex = 19
        Me.Label3.Text = "Checking any options below will cause them to be included in converted  result!"
        '
        'SelectEditorFontButton
        '
        Me.SelectEditorFontButton.Location = New Point(315, 209)
        Me.SelectEditorFontButton.Name = "SelectEditorFontButton"
        Me.SelectEditorFontButton.Size = New Size(158, 23)
        Me.SelectEditorFontButton.TabIndex = 20
        Me.SelectEditorFontButton.Text = "Select Editor Font"
        Me.SelectEditorFontButton.UseVisualStyleBackColor = True
        '
        'FontDialog1
        '
        '
        'ModeTextBox
        '
        Me.ModeTextBox.Location = New Point(193, 209)
        Me.ModeTextBox.Name = "ModeTextBox"
        Me.ModeTextBox.ReadOnly = True
        Me.ModeTextBox.Size = New Size(100, 23)
        Me.ModeTextBox.TabIndex = 21
        Me.ModeTextBox.TextAlign = HorizontalAlignment.Center
        '
        'SampleTextBox
        '
        Me.SampleTextBox.Location = New Point(13, 251)
        Me.SampleTextBox.Name = "SampleTextBox"
        Me.SampleTextBox.ReadOnly = True
        Me.SampleTextBox.Size = New Size(127, 23)
        Me.SampleTextBox.TabIndex = 22
        Me.SampleTextBox.Text = "Sample Color"
        Me.SampleTextBox.TextAlign = HorizontalAlignment.Center
        '
        'UpdateBackground_Button
        '
        Me.UpdateBackground_Button.Location = New Point(315, 249)
        Me.UpdateBackground_Button.Name = "UpdateBackground_Button"
        Me.UpdateBackground_Button.Size = New Size(158, 27)
        Me.UpdateBackground_Button.TabIndex = 24
        Me.UpdateBackground_Button.Text = "Update Background Color"
        Me.UpdateBackground_Button.UseVisualStyleBackColor = True
        '
        'ResetThemeButton
        '
        Me.ResetThemeButton.Location = New Point(13, 340)
        Me.ResetThemeButton.Name = "ResetThemeButton"
        Me.ResetThemeButton.Size = New Size(127, 23)
        Me.ResetThemeButton.TabIndex = 25
        Me.ResetThemeButton.Text = "Reset Theme"
        Me.ResetThemeButton.UseVisualStyleBackColor = True
        '
        'CheckBoxTopLevelStatements
        '
        Me.CheckBoxTopLevelStatements.AutoSize = True
        Me.CheckBoxTopLevelStatements.Checked = True
        Me.CheckBoxTopLevelStatements.CheckState = CheckState.Checked
        Me.CheckBoxTopLevelStatements.Location = New Point(336, 95)
        Me.CheckBoxTopLevelStatements.Name = "CheckBoxTopLevelStatements"
        Me.CheckBoxTopLevelStatements.Size = New Size(137, 19)
        Me.CheckBoxTopLevelStatements.TabIndex = 26
        Me.CheckBoxTopLevelStatements.Text = "Top Level Statements"
        Me.CheckBoxTopLevelStatements.UseVisualStyleBackColor = True
        '
        'OptionsDialog
        '
        Me.AcceptButton = Me.OK_Button
        Me.AutoScaleDimensions = New SizeF(7.0!, 15.0!)
        Me.AutoScaleMode = AutoScaleMode.Font
        Me.CancelButton = Me.Cancel_Button
        Me.ClientSize = New Size(486, 380)
        Me.Controls.Add(Me.CheckBoxTopLevelStatements)
        Me.Controls.Add(Me.ResetThemeButton)
        Me.Controls.Add(Me.UpdateBackground_Button)
        Me.Controls.Add(Me.SampleTextBox)
        Me.Controls.Add(Me.ModeTextBox)
        Me.Controls.Add(Me.SelectEditorFontButton)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.ComboBoxStrict)
        Me.Controls.Add(Me.CheckBoxStrict)
        Me.Controls.Add(Me.ComboBoxInfer)
        Me.Controls.Add(Me.CheckBoxInfer)
        Me.Controls.Add(Me.ComboBoxExplicit)
        Me.Controls.Add(Me.CheckBoxExplicit)
        Me.Controls.Add(Me.ComboBoxCompare)
        Me.Controls.Add(Me.CheckBoxCompare)
        Me.Controls.Add(Me.Label5)
        Me.Controls.Add(Me.LabelHorizontalLine)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.ProjectDirectoryList)
        Me.Controls.Add(Me.UpdateForeground_Button)
        Me.Controls.Add(Me.ItemColor_ComboBox)
        Me.Controls.Add(Me.TableLayoutPanel1)
        Me.Font = New Font("Segoe UI", 9.0!, FontStyle.Regular, GraphicsUnit.Point)
        Me.FormBorderStyle = FormBorderStyle.FixedDialog
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "OptionsDialog"
        Me.ShowInTaskbar = False
        Me.StartPosition = FormStartPosition.CenterParent
        Me.Text = "Advanced Options"
        Me.TableLayoutPanel1.ResumeLayout(False)
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents TableLayoutPanel1 As TableLayoutPanel
    Friend WithEvents OK_Button As Button
    Friend WithEvents Cancel_Button As Button
    Friend WithEvents ProjectDirectoryList As ComboBox
    Friend WithEvents Label1 As Label
    Friend WithEvents ColorDialog1 As ColorDialog
    Friend WithEvents ItemColor_ComboBox As ComboBox
    Friend WithEvents Label2 As Label
    Friend WithEvents UpdateForeground_Button As Button
    Friend WithEvents LabelHorizontalLine As Label
    Friend WithEvents Label5 As Label
    Friend WithEvents OptionsBindingSource As BindingSource
    Friend WithEvents CheckBoxCompare As CheckBox
    Friend WithEvents ComboBoxCompare As ComboBox
    Friend WithEvents ComboBoxExplicit As ComboBox
    Friend WithEvents CheckBoxExplicit As CheckBox
    Friend WithEvents ComboBoxStrict As ComboBox
    Friend WithEvents CheckBoxStrict As CheckBox
    Friend WithEvents CheckBoxTopLevelStatements As CheckBox
    Friend WithEvents ComboBoxInfer As ComboBox
    Friend WithEvents CheckBoxInfer As CheckBox
    Friend WithEvents Label3 As Label
    Friend WithEvents SelectEditorFontButton As Button
    Friend WithEvents FontDialog1 As FontDialog
    Friend WithEvents ModeTextBox As TextBox
    Friend WithEvents SampleTextBox As TextBox
    Friend WithEvents UpdateBackground_Button As Button
    Friend WithEvents ResetThemeButton As Button
End Class
