<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class FindDialog
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()>
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
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
        Me.FindPreviousButton = New System.Windows.Forms.Button()
        Me.ClearHighlightsButton = New System.Windows.Forms.Button()
        Me.FindNextButton = New System.Windows.Forms.Button()
        Me.CloseButton = New System.Windows.Forms.Button()
        Me.FindWhatComboBox = New System.Windows.Forms.ComboBox()
        Me.LookInLabel = New System.Windows.Forms.Label()
        Me.LookInComboBox = New System.Windows.Forms.ComboBox()
        Me.MatchCaseCheckBox = New System.Windows.Forms.CheckBox()
        Me.MatchWholeWordCheckBox = New System.Windows.Forms.CheckBox()
        Me.TableLayoutPanel1.SuspendLayout()
        Me.SuspendLayout()
        '
        'TableLayoutPanel1
        '
        Me.TableLayoutPanel1.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
        Me.TableLayoutPanel1.AutoSize = True
        Me.TableLayoutPanel1.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink
        Me.TableLayoutPanel1.ColumnCount = 3
        Me.TableLayoutPanel1.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 33.0!))
        Me.TableLayoutPanel1.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 34.0!))
        Me.TableLayoutPanel1.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 33.0!))
        Me.TableLayoutPanel1.Controls.Add(Me.FindPreviousButton, 0, 0)
        Me.TableLayoutPanel1.Controls.Add(Me.ClearHighlightsButton, 1, 0)
        Me.TableLayoutPanel1.Controls.Add(Me.FindNextButton, 2, 0)
        Me.TableLayoutPanel1.Location = New System.Drawing.Point(12, 62)
        Me.TableLayoutPanel1.Name = "TableLayoutPanel1"
        Me.TableLayoutPanel1.RowCount = 1
        Me.TableLayoutPanel1.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 50.0!))
        Me.TableLayoutPanel1.Size = New System.Drawing.Size(359, 33)
        Me.TableLayoutPanel1.TabIndex = 6
        '
        'FindPreviousButton
        '
        Me.FindPreviousButton.Anchor = System.Windows.Forms.AnchorStyles.None
        Me.FindPreviousButton.Location = New System.Drawing.Point(10, 3)
        Me.FindPreviousButton.Name = "FindPreviousButton"
        Me.FindPreviousButton.Size = New System.Drawing.Size(98, 27)
        Me.FindPreviousButton.TabIndex = 1
        Me.FindPreviousButton.Text = "Find Previous"
        '
        'ClearHighlightsButton
        '
        Me.ClearHighlightsButton.Anchor = System.Windows.Forms.AnchorStyles.None
        Me.ClearHighlightsButton.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.ClearHighlightsButton.Location = New System.Drawing.Point(121, 3)
        Me.ClearHighlightsButton.Name = "ClearHighlightsButton"
        Me.ClearHighlightsButton.Size = New System.Drawing.Size(116, 27)
        Me.ClearHighlightsButton.TabIndex = 0
        Me.ClearHighlightsButton.Text = "Clear Highlights"
        '
        'FindNextButton
        '
        Me.FindNextButton.Anchor = System.Windows.Forms.AnchorStyles.None
        Me.FindNextButton.DialogResult = System.Windows.Forms.DialogResult.Cancel
        Me.FindNextButton.Location = New System.Drawing.Point(250, 3)
        Me.FindNextButton.Name = "FindNextButton"
        Me.FindNextButton.Size = New System.Drawing.Size(99, 27)
        Me.FindNextButton.TabIndex = 0
        Me.FindNextButton.Text = "Find Next"
        '
        'CloseButton
        '
        Me.CloseButton.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.CloseButton.Location = New System.Drawing.Point(410, 65)
        Me.CloseButton.Name = "CloseButton"
        Me.CloseButton.Size = New System.Drawing.Size(99, 27)
        Me.CloseButton.TabIndex = 5
        Me.CloseButton.Text = "Close"
        '
        'FindWhatComboBox
        '
        Me.FindWhatComboBox.FormattingEnabled = True
        Me.FindWhatComboBox.Location = New System.Drawing.Point(12, 6)
        Me.FindWhatComboBox.Name = "FindWhatComboBox"
        Me.FindWhatComboBox.Size = New System.Drawing.Size(497, 23)
        Me.FindWhatComboBox.TabIndex = 0
        '
        'LookInLabel
        '
        Me.LookInLabel.AutoSize = True
        Me.LookInLabel.Location = New System.Drawing.Point(237, 37)
        Me.LookInLabel.Name = "LookInLabel"
        Me.LookInLabel.Size = New System.Drawing.Size(46, 15)
        Me.LookInLabel.TabIndex = 3
        Me.LookInLabel.Text = "Look in"
        '
        'LookInComboBox
        '
        Me.LookInComboBox.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.LookInComboBox.FormattingEnabled = True
        Me.LookInComboBox.ItemHeight = 15
        Me.LookInComboBox.Items.AddRange(New Object() {"C# Source", "Visual Basic Result", "Both"})
        Me.LookInComboBox.Location = New System.Drawing.Point(299, 33)
        Me.LookInComboBox.Name = "LookInComboBox"
        Me.LookInComboBox.Size = New System.Drawing.Size(213, 23)
        Me.LookInComboBox.TabIndex = 4
        '
        'MatchCaseCheckBox
        '
        Me.MatchCaseCheckBox.AutoSize = True
        Me.MatchCaseCheckBox.Location = New System.Drawing.Point(12, 35)
        Me.MatchCaseCheckBox.Name = "MatchCaseCheckBox"
        Me.MatchCaseCheckBox.Size = New System.Drawing.Size(86, 19)
        Me.MatchCaseCheckBox.TabIndex = 1
        Me.MatchCaseCheckBox.Text = "Match case"
        Me.MatchCaseCheckBox.UseVisualStyleBackColor = True
        '
        'MatchWholeWordCheckBox
        '
        Me.MatchWholeWordCheckBox.AutoSize = True
        Me.MatchWholeWordCheckBox.Location = New System.Drawing.Point(105, 35)
        Me.MatchWholeWordCheckBox.Name = "MatchWholeWordCheckBox"
        Me.MatchWholeWordCheckBox.Size = New System.Drawing.Size(125, 19)
        Me.MatchWholeWordCheckBox.TabIndex = 1
        Me.MatchWholeWordCheckBox.Text = "Match whole word"
        Me.MatchWholeWordCheckBox.UseVisualStyleBackColor = True
        '
        'FindDialog
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(7.0!, 15.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(525, 101)
        Me.Controls.Add(Me.MatchCaseCheckBox)
        Me.Controls.Add(Me.MatchWholeWordCheckBox)
        Me.Controls.Add(Me.LookInComboBox)
        Me.Controls.Add(Me.LookInLabel)
        Me.Controls.Add(Me.FindWhatComboBox)
        Me.Controls.Add(Me.CloseButton)
        Me.Controls.Add(Me.TableLayoutPanel1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "FindDialog"
        Me.ShowIcon = False
        Me.ShowInTaskbar = False
        Me.SizeGripStyle = System.Windows.Forms.SizeGripStyle.Hide
        Me.StartPosition = System.Windows.Forms.FormStartPosition.Manual
        Me.Text = "Find"
        Me.TopMost = True
        Me.TableLayoutPanel1.ResumeLayout(False)
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents TableLayoutPanel1 As System.Windows.Forms.TableLayoutPanel
    Friend WithEvents CloseButton As System.Windows.Forms.Button
    Friend WithEvents ClearHighlightsButton As System.Windows.Forms.Button
    Friend WithEvents FindPreviousButton As System.Windows.Forms.Button
    Friend WithEvents FindNextButton As System.Windows.Forms.Button
    Friend WithEvents FindWhatComboBox As ComboBox
    Friend WithEvents LookInLabel As Label
    Friend WithEvents LookInComboBox As ComboBox
    Friend WithEvents MatchCaseCheckBox As CheckBox
    Friend WithEvents MatchWholeWordCheckBox As CheckBox
End Class
