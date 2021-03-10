<CompilerServices.DesignerGenerated()> _
Partial Class AboutBox1
    Inherits Form

    'Form overrides dispose to clean up the component list.
    <DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    Friend WithEvents TableLayoutPanel As TableLayoutPanel
    Friend WithEvents LogoPictureBox As PictureBox
    Friend WithEvents LabelProductName As Label
    Friend WithEvents LabelVersion As Label
    Friend WithEvents LabelCompanyName As Label
    Friend WithEvents TextBoxDescription As TextBox
    Friend WithEvents OKButton As Button
    Friend WithEvents LabelCopyright As Label

    'Required by the Windows Form Designer
    Private components As ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Dim resources As ComponentModel.ComponentResourceManager = New ComponentModel.ComponentResourceManager(GetType(AboutBox1))
        Me.TableLayoutPanel = New TableLayoutPanel()
        Me.LogoPictureBox = New PictureBox()
        Me.LabelProductName = New Label()
        Me.LabelVersion = New Label()
        Me.LabelCopyright = New Label()
        Me.LabelCompanyName = New Label()
        Me.TextBoxDescription = New TextBox()
        Me.OKButton = New Button()
        Me.TableLayoutPanel.SuspendLayout()
        CType(Me.LogoPictureBox, ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'TableLayoutPanel
        '
        Me.TableLayoutPanel.ColumnCount = 2
        Me.TableLayoutPanel.ColumnStyles.Add(New ColumnStyle(SizeType.Percent, 33.0!))
        Me.TableLayoutPanel.ColumnStyles.Add(New ColumnStyle(SizeType.Percent, 67.0!))
        Me.TableLayoutPanel.Controls.Add(Me.LogoPictureBox, 0, 0)
        Me.TableLayoutPanel.Controls.Add(Me.LabelProductName, 1, 0)
        Me.TableLayoutPanel.Controls.Add(Me.LabelVersion, 1, 1)
        Me.TableLayoutPanel.Controls.Add(Me.LabelCopyright, 1, 2)
        Me.TableLayoutPanel.Controls.Add(Me.LabelCompanyName, 1, 3)
        Me.TableLayoutPanel.Controls.Add(Me.TextBoxDescription, 1, 4)
        Me.TableLayoutPanel.Controls.Add(Me.OKButton, 1, 5)
        Me.TableLayoutPanel.Dock = DockStyle.Fill
        Me.TableLayoutPanel.Location = New Point(10, 10)
        Me.TableLayoutPanel.Margin = New Padding(4, 3, 4, 3)
        Me.TableLayoutPanel.Name = "TableLayoutPanel"
        Me.TableLayoutPanel.RowCount = 6
        Me.TableLayoutPanel.RowStyles.Add(New RowStyle(SizeType.Percent, 10.0!))
        Me.TableLayoutPanel.RowStyles.Add(New RowStyle(SizeType.Percent, 10.0!))
        Me.TableLayoutPanel.RowStyles.Add(New RowStyle(SizeType.Percent, 10.0!))
        Me.TableLayoutPanel.RowStyles.Add(New RowStyle(SizeType.Percent, 10.0!))
        Me.TableLayoutPanel.RowStyles.Add(New RowStyle(SizeType.Percent, 50.0!))
        Me.TableLayoutPanel.RowStyles.Add(New RowStyle(SizeType.Percent, 10.0!))
        Me.TableLayoutPanel.Size = New Size(463, 298)
        Me.TableLayoutPanel.TabIndex = 0
        '
        'LogoPictureBox
        '
        Me.LogoPictureBox.Dock = DockStyle.Fill
        Me.LogoPictureBox.Image = CType(resources.GetObject("LogoPictureBox.Image"), Image)
        Me.LogoPictureBox.InitialImage = Nothing
        Me.LogoPictureBox.Location = New Point(4, 3)
        Me.LogoPictureBox.Margin = New Padding(4, 3, 4, 3)
        Me.LogoPictureBox.Name = "LogoPictureBox"
        Me.TableLayoutPanel.SetRowSpan(Me.LogoPictureBox, 6)
        Me.LogoPictureBox.Size = New Size(144, 292)
        Me.LogoPictureBox.SizeMode = PictureBoxSizeMode.StretchImage
        Me.LogoPictureBox.TabIndex = 0
        Me.LogoPictureBox.TabStop = False
        '
        'LabelProductName
        '
        Me.LabelProductName.Dock = DockStyle.Fill
        Me.LabelProductName.Location = New Point(159, 0)
        Me.LabelProductName.Margin = New Padding(7, 0, 4, 0)
        Me.LabelProductName.MaximumSize = New Size(0, 20)
        Me.LabelProductName.Name = "LabelProductName"
        Me.LabelProductName.Size = New Size(300, 20)
        Me.LabelProductName.TabIndex = 0
        Me.LabelProductName.Text = "Product Name"
        Me.LabelProductName.TextAlign = ContentAlignment.MiddleLeft
        '
        'LabelVersion
        '
        Me.LabelVersion.Dock = DockStyle.Fill
        Me.LabelVersion.Location = New Point(159, 29)
        Me.LabelVersion.Margin = New Padding(7, 0, 4, 0)
        Me.LabelVersion.MaximumSize = New Size(0, 20)
        Me.LabelVersion.Name = "LabelVersion"
        Me.LabelVersion.Size = New Size(300, 20)
        Me.LabelVersion.TabIndex = 0
        Me.LabelVersion.Text = "Version"
        Me.LabelVersion.TextAlign = ContentAlignment.MiddleLeft
        '
        'LabelCopyright
        '
        Me.LabelCopyright.Dock = DockStyle.Fill
        Me.LabelCopyright.Location = New Point(159, 58)
        Me.LabelCopyright.Margin = New Padding(7, 0, 4, 0)
        Me.LabelCopyright.MaximumSize = New Size(0, 20)
        Me.LabelCopyright.Name = "LabelCopyright"
        Me.LabelCopyright.Size = New Size(300, 20)
        Me.LabelCopyright.TabIndex = 0
        Me.LabelCopyright.Text = "Copyright"
        Me.LabelCopyright.TextAlign = ContentAlignment.MiddleLeft
        '
        'LabelCompanyName
        '
        Me.LabelCompanyName.Dock = DockStyle.Fill
        Me.LabelCompanyName.Location = New Point(159, 87)
        Me.LabelCompanyName.Margin = New Padding(7, 0, 4, 0)
        Me.LabelCompanyName.MaximumSize = New Size(0, 20)
        Me.LabelCompanyName.Name = "LabelCompanyName"
        Me.LabelCompanyName.Size = New Size(300, 20)
        Me.LabelCompanyName.TabIndex = 0
        Me.LabelCompanyName.Text = "Company Name"
        Me.LabelCompanyName.TextAlign = ContentAlignment.MiddleLeft
        '
        'TextBoxDescription
        '
        Me.TextBoxDescription.Dock = DockStyle.Fill
        Me.TextBoxDescription.Location = New Point(159, 119)
        Me.TextBoxDescription.Margin = New Padding(7, 3, 4, 3)
        Me.TextBoxDescription.Multiline = True
        Me.TextBoxDescription.Name = "TextBoxDescription"
        Me.TextBoxDescription.ReadOnly = True
        Me.TextBoxDescription.ScrollBars = ScrollBars.Both
        Me.TextBoxDescription.Size = New Size(300, 143)
        Me.TextBoxDescription.TabIndex = 0
        Me.TextBoxDescription.TabStop = False
        Me.TextBoxDescription.Text = resources.GetString("TextBoxDescription.Text")
        '
        'OKButton
        '
        Me.OKButton.Anchor = CType((AnchorStyles.Bottom Or AnchorStyles.Right), AnchorStyles)
        Me.OKButton.DialogResult = DialogResult.Cancel
        Me.OKButton.Location = New Point(371, 269)
        Me.OKButton.Margin = New Padding(4, 3, 4, 3)
        Me.OKButton.Name = "OKButton"
        Me.OKButton.Size = New Size(88, 26)
        Me.OKButton.TabIndex = 0
        Me.OKButton.Text = "&OK"
        '
        'AboutBox1
        '
        Me.AutoScaleDimensions = New SizeF(7.0!, 15.0!)
        Me.AutoScaleMode = AutoScaleMode.Font
        Me.CancelButton = Me.OKButton
        Me.ClientSize = New Size(483, 318)
        Me.Controls.Add(Me.TableLayoutPanel)
        Me.FormBorderStyle = FormBorderStyle.FixedDialog
        Me.Margin = New Padding(4, 3, 4, 3)
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "AboutBox1"
        Me.Padding = New Padding(10, 10, 10, 10)
        Me.ShowInTaskbar = False
        Me.StartPosition = FormStartPosition.CenterParent
        Me.Text = "AboutBox1"
        Me.TableLayoutPanel.ResumeLayout(False)
        Me.TableLayoutPanel.PerformLayout()
        CType(Me.LogoPictureBox, ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub

End Class
