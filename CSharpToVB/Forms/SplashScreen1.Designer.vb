<CompilerServices.DesignerGenerated()>
Partial Class SplashScreen1
    Inherits Form

    'Form overrides dispose to clean up the component list.
    <DebuggerNonUserCode()>
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub
    Friend WithEvents ApplicationTitle As Label
    Friend WithEvents Version As Label
    Friend WithEvents Copyright As Label
    Friend WithEvents MainLayoutPanel As TableLayoutPanel
    Friend WithEvents DetailsLayoutPanel As TableLayoutPanel
    Friend WithEvents UserName As Label

    'Required by the Windows Form Designer
    Private components As ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <DebuggerStepThrough()>
    Private Sub InitializeComponent()
        Dim resources As ComponentModel.ComponentResourceManager = New ComponentModel.ComponentResourceManager(GetType(SplashScreen1))
        Me.MainLayoutPanel = New TableLayoutPanel()
        Me.DetailsLayoutPanel = New TableLayoutPanel()
        Me.ApplicationTitle = New Label()
        Me.Copyright = New Label()
        Me.UserName = New Label()
        Me.Version = New Label()
        Me.MainLayoutPanel.SuspendLayout()
        Me.DetailsLayoutPanel.SuspendLayout()
        Me.SuspendLayout()
        '
        'MainLayoutPanel
        '
        Me.MainLayoutPanel.BackgroundImage = CType(resources.GetObject("MainLayoutPanel.BackgroundImage"), Image)
        Me.MainLayoutPanel.BackgroundImageLayout = ImageLayout.Stretch
        Me.MainLayoutPanel.ColumnCount = 2
        Me.MainLayoutPanel.ColumnStyles.Add(New ColumnStyle(SizeType.Absolute, 314.0!))
        Me.MainLayoutPanel.ColumnStyles.Add(New ColumnStyle(SizeType.Absolute, 265.0!))
        Me.MainLayoutPanel.Controls.Add(Me.DetailsLayoutPanel, 0, 1)
        Me.MainLayoutPanel.Controls.Add(Me.ApplicationTitle, 1, 1)
        Me.MainLayoutPanel.Dock = DockStyle.Fill
        Me.MainLayoutPanel.Location = New Point(0, 0)
        Me.MainLayoutPanel.Name = "MainLayoutPanel"
        Me.MainLayoutPanel.RowStyles.Add(New RowStyle(SizeType.Absolute, 285.0!))
        Me.MainLayoutPanel.RowStyles.Add(New RowStyle(SizeType.Absolute, 22.0!))
        Me.MainLayoutPanel.RowStyles.Add(New RowStyle(SizeType.Absolute, 20.0!))
        Me.MainLayoutPanel.Size = New Size(579, 350)
        Me.MainLayoutPanel.TabIndex = 0
        '
        'DetailsLayoutPanel
        '
        Me.DetailsLayoutPanel.Anchor = AnchorStyles.None
        Me.DetailsLayoutPanel.BackColor = Color.White
        Me.DetailsLayoutPanel.ColumnStyles.Add(New ColumnStyle(SizeType.Absolute, 308.0!))
        Me.DetailsLayoutPanel.ColumnStyles.Add(New ColumnStyle(SizeType.Absolute, 20.0!))
        Me.DetailsLayoutPanel.Controls.Add(Me.Version, 0, 0)
        Me.DetailsLayoutPanel.Controls.Add(Me.UserName, 0, 1)
        Me.DetailsLayoutPanel.Controls.Add(Me.Copyright, 0, 2)
        Me.DetailsLayoutPanel.Location = New Point(3, 288)
        Me.DetailsLayoutPanel.Name = "DetailsLayoutPanel"
        Me.DetailsLayoutPanel.RowStyles.Add(New RowStyle(SizeType.Percent, 33.0!))
        Me.DetailsLayoutPanel.RowStyles.Add(New RowStyle(SizeType.Percent, 33.0!))
        Me.DetailsLayoutPanel.RowStyles.Add(New RowStyle(SizeType.Percent, 34.0!))
        Me.DetailsLayoutPanel.Size = New Size(308, 58)
        Me.DetailsLayoutPanel.TabIndex = 1
        '
        'Version
        '
        Me.Version.Anchor = AnchorStyles.Left
        Me.Version.AutoSize = True
        Me.Version.BackColor = Color.White
        Me.Version.Font = New Font("Segoe UI", 9.0!, FontStyle.Bold, GraphicsUnit.Point)
        Me.Version.Location = New Point(3, 2)
        Me.Version.Name = "Version"
        Me.Version.Size = New Size(140, 15)
        Me.Version.TabIndex = 1
        Me.Version.Text = "Version: {0}.{1:00}.{2}.{3}"
        Me.Version.TextAlign = ContentAlignment.MiddleLeft
        '
        'UserName
        '
        Me.UserName.Anchor = AnchorStyles.Left
        Me.UserName.AutoSize = True
        Me.UserName.BackColor = Color.White
        Me.UserName.Font = New Font("Segoe UI", 9.0!, FontStyle.Bold, GraphicsUnit.Point)
        Me.UserName.Location = New Point(3, 21)
        Me.UserName.Name = "UserName"
        Me.UserName.Size = New Size(69, 15)
        Me.UserName.TabIndex = 2
        Me.UserName.Text = "User Name"
        Me.UserName.TextAlign = ContentAlignment.MiddleLeft
        '
        'Copyright
        '
        Me.Copyright.Anchor = AnchorStyles.Left
        Me.Copyright.AutoSize = True
        Me.Copyright.BackColor = Color.White
        Me.Copyright.Font = New Font("Segoe UI", 9.0!, FontStyle.Bold, GraphicsUnit.Point)
        Me.Copyright.Location = New Point(3, 40)
        Me.Copyright.Name = "Copyright"
        Me.Copyright.Size = New Size(61, 15)
        Me.Copyright.TabIndex = 2
        Me.Copyright.Text = "Copyright"
        Me.Copyright.TextAlign = ContentAlignment.MiddleLeft
        '
        'ApplicationTitle
        '
        Me.ApplicationTitle.AutoSize = True
        Me.ApplicationTitle.BackColor = Color.Transparent
        Me.ApplicationTitle.Dock = DockStyle.Bottom
        Me.ApplicationTitle.Font = New Font("Segoe UI", 15.75!, FontStyle.Regular, GraphicsUnit.Point)
        Me.ApplicationTitle.ForeColor = SystemColors.MenuHighlight
        Me.ApplicationTitle.Location = New Point(317, 320)
        Me.ApplicationTitle.Name = "ApplicationTitle"
        Me.ApplicationTitle.Size = New Size(259, 30)
        Me.ApplicationTitle.TabIndex = 0
        Me.ApplicationTitle.Text = "Application Title"
        Me.ApplicationTitle.TextAlign = ContentAlignment.MiddleRight
        '
        'SplashScreen1
        '
        Me.AutoScaleDimensions = New SizeF(7.0!, 15.0!)
        Me.AutoScaleMode = AutoScaleMode.Font
        Me.ClientSize = New Size(579, 350)
        Me.ControlBox = False
        Me.Controls.Add(Me.MainLayoutPanel)
        Me.Font = New Font("Segoe UI", 9.0!, FontStyle.Regular, GraphicsUnit.Point)
        Me.FormBorderStyle = FormBorderStyle.FixedSingle
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "SplashScreen1"
        Me.ShowInTaskbar = False
        Me.StartPosition = FormStartPosition.CenterScreen
        Me.MainLayoutPanel.ResumeLayout(False)
        Me.MainLayoutPanel.PerformLayout()
        Me.DetailsLayoutPanel.ResumeLayout(False)
        Me.DetailsLayoutPanel.PerformLayout()
        Me.ResumeLayout(False)

    End Sub

End Class
