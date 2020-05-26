<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class AdvancedRTB
    Inherits System.Windows.Forms.RichTextBox

    Private _disposedValue As Boolean ' To detect redundant calls

    <System.Diagnostics.DebuggerNonUserCode()>
    Protected Overrides Sub Dispose(disposing As Boolean)
        MyBase.Dispose(disposing)
        If Not _disposedValue Then
            If disposing Then
                components.Dispose()
                ' TODO: dispose managed state (managed objects).
            End If

            ' TODO: free unmanaged resources (unmanaged objects) and override Finalize() below.
            ' TODO: set large fields to null.
        End If
        _disposedValue = True
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()>
    Private Sub InitializeComponent()
        components = New System.ComponentModel.Container()
        ContextMenuStrip1 = New ContextMenuStrip(components)
        mnuScrollHere = New ToolStripMenuItem()
        mnuScrollTop = New ToolStripMenuItem()
        mnuScrollBottom = New ToolStripMenuItem()
        ToolStripSeparator1 = New ToolStripSeparator()
        ContextMenuStrip1.SuspendLayout()
        SuspendLayout()
        '
        'ContextMenuStrip1
        '
        ContextMenuStrip1.Items.AddRange(New ToolStripItem() {mnuScrollHere, ToolStripSeparator1, mnuScrollTop, mnuScrollBottom})
        ContextMenuStrip1.Name = "ContextMenuStrip1"
        ContextMenuStrip1.Size = New Size(147, 76)
        '
        'mnuScrollHere
        '
        mnuScrollHere.Name = "mnuScrollHere"
        mnuScrollHere.Size = New Size(146, 22)
        mnuScrollHere.Text = "Scroll Here"
        '
        'mnuScrollTop
        '
        mnuScrollTop.Name = "mnuScrollTop"
        mnuScrollTop.Size = New Size(146, 22)
        mnuScrollTop.Text = "Scroll Top"
        '
        'ToolStripMenuItem3
        '
        mnuScrollBottom.Name = "ToolStripMenuItem3"
        mnuScrollBottom.Size = New Size(146, 22)
        mnuScrollBottom.Text = "Scroll Bottom"
        '
        'ToolStripSeparator1
        '
        ToolStripSeparator1.Name = "ToolStripSeparator1"
        ToolStripSeparator1.Size = New Size(143, 6)
        ContextMenuStrip1.ResumeLayout(False)
        ResumeLayout(False)
    End Sub

    Friend WithEvents ContextMenuStrip1 As ContextMenuStrip
    Friend WithEvents mnuScrollHere As ToolStripMenuItem
    Friend WithEvents mnuScrollTop As ToolStripMenuItem
    Friend WithEvents mnuScrollBottom As ToolStripMenuItem
    Friend WithEvents ToolStripSeparator1 As ToolStripSeparator
End Class
