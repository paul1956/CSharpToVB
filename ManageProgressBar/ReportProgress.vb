Imports System.Drawing
Imports System.Drawing.Drawing2D
Imports System.Windows.Forms

Public Class ReportProgress
    Implements IReportProgress

    Sub New(Bar As ToolStripProgressBar)
        Me._ProgressBar = Bar
        Me.ProgressBar.Value = 0
    End Sub

    Private ReadOnly Property ProgressBar As ToolStripProgressBar

    Private Sub pbPrecentage(pb As ToolStripProgressBar, Text As String)
        Using gr As Graphics = pb.ProgressBar.CreateGraphics()
            'Switch to Anti-aliased drawing for better (smoother) graphic results
            gr.SmoothingMode = SmoothingMode.AntiAlias
            gr.DrawString(Text, SystemFonts.DefaultFont, Brushes.Black, New PointF(pb.Width \ 2 - (gr.MeasureString(Text, SystemFonts.DefaultFont).Width / 2.0F), pb.Height \ 2 - (gr.MeasureString(Text, SystemFonts.DefaultFont).Height / 2.0F)))
        End Using
    End Sub

    Public Sub Clear() Implements IReportProgress.Clear
        Me.ProgressBar.Value = 0
    End Sub

    Public Sub SetTotalItems(TotalItems As Integer) Implements IReportProgress.SetTotalItems
        Me.ProgressBar.Visible = True
        Me.ProgressBar.Value = 0
        Me.ProgressBar.Maximum = TotalItems
    End Sub

    Public Sub UpdateProgress(Increment As Integer) Implements IReportProgress.UpdateProgress
        Me.ProgressBar.Step = Increment
        Me.ProgressBar.PerformStep()
        Me.ProgressBar.Step = -1
        Me.ProgressBar.PerformStep()
        Me.ProgressBar.Step = 1
        Me.ProgressBar.PerformStep()
        Me.ProgressBar.TextImageRelation = TextImageRelation.Overlay
        Me.pbPrecentage(Me.ProgressBar, $"{Me.ProgressBar.Value:N0} of {Me.ProgressBar.Maximum:N0}")
        If Me.ProgressBar.Value >= Me.ProgressBar.Maximum Then
            Me.ProgressBar.Visible = False
        End If
        If Me.ProgressBar.Value Mod 100 = 0 Then
            Application.DoEvents()
        End If
    End Sub
End Class
