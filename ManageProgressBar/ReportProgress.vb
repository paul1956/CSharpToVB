Imports System.Drawing
Imports System.Drawing.Drawing2D
Imports System.Windows.Forms

Public Class ReportProgress
    Implements IReportProgress

    Sub New(Bar As ToolStripProgressBar)
        _ProgressBar = Bar
        ProgressBar.Value = 0
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
        ProgressBar.Value = 0
    End Sub

    Public Sub SetTotalItems(TotalItems As Integer) Implements IReportProgress.SetTotalItems
        ProgressBar.Visible = True
        ProgressBar.Value = 0
        ProgressBar.Maximum = TotalItems
    End Sub

    Public Sub UpdateProgress(Increment As Integer) Implements IReportProgress.UpdateProgress
        ProgressBar.Step = Increment
        ProgressBar.PerformStep()
        ProgressBar.Step = -1
        ProgressBar.PerformStep()
        ProgressBar.Step = 1
        ProgressBar.PerformStep()
        ProgressBar.TextImageRelation = TextImageRelation.Overlay
        pbPrecentage(ProgressBar, $"{ProgressBar.Value:N0} of {ProgressBar.Maximum:N0}")
        If ProgressBar.Value >= ProgressBar.Maximum Then
            ProgressBar.Visible = False
        End If
        If ProgressBar.Value Mod 100 = 0 Then
            Application.DoEvents()
        End If
    End Sub
End Class
