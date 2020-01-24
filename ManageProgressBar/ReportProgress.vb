Imports System.Drawing
Imports System.Drawing.Drawing2D
Imports System.Threading
Imports System.Windows.Forms

Public Class ReportProgress
    Implements IReportProgress

    Private _currentValue As Integer
    Private ReadOnly _defaultFont As Font = New Font("Segoe UI", 7, FontStyle.Bold)

    Public Sub New(Bar As ToolStripProgressBar)
        _ProgressBar = Bar
        ProgressBar.Value = 0
        _currentValue = 0
    End Sub

    Private ReadOnly Property ProgressBar As ToolStripProgressBar

    Private Sub pbPrecentage(pb As ToolStripProgressBar, Text As String)
        pb.Invalidate()
        Using g As Graphics = pb.ProgressBar.CreateGraphics()
            'Switch to Anti-aliased drawing for better (smoother) graphic results
            g.PixelOffsetMode = PixelOffsetMode.HighQuality
            g.SmoothingMode = SmoothingMode.AntiAlias
            g.DrawString(Text, _defaultFont, Brushes.Black, New PointF(pb.Width \ 2 - (g.MeasureString(Text, _defaultFont).Width / 2.0F), pb.Height \ 2 - (g.MeasureString(Text, _defaultFont).Height / 2.0F)))
        End Using
    End Sub

    Public Sub Clear() Implements IReportProgress.Clear
        ProgressBar.Value = 0
        ProgressBar.Visible = False
    End Sub

    Public Sub SetTotalItems(TotalItems As Integer) Implements IReportProgress.SetTotalItems
        ProgressBar.Visible = True
        ProgressBar.Value = 0
        ProgressBar.Maximum = TotalItems
    End Sub

    Public Sub UpdateProgress(Increment As Integer) Implements IReportProgress.UpdateProgress
        _currentValue += Increment
        ProgressBar.TextImageRelation = TextImageRelation.Overlay
        ProgressBar.Increment(Increment)
        pbPrecentage(ProgressBar, $"{ProgressBar.Value:N0} of {ProgressBar.Maximum:N0}")
        Thread.Sleep(5)
        If _currentValue >= ProgressBar.Maximum Then
            ProgressBar.Visible = False
        End If
    End Sub

End Class
