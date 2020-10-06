' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Drawing.Drawing2D
Imports System.Threading
Imports ProgressReportLibrary

Public Class ToolStripTextProgressBar
    Inherits ToolStripProgressBar

    Public Sub New()
        Me.TextImageRelation = TextImageRelation.Overlay
        Me.Value = 0
    End Sub

    Public Overloads Property Maximum() As Integer
        Get
            Return MyBase.Maximum
        End Get
        Set(value As Integer)
            MyBase.Maximum = value

        End Set
    End Property

    Private Sub pbPrecentage(Text As String)
        Me.Invalidate()
        Using g As Graphics = Me.ProgressBar.CreateGraphics()
            g.PixelOffsetMode = PixelOffsetMode.HighSpeed
            'Switch to Anti-aliased drawing for better (smoother) graphic results
            g.SmoothingMode = SmoothingMode.AntiAlias
            Dim sizeF As SizeF = g.MeasureString(Text, Me.Font)
            g.DrawString(Text, Me.Font, Brushes.Black, New PointF((Me.Width \ 2) - (sizeF.Width / 2.0F), (Me.Height \ 2) - (sizeF.Height / 2.0F)))
        End Using
    End Sub

    Public Sub Clear()
        Me.Value = 0
    End Sub

    ''' <summary>
    ''' Advances the current position of the underlying Progress Bar by the specified amount.
    ''' </summary>
    ''' <param name="Value">The amount by which to increment the underlying progress bar's current position.</param>
    Public Overloads Sub Increment(Value As Integer)
        MyBase.Increment(Value)
        Me.pbPrecentage($"{ MyBase.Value:N0} of { Me.Maximum:N0}")
        Thread.Sleep(2)
        If MyBase.Value = Me.Maximum Then
            MyBase.Value = 0
        End If
    End Sub

    Public Sub Update(value As ProgressReport)
        If Me.Maximum <> value.Maximum Then
            Me.Maximum = value.Maximum
        End If
        MyBase.Value = value.Current
        If value.Current = 0 OrElse value.Current > Me.Maximum Then
            Exit Sub
        End If

        Me.pbPrecentage($"{value.Current:N0} of {value.Maximum:N0}")
    End Sub

End Class
