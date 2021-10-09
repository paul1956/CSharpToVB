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

    Public Property DisplayIncrement As Integer = 10

    Public Overloads Property Maximum() As Integer
        Get
            Return MyBase.Maximum
        End Get
        Set
            MyBase.Maximum = Value

        End Set
    End Property

    Private Sub PbPrecentage(txt As String)
        Me.Invalidate()
        Using g As Graphics = Me.ProgressBar.CreateGraphics()
            g.PixelOffsetMode = PixelOffsetMode.HighSpeed
            'Switch to Anti-aliased drawing for better (smoother) graphic results
            g.SmoothingMode = SmoothingMode.AntiAlias
            TextRenderer.MeasureText(g, txt, Me.Font)
            Dim sizeF As SizeF = g.MeasureString(txt, Me.Font)
            Dim pt As New Point(CInt((Me.Width / 2) - (sizeF.Width / 2.0F)), CInt((Me.Height / 2) - (sizeF.Height / 2.0F)))
            TextRenderer.DrawText(g, txt, Me.Font, pt, Color.Black)
        End Using
    End Sub

    Public Sub Clear()
        Me.Value = 0
    End Sub

    ''' <summary>
    ''' Advances the current position of the underlying Progress Bar by the specified amount.
    ''' </summary>
    ''' <param name="val">The amount by which to increment the underlying progress bar's current position.</param>
    Public Overloads Sub Increment(val As Integer)
        MyBase.Increment(val)
        If Me.Value >= Me.Maximum Then
            Me.Value = 0
            Exit Sub
        End If
        If Me.Value Mod Me.DisplayIncrement = 0 Then
            Me.PbPrecentage($"{ Me.Value:N0} of { Me.Maximum:N0}")
            Thread.Sleep(1)
        End If
    End Sub

    Public Sub Update(val As ProgressReport)
        If Me.Maximum <> val.Maximum Then
            Me.Maximum = val.Maximum
        End If
        Me.Value = val.Current
        If val.Current = 0 Then
            Exit Sub
        End If
        If val.Current >= Me.Maximum Then
            Me.Clear()
            Exit Sub
        End If

        If Me.Value Mod Me.DisplayIncrement = 0 Then
            Me.PbPrecentage($"{val.Current:N0} of {val.Maximum:N0}")
            If Debugger.IsAttached Then
                Thread.Sleep(1)
            End If
        End If
    End Sub

End Class
