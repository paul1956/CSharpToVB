' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Option Explicit On
Option Strict On
Option Infer On

Imports System.ComponentModel
Imports System.Drawing.Drawing2D

<ToolboxBitmap(GetType(Panel))>
Public Class PanelEx
  Inherits Panel

    Private _borderStyle As BorderStyle = BorderStyle.FixedSingle
    Private _borderColor As Color = SystemColors.WindowFrame
    Private _borderWidth As Integer = 1

  Public Sub New()
    MyBase.New()
        'SetDefaultControlStyles()
        Me.CustomInitialize()
  End Sub

  Private Sub CustomInitialize()
        Me.SuspendLayout()
        Me.BackColor = Color.Transparent
        Me.BorderStyle = BorderStyle.FixedSingle
        Me.ResumeLayout(False)
  End Sub

  <DefaultValue(GetType(BorderStyle), "FixedSingle"),
   Category("Appearance"),
   Description("The border style used to paint the control.")>
  Public Shadows Property BorderStyle() As BorderStyle
    Get
      Return _borderStyle
    End Get
    Set
      _borderStyle = value
      If Me.DesignMode Then Me.Invalidate()
    End Set
  End Property

  <DefaultValue(GetType(Color), "WindowFrame"),
   Category("Appearance"),
   Description("The border color used to paint the control.")>
  Public Property BorderColor() As Color
    Get
      Return _borderColor
    End Get
    Set
      _borderColor = value
            'If DesignMode Then
            Me.Invalidate()
      'End If
    End Set
  End Property

  <DefaultValue(GetType(Integer), "1"),
   Category("Appearance"),
   Description("The width of the border used to paint the control.")>
  Public Property BorderWidth() As Integer
    Get
      Return _borderWidth
    End Get
    Set
      _borderWidth = value
      If Me.DesignMode Then Me.Invalidate()
    End Set
  End Property

  Protected Overrides Sub OnPaintBackGround(e As PaintEventArgs)

    MyBase.OnPaintBackground(e)

    e.Graphics.SmoothingMode = SmoothingMode.AntiAlias

    Select Case _borderStyle
      Case BorderStyle.FixedSingle
        Using borderPen = New Pen(_borderColor, _borderWidth)
          If _borderWidth > 1 Then
            Dim path = New GraphicsPath
            Try
              Dim offset = If(_borderStyle = BorderStyle.FixedSingle AndAlso
                              _borderWidth > 1, Me.BorderWidth \ 2, 0)
              path.AddRectangle(Rectangle.Inflate(Me.ClientRectangle, -offset, -offset))
              e.Graphics.DrawPath(borderPen, path)
            Finally
              path.Dispose()
            End Try
          Else
            'path.AddRectangle(ClientRectangle)
            Dim rect = Me.ClientRectangle
            rect.Width -= 1 : rect.Height -= 1
            e.Graphics.DrawRectangle(borderPen, rect)
          End If
        End Using

      Case BorderStyle.Fixed3D
        e.Graphics.SmoothingMode = SmoothingMode.Default
        e.Graphics.DrawLine(SystemPens.ControlDark, Me.ClientRectangle.X, Me.ClientRectangle.Y, Me.ClientRectangle.Width - 1, Me.ClientRectangle.Y)
        e.Graphics.DrawLine(SystemPens.ControlDark, Me.ClientRectangle.X, Me.ClientRectangle.Y, Me.ClientRectangle.X, Me.ClientRectangle.Height - 1)
        e.Graphics.DrawLine(SystemPens.ControlDarkDark, Me.ClientRectangle.X + 1, Me.ClientRectangle.Y + 1, Me.ClientRectangle.Width - 1, Me.ClientRectangle.Y + 1)
        e.Graphics.DrawLine(SystemPens.ControlDarkDark, Me.ClientRectangle.X + 1, Me.ClientRectangle.Y + 1, Me.ClientRectangle.X + 1, Me.ClientRectangle.Height - 1)
        e.Graphics.DrawLine(SystemPens.ControlLight, Me.ClientRectangle.X + 1, Me.ClientRectangle.Height - 2, Me.ClientRectangle.Width - 2, Me.ClientRectangle.Height - 2)
        e.Graphics.DrawLine(SystemPens.ControlLight, Me.ClientRectangle.Width - 2, Me.ClientRectangle.Y + 1, Me.ClientRectangle.Width - 2, Me.ClientRectangle.Height - 2)
        e.Graphics.DrawLine(SystemPens.ControlLightLight, Me.ClientRectangle.X, Me.ClientRectangle.Height - 1, Me.ClientRectangle.Width - 1, Me.ClientRectangle.Height - 1)
        e.Graphics.DrawLine(SystemPens.ControlLightLight, Me.ClientRectangle.Width - 1, Me.ClientRectangle.Y, Me.ClientRectangle.Width - 1, Me.ClientRectangle.Height - 1)

      Case BorderStyle.None

    End Select

  End Sub

End Class
