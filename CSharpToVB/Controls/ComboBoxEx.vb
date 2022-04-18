' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.ComponentModel

Public Class ComboBoxEx
    Inherits ComboBox

    Private ReadOnly _buttonWidth As Integer = SystemInformation.HorizontalScrollBarArrowWidth
    Private _borderColor As Color = Color.Red
    Private _borderDrawArea As Rectangle = Rectangle.Empty
    Private _borderDrawMode As ControlBorderDrawMode = ControlBorderDrawMode.Full
    Private _fadedBorderColor As Color = Color.Red

    Public Sub New()
        Me.InitializeComponent()
    End Sub

    Public Enum ControlBorderDrawMode As Integer
        Full = 0
        Internal
        InternalFaded
    End Enum

    <Browsable(True), DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>
    <EditorBrowsable(EditorBrowsableState.Always), Category("Appearance")>
    <Description("Get or Set the Color of the Control's border")>
    Public Property BorderColor As Color
        Get
            Return _borderColor
        End Get
        Set
            _borderColor = Value
            Me.Invalidate()
        End Set
    End Property

    Public Property BorderDrawArea As Rectangle
        Get
            Return _borderDrawArea
        End Get
        Set(value As Rectangle)
            _borderDrawArea = value
        End Set
    End Property

    <Browsable(True), DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)>
    <EditorBrowsable(EditorBrowsableState.Always), Category("Appearance")>
    <Description("Determines how the colored border is drawn.
Full: specifies the control's client area and the DropDown Button
Internal: the internal section of the control, excluding the DropDown Button.")>
    Public Property BorderDrawMode As ControlBorderDrawMode
        Get
            Return _borderDrawMode
        End Get
        Set
            _borderDrawMode = Value
            Me.Invalidate()
        End Set
    End Property

    Public Property FadedBorderColor As Color
        Get
            Return _fadedBorderColor
        End Get
        Set(value As Color)
            _fadedBorderColor = value
        End Set
    End Property

    Private Sub InitializeComponent()
        Me.SetStyle(ControlStyles.ResizeRedraw Or
             ControlStyles.SupportsTransparentBackColor, True)
        Me.DrawMode = DrawMode.Normal
        Me.BorderColor = SystemColors.WindowFrame
    End Sub

    Private Sub SetBorderArea()
        Select Case _borderDrawMode
            Case ControlBorderDrawMode.Full
                Me.BorderDrawArea = New Rectangle(Point.Empty,
                                                 New Size(Me.ClientRectangle.Width - 1, Me.ClientRectangle.Height - 1))
            Case ControlBorderDrawMode.Internal
                Me.BorderDrawArea = New Rectangle(Point.Empty,
                                                 New Size(Me.ClientRectangle.Width - _buttonWidth, Me.ClientRectangle.Height - 1))
            Case ControlBorderDrawMode.InternalFaded
                Me.FadedBorderColor = Color.FromArgb(96, _borderColor)
                Me.BorderDrawArea = New Rectangle(New Point(0, 0),
                                                 New Size(Me.ClientRectangle.Width - _buttonWidth - 2, Me.ClientRectangle.Height - 2))
        End Select
    End Sub

    Protected Overrides Sub OnHandleCreated(e As EventArgs)
        Me.SetBorderArea()
        MyBase.OnHandleCreated(e)
    End Sub

    Protected Overrides Sub WndProc(ByRef m As Message)
        MyBase.WndProc(m)
        If m.Msg = WM_PAINT Or m.Msg = WM_SYNCPAINT Or m.Msg = WM_NCPAINT Then
            Using g As Graphics = Graphics.FromHwnd(Me.Handle)

                'SetBorderArea()
                ''Using pen1 As New Pen(If(Me.m_borderDrawMode = ControlBorderDrawMode.InternalFaded, m_FadedBorderColor, m_BorderColor), 1)
                'Dim pen1 = Pens.Purple
                'If Me.BorderDrawMode = ControlBorderDrawMode.Full Then
                '    g.DrawLine(pen1, ClientRectangle.Width - m_buttonWidth, 0,
                '                    ClientRectangle.Width - m_buttonWidth, ClientRectangle.Height - 1)
                '  End If
                'g.DrawRectangle(pen1, m_BorderDrawArea)

                Using p As New Pen(_borderColor, 1)

                    Dim rect As Rectangle = Me.ClientRectangle
                    rect.Width -= 1
                    rect.Height -= 1
                    g.DrawRectangle(p, rect)

                    Dim bRect As New Rectangle(rect.Left + rect.Width - 17, rect.Top, 17, rect.Height)
                    Using b As New SolidBrush(Me.BackColor)
                        g.FillRectangle(b, bRect)
                    End Using
                    g.DrawRectangle(p, bRect)

                    Dim x1 As Integer = bRect.Left + 7
                    Dim x2 As Integer = x1 + 4
                    Dim y As Integer = bRect.Top + 10
                    g.DrawLine(If(Me.BackColor = Color.White, Pens.Black, p), x1, y, x2, y)
                    x1 += 1 : x2 -= 1 : y += 1
                    g.DrawLine(If(Me.BackColor = Color.White, Pens.Black, p), x1, y, x2, y)
                    x1 += 1 : x2 -= 1 : y += 1
                    g.DrawLine(If(Me.BackColor = Color.White, Pens.Black, p), x1, y - 1, x2, y)

                End Using

                'rect.Location = New Point(rect.Left + 1, rect.Top + 1)
                'rect.Width -= 2
                'rect.Height -= 2
                'g.DrawRectangle(Pens.Purple, rect)

                'End Using
            End Using
            m.Result = IntPtr.Zero
        End If
    End Sub

End Class
