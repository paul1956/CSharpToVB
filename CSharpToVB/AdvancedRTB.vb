' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.Runtime.InteropServices

Imports CSharpToVBApp.Microsoft.VisualBasic.CompilerServices.NativeMethods

Public Class AdvancedRTB

    'the vertical scroll bar of the hwnd window

    Private _sbi As SCROLLBARINFO

    Private _vertRightClicked As Boolean = False

    Public Sub New()
        InitializeComponent()
    End Sub

    Public Event HorizScrollBarRightClicked(sender As Object, loc As Point)

    Public Event VertScrollBarRightClicked(sender As Object, loc As Point)

    ''' <summary>
    ''' Gets and Sets the Horizontal Scroll position of the control.
    ''' </summary>
    Public Property HScrollPos() As Integer
        Get
            Return GetScrollPos(Handle, SBOrientation.HORZ)

        End Get
        Set(value As Integer)
            SetScrollPos(Handle, SBOrientation.HORZ, True)
        End Set
    End Property

    ''' <summary>
    ''' Gets and Sets the Vertical Scroll position of the control.
    ''' </summary>
    Public Property VScrollPos() As Integer
        Get
            Return GetScrollPos(Handle, SBOrientation.VERT)
        End Get
        Set(value As Integer)
            SetScrollPos(Handle, SBOrientation.VERT, True)
        End Set
    End Property

    ''' <summary>
    ''' Returns the Position of the Scroll Bar Thumb
    ''' </summary>
    ''' <param name="handle"></param>
    ''' <param name="SB_Orientation"></param>
    ''' <returns></returns>
    Private Shared Function GetScrollPos(handle As IntPtr, SB_Orientation As SBOrientation) As Integer
        Dim si As New SCROLLINFO
        With si
            .CB_Size = CUInt(Marshal.SizeOf(si))
            .F_Mask = ScrollInfoMasks.ALL
        End With
        Dim lRet As Integer = GetScrollInfo(handle, SB_Orientation, si)
        Return If(lRet <> 0, si.N_Pos, -1)
    End Function

    Private Shared Sub SetScrollPos(handle As IntPtr, SB_Orientation As SBOrientation, v As Boolean)
        Dim scrollinfo As New SCROLLINFO With {
            .CB_Size = CUInt(Marshal.SizeOf(GetType(SCROLLINFO))),
            .F_Mask = ScrollInfoMasks.POS
        }
        Dim unused As Integer = SetScrollInfo(hWnd:=handle, nBar:=SB_Orientation, lpsi:=scrollinfo, bRepaint:=v)
    End Sub

    Private Sub ToolStripMenuItem1_Click(sender As Object, e As EventArgs) Handles mnuScrollHere.Click
        With _sbi
            Dim SB_Size As Integer = .RC_ScrollBar.Bottom - .RC_ScrollBar.Top
            Dim Percent As Double = 1 - (SB_Size - PointToClient(MousePosition).Y) / SB_Size
            Dim DesiredLine As Integer = CInt(Lines.Length * Percent)
            [Select](GetFirstCharIndexFromLine(DesiredLine), 0)
            ScrollToCaret()
        End With
    End Sub

    Private Sub ToolStripMenuItem2_Click(sender As Object, e As EventArgs) Handles mnuScrollTop.Click
        SelectionStart = 0
    End Sub

    Private Sub ToolStripMenuItem3_Click(sender As Object, e As EventArgs) Handles mnuScrollBottom.Click
        SelectionStart = Text.Length
    End Sub

    <DebuggerStepThrough>
    Protected Overrides Sub WndProc(ByRef m As Message)
        If m.Msg = NativeTypes.WM_NCRBUTTONDOWN Then
            _sbi = New SCROLLBARINFO
            _sbi.CB_Size = Marshal.SizeOf(_sbi)
            GetScrollBarInfo(Handle, NativeTypes.OBJID_VSCROLL, _sbi)
            If _sbi.RC_ScrollBar.ToRectangle.Contains(MousePosition) Then
                _vertRightClicked = True
            Else
                _sbi.CB_Size = 0
                MyBase.WndProc(m)
            End If
        Else
            MyBase.WndProc(m)
            Exit Sub
        End If

        If _vertRightClicked Then
            _vertRightClicked = False
            ContextMenuStrip1.Show(Me, PointToClient(MousePosition))
        End If
    End Sub

End Class
