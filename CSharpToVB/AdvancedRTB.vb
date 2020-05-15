' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.Runtime.InteropServices

Public Class AdvancedRTB
    Inherits RichTextBox

    Friend WithEvents ContextMenuStrip1 As ContextMenuStrip

    Friend WithEvents ToolStripMenuItem1 As ToolStripMenuItem

    Friend WithEvents ToolStripMenuItem2 As ToolStripMenuItem

    Friend WithEvents ToolStripMenuItem3 As ToolStripMenuItem

    Friend WithEvents ToolStripSeparator1 As ToolStripSeparator

    Private Const OBJID_VSCROLL As Long = &HFFFFFFFB

    Private Const WM_NCRBUTTONDOWN As Integer = &HA4

    'the vertical scroll bar of the hwnd window
    <CodeAnalysis.SuppressMessage("Code Quality", "IDE0069:Disposable fields should be disposed", Justification:="Can't figure out how to dispose")>
    Private _components As System.ComponentModel.IContainer

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

    Private Sub InitializeComponent()
        _components = New ComponentModel.Container()
        ContextMenuStrip1 = New ContextMenuStrip(_components)
        ToolStripMenuItem1 = New ToolStripMenuItem()
        ToolStripMenuItem2 = New ToolStripMenuItem()
        ToolStripMenuItem3 = New ToolStripMenuItem()
        ToolStripSeparator1 = New ToolStripSeparator()
        ContextMenuStrip1.SuspendLayout()
        SuspendLayout()
        '
        'ContextMenuStrip1
        '
        ContextMenuStrip1.Items.AddRange(New ToolStripItem() {ToolStripMenuItem1, ToolStripSeparator1, ToolStripMenuItem2, ToolStripMenuItem3})
        ContextMenuStrip1.Name = "ContextMenuStrip1"
        ContextMenuStrip1.Size = New Size(147, 76)
        '
        'ToolStripMenuItem1
        '
        ToolStripMenuItem1.Name = "ToolStripMenuItem1"
        ToolStripMenuItem1.Size = New Size(146, 22)
        ToolStripMenuItem1.Text = "Scroll Here"
        '
        'ToolStripMenuItem2
        '
        ToolStripMenuItem2.Name = "ToolStripMenuItem2"
        ToolStripMenuItem2.Size = New Size(146, 22)
        ToolStripMenuItem2.Text = "Scroll Top"
        '
        'ToolStripMenuItem3
        '
        ToolStripMenuItem3.Name = "ToolStripMenuItem3"
        ToolStripMenuItem3.Size = New Size(146, 22)
        ToolStripMenuItem3.Text = "Scroll Bottom"
        '
        'ToolStripSeparator1
        '
        ToolStripSeparator1.Name = "ToolStripSeparator1"
        ToolStripSeparator1.Size = New Size(143, 6)
        ContextMenuStrip1.ResumeLayout(False)
        ResumeLayout(False)
    End Sub

    Private Sub ToolStripMenuItem1_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem1.Click
        With _sbi
            Dim SB_Size As Integer = .RC_ScrollBar.Bottom - .RC_ScrollBar.Top
            Dim Percent As Double = 1 - (SB_Size - PointToClient(MousePosition).Y) / SB_Size
            Dim DesiredLine As Integer = CInt(Lines.Length * Percent)
            [Select](GetFirstCharIndexFromLine(DesiredLine), 0)
            ScrollToCaret()
        End With
    End Sub

    Private Sub ToolStripMenuItem2_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem2.Click
        SelectionStart = 0
    End Sub

    Private Sub ToolStripMenuItem3_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem3.Click
        SelectionStart = Text.Length

    End Sub

    <DebuggerStepThrough>
    Protected Overrides Sub WndProc(ByRef m As Message)
        If m.Msg = WM_NCRBUTTONDOWN Then
            _sbi = New SCROLLBARINFO
            _sbi.CB_Size = Marshal.SizeOf(_sbi)
            GetScrollBarInfo(Handle, OBJID_VSCROLL, _sbi)
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

#Region "IDisposable Support"

    Private _disposedValue As Boolean ' To detect redundant calls

    ' IDisposable
    Protected Overrides Sub Dispose(disposing As Boolean)
        If Not _disposedValue Then
            MyBase.Dispose(disposing)
            If disposing Then
                _components.Dispose()
                ' TODO: dispose managed state (managed objects).
            End If

            ' TODO: free unmanaged resources (unmanaged objects) and override Finalize() below.
            ' TODO: set large fields to null.
        End If
        _disposedValue = True
    End Sub

#End Region

End Class
