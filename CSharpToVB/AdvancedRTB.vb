Option Explicit On
Option Infer Off
Option Strict On

Imports System.Runtime.InteropServices
Imports CSharpToVBApp

Public Class AdvancedRTB
    Inherits RichTextBox

    Sub New()
        Me.InitializeComponent()
    End Sub

    Public Event VertScrollBarRightClicked(ByVal sender As Object, ByVal loc As Point)
    Public Event HorizScrollBarRightClicked(ByVal sender As Object, ByVal loc As Point)

    'Private Const OBJID_CLIENT As Long = &HFFFFFFFC 'the hwnd parm is a handle to a scroll bar control
    'Private Const OBJID_HSCROLL As Long = &HFFFFFFFA 'the horizontal scroll bar of the hwnd window
    Private Const OBJID_VSCROLL As Long = &HFFFFFFFB 'the vertical scroll bar of the hwnd window
    Friend WithEvents ContextMenuStrip1 As ContextMenuStrip
    <CodeAnalysis.SuppressMessage("Code Quality", "IDE0069:Disposable fields should be disposed", Justification:="<Pending>")>
    Private components As System.ComponentModel.IContainer
    Friend WithEvents ToolStripMenuItem1 As ToolStripMenuItem
    Friend WithEvents ToolStripSeparator1 As ToolStripSeparator
    Friend WithEvents ToolStripMenuItem2 As ToolStripMenuItem
    Friend WithEvents ToolStripMenuItem3 As ToolStripMenuItem
    Private Const WM_NCRBUTTONDOWN As Integer = &HA4

    <DllImport("user32.dll", SetLastError:=True, ThrowOnUnmappableChar:=True, CharSet:=CharSet.Auto)>
    Private Shared Function SetScrollInfo(
                                        hWnd As IntPtr,
                                        <MarshalAs(UnmanagedType.I4)> nBar As SBOrientation,
                                        <MarshalAs(UnmanagedType.Struct)> ByRef lpsi As SCROLLINFO,
                                        <MarshalAs(UnmanagedType.Bool)> bRepaint As Boolean) As Integer
    End Function

    <DllImport("user32.dll", SetLastError:=True)>
    Private Shared Function GetScrollInfo(
                                         hWnd As IntPtr,
                                         <MarshalAs(UnmanagedType.I4)> fnBar As SBOrientation,
                                         ByRef lpsi As SCROLLINFO) As Integer
    End Function

    <DllImport("user32.dll", EntryPoint:=NameOf(GetScrollBarInfo))>
    Private Shared Function GetScrollBarInfo(ByVal hwnd As IntPtr,
                                            ByVal idObject As Integer,
                                            ByRef psbi As SCROLLBARINFO) As <MarshalAs(UnmanagedType.Bool)> Boolean
    End Function

    Private VertRightClicked As Boolean = False
    Private sbi As SCROLLBARINFO
    Public Enum SBOrientation As Integer
        SB_HORZ = &H0
        SB_VERT = &H1
        SB_CTL = &H2
        SB_BOTH = &H3
    End Enum

    <Flags>
    Public Enum ScrollInfoMask As UInteger
        SIF_RANGE = &H1
        SIF_PAGE = &H2
        SIF_POS = &H4
        SIF_DISABLENOSCROLL = &H8
        SIF_TRACKPOS = &H10
        SIF_ALL = (SIF_RANGE Or SIF_PAGE Or SIF_POS Or SIF_TRACKPOS)
    End Enum

    <Serializable, StructLayout(LayoutKind.Sequential)>
    Private Structure SCROLLINFO
        Public CB_Size As UInteger
        <MarshalAs(UnmanagedType.U4)> Public F_Mask As ScrollInfoMask
        Public N_Min As Integer
        Public N_Max As Integer
        Public N_Page As UInteger
        Public N_Pos As Integer
        Public N_TrackPos As Integer
    End Structure

    <StructLayout(LayoutKind.Sequential)>
    Private Structure SCROLLBARINFO
        Public CB_Size As Integer
        Public RC_ScrollBar As RECT
        Public DXY_LineButton As Integer
        Public XY_ThumbTop As Integer
        Public XY_ThumbBottom As Integer
        Public Reserved As Integer
        <MarshalAs(UnmanagedType.ByValArray, SizeConst:=6, ArraySubType:=UnmanagedType.U4)>
        Public RgState() As Integer
    End Structure

    <StructLayout(LayoutKind.Sequential)>
    Private Structure RECT
        Public Left, Top, Right, Bottom As Integer
        Public Function ToRectangle() As Rectangle
            Return New Rectangle(Me.Left, Me.Top, Me.Right - Me.Left, Me.Bottom - Me.Top)
        End Function
    End Structure

    Private Sub SetScrollPos(handle As IntPtr, SB_Orientation As SBOrientation, v As Boolean)
        Dim scrollinfo As New SCROLLINFO With {
            .CB_Size = CUInt(Marshal.SizeOf(GetType(SCROLLINFO))),
            .F_Mask = ScrollInfoMask.SIF_POS
        }
        SetScrollInfo(hWnd:=handle, nBar:=SB_Orientation, lpsi:=scrollinfo, bRepaint:=v)
    End Sub
    ''' <summary>
    ''' Returns the Position of the Scroll Bar Thumb
    ''' </summary>
    ''' <param name="handle"></param>
    ''' <param name="SB_Orientation"></param>
    ''' <returns></returns>
    Private Function GetScrollPos(handle As IntPtr, SB_Orientation As SBOrientation) As Integer
        Dim si As New SCROLLINFO
        With si
            .CB_Size = CUInt(Marshal.SizeOf(si))
            .F_Mask = ScrollInfoMask.SIF_ALL
        End With
        Dim lRet As Integer = GetScrollInfo(handle, SB_Orientation, si)
        Return If(lRet <> 0, si.N_Pos, -1)
    End Function

    ''' <summary>
    ''' Gets and Sets the Horizontal Scroll position of the control.
    ''' </summary>
    Public Property HScrollPos() As Integer
        Get
            Return Me.GetScrollPos(Me.Handle, SBOrientation.SB_HORZ)

        End Get
        Set(ByVal value As Integer)
            Me.SetScrollPos(Me.Handle, SBOrientation.SB_HORZ, True)
        End Set
    End Property

    ''' <summary>
    ''' Gets and Sets the Vertical Scroll position of the control.
    ''' </summary>
    Public Property VScrollPos() As Integer
        Get
            Return Me.GetScrollPos(Me.Handle, SBOrientation.SB_VERT)
        End Get
        Set(ByVal value As Integer)
            Me.SetScrollPos(Me.Handle, SBOrientation.SB_VERT, True)
        End Set
    End Property

    Protected Overrides Sub WndProc(ByRef m As Message)
        If m.Msg = WM_NCRBUTTONDOWN Then
            Me.sbi = New SCROLLBARINFO
            Me.sbi.CB_Size = Marshal.SizeOf(Me.sbi)
            GetScrollBarInfo(Me.Handle, OBJID_VSCROLL, Me.sbi)
            If Me.sbi.RC_ScrollBar.ToRectangle.Contains(MousePosition) Then
                Me.VertRightClicked = True
            Else
                Me.sbi.CB_Size = 0
                MyBase.WndProc(m)
            End If
        Else
            MyBase.WndProc(m)
            Exit Sub
        End If

        If Me.VertRightClicked Then
            Me.VertRightClicked = False
            Me.ContextMenuStrip1.Show(Me, Me.PointToClient(MousePosition))
        End If
    End Sub

    Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Me.ContextMenuStrip1 = New ContextMenuStrip(Me.components)
        Me.ToolStripMenuItem1 = New ToolStripMenuItem()
        Me.ToolStripMenuItem2 = New ToolStripMenuItem()
        Me.ToolStripMenuItem3 = New ToolStripMenuItem()
        Me.ToolStripSeparator1 = New ToolStripSeparator()
        Me.ContextMenuStrip1.SuspendLayout()
        Me.SuspendLayout()
        '
        'ContextMenuStrip1
        '
        Me.ContextMenuStrip1.Items.AddRange(New ToolStripItem() {Me.ToolStripMenuItem1, Me.ToolStripSeparator1, Me.ToolStripMenuItem2, Me.ToolStripMenuItem3})
        Me.ContextMenuStrip1.Name = "ContextMenuStrip1"
        Me.ContextMenuStrip1.Size = New System.Drawing.Size(147, 76)
        '
        'ToolStripMenuItem1
        '
        Me.ToolStripMenuItem1.Name = "ToolStripMenuItem1"
        Me.ToolStripMenuItem1.Size = New System.Drawing.Size(146, 22)
        Me.ToolStripMenuItem1.Text = "Scroll Here"
        '
        'ToolStripMenuItem2
        '
        Me.ToolStripMenuItem2.Name = "ToolStripMenuItem2"
        Me.ToolStripMenuItem2.Size = New System.Drawing.Size(146, 22)
        Me.ToolStripMenuItem2.Text = "Scroll Top"
        '
        'ToolStripMenuItem3
        '
        Me.ToolStripMenuItem3.Name = "ToolStripMenuItem3"
        Me.ToolStripMenuItem3.Size = New System.Drawing.Size(146, 22)
        Me.ToolStripMenuItem3.Text = "Scroll Bottom"
        '
        'ToolStripSeparator1
        '
        Me.ToolStripSeparator1.Name = "ToolStripSeparator1"
        Me.ToolStripSeparator1.Size = New System.Drawing.Size(143, 6)
        Me.ContextMenuStrip1.ResumeLayout(False)
        Me.ResumeLayout(False)
    End Sub

    Private Sub ToolStripMenuItem1_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem1.Click
        With Me.sbi
            Dim SB_Size As Integer = .RC_ScrollBar.Bottom - .RC_ScrollBar.Top
            Dim Percent As Double = 1 - (SB_Size - Me.PointToClient(MousePosition).Y) / SB_Size
            Dim DesiredLine As Integer = CInt(Me.Lines.Count * Percent)
            Me.[Select](Me.GetFirstCharIndexFromLine(DesiredLine), 0)
            Me.ScrollToCaret()
        End With
    End Sub

    Private Sub ToolStripMenuItem2_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem2.Click
        Me.SelectionStart = 0
    End Sub

    Private Sub ToolStripMenuItem3_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem3.Click
        Me.SelectionStart = Me.Text.Length

    End Sub
End Class
