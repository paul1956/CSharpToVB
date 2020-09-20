' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.InteropServices

<ComVisible(False)>
Friend Module NativeMethods

    <DllImport("user32.dll")>
    Public Function LockWindowUpdate(hWndLock As IntPtr) As Boolean
    End Function

#Region "Scroll Bar Support"

    Public Enum SBOrientation As Integer
        HORZ = &H0
        VERT = &H1
        CTL = &H2
        BOTH = &H3
    End Enum

    <Flags>
    Public Enum ScrollInfoMasks As Integer
        RANGE = &H1
        PAGE = &H2
        POS = &H4
        DISABLENOSCROLL = &H8
        TRACKPOS = &H10
        ALL = RANGE Or PAGE Or POS Or TRACKPOS
    End Enum

    <DllImport("user32.dll", EntryPoint:=NameOf(GetScrollBarInfo))>
    Friend Function GetScrollBarInfo(hwnd As IntPtr,
                                     idObject As Integer,
                                     ByRef psbi As SCROLLBARINFO
                                     ) As <MarshalAs(UnmanagedType.Bool)> Boolean
    End Function

    <DllImport("user32.dll", SetLastError:=True)>
    Friend Function GetScrollInfo(
                                 hWnd As IntPtr,
                                 <MarshalAs(UnmanagedType.I4)> fnBar As SBOrientation,
                                 ByRef lpsi As SCROLLINFO
                                 ) As Integer
    End Function

    <DllImport("user32.dll", SetLastError:=True, ThrowOnUnmappableChar:=True, CharSet:=CharSet.Auto)>
    Friend Function SetScrollInfo(
                                 hWnd As IntPtr,
                                 <MarshalAs(UnmanagedType.I4)> nBar As SBOrientation,
                                 <MarshalAs(UnmanagedType.Struct)> ByRef lpsi As SCROLLINFO,
                                 <MarshalAs(UnmanagedType.Bool)> bRepaint As Boolean
                                 ) As Integer
    End Function

    'This function queries or sets system-wide parameters, and updates the user profile during the process.
    <DllImport("user32", EntryPoint:=NameOf(SystemParametersInfo), CharSet:=CharSet.Unicode, SetLastError:=True)>
    Public Function SystemParametersInfo(intAction As Integer,
                                         intParam As Integer,
                                         strParam As String,
                                         intWinIniFlag As Integer
                                        ) As Integer
        ' returns non-zero value if function succeeds
    End Function

    <StructLayout(LayoutKind.Sequential)>
    Friend Structure RECT
        Public Left, Top, Right, Bottom As Integer

        Public Function ToRectangle() As Rectangle
            Return New Rectangle(Left, Top, Right - Left, Bottom - Top)
        End Function

    End Structure

    <Serializable, StructLayout(LayoutKind.Sequential)>
    Friend Structure SCROLLINFO
        Public CB_Size As UInteger
        <MarshalAs(UnmanagedType.U4)> Public F_Mask As ScrollInfoMasks
        Public N_Min As Integer
        Public N_Max As Integer
        Public N_Page As UInteger
        Public N_Pos As Integer
        Public N_TrackPos As Integer
    End Structure

    <StructLayout(LayoutKind.Sequential)>
    Public Structure SCROLLBARINFO
        Public CB_Size As Integer
        Public RC_ScrollBar As RECT
        Public DXY_LineButton As Integer
        Public XY_ThumbTop As Integer
        Public XY_ThumbBottom As Integer
        Public Reserved As Integer

        <MarshalAs(UnmanagedType.ByValArray, SizeConst:=6, ArraySubType:=UnmanagedType.U4)>
        Public RgState() As Integer

    End Structure

#End Region

End Module
