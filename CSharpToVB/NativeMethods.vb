Option Explicit On
Option Infer Off
Option Strict On

Imports System.Runtime.CompilerServices
Imports System.Runtime.InteropServices

Friend Module NativeMethods

#Region "Constants"

    <Flags>
    Public Enum FILEOPENDIALOGOPTIONS As UInteger
        FOS_OVERWRITEPROMPT = &H2
        FOS_STRICTFILETYPES = &H4
        FOS_NOCHANGEDIR = &H8
        FOS_PICKFOLDERS = &H20
        FOS_FORCEFILESYSTEM = &H40
        FOS_ALLNONSTORAGEITEMS = &H80
        FOS_NOVALIDATE = &H100
        FOS_ALLOWMULTISELECT = &H200
        FOS_PATHMUSTEXIST = &H800
        FOS_FILEMUSTEXIST = &H1000
        FOS_CREATEPROMPT = &H2000
        FOS_SHAREAWARE = &H4000
        FOS_NOREADONLYRETURN = &H8000
        FOS_NOTESTFILECREATE = &H10000
        FOS_HIDEMRUPLACES = &H20000
        FOS_HIDEPINNEDPLACES = &H40000
        FOS_NODEREFERENCELINKS = &H100000
        FOS_DONTADDTORECENT = &H2000000
        FOS_FORCESHOWHIDDEN = &H10000000
        FOS_DEFAULTNOMINIMODE = &H20000000
        FOS_FORCEPREVIEWPANEON = &H40000000
        FOS_SUPPORTSTREAMABLEITEMS = &H80000000UI
    End Enum

    Friend Enum HRESULT As Long
        S_FALSE = &H1
        S_OK = &H0
        E_INVALIDARG = &H80070057UI
        E_OUTOFMEMORY = &H8007000EUI
    End Enum

#End Region

#Region "COM"

    <ComImport(), ClassInterface(ClassInterfaceType.None), TypeLibType(TypeLibTypeFlags.FCanCreate), Guid(CLSIDGuid.FileOpenDialog)>
    Friend Class FileOpenDialogRCW
    End Class

    <ComImport(), Guid(ComGuids.IFileDialog), InterfaceType(ComInterfaceType.InterfaceIsIUnknown)>
    Friend Interface IFileDialog

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        <PreserveSig()>
        Function Show(<[In](), [Optional]()> ByVal hwndOwner As IntPtr) As UInteger

        'IModalWindow
        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        Function SetFileTypes(<[In]()> ByVal cFileTypes As UInteger, <[In](), MarshalAs(UnmanagedType.LPArray)> ByVal rgFilterSpec As IntPtr) As UInteger

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        Function SetFileTypeIndex(<[In]()> ByVal iFileType As UInteger) As UInteger

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        Function GetFileTypeIndex(ByRef piFileType As UInteger) As UInteger

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        Function Advise(<[In](), MarshalAs(UnmanagedType.[Interface])> ByVal pfde As IntPtr, ByRef pdwCookie As UInteger) As UInteger

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        Function Unadvise(<[In]()> ByVal dwCookie As UInteger) As UInteger

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        Function SetOptions(<[In]()> ByVal fos As FILEOPENDIALOGOPTIONS) As UInteger

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        Function GetOptions(ByRef fos As FILEOPENDIALOGOPTIONS) As UInteger

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        Sub SetDefaultFolder(<[In](), MarshalAs(UnmanagedType.[Interface])> ByVal psi As IShellItem)

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        Function SetFolder(<[In](), MarshalAs(UnmanagedType.[Interface])> ByVal psi As IShellItem) As UInteger

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        Function GetFolder(<MarshalAs(UnmanagedType.[Interface])> ByRef ppsi As IShellItem) As UInteger

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        Function GetCurrentSelection(<MarshalAs(UnmanagedType.[Interface])> ByRef ppsi As IShellItem) As UInteger

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        Function SetFileName(<[In](), MarshalAs(UnmanagedType.LPWStr)> ByVal pszName As String) As UInteger

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        Function GetFileName(<MarshalAs(UnmanagedType.LPWStr)> ByRef pszName As String) As UInteger

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        Function SetTitle(<[In](), MarshalAs(UnmanagedType.LPWStr)> ByVal pszTitle As String) As UInteger

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        Function SetOkButtonLabel(<[In](), MarshalAs(UnmanagedType.LPWStr)> ByVal pszText As String) As UInteger

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        Function SetFileNameLabel(<[In](), MarshalAs(UnmanagedType.LPWStr)> ByVal pszLabel As String) As UInteger

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        Function GetResult(<MarshalAs(UnmanagedType.[Interface])> ByRef ppsi As IShellItem) As UInteger

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        Function AddPlace(<[In](), MarshalAs(UnmanagedType.[Interface])> ByVal psi As IShellItem, ByVal fdap As UInteger) As UInteger

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        Function SetDefaultExtension(<[In](), MarshalAs(UnmanagedType.LPWStr)> ByVal pszDefaultExtension As String) As UInteger

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        Function Close(<MarshalAs(UnmanagedType.[Error])> ByVal hr As UInteger) As UInteger

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        Function SetClientGuid(<[In]()> ByRef guid As Guid) As UInteger

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        Function ClearClientData() As UInteger

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        Function SetFilter(<MarshalAs(UnmanagedType.[Interface])> ByVal pFilter As IntPtr) As UInteger

    End Interface

    <ComImport(), Guid(ComGuids.IFileDialogEvents), InterfaceType(ComInterfaceType.InterfaceIsIUnknown)>
    Friend Interface IFileDialogEvents

        ' NOTE: some of these callbacks are cancelable - returning S_FALSE means that
        ' the dialog should not proceed (e.g. with closing, changing folder); to
        ' support this, we need to use the PreserveSig attribute to enable us to return
        ' the proper HRESULT
        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime), PreserveSig()>
        Function OnFileOk(<[In](), MarshalAs(UnmanagedType.[Interface])> pfd As IFileDialog) As HRESULT

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime), PreserveSig()>
        Function OnFolderChanging(<[In](), MarshalAs(UnmanagedType.[Interface])> pfd As IFileDialog, <[In](), MarshalAs(UnmanagedType.[Interface])> psiFolder As IShellItem) As HRESULT

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        Sub OnFolderChange(<[In](), MarshalAs(UnmanagedType.[Interface])> pfd As IFileDialog)

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        Sub OnSelectionChange(<[In](), MarshalAs(UnmanagedType.[Interface])> pfd As IFileDialog)

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        Sub OnShareViolation(<[In](), MarshalAs(UnmanagedType.[Interface])> pfd As IFileDialog, <[In](), MarshalAs(UnmanagedType.[Interface])> psi As IShellItem, pResponse As NativeMethods.FDE_SHAREVIOLATION_RESPONSE)

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        Sub OnTypeChange(<[In](), MarshalAs(UnmanagedType.[Interface])> pfd As IFileDialog)

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        Sub OnOverwrite(<[In](), MarshalAs(UnmanagedType.[Interface])> pfd As IFileDialog, <[In](), MarshalAs(UnmanagedType.[Interface])> psi As IShellItem, pResponse As NativeMethods.FDE_OVERWRITE_RESPONSE)

    End Interface

    <ComImport(), Guid(ComGuids.IShellItem), InterfaceType(ComInterfaceType.InterfaceIsIUnknown)>
    Friend Interface IShellItem

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        Function BindToHandler(<[In]()> ByVal pbc As IntPtr, <[In]()> ByRef rbhid As Guid, <[In]()> ByRef riid As Guid, <Out(), MarshalAs(UnmanagedType.[Interface])> ByRef ppvOut As IntPtr) As UInteger

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        Function GetParent(<MarshalAs(UnmanagedType.[Interface])> ByRef ppsi As IShellItem) As UInteger

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        Function GetDisplayName(<[In]()> ByVal sigdnName As UInteger, ByRef ppszName As IntPtr) As UInteger

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        Function GetAttributes(<[In]()> ByVal sfgaoMask As UInteger, ByRef psfgaoAttribs As UInteger) As UInteger

        <MethodImpl(MethodImplOptions.InternalCall, MethodCodeType:=MethodCodeType.Runtime)>
        Function Compare(<[In](), MarshalAs(UnmanagedType.[Interface])> ByVal psi As IShellItem, <[In]()> ByVal hint As UInteger, ByRef piOrder As Integer) As UInteger

    End Interface

#End Region

    <DllImport("shell32.dll", CharSet:=CharSet.Unicode, PreserveSig:=False)>
    Public Function SHCreateItemFromParsingName(
         <MarshalAs(UnmanagedType.LPWStr)> ByVal pszPath As String,
         ByVal pbc As IntPtr,
         <MarshalAs(UnmanagedType.LPStruct)> ByVal riid As Guid,
         <MarshalAs(UnmanagedType.Interface, IidParameterIndex:=2)> ByRef ppv As IShellItem) As Integer
    End Function

#Region "File Operations Definitions"

    <StructLayout(LayoutKind.Sequential, CharSet:=CharSet.Unicode, Pack:=4)>
    Friend Structure COMDLG_FILTERSPEC

        <MarshalAs(UnmanagedType.LPWStr)>
        Friend pszName As String

        <MarshalAs(UnmanagedType.LPWStr)>
        Friend pszSpec As String

    End Structure

    Friend Enum FDAP
        FDAP_BOTTOM = &H0
        FDAP_TOP = &H1
    End Enum

    Friend Enum FDE_SHAREVIOLATION_RESPONSE
        FDESVR_DEFAULT = &H0
        FDESVR_ACCEPT = &H1
        FDESVR_REFUSE = &H2
    End Enum

    Friend Enum FDE_OVERWRITE_RESPONSE
        FDEOR_DEFAULT = &H0
        FDEOR_ACCEPT = &H1
        FDEOR_REFUSE = &H2
    End Enum

    Friend Enum SIATTRIBFLAGS
        SIATTRIBFLAGS_AND = &H1

        ' if multiple items and the attributes together.
        SIATTRIBFLAGS_OR = &H2

        ' if multiple items or the attributes together.
        SIATTRIBFLAGS_APPCOMPAT = &H3

        ' Call GetAttributes directly on the ShellFolder for multiple attributes
    End Enum

    Friend Enum SIGDN As UInteger
        SIGDN_NORMALDISPLAY = &H0

        ' SHGDN_NORMAL
        SIGDN_PARENTRELATIVEPARSING = &H80018001UI

        ' SHGDN_INFOLDER | SHGDN_FORPARSING
        SIGDN_DESKTOPABSOLUTEPARSING = &H80028000UI

        ' SHGDN_FORPARSING
        SIGDN_PARENTRELATIVEEDITING = &H80031001UI

        ' SHGDN_INFOLDER | SHGDN_FOREDITING
        SIGDN_DESKTOPABSOLUTEEDITING = &H8004C000UI

        ' SHGDN_FORPARSING | SHGDN_FORADDRESSBAR
        SIGDN_FILESYSPATH = &H80058000UI

        ' SHGDN_FORPARSING
        SIGDN_URL = &H80068000UI

        ' SHGDN_FORPARSING
        SIGDN_PARENTRELATIVEFORADDRESSBAR = &H8007C001UI

        ' SHGDN_INFOLDER | SHGDN_FORPARSING | SHGDN_FORADDRESSBAR
        SIGDN_PARENTRELATIVE = &H80080001UI

        ' SHGDN_INFOLDER
    End Enum

    Friend Enum CDCONTROLSTATE
        CDCS_INACTIVE = &H0
        CDCS_ENABLED = &H1
        CDCS_VISIBLE = &H2
    End Enum

#End Region

End Module