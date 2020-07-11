Option Explicit On
Option Infer Off
Option Strict On
Imports System.Runtime.CompilerServices
Imports CSharpToVBApp.Microsoft.VisualBasic.CompilerServices.NativeMethods

Public Module ControlExtensions
    <Extension()>
    Public Sub Suspend(Ctrl As Control)
        LockWindowUpdate(Ctrl.Handle)
    End Sub

    <Extension()>
    Public Sub [Resume](_1 As Control)
        LockWindowUpdate(IntPtr.Zero)
    End Sub

End Module
