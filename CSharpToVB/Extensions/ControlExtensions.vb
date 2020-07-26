' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports CSharpToVBApp.Microsoft.VisualBasic.CompilerServices.NativeMethods

Public Module ControlExtensions

    <Extension()>
    Friend Sub Suspend(Ctrl As Control)
        LockWindowUpdate(Ctrl.Handle)
    End Sub

    <Extension()>
    Friend Sub [Resume](_1 As Control)
        LockWindowUpdate(IntPtr.Zero)
    End Sub

End Module
