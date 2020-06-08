' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

<ComponentModel.EditorBrowsable(ComponentModel.EditorBrowsableState.Never)>
Public NotInheritable Class NativeTypes

#Disable Warning CA1707 ' Remove the underscore
#Disable Warning IDE0049 ' Simplify Names
    Public Const OBJID_VSCROLL As Long = &HFFFFFFFB

    Public Const WM_MOUSELEAVE As Int32 = &H2A3

    Public Const WM_MOUSEMOVE As Int32 = &H200

    Public Const WM_NCRBUTTONDOWN As Integer = &HA4
#Enable Warning IDE0049 ' Simplify Names
#Enable Warning CA1707 ' Remove the underscore

End Class
