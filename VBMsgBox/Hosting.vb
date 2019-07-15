' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

Imports System
Imports System.Windows.Forms

<ComponentModel.EditorBrowsable(ComponentModel.EditorBrowsableState.Never)>
Public Interface IVbHost
    Function GetParentWindow() As IWin32Window
    Function GetWindowTitle() As String
End Interface

<ComponentModel.EditorBrowsable(ComponentModel.EditorBrowsableState.Never)>
    Public NotInheritable Class HostServices

#Disable Warning IDE0032 ' Use auto property
        Private Shared m_host As IVbHost
#Enable Warning IDE0032 ' Use auto property

        Public Shared Property VBHost() As IVbHost
            Get
                Return m_host
            End Get

            Set(ByVal Value As IVbHost)
                m_host = Value
            End Set
        End Property

    End Class
