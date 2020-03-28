' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports CSharpToVBApp.CSharpToVBApp.My_Project

Namespace My

    <HideModuleName(),
     DebuggerNonUserCode(),
     Runtime.CompilerServices.CompilerGenerated()>
    Friend Module MySettingsProperty

        <ComponentModel.Design.HelpKeyword("My.Settings")>
        Friend ReadOnly Property Settings() As Settings
            Get
                Return Settings.Default
            End Get
        End Property

    End Module
End Namespace
