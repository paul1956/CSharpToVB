' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports CSharpToVBApp.Microsoft.VisualBasic.ApplicationServices

Namespace My
    Partial Public Module Application
        Private s_info As AssemblyInfo

        Public ReadOnly Property Info As AssemblyInfo
            Get
                If s_info Is Nothing Then
                    s_info = New AssemblyInfo(GetType(Form1).Assembly)
                End If
                Return s_info
            End Get
        End Property

    End Module

End Namespace
