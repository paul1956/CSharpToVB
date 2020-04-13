' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Option Strict On
Option Explicit On

Namespace Microsoft.VisualBasic.ApplicationServices

    ''' <summary>
    ''' Abstract class that defines the application Startup/Shutdown model for VB 
    ''' Windows Applications such as console, winforms, dll, service.
    ''' </summary>
    Public Class ApplicationBase

        Public Sub New()
        End Sub

        ''' <summary>
        ''' Returns the value of the specified environment variable.
        ''' </summary>
        ''' <param name="Name">A String containing the name of the environment variable.</param>
        ''' <returns>A string containing the value of the environment variable.</returns>
        ''' <exception cref="System.ArgumentNullException">if name is Nothing.</exception>
        ''' <exception cref="System.ArgumentException">if the specified environment variable does not exist.</exception>
        Public Overridable Function GetEnvironmentVariable(ByVal name As String) As String
            Return Environment.GetEnvironmentVariable(name)
        End Function
    End Class 'ApplicationBase
End Namespace
