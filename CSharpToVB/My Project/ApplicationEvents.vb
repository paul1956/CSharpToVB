' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports Microsoft.VisualBasic.ApplicationServices

#If NET5_0 Then
Imports Microsoft.VisualBasic.Devices
#End If


Namespace My
    ' The following events are available for MyApplication:
    ' Startup: Raised when the application starts, before the startup form is created.
    ' Shutdown: Raised after all application forms are closed.  This event is not raised if the application terminates abnormally.
    ' UnhandledException: Raised if the application encounters an unhandled exception.
    ' StartupNextInstance: Raised when launching a single-instance application and the application is already active.
    ' NetworkAvailabilityChanged: Raised when the network connection is connected or disconnected.
    Partial Friend Class MyApplication

#If NET5_0 Then
        Private Sub MyApplication_NetworkAvailabilityChanged(sender As Object, e As NetworkAvailableEventArgs) Handles Me.NetworkAvailabilityChanged
            ' My.Forms.Form1.SetConnectionStatus(e.IsNetworkAvailable)
        End Sub
#End If

        Private Sub MyApplication_Shutdown(sender As Object, e As EventArgs) Handles Me.Shutdown
            ' My.Application.Log.WriteEntry("Application Shut Down.")
        End Sub

        Private Sub MyApplication_Startup(sender As Object, e As StartupEventArgs) Handles Me.Startup
#If NET5_0 Then
            ' Get the splash screen.
            CType(SplashScreen, SplashScreen1).UserName.Text = "Current user: " & MyApplication.User.Name
#End If
        End Sub

        Private Sub MyApplication_StartupNextInstance(sender As Object, e As StartupNextInstanceEventArgs) Handles Me.StartupNextInstance
            Stop
        End Sub

        Private Sub MyApplication_UnhandledException(sender As Object, e As UnhandledExceptionEventArgs) Handles Me.UnhandledException
            ' My.Application.Log.WriteException(e.Exception, TraceEventType.Critical, "Unhandled Exception.")
        End Sub
    End Class
End Namespace
