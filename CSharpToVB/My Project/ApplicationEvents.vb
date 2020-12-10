' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports Microsoft.VisualBasic.ApplicationServices

Imports Microsoft.VisualBasic.Devices

Namespace My

    ' The following events are available for MyApplication:
    ' Startup: Raised when the application starts, before the startup form is created.
    ' Shutdown: Raised after all application forms are closed.  This event is not raised if the application terminates abnormally.
    ' UnhandledException: Raised if the application encounters an unhandled exception.
    ' StartupNextInstance: Raised when launching a single-instance application and the application is already active.
    ' NetworkAvailabilityChanged: Raised when the network connection is connected or disconnected.

    Partial Friend Class MyApplication

        Private Sub MyApplication_NetworkAvailabilityChanged(sender As Object, e As NetworkAvailableEventArgs) Handles Me.NetworkAvailabilityChanged
            ' My.Forms.Form1.SetConnectionStatus(e.IsNetworkAvailable)
        End Sub

        Private Sub MyApplication_Shutdown(sender As Object, e As EventArgs) Handles Me.Shutdown
            ' My.Application.Log.WriteEntry("Application Shut Down.")
        End Sub

        Private Sub MyApplication_Startup(sender As Object, e As StartupEventArgs) Handles Me.Startup
            If Me.SplashScreen.InvokeRequired Then
                Call Me.SplashScreen.Invoke(New StartupEventHandler(AddressOf Me.MyApplication_Startup), sender, e)
                Exit Sub
            End If

            ' Get the splash screen.
            CType(Me.SplashScreen, SplashScreen1).UserName.Text = "Current user: " & User.Name
        End Sub
        Private Sub MyApplication_StartupNextInstance(sender As Object, e As StartupNextInstanceEventArgs) Handles Me.StartupNextInstance
            Stop
        End Sub

        Private Sub MyApplication_UnhandledException(sender As Object, e As UnhandledExceptionEventArgs) Handles Me.UnhandledException
            Stop
            ' My.Application.Log.WriteException(e.Exception, TraceEventType.Critical, "Unhandled Exception.")
        End Sub

    End Class

End Namespace
