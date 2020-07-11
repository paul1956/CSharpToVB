' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
' See the LICENSE file in the project root for more information.

Imports Microsoft.VisualBasic.ApplicationServices
Imports AppFramework = System.Windows.Forms.Application

Namespace My

    ' The following events are available for MyApplication:
    Partial Friend Class MyApplication

        ' Startup: Raised when the application starts, before the startup form is created.
        Private Sub MyApplication_Startup(sender As Object, e As StartupEventArgs) Handles Me.Startup
            AppFramework.SetHighDpiMode(HighDpiMode.SystemAware)
            ' Get the splash screen.
            'Dim splash As SplashScreen1 = CType(My.Application.SplashScreen, SplashScreen1)
            ' Display current status information.
            'splash.Status = "Current user: " & My.User.Name
        End Sub

        ' Shutdown: Raised after all application forms are closed.  This event is not raised if the application terminates abnormally.
        Private Sub MyApplication_Shutdown(sender As Object, e As EventArgs) Handles Me.Shutdown
            'My.Application.Log.WriteEntry("Application Shut Down.")
        End Sub

        ' UnhandledException: Raised if the application encounters an unhandled exception.
        Private Sub MyApplication_UnhandledException(sender As Object, e As UnhandledExceptionEventArgs) Handles Me.UnhandledException
            'My.Application.Log.WriteException(e.Exception, TraceEventType.Critical, "Unhandled Exception.")
        End Sub

        ' StartupNextInstance: Raised when launching a single-instance application and the application is already active. 
        Public Sub MyApplication_StartupNextInstance(sender As Object, e As StartupNextInstanceEventArgs) Handles Me.StartupNextInstance
            Stop
        End Sub

        ' NetworkAvailabilityChanged: Raised when the network connection is connected or disconnected.
        'Private Sub MyApplication_NetworkAvailabilityChanged(sender As Object, e As Devices.NetworkAvailableEventArgs) Handles Me.NetworkAvailabilityChanged
        '    'My.Forms.Form1.SetConnectionStatus(e.IsNetworkAvailable)
        'End Sub

    End Class

End Namespace
