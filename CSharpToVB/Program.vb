' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

#If NETCOREAPP3_1 Then
Friend Module Program

    <STAThread()>
    Friend Sub main(args As String())
        Application.SetHighDpiMode(HighDpiMode.PerMonitorV2)
        Application.EnableVisualStyles()
        Application.SetCompatibleTextRenderingDefault(False)
        Using MyApp As New My.MyApplication
            MyApp.Run(args)
        End Using
    End Sub

End Module
#End If
