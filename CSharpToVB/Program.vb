Friend Module Program

    <STAThread()>
    Friend Sub main(args As String())
        Application.SetHighDpiMode(HighDpiMode.PerMonitorV2)
        Application.EnableVisualStyles()
        Application.SetCompatibleTextRenderingDefault(False)
#If Not NET5_0 Then
        Using MyApp As New My.MyApplication
            MyApp.Run(args)
        End Using
#Else
        Application.Run(New Form1)
#End If
    End Sub

End Module
