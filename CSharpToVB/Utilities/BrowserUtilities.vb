' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports Microsoft.Win32

Friend Module BrowserUtilities

    Friend Sub launchBrowser(url As String)
        Using userChoiceKey As RegistryKey = Registry.CurrentUser.OpenSubKey("Software\Microsoft\Windows\Shell\Associations\UrlAssociations\http\UserChoice")
            If userChoiceKey Is Nothing Then
                Exit Sub
            End If
            Dim progIdObject As Object = userChoiceKey.GetValue("Progid")
            If progIdObject Is Nothing Then
                Exit Sub
            End If
            Dim progIdValue As String = CStr(progIdObject)
            If progIdValue Is Nothing Then
                Exit Sub
            End If
            Dim msgResult As MsgBoxResult = MsgBoxResult.Ok
            Dim browserPath As String = "%ProgramFiles(x86)%\Internet Explorer\iexplore.exe"
            If progIdValue.Contains("chrome", StringComparison.OrdinalIgnoreCase) Then
                browserPath = "%ProgramFiles(x86)%\Google\Chrome\Application\chrome.exe"
            ElseIf progIdValue.Contains("firefox", StringComparison.OrdinalIgnoreCase) Then
                browserPath = "C:\Program Files\Mozilla Firefox\firefox.exe"
            ElseIf progIdValue.Contains("msedgehtm", StringComparison.OrdinalIgnoreCase) Then
                browserPath = "%ProgramFiles(x86)%\Microsoft\Edge\Application\msedge.exe"
                'ElseIf progIdValue.Contains("opera", StringComparison.OrdinalIgnoreCase) Then
                '    browserPath = "opera.exe"
            Else
                msgResult = MsgBox($"Your default browser {progIdValue} is not supported, iExplorer will be used if you select OK!, please enter an issue with browser 'ProgId' and full path",
                                   MsgBoxStyle.OkCancel Or MsgBoxStyle.Exclamation Or MsgBoxStyle.MsgBoxSetForeground)
            End If
            If msgResult = MsgBoxResult.Ok Then
                Dim info As New ProcessStartInfo(Environment.ExpandEnvironmentVariables(browserPath), url)
                Process.Start(info)
            End If
        End Using
    End Sub

End Module
