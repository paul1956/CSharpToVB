' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.IO
Imports System.Net
Imports Microsoft.VisualBasic.ApplicationServices
Imports Microsoft.Win32
Imports SupportClasses

Friend Module BrowserUtilities

    Private Function IsNewerVersion(gitHubVersions() As String, appVersion As Version, converterVersion As Version) As Boolean
        If String.IsNullOrWhiteSpace(gitHubVersions(0)) OrElse Version.Parse(gitHubVersions(0)) > Version.Parse(appVersion.ToString) Then
            Return True
        End If
        If gitHubVersions.Length = 1 Then
            Return False
        End If
        Return Version.Parse(gitHubVersions(1)) > Version.Parse(converterVersion.ToString)
    End Function

    Private Sub LaunchBrowser(url As String)
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
            Dim browserPath As String = "%ProgramFiles(x86)%\Internet Explorer\iExplore.exe"
            If progIdValue.Contains("chrome", StringComparison.OrdinalIgnoreCase) Then
                browserPath = "%ProgramFiles(x86)%\Google\Chrome\Application\chrome.exe"
            ElseIf progIdValue.Contains("Firefox", StringComparison.OrdinalIgnoreCase) Then
                browserPath = "C:\Program Files\Mozilla Firefox\Firefox.exe"
            ElseIf progIdValue.Contains("msEdgeHtm", StringComparison.OrdinalIgnoreCase) Then
                browserPath = "%ProgramFiles(x86)%\Microsoft\Edge\Application\msEdge.exe"
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

    Friend Sub CheckForUpdates(mainForm As Form1, reportResults As Boolean)
        Try
            Dim request As WebRequest = WebRequest.Create($"{Form1.ProjectGitHubURL}blob/master/ReadMe.MD")
            request.Timeout = 4000
            Dim response As WebResponse = request.GetResponse()
            Using reader As New StreamReader(response.GetResponseStream())
                Dim line As String
                Dim index As Integer = -1
                Do
                    line = reader.ReadLine
                    If line Is Nothing Then
                        Exit Do
                    End If
                    If line.Contains("What's New in this release", StringComparison.Ordinal) Then
                        Exit Do
                    End If
                Loop
                Do
                    line = reader.ReadLine
                    If line Is Nothing Then
                        Exit Do
                    End If
                    index = line.IndexOf("New in ", StringComparison.OrdinalIgnoreCase)
                    Exit Do
                Loop
                If index < 0 Then
                    Exit Sub
                End If
                Dim versionStr As String = line.Substring(index + "New In ".Length)

                index = versionStr.IndexOf("<"c)
                If index > 0 Then
                    versionStr = versionStr.Substring(0, index)
                End If
                Dim gitHubVersion() As String = versionStr.Split("/")
                Dim codeConverterInfo As New AssemblyInfo(GetType(CodeWithOptions).Assembly)
                If IsNewerVersion(gitHubVersion, My.Application.Info.Version, codeConverterInfo.Version) Then
                    mainForm.StatusStripUpdateAvailable.Visible = True
                    If reportResults Then
                        If MsgBox("There is a newer version available, do you want to install now?", MsgBoxStyle.YesNo, "Updates Available") = MsgBoxResult.Yes Then
                            OpenUrlInBrowser(Form1.ProjectGitHubURL)
                        End If
                    End If
                Else
                    mainForm.StatusStripUpdateAvailable.Visible = False
                    If reportResults Then
                        MsgBox("You are running latest version", MsgBoxStyle.OkOnly, "No Updates Available")
                    End If
                End If
            End Using
        Catch ex As Exception
            If reportResults Then
                MsgBox("Failed while checking for new  version: " + ex.Message, MsgBoxStyle.Information, "Version Check Failed")
            End If
        End Try
    End Sub

    Friend Sub OpenUrlInBrowser(webAddress As String)
        Try
            'Devices.Mouse.OverrideCursor = Cursors.AppStarting
            Form1.Cursor = Cursors.AppStarting
            LaunchBrowser(webAddress)
        Catch ex As Exception
            Throw
        Finally
            Form1.Cursor = Cursors.AppStarting
            'Devices.Mouse.OverrideCursor = Nothing
        End Try
    End Sub

End Module
