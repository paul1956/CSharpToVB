' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.IO
Imports System.Text

Public Module ConvertSolutionFileUtilities

    Private ReadOnly s_guidMappingDictionary As New Dictionary(Of String, String) From {
            {"8BB2217D-0F2D-49D1-97BC-3654ED321F3B", ""}, ' ASP.NET 5
            {"603C0E0B-DB56-11DC-BE95-000D561079B0", ""}, ' ASP.NET MVC 1
            {"F85E285D-A4E0-4152-9332-AB1D724D3325", ""}, ' ASP.NET MVC 2, Model-View-Controller v2 (MVC 2)
            {"E53F8FEA-EAE0-44A6-8774-FFD645390401", ""}, ' ASP.NET MVC 3, Model-View-Controller v3 (MVC 3)
            {"E3E379DF-F4C6-4180-9B81-6769533ABE47", ""}, ' ASP.NET MVC 4, Model-View-Controller v4 (MVC 4)
            {"349C5851-65DF-11DA-9384-00065B846F21", ""}, ' ASP.NET MVC 5, Model-View-Controller v5 (MVC 5), Web Application
            {"9A19103F-16F7-4668-BE54-9A1E7A4F7556", "778DAE3C-4631-46EA-AA77-85C1314464D9"}, ' C# (forces use of SDK project system
            {"FAE04EC0-301F-11D3-BF4B-00C04F79EFBC", "F184B08F-C81C-45F6-A57F-5ABD9991F28F"}, ' C#, Windows (C#)
            {"8BC9CEB8-8B4A-11D0-8D11-00A0C91BC942", ""}, ' C++, Windows (Visual C++)
            {"13B669BE-BB05-4DDF-9536-439F39A36129", ""}, ' CPS barebones project
            {"A9ACE9BB-CECE-4E62-9AA4-C7E7C5BD2124", ""}, ' Database
            {"4F174C21-8C12-11D0-8340-0000F80270F8", ""}, ' Database (other project types)
            {"C8D11400-126E-41CD-887F-60BD40844F9E", ""}, ' Database
            {"3EA9E505-35AC-4774-B492-AD1749C4943A", ""}, ' Deployment Cab
            {"06A35CCD-C46D-44D5-987B-CF40FF872267", ""}, ' Deployment Merge Module
            {"978C614F-708E-4E1A-B201-565925725DBA", ""}, ' Deployment Setup
            {"AB322303-2255-48EF-A496-5904EB18DA55", ""}, ' Deployment Smart Device Cab
            {"F135691A-BF7E-435D-8960-F99683D2D49C", ""}, ' Distributed System
            {"BF6F8E12-879D-49E7-ADF0-5503146B24B8", "F184B08F-C81C-45F6-A57F-5ABD9991F28F"}, ' Dynamics 2012 AX C# in AOT
            {"82B43B9B-A64C-4715-B499-D71E9CA2BD60", ""}, ' Extensibility
            {"F2A71F9B-5D33-465A-A702-920D77279786", ""}, ' F#
            {"6EC3EE1D-3C4E-46DD-8F32-0CC8E7565705", ""}, ' F# (forces use of SDK project system)
            {"95DFC527-4DC1-495E-97D7-E94EE1F7140D", ""}, ' IL project
            {"E6FDF86B-F3D1-11D4-8576-0002A516ECE8", ""}, ' J#
            {"262852C6-CD72-467D-83FE-5EEB1973A190", ""}, ' JScript
            {"20D4826A-C6FA-45DB-90F4-C717570B9F32", "CB4CE8C6-1BDB-4DC7-A4D3-65A1999772F8"}, ' Legacy (2003) Smart Device (C#)
            {"CB4CE8C6-1BDB-4DC7-A4D3-65A1999772F8", "CB4CE8C6-1BDB-4DC7-A4D3-65A1999772F8"}, ' Legacy (2003) Smart Device (VB.NET)
            {"581633EB-B896-402F-8E60-36F3DA191C85", ""}, ' LightSwitch Project
            {"8BB0C5E8-0616-4F60-8E55-A43933E57E9C", ""}, ' LightSwitch
            {"B69E3092-B931-443C-ABE7-7E7B65F2A37F", ""}, ' Micro Framework
            {"C1CDDADD-2546-481F-9697-4EA41081F2FC", ""}, ' Office/SharePoint App
            {"786C830F-07A1-408B-BD7F-6EE04809D6DB", ""}, ' Portable Class Library
            {"66A26720-8FB5-11D2-AA7E-00C04F688DDE", ""}, ' Project Folders
            {"D954291E-2A0B-460D-934E-DC6B0785DB48", ""}, ' Shared Project
            {"593B0543-81F6-4436-BA1E-4747859CAAE2", "EC05E597-79D4-47f3-ADA0-324C4F7C7484"}, ' SharePoint (C#)
            {"EC05E597-79D4-47f3-ADA0-324C4F7C7484", "EC05E597-79D4-47f3-ADA0-324C4F7C7484"}, ' SharePoint (VB.NET)
            {"F8810EC1-6754-47FC-A15F-DFABD2E3FA90", ""}, ' SharePoint Workflow
            {"A1591282-1198-4647-A2B1-27E5FF5F6F3B", ""}, ' Silverlight
            {"2150E333-8FDC-42A3-9474-1A3956D46DE8", ""}, ' Solution Folder
            {"4D628B5B-2FBC-4AA6-8C16-197242AEB884", "68B1623D-7FB9-47D8-8664-7ECEA3297D4F"}, ' Smart Device (C#)
            {"68B1623D-7FB9-47D8-8664-7ECEA3297D4F", "68B1623D-7FB9-47D8-8664-7ECEA3297D4F"}, ' Smart Device (VB.NET)
            {"3AC096D0-A1C2-E12C-1390-A8335801FDAB", ""}, ' Test
            {"A5A43C5B-DE2A-4C0C-9213-0A381AF9435A", ""}, ' Universal Windows Class Library
            {"F184B08F-C81C-45F6-A57F-5ABD9991F28F", "F184B08F-C81C-45F6-A57F-5ABD9991F28F"}, ' VB.NET, Windows (VB.NET)
            {"778DAE3C-4631-46EA-AA77-85C1314464D9", "778DAE3C-4631-46EA-AA77-85C1314464D9"}, ' VB.NET (forces use of SDK project system)
            {"C252FEB5-A946-4202-B1D4-9916A0590387", ""}, ' Visual Database Tools
            {"54435603-DBB4-11D2-8724-00A0C9A8B90C", ""}, ' Visual Studio 2015 Installer Project Extension
            {"A860303F-1F3F-4691-B57E-529FC101A107", ""}, ' Visual Studio Tools for Applications (VSTA)
            {"BAA0C2D2-18E2-41B9-852F-F413020CAA33", ""}, ' Visual Studio Tools for Office (VSTO)
            {"2CFEAB61-6A3B-4EB8-B523-560B4BEEF521", ""}, ' Web Deployment
            {"E24C65DC-7377-472B-9ABA-BC803B73C61A", ""}, ' Web Site
            {"3D9AD99F-2412-4246-B90B-4EAA41C64699", ""}, ' Windows Communication Foundation (WCF)
            {"76F1466A-8B6D-4E39-A767-685A06062A39", ""}, ' Windows Phone 8/8.1 Blank/Hub/Webview App
            {"C089C8C0-30E0-4E22-80C0-CE093F111A43", "DB03555F-0C8B-43BE-9FF9-57896B3C5E56"}, ' Windows Phone 8/8.1 App (C#)
            {"DB03555F-0C8B-43BE-9FF9-57896B3C5E56", "DB03555F-0C8B-43BE-9FF9-57896B3C5E56"}, ' Windows Phone 8/8.1 App (VB.NET)
            {"60DC8134-EBA5-43B8-BCC9-BB4BC16C2548", ""}, ' Windows Presentation Foundation (WPF)
            {"BC8A1FFA-BEE3-4634-8014-F334798102B3", ""}, ' Windows Store (Metro) Apps & Components
            {"14822709-B5A1-4724-98CA-57A101D1B079", "D59BE175-2ED0-4C54-BE3D-CDAA9F3214C8"}, ' Workflow (C#)
            {"D59BE175-2ED0-4C54-BE3D-CDAA9F3214C8", "D59BE175-2ED0-4C54-BE3D-CDAA9F3214C8"}, ' Workflow (VB.NET)
            {"32F31D43-81CC-4C15-9DE6-3FC5453562B6", ""}, ' Workflow Foundation
            {"EFBA0AD7-5A72-4C68-AF49-83D382785DCF", ""}, ' Xamarin.Android / Mono for Android
            {"6BC8ED88-2882-458C-8E55-DFD12B67127B", ""}, ' Xamarin.iOS / MonoTouch
            {"F5B4F3BC-B597-4E2B-B552-EF5D8A32436F", ""}, ' MonoTouch Binding
            {"6D335F3A-9D43-41b4-9D22-F6F17C4BE596", ""}, ' XNA (Windows)
            {"2DF5C3F4-5A5F-47a9-8E94-23B4456F55E2", ""}, ' XNA (XBox)
            {"D399B71A-8929-442a-A9AC-8BEC78BB2433", ""}  ' XNA (Zune)
            }

    Private ReadOnly s_headerLines As New List(Of String) From {
                "Microsoft Visual Studio Solution File, Format Version",
                "# Visual Studio Version",
                "VisualStudioVersion =",
                "MinimumVisualStudioVersion ="
                }

    Private Function GetGuid(CurrentLine As String, First As Boolean) As String
        Dim start As Integer
        Dim len As Integer
        If First Then
            start = CurrentLine.IndexOf("{", StringComparison.OrdinalIgnoreCase) + 1
            len = CurrentLine.IndexOf("}", StringComparison.OrdinalIgnoreCase) - start
        Else
            start = CurrentLine.LastIndexOf("{", StringComparison.OrdinalIgnoreCase) + 1
            len = CurrentLine.LastIndexOf("}", StringComparison.OrdinalIgnoreCase) - start
        End If
        If start > 0 Then
            Return CurrentLine.Substring(start, len).ToUpperInvariant
        End If
        Return String.Empty
    End Function

    Private Function GetNextTextLine(SR As StreamReader) As String
        While True
            Dim aLine As String = SR.ReadLine()
            If aLine Is Nothing Then
                Return Nothing
            End If
            If aLine.Length > 0 Then
                Return aLine
            End If
        End While
        Return Nothing
    End Function

    Public Sub ConvertSolutionFile(SolutionFilePath As String, saveSolutionRoot As String, ProjectsToBeAdded As List(Of String), Optional Testing As Boolean = False)
        If String.IsNullOrWhiteSpace(SolutionFilePath) Then
            Throw New ArgumentException($"'{NameOf(SolutionFilePath)}' cannot be null or whitespace", NameOf(SolutionFilePath))
        End If

        If String.IsNullOrEmpty(saveSolutionRoot) Then
            Throw New ArgumentException($"'{NameOf(saveSolutionRoot)}' cannot be null or empty", NameOf(saveSolutionRoot))
        End If

        If ProjectsToBeAdded Is Nothing Then
            Throw New ArgumentNullException(NameOf(ProjectsToBeAdded))
        End If

        If Not File.Exists(SolutionFilePath) Then
            Throw New IOException($"Destination directory file name missing in {NameOf(ConvertSolutionFile)}")
        End If

        Dim sb As New StringBuilder
        Using sr As New StreamReader(SolutionFilePath)
            ' Process File header
            Dim currentLine As String
            For Each headerLine As String In s_headerLines
                currentLine = GetNextTextLine(sr)
                If String.IsNullOrEmpty(currentLine) OrElse Not currentLine?.StartsWith(headerLine, StringComparison.Ordinal) Then
                    Throw New Exception($"Error: Invalid solution header line '{currentLine}' does not start with '{headerLine}', while converting '{SolutionFilePath}'")
                End If
                sb.AppendLine(currentLine)
            Next
            ' Process File body
            '   Process Projects
            Dim projectsGUIDMap As New Dictionary(Of String, String)
            currentLine = GetNextTextLine(sr)
            If currentLine Is Nothing Then
                Throw New IOException($"Premature EOF in solution file '{SolutionFilePath}'")
            End If
            While currentLine.StartsWith("Project(""{", StringComparison.Ordinal)
                Dim oldProjectTypeGUID As String = GetGuid(currentLine, First:=True)
                Dim newProjectTypeGuid As String = Nothing
                If Not s_guidMappingDictionary.TryGetValue(oldProjectTypeGUID, newProjectTypeGuid) Then
                    Throw New Exception($"Error: Unknown Project Type GUID {oldProjectTypeGUID}, while converting '{SolutionFilePath}'")
                End If
                Dim oldProjectGUID As String = GetGuid(currentLine, First:=False)
                Dim newProjectGuid As String
                Dim projectConverted As Boolean = newProjectTypeGuid.Length <> 0
                If projectConverted Then
                    newProjectGuid = Guid.NewGuid.ToString
                Else
                    newProjectTypeGuid = oldProjectTypeGUID
                    newProjectGuid = oldProjectGUID
                End If
                projectsGUIDMap.Add(oldProjectGUID, newProjectGuid)
                If projectConverted Then
                    sb.AppendLine(currentLine.Replace(oldProjectTypeGUID, newProjectTypeGuid, StringComparison.OrdinalIgnoreCase) _
                                             .Replace(oldProjectGUID, newProjectGuid, StringComparison.OrdinalIgnoreCase) _
                                             .Replace(".csproj", ".vbproj", StringComparison.OrdinalIgnoreCase))
                Else
                    sb.AppendLine(currentLine)
                End If
                currentLine = GetNextTextLine(sr)
                Select Case currentLine.Trim(" "c, CChar(vbTab))
                    Case "ProjectSection(SolutionItems) = preProject"
                        sb.AppendLine(currentLine)
                        currentLine = GetNextTextLine(sr)
                        Dim trimmedCurrentLine As String = currentLine.Trim(" "c, CChar(vbTab))
                        While currentLine.Length > 0 AndAlso trimmedCurrentLine <> "EndProjectSection"
                            Dim endIndex As Integer = trimmedCurrentLine.IndexOf(" = ", StringComparison.OrdinalIgnoreCase)
                            If Not Testing Then
                                Dim fileName As String = trimmedCurrentLine.Substring(0, endIndex)
                                Dim sourceFileFullPath As String = Path.Combine(Path.GetDirectoryName(SolutionFilePath), fileName)
                                If File.Exists(sourceFileFullPath) Then
                                    FileIO.FileSystem.CopyFile(sourceFileFullPath, Path.Combine(saveSolutionRoot, fileName), overwrite:=True)
                                End If
                            End If
                            sb.AppendLine(currentLine)
                            currentLine = GetNextTextLine(sr)
                            trimmedCurrentLine = currentLine.Trim(" "c, CChar(vbTab))
                        End While
                        sb.AppendLine(currentLine)
                        currentLine = GetNextTextLine(sr)
                    Case "ProjectSection(ProjectDependencies) = postProject"
                        sb.AppendLine(currentLine)
                        currentLine = GetNextTextLine(sr)
                        While currentLine.Length > 0 AndAlso currentLine.Trim(" "c, CChar(vbTab)) <> "EndProjectSection"
                            oldProjectTypeGUID = GetGuid(currentLine, First:=False)
                            If String.IsNullOrWhiteSpace(oldProjectTypeGUID) Then
                                Stop
                                sb.AppendLine(currentLine)
                            Else
                                sb.AppendLine(currentLine.Replace(oldProjectTypeGUID, projectsGUIDMap(oldProjectTypeGUID), StringComparison.OrdinalIgnoreCase))
                            End If
                            currentLine = GetNextTextLine(sr)
                        End While
                        sb.AppendLine(currentLine)
                        currentLine = GetNextTextLine(sr)
                    Case "EndProject"
                    Case Else
                        Throw New Exception($"Unknown Project Section Type: {currentLine.Trim(" "c, CChar(vbTab))}, while converting '{SolutionFilePath}'")
                End Select
                sb.AppendLine(currentLine)
                currentLine = GetNextTextLine(sr)
            End While
            For Each project As String In ProjectsToBeAdded
                sb.Append(project)
            Next
            '   Process Global
            If Not currentLine = "Global" Then
                Stop
                Throw New Exception($"""Global"" section missing, while converting '{SolutionFilePath}'")
            End If
            sb.AppendLine(currentLine)
            currentLine = GetNextTextLine(sr)

            Do
                Select Case currentLine.Trim(" "c, CChar(vbTab))
                    Case "GlobalSection(SolutionNotes) = postSolution",
                         "GlobalSection(SolutionConfiguration) = preSolution",
                         "GlobalSection(SolutionConfigurationPlatforms) = preSolution",
                         "GlobalSection(SolutionProperties) = preSolution",
                         "GlobalSection(ExtensibilityAddIns) = postSolution"
                        sb.AppendLine(currentLine)
                        currentLine = GetNextTextLine(sr)
                        While (Not String.IsNullOrEmpty(currentLine)) AndAlso Not currentLine.Contains("EndGlobalSection", StringComparison.OrdinalIgnoreCase)
                            sb.AppendLine(currentLine)
                            currentLine = GetNextTextLine(sr)
                        End While
                    Case "GlobalSection(ProjectConfigurationPlatforms) = postSolution",
                         "GlobalSection(ProjectConfiguration) = postSolution"
                        sb.AppendLine(currentLine)
                        currentLine = GetNextTextLine(sr)
                        While (Not String.IsNullOrEmpty(currentLine)) AndAlso Not currentLine.Contains("EndGlobalSection", StringComparison.OrdinalIgnoreCase)
                            Dim oldGUID As String = GetGuid(currentLine, First:=True)
                            If String.IsNullOrWhiteSpace(oldGUID) Then
                                Stop
                                sb.AppendLine(currentLine)
                            Else
                                sb.AppendLine(currentLine.Replace(oldGUID, projectsGUIDMap(oldGUID), StringComparison.OrdinalIgnoreCase))
                            End If
                            currentLine = GetNextTextLine(sr)
                        End While
                    Case "GlobalSection(SharedMSBuildProjectFiles) = preSolution",
                         "GlobalSection(ExtensibilityGlobals) = postSolution"
                        sb.AppendLine(currentLine)
                        currentLine = GetNextTextLine(sr)
                        While (Not String.IsNullOrEmpty(currentLine)) AndAlso Not currentLine.Contains("EndGlobalSection", StringComparison.OrdinalIgnoreCase)
                            Dim oldGUID As String = GetGuid(currentLine, First:=True)
                            If currentLine.Contains("SolutionGuid", StringComparison.OrdinalIgnoreCase) OrElse String.IsNullOrWhiteSpace(oldGUID) Then
                                sb.AppendLine(currentLine)
                            Else
                                sb.AppendLine(currentLine.Replace(oldGUID, projectsGUIDMap(oldGUID), StringComparison.OrdinalIgnoreCase))
                            End If
                            currentLine = GetNextTextLine(sr)
                        End While
                    Case "GlobalSection(SharedMSBuildProjectFiles) = preSolution"
                        sb.AppendLine(currentLine)
                        currentLine = GetNextTextLine(sr)
                        While (Not String.IsNullOrEmpty(currentLine)) AndAlso Not currentLine.Contains("EndGlobalSection", StringComparison.OrdinalIgnoreCase)
                            currentLine = GetNextTextLine(sr)
                        End While
                    Case "GlobalSection(NestedProjects) = preSolution"
                        sb.AppendLine(currentLine)
                        currentLine = GetNextTextLine(sr)
                        While (Not String.IsNullOrEmpty(currentLine)) AndAlso Not currentLine.Contains("EndGlobalSection", StringComparison.OrdinalIgnoreCase)
                            Dim oldGUID As String = GetGuid(currentLine, First:=True)
                            If String.IsNullOrWhiteSpace(oldGUID) Then
                                Stop
                                sb.AppendLine(currentLine)
                            Else
                                currentLine = currentLine.Replace(oldGUID, projectsGUIDMap(oldGUID), StringComparison.OrdinalIgnoreCase)
                            End If
                            oldGUID = GetGuid(currentLine, First:=False)
                            If String.IsNullOrWhiteSpace(oldGUID) Then
                                Stop
                                sb.AppendLine(currentLine)
                            Else
                                sb.AppendLine(currentLine.Replace(oldGUID, projectsGUIDMap(oldGUID), StringComparison.OrdinalIgnoreCase))
                            End If
                            currentLine = GetNextTextLine(sr)
                        End While

                    Case Else
                        Stop
                End Select
                If String.IsNullOrEmpty(currentLine) Then
                    Throw New IOException($"Premature EOF in solution file '{SolutionFilePath}'")
                End If
                sb.AppendLine(currentLine)
                currentLine = GetNextTextLine(sr)
            Loop Until String.IsNullOrWhiteSpace(currentLine) OrElse currentLine.Contains("EndGlobal", StringComparison.OrdinalIgnoreCase)
            sb.AppendLine(currentLine)
            currentLine = GetNextTextLine(sr)

            If Not String.IsNullOrWhiteSpace(currentLine) Then
                Throw New IOException($"Premature EOF in solution file '{SolutionFilePath}'")
            End If
        End Using
        Using sw As New StreamWriter(Path.Combine(saveSolutionRoot, Path.GetFileName(SolutionFilePath)))
            sw.Write(sb.ToString)
        End Using
    End Sub

End Module
