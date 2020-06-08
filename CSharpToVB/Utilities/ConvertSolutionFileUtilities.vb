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
            {"9A19103F-16F7-4668-BE54-9A1E7A4F7556", "F184B08F-C81C-45F6-A57F-5ABD9991F28F"}, ' C# Legacy
            {"FAE04EC0-301F-11D3-BF4B-00C04F79EFBC", "F184B08F-C81C-45F6-A57F-5ABD9991F28F"}, ' C#, Windows (C#)
            {"8BC9CEB8-8B4A-11D0-8D11-00A0C91BC942", ""}, ' C++, Windows (Visual C++)
            {"A9ACE9BB-CECE-4E62-9AA4-C7E7C5BD2124", ""}, ' Database
            {"4F174C21-8C12-11D0-8340-0000F80270F8", ""}, ' Database (other project types)
            {"3EA9E505-35AC-4774-B492-AD1749C4943A", ""}, ' Deployment Cab
            {"06A35CCD-C46D-44D5-987B-CF40FF872267", ""}, ' Deployment Merge Module
            {"978C614F-708E-4E1A-B201-565925725DBA", ""}, ' Deployment Setup
            {"AB322303-2255-48EF-A496-5904EB18DA55", ""}, ' Deployment Smart Device Cab
            {"F135691A-BF7E-435D-8960-F99683D2D49C", ""}, ' Distributed System
            {"BF6F8E12-879D-49E7-ADF0-5503146B24B8", "F184B08F-C81C-45F6-A57F-5ABD9991F28F"}, ' Dynamics 2012 AX C# in AOT
            {"F2A71F9B-5D33-465A-A702-920D77279786", ""}, ' F#
            {"E6FDF86B-F3D1-11D4-8576-0002A516ECE8", ""}, ' J#
            {"20D4826A-C6FA-45DB-90F4-C717570B9F32", "CB4CE8C6-1BDB-4DC7-A4D3-65A1999772F8"}, ' Legacy (2003) Smart Device (C#)
            {"CB4CE8C6-1BDB-4DC7-A4D3-65A1999772F8", "CB4CE8C6-1BDB-4DC7-A4D3-65A1999772F8"}, ' Legacy (2003) Smart Device (VB.NET)
            {"B69E3092-B931-443C-ABE7-7E7B65F2A37F", ""}, ' Micro Framework
            {"EFBA0AD7-5A72-4C68-AF49-83D382785DCF", ""}, ' Mono for Android, Xamarin.Android
            {"6BC8ED88-2882-458C-8E55-DFD12B67127B", ""}, ' MonoTouch, Xamarin.iOS
            {"F5B4F3BC-B597-4E2B-B552-EF5D8A32436F", ""}, ' MonoTouch Binding
            {"786C830F-07A1-408B-BD7F-6EE04809D6DB", ""}, ' Portable Class Library
            {"66A26720-8FB5-11D2-AA7E-00C04F688DDE", ""}, ' Project Folders
            {"593B0543-81F6-4436-BA1E-4747859CAAE2", "EC05E597-79D4-47f3-ADA0-324C4F7C7484"}, ' SharePoint (C#)
            {"EC05E597-79D4-47f3-ADA0-324C4F7C7484", "EC05E597-79D4-47f3-ADA0-324C4F7C7484"}, ' SharePoint (VB.NET)
            {"F8810EC1-6754-47FC-A15F-DFABD2E3FA90", ""}, ' SharePoint Workflow
            {"A1591282-1198-4647-A2B1-27E5FF5F6F3B", ""}, ' Silverlight
            {"4D628B5B-2FBC-4AA6-8C16-197242AEB884", "68B1623D-7FB9-47D8-8664-7ECEA3297D4F"}, ' Smart Device (C#)
            {"68B1623D-7FB9-47D8-8664-7ECEA3297D4F", "68B1623D-7FB9-47D8-8664-7ECEA3297D4F"}, ' Smart Device (VB.NET)
            {"2150E333-8FDC-42A3-9474-1A3956D46DE8", ""}, ' Solution Folder
            {"3AC096D0-A1C2-E12C-1390-A8335801FDAB", ""}, ' Test
            {"A5A43C5B-DE2A-4C0C-9213-0A381AF9435A", ""}, ' Universal Windows Class Library
            {"F184B08F-C81C-45F6-A57F-5ABD9991F28F", "F184B08F-C81C-45F6-A57F-5ABD9991F28F"}, ' VB.NET, Windows (VB.NET)
            {"C252FEB5-A946-4202-B1D4-9916A0590387", ""}, ' Visual Database Tools
            {"54435603-DBB4-11D2-8724-00A0C9A8B90C", ""}, ' Visual Studio 2015 Installer Project Extension
            {"A860303F-1F3F-4691-B57E-529FC101A107", ""}, ' Visual Studio Tools for Applications (VSTA)
            {"BAA0C2D2-18E2-41B9-852F-F413020CAA33", ""}, ' Visual Studio Tools for Office (VSTO)
            {"E24C65DC-7377-472B-9ABA-BC803B73C61A", ""}, ' Web Site
            {"3D9AD99F-2412-4246-B90B-4EAA41C64699", ""}, ' Windows Communication Foundation (WCF)
            {"76F1466A-8B6D-4E39-A767-685A06062A39", ""}, ' Windows Phone 8/8.1 Blank/Hub/Webview App
            {"C089C8C0-30E0-4E22-80C0-CE093F111A43", "DB03555F-0C8B-43BE-9FF9-57896B3C5E56"}, ' Windows Phone 8/8.1 App (C#)
            {"60DC8134-EBA5-43B8-BCC9-BB4BC16C2548", ""}, ' Windows Presentation Foundation (WPF)
            {"BC8A1FFA-BEE3-4634-8014-F334798102B3", ""}, ' Windows Store (Metro) Apps & Components
            {"14822709-B5A1-4724-98CA-57A101D1B079", "D59BE175-2ED0-4C54-BE3D-CDAA9F3214C8"}, ' Workflow (C#)
            {"D59BE175-2ED0-4C54-BE3D-CDAA9F3214C8", "D59BE175-2ED0-4C54-BE3D-CDAA9F3214C8"}, ' Workflow (VB.NET)
            {"32F31D43-81CC-4C15-9DE6-3FC5453562B6", ""}, ' Workflow Foundation
            {"6D335F3A-9D43-41b4-9D22-F6F17C4BE596", ""}, ' XNA (Windows)
            {"2DF5C3F4-5A5F-47a9-8E94-23B4456F55E2", ""}, ' XNA (XBox)
            {"D399B71A-8929-442a-A9AC-8BEC78BB2433", ""}  '	XNA (Zune)
            }

    Private ReadOnly s_headerLines As New List(Of String) From {
                "Microsoft Visual Studio Solution File, Format Version",
                "# Visual Studio Version",
                "VisualStudioVersion =",
                "MinimumVisualStudioVersion ="
                }

    Private Function GetGuid(CurrentLine As String, First As Boolean) As String
        Dim start As Integer
        Dim Len As Integer
        If First Then
            start = CurrentLine.IndexOf("{", StringComparison.OrdinalIgnoreCase) + 1
            Len = CurrentLine.IndexOf("}", StringComparison.OrdinalIgnoreCase) - start
        Else
            start = CurrentLine.LastIndexOf("{", StringComparison.OrdinalIgnoreCase) + 1
            Len = CurrentLine.LastIndexOf("}", StringComparison.OrdinalIgnoreCase) - start
        End If
        If start > 0 Then
            Return CurrentLine.Substring(start, Len).ToUpperInvariant
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

    Public Function ConvertSolutionFile(SolutionFilePath As String, solutionRoot As String) As String
        If String.IsNullOrWhiteSpace(SolutionFilePath) Then
            Throw New ArgumentException($"'{NameOf(SolutionFilePath)}' cannot be null or whitespace", NameOf(SolutionFilePath))
        End If

        If String.IsNullOrWhiteSpace(solutionRoot) Then
            Throw New ArgumentException($"'{NameOf(solutionRoot)}' cannot be null or whitespace", NameOf(solutionRoot))
        End If

        If Not File.Exists(SolutionFilePath) Then
            Return String.Empty
        End If

        Dim sb As New StringBuilder
        Using sr As New StreamReader(SolutionFilePath)
            ' Process File header
            Dim currentLine As String
            For Each headerLine As String In s_headerLines
                currentLine = GetNextTextLine(sr)
                If String.IsNullOrEmpty(currentLine) OrElse Not currentLine?.StartsWith(headerLine, StringComparison.Ordinal) Then
                    Stop
                    Return String.Empty
                End If
                sb.AppendLine(currentLine)
            Next
            ' Process File body
            '   Process Projects
            Dim ProjectsGUIDMap As New Dictionary(Of String, String)
            currentLine = GetNextTextLine(sr)
            If currentLine Is Nothing Then
                Stop
                Return String.Empty
            End If
            While currentLine.StartsWith("Project(""{", StringComparison.Ordinal)
                Dim oldGUID As String = GetGuid(currentLine, First:=True)
                Dim newGuid As String = Nothing
                If Not s_guidMappingDictionary.TryGetValue(oldGUID, newGuid) Then
                    Stop
                    Return String.Empty
                End If
                If String.IsNullOrWhiteSpace(newGuid) Then
                    sb.AppendLine(currentLine)
                Else
                    currentLine = currentLine.Replace(oldGUID, newGuid, StringComparison.OrdinalIgnoreCase) _
                                             .Replace(".csproj", ".vbproj", StringComparison.OrdinalIgnoreCase)
                    oldGUID = GetGuid(currentLine, First:=False)
                    newGuid = Guid.NewGuid.ToString.ToUpperInvariant
                    ProjectsGUIDMap.Add(oldGUID, newGuid)
                    sb.AppendLine(currentLine.Replace(oldGUID, newGuid, StringComparison.OrdinalIgnoreCase))
                End If
                currentLine = GetNextTextLine(sr)
                If Not currentLine = "EndProject" Then
                    Stop
                    Return String.Empty
                End If
                sb.AppendLine(currentLine)
                currentLine = GetNextTextLine(sr)
            End While
            '   Process Global
            If Not currentLine = "Global" Then
                Stop
                Return String.Empty
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
                                sb.AppendLine(currentLine)
                            Else
                                sb.AppendLine(currentLine.Replace(oldGUID, ProjectsGUIDMap(oldGUID), StringComparison.OrdinalIgnoreCase))
                            End If
                            currentLine = GetNextTextLine(sr)
                        End While
                    Case "GlobalSection(ExtensibilityGlobals) = postSolution"
                        sb.AppendLine(currentLine)
                        currentLine = GetNextTextLine(sr)
                        While (Not String.IsNullOrEmpty(currentLine)) AndAlso Not currentLine.Contains("EndGlobalSection", StringComparison.OrdinalIgnoreCase)
                            Dim oldGUID As String = GetGuid(currentLine, First:=True)
                            If String.IsNullOrWhiteSpace(oldGUID) Then
                                sb.AppendLine(currentLine)
                            Else
                                sb.AppendLine(currentLine.Replace(oldGUID, Guid.NewGuid.ToString.ToUpperInvariant, StringComparison.OrdinalIgnoreCase))
                            End If
                            currentLine = GetNextTextLine(sr)
                        End While

                    Case Else
                        Stop
                End Select
                If String.IsNullOrEmpty(currentLine) Then
                    Stop
                    Return String.Empty
                End If
                sb.AppendLine(currentLine)
                currentLine = GetNextTextLine(sr)
            Loop Until String.IsNullOrWhiteSpace(currentLine) OrElse currentLine.Contains("EndGlobal", StringComparison.OrdinalIgnoreCase)
            sb.AppendLine(currentLine)
            currentLine = GetNextTextLine(sr)

            If Not String.IsNullOrWhiteSpace(currentLine) Then
                Stop
                Return String.Empty
            End If
        End Using
        If solutionRoot.Length > 0 Then
            Using sw As New StreamWriter(Path.Combine(solutionRoot, Path.GetFileName(SolutionFilePath)))
                sw.Write(sb.ToString)
            End Using
        End If
        Return sb.ToString
    End Function

End Module
