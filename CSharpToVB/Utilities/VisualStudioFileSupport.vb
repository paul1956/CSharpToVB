' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.IO

Public Module VisualStudioFileSupport
    Private Const VisualStudioBaseName As String = "Visual Studio "

    Private Function GetUserDirectoryFromTemp() As String
        Dim SourceDirectory As String = FileIO.SpecialDirectories.Temp
        SourceDirectory = Directory.GetParent(SourceDirectory).FullName
        SourceDirectory = Directory.GetParent(SourceDirectory).FullName
        SourceDirectory = Directory.GetParent(SourceDirectory).FullName
        Return SourceDirectory
    End Function

    Public Function GetAlternetVisualStudioProjectsPath() As String
        Dim SourceDirectory As String = GetUserDirectoryFromTemp()

        If Directory.Exists(SourceDirectory) Then
            Dim ReposPath As String = Path.Combine(SourceDirectory, "Source", "Repos")
            If Directory.Exists(ReposPath) Then
                Return ReposPath
            End If
        End If
        Return ""
    End Function

    Public Function GetLatestVisualStudioProjectPath() As String
        Dim DirectoryEntries As String() = Directory.GetDirectories((FileIO.SpecialDirectories.MyDocuments), VisualStudioBaseName.Trim & "*")
        Dim LatestVersion As Integer = 0
        For Each dir As String In DirectoryEntries
            Dim DirectoryFileName As String = Path.GetFileName(dir)
            If DirectoryFileName.StartsWith(VisualStudioBaseName, StringComparison.OrdinalIgnoreCase) Then
                If Directory.Exists(Path.Combine(dir, "Projects")) Then
                    Dim VSVersion As Integer = CInt(DirectoryFileName.Replace(VisualStudioBaseName, "", StringComparison.OrdinalIgnoreCase))
                    If VSVersion > LatestVersion Then
                        LatestVersion = VSVersion
                    End If
                End If
            End If
        Next
        If LatestVersion = 0 Then
            Return FileIO.SpecialDirectories.MyDocuments
        End If
        Return Path.Combine(FileIO.SpecialDirectories.MyDocuments, $"{VisualStudioBaseName}{LatestVersion:0000}", "Projects")
    End Function

End Module
