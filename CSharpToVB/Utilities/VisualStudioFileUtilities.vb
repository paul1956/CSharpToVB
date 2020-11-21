' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.IO

Public Module VisualStudioFileUtilities
    Private Const VisualStudioBaseName As String = "Visual Studio "

    Private Function GetUserDirectoryFromTemp() As String
        Dim sourceDirectory As String = FileIO.SpecialDirectories.Temp
        sourceDirectory = Directory.GetParent(sourceDirectory).FullName
        sourceDirectory = Directory.GetParent(sourceDirectory).FullName
        sourceDirectory = Directory.GetParent(sourceDirectory).FullName
        Return sourceDirectory
    End Function

    Public Function GetAlternetVisualStudioProjectsPath() As String
        Dim sourceDirectory As String = GetUserDirectoryFromTemp()

        If Directory.Exists(sourceDirectory) Then
            Dim reposPath As String = Path.Combine(sourceDirectory, "Source", "Repos")
            If Directory.Exists(reposPath) Then
                Return reposPath
            End If
        End If
        Return ""
    End Function

    Public Function GetLatestVisualStudioProjectPath() As String
        Dim directoryEntries As String() = Directory.GetDirectories(FileIO.SpecialDirectories.MyDocuments, VisualStudioBaseName.Trim & "*")
        Dim latestVersion As Integer = 0
        For Each dir As String In directoryEntries
            Dim directoryFileName As String = Path.GetFileName(dir)
            If directoryFileName.StartsWith(VisualStudioBaseName, StringComparison.OrdinalIgnoreCase) Then
                If Directory.Exists(Path.Combine(dir, "Projects")) Then
                    Dim vsVersion As Integer = CInt(directoryFileName.Replace(VisualStudioBaseName, "", StringComparison.OrdinalIgnoreCase))
                    If vsVersion > latestVersion Then
                        latestVersion = vsVersion
                    End If
                End If
            End If
        Next
        If latestVersion = 0 Then
            Return FileIO.SpecialDirectories.MyDocuments
        End If
        Return Path.Combine(FileIO.SpecialDirectories.MyDocuments, $"{VisualStudioBaseName}{latestVersion:0000}", "Projects")
    End Function

End Module
