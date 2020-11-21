' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.IO
Imports System.Runtime.CompilerServices
Imports System.Threading
Imports CSharpToVBConverter
Imports Microsoft.CodeAnalysis

Public Module FilePathExtensions

    <Extension>
    Friend Function GetFileCount(DirPath As String, SourceLanguageExtension As String, SkipBinAndObjFolders As Boolean, SkipTestResourceFiles As Boolean, Optional Depth As Integer = 0) As Long
        Dim totalFilesToProcess As Long = 0L

        Try
            For Each subdirectory As String In Directory.GetDirectories(DirPath)
                If SkipTestResourceFiles AndAlso
                        (subdirectory.EndsWith("Test\Resources", StringComparison.OrdinalIgnoreCase) OrElse
                         subdirectory.EndsWith("Setup\Templates", StringComparison.OrdinalIgnoreCase)) Then
                    Continue For
                End If
                Dim subdirectoryName As String = Path.GetFileName(subdirectory)
                If SkipBinAndObjFolders AndAlso (subdirectoryName = "bin" OrElse
                    subdirectoryName = "obj" OrElse
                    subdirectoryName = "g") Then
                    Continue For
                End If
                totalFilesToProcess += subdirectory.GetFileCount(SourceLanguageExtension, SkipBinAndObjFolders, SkipTestResourceFiles, Depth + 1)
            Next
            For Each file As String In Directory.GetFiles(path:=DirPath, searchPattern:=$"*.{SourceLanguageExtension}")
                If Not ParseCSharpSource(file, New List(Of String)).
                    GetRoot.SyntaxTree.IsGeneratedCode(Function(t As SyntaxTrivia) As Boolean
                                                           Return t.IsComment OrElse t.IsRegularOrDocComment
                                                       End Function, CancellationToken.None) Then
                    totalFilesToProcess += 1
                End If
            Next
        Catch ex As OperationCanceledException
            Throw
        Catch ua As UnauthorizedAccessException
            ' Ignore
        Catch ex As Exception
            Stop
            Throw
        End Try

        Return totalFilesToProcess
    End Function

    ''' <summary>
    ''' To work with Git we need to create a new folder tree from the parent of this project
    ''' </summary>
    ''' <param name="MyForm"></param>
    ''' <param name="DirOrFileToBeTranslated"></param>
    ''' <returns></returns>
    ''' <param name="PromptIfDirExsits"></param>
    <Extension>
    Friend Function GetSavePath(MyForm As Form1, DirOrFileToBeTranslated As String, PromptIfDirExsits As Boolean) As (SolutionRoot As String, ProjectRelativePath As String)
        Debug.Assert(Directory.GetDirectoryRoot(DirOrFileToBeTranslated) <> DirOrFileToBeTranslated, $"{DirOrFileToBeTranslated} does Not exist")
        Dim sourceRoot As String = DirOrFileToBeTranslated
        Dim currentDirectory As String = sourceRoot
        Dim systemtRootDirectory As String = Directory.GetDirectoryRoot(currentDirectory)
        If File.Exists(DirOrFileToBeTranslated) Then
            sourceRoot = Directory.GetParent(DirOrFileToBeTranslated).FullName
            currentDirectory = sourceRoot
            DirOrFileToBeTranslated = sourceRoot
        End If
        Debug.Assert(Directory.Exists(sourceRoot), $"{DirOrFileToBeTranslated} does Not exist")

        While systemtRootDirectory <> currentDirectory
            If Directory.GetFiles(currentDirectory, "*.sln").Any Then
                sourceRoot = Directory.GetParent(currentDirectory).FullName
                Exit While
            End If
            currentDirectory = Directory.GetParent(currentDirectory).FullName
        End While
        If systemtRootDirectory = currentDirectory Then
            Dim defaultRoot As String = Directory.GetParent(DirOrFileToBeTranslated).FullName
            sourceRoot = defaultRoot
        End If
        ' At this point Solution Directory is the remainder of the path from SolutionRoot
        Dim pathFromSolutionRoot As List(Of String) = DirOrFileToBeTranslated.Replace(sourceRoot, "", StringComparison.OrdinalIgnoreCase) _
                                                                    .Trim(Path.DirectorySeparatorChar) _
                                                                    .Split(Path.DirectorySeparatorChar).ToList
        pathFromSolutionRoot(0) = pathFromSolutionRoot(0) & "_vb"
        Dim solutionRoot As String = $"{sourceRoot}{Path.DirectorySeparatorChar}{pathFromSolutionRoot(0)}"
        ' Remove top director because it will be change to end in _vb
        pathFromSolutionRoot.RemoveAt(0)
        If File.Exists(solutionRoot) Then
            MsgBox($"A file exists at {solutionRoot} this Is a fatal error the program will exit",
                   MsgBoxStyle.OkOnly Or MsgBoxStyle.Critical Or MsgBoxStyle.MsgBoxSetForeground,
                   "Fatal Error")
            MyForm.Close()
            End
        End If
        If PromptIfDirExsits AndAlso Directory.Exists(solutionRoot) Then
            Select Case MsgBox($"The converted project will be save to {solutionRoot} a directory which already exists. To use it And overwrite existing files select Yes. Selecting No will delete existing content, Selecting Cancel will stop conversion. , ",
                               MsgBoxStyle.YesNoCancel Or MsgBoxStyle.Question Or MsgBoxStyle.MsgBoxSetForeground,
                               "Target Directory Save Options")
                Case MsgBoxResult.Cancel
                    Return (String.Empty, String.Empty)
                Case MsgBoxResult.No
                    If MsgBoxResult.Yes = MsgBox($"Are you sure you want to delete {solutionRoot}?",
                                                 MsgBoxStyle.YesNo Or MsgBoxStyle.Critical Or MsgBoxStyle.MsgBoxSetForeground,
                                                 "Warning Deleting Directory") Then
                        Directory.Delete(solutionRoot, recursive:=True)
                        Directory.CreateDirectory(solutionRoot)
                    End If
                Case MsgBoxResult.Yes
            End Select
        End If
        Dim relativePath As String = pathFromSolutionRoot.Join(Path.DirectorySeparatorChar)
        CreateDirectoryIfNonexistent(Path.Combine(solutionRoot, relativePath))
        Return (solutionRoot, relativePath)
    End Function

    <Extension>
    Public Function GetFileTextFromStream(fileStream As Stream) As String
        Using sw As New StreamReader(fileStream)
            Dim sourceText As String = sw.ReadToEnd()
            sw.Close()
            Return sourceText
        End Using
    End Function

End Module
