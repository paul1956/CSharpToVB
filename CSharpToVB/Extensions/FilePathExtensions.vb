' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.IO
Imports System.Runtime.CompilerServices
Imports System.Threading
Imports Extensions
Imports Microsoft.CodeAnalysis

Public Module FilePathExtensions

    Private Sub CreateDirectoryIfNonexistent(solutionRoot As String)
        If Not Directory.Exists(solutionRoot) Then
            Directory.CreateDirectory(solutionRoot)
        End If
    End Sub

    <Extension>
    Private Function IsComment(trivia As SyntaxTrivia) As Boolean
        Return trivia.IsSingleLineComment OrElse trivia.IsMultiLineComment
    End Function

    <Extension>
    Private Function IsDocComment(trivia As SyntaxTrivia) As Boolean
        Return trivia.IsSingleLineDocComment() OrElse trivia.IsMultiLineDocComment()
    End Function

    <Extension>
    Private Function IsMultiLineComment(trivia As SyntaxTrivia) As Boolean
        Return trivia.IsKind(CSharp.SyntaxKind.MultiLineCommentTrivia) OrElse
                trivia.IsKind(CSharp.SyntaxKind.DocumentationCommentExteriorTrivia) OrElse
                trivia.IsKind(CSharp.SyntaxKind.MultiLineDocumentationCommentTrivia)
    End Function

    <Extension>
    Private Function IsMultiLineDocComment(trivia As SyntaxTrivia) As Boolean
        Return trivia.IsKind(CSharp.SyntaxKind.MultiLineDocumentationCommentTrivia)
    End Function

    <Extension>
    Private Function IsRegularOrDocComment(trivia As SyntaxTrivia) As Boolean
        Return trivia.IsSingleLineComment() OrElse trivia.IsMultiLineComment() OrElse trivia.IsDocComment()
    End Function

    <Extension>
    Private Function IsSingleLineComment(trivia As SyntaxTrivia) As Boolean
        Return trivia.IsKind(CSharp.SyntaxKind.SingleLineCommentTrivia) OrElse
                trivia.IsKind(CSharp.SyntaxKind.SingleLineDocumentationCommentTrivia) OrElse
                trivia.IsKind(VisualBasic.SyntaxKind.CommentTrivia)
    End Function

    <Extension>
    Private Function IsSingleLineDocComment(trivia As SyntaxTrivia) As Boolean
        Return trivia.IsKind(CSharp.SyntaxKind.SingleLineDocumentationCommentTrivia)
    End Function

    <Extension>
    Friend Function GetFileCount(dirPath As String, sourceLanguageExtension As String, skipBinAndObjFolders As Boolean, skipTestResourceFiles As Boolean, Optional depth As Integer = 0) As Long
        Dim totalFilesToProcess As Long = 0L

        Try
            For Each subDirectory As String In Directory.EnumerateDirectories(dirPath)
                If skipTestResourceFiles AndAlso
                        (subDirectory.EndsWith("Test\Resources", StringComparison.OrdinalIgnoreCase) OrElse
                         subDirectory.EndsWith("Setup\Templates", StringComparison.OrdinalIgnoreCase)) Then
                    Continue For
                End If
                Dim subDirectoryName As String = Path.GetFileName(subDirectory)
                If skipBinAndObjFolders AndAlso (subDirectoryName = "bin" OrElse
                    subDirectoryName = "obj" OrElse
                    subDirectoryName = "g") Then
                    Continue For
                End If
                totalFilesToProcess += subDirectory.GetFileCount(sourceLanguageExtension, skipBinAndObjFolders, skipTestResourceFiles, depth + 1)
            Next
            For Each file As String In Directory.EnumerateDirectories(path:=dirPath, searchPattern:=$"*.{sourceLanguageExtension}")
                If Not ParseCSharpSource(file, New List(Of String)).
                    GetRoot.SyntaxTree.IsGeneratedCode(Function(t As SyntaxTrivia) As Boolean
                                                           Return t.IsComment OrElse t.IsRegularOrDocComment
                                                       End Function, CancellationToken.None) Then
                    totalFilesToProcess += 1
                End If
            Next
        Catch ex As ObjectDisposedException
            End
        Catch ex As OperationCanceledException
            Throw
        Catch ua As UnauthorizedAccessException
            ' Ignore
        Catch ex As Exception
            Throw
        End Try

        Return totalFilesToProcess
    End Function

    ''' <summary>
    ''' To work with Git we need to create a new folder tree from the parent of this project
    ''' </summary>
    ''' <param name="myForm"></param>
    ''' <param name="dirOrFileToBeTranslated"></param>
    ''' <returns></returns>
    ''' <param name="promptIfDirExists"></param>
    <Extension>
    Friend Function GetSavePath(myForm As Form1, dirOrFileToBeTranslated As String, promptIfDirExists As Boolean) As (SolutionRoot As String, ProjectRelativePath As String)
        Debug.Assert(Directory.GetDirectoryRoot(dirOrFileToBeTranslated) <> dirOrFileToBeTranslated, $"{dirOrFileToBeTranslated} does Not exist")
        Dim sourceRoot As String = dirOrFileToBeTranslated
        Dim currentDirectory As String = sourceRoot
        Dim systemRootDirectory As String = Directory.GetDirectoryRoot(currentDirectory)
        If File.Exists(dirOrFileToBeTranslated) Then
            sourceRoot = Directory.GetParent(dirOrFileToBeTranslated).FullName
            currentDirectory = sourceRoot
            dirOrFileToBeTranslated = sourceRoot
        End If
        Debug.Assert(Directory.Exists(sourceRoot), $"{dirOrFileToBeTranslated} does Not exist")

        While systemRootDirectory <> currentDirectory
            If Directory.GetFiles(currentDirectory, "*.sln").Any Then
                sourceRoot = Directory.GetParent(currentDirectory).FullName
                Exit While
            End If
            currentDirectory = Directory.GetParent(currentDirectory).FullName
        End While
        If systemRootDirectory = currentDirectory Then
            Dim defaultRoot As String = Directory.GetParent(dirOrFileToBeTranslated).FullName
            sourceRoot = defaultRoot
        End If
        ' At this point Solution Directory is the remainder of the path from SolutionRoot
        Dim pathFromSolutionRoot As List(Of String) = dirOrFileToBeTranslated.Replace(sourceRoot, "", StringComparison.OrdinalIgnoreCase) _
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
            myForm.Close()
            End
        End If
        If promptIfDirExists AndAlso Directory.Exists(solutionRoot) Then
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
