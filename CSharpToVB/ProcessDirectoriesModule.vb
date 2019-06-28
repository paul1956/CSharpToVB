Option Explicit On
Option Infer Off
Option Strict On

Imports System.IO

Imports Microsoft.CodeAnalysis

Public Module ProcessDirectoriesModule

    ''' <summary>
    ''' Converts new directory from TargetDirectory/SubdirectoryName
    ''' </summary>
    ''' <param name="TargetDirectory"></param>
    ''' <param name="SubdirectoryName"></param>
    ''' <returns></returns>
    Private Function ConvertSourceToTargetDirectory(TargetDirectory As String, SubdirectoryName As String) As String
        If TargetDirectory = "" Then
            Return ""
        End If
        Return Path.Combine(TargetDirectory, New DirectoryInfo(SubdirectoryName).Name)
    End Function

    Public Function GetFileTextFromStream(fileStream As Stream) As String
        Using sw As New StreamReader(fileStream)
            Dim SourceText As String = sw.ReadToEnd()
            sw.Close()
            Return SourceText
        End Using
    End Function

    Public Sub LocalUseWaitCutsor(MeForm As Form1, Enable As Boolean)
        If MeForm Is Nothing Then
            Exit Sub
        End If
        If MeForm.UseWaitCursor <> Enable Then
            MeForm.UseWaitCursor = Enable
            Application.DoEvents()
        End If
    End Sub

    ''' <summary>
    ''' Needed for Unit Tests
    ''' </summary>
    ''' <param name="SourceDirectory">Start location of where to process directories</param>
    ''' <param name="TargetDirectory">Start location of where to process directories</param>
    ''' <param name="StopButton">Pass Nothing for Unit Tests</param>
    ''' <param name="LastFileNameWithPath">Pass Last File Name to Start Conversion where you left off</param>
    ''' <param name="SourceLanguageExtension">vb or cs</param>
    ''' <param name="FilesProcessed">Count of the number of tiles processed</param>
    ''' <returns>
    ''' False if error and user wants to stop, True if success or user wants to ignore error
    ''' </returns>
    Public Function ProcessDirectory(SourceDirectory As String, TargetDirectory As String, MeForm As Form1, StopButton As Button, RichTextBoxFileList As RichTextBox, ByRef LastFileNameWithPath As String, SourceLanguageExtension As String, ByRef FilesProcessed As Long, ByRef TotalFilesToProcess As Long, ProcessFile As Func(Of String, String, String, MetadataReference(), Boolean)) As Boolean
        ' Process the list of files found in the directory.
        Dim DirectoryList As String() = Directory.GetFiles(path:=SourceDirectory, searchPattern:=$"*.{SourceLanguageExtension}")
        Dim TargetExtension As String = If(SourceLanguageExtension = "vb", "cs", "vb")
        For Each PathWithFileName As String In DirectoryList
            FilesProcessed += 1
            If LastFileNameWithPath.Length = 0 OrElse LastFileNameWithPath = PathWithFileName Then
                LastFileNameWithPath = ""
                If RichTextBoxFileList IsNot Nothing Then
                    RichTextBoxFileList.AppendText($"{FilesProcessed.ToString.PadLeft(5)} {PathWithFileName}{vbCrLf}")
                    RichTextBoxFileList.Select(RichTextBoxFileList.TextLength, 0)
                    RichTextBoxFileList.ScrollToCaret()
                    Application.DoEvents()
                End If

                If Not ProcessFile(PathWithFileName, TargetDirectory, SourceLanguageExtension, References) Then
                    SetButtonStopAndCursor(MeForm:=MeForm, StopButton:=StopButton, StopButtonVisible:=False)
                    Return False
                End If
                If MeForm IsNot Nothing Then
                    MeForm.FilesConversionProgress.Text = $"Processed {FilesProcessed:N0} of {TotalFilesToProcess:N0} Files"
                    Application.DoEvents()
                End If
            End If
        Next PathWithFileName
        Dim subdirectoryEntries As String() = Directory.GetDirectories(path:=SourceDirectory)
        ' Recurse into subdirectories of this directory.
        For Each Subdirectory As String In subdirectoryEntries
            Dim DirName As String = New DirectoryInfo(Subdirectory).Name.ToLower
            If (DirName = "bin" OrElse DirName = "obj" OrElse DirName = "g") AndAlso (MeForm Is Nothing OrElse My.Settings.SkipBinAndObjFolders) Then
                Continue For
            End If

            If (Subdirectory.EndsWith("Test\Resources") OrElse Subdirectory.EndsWith("Setup\Templates")) AndAlso (MeForm Is Nothing OrElse My.Settings.SkipTestResourceFiles) Then
                Continue For
            End If
            If Not ProcessDirectory(Subdirectory, ConvertSourceToTargetDirectory(TargetDirectory, Subdirectory), MeForm, StopButton, RichTextBoxFileList, LastFileNameWithPath, SourceLanguageExtension, FilesProcessed, TotalFilesToProcess, ProcessFile) Then
                SetButtonStopAndCursor(MeForm:=MeForm, StopButton:=StopButton, StopButtonVisible:=False)
                Return False
            End If
        Next Subdirectory
        Return True
    End Function

    Public Sub SetButtonStopAndCursor(MeForm As Form1, StopButton As Button, StopButtonVisible As Boolean)
        If StopButton IsNot Nothing Then
            StopButton.Visible = StopButtonVisible
            MeForm.StopRequested = False
        End If
        LocalUseWaitCutsor(MeForm:=MeForm, Enable:=StopButtonVisible)
    End Sub

    Public Sub WriteTextToStream(DirectoryName As String, FileName As String, SourceText As String)
        If Not Directory.Exists(DirectoryName) Then
            Directory.CreateDirectory(DirectoryName)
        End If
        Using sw As New StreamWriter(Path.Combine(DirectoryName, FileName), append:=False)
            sw.Write(SourceText)
            sw.Close()
        End Using
    End Sub

End Module