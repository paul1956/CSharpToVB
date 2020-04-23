﻿' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.IO
Imports System.Reflection
Imports System.Threading

Imports Microsoft.CodeAnalysis

Public Module ProcessDirectoriesModule

    ''' <summary>
    ''' Converts new directory from TargetDirectory/SubdirectoryName
    ''' </summary>
    ''' <param name="TargetDirectory"></param>
    ''' <param name="SubdirectoryName"></param>
    ''' <returns></returns>
    Private Function ConvertSourceToTargetDirectory(TargetDirectory As String, SubdirectoryName As String) As String
        If String.IsNullOrWhiteSpace(TargetDirectory) Then
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

    Public Sub LocalUseWaitCursor(MeForm As Form1, WaitCursorEnable As Boolean)
        If MeForm Is Nothing Then
            Exit Sub
        End If
        If MeForm.UseWaitCursor <> WaitCursorEnable Then
            MeForm.UseWaitCursor = WaitCursorEnable
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
    Public Async Function ProcessDirectoryAsync(SourceDirectory As String, TargetDirectory As String, MeForm As Form1, StopButton As Button, ListBoxFileList As ListBox, SourceLanguageExtension As String, Stats As ProcessingStats, ProcessFileAsync As Func(Of String, String, String, List(Of String), List(Of KeyValuePair(Of String, Object)), MetadataReference(), CancellationToken, Task(Of Boolean)), CancelToken As CancellationToken) As Task(Of Boolean)
        If String.IsNullOrWhiteSpace(SourceDirectory) OrElse Not Directory.Exists(SourceDirectory) Then
            Return True
        End If
        ' Process the list of files found in the directory.
        Try
            Dim DirectoryList As String() = Directory.GetFiles(path:=SourceDirectory, searchPattern:=$"*.{SourceLanguageExtension}")
            Dim CSPreprocessorSymbols As New List(Of String) From {
                                        My.Settings.Framework
                                    }
            Dim VBPreprocessorSymbols As New List(Of KeyValuePair(Of String, Object)) From {
                                        KeyValuePair.Create(Of String, Object)(My.Settings.Framework, True)
                                    }
            For Each SourcePathWithFileName As String In DirectoryList
                Stats.FilesProcessed += 1
                If Stats.LastFileNameWithPath.Length = 0 OrElse Stats.LastFileNameWithPath = SourcePathWithFileName Then
                    Stats.LastFileNameWithPath = ""
                    If ListBoxFileList IsNot Nothing Then
                        ListBoxFileList.Items.Add(New NumberedListItem($"{Stats.FilesProcessed.ToString(Globalization.CultureInfo.InvariantCulture),-5} {SourcePathWithFileName}", $"{Path.Combine(TargetDirectory, Path.GetFileNameWithoutExtension(SourcePathWithFileName))}.vb"))
                        ListBoxFileList.SelectedIndex = ListBoxFileList.Items.Count - 1
                        Application.DoEvents()
                    End If

                    If Not Await ProcessFileAsync(SourcePathWithFileName, TargetDirectory, SourceLanguageExtension, CSPreprocessorSymbols, VBPreprocessorSymbols, CSharpReferences(Assembly.Load("System.Windows.Forms").Location, OptionalReference:=Nothing).ToArray, CancelToken).ConfigureAwait(True) Then
                        SetButtonStopAndCursor(MeForm:=MeForm, StopButton:=StopButton, StopButtonVisible:=False)
                        Return False
                    End If
                    If MeForm IsNot Nothing Then
                        MeForm.FilesConversionProgress.Text = $"Processed {Stats.FilesProcessed:N0} of {Stats.TotalFilesToProcess:N0} Files"
                        Application.DoEvents()
                    End If
                End If
            Next SourcePathWithFileName
        Catch ex As Exception
            Stop
            Throw
        End Try
        Dim subdirectoryEntries As String() = Directory.GetDirectories(path:=SourceDirectory)
        Try
            ' Recurse into subdirectories of this directory.
            For Each Subdirectory As String In subdirectoryEntries
                Dim DirName As String = New DirectoryInfo(Subdirectory).Name.ToUpperInvariant
                If (DirName = "BIN" OrElse DirName = "OBJ" OrElse DirName = "G") AndAlso
                    (MeForm Is Nothing OrElse My.Settings.SkipBinAndObjFolders) Then
                    Continue For
                End If

                If (Subdirectory.EndsWith("Test\Resources", StringComparison.OrdinalIgnoreCase) OrElse Subdirectory.EndsWith("Setup\Templates", StringComparison.OrdinalIgnoreCase)) AndAlso (MeForm Is Nothing OrElse My.Settings.SkipTestResourceFiles) Then
                    Continue For
                End If
                If Not Await ProcessDirectoryAsync(Subdirectory, ConvertSourceToTargetDirectory(TargetDirectory, Subdirectory), MeForm, StopButton, ListBoxFileList, SourceLanguageExtension, Stats, ProcessFileAsync, CancelToken).ConfigureAwait(True) Then
                    SetButtonStopAndCursor(MeForm:=MeForm, StopButton:=StopButton, StopButtonVisible:=False)
                    Return False
                End If
            Next Subdirectory
        Catch ex As Exception
            Stop
            Throw
        End Try
        Return True
    End Function

    Friend Sub SetButtonStopAndCursor(MeForm As Form1, StopButton As Button, StopButtonVisible As Boolean)
        If MeForm Is Nothing Then
            Exit Sub
        End If
        If StopButton IsNot Nothing Then
            StopButton.Visible = StopButtonVisible
        End If
        Dim enableControl As Boolean = Not StopButtonVisible
        MeForm.RichTextBoxConversionInput.Enabled = enableControl
        MeForm.mnuFile.Enabled = enableControl
        MeForm.mnuConvert.Enabled = enableControl
        LocalUseWaitCursor(MeForm:=MeForm, WaitCursorEnable:=StopButtonVisible)
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
