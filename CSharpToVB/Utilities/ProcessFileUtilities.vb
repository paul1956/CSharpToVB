﻿' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Diagnostics.CodeAnalysis
Imports System.Globalization
Imports System.IO
Imports System.Reflection
Imports System.Runtime.CompilerServices
Imports System.Text
Imports System.Threading
Imports CSharpToVBConverter
Imports CSharpToVBConverter.ConversionResult
Imports Microsoft.CodeAnalysis
Imports ProgressReportLibrary

Friend Module ProcessFileUtilities

    Private Async Function Convert_Compile_ColorizeAsync(
        MainForm As Form1,
        RequestToConvert As ConvertRequest,
        CSPreprocessorSymbols As List(Of String),
        VBPreprocessorSymbols As List(Of KeyValuePair(Of String, Object)),
        OptionalReferences() As MetadataReference, CancelToken As CancellationToken) As Task(Of Boolean)
        Dim uiContext As SynchronizationContext = SynchronizationContext.Current

        Dim reportException As Action(Of Exception) =
            Sub(ex As Exception)
                ' Use the Windows Forms synchronization context in order to call MsgBox from the UI thread.
                uiContext.Post(Function(state) MsgBox(ex.Message,
                                                      MsgBoxStyle.OkOnly Or MsgBoxStyle.Critical Or MsgBoxStyle.MsgBoxSetForeground,
                                                      "Stack Overflow"), state:=Nothing)
            End Sub

        Dim defaultVBOptions As New DefaultVbOptions
        With My.Settings
            defaultVBOptions = New DefaultVbOptions(.OptionCompare, .OptionCompareIncludeInCode, .OptionExplicit, .OptionExplicitIncludeInCode, .OptionInfer, .OptionInferIncludeInCode, .OptionStrict, .OptionStrictIncludeInCode)
        End With
        ' The System.Progress class invokes the callback on the UI thread. It does this because we create the
        ' System.Progress object on the main thread. During creation, it reads SynchronizationContext.Current so
        ' that it knows how to get back to the main thread to invoke the callback there no matter what thread calls
        ' IProgress.Report.
        Dim progress As New Progress(Of ProgressReport)(AddressOf MainForm.StatusStripConversionProgressBar.Update)
        MainForm._resultOfConversion = Await Task.Run(Function() ConvertInputRequest(RequestToConvert,
                                                                                     defaultVBOptions,
                                                                                     CSPreprocessorSymbols,
                                                                                     VBPreprocessorSymbols,
                                                                                     OptionalReferences,
                                                                                     reportException,
                                                                                     progress,
                                                                                     CancelToken)
                                                                                    ).ConfigureAwait(True)

        If MainForm._resultOfConversion Is Nothing Then
            MainForm.mnuFileSaveAs.Enabled = False
            Return False
        Else
            MainForm.mnuFileSaveAs.Enabled = MainForm._resultOfConversion.ResultStatus = ResultTriState.Success
        End If
        Select Case MainForm._resultOfConversion.ResultStatus
            Case ResultTriState.Success
                Compile_Colorize(MainForm, MainForm._resultOfConversion.ConvertedCode, VBPreprocessorSymbols)
                Dim filteredErrorCount As Integer = MainForm._resultOfConversion.GetFilteredListOfFailures().Count
                MainForm.LabelErrorCount.Text = $"Number of Errors: {filteredErrorCount}"
                Return filteredErrorCount = 0
            Case ResultTriState.Failure
                If TypeOf MainForm._resultOfConversion.Exceptions(0) IsNot OperationCanceledException Then
                    Dim selectionColor As ColorDescriptor = GetColorFromName(ThemeErrorColor)
                    MainForm.ConversionOutput.SelectionBackColor = selectionColor.Background
                    MainForm.ConversionOutput.SelectionColor = selectionColor.Foreground
                    MainForm.ConversionOutput.Text = GetExceptionsAsString(MainForm._resultOfConversion.Exceptions)
                End If
            Case ResultTriState.Ignore
                MainForm.ConversionOutput.Text = String.Empty
                MainForm.LabelErrorCount.Text = "File Skipped"
        End Select
        Return MainForm._resultOfConversion.ResultStatus <> ResultTriState.Failure
    End Function

    <ExcludeFromCodeCoverage>
    Private Function GetExceptionsAsString(Exceptions As IReadOnlyList(Of Exception)) As String
        If Exceptions Is Nothing OrElse Not Exceptions.Any Then
            Return String.Empty
        End If

        Dim builder As New StringBuilder()
        For index As Integer = 0 To Exceptions.Count - 1
            builder.AppendFormat(CultureInfo.InvariantCulture, "----- Exception {0} Of {1} -----" & Environment.NewLine, index + 1, Exceptions.Count)
            builder.AppendLine(Exceptions(index).ToString())
        Next index
        Return builder.ToString()
    End Function

    <Extension>
    Friend Async Function ConvertSnippetOfTopLevelStmt(MainForm As Form1, SourceCode As String) As Task
        SetButtonStopAndCursor(MeForm:=MainForm, StopButton:=MainForm.ButtonStopConversion, StopButtonVisible:=True)
        MainForm.ListBoxErrorList.Items.Clear()
        MainForm.ListBoxFileList.Items.Clear()
        MainForm.LineNumbersForConversionOutput.Visible = False
        MainForm.StatusStripCurrentFileName.Text = ""
        MainForm.ResizeRichTextBuffers()
        If MainForm._cancellationTokenSource IsNot Nothing Then
            MainForm._cancellationTokenSource.Dispose()
        End If
        MainForm._cancellationTokenSource = New CancellationTokenSource

        MainForm._requestToConvert = New ConvertRequest(My.Settings.SkipAutoGenerated, New Progress(Of ProgressReport)(AddressOf MainForm.StatusStripConversionProgressBar.Update), MainForm._cancellationTokenSource.Token) With
                {
                .SourceCode = SourceCode
                }

        Dim dontDisplayLineNumbers As Boolean = Await Convert_Compile_ColorizeAsync(MainForm,
                                                                                    MainForm._requestToConvert,
                                                                                    New List(Of String) From {My.Settings.Framework},
                                                                                    New List(Of KeyValuePair(Of String, Object)) From {KeyValuePair.Create(Of String, Object)(My.Settings.Framework, True)},
                                                                                    SharedReferences.CSharpReferences(Assembly.Load("System.Windows.Forms").Location,
                                                                                    OptionalReference:=Nothing).ToArray,
                                                                                    MainForm._cancellationTokenSource.Token
                                                                                   ).ConfigureAwait(True)
        If MainForm._requestToConvert.CancelToken.IsCancellationRequested Then
            MsgBox($"Conversion canceled.",
                       MsgBoxStyle.OkOnly Or MsgBoxStyle.Information Or MsgBoxStyle.MsgBoxSetForeground,
                       Title:="C# to Visual Basic")
            MainForm.StatusStripConversionProgressBar.Clear()
        End If
        SetButtonStopAndCursor(MainForm, MainForm.ButtonStopConversion, StopButtonVisible:=False)
        MainForm.LineNumbersForConversionOutput.Visible = (Not dontDisplayLineNumbers) OrElse My.Settings.ShowDestinationLineNumbers
    End Function

    ''' <summary>
    ''' Convert one file
    ''' </summary>
    ''' <param name="MainForm"></param>
    ''' <param name="SourceFileNameWithPath">Complete path including file name to file to be converted</param>
    ''' <param name="TargetDirectory">Complete path up to File to be converted</param>
    ''' <param name="SourceLanguageExtension">vb or cs</param>
    ''' <param name="CSPreprocessorSymbols"></param>
    ''' <param name="VBPreprocessorSymbols"></param>
    ''' <param name="OptionalReferences"></param>
    ''' <param name="SkipAutoGenerated"></param>
    ''' <param name="CancelToken"></param>
    ''' <returns>False if error and user wants to stop, True if success or user wants to ignore error.</returns>
    Friend Async Function ProcessFileAsync(MainForm As Form1, SourceFileNameWithPath As String, TargetDirectory As String, SourceLanguageExtension As String, CSPreprocessorSymbols As List(Of String), VBPreprocessorSymbols As List(Of KeyValuePair(Of String, Object)), OptionalReferences() As MetadataReference, SkipAutoGenerated As Boolean, CancelToken As CancellationToken) As Task(Of Boolean)
        If My.Settings.IgnoreFileList.Contains(SourceFileNameWithPath) Then
            Return True
        End If
        With MainForm
            .ButtonStopConversion.Visible = True
            .UseWaitCursor = True
            .ConversionOutput.Text = ""
            My.Settings.MRU_Data.mnuAddToMRU(SourceFileNameWithPath)
            .UpdateLastFileMenu()
            .mnuFile.DropDownItems.FileMenuMRUUpdateUI(AddressOf .mnu_MRUList_Click)
            MainForm.ProjectConversionInitProgressBar.Visible = True
            Application.DoEvents()
            Dim lines As Integer = LoadInputBufferFromStream(MainForm, SourceFileNameWithPath)
            If lines > 0 Then

                ._requestToConvert = New ConvertRequest(SkipAutoGenerated, New Progress(Of ProgressReport)(AddressOf MainForm.StatusStripConversionProgressBar.Update), ._cancellationTokenSource.Token) With {
                        .SourceCode = MainForm.ConversionInput.Text
                    }
                If Not Await Convert_Compile_ColorizeAsync(MainForm, ._requestToConvert, CSPreprocessorSymbols, VBPreprocessorSymbols, OptionalReferences, CancelToken).ConfigureAwait(True) Then
                    If ._requestToConvert.CancelToken.IsCancellationRequested Then
                        .StatusStripConversionProgressBar.Clear()
                        Return False
                    End If
                    Dim msgBoxResult As MsgBoxResult
                    If ._doNotFailOnError Then
                        msgBoxResult = MsgBoxResult.Yes
                    Else
                        msgBoxResult = MsgBox($"Conversion failed, do you want to stop processing this file automatically in the future? Yes and No will continue processing files, Cancel will stop conversions!",
                                             MsgBoxStyle.YesNoCancel Or MsgBoxStyle.Exclamation Or MsgBoxStyle.MsgBoxSetForeground)
                    End If
                    Select Case msgBoxResult
                        Case MsgBoxResult.Cancel
                            ._cancellationTokenSource.Cancel()
                            Return False
                        Case MsgBoxResult.No
                            Return True
                        Case MsgBoxResult.Yes
                            If Not My.Settings.IgnoreFileList.Contains(SourceFileNameWithPath) Then
                                My.Settings.IgnoreFileList.Add(SourceFileNameWithPath)
                                My.Settings.Save()
                            End If
                            .ListBoxErrorList.Items.Clear()
                            .LineNumbersForConversionInput.Visible = My.Settings.ShowSourceLineNumbers
                            .LineNumbersForConversionOutput.Visible = My.Settings.ShowDestinationLineNumbers
                            ._doNotFailOnError = True
                            Return True
                    End Select
                Else
                    If Not String.IsNullOrWhiteSpace(TargetDirectory) Then
                        If ._requestToConvert.CancelToken.IsCancellationRequested Then
                            Return False
                        End If
                        If .LabelErrorCount.Text = "File Skipped" Then
                            Return True
                        End If
                        Dim newFileName As String = Path.ChangeExtension(New FileInfo(SourceFileNameWithPath).Name, If(SourceLanguageExtension = "vb", "cs", "vb"))
                        WriteTextToStream(TargetDirectory, newFileName, .ConversionOutput.Text)
                    End If
                    If My.Settings.PauseConvertOnSuccess Then
                        If MsgBox($"{SourceFileNameWithPath} successfully converted, Continue?",
                                  MsgBoxStyle.YesNo Or MsgBoxStyle.Question Or MsgBoxStyle.MsgBoxSetForeground) = MsgBoxResult.No Then
                            Return False
                        End If
                    End If
                End If
                MainForm.ProjectConversionInitProgressBar.Visible = False

                ' 5 second delay
                Const loopSleep As Integer = 25
                Dim delay As Integer = (1000 * My.Settings.ConversionDelay) \ loopSleep
                For index As Integer = 1 To delay
                    Application.DoEvents()
                    Thread.Sleep(loopSleep)
                    If CancelToken.IsCancellationRequested Then
                        Return False
                    End If
                Next
                Application.DoEvents()
            Else
                .ConversionOutput.Clear()
            End If
        End With
        Return True
    End Function

    ''' <summary>
    ''' Process all files in the directory passed in, recurse on any directories
    ''' that are found, and process the files they contain.
    ''' </summary>
    ''' <param name="SourceDirectory">Start location of where to process directories</param>
    ''' <param name="TargetDirectory">Start location of where to process directories</param>
    ''' <param name="LastFileNameWithPath">Pass Last File Name to Start Conversion where you left off</param>
    ''' <param name="SourceLanguageExtension">vb or cs</param>
    ''' <param name="FilesProcessed">Count of the number of tiles processed</param>
    ''' <returns>
    ''' False if error and user wants to stop, True if success or user wants to ignore error
    ''' </returns>
    Friend Async Function ProcessFilesAsync(MainForm As Form1, SourceDirectory As String, TargetDirectory As String, SourceLanguageExtension As String, Stats As ProcessingStats, CancelToken As CancellationToken) As Task(Of Boolean)
        With MainForm
            Try
                .ListBoxErrorList.Items.Clear()
                .ListBoxFileList.Items.Clear()
                SetButtonStopAndCursor(MainForm,
                                       .ButtonStopConversion,
                                       StopButtonVisible:=True)
                MainForm.UpdateProgress("Getting Files List to Process")
                Dim fileCount As Task(Of Long) = Task.Run(Function() As Long
                                                              Return SourceDirectory.GetFileCount(SourceLanguageExtension,
                                                                 My.Settings.SkipBinAndObjFolders,
                                                                 My.Settings.SkipTestResourceFiles
                                                                 )
                                                          End Function
                                                          )
                While Not fileCount.IsCompleted
                    If MainForm._cancellationTokenSource.IsCancellationRequested Then
                        Exit Try
                    End If
                    Await Task.Delay(100, MainForm._cancellationTokenSource.Token).ConfigureAwait(True)
                End While
                Stats.TotalFilesToProcess = fileCount.Result

                MainForm.UpdateProgress("")
                ' Process the list of files found in the directory.
                Return Await ProcessDirectoryAsync(MainForm,
                                                   SourceDirectory,
                                                   TargetDirectory,
                                                   .ButtonStopConversion,
                                                   .ListBoxFileList,
                                                   SourceLanguageExtension,
                                                   Stats,
                                                   AddressOf ProcessFileAsync,
                                                   CancelToken).ConfigureAwait(True)
            Catch ex As OperationCanceledException
                .StatusStripConversionProgressBar.Clear()
            Catch ex As Exception
                ' don't crash on exit
                End
            Finally
                MainForm.UpdateProgress("")
                SetButtonStopAndCursor(MainForm, .ButtonStopConversion, StopButtonVisible:=False)
            End Try
        End With
        Return False
    End Function

    <Extension>
    Friend Sub UpdateProgress(MainForm As Form1, progressStr As String)
        MainForm.ProjectConversionInitProgressLabel.Text = progressStr
        If progressStr.Any Then
            MainForm.ConversionInput.Text = ""
            MainForm.ConversionOutput.Text = ""
            MainForm.ProjectConversionInitProgressLabel.Visible = True
            MainForm.ProjectConversionInitProgressLabel.Left = Math.Max((MainForm.ConversionInput.Width \ 2) - (MainForm.ProjectConversionInitProgressLabel.Width \ 2), 0)
            MainForm.ProjectConversionInitProgressLabel.Top = (MainForm.ConversionInput.Height \ 2) - (MainForm.ProjectConversionInitProgressLabel.Height \ 2)

            MainForm.ProjectConversionInitProgressBar.Visible = True
            MainForm.ProjectConversionInitProgressBar.Left = Math.Max((MainForm.ConversionInput.Width \ 2) - (MainForm.ProjectConversionInitProgressBar.Width \ 2), 0)
            MainForm.ProjectConversionInitProgressBar.Top = MainForm.ProjectConversionInitProgressLabel.Bottom + 5
        Else
            MainForm.ProjectConversionInitProgressLabel.Visible = False
            MainForm.ProjectConversionInitProgressBar.Visible = False
        End If
        Application.DoEvents()
    End Sub

End Module
