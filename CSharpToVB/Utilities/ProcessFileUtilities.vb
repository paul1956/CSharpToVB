' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Diagnostics.CodeAnalysis
Imports System.Globalization
Imports System.IO
Imports System.Reflection
Imports System.Runtime.CompilerServices
Imports System.Text
Imports System.Threading
Imports Microsoft.CodeAnalysis
Imports ProgressReportLibrary
Imports SupportClasses
Imports SupportClasses.ConversionResult
Imports Utilities

Friend Module ProcessFileUtilities

    Private Async Function Convert_Compile_ColorizeAsync(
        mainForm As Form1,
        requestToConvert As ConvertRequest,
        csPreprocessorSymbols As List(Of String),
        vbPreprocessorSymbols As List(Of KeyValuePair(Of String, Object)),
        optionalReferences() As MetadataReference, cancelToken As CancellationToken) As Task(Of Boolean)
        Dim uiContext As SynchronizationContext = SynchronizationContext.Current

        Dim reportException As Action(Of Exception) =
            Sub(ex As Exception)
                ' Use the Windows Forms synchronization context in order to call MsgBox from the UI thread.
                uiContext.Post(Function(state) MsgBox(ex.Message,
                                                      MsgBoxStyle.OkOnly Or MsgBoxStyle.Critical Or MsgBoxStyle.MsgBoxSetForeground,
                                                      "Stack Overflow"), state:=Nothing)
            End Sub

        Dim defaultVbOptions As DefaultVbOptions
        With My.Settings
            defaultVbOptions = New DefaultVbOptions(.OptionCompare,
                                                    .OptionCompareIncludeInCode,
                                                    .OptionExplicit,
                                                    .OptionExplicitIncludeInCode,
                                                    .OptionInfer,
                                                    .OptionInferIncludeInCode,
                                                    .OptionStrict,
                                                    .OptionStrictIncludeInCode)
        End With
        ' The System.Progress class invokes the callback on the UI thread. It does this because we create the
        ' System.Progress object on the main thread. During creation, it reads SynchronizationContext.Current so
        ' that it knows how to get back to the main thread to invoke the callback there no matter what thread calls
        ' IProgress.Report.
        Dim progress As New Progress(Of ProgressReport)(AddressOf mainForm.StatusStripConversionProgressBar.Update)
        mainForm._resultOfConversion = Await Task.Run(Function() ConvertInputRequest(requestToConvert,
                                                                                     defaultVbOptions,
                                                                                     csPreprocessorSymbols,
                                                                                     vbPreprocessorSymbols,
                                                                                     optionalReferences,
                                                                                     reportException,
                                                                                     progress,
                                                                                     cancelToken)
                                                                                    ).ConfigureAwait(True)

        If mainForm._resultOfConversion Is Nothing Then
            mainForm.mnuFileSaveAs.Enabled = False
            Return False
        Else
            mainForm.mnuFileSaveAs.Enabled = mainForm._resultOfConversion.ResultStatus = ResultTriState.Success
        End If
        Select Case mainForm._resultOfConversion.ResultStatus
            Case ResultTriState.Success
                Compile_Colorize(mainForm, mainForm._resultOfConversion.ConvertedCode, vbPreprocessorSymbols)
                Dim filteredErrorCount As Integer = mainForm._resultOfConversion.GetFilteredListOfFailures().Count
                mainForm.LabelErrorCount.Text = $"Number of Errors: {filteredErrorCount}"
                Return filteredErrorCount = 0
            Case ResultTriState.Failure
                If TypeOf mainForm._resultOfConversion.Exceptions(0) IsNot OperationCanceledException Then
                    Dim selectionColor As ColorDescriptor = GetColorFromName(ThemeErrorColor)
                    mainForm.ConversionOutput.SelectionBackColor = selectionColor.Background
                    mainForm.ConversionOutput.SelectionColor = selectionColor.Foreground
                    mainForm.ConversionOutput.Text = GetExceptionsAsString(mainForm._resultOfConversion.Exceptions)
                End If
            Case ResultTriState.Ignore
                mainForm.ConversionOutput.Text = String.Empty
                mainForm.LabelErrorCount.Text = $"File Skipped"
        End Select
        Return mainForm._resultOfConversion.ResultStatus <> ResultTriState.Failure
    End Function

    <ExcludeFromCodeCoverage>
    Private Function GetExceptionsAsString(exceptions As IReadOnlyList(Of Exception)) As String
        If exceptions Is Nothing OrElse Not exceptions.Any Then
            Return String.Empty
        End If

        Dim builder As New StringBuilder()
        For index As Integer = 0 To exceptions.Count - 1
            builder.AppendFormat(CultureInfo.InvariantCulture, "----- Exception {0} Of {1} -----" & Environment.NewLine, index + 1, exceptions.Count)
            builder.AppendLine(exceptions(index).ToString())
        Next index
        Return builder.ToString()
    End Function

    <Extension>
    Friend Async Function ConvertSnippetOfTopLevelStmt(mainForm As Form1, sourceCode As String) As Task
        SetButtonStopAndCursor(meForm:=mainForm, stopButton:=mainForm.ButtonStopConversion, stopButtonVisible:=True)
        mainForm.ErrorListListBox.Items.Clear()
        mainForm.FileListListBox.Items.Clear()
        mainForm.LineNumbersForConversionOutput.Visible = False
        mainForm.StatusStripCurrentFileName.Text = ""
        mainForm.ResizeRichTextBuffers()
        If mainForm._cancellationTokenSource IsNot Nothing Then
            mainForm._cancellationTokenSource.Dispose()
        End If
        mainForm._cancellationTokenSource = New CancellationTokenSource

        mainForm._requestToConvert = New ConvertRequest(My.Settings.SkipAutoGenerated, New Progress(Of ProgressReport)(AddressOf mainForm.StatusStripConversionProgressBar.Update), mainForm._cancellationTokenSource.Token) With
                {
                .SourceCode = sourceCode
                }

        Dim doNotDisplayLineNumbers As Boolean = Await Convert_Compile_ColorizeAsync(mainForm,
                                                                                    mainForm._requestToConvert,
                                                                                    New List(Of String) From {My.Settings.Framework},
                                                                                    New List(Of KeyValuePair(Of String, Object)) From {KeyValuePair.Create(Of String, Object)(My.Settings.Framework, True)},
                                                                                    CSharpReferences(Assembly.Load("System.Windows.Forms").Location,
                                                                                    optionalReference:=Nothing).ToArray,
                                                                                    mainForm._cancellationTokenSource.Token
                                                                                   ).ConfigureAwait(True)
        If mainForm._requestToConvert.CancelToken.IsCancellationRequested Then
            MsgBox($"Conversion canceled.",
                       MsgBoxStyle.OkOnly Or MsgBoxStyle.Information Or MsgBoxStyle.MsgBoxSetForeground,
                       Title:="C# to Visual Basic")
            mainForm.StatusStripConversionProgressBar.Clear()
        End If
        SetButtonStopAndCursor(mainForm, mainForm.ButtonStopConversion, stopButtonVisible:=False)
        mainForm.LineNumbersForConversionOutput.Visible = (Not doNotDisplayLineNumbers) OrElse My.Settings.ShowDestinationLineNumbers
    End Function

    ''' <summary>
    ''' Convert one file
    ''' </summary>
    ''' <param name="mainForm"></param>
    ''' <param name="sourceFileNameWithPath">Complete path including file name to file to be converted</param>
    ''' <param name="targetDirectory">Complete path up to File to be converted</param>
    ''' <param name="sourceLanguageExtension">vb or cs</param>
    ''' <param name="csPreprocessorSymbols"></param>
    ''' <param name="vbPreprocessorSymbols"></param>
    ''' <param name="optionalReferences"></param>
    ''' <param name="skipAutoGenerated"></param>
    ''' <param name="cancelToken"></param>
    ''' <returns>False if error and user wants to stop, True if success or user wants to ignore error.</returns>
    Friend Async Function ProcessFileAsync(mainForm As Form1, sourceFileNameWithPath As String, targetDirectory As String, sourceLanguageExtension As String, csPreprocessorSymbols As List(Of String), vbPreprocessorSymbols As List(Of KeyValuePair(Of String, Object)), optionalReferences() As MetadataReference, skipAutoGenerated As Boolean, cancelToken As CancellationToken) As Task(Of Boolean)
        If My.Settings.IgnoreFileList.Contains(sourceFileNameWithPath) Then
            Return True
        End If
        With mainForm
            .ButtonStopConversion.Visible = True
            .UseWaitCursor = True
            .ConversionOutput.Text = ""
            My.Settings.MRU_Data.MnuAddToMru(sourceFileNameWithPath)
            .UpdateLastFileMenu()
            .mnuFile.DropDownItems.FileMenuMruUpdateUi(AddressOf .mnu_MRUList_Click)
            mainForm.ProjectConversionInitProgressBar.Visible = True
            Application.DoEvents()
            Dim lines As Integer = LoadInputBufferFromStream(mainForm, sourceFileNameWithPath)
            If lines > 0 Then

                ._requestToConvert = New ConvertRequest(skipAutoGenerated, New Progress(Of ProgressReport)(AddressOf mainForm.StatusStripConversionProgressBar.Update), ._cancellationTokenSource.Token) With {
                        .SourceCode = mainForm.ConversionInput.Text
                    }
                If Not Await Convert_Compile_ColorizeAsync(mainForm, ._requestToConvert, csPreprocessorSymbols, vbPreprocessorSymbols, optionalReferences, cancelToken).ConfigureAwait(True) Then
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
                            If Not My.Settings.IgnoreFileList.Contains(sourceFileNameWithPath) Then
                                My.Settings.IgnoreFileList.Add(sourceFileNameWithPath)
                                My.Settings.Save()
                            End If
                            .ErrorListListBox.Items.Clear()
                            .LineNumbersForConversionInput.Visible = My.Settings.ShowSourceLineNumbers
                            .LineNumbersForConversionOutput.Visible = My.Settings.ShowDestinationLineNumbers
                            ._doNotFailOnError = True
                            Return True
                    End Select
                Else
                    If Not String.IsNullOrWhiteSpace(targetDirectory) Then
                        If ._requestToConvert.CancelToken.IsCancellationRequested Then
                            Return False
                        End If
                        If .LabelErrorCount.Text = $"File Skipped" Then
                            Return True
                        End If
                        Dim newFileName As String = Path.ChangeExtension(New FileInfo(sourceFileNameWithPath).Name, If(sourceLanguageExtension = "vb", "cs", "vb"))
                        WriteTextToStream(targetDirectory, newFileName, .ConversionOutput.Text)
                    End If
                    If My.Settings.PauseConvertOnSuccess Then
                        If MsgBox($"{sourceFileNameWithPath} successfully converted, Continue?",
                                  MsgBoxStyle.YesNo Or MsgBoxStyle.Question Or MsgBoxStyle.MsgBoxSetForeground) = MsgBoxResult.No Then
                            Return False
                        End If
                    End If
                End If
                mainForm.ProjectConversionInitProgressBar.Visible = False

                ' 5 second delay
                Const loopSleep As Integer = 25
                Dim delay As Integer = (1000 * My.Settings.ConversionDelay) \ loopSleep
                ' ReSharper disable once RedundantAssignment
                For index As Integer = 1 To delay
                    Application.DoEvents()
                    Thread.Sleep(loopSleep)
                    If cancelToken.IsCancellationRequested Then
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
    ''' <param name="mainForm"></param>
    ''' <param name="sourceDirectory">Start location of where to process directories</param>
    ''' <param name="targetDirectory">Start location of where to process directories</param>
    ''' <param name="sourceLanguageExtension">vb or cs</param>
    ''' <param name="stats"></param>
    ''' <param name="cancelToken"></param>
    ''' <returns>
    ''' False if error and user wants to stop, True if success or user wants to ignore error
    ''' </returns>
    Friend Async Function ProcessFilesAsync(mainForm As Form1, sourceDirectory As String, targetDirectory As String, sourceLanguageExtension As String, stats As ProcessingStats, cancelToken As CancellationToken) As Task(Of Boolean)
        With mainForm
            Try
                .ErrorListListBox.Items.Clear()
                .FileListListBox.Items.Clear()
                SetButtonStopAndCursor(mainForm,
                                       .ButtonStopConversion,
                                       stopButtonVisible:=True)
                mainForm.UpdateProgress("Getting Files List to Process")
                Dim fileCount As Task(Of Long) = Task.Run(Function() As Long
                                                              Return sourceDirectory.GetFileCount(sourceLanguageExtension,
                                                                 My.Settings.SkipBinAndObjFolders,
                                                                 My.Settings.SkipTestResourceFiles
                                                                 )
                                                          End Function
                                                          )
                While Not fileCount.IsCompleted
                    If mainForm._cancellationTokenSource.IsCancellationRequested Then
                        Exit Try
                    End If
                    Await Task.Delay(100, mainForm._cancellationTokenSource.Token).ConfigureAwait(True)
                End While
                stats.TotalFilesToProcess = fileCount.Result

                mainForm.UpdateProgress("")
                ' Process the list of files found in the directory.
                Return Await ProcessDirectoryAsync(mainForm,
                                                   sourceDirectory,
                                                   targetDirectory,
                                                   .ButtonStopConversion,
                                                   .FileListListBox,
                                                   sourceLanguageExtension,
                                                   stats,
                                                   AddressOf ProcessFileAsync,
                                                   cancelToken).ConfigureAwait(True)
            Catch ex As OperationCanceledException
                .StatusStripConversionProgressBar.Clear()
            Catch ex As Exception
                ' don't crash on exit
                End
            Finally
                mainForm.UpdateProgress("")
                SetButtonStopAndCursor(mainForm, .ButtonStopConversion, stopButtonVisible:=False)
            End Try
        End With
        Return False
    End Function

    <Extension>
    Friend Sub UpdateProgress(mainForm As Form1, progressStr As String)
        mainForm.ProjectConversionInitProgressLabel.Text = progressStr
        If progressStr.Any Then
            mainForm.ConversionInput.Text = ""
            mainForm.ConversionOutput.Text = ""
            mainForm.ProjectConversionInitProgressLabel.Visible = True
            mainForm.ProjectConversionInitProgressLabel.Left = Math.Max((mainForm.ConversionInput.Width \ 2) - (mainForm.ProjectConversionInitProgressLabel.Width \ 2), 0)
            mainForm.ProjectConversionInitProgressLabel.Top = (mainForm.ConversionInput.Height \ 2) - (mainForm.ProjectConversionInitProgressLabel.Height \ 2)

            mainForm.ProjectConversionInitProgressBar.Visible = True
            mainForm.ProjectConversionInitProgressBar.Left = Math.Max((mainForm.ConversionInput.Width \ 2) - (mainForm.ProjectConversionInitProgressBar.Width \ 2), 0)
            mainForm.ProjectConversionInitProgressBar.Top = mainForm.ProjectConversionInitProgressLabel.Bottom + 5
        Else
            mainForm.ProjectConversionInitProgressLabel.Visible = False
            mainForm.ProjectConversionInitProgressBar.Visible = False
        End If
        Application.DoEvents()
    End Sub

End Module
