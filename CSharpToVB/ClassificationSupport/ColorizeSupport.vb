' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Threading

Imports CSharpToVBApp
Imports CSharpToVBConverter
Imports CSharpToVBConverter.ConversionResult
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Emit
Imports ProgressReportLibrary

Public Module ColorizeSupport

    Friend Sub Colorize(MainForm As Form1, FragmentRange As IEnumerable(Of Range), ConversionBuffer As RichTextBox, Lines As Integer, Optional failures As IEnumerable(Of Diagnostic) = Nothing)
        If MainForm._inColorize Then
            Exit Sub
        End If
        MainForm._inColorize = True
        If ConversionBuffer.Visible Then
            ConversionBuffer.Visible = False
        End If
        Try ' Prevent crash when exiting
            If failures Is Nothing Then
                MainForm.ListBoxErrorList.Enabled = False
            Else
                MainForm.ListBoxErrorList.Enabled = True
                For Each dia As Diagnostic In failures
                    MainForm.ListBoxErrorList.Items.Add($"{dia.Id} Line = {dia.Location.GetLineSpan.StartLinePosition.Line + 1} {dia.GetMessage}")
                Next
            End If

            MainForm.StatusStripConversionProgressBar.Maximum = Lines

            With ConversionBuffer
                .Clear()
                .Select(.TextLength, 0)
                For Each range As Range In FragmentRange
                    .Select(.TextLength, 0)
                    .SelectionColor = ColorSelector.GetColorFromName(range.ClassificationType)
                    .AppendText(range.Text)
                    If range.Text.Contains(vbLf, StringComparison.OrdinalIgnoreCase) Then
                        MainForm.StatusStripConversionProgressBar.Increment(range.Text.Count(CType(vbLf, Char)))
                        Application.DoEvents()
                    End If
                    If MainForm._requestToConvert?.CancelToken.IsCancellationRequested Then
                        Exit Sub
                    End If
                Next range
                Application.DoEvents()
                If failures?.Count > 0 Then
                    For Each dia As Diagnostic In failures
                        Dim errorLine As Integer = dia.Location.GetLineSpan.StartLinePosition.Line
                        Dim errorCharactorPosition As Integer = dia.Location.GetLineSpan.StartLinePosition.Character
                        Dim length As Integer = dia.Location.GetLineSpan.EndLinePosition.Character - errorCharactorPosition
                        .Select(.GetFirstCharIndexFromLine(errorLine) + errorCharactorPosition, length)
                        .SelectionColor = Color.Red
                        .Select(.TextLength, 0)
                    Next
                    .Select(.GetFirstCharIndexFromLine(failures(0).Location.GetLineSpan.StartLinePosition.Line), 0)
                    .ScrollToCaret()
                End If
            End With
            If failures?.Count > 0 Then
                MainForm.LineNumbersForConversionInput.Visible = True
                MainForm.LineNumbersForConversionOutput.Visible = True
            End If
            MainForm.StatusStripConversionProgressBar.Clear()
        Catch ex As Exception
            Stop
            Throw
        End Try
        ConversionBuffer.Visible = True
        MainForm._inColorize = False
    End Sub

    Friend Sub Compile_Colorize(MainForm As Form1, TextToCompile As String, VBPreprocessorSymbols As List(Of KeyValuePair(Of String, Object)))
        MainForm._inColorize = False
        Dim compileResult As (Success As Boolean, EmitResult As EmitResult) = CompileVisualBasicString(TextToCompile, VBPreprocessorSymbols, DiagnosticSeverity.Error, MainForm._resultOfConversion)

        MainForm.LabelErrorCount.Text = $"Number Of Errors:  {MainForm._resultOfConversion.GetFilteredListOfFailures().Count}"
        Dim fragmentRange As IEnumerable(Of Range) = GetClassifiedRanges(TextToCompile, LanguageNames.VisualBasic)

        If compileResult.Success AndAlso compileResult.EmitResult.Success Then
            If My.Settings.ColorizeOutput Then
                Colorize(MainForm, fragmentRange, MainForm.ConversionOutput, TextToCompile.SplitLines.Length)
            Else
                MainForm.ConversionOutput.Text = TextToCompile
            End If
        Else
            If Not MainForm._resultOfConversion.GetFilteredListOfFailures().Any Then
                MainForm._resultOfConversion.ResultStatus = ResultTriState.Success
                If My.Settings.ColorizeOutput Then
                    Colorize(MainForm, fragmentRange, MainForm.ConversionOutput, TextToCompile.SplitLines.Length, MainForm._resultOfConversion.GetFilteredListOfFailures())
                Else
                    MainForm.ConversionOutput.Text = TextToCompile
                End If
            Else
                Colorize(MainForm, fragmentRange, MainForm.ConversionOutput, TextToCompile.SplitLines.Length, MainForm._resultOfConversion.GetFilteredListOfFailures())
            End If
        End If
        MainForm.ConversionOutput.Visible = True
        Application.DoEvents()
    End Sub

    Friend Async Function Convert_Compile_ColorizeAsync(
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

        Dim defaultVBOptions As New DefaultVBOptions
        With My.Settings
            defaultVBOptions = New DefaultVBOptions(.OptionCompare, .OptionCompareIncludeInCode, .OptionExplicit, .OptionExplicitIncludeInCode, .OptionInfer, .OptionInferIncludeInCode, .OptionStrict, .OptionStrictIncludeInCode)
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
                    MainForm.ConversionOutput.SelectionColor = Color.Red
                    MainForm.ConversionOutput.Text = GetExceptionsAsString(MainForm._resultOfConversion.Exceptions)
                End If
            Case ResultTriState.Ignore
                MainForm.ConversionOutput.Text = ""
                MainForm.LabelErrorCount.Text = "File Skipped"
        End Select
        Return MainForm._resultOfConversion.ResultStatus <> ResultTriState.Failure
    End Function

End Module
