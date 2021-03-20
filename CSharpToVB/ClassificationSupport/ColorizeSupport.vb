' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Text
Imports Extensions
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Emit
Imports SupportClasses.ConversionResult
Imports Utilities

Public Module ColorizeSupport

    Private Function FilterOutTopLevelStatementCode(textToCompile As String) As String
        If Not My.Settings.IncludeTopLevelStmtProtoInCode AndAlso textToCompile.Contains("Top Level Code boilerplate is included,") Then
            Dim filteredCode As New StringBuilder
            Dim skipNext As Boolean
            Dim skipBlank As Boolean
            For Each e As IndexClass(Of String) In textToCompile.SplitLines.WithIndex
                Dim stmt As String = e.Value
                If String.IsNullOrEmpty(stmt) Then
                    If Not skipBlank Then
                        filteredCode.AppendLine()
                        skipBlank = True
                    End If
                    Continue For
                End If
                skipBlank = False
                Select Case True
                    Case stmt.Trim.StartsWith("' Top Level Code boilerplate is included")
                        skipBlank = True
                    Case stmt.Trim.StartsWith("Namespace Application")
                    Case stmt.Trim.StartsWith("NotInheritable Class Program")
                    Case stmt.Trim.StartsWith("Private Shared ")
                    Case stmt.Trim.StartsWith("Public Shared ")
                        skipBlank = True
                        skipNext = True
                    Case stmt.Trim.StartsWith("End Sub")
                    Case stmt.Trim.StartsWith("End Class")
                    Case stmt.Trim.StartsWith("End Namespace")
                    Case stmt.StartsWith("            ")
                        filteredCode.AppendLine(stmt.Substring(12))
                    Case skipNext
                        skipNext = False
                    Case Else
                        filteredCode.AppendLine(stmt)
                End Select
            Next
            textToCompile = filteredCode.ToString
        End If

        Return textToCompile
    End Function

    ''' <summary>
    ''' This function only colorizes 1 range it is an optimization to handle editing and is not perfect
    ''' </summary>
    ''' <param name="conversionBuffer"></param>
    ''' <param name="sourceLanguage"></param>
    Friend Sub Colorize1Range(conversionBuffer As RichTextBox, sourceLanguage As String)
        Dim currentChar As Integer = conversionBuffer.SelectionStart
        Dim currentLength As Integer = conversionBuffer.SelectionLength
        Dim fragmentRange As List(Of Range) = GetClassifiedRanges(conversionBuffer.Text, sourceLanguage).ToList()
        With conversionBuffer
            For Each range As Range In fragmentRange
                If currentChar < range.TextSpan.Start Then
                    Continue For
                End If
                .Select(range.TextSpan.Start, range.TextSpan.Length)
                .SelectionColor = GetColorFromName(range.ClassificationType).Foreground
                Application.DoEvents()
                Exit For
            Next range
            .Select(currentChar, currentLength)
        End With
    End Sub

    Friend Sub Colorize(mainForm As Form1, fragmentRange As List(Of Range), conversionBuffer As RichTextBox, lines As Integer)
        Colorize(mainForm, fragmentRange, conversionBuffer, lines, Nothing)
    End Sub

    Friend Sub Colorize(mainForm As Form1, fragmentRange As List(Of Range), conversionBuffer As RichTextBox, lines As Integer, Optional failures As IEnumerable(Of Diagnostic) = Nothing)
        If mainForm._inColorize Then
            Exit Sub
        End If
        Try ' Prevent crash when exiting
            mainForm._inColorize = True
            If conversionBuffer.Visible Then
                conversionBuffer.Visible = False
            End If
            Dim dias As IEnumerable(Of Diagnostic) = If(TryCast(failures, Diagnostic()), failures?.ToArray())

            mainForm.StatusStripConversionProgressBar.Maximum = lines

            With conversionBuffer
                .Clear()
                .BackColor = GetColorFromName(ThemeDefaultColor).Background
                .Select(.TextLength, 0)
                For Each range As Range In fragmentRange
                    .Select(.TextLength, 0)
                    .SelectionColor = GetColorFromName(range.ClassificationType).Foreground
                    .AppendText(range.Text)
                    If range.Text.Contains(vbLf, StringComparison.OrdinalIgnoreCase) Then
                        mainForm.StatusStripConversionProgressBar.Increment(range.Text.Count(CType(vbLf, Char)))
                        Application.DoEvents()
                    End If
                    If mainForm._requestToConvert?.CancelToken.IsCancellationRequested Then
                        Exit Sub
                    End If
                    Application.DoEvents()
                Next range
                If failures?.Count > 0 Then
                    For Each dia As Diagnostic In dias
                        Dim errorLine As Integer = dia.Location.GetLineSpan.StartLinePosition.Line
                        Dim errorCharacterPosition As Integer = dia.Location.GetLineSpan.StartLinePosition.Character
                        Dim length As Integer = dia.Location.GetLineSpan.EndLinePosition.Character - errorCharacterPosition
                        .Select(.GetFirstCharIndexFromLine(errorLine) + errorCharacterPosition, length)
                        Dim selectionColor As ColorDescriptor = GetColorFromName("Error")
                        .SelectionBackColor = selectionColor.Background
                        .SelectionColor = selectionColor.Foreground
                        .Select(.TextLength, 0)
                    Next
                    .Select(.GetFirstCharIndexFromLine(failures(0).Location.GetLineSpan.StartLinePosition.Line), 0)
                    .ScrollToCaret()
                End If
            End With
            If failures Is Nothing Then
                mainForm.ErrorListListBox.Enabled = False
            Else
                mainForm.ErrorListListBox.Enabled = True
                For Each dia As Diagnostic In dias
                    mainForm.ErrorListListBox.Items.Add($"{dia.Id} Line = {dia.Location.GetLineSpan.StartLinePosition.Line + 1} {dia.GetMessage}")
                Next
                If failures?.Count > 0 Then
                    mainForm.LineNumbersForConversionInput.Visible = True
                    mainForm.LineNumbersForConversionOutput.Visible = True
                End If
            End If
            mainForm.StatusStripConversionProgressBar.Clear()
        Catch ex As OperationCanceledException
        Catch ex As ObjectDisposedException
            End
        Catch ex As Exception
            Stop
        Finally
            conversionBuffer.Visible = True
            conversionBuffer.Refresh()
            Application.DoEvents()
            mainForm._inColorize = False
        End Try
    End Sub

    Friend Sub Compile_Colorize(mainForm As Form1, textToCompile As String, vbPreprocessorSymbols As List(Of KeyValuePair(Of String, Object)))
        Dim compileResult As (Success As Boolean, EmitResult As EmitResult) = CompileVisualBasicString(textToCompile, vbPreprocessorSymbols, DiagnosticSeverity.Error, mainForm._resultOfConversion)

        mainForm.LabelErrorCount.Text = $"Number of Errors:  {mainForm._resultOfConversion.GetFilteredListOfFailures().Count}"
        If Not My.Settings.IncludeTopLevelStmtProtoInCode Then
            mainForm._inColorize = True

            mainForm._inColorize = False
        End If

        If compileResult.Success AndAlso compileResult.EmitResult.Success Then
            textToCompile = FilterOutTopLevelStatementCode(textToCompile)
            If My.Settings.ColorizeOutput Then
                Colorize(mainForm, GetClassifiedRanges(textToCompile, LanguageNames.VisualBasic).ToList(), mainForm.ConversionOutput, textToCompile.SplitLines.Length)
                mainForm.ConversionOutput.Select(0, 0)
            Else
                mainForm.ConversionOutput.Text = textToCompile
            End If
        Else
            If Not mainForm._resultOfConversion.GetFilteredListOfFailures().Any Then
                textToCompile = FilterOutTopLevelStatementCode(textToCompile)
                mainForm._resultOfConversion.ResultStatus = ResultTriState.Success
                If My.Settings.ColorizeOutput Then
                    Colorize(mainForm, GetClassifiedRanges(textToCompile, LanguageNames.VisualBasic).ToList(), mainForm.ConversionOutput, textToCompile.SplitLines.Length, mainForm._resultOfConversion.GetFilteredListOfFailures())
                    mainForm.ConversionOutput.Select(0, 0)
                Else
                    mainForm.ConversionOutput.Text = textToCompile
                End If
            Else
                Colorize(mainForm, GetClassifiedRanges(textToCompile, LanguageNames.VisualBasic).ToList(), mainForm.ConversionOutput, textToCompile.SplitLines.Length, mainForm._resultOfConversion.GetFilteredListOfFailures())
            End If
        End If
        mainForm.ConversionOutput.Visible = True
        Application.DoEvents()
    End Sub

End Module
