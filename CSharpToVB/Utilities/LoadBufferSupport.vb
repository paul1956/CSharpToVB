﻿' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.IO
Imports Microsoft.CodeAnalysis
Imports Utilities

Friend Module LoadBufferSupport

    Public Function LoadInputBufferFromStream(mainForm As Form1, path As String) As Integer
        If Not File.Exists(path) Then
            Return 0
        End If
        mainForm.ConversionInput.Visible = False
        LocalUseWaitCursor(mainForm, waitCursorEnable:=True)
        Dim sourceText As String
        Using myFileStream As FileStream = File.OpenRead(path)
            sourceText = myFileStream.GetFileTextFromStream()
        End Using

        Dim lines() As String = sourceText.SplitLines
        If mainForm.mnuOptionsColorizeSource.Checked Then
            Colorize(mainForm, GetClassifiedRanges(lines.Join(vbCrLf), LanguageNames.CSharp), mainForm.ConversionInput, lines.Length)
            mainForm.ConversionInput.Select(0, 0)
        Else
            mainForm.ConversionInput.Text = lines.Join(vbCrLf)
        End If
        mainForm.ConversionInput.Visible = True
        If mainForm.mnuViewShowSourceLineNumbers.Checked Then
            mainForm.LineNumbersForConversionInput.Visible = False
            Application.DoEvents()
            mainForm.LineNumbersForConversionInput.Visible = True
            Application.DoEvents()
        End If
        LocalUseWaitCursor(mainForm, waitCursorEnable:=False)
        Return lines.Length
    End Function

    Friend Sub LoadOutputBufferFromStream(mainForm As Form1, path As String)
        LocalUseWaitCursor(mainForm, waitCursorEnable:=True)
        Dim sourceText As String
        Using myFileStream As FileStream = File.OpenRead(path)
            sourceText = myFileStream.GetFileTextFromStream()
        End Using

        Dim lines() As String = sourceText.SplitLines
        If mainForm.mnuOptionsColorizeSource.Checked Then
            Colorize(mainForm, GetClassifiedRanges(lines.Join(vbCrLf), LanguageNames.VisualBasic), mainForm.ConversionOutput, lines.Length)
            mainForm.ConversionOutput.Select(0, 0)
        Else
            mainForm.ConversionOutput.Text = lines.Join(vbCrLf)
        End If
        LocalUseWaitCursor(mainForm, waitCursorEnable:=False)
    End Sub

    Friend Sub OpenSourceFile(mainForm As Form1, path As String)
        Dim lines As Integer = LoadInputBufferFromStream(mainForm, path)
        If lines = 0 Then
            mainForm.mnuConvertConvertSnippet.Enabled = False
            mainForm.mnuConvertConvertTopLevelStmts.Enabled = False
            If My.Settings.MRU_Data.Contains(path) Then
                My.Settings.MRU_Data.Remove(path)
            End If
        Else
            mainForm.mnuConvertConvertSnippet.Enabled = True
            mainForm.mnuConvertConvertTopLevelStmts.Enabled = True
            My.Settings.MRU_Data.MnuAddToMru(path)
        End If
        mainForm.mnuFile.DropDownItems.FileMenuMruUpdateUi(AddressOf mainForm.mnu_MRUList_Click)
        mainForm.UpdateLastFileMenu()
    End Sub

End Module
