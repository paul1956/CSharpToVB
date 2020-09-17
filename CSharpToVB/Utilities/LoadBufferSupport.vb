' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.IO
Imports CSharpToVBConverter
Imports Microsoft.CodeAnalysis

Module LoadBufferSupport

    Friend Function LoadInputBufferFromStream(MainForm As Form1, Path As String) As Integer
        MainForm.ConversionInput.Visible = False
        LocalUseWaitCursor(MainForm, WaitCursorEnable:=True)
        Dim SourceText As String
        Using myFileStream As FileStream = File.OpenRead(Path)
            SourceText = GetFileTextFromStream(myFileStream)
        End Using

        Dim Lines() As String = SourceText.SplitLines
        If MainForm.mnuOptionsColorizeSource.Checked Then
            Colorize(MainForm, GetClassifiedRanges(Lines.Join(vbCrLf), LanguageNames.CSharp), MainForm.ConversionInput, Lines.Length)
        Else
            MainForm.ConversionInput.Text = Lines.Join(vbCrLf)
        End If
        MainForm.ConversionInput.Visible = True
        If MainForm.mnuViewShowSourceLineNumbers.Checked Then
            MainForm.LineNumbersForConversionInput.Visible = False
            Application.DoEvents()
            MainForm.LineNumbersForConversionInput.Visible = True
            Application.DoEvents()
        End If
        LocalUseWaitCursor(MainForm, WaitCursorEnable:=False)
        Return Lines.Length
    End Function

    Friend Sub LoadOutputBufferFromStream(Path As String, MainForm As Form1)
        LocalUseWaitCursor(MainForm, WaitCursorEnable:=True)
        Dim SourceText As String
        Using myFileStream As FileStream = File.OpenRead(Path)
            SourceText = GetFileTextFromStream(myFileStream)
        End Using

        Dim Lines() As String = SourceText.SplitLines
        If MainForm.mnuOptionsColorizeSource.Checked Then
            Colorize(MainForm, GetClassifiedRanges(Lines.Join(vbCrLf), LanguageNames.VisualBasic), MainForm.ConversionOutput, Lines.Length)
        Else
            MainForm.ConversionOutput.Text = Lines.Join(vbCrLf)
        End If
        LocalUseWaitCursor(MainForm, WaitCursorEnable:=False)
    End Sub

    Friend Sub OpenSourceFile(MainForm As Form1, Path As String)
        MainForm.mnuConvertConvertSnippet.Enabled = LoadInputBufferFromStream(MainForm, Path) <> 0
        mnuAddToMRU(My.Settings.MRU_Data, Path)
        FileMenuMRUUpdateUI(MainForm.mnuFile.DropDownItems, AddressOf MainForm.mnu_MRUList_Click)
        MainForm.UpdateLastFileMeus()
    End Sub

End Module
