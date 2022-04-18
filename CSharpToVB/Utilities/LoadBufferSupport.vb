' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.IO
Imports Microsoft.CodeAnalysis

Friend Module LoadBufferSupport

    Public Function LoadInputBufferFromStream(mainForm As Form1, path As String) As Integer
        If Not File.Exists(path) Then
            Return 0
        End If
        mainForm.ConversionInput.Visible = False
        Dim sourceText As String
        mainForm.LineNumbersForConversionInput.Visible = False
        Using myFileStream As FileStream = File.OpenRead(path)
            sourceText = myFileStream.GetFileTextFromStream()
        End Using

        Dim lines() As String = sourceText.SplitLines
        If mainForm.MenuOptionsColorizeSource.Checked Then
            Colorize(mainForm, GetClassifiedRanges(lines.Join(vbCrLf), LanguageNames.CSharp).ToList(), mainForm.ConversionInput, lines.Length)
            mainForm.ConversionInput.Select(0, 0)
        Else
            mainForm.ConversionInput.Text = lines.Join(vbCrLf)
        End If
        mainForm.ConversionInput.Visible = True
        mainForm.LineNumbersForConversionInput.Visible = mainForm.MenuViewShowSourceLineNumbers.Checked
        Application.DoEvents()
        Return lines.Length
    End Function

    Friend Sub LoadOutputBufferFromStream(mainForm As Form1, path As String)
        Dim sourceText As String
        mainForm.LineNumbersForConversionOutput.Visible = False
        Using myFileStream As FileStream = File.OpenRead(path)
            sourceText = myFileStream.GetFileTextFromStream()
        End Using

        Dim lines() As String = sourceText.SplitLines
        If mainForm.MenuOptionsColorizeSource.Checked Then
            Colorize(mainForm, GetClassifiedRanges(lines.Join(vbCrLf), LanguageNames.VisualBasic).ToList(), mainForm.ConversionOutput, lines.Length)
            mainForm.ConversionOutput.Select(0, 0)
        Else
            mainForm.ConversionOutput.Text = lines.Join(vbCrLf)
        End If
        mainForm.LineNumbersForConversionOutput.Visible = mainForm.MenuViewShowDestinationLineNumbers.Checked
    End Sub

    Friend Sub OpenSourceFile(mainForm As Form1, path As String)
        Dim lines As Integer = LoadInputBufferFromStream(mainForm, path)
        If lines = 0 Then
            mainForm.MenuConvertConvertSnippet.Enabled = False
            mainForm.MenuConvertConvertTopLevelStmts.Enabled = False
            If My.Settings.MRU_Data.Contains(path) Then
                My.Settings.MRU_Data.Remove(path)
            End If
        Else
            mainForm.MenuConvertConvertSnippet.Enabled = True
            mainForm.MenuConvertConvertTopLevelStmts.Enabled = True
            My.Settings.MRU_Data.MenuAddToMru(path)
        End If
        mainForm.MenuFile.DropDownItems.FileMenuMruUpdateUi(AddressOf mainForm.Menu_MRUList_Click)
        mainForm.UpdateLastFileMenu()
    End Sub

End Module
