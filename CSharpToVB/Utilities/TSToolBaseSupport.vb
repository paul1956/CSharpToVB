' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports CSharpToVBConverter

Public Module TSToolBaseSupport

    <Flags>
    Public Enum SearchBuffers
        CS = 1
        VB = 2
        Both = CS Or VB
    End Enum

    <Extension>
    Friend Sub DoFind(MainForm As Form1, SearchForward As Boolean)
        If MainForm.TSFindFindWhatComboBox.Text = "Search..." Then
            Exit Sub
        End If
        mnuAddToMRU(My.Settings.TSFindMRU_Data, MainForm.TSFindFindWhatComboBox.Text)
        My.Settings.Save()
        TSFindWhatMRUUpdateUI(MainForm.TSFindFindWhatComboBox)
        Dim prompt As String = ""
        If MainForm.BufferToSearch.IsFlagSet(SearchBuffers.CS) AndAlso Not FindTextInBuffer(MainForm, MainForm.ConversionInput, SearchForward) Then
            prompt = $"'{MainForm.TSFindFindWhatComboBox.Text}' not found in C# code!"
        End If

        If MainForm.BufferToSearch.IsFlagSet(SearchBuffers.VB) AndAlso Not FindTextInBuffer(MainForm, MainForm.ConversionOutput, SearchForward) Then
            If prompt.Any Then
                prompt = $"'{MainForm.TSFindFindWhatComboBox.Text}' not found in C# or Visual Basic code!"
            Else
                prompt = $"'{MainForm.TSFindFindWhatComboBox.Text}' not found in Visual Basic code!"
            End If
        End If

        If prompt.Any Then
            MsgBox(prompt,
                   MsgBoxStyle.OkOnly Or MsgBoxStyle.Information Or MsgBoxStyle.MsgBoxSetForeground,
                   "Text Not Found!")
        End If

    End Sub

    ''' <summary>
    ''' Look in SearchBuffer for text and highlight it
    ''' No error is displayed if not found
    ''' </summary>
    ''' <param name="MainForm"></param>
    ''' <param name="SearchBuffer"></param>
    ''' <returns>True if found, False is not found</returns>
    ''' <param name="SearchForward"></param>
    Friend Function FindTextInBuffer(MainForm As Form1, SearchBuffer As RichTextBox, SearchForward As Boolean) As Boolean
        If SearchBuffer.SelectionStart >= SearchBuffer.Text.Length - 1 OrElse SearchBuffer.SelectionStart < 0 Then
            SearchBuffer.SelectionStart = If(SearchForward, 0, SearchBuffer.Text.Length - 1)
            SearchBuffer.SelectionLength = 0
        End If
        Dim findFrom As Integer = SearchBuffer.SelectionStart
        Dim findTo As Integer = SearchBuffer.TextLength - 1
        Dim options As RichTextBoxFinds = If(MainForm.TSFindMatchCaseCheckBox.Checked, RichTextBoxFinds.MatchCase, RichTextBoxFinds.None)
        options = options Or If(MainForm.TSFindMatchWholeWordCheckBox.Checked, RichTextBoxFinds.WholeWord, RichTextBoxFinds.None)
        If Not SearchForward Then
            options = options Or RichTextBoxFinds.Reverse
            findTo = findFrom
            findFrom = 0
        End If
        Dim findIndex As Integer = SearchBuffer.Find(MainForm.TSFindFindWhatComboBox.Text, findFrom, findTo, options)

        If findIndex < 0 Then
            SearchBuffer.SelectionStart = If(SearchForward, 0, MainForm.TSFindFindWhatComboBox.Text.Length - 1)
            SearchBuffer.SelectionLength = 0
            Return False
        End If
        SearchBuffer.SelectionStart = findIndex
        SearchBuffer.ScrollToCaret()
        SearchBuffer.SelectionBackColor = Color.Orange
        ' Find the end index. End Index = number of characters in textbox
        SearchBuffer.SelectionLength = MainForm.TSFindFindWhatComboBox.Text.Length
        ' Highlight the search string
        SearchBuffer.Select(SearchBuffer.SelectionStart, SearchBuffer.SelectionLength)
        Application.DoEvents()
        ' mark the start position after the position of
        ' last search string
        SearchBuffer.SelectionStart = If(SearchForward, SearchBuffer.SelectionStart + SearchBuffer.SelectionLength, SearchBuffer.SelectionStart)
        Return True
    End Function

    <Extension>
    Friend Function SetSearchControls(MainForm As Form1, Optional ReturnInputInUse As Boolean = False) As Boolean
        Dim inputBufferInUse As Boolean
        Dim outputBufferInUse As Boolean
        inputBufferInUse = MainForm.ConversionInput.Text.Any
        MainForm.mnuConvertConvertSnippet.Enabled = inputBufferInUse
        outputBufferInUse = MainForm.ConversionOutput.Text.Any
        Dim enableFind As Boolean = (inputBufferInUse Or outputBufferInUse) And MainForm.TSFindFindWhatComboBox.Text.Any
        MainForm.TSFindClearHighlightsButton.Enabled = enableFind
        MainForm.TSFindFindNextButton.Enabled = enableFind
        MainForm.TSFindFindPreviousButton.Enabled = enableFind
        MainForm.TSFindMatchCaseCheckBox.Enabled = enableFind
        MainForm.TSFindMatchWholeWordCheckBox.Enabled = enableFind
        Dim selectedIndex As Integer = MainForm.TSFindLookInComboBox.SelectedIndex
        If MainForm.TSFindLookInComboBox.Items.Count > 0 AndAlso MainForm.mnuConvert.Enabled Then
            If inputBufferInUse AndAlso outputBufferInUse Then
                MainForm.TSFindLookInComboBox.DropDownStyle = ComboBoxStyle.DropDown
                MainForm.TSFindLookInComboBox.SelectedIndex = selectedIndex
            ElseIf inputBufferInUse Then
                MainForm.TSFindLookInComboBox.DropDownStyle = ComboBoxStyle.Simple
            ElseIf outputBufferInUse Then
                MainForm.TSFindLookInComboBox.DropDownStyle = ComboBoxStyle.Simple
                MainForm.TSFindLookInComboBox.SelectedIndex = 1
            Else
                MainForm.TSFindLookInComboBox.DropDownStyle = ComboBoxStyle.Simple
                MainForm.TSFindLookInComboBox.SelectedIndex = 0
            End If
            MainForm.TSFindMatchCaseCheckBox.Enabled = enableFind
            MainForm.TSFindMatchWholeWordCheckBox.Enabled = enableFind
        End If
        Return If(ReturnInputInUse, inputBufferInUse, outputBufferInUse)
    End Function

End Module
