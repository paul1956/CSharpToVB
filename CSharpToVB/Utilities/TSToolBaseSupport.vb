' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices

Public Module TsToolBaseSupport

    <Flags>
    Public Enum LanguageBufferToSearch
        Csharp = 1
        VisualBasic = 2
    End Enum

    <Extension>
    Friend Sub DoFind(mainForm As Form1, searchForward As Boolean)
        If mainForm.TSFindFindWhatComboBox.Text = $"Search..." Then
            Exit Sub
        End If
        mainForm._inColorize = True
        My.Settings.TSFindWhatMRU_Data.MnuAddToMru(mainForm.TSFindFindWhatComboBox.Text)
        My.Settings.Save()
        mainForm.TSFindFindWhatComboBox.TsFindWhatMruUpdateUi()
        Dim prompt As String = ""
        If mainForm.LanguageBuffersToSearch.IsFlagSet(LanguageBufferToSearch.Csharp) AndAlso Not FindTextInBuffer(mainForm, mainForm.ConversionInput, searchForward) Then
            prompt = $"'{mainForm.TSFindFindWhatComboBox.Text}' not found in C# code!"
        End If

        If mainForm.LanguageBuffersToSearch.IsFlagSet(LanguageBufferToSearch.VisualBasic) AndAlso Not FindTextInBuffer(mainForm, mainForm.ConversionOutput, searchForward) Then
            If prompt.Any Then
                prompt = $"'{mainForm.TSFindFindWhatComboBox.Text}' not found in C# or Visual Basic code!"
            Else
                prompt = $"'{mainForm.TSFindFindWhatComboBox.Text}' not found in Visual Basic code!"
            End If
        End If

        If prompt.Any Then
            MsgBox(prompt,
                   MsgBoxStyle.OkOnly Or MsgBoxStyle.Information Or MsgBoxStyle.MsgBoxSetForeground,
                   "Text Not Found!")
        End If
        mainForm._inColorize = False
    End Sub

    ''' <summary>
    ''' Look in SearchBuffer for text and highlight it
    ''' No error is displayed if not found
    ''' </summary>
    ''' <param name="mainForm"></param>
    ''' <param name="searchBuffer"></param>
    ''' <returns>True if found, False is not found</returns>
    ''' <param name="searchForward"></param>
    Friend Function FindTextInBuffer(mainForm As Form1, searchBuffer As RichTextBox, searchForward As Boolean) As Boolean
        If searchBuffer.SelectionStart >= searchBuffer.Text.Length - 1 OrElse searchBuffer.SelectionStart < 0 Then
            searchBuffer.SelectionStart = If(searchForward, 0, searchBuffer.Text.Length - 1)
            searchBuffer.SelectionLength = 0
        End If
        Dim findFrom As Integer = searchBuffer.SelectionStart
        Dim findTo As Integer = searchBuffer.TextLength - 1
        Dim options As RichTextBoxFinds = If(mainForm.TSFindMatchCaseCheckBox.Checked, RichTextBoxFinds.MatchCase, RichTextBoxFinds.None)
        options = options Or If(mainForm.TSFindMatchWholeWordCheckBox.Checked, RichTextBoxFinds.WholeWord, RichTextBoxFinds.None)
        If Not searchForward Then
            options = options Or RichTextBoxFinds.Reverse
            findTo = findFrom
            findFrom = 0
        End If
        Dim findIndex As Integer = searchBuffer.Find(mainForm.TSFindFindWhatComboBox.Text, findFrom, findTo, options)

        If findIndex < 0 Then
            searchBuffer.SelectionStart = If(searchForward, 0, mainForm.TSFindFindWhatComboBox.Text.Length - 1)
            searchBuffer.SelectionLength = 0
            Return False
        End If
        searchBuffer.SelectionStart = findIndex
        searchBuffer.SelectionBackColor = Color.Orange
        ' Find the end index. End Index = number of characters in TextBox
        searchBuffer.SelectionLength = mainForm.TSFindFindWhatComboBox.Text.Length
        ' Highlight the search string
        searchBuffer.Select(searchBuffer.SelectionStart, searchBuffer.SelectionLength)
        ' mark the start position after the position of
        ' last search string
        searchBuffer.SelectionStart = If(searchForward, searchBuffer.SelectionStart + searchBuffer.SelectionLength, searchBuffer.SelectionStart)
        searchBuffer.ScrollToCaret()
        Application.DoEvents()
        Return True
    End Function

    <Extension>
    Friend Function SetSearchControls(mainForm As Form1) As Boolean
        Return mainForm.SetSearchControls(False)
    End Function

    <Extension>
    Friend Function SetSearchControls(mainForm As Form1, returnInputInUse As Boolean) As Boolean
        Dim inputBufferInUse As Boolean
        Dim outputBufferInUse As Boolean
        inputBufferInUse = mainForm.ConversionInput.Text.Any
        mainForm.mnuConvertConvertSnippet.Enabled = inputBufferInUse
        mainForm.mnuConvertConvertTopLevelStmts.Enabled = inputBufferInUse
        outputBufferInUse = mainForm.ConversionOutput.Text.Any
        Dim enableFind As Boolean = (inputBufferInUse Or outputBufferInUse) And mainForm.TSFindFindWhatComboBox.Text.Any
        mainForm.TSFindClearHighlightsButton.Enabled = enableFind
        mainForm.TSFindFindNextButton.Enabled = enableFind
        mainForm.TSFindFindPreviousButton.Enabled = enableFind
        mainForm.TSFindMatchCaseCheckBox.Enabled = enableFind
        mainForm.TSFindMatchWholeWordCheckBox.Enabled = enableFind
        Dim selectedIndex As Integer = mainForm.TSFindLookInComboBox.SelectedIndex
        If mainForm.TSFindLookInComboBox.Items.Count > 0 AndAlso mainForm.mnuConvert.Enabled Then
            If inputBufferInUse AndAlso outputBufferInUse Then
                mainForm.TSFindLookInComboBox.DropDownStyle = ComboBoxStyle.DropDown
                mainForm.TSFindLookInComboBox.SelectedIndex = selectedIndex
            ElseIf inputBufferInUse Then
                mainForm.TSFindLookInComboBox.DropDownStyle = ComboBoxStyle.Simple
            ElseIf outputBufferInUse Then
                mainForm.TSFindLookInComboBox.DropDownStyle = ComboBoxStyle.Simple
                mainForm.TSFindLookInComboBox.SelectedIndex = 1
            Else
                mainForm.TSFindLookInComboBox.DropDownStyle = ComboBoxStyle.Simple
                mainForm.TSFindLookInComboBox.SelectedIndex = 0
            End If
            mainForm.TSFindMatchCaseCheckBox.Enabled = enableFind
            mainForm.TSFindMatchWholeWordCheckBox.Enabled = enableFind
        End If
        Return If(returnInputInUse, inputBufferInUse, outputBufferInUse)
    End Function

End Module
