' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports CSharpToVBConverter

#If Not NET Then

Imports VBMsgBox

#End If

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
        If MainForm._searchBuffer.IsFlagSet(SearchBuffers.CS) AndAlso Not FindTextInBuffer(MainForm, MainForm.ConversionInput, SearchForward) Then
            prompt = $"'{MainForm.TSFindFindWhatComboBox.Text}' not found in C# code!"
        End If

        If MainForm._searchBuffer.IsFlagSet(SearchBuffers.VB) AndAlso Not FindTextInBuffer(MainForm, MainForm.ConversionOutput, SearchForward) Then
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
        Dim Options As RichTextBoxFinds = If(MainForm.TSFindMatchCaseCheckBox.Checked, RichTextBoxFinds.MatchCase, RichTextBoxFinds.None)
        Options = Options Or If(MainForm.TSFindMatchWholeWordCheckBox.Checked, RichTextBoxFinds.WholeWord, RichTextBoxFinds.None)
        If Not SearchForward Then
            Options = Options Or RichTextBoxFinds.Reverse
            findTo = findFrom
            findFrom = 0
        End If
        Dim findIndex As Integer = SearchBuffer.Find(MainForm.TSFindFindWhatComboBox.Text, findFrom, findTo, Options)

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
        With MainForm
            inputBufferInUse = .ConversionInput.Text.Any
            .mnuConvertConvertSnippet.Enabled = inputBufferInUse
            outputBufferInUse = .ConversionOutput.Text.Any
            Dim EnableFind As Boolean = (inputBufferInUse Or outputBufferInUse) And .TSFindFindWhatComboBox.Text.Any
            .TSFindClearHighlightsButton.Enabled = EnableFind
            .TSFindFindNextButton.Enabled = EnableFind
            .TSFindFindPreviousButton.Enabled = EnableFind
            .TSFindMatchCaseCheckBox.Enabled = EnableFind
            .TSFindMatchWholeWordCheckBox.Enabled = EnableFind
            Dim selectedIndex As Integer = .TSFindLookInComboBox.SelectedIndex
            If inputBufferInUse AndAlso outputBufferInUse Then
                .TSFindLookInComboBox.DropDownStyle = ComboBoxStyle.DropDown
                .TSFindLookInComboBox.SelectedIndex = selectedIndex
            Else
                If .TSFindFindWhatComboBox.Items.Count > 0 Then
                    .TSFindLookInComboBox.DropDownStyle = ComboBoxStyle.Simple
                    .TSFindLookInComboBox.SelectedIndex = If(inputBufferInUse, 0, If(outputBufferInUse, 1, 0))
                End If
            End If
            .TSFindMatchCaseCheckBox.Enabled = EnableFind
            .TSFindMatchWholeWordCheckBox.Enabled = EnableFind
        End With
        Return If(ReturnInputInUse, inputBufferInUse, outputBufferInUse)
    End Function

End Module
