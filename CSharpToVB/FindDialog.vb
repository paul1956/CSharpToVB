' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports VBMsgBox

Public Class FindDialog
    Implements IMessageFilter

    Private ReadOnly _csBuffer As RichTextBox
    Private ReadOnly _rootForm As Form1
    Private ReadOnly _vbBuffer As RichTextBox

    Private _mouseInForm As Boolean = False
    Private _searchBuffer As SearchBuffers = SearchBuffers.CS

    Sub New(SourceBuffer As RichTextBox, ResultBuffer As AdvancedRTB, RootForm As Form1)
        ' This call is required by the designer.
        InitializeComponent()

        ' Add any initialization after the InitializeComponent() call.
        _vbBuffer = ResultBuffer
        _csBuffer = SourceBuffer
        _rootForm = RootForm
    End Sub

    <Flags>
    Enum SearchBuffers
        CS = 1
        VB = 2
        Both = CS Or VB
    End Enum

    Private Shared Sub MRU_UpdateUI(FindWhat As ComboBox)
        Dim FindItem As String = FindWhat.Text
        Dim index As Integer = FindWhat.Items.IndexOf(FindItem)
        If index >= 0 Then
            FindWhat.Items.RemoveAt(index)
        End If
        FindWhat.Items.Insert(0, FindItem)
        If FindWhat.Items.Count > 5 Then
            FindWhat.Items.RemoveAt(FindWhat.Items.Count - 1)
        End If
        FindWhat.SelectedItem = FindWhat.Items(0)
    End Sub

    Private Sub ClearHighlightsButton_Click(sender As Object, e As EventArgs) Handles ClearHighlightsButton.Click
        Dim selectionstart As Integer
        If _searchBuffer.IsFlagSet(SearchBuffers.CS) Then
            selectionstart = _csBuffer.SelectionStart
            _csBuffer.SelectAll()
            _csBuffer.SelectionBackColor = Color.White
            _csBuffer.Select(selectionstart, 0)
            _csBuffer.ScrollToCaret()
        End If
        If _searchBuffer.IsFlagSet(SearchBuffers.VB) Then
            selectionstart = _vbBuffer.SelectionStart
            _vbBuffer.SelectAll()
            _vbBuffer.SelectionBackColor = Color.White
            _vbBuffer.Select(selectionstart, 0)
            _vbBuffer.ScrollToCaret()
        End If
        Application.DoEvents()
    End Sub

    Private Sub CloseButton_Click(sender As Object, e As EventArgs) Handles CloseButton.Click
        _rootForm.mnuEditFind.Enabled = True
        Hide()
    End Sub

    Private Sub DoFind(SearchForward As Boolean)
        MRU_UpdateUI(FindWhatComboBox)
        Dim prompt As String = ""
        If _searchBuffer.IsFlagSet(SearchBuffers.CS) AndAlso Not FindTextInBuffer(_csBuffer, SearchForward) Then
            prompt = $"'{FindWhatComboBox.Text}' not found in C# code!"
        End If

        If _searchBuffer.IsFlagSet(SearchBuffers.VB) AndAlso Not FindTextInBuffer(_vbBuffer, SearchForward) Then
            If prompt.Any Then
                prompt = $"'{FindWhatComboBox.Text}' not found in C# or Visual Basic code!"
            Else
                prompt = $"'{FindWhatComboBox.Text}' not found in Visual Basic code!"
            End If
        End If

        If prompt.Any Then
            MsgBox(prompt,
                   MsgBoxStyle.OkOnly Or MsgBoxStyle.Information Or MsgBoxStyle.MsgBoxSetForeground,
                   "Text Not Found!")
        End If

    End Sub

    Private Sub SetEnableFindButtons()
        Dim enableFind As Boolean = FindWhatComboBox.Text.Any AndAlso
            (_csBuffer.Text.Any AndAlso _searchBuffer.IsFlagSet(SearchBuffers.CS) OrElse
            (_vbBuffer.Text.Any AndAlso _searchBuffer.IsFlagSet(SearchBuffers.VB)))
        ClearHighlightsButton.Enabled = enableFind
        FindNextButton.Enabled = enableFind
        FindPreviousButton.Enabled = enableFind
    End Sub

    Private Sub FindDialog_FormClosing(sender As Object, e As FormClosingEventArgs) Handles Me.FormClosing
        _rootForm.mnuEditFind.Enabled = True
        If e.CloseReason <> CloseReason.UserClosing Then
            Application.RemoveMessageFilter(Me)
        Else
            Hide()
            e.Cancel = True
        End If
    End Sub

    Private Sub FindDialog_GotFocus(sender As Object, e As EventArgs) Handles Me.GotFocus
        SetEnableFindButtons()
    End Sub

    Private Sub FindDialog_Load(sender As Object, e As EventArgs) Handles Me.Load
        Application.AddMessageFilter(Me)
        If _vbBuffer.Text.Any Then
            LookInComboBox.DropDownStyle = ComboBoxStyle.DropDownList
        Else
            LookInComboBox.DropDownStyle = ComboBoxStyle.Simple
        End If
        LookInComboBox.SelectedIndex = 0
    End Sub

    Private Sub FindDialog_MouseEnter(sender As Object, e As EventArgs) Handles Me.MouseEnter
        Focus()
        Opacity = 1
        If _vbBuffer.Text.Any Then
            LookInComboBox.DropDownStyle = ComboBoxStyle.DropDownList
        Else
            LookInComboBox.DropDownStyle = ComboBoxStyle.Simple
        End If

        SetEnableFindButtons()
    End Sub

    Private Sub FindDialog_MouseLeave(sender As Object, e As EventArgs) Handles Me.MouseLeave
        If Not _mouseInForm Then
            Opacity = 0.33
        End If
    End Sub

    Private Sub FindDialog_VisibleChanged(sender As Object, e As EventArgs) Handles Me.VisibleChanged
        If Visible Then
            Location = New Point(_rootForm.Location.X + _rootForm.ListBoxErrorList.Left, _rootForm.Location.Y + _rootForm.SplitContainer1.Top + _rootForm.SplitContainer1.Height - _rootForm.ListBoxFileList.Height)
        End If
    End Sub

    Private Sub FindNextButton_Click(sender As Object, e As EventArgs) Handles FindNextButton.Click
        DoFind(SearchForward:=True)
    End Sub

    Private Sub FindPreviousButton_Click(sender As Object, e As EventArgs) Handles FindPreviousButton.Click
        DoFind(SearchForward:=False)
    End Sub

    ''' <summary>
    ''' Look in SearchBuffer for text and highlight it
    ''' No error is displayed if not found
    ''' </summary>
    ''' <param name="SearchBuffer"></param>
    ''' <param name="StartLocation"></param>
    ''' <param name="SelectionTextLength"></param>
    ''' <returns>True if found, False is not found</returns>
    Private Function FindTextInBuffer(SearchBuffer As RichTextBox, SearchForward As Boolean) As Boolean
        If SearchBuffer.SelectionStart >= SearchBuffer.Text.Length - 1 OrElse SearchBuffer.SelectionStart < 0 Then
            SearchBuffer.SelectionStart = If(SearchForward, 0, SearchBuffer.Text.Length - 1)
            SearchBuffer.SelectionLength = 0
        End If
        Dim findFrom As Integer = SearchBuffer.SelectionStart
        Dim findTo As Integer = SearchBuffer.TextLength - 1
        Dim Options As RichTextBoxFinds = If(MatchCaseCheckBox.Checked, RichTextBoxFinds.MatchCase, RichTextBoxFinds.None)
        Options = Options Or If(MatchWholeWordCheckBox.Checked, RichTextBoxFinds.WholeWord, RichTextBoxFinds.None)
        If Not SearchForward Then
            Options = Options Or RichTextBoxFinds.Reverse
            findTo = findFrom
            findFrom = 0
        End If
        Dim findIndex As Integer = SearchBuffer.Find(FindWhatComboBox.Text, findFrom, findTo, Options)

        If findIndex < 0 Then
            SearchBuffer.SelectionStart = If(SearchForward, 0, FindWhatComboBox.Text.Length - 1)
            SearchBuffer.SelectionLength = 0
            Return False
        End If
        SearchBuffer.SelectionStart = findIndex
        SearchBuffer.ScrollToCaret()
        SearchBuffer.SelectionBackColor = Color.Orange
        ' Find the end index. End Index = number of characters in textbox
        SearchBuffer.SelectionLength = FindWhatComboBox.Text.Length
        ' Highlight the search string
        SearchBuffer.Select(SearchBuffer.SelectionStart, SearchBuffer.SelectionLength)
        Application.DoEvents()
        ' mark the start position after the position of
        ' last search string
        SearchBuffer.SelectionStart = If(SearchForward, SearchBuffer.SelectionStart + SearchBuffer.SelectionLength, SearchBuffer.SelectionStart)
        Return True
    End Function

    Private Sub FindWhatComboBox_SelectedIndexChanged(sender As Object, e As EventArgs) Handles FindWhatComboBox.SelectedIndexChanged
        SetEnableFindButtons()
    End Sub

    Private Sub FindWhatComboBox_TextChanged(sender As Object, e As EventArgs) Handles FindWhatComboBox.TextChanged
        SetEnableFindButtons()
    End Sub

    Private Sub LookInComboBox_SelectedIndexChanged(sender As Object, e As EventArgs) Handles LookInComboBox.SelectedIndexChanged
        Select Case LookInComboBox.SelectedIndex
            Case 0
                _searchBuffer = SearchBuffers.CS
            Case 1
                _searchBuffer = SearchBuffers.VB
            Case 2
                _searchBuffer = SearchBuffers.Both
        End Select
        SetEnableFindButtons()
    End Sub

    Private Sub MatchWholeWordCheckBox_CheckedChanged(sender As Object, e As EventArgs)

    End Sub

    Public Function PreFilterMessage(ByRef m As Message) As Boolean Implements IMessageFilter.PreFilterMessage
        If m.Msg = WM_MOUSELEAVE OrElse m.Msg = WM_MOUSEMOVE Then
            'hit test the client rectangle
            'since WM_MOUSELEAVE does not provide the mouse location, use MousePosition
            Dim hit As Boolean = ClientRectangle.Contains(PointToClient(MousePosition))
            If hit Then
                If Not _mouseInForm Then
                    OnMouseEnter(Nothing)
                End If
                _mouseInForm = True
            Else
                If _mouseInForm Then
                    OnMouseLeave(Nothing)
                End If
                _mouseInForm = False
            End If
        End If
        Return False
    End Function

End Class
