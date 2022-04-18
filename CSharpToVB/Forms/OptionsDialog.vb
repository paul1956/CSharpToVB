' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.IO

Public Class OptionsDialog
    Private _selectedColor As ColorDescriptor
    Private _selectedColorName As String = ThemeDefaultColor

    Public Property MainForm As Form1

    Private Sub Cancel_Button_Click(sender As Object, e As EventArgs) Handles Cancel_Button.Click
        Me.DialogResult = DialogResult.Cancel
        Me.Close()
    End Sub

    ' Handle the Apply event by setting all buffers' fonts to the chosen font.
    Private Sub FontDialog1_Apply(sender As Object, e As EventArgs) Handles FontDialog1.Apply
        Me.MainForm.ConversionInput.Font = Me.FontDialog1.Font
        Me.MainForm.ConversionOutput.Font = Me.FontDialog1.Font
        Me.SampleTextBox.Font = Me.FontDialog1.Font
        My.Settings.EditorFont = Me.FontDialog1.Font
        My.Settings.Save()
    End Sub

    Private Sub ItemColor_ComboBox_DrawItem(sender As Object, e As DrawItemEventArgs) Handles ItemColor_ComboBox.DrawItem
        If e.Index >= 0 Then
            Dim itemName As String = CType(sender, ComboBox).Items(e.Index).ToString()
            Dim itemColor As ColorDescriptor = GetColorFromName(itemName)

            Dim eBounds As Rectangle = e.Bounds
            Using b As Brush = New SolidBrush(DefaultColor.Background)
                Dim pt As New Point(eBounds.X, eBounds.Top)
                e.Graphics.FillRectangle(b, eBounds.X, eBounds.Y, eBounds.Width - 200, eBounds.Height)
                TextRenderer.DrawText(e.Graphics, itemName, Me.Font, pt, DefaultColor.Foreground, DefaultColor.Background)
            End Using
            Using b As Brush = New SolidBrush(itemColor.Background)
                e.Graphics.FillRectangle(b, eBounds.X + 250, eBounds.Y, eBounds.Width - 250, eBounds.Height)
                TextRenderer.DrawText(e.Graphics, itemName, Me.Font, New Point(eBounds.X + 250, eBounds.Top), itemColor.Foreground, itemColor.Background)
            End Using
        End If
    End Sub

    Private Sub ItemColor_ComboBox_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ItemColor_ComboBox.SelectedIndexChanged
        _selectedColorName = CStr(Me.ItemColor_ComboBox.SelectedItem)
        _selectedColor = GetColorFromName(_selectedColorName)
        Me.SampleTextBox.BackColor = _selectedColor.Background
        Me.SampleTextBox.ForeColor = _selectedColor.Foreground
    End Sub

    Private Sub OK_Button_Click(sender As Object, e As EventArgs) Handles OK_Button.Click
        My.Settings.DefaultProjectDirectory = CType(Me.ProjectDirectoryList.SelectedItem, MyListItem).Value
        Me.DialogResult = DialogResult.OK
        Me.Cursor = Cursors.WaitCursor
        Application.DoEvents()
        WriteColorDictionaryToFile()

        Me.Cursor = Cursors.Default
        My.Settings.EditorFont = Me.MainForm.ConversionInput.Font
        My.Settings.EditorFontName = Me.MainForm.ConversionInput.Font.Name
        My.Settings.IncludeTopLevelStmtProtoInCode = Me.CheckBoxTopLevelStatements.Checked
        My.Settings.OptionCompare = Me.ComboBoxCompare.SelectedItem.ToString
        My.Settings.OptionCompareIncludeInCode = Me.CheckBoxCompare.Checked
        My.Settings.OptionExplicit = Me.ComboBoxExplicit.SelectedItem.ToString
        My.Settings.OptionExplicitIncludeInCode = Me.CheckBoxExplicit.Checked
        My.Settings.OptionInfer = Me.ComboBoxInfer.SelectedItem.ToString
        My.Settings.OptionInferIncludeInCode = Me.CheckBoxInfer.Checked
        My.Settings.OptionStrict = Me.ComboBoxStrict.SelectedItem.ToString
        My.Settings.OptionStrictIncludeInCode = Me.CheckBoxStrict.Checked
        My.Settings.Save()
        Me.Cursor = Cursors.Default
        Application.DoEvents()
        Me.Close()
    End Sub

    Private Sub OptionsDialog_Load(sender As Object, e As EventArgs) Handles Me.Load
        DarkMode.ToggleImmersiveDarkMode(CType(Me.Controls(0).Parent, Form).Handle, True)
        Me.FontDialog1.Font = My.Settings.EditorFont

        Me.ProjectDirectoryList.Items.Add(New MyListItem("Projects", GetLatestVisualStudioProjectPath))
        Me.ProjectDirectoryList.Items.Add(New MyListItem("Repos", GetAlternateVisualStudioProjectsPath))
        Me.ProjectDirectoryList.SelectedIndex = 0
        For index As Integer = 0 To Me.ProjectDirectoryList.Items.Count - 1
            If CType(Me.ProjectDirectoryList.Items(index), MyListItem).Value = My.Settings.DefaultProjectDirectory Then
                Me.ProjectDirectoryList.SelectedIndex = index
                Exit For
            End If
        Next
        Me.ModeTextBox.Text = My.Forms.Form1.TSThemeButton.Text
        For Each colorName As String In GetColorNameList()
            Me.ItemColor_ComboBox.Items.Add(colorName)
        Next colorName
        Me.ItemColor_ComboBox.SelectedIndex = Me.ItemColor_ComboBox.FindStringExact(ThemeDefaultColor)

        Me.ComboBoxCompare.SelectedItem = My.Settings.OptionCompare
        Me.ComboBoxExplicit.SelectedItem = My.Settings.OptionExplicit
        Me.SampleTextBox.ForeColor = DefaultColor.Foreground
        Me.SampleTextBox.BackColor = DefaultColor.Background
        Me.ComboBoxInfer.SelectedItem = My.Settings.OptionInfer
        Me.ComboBoxStrict.SelectedItem = My.Settings.OptionStrict
        Me.CheckBoxCompare.Checked = My.Settings.OptionCompareIncludeInCode
        Me.CheckBoxExplicit.Checked = My.Settings.OptionExplicitIncludeInCode
        Me.CheckBoxInfer.Checked = My.Settings.OptionInferIncludeInCode
        Me.CheckBoxTopLevelStatements.Checked = My.Settings.IncludeTopLevelStmtProtoInCode
        Me.CheckBoxStrict.Checked = My.Settings.OptionStrictIncludeInCode
    End Sub

    Private Sub ResetThemeButton_Click(sender As Object, e As EventArgs) Handles ResetThemeButton.Click
        If MessageBox.Show($"You are about to reset the '{My.Forms.Form1.TSThemeButton.Text}' theme any customizations will be lost, are you sure?", $"Confirm Theme Reset", MessageBoxButtons.OKCancel, MessageBoxIcon.Question) <> DialogResult.OK Then
            Exit Sub
        End If
        Dim executableDirectoryPath As String = Path.Combine(Path.GetDirectoryName(Application.ExecutablePath), "Assets")

        Dim userColorFile As String
        If My.Settings.ColorMode.IsLightMode Then
            userColorFile = Path.Combine(FileIO.SpecialDirectories.MyDocuments, LightModeDictionaryFileName)
            LoadColorDictionaryFromFile(Path.Combine(executableDirectoryPath, LightModeDictionaryFileName), _lightModeColorDictionary)
            Me.MainForm.CurrentThemeDictionary = _lightModeColorDictionary
        Else
            userColorFile = Path.Combine(FileIO.SpecialDirectories.MyDocuments, DarkModeDictionaryFileName)
            LoadColorDictionaryFromFile(Path.Combine(executableDirectoryPath, DarkModeDictionaryFileName), _darkModeColorDictionary)
            Me.MainForm.CurrentThemeDictionary = _darkModeColorDictionary
        End If
        If File.Exists(userColorFile) Then
            File.Delete(userColorFile)
        End If
        DefaultColor = Me.MainForm.CurrentThemeDictionary(ThemeDefaultColor)
    End Sub

    Private Sub SelectEditorFontButton_Click(sender As Object, e As EventArgs) Handles SelectEditorFontButton.Click
        Dim oldFont As New Font(Me.MainForm.ConversionInput.Font.Name, Me.MainForm.ConversionInput.Font.SizeInPoints, FontStyle.Regular, GraphicsUnit.Point)
        ' Display the font being used in Conversion buffers
        Me.FontDialog1.Font = oldFont

        ' Set FontMustExist to true, which causes message box error
        ' if the user enters a font that does not exist.
        Me.FontDialog1.FontMustExist = True
        Me.FontDialog1.ShowApply = True
        ' Do not show effects such as Underline and Bold.
        Me.FontDialog1.ShowEffects = False
        ' Set a minimum and maximum size to be shown in the FontDialog.
        Me.FontDialog1.MinSize = 7
        Me.FontDialog1.MaxSize = 14

        ' Show the dialog and save the result.
        Dim result As DialogResult = Me.FontDialog1.ShowDialog()

        ' If The OK button in the Font dialog box is clicked,
        ' set all the conversion buffers' fonts to the chosen font by
        ' calling the FontDialog1_Apply method.
        If result = DialogResult.OK Then
            Me.FontDialog1_Apply(Nothing, New EventArgs)

            ' If the Cancel button is clicked, set the buffers'
            ' fonts back to the original font.
        ElseIf result = DialogResult.Cancel Then
            Me.MainForm.ConversionInput.Font = oldFont
            Me.MainForm.ConversionOutput.Font = oldFont
            Me.SampleTextBox.Font = oldFont
        End If
    End Sub

    Private Sub UpdateBackground_Button_Click(sender As Object, e As EventArgs) Handles UpdateBackground_Button.Click
        Me.ColorDialog1.Color = _selectedColor.Background
        If Me.ColorDialog1.ShowDialog <> DialogResult.Cancel Then
            Me.SampleTextBox.BackColor = Me.ColorDialog1.Color
            My.Forms.Form1.CurrentThemeDictionary(Me.ItemColor_ComboBox.Items(Me.ItemColor_ComboBox.SelectedIndex).ToString) = New ColorDescriptor(Me.SampleTextBox.ForeColor, Me.SampleTextBox.BackColor)
            Application.DoEvents()
        End If
    End Sub

    Private Sub UpdateForeground_Button_Click(sender As Object, e As EventArgs) Handles UpdateForeground_Button.Click
        Me.ColorDialog1.Color = _selectedColor.Foreground
        If Me.ColorDialog1.ShowDialog <> DialogResult.Cancel Then
            Me.SampleTextBox.ForeColor = Me.ColorDialog1.Color
            My.Forms.Form1.CurrentThemeDictionary(Me.ItemColor_ComboBox.Items(Me.ItemColor_ComboBox.SelectedIndex).ToString) = New ColorDescriptor(Me.SampleTextBox.ForeColor, Me.SampleTextBox.BackColor)
            Application.DoEvents()
        End If
    End Sub

End Class
