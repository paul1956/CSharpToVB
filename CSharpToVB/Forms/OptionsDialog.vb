' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.IO

Public Class OptionsDialog
    Private _selectedColor As (Foreground As Color, Background As Color)
    Private _selectedColorName As String = DefaultValue

    Public MainForm As Form1

    Private Sub Cancel_Button_Click(sender As Object, e As EventArgs) Handles Cancel_Button.Click
        Me.DialogResult = DialogResult.Cancel
        Me.Close()
    End Sub

    ' Handle the Apply event by setting all buffers' fonts to the chosen font.
    Private Sub FontDialog1_Apply(sender As Object, e As EventArgs) Handles FontDialog1.Apply
        MainForm.ConversionInput.Font = Me.FontDialog1.Font
        MainForm.ConversionOutput.Font = Me.FontDialog1.Font
        Me.SampleTextBox.Font = Me.FontDialog1.Font
        My.Settings.EditorFont = Me.FontDialog1.Font
        My.Settings.Save()
    End Sub

    Private Sub ItemColor_ComboBox_DrawItem(sender As Object, e As DrawItemEventArgs) Handles ItemColor_ComboBox.DrawItem
        If e.Index >= 0 Then
            Dim itemName As String = CType(sender, ComboBox).Items(e.Index).ToString()
            Dim itemColor As (Foreground As Color, Background As Color) = (ColorSelector.GetColorFromName(itemName), Color.White)

            Dim eBounds As Rectangle = e.Bounds
            Using b As Brush = New SolidBrush(Color.Black)
                Dim pt As New Point(eBounds.X, eBounds.Top)
                e.Graphics.FillRectangle(b, eBounds.X, eBounds.Y, eBounds.Width - 200, eBounds.Height)
                TextRenderer.DrawText(e.Graphics, itemName, Me.Font, pt, Color.Black, Color.White)
            End Using
            Using b As Brush = New SolidBrush(itemColor.Background)
                e.Graphics.FillRectangle(b, eBounds.X + 250, eBounds.Y, eBounds.Width - 250, eBounds.Height)
                TextRenderer.DrawText(e.Graphics, itemName, Me.Font, New Point(eBounds.X + 250, eBounds.Top), itemColor.Foreground, itemColor.Background)
            End Using
        End If
    End Sub

    Private Sub ItemColor_ComboBox_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ItemColor_ComboBox.SelectedIndexChanged
        _selectedColorName = CStr(Me.ItemColor_ComboBox.SelectedItem)
        _selectedColor = (ColorSelector.GetColorFromName(_selectedColorName), Color.White)
        Me.SampleTextBox.BackColor = _selectedColor.Background
        Me.SampleTextBox.ForeColor = _selectedColor.Foreground
    End Sub

    Private Sub OK_Button_Click(sender As Object, e As EventArgs) Handles OK_Button.Click
        My.Settings.DefaultProjectDirectory = CType(Me.ProjectDirectoryList.SelectedItem, MyListItem).Value
        Me.DialogResult = DialogResult.OK
        Me.Cursor = Cursors.WaitCursor
        Application.DoEvents()
        Me.Cursor = Cursors.Default
        My.Settings.EditorFont = MainForm.ConversionInput.Font
        My.Settings.EditorFontName = MainForm.ConversionInput.Font.Name
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
        Me.FontDialog1.Font = My.Settings.EditorFont

        Me.ProjectDirectoryList.Items.Add(New MyListItem("Projects", GetLatestVisualStudioProjectPath))
        Me.ProjectDirectoryList.Items.Add(New MyListItem("Repos", GetAlternetVisualStudioProjectsPath))
        Me.ProjectDirectoryList.SelectedIndex = 0
        For index As Integer = 0 To Me.ProjectDirectoryList.Items.Count - 1
            If CType(Me.ProjectDirectoryList.Items(index), MyListItem).Value = My.Settings.DefaultProjectDirectory Then
                Me.ProjectDirectoryList.SelectedIndex = index
                Exit For
            End If
        Next
        Me.ModeTextBox.Text = "Light Mode"
        For Each name As String In ColorSelector.GetColorNameList()
            Me.ItemColor_ComboBox.Items.Add(name)
        Next name
        Me.ItemColor_ComboBox.SelectedIndex = Me.ItemColor_ComboBox.FindStringExact(DefaultValue)

        Me.ComboBoxCompare.SelectedItem = My.Settings.OptionCompare
        Me.ComboBoxExplicit.SelectedItem = My.Settings.OptionExplicit
        Me.SampleTextBox.ForeColor = Color.Black
        Me.SampleTextBox.BackColor = Color.White

        Me.ComboBoxInfer.SelectedItem = My.Settings.OptionInfer
        Me.ComboBoxStrict.SelectedItem = My.Settings.OptionStrict
        Me.CheckBoxCompare.Checked = My.Settings.OptionCompareIncludeInCode
        Me.CheckBoxExplicit.Checked = My.Settings.OptionExplicitIncludeInCode
        Me.CheckBoxInfer.Checked = My.Settings.OptionInferIncludeInCode
        Me.CheckBoxTopLevelStatements.Checked = My.Settings.IncludeTopLevelStmtProtoInCode
        Me.CheckBoxStrict.Checked = My.Settings.OptionStrictIncludeInCode
    End Sub

    Private Sub ResetThemeButton_Click(sender As Object, e As EventArgs) Handles ResetThemeButton.Click
        If MessageBox.Show("You are about to reset the '{My.Forms.Form1.TSThemeButton.Text}' theme any customizations will be lost, are you sure?", "Confirm Theme Reset", MessageBoxButtons.OKCancel, MessageBoxIcon.Question) <> DialogResult.OK Then
            Exit Sub
        End If
        Dim executableDirectoryPath As String = Path.Combine(Path.GetDirectoryName(Application.ExecutablePath), "Assets")
    End Sub

    Private Sub SelectEditorFontButton_Click(sender As Object, e As EventArgs) Handles SelectEditorFontButton.Click
        Dim oldFont As Font = New Font(MainForm.ConversionInput.Font.Name, MainForm.ConversionInput.Font.SizeInPoints, FontStyle.Regular, GraphicsUnit.Point)
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
            Me.FontDialog1_Apply(Nothing, New System.EventArgs)

            ' If the Cancel button is clicked, set the buffers'
            ' fonts back to the original font.
        ElseIf result = DialogResult.Cancel Then
            MainForm.ConversionInput.Font = oldFont
            MainForm.ConversionOutput.Font = oldFont
            Me.SampleTextBox.Font = oldFont
        End If
    End Sub

    Private Sub UpdateForeground_Button_Click(sender As Object, e As EventArgs) Handles UpdateForeground_Button.Click
        Me.ColorDialog1.Color = _selectedColor.Foreground
        If Me.ColorDialog1.ShowDialog <> DialogResult.Cancel Then
            Me.SampleTextBox.ForeColor = Me.ColorDialog1.Color
            ColorSelector.s_colorMappingDictionary(Me.ItemColor_ComboBox.Items(Me.ItemColor_ComboBox.SelectedIndex).ToString) = Me.SampleTextBox.ForeColor
            Application.DoEvents()
        End If
    End Sub

End Class
