' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Public Class OptionsDialog
    Private _selectedColor As Color
    Private _selectedColorName As String = "default"

    Public MainForm As Form1

    Private Sub Cancel_Button_Click(sender As Object, e As EventArgs) Handles Cancel_Button.Click
        Me.DialogResult = DialogResult.Cancel
        Me.Close()
    End Sub

    ' Handle the Apply event by setting all buffers' fonts to the chosen font.
    Private Sub FontDialog1_Apply(sender As Object, e As EventArgs) Handles FontDialog1.Apply
        MainForm.ConversionInput.Font = Me.FontDialog1.Font
        MainForm.ConversionOutput.Font = Me.FontDialog1.Font
    End Sub

    Private Sub ItemColor_ComboBox_DrawItem(sender As Object, e As DrawItemEventArgs) Handles ItemColor_ComboBox.DrawItem
        If e.Index >= 0 Then
            Dim itemName As String = CType(sender, ComboBox).Items(e.Index).ToString()
            Using b As Brush = New SolidBrush(ColorSelector.GetColorFromName(itemName))
                Dim eBounds As Rectangle = e.Bounds
                Dim pt As New Point(eBounds.X, eBounds.Top)
                TextRenderer.DrawText(e.Graphics, itemName, Me.Font, pt, Color.Black)
                e.Graphics.FillRectangle(b, eBounds.X + 250, eBounds.Y + 2, eBounds.Width - 10, eBounds.Height - 6)
            End Using
        End If
    End Sub

    Private Sub ItemColor_ComboBox_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ItemColor_ComboBox.SelectedIndexChanged
        _selectedColorName = CStr(Me.ItemColor_ComboBox.SelectedItem)
        _selectedColor = ColorSelector.GetColorFromName(_selectedColorName)
    End Sub

    Private Sub OK_Button_Click(sender As Object, e As EventArgs) Handles OK_Button.Click
        My.Settings.DefaultProjectDirectory = CType(Me.ProjectDirectoryList.SelectedItem, MyListItem).Value
        Me.DialogResult = DialogResult.OK
        Me.Cursor = Cursors.WaitCursor
        Application.DoEvents()
        ColorSelector.WriteColorDictionaryToFile()
        Me.Cursor = Cursors.Default
        My.Settings.EditorFont = MainForm.ConversionInput.Font
        My.Settings.EditorFontName = MainForm.ConversionInput.Font.Name
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
        Me.ProjectDirectoryList.Items.Add(New MyListItem("Projects", GetLatestVisualStudioProjectPath))
        Me.ProjectDirectoryList.Items.Add(New MyListItem("Repos", GetAlternetVisualStudioProjectsPath))
        Me.ProjectDirectoryList.SelectedIndex = 0
        For index As Integer = 0 To Me.ProjectDirectoryList.Items.Count - 1
            If CType(Me.ProjectDirectoryList.Items(index), MyListItem).Value = My.Settings.DefaultProjectDirectory Then
                Me.ProjectDirectoryList.SelectedIndex = index
                Exit For
            End If
        Next
        For Each name As String In ColorSelector.GetColorNameList()
            Me.ItemColor_ComboBox.Items.Add(name)
        Next name
        Me.ItemColor_ComboBox.SelectedIndex = Me.ItemColor_ComboBox.FindStringExact("default")
        Me.ComboBoxCompare.SelectedItem = My.Settings.OptionCompare
        Me.ComboBoxExplicit.SelectedItem = My.Settings.OptionExplicit
        Me.ComboBoxInfer.SelectedItem = My.Settings.OptionInfer
        Me.ComboBoxStrict.SelectedItem = My.Settings.OptionStrict
        Me.CheckBoxCompare.Checked = My.Settings.OptionCompareIncludeInCode
        Me.CheckBoxExplicit.Checked = My.Settings.OptionExplicitIncludeInCode
        Me.CheckBoxInfer.Checked = My.Settings.OptionInferIncludeInCode
        Me.CheckBoxStrict.Checked = My.Settings.OptionStrictIncludeInCode
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
        ElseIf result = Global.System.Windows.Forms.DialogResult.Cancel Then
            MainForm.ConversionInput.Font = oldFont
            MainForm.ConversionOutput.Font = oldFont
        End If
    End Sub

    Private Sub UpdateColor_Button_Click(sender As Object, e As EventArgs) Handles UpdateColor_Button.Click
        Me.ColorDialog1.Color = _selectedColor
        If Me.ColorDialog1.ShowDialog <> DialogResult.Cancel Then
            ColorSelector.SetColor(Me.ItemColor_ComboBox.Items(Me.ItemColor_ComboBox.SelectedIndex).ToString, Me.ColorDialog1.Color)
            Application.DoEvents()
        End If
    End Sub

    Friend Class MyListItem

        Public Sub New(pText As String, pValue As String)
            _Text = pText
            _Value = pValue
        End Sub

        Public ReadOnly Property Text() As String

        Public ReadOnly Property Value() As String

        Public Overrides Function ToString() As String
            Return Me.Text
        End Function

    End Class

End Class
