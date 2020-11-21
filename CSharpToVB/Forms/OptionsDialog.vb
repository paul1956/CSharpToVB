' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Public Class OptionsDialog
    Private _selectedColor As Color
    Private _selectedColorName As String = "default"

    Private Sub Cancel_Button_Click(sender As Object, e As EventArgs) Handles Cancel_Button.Click
        Me.DialogResult = DialogResult.Cancel
        Me.Close()
    End Sub

    Private Sub ItemColor_ComboBox_DrawItem(sender As Object, e As DrawItemEventArgs) Handles ItemColor_ComboBox.DrawItem
        If e.Index >= 0 Then
            Dim n As String = CType(sender, ComboBox).Items(e.Index).ToString()
            Using f As New Font("Segoe UI", 9, FontStyle.Regular)
                Dim c As Color = ColorSelector.GetColorFromName(n)
                Using b As Brush = New SolidBrush(c)
                    Dim rect As Rectangle = e.Bounds
                    Dim g As Graphics = e.Graphics
                    g.DrawString(n, f, Brushes.Black, rect.X, rect.Top)
                    g.FillRectangle(b, rect.X + 250, rect.Y + 2, rect.Width - 10, rect.Height - 6)
                End Using
            End Using
        End If
    End Sub

    Private Sub ItemColor_ComboBox_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ItemColor_ComboBox.SelectedIndexChanged
        _selectedColorName = CStr(Me.ItemColor_ComboBox.SelectedItem)
        _selectedColor = ColorSelector.GetColorFromName(_selectedColorName)
    End Sub

    Private Sub OK_Button_Click(sender As Object, e As EventArgs) Handles OK_Button.Click
        My.Settings.DefaultProjectDirectory = CType(Me.ProjectDirectoryList.SelectedItem, MyListItem).Value
        My.Settings.Save()
        Me.DialogResult = DialogResult.OK
        Me.Cursor = Cursors.WaitCursor
        Application.DoEvents()
        ColorSelector.WriteColorDictionaryToFile()
        Me.Cursor = Cursors.Default
        My.Settings.OptionCompare = Me.ComboBoxCompare.SelectedItem.ToString
        My.Settings.OptionCompareIncludeInCode = Me.CheckBoxCompare.Checked
        My.Settings.OptionExplicit = Me.ComboBoxExplicit.SelectedItem.ToString
        My.Settings.OptionExplicitIncludeInCode = Me.CheckBoxExplicit.Checked
        My.Settings.OptionInfer = Me.ComboBoxInfer.SelectedItem.ToString
        My.Settings.OptionInferIncludeInCode = Me.CheckBoxInfer.Checked
        My.Settings.OptionStrict = Me.ComboBoxStrict.SelectedItem.ToString
        My.Settings.OptionStrictIncludeInCode = Me.CheckBoxStrict.Checked
        My.Settings.Save()
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
