' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

Public Class OptionsDialog
    Private SelectedColor As Color
    Private SelectedColorName As String = "default"

    Private Sub Cancel_Button_Click(sender As Object, e As EventArgs) Handles Cancel_Button.Click
        DialogResult = DialogResult.Cancel
        Close()
    End Sub

    Private Sub ItemColor_ComboBox_DrawItem(sender As Object, e As DrawItemEventArgs) Handles ItemColor_ComboBox.DrawItem
        If e.Index >= 0 Then
            Dim n As String = CType(sender, ComboBox).Items(e.Index).ToString()
            Using f As New Font("Arial", 9, FontStyle.Regular)
                Dim c As Color = ColorSelector.GetColorFromName(n)
                Using b As Brush = New SolidBrush(c)
                    Dim rect As Rectangle = e.Bounds
                    Dim g As Graphics = e.Graphics
                    g.DrawString(n, f, Brushes.Black, rect.X, rect.Top)
                    g.FillRectangle(b, rect.X + 220, rect.Y + 2, rect.Width - 10, rect.Height - 6)
                End Using
            End Using
        End If
    End Sub

    Private Sub ItemColor_ComboBox_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ItemColor_ComboBox.SelectedIndexChanged
        SelectedColorName = CStr(ItemColor_ComboBox.SelectedItem)
        SelectedColor = ColorSelector.GetColorFromName(SelectedColorName)
    End Sub

    Private Sub OK_Button_Click(sender As Object, e As EventArgs) Handles OK_Button.Click
        My.Settings.DefaultProjectDirectory = CType(ProjectDirectoryList.SelectedItem, MyListItem).Value
        My.Settings.Save()
        DialogResult = DialogResult.OK
        ColorSelector.WriteColorDictionaryToFile()
        Application.DoEvents()
        Close()
    End Sub

    Private Sub OptionsDialog_Load(sender As Object, e As EventArgs) Handles Me.Load
        ProjectDirectoryList.Items.Add(New MyListItem("Projects", GetLatestVisualStudioProjectPath))
        ProjectDirectoryList.Items.Add(New MyListItem("Repos", GetAlternetVisualStudioProjectsPath))
        For i As Integer = 0 To ProjectDirectoryList.Items.Count - 1
            If CType(ProjectDirectoryList.Items(i), MyListItem).Value = My.Settings.DefaultProjectDirectory Then
                ProjectDirectoryList.SelectedIndex = i
                Exit For
            End If
        Next
        For Each Name As String In ColorSelector.GetColorNameList()
            ItemColor_ComboBox.Items.Add(Name)
        Next Name
        ItemColor_ComboBox.SelectedIndex = ItemColor_ComboBox.FindStringExact("default")
    End Sub

    Private Sub UpdateColor_Button_Click(sender As Object, e As EventArgs) Handles UpdateColor_Button.Click
        ColorDialog1.Color = SelectedColor
        If ColorDialog1.ShowDialog <> DialogResult.Cancel Then
            ColorSelector.SetColor(ItemColor_ComboBox.Items(ItemColor_ComboBox.SelectedIndex).ToString, ColorDialog1.Color)
            Application.DoEvents()
        End If
    End Sub

    Private Class MyListItem

        Public Sub New(pText As String, pValue As String)
            _Text = pText
            _Value = pValue
        End Sub

        Public ReadOnly Property Text() As String

        Public ReadOnly Property Value() As String

        Public Overrides Function ToString() As String
            Return Text
        End Function

    End Class

End Class
