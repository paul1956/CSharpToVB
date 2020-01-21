Option Explicit On
Option Infer Off
Option Strict On

' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.ComponentModel
Imports System.IO

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CSharp
Imports Microsoft.CodeAnalysis.Emit

Public Class OptionsDialog
    Private _selectedColor As Color
    Private _selectedColorName As String = "default"

    Private Sub Cancel_Button_Click(sender As Object, e As EventArgs) Handles Cancel_Button.Click
        DialogResult = DialogResult.Cancel
        Close()
    End Sub

    Private Sub CSharpFooterTextBox_Validating(sender As Object, e As CancelEventArgs) Handles CSharpFooterTextBox.Validating
        Dim OpenBracketCount As Integer = CSharpFooterTextBox.Text.Count("{"c)
        Dim CloseBracketCount As Integer = CSharpFooterTextBox.Text.Count("}"c)
        If CloseBracketCount < 3 Then
            VBMsgBox.MsgBox("There must be at least 3 '}' in the footer", MsgBoxStyle.Exclamation, "C# Footer Validation Error")
            e.Cancel = True
            Exit Sub
        End If
        If CloseBracketCount - OpenBracketCount < 1 Then
            VBMsgBox.MsgBox("There must be at least 3 more '}' then '{' in the footer", MsgBoxStyle.Exclamation, "C# Footer Validation Error")
            e.Cancel = True
            Exit Sub
        End If

    End Sub

    Private Sub CSharpHeaderTextBox_Validating(sender As Object, e As CancelEventArgs) Handles CSharpHeaderTextBox.Validating
        Dim OpenBracketCount As Integer = CSharpHeaderTextBox.Text.Count("{"c)
        Dim CloseBracketCount As Integer = CSharpHeaderTextBox.Text.Count("}"c)
        If OpenBracketCount < 3 Then
            VBMsgBox.MsgBox("There must be at least 3 '{' in the header", MsgBoxStyle.Exclamation, "C# Header Validation Error")
            e.Cancel = True
            Exit Sub
        End If
        If OpenBracketCount - CloseBracketCount < 3 Then
            VBMsgBox.MsgBox("There must be at least 3 more '{' then '}' in the header", MsgBoxStyle.Exclamation, "C# Header Validation Error")
            e.Cancel = True
            Exit Sub
        End If
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
        _selectedColorName = CStr(ItemColor_ComboBox.SelectedItem)
        _selectedColor = ColorSelector.GetColorFromName(_selectedColorName)
    End Sub

    Private Sub OK_Button_Click(sender As Object, e As EventArgs) Handles OK_Button.Click
        My.Settings.DefaultProjectDirectory = CType(ProjectDirectoryList.SelectedItem, MyListItem).Value
        My.Settings.Save()
        DialogResult = DialogResult.OK
        ColorSelector.WriteColorDictionaryToFile()
        Cursor = Cursors.WaitCursor
        Application.DoEvents()
        Dim CompileResult As (CompileSuccess As Boolean, EmitResult) = CompileCSharpString(CSharpHeaderTextBox.Text & vbLf & CSharpFooterTextBox.Text)

        If CompileResult.CompileSuccess Then
            My.Settings.BoilerPlateHeader = CSharpHeaderTextBox.Text
            My.Settings.BoilderPlateFooter = CSharpFooterTextBox.Text
            My.Settings.Save()
        Else
            DialogResult = DialogResult.None
            VBMsgBox.MsgBox(String.Join(vbCrLf, CompileResult.Item2.Diagnostics), MsgBoxStyle.Exclamation, "Compilation Failed")
            Cursor = Cursors.Default
            Application.DoEvents()
            Exit Sub
        End If
        Cursor = Cursors.Default
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
        CSharpHeaderTextBox.Text = My.Settings.BoilerPlateHeader
        CSharpFooterTextBox.Text = My.Settings.BoilderPlateFooter
    End Sub

    Private Sub UpdateColor_Button_Click(sender As Object, e As EventArgs) Handles UpdateColor_Button.Click
        ColorDialog1.Color = _selectedColor
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
