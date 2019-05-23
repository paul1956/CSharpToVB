Option Explicit On
Option Infer Off
Option Strict On

Imports System.Windows.Forms

Public Class OptionsDialog
    Private SelectedColor As Color
    Private SelectedColorName As String = "default"

    Private Sub Cancel_Button_Click(ByVal sender As Object, ByVal e As EventArgs) Handles Cancel_Button.Click
        Me.DialogResult = DialogResult.Cancel
        Me.Close()
    End Sub

    Private Sub ItemColor_ComboBox_DrawItem(sender As Object, e As DrawItemEventArgs) Handles ItemColor_ComboBox.DrawItem
        Dim g As Graphics = e.Graphics
        Dim rect As Rectangle = e.Bounds
        If e.Index >= 0 Then
            Dim n As String = CType(sender, ComboBox).Items(e.Index).ToString()
            Dim f As New Font("Arial", 9, FontStyle.Regular)
            Dim c As Color = ColorSelector.GetColorFromName(n)
            Dim b As Brush = New SolidBrush(c)
            g.DrawString(n, f, Brushes.Black, rect.X, rect.Top)
            g.FillRectangle(b, rect.X + 220, rect.Y + 2, rect.Width - 10, rect.Height - 6)
        End If
    End Sub

    Private Sub ItemColor_ComboBox_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ItemColor_ComboBox.SelectedIndexChanged
        Me.SelectedColorName = CStr(Me.ItemColor_ComboBox.SelectedItem)
        Me.SelectedColor = ColorSelector.GetColorFromName(Me.SelectedColorName)
    End Sub

    Private Sub OK_Button_Click(ByVal sender As Object, ByVal e As EventArgs) Handles OK_Button.Click
        My.Settings.DefaultProjectDirectory = CType(Me.ProjectDirectoryList.SelectedItem, MyListItem).Value
        My.Settings.Save()
        Me.DialogResult = DialogResult.OK
        ColorSelector.WriteColorDictionaryToFile()
        Application.DoEvents()
        Me.Close()
    End Sub

    Private Sub OptionsDialog_Load(sender As Object, e As EventArgs) Handles Me.Load
        Me.ProjectDirectoryList.Items.Add(New MyListItem("Projects", GetLatestVisualStudioProjectPath))
        Me.ProjectDirectoryList.Items.Add(New MyListItem("Repos", GetAlternetVisualStudioProjectsPath))
        For i As Integer = 0 To Me.ProjectDirectoryList.Items.Count - 1
            If CType(Me.ProjectDirectoryList.Items(i), MyListItem).Value = My.Settings.DefaultProjectDirectory Then
                Me.ProjectDirectoryList.SelectedIndex = i
                Exit For
            End If
        Next
        For Each Name As String In ColorSelector.GetColorNameList()
            Me.ItemColor_ComboBox.Items.Add(Name)
        Next Name
        Me.ItemColor_ComboBox.SelectedIndex = Me.ItemColor_ComboBox.FindStringExact("default")
    End Sub

    Private Sub UpdateColor_Button_Click(sender As Object, e As EventArgs) Handles UpdateColor_Button.Click
        Me.ColorDialog1.Color = Me.SelectedColor
        If Me.ColorDialog1.ShowDialog <> Windows.Forms.DialogResult.Cancel Then
            ColorSelector.SetColor(Me.ItemColor_ComboBox.Items(Me.ItemColor_ComboBox.SelectedIndex).ToString, Me.ColorDialog1.Color)
            Application.DoEvents()
        End If
    End Sub

    Private Class MyListItem
        Public Sub New(ByVal pText As String, ByVal pValue As String)
            Me._Text = pText
            Me._Value = pValue
        End Sub

        Public ReadOnly Property Text() As String

        Public ReadOnly Property Value() As String

        Public Overrides Function ToString() As String
            Return Me.Text
        End Function
    End Class

End Class
