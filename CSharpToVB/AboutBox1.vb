
Public NotInheritable Class AboutBox1

    Private Sub AboutBox1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        ' Set the title of the form.
        Dim ApplicationTitle As String = If(String.IsNullOrEmpty(My.Info.Title), IO.Path.GetFileNameWithoutExtension(My.Info.AssemblyName), My.Info.Title)
        Text = $"About {ApplicationTitle}"
        ' Initialize all of the text displayed on the About Box.
        ' TODO: Customize the application's assembly information in the "Application" pane of the project
        '    properties dialog (under the "Project" menu).
        LabelProductName.Text = My.Info.ProductName
        LabelVersion.Text = $"Version {My.Info.Version.ToString}"
        LabelCopyright.Text = My.Info.Copyright
        LabelCompanyName.Text = My.Info.CompanyName
        TextBoxDescription.Text = My.Info.Description
    End Sub

    Private Sub OKButton_Click(sender As Object, e As EventArgs) Handles OKButton.Click
        Close()
    End Sub

End Class
