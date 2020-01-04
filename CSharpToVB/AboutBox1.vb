Imports System.Reflection

Public NotInheritable Class AboutBox1

    Private Sub AboutBox1_Load(sender As System.Object, e As System.EventArgs) Handles MyBase.Load
        ' Set the title of the form.
        Dim ApplicationTitle As String
        Dim AssemplyTitle As String = GetType(AboutBox1).Assembly.GetCustomAttributes(Of AssemblyTitleAttribute).ToArray(0)?.Title
        Dim Company As String = GetType(AboutBox1).Assembly.GetCustomAttributes(Of AssemblyCompanyAttribute).ToArray(0)?.Company
        Dim Copyright As String = GetType(AboutBox1).Assembly.GetCustomAttributes(Of AssemblyCopyrightAttribute).ToArray(0)?.Copyright
        Dim Description As String = GetType(AboutBox1).Assembly.GetCustomAttributes(Of AssemblyDescriptionAttribute).ToArray(0)?.Description
        Dim FileVersion As String = GetType(AboutBox1).Assembly.GetCustomAttributes(Of AssemblyFileVersionAttribute).ToArray(0)?.Version
        Dim InformationVersion As String = GetType(AboutBox1).Assembly.GetCustomAttributes(Of AssemblyInformationalVersionAttribute).ToArray(0)?.InformationalVersion
        Dim Product As String = GetType(AboutBox1).Assembly.GetCustomAttributes(Of AssemblyProductAttribute).ToArray(0)?.Product
        Dim ExecutablePath As String = Application.ExecutablePath
        Dim FileInfo As FileVersionInfo = FileVersionInfo.GetVersionInfo(ExecutablePath)
        If Not String.IsNullOrEmpty(Product) Then
            ApplicationTitle = Product
        Else
            ApplicationTitle = IO.Path.GetFileNameWithoutExtension(ExecutablePath)
        End If
        Text = $"About {ApplicationTitle }"
        LabelProductName.Text = ApplicationTitle
        LabelVersion.Text = $"Version {Application.ProductVersion}"
        LabelCompanyName.Text = Application.CompanyName
        LabelCopyright.Text = Copyright
        TextBoxDescription.Text = Description
    End Sub

    Private Sub OKButton_Click(sender As System.Object, e As System.EventArgs) Handles OKButton.Click
        Close()
    End Sub

End Class
