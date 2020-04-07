' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Public NotInheritable Class AboutBox1

    Private Sub AboutBox1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        ' Set the title of the form.
        Dim ApplicationTitle As String = If(My.Info.Title, IO.Path.GetFileNameWithoutExtension(My.Info.AssemblyName))
        Text = $"About {ApplicationTitle}"
        ' Initialize all of the text displayed on the About Box.

        LabelProductName.Text = $"{My.Info.ProductName}"
        LabelVersion.Text = $"Version {My.Info.Version}"

        LabelCopyright.Text = My.Info.Copyright
        LabelCompanyName.Text = $"Developer {My.Info.CompanyName}"
        Dim coreinfo As New Microsoft.VisualBasic.ApplicationServices.AssemblyInfo(GetType(CSharpToVBCodeConverter.CodeWithOptions).Assembly)
        TextBoxDescription.Text = $"{My.Info.Description}{vbCrLf}{vbCrLf}{coreinfo.ProductName} {coreinfo.Version}"
    End Sub

    Private Sub OKButton_Click(sender As Object, e As EventArgs) Handles OKButton.Click
        Close()
    End Sub

End Class
