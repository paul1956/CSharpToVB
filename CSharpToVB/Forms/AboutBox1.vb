' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports Microsoft.VisualBasic.ApplicationServices

Public NotInheritable Class AboutBox1

    Private Sub AboutBox1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        ' Set the title of the form.
        Dim applicationTitle As String = If(My.Application.Info.Title, IO.Path.GetFileNameWithoutExtension(My.Application.Info.AssemblyName))
        Me.Text = $"About {applicationTitle}"
        ' Initialize all of the text displayed on the About Box.

        Me.LabelProductName.Text = $"{My.Application.Info.ProductName}"
        Me.LabelVersion.Text = $"Version {My.Application.Info.Version}"

        Me.LabelCopyright.Text = My.Application.Info.Copyright
        Me.LabelCompanyName.Text = $"Developer {My.Application.Info.CompanyName}"
        Dim codeConverterInfo As New AssemblyInfo(GetType(CSharpToVBConverter.CodeWithOptions).Assembly)
        Dim hashLibrary As New AssemblyInfo(GetType(HashLibrary.CodeRefactoringHash).Assembly)
        Dim progressReportLibrary As New AssemblyInfo(GetType(ProgressReportLibrary.ProgressReport).Assembly)
        Me.TextBoxDescription.Text = $"{My.Application.Info.Description}

{codeConverterInfo.ProductName} {codeConverterInfo.Version}
{hashLibrary.ProductName} {hashLibrary.Version}
{progressReportLibrary.ProductName} {progressReportLibrary.Version}"
    End Sub

    Private Sub OKButton_Click(sender As Object, e As EventArgs) Handles OKButton.Click
        Me.Close()
    End Sub

End Class
