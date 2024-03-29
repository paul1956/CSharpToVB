﻿' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports Microsoft.VisualBasic.ApplicationServices

Public NotInheritable Class AboutBox1
    Private ReadOnly _enableDarkMode As Boolean = False

    Public Sub New(lightMode As Boolean)
        Me.InitializeComponent()
        _enableDarkMode = Not lightMode
    End Sub

    Private Sub AboutBox1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        DarkMode.ToggleImmersiveDarkMode(Me.Handle, _enableDarkMode)
        ' Set the title of the form.
        Dim applicationTitle As String = If(My.Application.Info.Title, IO.Path.GetFileNameWithoutExtension(My.Application.Info.AssemblyName))
        Me.Text = $"About {applicationTitle}"
        ' Initialize all of the text displayed on the About Box.

        Me.LabelProductName.Text = $"{My.Application.Info.ProductName}"
        Me.LabelVersion.Text = $"Version {My.Application.Info.Version}"

        Me.LabelCopyright.Text = My.Application.Info.Copyright
        Me.LabelCompanyName.Text = $"Developer {My.Application.Info.CompanyName}"
        Dim codeConverterInfo As New AssemblyInfo(GetType(CodeWithOptions).Assembly)
        Dim objectPoolLibrary As New AssemblyInfo(GetType(ObjectPoolLibrary.ObjectPool(Of String)).Assembly)
        Dim progressReportLibrary As New AssemblyInfo(GetType(ProgressReportLibrary.ProgressReport).Assembly)
        Me.TextBoxDescription.Text = $"{My.Application.Info.Description}

{codeConverterInfo.ProductName} {codeConverterInfo.Version}
{objectPoolLibrary.ProductName} {objectPoolLibrary.Version}
{progressReportLibrary.ProductName} {progressReportLibrary.Version}"
    End Sub

    Private Sub OKButton_Click(sender As Object, e As EventArgs) Handles OKButton.Click
        Me.Close()
    End Sub

End Class
