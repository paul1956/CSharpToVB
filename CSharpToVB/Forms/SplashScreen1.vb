' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Globalization

Public NotInheritable Class SplashScreen1

    Private Sub SplashScreen1_Load(sender As Object, e As EventArgs) Handles Me.Load
        'Set up the dialog text at runtime according to the application's assembly information.

        Me.ApplicationTitle.Text = If(String.IsNullOrWhiteSpace(My.Application.Info.Title), IO.Path.GetFileNameWithoutExtension(My.Application.Info.AssemblyName), My.Application.Info.Title)

        'Format the version information using the text set into the Version control at design time as the
        '  formatting string.  This allows for effective localization if desired.
        '  Build and revision information could be included by using the following code and changing the
        '  Version control's design-time text to "Version {0}.{1:00}.{2}.{3}" or something similar.  See
        '  String.Format() in Help for more information.
        '
        Me.Version.Text = String.Format(CultureInfo.InvariantCulture, Me.Version.Text, My.Application.Info.Version.Major, My.Application.Info.Version.Minor, My.Application.Info.Version.Build, My.Application.Info.Version.Revision)

        'Copyright info
        Me.Copyright.Text = My.Application.Info.Copyright
    End Sub

End Class
