' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports Xunit

Namespace OptionsDialog.Test

    Public NotInheritable Class OptionsDialogTests

        Public Sub New()
        End Sub

        Public Shared ReadOnly Property ManualTestsEnabled() As Boolean
            Get
                Return Not String.IsNullOrEmpty(Environment.GetEnvironmentVariable("MANUAL_TESTS"))
            End Get
        End Property

        <ConditionalFact(NameOf(ManualTestsEnabled))>
        <PlatformSpecific(TestPlatforms.Windows)>
        Public Sub OptionsDialogTestWindows()
            Using o As New CSharpToVBApp.OptionsDialog
                o.ShowDialog()
            End Using
        End Sub

    End Class

End Namespace
