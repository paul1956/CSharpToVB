Option Explicit On
Option Strict On

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
