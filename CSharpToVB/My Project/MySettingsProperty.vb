Namespace My

    <HideModuleName(),
     DebuggerNonUserCode(),
     Runtime.CompilerServices.CompilerGenerated()>
    Friend Module MySettingsProperty

        <ComponentModel.Design.HelpKeyword("My.Settings")>
        Friend ReadOnly Property Settings() As MySettings
            Get
                Return MySettings.Default
            End Get
        End Property
    End Module
End Namespace
