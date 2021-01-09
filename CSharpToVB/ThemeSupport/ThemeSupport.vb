' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Module ThemeSupport
    Public Sub ChangeTheme(scheme As Dictionary(Of String, (ForeGround As Color, Background As Color)), container As Control.ControlCollection)
        For Each component As Control In container
            Dim enabled As Boolean = component.Enabled
            Dim isInToolStrip As Boolean = TypeOf component.Parent Is ToolStrip
            If TypeOf component Is Panel Then
                ChangeTheme(scheme, component.Controls)
                CheckAndSetColor(component, scheme, "PanelBoarderStyle", isInToolStrip, enabled)
            ElseIf TypeOf component Is MenuStrip Then
                ChangeTheme(scheme, component.Controls)
                CheckAndSetColor(component, scheme, "MenuStrip", isInToolStrip, enabled)
            ElseIf TypeOf component Is SplitContainer Then
                ChangeTheme(scheme, component.Controls)
                CheckAndSetColor(component, scheme, "SplitContainer", isInToolStrip, enabled)
            ElseIf TypeOf component Is StatusStrip Then
                ChangeTheme(scheme, component.Controls)
                CheckAndSetColor(component, scheme, "StatusBar", isInToolStrip, enabled)
            ElseIf TypeOf component Is ToolStrip Then
                ChangeTheme(scheme, component.Controls)
                CheckAndSetColor(component, scheme, "Toolbar", isInToolStrip, enabled)
            ElseIf TypeOf component Is ContextMenuStrip Then
                ChangeTheme(scheme, component.Controls)
                CheckAndSetColor(component, scheme, "FloatingMenu", isInToolStrip, enabled)
            ElseIf TypeOf component Is Button Then
                CheckAndSetColor(component, scheme, "Button", isInToolStrip, enabled)
            ElseIf TypeOf component Is CheckBox Then
                CheckAndSetColor(component, scheme, "CheckBox", isInToolStrip, enabled)
            ElseIf TypeOf component Is ComboBox Then
                CheckAndSetColor(component, scheme, "ComboBox", isInToolStrip, enabled)
            ElseIf TypeOf component Is Label Then
                CheckAndSetColor(component, scheme, "Label", isInToolStrip, enabled)
            ElseIf TypeOf component Is ListBox Then
                CheckAndSetColor(component, scheme, "ListBox", isInToolStrip, enabled)
            ElseIf TypeOf component Is ProgressBar Then
                CheckAndSetColor(component, scheme, "ProgressBar", isInToolStrip, enabled)
            ElseIf TypeOf component Is LineNumbersForRichTextBox Then
                CheckAndSetColor(component, scheme, "LineNumbersForRichTextBox", isInToolStrip, enabled)
            ElseIf TypeOf component Is RichTextBox Then
                CheckAndSetColor(component, scheme, "RichTextBox", isInToolStrip, enabled)
            Else
                Stop
            End If
        Next
    End Sub

    Sub CheckAndSetColor(cont As Control, scheme As Dictionary(Of String, (ForeGround As Color, Background As Color)), ControlName As String, IsInToolStrip As Boolean, Enabled As Boolean)
        If Not Enabled Then
            ControlName = $"{ControlName}Disabled"
        End If
        If IsInToolStrip Then
            ControlName = $"ToolStrip{ControlName}"
        End If
        Dim colors As (Foreground As Color, Background As Color) = scheme(ControlName)

        ' For light mode you should never hit a stop (when enabled) but you do
        ' Need to investigate
        If cont.BackColor <> colors.Background Then
            'Stop
            cont.BackColor = colors.Background
        End If
        If cont.ForeColor <> colors.Foreground Then
            'Stop
            cont.ForeColor = colors.Foreground
        End If
    End Sub
End Module
