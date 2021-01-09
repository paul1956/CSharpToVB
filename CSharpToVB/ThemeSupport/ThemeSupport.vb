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
                CheckAndSetColor(component, scheme, "PanelBoarderStyle", isInToolStrip)
            ElseIf TypeOf component Is MenuStrip Then
                ChangeTheme(scheme, component.Controls)
                CheckAndSetColor(component, scheme, "MenuStrip", isInToolStrip)
            ElseIf TypeOf component Is SplitContainer Then
                ChangeTheme(scheme, component.Controls)
                CheckAndSetColor(component, scheme, "SplitContainer", isInToolStrip)
            ElseIf TypeOf component Is StatusStrip Then
                ChangeTheme(scheme, component.Controls)
                CheckAndSetColor(component, scheme, "StatusBar", isInToolStrip)
            ElseIf TypeOf component Is ToolStrip Then
                ChangeTheme(scheme, component.Controls)
                CheckAndSetColor(component, scheme, "Toolbar", isInToolStrip)
            ElseIf TypeOf component Is ContextMenuStrip Then
                ChangeTheme(scheme, component.Controls)
                CheckAndSetColor(component, scheme, "FloatingMenu", isInToolStrip)
            ElseIf TypeOf component Is Button Then
                CheckAndSetColor(component, scheme, "Button", isInToolStrip)
            ElseIf TypeOf component Is CheckBox Then
                CheckAndSetColor(component, scheme, "CheckBox", isInToolStrip)
            ElseIf TypeOf component Is ComboBox Then
                CheckAndSetColor(component, scheme, "ComboBox", isInToolStrip)
            ElseIf TypeOf component Is Label Then
                CheckAndSetColor(component, scheme, "Label", isInToolStrip)
            ElseIf TypeOf component Is ListBox Then
                CheckAndSetColor(component, scheme, "ListBox", isInToolStrip)
            ElseIf TypeOf component Is ProgressBar Then
                CheckAndSetColor(component, scheme, "ProgressBar", isInToolStrip)
            ElseIf TypeOf component Is LineNumbersForRichTextBox Then
                CheckAndSetColor(component, scheme, "LineNumbersForRichTextBox", isInToolStrip)
            ElseIf TypeOf component Is RichTextBox Then
                CheckAndSetColor(component, scheme, "RichTextBox", isInToolStrip)
            Else
                Stop
            End If
        Next
    End Sub

    Sub CheckAndSetColor(cont As Control, scheme As Dictionary(Of String, (ForeGround As Color, Background As Color)), ControlName As String, IsInToolStrip As Boolean)
        If IsInToolStrip Then
            ControlName = $"ToolStrip{ControlName}"
        End If
        Dim colors As (Foreground As Color, Background As Color) = scheme(ControlName)

        If cont.BackColor <> colors.Background Then
            cont.BackColor = colors.Background
        End If
        If cont.ForeColor <> colors.Foreground Then
            cont.ForeColor = colors.Foreground
        End If
    End Sub
End Module
