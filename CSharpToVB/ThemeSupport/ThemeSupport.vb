' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Module ThemeSupport
    Public Sub ChangeTheme(scheme As Dictionary(Of String, (ForeGround As Color, Background As Color)), container As Control.ControlCollection)
        For Each component As Control In container
            If TypeOf component Is Panel Then
                ChangeTheme(scheme, component.Controls)
                component.BackColor = scheme("Menu").Background
                component.ForeColor = scheme("MenuText").Background
            ElseIf TypeOf component Is MenuStrip Then
                ChangeTheme(scheme, component.Controls)
                component.BackColor = scheme("Menu").Background
                component.ForeColor = scheme("MenuText").Background
            ElseIf TypeOf component Is SplitContainer Then
                ChangeTheme(scheme, component.Controls)
                component.BackColor = scheme("SplitBar").Background
            ElseIf TypeOf component Is StatusStrip Then
                ChangeTheme(scheme, component.Controls)
                component.BackColor = scheme("StatusBarDefault").Background
                component.ForeColor = scheme("StatusBarDefault").ForeGround
            ElseIf TypeOf component Is ToolStrip Then
                ChangeTheme(scheme, component.Controls)
                component.BackColor = scheme("Toolbar").Background
            ElseIf TypeOf component Is ContextMenuStrip Then
                ChangeTheme(scheme, component.Controls)
                component.BackColor = scheme("FloatingMenu").Background
            ElseIf TypeOf component Is Button Then
                component.BackColor = scheme("Button").Background
                component.ForeColor = scheme("Button").ForeGround
            ElseIf TypeOf component Is CheckBox Then
                component.BackColor = scheme("CheckBox").Background
                component.ForeColor = scheme("CheckBox").ForeGround
            ElseIf TypeOf component Is ComboBox Then
                component.BackColor = scheme("ComboBox").Background
                component.ForeColor = scheme("ComboBox").ForeGround
            ElseIf TypeOf component Is Label Then
                component.BackColor = scheme("Label").Background
                component.ForeColor = scheme("Label").ForeGround
            ElseIf TypeOf component Is ListBox Then
                component.BackColor = scheme("ListBox").Background
                component.ForeColor = scheme("ListBox").ForeGround
            ElseIf TypeOf component Is ProgressBar Then
                component.BackColor = scheme("ProgressBar").Background
                component.ForeColor = scheme("ProgressBar").ForeGround
            ElseIf TypeOf component Is LineNumbersForRichTextBox Then
                component.BackColor = scheme("TextBox").Background
                component.ForeColor = scheme("TextBox").ForeGround
            ElseIf TypeOf component Is RichTextBox Then
                component.BackColor = scheme("TextBox").Background
                component.ForeColor = scheme("TextBox").ForeGround
            Else
                Stop
            End If
        Next
    End Sub

End Module
