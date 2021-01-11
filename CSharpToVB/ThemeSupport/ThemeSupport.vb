' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Module ThemeSupport

    Public Sub ChangeTheme(scheme As Dictionary(Of String, (ForeGround As Color, Background As Color)), container As Control.ControlCollection)
        For Each component As Control In container
            Dim isEnabled As Boolean = component.Enabled
            Dim isInToolStrip As Boolean = TypeOf component.Parent Is ToolStrip
            If TypeOf component Is Panel Then
                ChangeTheme(scheme, component.Controls)
                CheckAndSetColor(component, scheme, "PanelBoarderStyle", isInToolStrip, isEnabled)
            ElseIf TypeOf component Is MenuStrip Then
                ChangeTheme(scheme, component.Controls)
                For Each dropDownItem As Object In CType(component, MenuStrip).Items
                    If TypeOf dropDownItem Is ToolStripMenuItem Then
                        CheckAndSetColor(CType(dropDownItem, ToolStripMenuItem), scheme, "MenuItem", True, isEnabled)
                    End If
                Next
                CheckAndSetColor(component, scheme, "MenuStrip", isInToolStrip, isEnabled)
            ElseIf TypeOf component Is SplitContainer Then
                ChangeTheme(scheme, component.Controls)
                CheckAndSetColor(component, scheme, "SplitContainer", isInToolStrip, isEnabled)
            ElseIf TypeOf component Is StatusStrip Then
                ChangeTheme(scheme, component.Controls)
                CheckAndSetColor(component, scheme, "StatusBar", isInToolStrip, isEnabled)
            ElseIf TypeOf component Is ToolStrip Then
                ChangeTheme(scheme, component.Controls)
                CheckAndSetColor(component, scheme, "Toolbar", isInToolStrip, isEnabled)
            ElseIf TypeOf component Is ContextMenuStrip Then
                ChangeTheme(scheme, component.Controls)
                CheckAndSetColor(component, scheme, "FloatingMenu", isInToolStrip, isEnabled)
            ElseIf TypeOf component Is Button Then
                CheckAndSetColor(component, scheme, "Button", isInToolStrip, isEnabled)
            ElseIf TypeOf component Is CheckBox Then
                CheckAndSetColor(component, scheme, "CheckBox", isInToolStrip, isEnabled)
            ElseIf TypeOf component Is ComboBox Then
                CheckAndSetColor(component, scheme, "ComboBox", isInToolStrip, isEnabled)
            ElseIf TypeOf component Is Label Then
                CheckAndSetColor(component, scheme, "Label", isInToolStrip, isEnabled)
            ElseIf TypeOf component Is ListBox Then
                CheckAndSetColor(component, scheme, "ListBox", isInToolStrip, isEnabled)
            ElseIf TypeOf component Is ProgressBar Then
                CheckAndSetColor(component, scheme, "ProgressBar", isInToolStrip, isEnabled)
            ElseIf TypeOf component Is LineNumbersForRichTextBox Then
                CheckAndSetColor(component, scheme, "LineNumbersForRichTextBox", isInToolStrip, isEnabled)
            ElseIf TypeOf component Is RichTextBox Then
                CheckAndSetColor(component, scheme, "RichTextBox", isInToolStrip, isEnabled)
            ElseIf TypeOf component Is PictureBox Then
                ' skip for now
            ElseIf TypeOf component Is TextBox Then
                CheckAndSetColor(component, scheme, "TextBox", isInToolStrip, isEnabled)
            Else
                Stop
            End If
        Next
    End Sub

    Public Sub CheckAndSetColor(cont As Control, scheme As Dictionary(Of String, (ForeGround As Color, Background As Color)), ControlName As String, IsInToolStrip As Boolean, IsEnabled As Boolean)
        If IsInToolStrip Then
            ControlName = $"ToolStrip{ControlName}"
        End If
        If Not IsEnabled Then
            cont.Enabled = True
        End If
        Dim colors As (Foreground As Color, Background As Color) = scheme(ControlName)

        If cont.BackColor <> colors.Background Then
            cont.BackColor = colors.Background
        End If
        If cont.ForeColor <> colors.Foreground Then
            cont.ForeColor = colors.Foreground
        End If
        cont.Enabled = IsEnabled
    End Sub

    Public Sub CheckAndSetColor(cont As ToolStripItem, scheme As Dictionary(Of String, (ForeGround As Color, Background As Color)), ControlName As String, IsInToolStrip As Boolean, IsEnabled As Boolean)
        If IsInToolStrip Then
            ControlName = $"ToolStrip{ControlName}"
        End If
        If Not IsEnabled Then
            cont.Enabled = True
        End If
        Dim colors As (Foreground As Color, Background As Color) = scheme(ControlName)

        If TypeOf cont Is ToolStripMenuItem Then
            For Each item As ToolStripItem In CType(cont, ToolStripMenuItem).DropDownItems
                If TypeOf item Is ToolStripMenuItem Then
                    CheckAndSetColor(CType(item, ToolStripMenuItem), scheme, "MenuItem", True, IsEnabled)
                ElseIf TypeOf item Is ToolStripSeparator Then
                    If item.BackColor <> colors.Background Then
                        item.BackColor = colors.Background
                    End If
                    If item.ForeColor <> colors.Foreground Then
                        item.ForeColor = colors.Foreground
                    End If
                ElseIf TypeOf item Is ToolStripComboBox Then
                    If item.BackColor <> colors.Background Then
                        item.BackColor = colors.Background
                    End If
                    If item.ForeColor <> colors.Foreground Then
                        item.ForeColor = colors.Foreground
                    End If
                ElseIf TypeOf item Is ToolStripCheckBox Then
                    If item.BackColor <> colors.Background Then
                        item.BackColor = colors.Background
                    End If
                    If item.ForeColor <> colors.Foreground Then
                        item.ForeColor = colors.Foreground
                    End If
                Else
                    Stop
                End If
            Next
        End If

        If cont.BackColor <> colors.Background Then
            cont.BackColor = colors.Background
        End If
        If cont.ForeColor <> colors.Foreground Then
            cont.ForeColor = colors.Foreground
        End If
        cont.Enabled = IsEnabled
    End Sub

End Module
