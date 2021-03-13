' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Public Module ThemeSupport
    Friend Sub SetDarkMode(ctrl As Control.ControlCollection, ByRef mCapturedRenderer As ToolStripRenderer)
        ctrl(0).Parent.BackColor = Color.FromArgb(40, 40, 40)
        ctrl(0).Parent.ForeColor = Color.Silver

        For Each c As Control In ctrl

            If TypeOf c Is MenuStrip Then
                Dim m As MenuStrip = CType(c, MenuStrip)
                If mCapturedRenderer Is Nothing Then mCapturedRenderer = m.Renderer
                m.Renderer = New ToolStripProfessionalRenderer(New DarkColorTable)
                m.ForeColor = Color.Silver 'Color.White
                For Each item As ToolStripMenuItem In m.Items
                    For Each subItem As ToolStripItem In item.DropDownItems
                        Dim toolStripItem As ToolStripMenuItem = TryCast(subItem, ToolStripMenuItem)
                        If toolStripItem IsNot Nothing Then
                            toolStripItem.ForeColor = Color.Silver 'Color.White
                        End If
                    Next
                Next

            ElseIf TypeOf c Is StatusStrip Then
                Dim s As StatusStrip = CType(c, StatusStrip)
                s.BackColor = Color.FromArgb(40, 40, 40)
                s.ForeColor = Color.Silver

            ElseIf TypeOf c Is ToolStrip Then
                Dim ts As ToolStrip = CType(c, ToolStrip)
                ts.Renderer = New ToolStripProfessionalRenderer(New DarkColorTable)
                ts.ForeColor = Color.Silver 'Color.White
                SetDarkMode(ts.Controls, mCapturedRenderer)

            ElseIf TypeOf c Is CheckBox Then

            ElseIf TypeOf c Is ComboBox Then
                Dim cb As ComboBox = CType(c, ComboBox)
                cb.BackColor = Color.FromArgb(50, 50, 50)
                cb.ForeColor = Color.Silver 'Color.White
                Dim comboBoxEx As ComboBoxEx = TryCast(c, ComboBoxEx)
                If comboBoxEx IsNot Nothing Then
                    comboBoxEx.BorderColor = Color.FromArgb(60, 60, 60)
                End If

            ElseIf TypeOf c Is SplitContainer Then
                Dim s As SplitContainer = CType(c, SplitContainer)
                s.BackColor = Color.FromArgb(40, 40, 40)
                s.ForeColor = Color.Silver
                SetDarkMode(s.Panel1.Controls, mCapturedRenderer)
                SetDarkMode(s.Panel2.Controls, mCapturedRenderer)

            ElseIf TypeOf c Is TabControl Then
                Dim t As TabControl = CType(c, TabControl)
                t.BackColor = Color.FromArgb(40, 40, 40)
                t.ForeColor = Color.Silver
                For Each tab As TabPage In t.TabPages
                    tab.BackColor = Color.FromArgb(40, 40, 40)
                    tab.BorderStyle = BorderStyle.None
                    SetDarkMode(tab.Controls, mCapturedRenderer)
                Next

            ElseIf TypeOf c Is Panel Then
                Dim p As Panel = CType(c, Panel)
                p.BackColor = Color.FromArgb(40, 40, 40)
                p.ForeColor = Color.Silver
                p.Padding = New Padding(1)
                Dim panelEx As PanelEx = TryCast(c, PanelEx)
                If panelEx IsNot Nothing Then
                    panelEx.BorderColor = Color.FromArgb(60, 60, 60)
                End If
                SetDarkMode(p.Controls, mCapturedRenderer)

            ElseIf TypeOf c Is RichTextBox Then
                Dim rtb As RichTextBox = CType(c, RichTextBox)
                rtb.BackColor = Color.FromArgb(45, 45, 45)
                rtb.ForeColor = Color.Silver
                rtb.BorderStyle = BorderStyle.None
                'If TypeOf rtb.Parent Is PanelEx Then
                '  CType(rtb.Parent, PanelEx).BorderColor = Color.FromArgb(60, 60, 60)
                'End If

            ElseIf TypeOf c Is TreeView Then
                Dim tvw As TreeView = CType(c, TreeView)
                tvw.BackColor = Color.FromArgb(40, 40, 40)
                tvw.ForeColor = Color.Silver
                tvw.BorderStyle = BorderStyle.None

            ElseIf TypeOf c Is TextBox Then
                Dim tb As TextBox = CType(c, TextBox)
                tb.BorderStyle = BorderStyle.FixedSingle
                tb.BackColor = Color.FromArgb(60, 60, 60)
                tb.ForeColor = Color.Silver

            ElseIf TypeOf c Is Button Then
                Dim btn As Button = CType(c, Button)
                btn.FlatStyle = FlatStyle.Flat
                btn.BackColor = Color.FromArgb(60, 60, 60)
                btn.ForeColor = Color.Silver
            ElseIf TypeOf c Is Label Then
                Dim lb As Label = CType(c, Label)
                lb.BorderStyle = BorderStyle.FixedSingle
                lb.BackColor = Color.FromArgb(60, 60, 60)
                lb.ForeColor = Color.Silver
            ElseIf TypeOf c Is ListBox Then
                Dim lb As ListBox = CType(c, ListBox)
                lb.BorderStyle = BorderStyle.FixedSingle
                lb.BackColor = Color.FromArgb(60, 60, 60)
                lb.ForeColor = Color.Silver
            ElseIf TypeOf c Is ProgressBar Then
                Dim pb As ProgressBar = CType(c, ProgressBar)
                pb.BackColor = Color.FromArgb(60, 60, 60)
                pb.ForeColor = Color.Silver
            ElseIf TypeOf c Is DataGridView Then
                ' ignore for now
            ElseIf TypeOf c Is LineNumbersForRichTextBox Then
                ' ignore
            ElseIf TypeOf c Is PictureBox Then
                ' ignore
            Else
                MsgBox($"Unhandled Control: {c}")
            End If

        Next
    End Sub

    Friend Sub SetLightMode(ctrl As Control.ControlCollection, ByRef mCapturedRenderer As ToolStripRenderer)
        ctrl(0).Parent.BackColor = SystemColors.Control
        ctrl(0).Parent.ForeColor = SystemColors.ControlText

        For Each c As Control In ctrl

            If TypeOf c Is MenuStrip Then
                Dim m As MenuStrip = CType(c, MenuStrip)
                If mCapturedRenderer IsNot Nothing Then
                    m.Renderer = mCapturedRenderer
                End If
                m.ForeColor = SystemColors.ControlText
                For Each item As ToolStripMenuItem In m.Items
                    For Each subItem As ToolStripItem In item.DropDownItems
                        Dim toolStripItem As ToolStripMenuItem = TryCast(subItem, ToolStripMenuItem)
                        If toolStripItem IsNot Nothing Then
                            toolStripItem.ForeColor = SystemColors.ControlText
                        End If
                    Next
                Next

            ElseIf TypeOf c Is StatusStrip Then
                Dim s As StatusStrip = CType(c, StatusStrip)
                s.BackColor = SystemColors.Control
                s.ForeColor = SystemColors.ControlText

            ElseIf TypeOf c Is ToolStrip Then
                Dim ts As ToolStrip = CType(c, ToolStrip)
                If mCapturedRenderer IsNot Nothing Then
                    ts.Renderer = mCapturedRenderer
                End If
                ts.ForeColor = SystemColors.ControlText
                SetLightMode(ts.Controls, mCapturedRenderer)

            ElseIf TypeOf c Is CheckBox Then

            ElseIf TypeOf c Is ComboBox Then
                Dim cb As ComboBox = CType(c, ComboBox)
                cb.BackColor = Color.White 'SystemColors.Control
                cb.ForeColor = SystemColors.ControlText
                Dim comboBoxEx As ComboBoxEx = TryCast(c, ComboBoxEx)
                If comboBoxEx IsNot Nothing Then
                    If TypeOf cb.Parent Is ToolStrip Then
                        comboBoxEx.BorderColor = SystemColors.Control
                    Else
                        comboBoxEx.BorderColor = SystemColors.ControlText
                    End If
                End If

            ElseIf TypeOf c Is SplitContainer Then
                Dim s As SplitContainer = CType(c, SplitContainer)
                s.BackColor = SystemColors.Control
                s.ForeColor = SystemColors.ControlText
                SetLightMode(s.Panel1.Controls, mCapturedRenderer)
                SetLightMode(s.Panel2.Controls, mCapturedRenderer)

            ElseIf TypeOf c Is TabControl Then
                Dim t As TabControl = CType(c, TabControl)
                t.BackColor = SystemColors.Control
                t.ForeColor = SystemColors.ControlText
                For Each tab As TabPage In t.TabPages
                    tab.BackColor = SystemColors.Control
                    SetLightMode(tab.Controls, mCapturedRenderer)
                Next

            ElseIf TypeOf c Is Panel Then
                Dim p As Panel = CType(c, Panel)
                p.BackColor = SystemColors.Control
                p.ForeColor = SystemColors.ControlText
                p.Padding = New Padding(1)
                Dim panelEx As PanelEx = TryCast(c, PanelEx)
                If panelEx IsNot Nothing Then
                    panelEx.BorderColor = SystemColors.WindowFrame
                End If
                SetLightMode(p.Controls, mCapturedRenderer)

            ElseIf TypeOf c Is RichTextBox Then
                Dim rtb As RichTextBox = CType(c, RichTextBox)
                rtb.BackColor = Color.White
                rtb.ForeColor = SystemColors.ControlText

            ElseIf TypeOf c Is TreeView Then
                Dim tvw As TreeView = CType(c, TreeView)
                tvw.BackColor = Color.White
                tvw.ForeColor = SystemColors.ControlText

            ElseIf TypeOf c Is TextBox Then
                Dim tb As TextBox = CType(c, TextBox)
                tb.BorderStyle = BorderStyle.FixedSingle
                tb.BackColor = Color.White
                tb.ForeColor = SystemColors.ControlText

            ElseIf TypeOf c Is Button Then
                Dim btn As Button = CType(c, Button)
                btn.BackColor = SystemColors.Control
                btn.ForeColor = SystemColors.ControlText

            ElseIf TypeOf c Is Label Then
                Dim lb As Label = CType(c, Label)
                lb.BorderStyle = BorderStyle.FixedSingle
                lb.BackColor = Color.White
                lb.ForeColor = SystemColors.ControlText

            ElseIf TypeOf c Is ListBox Then
                Dim lb As ListBox = CType(c, ListBox)
                lb.BorderStyle = BorderStyle.FixedSingle
                lb.BackColor = Color.White
                lb.ForeColor = SystemColors.ControlText

            ElseIf TypeOf c Is ProgressBar Then
                Dim pb As ProgressBar = CType(c, ProgressBar)
                pb.BackColor = Color.White
                pb.ForeColor = SystemColors.ControlText

            ElseIf TypeOf c Is DataGridView Then
                ' ignore for now
            ElseIf TypeOf c Is LineNumbersForRichTextBox Then
                ' ignore
            ElseIf TypeOf c Is PictureBox Then
                ' ignore
            Else
                MsgBox($"Unhandled Control: {c}")
            End If

        Next
    End Sub


End Module
