' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On
Imports System.Security
Imports System.Windows.Forms

Partial Public Module Interaction

    Private Function GetTitleFromAssembly(ByVal CallingAssembly As Reflection.Assembly) As String
        Dim Title As String

        'Get the Assembly name of the calling assembly
        'Assembly.GetName requires PathDiscovery permission so we try this first
        'and if it throws we catch the security exception and parse the name
        'from the full assembly name
        Try
            Title = CallingAssembly.GetName().Name
        Catch ex As SecurityException
            Dim FullName As String = CallingAssembly.FullName
            'Find the text up to the first comma. Note, this fails if the assembly has a comma in its name
            Dim FirstCommaLocation As Integer = FullName.IndexOf(","c)
            'The name is not in the format we're expecting so return an empty string
            Title = If(FirstCommaLocation >= 0, FullName.Substring(0, FirstCommaLocation), "")
        End Try

        Return Title
    End Function

    Public Function MsgBox(ByVal Prompt As Object, Optional ByVal Buttons As MsgBoxStyle = MsgBoxStyle.OkOnly, Optional ByVal Title As Object = Nothing) As MsgBoxResult
        Dim sPrompt As String = Nothing
        Dim sTitle As String
        Dim vbhost As IVbHost
        Dim ParentWindow As IWin32Window = Nothing

        vbhost = HostServices.VBHost
        If Not vbhost Is Nothing Then
            ParentWindow = vbhost.GetParentWindow()
        End If

        'Only allow legal button combinations to be set, one choice from each group
        'These bit constants are defined in System.Windows.Forms.MessageBox
        'Low-order 4 bits (0x000f), legal values: 0, 1, 2, 3, 4, 5
        '     next 4 bits (0x00f0), legal values: 0, &H10, &H20, &H30, &H40
        '     next 4 bits (0x0f00), legal values: 0, &H100, &H200
        If ((Buttons And &HFI) > MsgBoxStyle.RetryCancel) OrElse ((Buttons And &HF0I) > MsgBoxStyle.Information) _
            OrElse ((Buttons And &HF00I) > MsgBoxStyle.DefaultButton3) Then
            Buttons = MsgBoxStyle.OkOnly
        End If

        Try
            If Not Prompt Is Nothing Then
                sPrompt = Convert.ToString(Prompt)
            End If
        Catch ex As StackOverflowException
            Throw ex
        Catch ex As OutOfMemoryException
            Throw ex
        Catch ex As Threading.ThreadAbortException
            Throw ex
        Catch
            Throw New ArgumentException(Utils.GetResourceString("Argument '{0}' cannot be converted to type '{1}'.", "Prompt", "String"))
        End Try

        Try
            If Title Is Nothing Then
                If vbhost Is Nothing Then
                    sTitle = GetTitleFromAssembly(Reflection.Assembly.GetCallingAssembly())
                Else
                    sTitle = vbhost.GetWindowTitle()
                End If
            Else
                sTitle = CStr(Title) 'allows the title to be an expression, e.g. msgbox(prompt, Title:=1+5)
            End If
        Catch ex As StackOverflowException
            Throw ex
        Catch ex As OutOfMemoryException
            Throw ex
        Catch ex As Threading.ThreadAbortException
            Throw ex
        Catch
            Throw New ArgumentException(Utils.GetResourceString("Argument '{0}' cannot be converted to type '{1}'.", "Title", "String"))
        End Try

        Return CType(MessageBox.Show(ParentWindow, sPrompt, sTitle,
             CType(Buttons And &HF, MessageBoxButtons),
             CType(Buttons And &HF0, MessageBoxIcon),
             CType(Buttons And &HF00, MessageBoxDefaultButton),
             CType(Buttons And &HFFFFF000, MessageBoxOptions)),
             MsgBoxResult)
    End Function
End Module
