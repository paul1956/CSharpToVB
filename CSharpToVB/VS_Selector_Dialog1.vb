' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

Imports Microsoft.Build.Locator

Public Class VS_Selector_Dialog1
    Private m_instance As VisualStudioInstance = Nothing
    Private ReadOnly visualStudioInstances() As VisualStudioInstance = MSBuildLocator.QueryVisualStudioInstances().ToArray()
    Public ReadOnly Property MSBuildInstance As VisualStudioInstance
        Get
            Return Me.m_instance
        End Get
    End Property

    Private Sub Cancel_Button_Click(ByVal sender As Object, ByVal e As EventArgs) Handles Cancel_Button.Click
        Me.DialogResult = DialogResult.Cancel
        Me.Close()
    End Sub

    Private Sub DataGridView1_SelectionChanged(sender As Object, e As EventArgs) Handles DataGridView1.SelectionChanged
        Me.m_instance = Me.visualStudioInstances(Me.DataGridView1.CurrentRow.Index)
    End Sub

    Private Sub OK_Button_Click(ByVal sender As Object, ByVal e As EventArgs) Handles OK_Button.Click
        If Me.m_instance Is Nothing Then
            Exit Sub
        End If
        Me.DialogResult = DialogResult.OK
        Me.Close()
    End Sub

    Private Sub VS_Selector_Dialog1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Dim BestIndex As Integer = -1
        Me.Text = "Multiple installs of MSBuild detected please select one:"
        Dim InstanceTable As New DataTable
        InstanceTable.Columns.Add("InstanceNumber")
        InstanceTable.Columns.Add("Edition")
        InstanceTable.Columns.Add("Version")
        InstanceTable.Columns.Add("MSBuildPath")
        For Index As Integer = 0 To Me.visualStudioInstances.Length - 1
            If Me.visualStudioInstances(Index).Name.Contains(" Build ") Then
                Continue For
            End If
            BestIndex = Index
            Dim rowToAdd As DataRow = InstanceTable.NewRow()
            rowToAdd("InstanceNumber") = Index + 1
            rowToAdd("Edition") = Me.visualStudioInstances(Index).Name
            rowToAdd("Version") = Me.visualStudioInstances(Index).Version
            rowToAdd("MSBuildPath") = Me.visualStudioInstances(Index).MSBuildPath
            InstanceTable.Rows.Add(rowToAdd)
        Next
        Me.DataGridView1.DataSource = InstanceTable
        If InstanceTable.Rows.Count = 1 Then
            Me.m_instance = Me.visualStudioInstances(BestIndex)
            Me.DialogResult = DialogResult.OK
            Me.Close()
        End If
    End Sub
End Class
