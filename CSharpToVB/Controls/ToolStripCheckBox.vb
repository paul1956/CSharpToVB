' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Public Class ToolStripCheckBox

    Public Sub New()
        MyBase.New(New CheckBox)
    End Sub

    'Declare the event
    Public Event CheckedChanged As EventHandler

    'Expose the CheckBoxControl's Checked Property
    Public Property Checked() As Boolean
        Get
            Return CType(Me.Control, CheckBox).Checked
        End Get
        Set(value As Boolean)
            CType(Me.Control, CheckBox).Checked = value
        End Set
    End Property

    'Raise the event
    Private Sub CheckedChangedHandler(sender As Object, e As EventArgs)
        RaiseEvent CheckedChanged(Me, e)
    End Sub

    'Subscribe and Unsubscribe the events you wish to expose
    Protected Overrides Sub OnSubscribeControlEvents(control As Control)
        If control Is Nothing Then
            Throw New ArgumentNullException(NameOf(control))
        End If
        'Connect the base events
        MyBase.OnSubscribeControlEvents(control)

        'Add any events you want to expose
        AddHandler CType(control, CheckBox).CheckedChanged, AddressOf Me.CheckedChangedHandler
    End Sub

    Protected Overrides Sub OnUnsubscribeControlEvents(control As Control)
        If control Is Nothing Then
            Throw New ArgumentNullException(NameOf(control))
        End If
        'Disconnect the base events
        MyBase.OnUnsubscribeControlEvents(control)

        'Remove any events you have exposed
        RemoveHandler CType(control, CheckBox).CheckedChanged, AddressOf Me.CheckedChangedHandler
    End Sub

End Class
