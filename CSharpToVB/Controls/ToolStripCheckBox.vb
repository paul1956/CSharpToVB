' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Public Class ToolStripCheckBox
    Inherits ToolStripControlHost

    <CodeAnalysis.SuppressMessage("Reliability", "CA2000:Dispose objects before losing scope", Justification:="No reason, it is disposed on close")>
    Public Sub New()
        MyBase.New(New CheckBox)
    End Sub

    Private ReadOnly Property CheckBoxControl() As CheckBox
        Get
            Return CType(Me.Control, CheckBox)
        End Get
    End Property

    'Expose the CheckBoxControl's Checked Property
    Public Property Checked() As Boolean
        Get
            Return Me.CheckBoxControl.Checked
        End Get
        Set(value As Boolean)
            Me.CheckBoxControl.Checked = value
        End Set
    End Property

    'Subscribe and Unsubscribe the events you wish to expose
    Protected Overrides Sub OnSubscribeControlEvents(control As Control)
        If control Is Nothing Then
            Throw New ArgumentNullException(NameOf(control))
        End If
        'Connect the base events
        MyBase.OnSubscribeControlEvents(control)

        'Cast the control to a ChckBox control
        Dim checkBoxControl As CheckBox = CType(control, CheckBox)
        'Add any events you want to expose
        AddHandler checkBoxControl.CheckedChanged, AddressOf Me.CheckedChangedHandler
    End Sub

    Protected Overrides Sub OnUnsubscribeControlEvents(control As Control)
        If control Is Nothing Then
            Throw New ArgumentNullException(NameOf(control))
        End If
        'Disconnect the base events
        MyBase.OnUnsubscribeControlEvents(control)

        'Cast the control to a CheckBox control
        Dim checkBoxControl As CheckBox = CType(control, CheckBox)
        'Remove any events you have exposed
        RemoveHandler checkBoxControl.CheckedChanged, AddressOf Me.CheckedChangedHandler
    End Sub

    'Declare the event
    Public Event CheckedChanged As EventHandler

    'Raise the event
    Private Sub CheckedChangedHandler(sender As Object, e As EventArgs)
        RaiseEvent CheckedChanged(Me, e)
    End Sub

End Class
