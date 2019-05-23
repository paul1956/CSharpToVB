' Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.
Option Explicit On
Option Infer Off
Option Strict On

Imports Microsoft.CodeAnalysis.Classification
Imports Microsoft.CodeAnalysis.Text

Public Class Range

    Public Sub New(classification As String, span As TextSpan, text As SourceText)
        Me.New(classification, span, text.GetSubText(span).ToString())
    End Sub

    Public Sub New(classification As String, span As TextSpan, text As String)
        Me.New(New ClassifiedSpan(classification, span), text)
    End Sub

    Public Sub New(classifiedSpan As ClassifiedSpan, text As String)
        Me._ClassifiedSpan = classifiedSpan
        Me._Text = text
    End Sub

    Public ReadOnly Property ClassificationType As String
        Get
            Return Me.ClassifiedSpan.ClassificationType
        End Get
    End Property

    Public ReadOnly Property ClassifiedSpan As ClassifiedSpan

    Public ReadOnly Property Text As String

    Public ReadOnly Property TextSpan As TextSpan
        Get
            Return Me.ClassifiedSpan.TextSpan
        End Get
    End Property

End Class