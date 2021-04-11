' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Public Class DefaultVbOptions

    Public Sub New(optionCompare As String, optionCompareInclude As Boolean, optionExplicit As String, optionExplicitInclude As Boolean, optionInfer As String, optionInferInclude As Boolean, optionStrict As String, optionStrictInclude As Boolean)
        Me.OptionCompare = optionCompare
        Me.OptionCompareInclude = optionCompareInclude
        Me.OptionExplicit = optionExplicit
        Me.OptionExplicitInclude = optionExplicitInclude
        Me.OptionInfer = optionInfer
        Me.OptionInferInclude = optionInferInclude
        Me.OptionStrict = optionStrict
        Me.OptionStrictInclude = optionStrictInclude
    End Sub

    Public Sub New()

    End Sub

    Public ReadOnly Property OptionCompare As String = "Text"
    Public ReadOnly Property OptionCompareInclude As Boolean
    Public ReadOnly Property OptionExplicit As String = "Off"
    Public ReadOnly Property OptionExplicitInclude As Boolean = True
    Public ReadOnly Property OptionInfer As String = "On"
    Public ReadOnly Property OptionInferInclude As Boolean = True
    Public ReadOnly Property OptionStrict As String = "Off"
    Public ReadOnly Property OptionStrictInclude As Boolean = True
End Class
