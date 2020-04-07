' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Public Class DefaultVBOptions
    Sub New(OptionCompare As String, OptionCompareInclude As Boolean, OptionExplicit As String, OptionExplicitInclude As Boolean, OptionInfer As String, OptionInferInclude As Boolean, OptionStrict As String, OptionStrictInclude As Boolean)
        Me.OptionCompare = OptionCompare
        Me.OptionCompareInclude = OptionCompareInclude
        Me.OptionExplicit = OptionExplicit
        Me.OptionExplicitInclude = OptionExplicitInclude
        Me.OptionInfer = OptionInfer
        Me.OptionInferInclude = OptionInferInclude
        Me.OptionStrict = OptionStrict
        Me.OptionStrictInclude = OptionStrictInclude
    End Sub

    Sub New()

    End Sub

    Public ReadOnly Property OptionCompare As String = "Text"
    Public ReadOnly Property OptionCompareInclude As Boolean = False
    Public ReadOnly Property OptionExplicit As String = "Off"
    Public ReadOnly Property OptionExplicitInclude As Boolean = True
    Public ReadOnly Property OptionInfer As String = "On"
    Public ReadOnly Property OptionInferInclude As Boolean = True
    Public ReadOnly Property OptionStrict As String = "Off"
    Public ReadOnly Property OptionStrictInclude As Boolean = True
End Class
