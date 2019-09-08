﻿' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp

Namespace CSharpToVBCodeConverter

    Public Class CodeWithOptions

        Public Sub New(_Request As ConvertRequest)
            If _Request Is Nothing Then
                Throw New ArgumentNullException(NameOf(_Request))
            End If
            Text = _Request.SourceCode
            FromLanguage = LanguageNames.CSharp
            ToLanguage = LanguageNames.VisualBasic
            FromLanguageVersion = CS.LanguageVersion.Latest
            ToLanguageVersion = VisualBasic.LanguageVersion.Latest
            Request = _Request
        End Sub

        Public Property FromLanguage As String
        Public Property FromLanguageVersion As Integer
        Public Property Text As String
        Public Property ToLanguage() As String
        Public Property ToLanguageVersion() As Integer
        Public Property Request As ConvertRequest

        Public Function SetFromLanguage(Optional name As String = LanguageNames.CSharp, Optional version As Integer = CS.LanguageVersion.Latest) As CodeWithOptions
            FromLanguage = name
            FromLanguageVersion = version
            Return Me
        End Function

        Public Function SetToLanguage(Optional name As String = LanguageNames.VisualBasic, Optional version As Integer = VisualBasic.LanguageVersion.Latest) As CodeWithOptions
            ToLanguage = name
            ToLanguageVersion = version
            Return Me
        End Function

    End Class

End Namespace
