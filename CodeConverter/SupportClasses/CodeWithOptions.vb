' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp

Namespace SupportClasses

    Public Class CodeWithOptions

        Friend Sub New(requestToConvert As ConvertRequest)
            If requestToConvert Is Nothing Then
                Throw New ArgumentNullException(NameOf(requestToConvert))
            End If
            Me.Text = requestToConvert.SourceCode
            Me.FromLanguage = LanguageNames.CSharp
            Me.ToLanguage = LanguageNames.VisualBasic
            Me.FromLanguageVersion = CS.LanguageVersion.Latest
            Me.ToLanguageVersion = VisualBasic.LanguageVersion.Latest
            Me.Request = requestToConvert
        End Sub

        Friend Property FromLanguage As String
        Friend Property FromLanguageVersion As Integer
        Friend Property Request As ConvertRequest
        Friend Property Text As String
        Friend Property ToLanguage() As String
        Friend Property ToLanguageVersion() As Integer

        Friend Function SetFromLanguageVersion(Optional version As Integer = CS.LanguageVersion.Latest) As CodeWithOptions
            Me.FromLanguageVersion = version
            Return Me
        End Function

        Friend Function SetToLanguageVersion(Optional version As Integer = VisualBasic.LanguageVersion.Latest) As CodeWithOptions
            Me.ToLanguageVersion = version
            Return Me
        End Function

    End Class

End Namespace
