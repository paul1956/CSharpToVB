' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp

Namespace CSharpToVBConverter

    Public Class CodeWithOptions

        Friend Sub New(RequestToConvert As ConvertRequest)
            If RequestToConvert Is Nothing Then
                Throw New ArgumentNullException(NameOf(RequestToConvert))
            End If
            Text = RequestToConvert.SourceCode
            FromLanguage = LanguageNames.CSharp
            ToLanguage = LanguageNames.VisualBasic
            FromLanguageVersion = CS.LanguageVersion.Latest
            ToLanguageVersion = VisualBasic.LanguageVersion.Latest
            Request = RequestToConvert
        End Sub

        Friend Property FromLanguage As String
        Friend Property FromLanguageVersion As Integer
        Friend Property Request As ConvertRequest
        Friend Property Text As String
        Friend Property ToLanguage() As String
        Friend Property ToLanguageVersion() As Integer

        Friend Function SetFromLanguageVersion(Optional version As Integer = CS.LanguageVersion.Latest) As CodeWithOptions
            FromLanguageVersion = version
            Return Me
        End Function

        Friend Function SetToLanguageVersion(Optional version As Integer = VisualBasic.LanguageVersion.Latest) As CodeWithOptions
            ToLanguageVersion = version
            Return Me
        End Function

    End Class

End Namespace
