Option Explicit On
Option Infer Off
Option Strict On

Imports IVisualBasicCode.CodeConverter.Visual_Basic

Imports Microsoft.CodeAnalysis

Namespace IVisualBasicCode.CodeConverter
    Public Module CodeConverter
        Friend OriginalRequest As ConvertRequest

        Private Function GetDefaultVersionForLanguage(language As String) As Integer
            If language Is Nothing Then
                Throw New ArgumentNullException(NameOf(language))
            End If
            If language.StartsWith("cs", StringComparison.OrdinalIgnoreCase) Then
                Return CSharp.LanguageVersion.Latest
            End If
            If language.StartsWith("vb", StringComparison.OrdinalIgnoreCase) Then
                Return Integer.MaxValue
            End If
            Throw New ArgumentException($"{language} not supported!")
        End Function

        Private Function IsSupportedSource(fromLanguage As String, fromLanguageVersion As Integer) As Boolean
            Return (fromLanguage = LanguageNames.CSharp AndAlso fromLanguageVersion <= CSharp.LanguageVersion.Latest) OrElse
                (fromLanguage = LanguageNames.VisualBasic AndAlso fromLanguageVersion <= VisualBasic.LanguageVersion.Latest)
        End Function

        Private Function IsSupportedTarget(toLanguage As String, toLanguageVersion As Integer) As Boolean
            Return (toLanguage = LanguageNames.VisualBasic AndAlso toLanguageVersion <= VisualBasic.LanguageVersion.Latest) OrElse
                (toLanguage = LanguageNames.CSharp AndAlso toLanguageVersion <= CSharp.LanguageVersion.Latest)
        End Function

        Private Function ParseLanguage(language As String) As String
            If language Is Nothing Then
                Throw New ArgumentNullException(NameOf(language))
            End If
            If language.StartsWith("cs", StringComparison.OrdinalIgnoreCase) Then
                Return LanguageNames.CSharp
            End If
            If language.StartsWith("vb", StringComparison.OrdinalIgnoreCase) Then
                Return LanguageNames.VisualBasic
            End If
            Throw New ArgumentException($"{language} not supported!")
        End Function

        Public Function Convert(code As CodeWithOptions, OptionalReferences() As MetadataReference) As ConversionResult
            OriginalRequest = CodeWithOptions.Request
            Select Case code.FromLanguage
                Case LanguageNames.CSharp
                    Select Case code.ToLanguage
                        Case LanguageNames.VisualBasic
                            Return CSharpConverter.ConvertText(code.Text, OptionalReferences)
                    End Select
                Case LanguageNames.VisualBasic
                    Select Case code.ToLanguage
                        Case LanguageNames.CSharp
#If VB_TO_CSharp = "True" Then
                            Return VisualBasicConverter.ConvertText(code.Text, code.References)
#Else
                            Throw New NotImplementedException("VB to C# is disabled")
#End If
                    End Select
            End Select
            If Not IsSupportedSource(code.FromLanguage, code.FromLanguageVersion) Then
                Return New ConversionResult(New NotSupportedException($"Source language {code.FromLanguage} {code.FromLanguageVersion} is not supported!"))
            End If
            If Not IsSupportedTarget(code.ToLanguage, code.ToLanguageVersion) Then
                Return New ConversionResult(New NotSupportedException($"Target language {code.ToLanguage} {code.ToLanguageVersion} is not supported!"))
            End If
            If code.FromLanguage = code.ToLanguage AndAlso code.FromLanguageVersion <> code.ToLanguageVersion Then
                Return New ConversionResult(New NotSupportedException($"Converting from {code.FromLanguage} {code.FromLanguageVersion} to {code.ToLanguage} {code.ToLanguageVersion} is not supported!"))
            End If
            Return New ConversionResult(New NotSupportedException($"Converting from {code.FromLanguage} {code.FromLanguageVersion} to {code.ToLanguage} {code.ToLanguageVersion} is not supported!"))
        End Function

        Public Function ConvertInputRequest(RequestToConvert As ConvertRequest, OptionalReferences() As MetadataReference) As ConversionResult

            Dim languages As String() = {RequestToConvert.GetSourceExtension, RequestToConvert.GetTargetExtension}
            Dim fromLanguage As String = LanguageNames.CSharp
            Dim toLanguage As String = LanguageNames.VisualBasic
            Dim fromVersion As Integer = CSharp.LanguageVersion.Latest
            Dim toVersion As Integer = VisualBasic.LanguageVersion.VisualBasic16
            If languages.Length = 2 Then
                fromLanguage = ParseLanguage(languages(0))
                fromVersion = GetDefaultVersionForLanguage(languages(0))
                toLanguage = ParseLanguage(languages(1))
                toVersion = GetDefaultVersionForLanguage(languages(1))
            End If
            Dim codeWithOptions As CodeWithOptions = (New CodeWithOptions(RequestToConvert)).SetFromLanguage(fromLanguage, fromVersion).SetToLanguage(toLanguage, toVersion)
            Dim ResultOfConversion As ConversionResult = CodeConverter.Convert(codeWithOptions, OptionalReferences)
            Return ResultOfConversion
        End Function

    End Module
End Namespace