Option Explicit On
Option Infer Off
Option Strict On

Imports Microsoft.CodeAnalysis

Namespace IVisualBasicCode.CodeConverter
    Public Class CodeWithOptions

        Public Sub New(_Request As ConvertRequest)
            Me.Text = _Request.SourceCode
            Me.FromLanguage = LanguageNames.CSharp
            Me.ToLanguage = LanguageNames.VisualBasic
            Me.FromLanguageVersion = CSharp.LanguageVersion.Latest
            Me.ToLanguageVersion = VisualBasic.LanguageVersion.Latest
            Request = _Request
        End Sub

        Public Property FromLanguage As String
            Public Property FromLanguageVersion As Integer
            Public Property Text As String
            Public Property ToLanguage() As String
            Public Property ToLanguageVersion() As Integer
        Friend Shared Property Request As ConvertRequest

        Public Function SetFromLanguage(Optional name As String = LanguageNames.CSharp, Optional version As Integer = CSharp.LanguageVersion.Latest) As CodeWithOptions
            Me.FromLanguage = name
            Me.FromLanguageVersion = version
                Return Me
            End Function

            Public Function SetToLanguage(Optional name As String = LanguageNames.VisualBasic, Optional version As Integer = VisualBasic.LanguageVersion.Latest) As CodeWithOptions
            Me.ToLanguage = name
            Me.ToLanguageVersion = version
                Return Me
            End Function
        End Class
End Namespace
