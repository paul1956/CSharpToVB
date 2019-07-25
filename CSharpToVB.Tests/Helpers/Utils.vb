Option Explicit On
Option Infer Off
Option Strict On

Imports System.Text

Namespace CodeConverter.Tests
    Friend Module Utils

        Friend Function HomogenizeEol(str As String) As String
            Dim sb As New StringBuilder()
            For i As Integer = 0 To str.Length - 1
                Dim ch As Char = str.Chars(i)
                Dim possibleNewline As Integer = NewLine.GetDelimiterLength(ch, If(i + 1 < str.Length, str.Chars(i + 1), ControlChars.NullChar))
                If possibleNewline > 0 Then
                    sb.AppendLine()
                    If possibleNewline = 2 Then
                        i += 1
                    End If
                Else
                    sb.Append(ch)
                End If
            Next i
            Return sb.ToString()
        End Function

        Private _roslynRootDirectory As String = ""

        Public Function GetRoslynRootDirectory() As String
            If _roslynRootDirectory.IsEmptyNullOrWhitespace Then
                _roslynRootDirectory = Environment.GetFolderPath(Environment.SpecialFolder.UserProfile) & "\Source\Repos\roslyn"
            End If
            Return _roslynRootDirectory
        End Function

    End Module
End Namespace