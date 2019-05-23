Option Compare Text
Option Explicit On
Option Infer Off
Option Strict On

Imports System.Diagnostics.CodeAnalysis
Imports System.Runtime.CompilerServices
Imports System.Text
Imports System.Text.RegularExpressions

<ExcludeFromCodeCoverage>
Public Module StringExtensions

    Private Function Replace(original As String, pattern As String, replacement As String, comparisonType As StringComparison, stringBuilderInitialSize As Integer) As String
        If original Is Nothing Then
            Return Nothing
        End If

        If String.IsNullOrEmpty(pattern) Then
            Return original
        End If

        Dim posCurrent As Integer = 0
        Dim lenPattern As Integer = pattern.Length
        Dim idxNext As Integer = original.IndexOf(pattern, comparisonType)
        Dim result As New StringBuilder(If(stringBuilderInitialSize < 0, Math.Min(4096, original.Length), stringBuilderInitialSize))

        While idxNext >= 0
            result.Append(original, posCurrent, idxNext - posCurrent)
            result.Append(replacement)

            posCurrent = idxNext + lenPattern

            idxNext = original.IndexOf(pattern, posCurrent, comparisonType)
        End While

        result.Append(original, posCurrent, original.Length - posCurrent)

        Return result.ToString()
    End Function

    <Extension()>
    Public Function Contains(s As String, StringList() As String, comparisonType As StringComparison) As Boolean
        If s.IsEmptyNullOrWhitespace Then
            Return False
        End If
        If StringList Is Nothing OrElse StringList.Length = 0 Then
            Return False
        End If
        For Each strTemp As String In StringList
            If s.IndexOf(strTemp, comparisonType) >= 0 Then
                Return True
            End If
        Next
        Return False
    End Function

    <Extension()>
    Public Function Contains(ByVal s As String, ByVal StringList() As String) As Boolean
        If StringList Is Nothing OrElse StringList.Length = 0 Then
            Return False
        End If
        For Each strTemp As String In StringList
            If s.IndexOf(strTemp, StringComparison.OrdinalIgnoreCase) >= 0 Then
                Return True
            End If
        Next
        Return False
    End Function

    <Extension()>
    Public Function IsEmptyNullOrWhitespace(StringToCheck As String) As Boolean
        If StringToCheck Is Nothing Then
            Return True
        End If
        Return StringToCheck.Trim.Length = 0
    End Function

    <Extension()>
    Public Function IsInteger(s As String) As Boolean
        If s.IsEmptyNullOrWhitespace Then Return False
        Dim regularExpression As New Regex("^-[0-9]+$|^[0-9]+$")
        Return regularExpression.Match(s).Success
    End Function

    <Extension()>
    Public Function IsNotEmptyNullOrWhitespace(StringToCheck As String) As Boolean
        Return Not IsEmptyNullOrWhitespace(StringToCheck)
    End Function

    ' String isn't IEnumerable<char> in the current Portable profile.
    <Extension>
    Public Function Last(arg As String) As Char
        If arg.Length = 0 Then
            Return CChar(vbNullChar)
        End If
        Return arg(arg.Length - 1)
    End Function

    <Extension()>
    Public Function Left(str As String, Length As Integer) As String
        Return str.Substring(0, Math.Min(Length, str.Length))
    End Function

    <Extension()>
    Public Function Replace(original As String, pattern As String, replacement As String, comparisonType As StringComparison) As String
        Return Replace(original, pattern, replacement, comparisonType, -1)
    End Function

    <Extension()>
    Public Function Right(ByVal str As String, ByVal Length As Integer) As String
        Return str.Substring(Math.Max(str.Length, Length) - Length)
    End Function

End Module