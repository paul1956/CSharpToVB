' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.Runtime.CompilerServices
Imports System.Text.RegularExpressions

Public Module StringExtensions

    <Extension()>
    Public Function Contains(s As String, StringList() As String, comparisonType As StringComparison) As Boolean
        If String.IsNullOrWhiteSpace(s) Then
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
    Public Function IsInteger(s As String) As Boolean
        If String.IsNullOrWhiteSpace(s) Then
            Return False
        End If
        Dim regularExpression As New Regex("^-[0-9]+$|^[0-9]+$")
        Return regularExpression.Match(s).Success
    End Function

    ' String isn't IEnumerable<char> in the current Portable profile.
    <Extension>
    Public Function Last(arg As String) As Char
        If String.IsNullOrEmpty(arg) Then
            Return CChar(vbNullChar)
        End If
        Return arg(arg.Length - 1)
    End Function

    <Extension()>
    Public Function Left(str As String, Length As Integer) As String
        If str Is Nothing Then
            Return ""
        End If
        Return str.Substring(0, Math.Min(Length, str.Length))
    End Function

    <Extension()>
    Public Function Right(str As String, Length As Integer) As String
        If str Is Nothing Then
            Return ""
        End If
        Return str.Substring(Math.Max(str.Length, Length) - Length)
    End Function

#If NET48 Then
#Region "Workarounds so that CA1307 is enabled until https://github.com/dotnet/roslyn-analyzers/issues/2581 is fixed"

    <Extension>
    Public Function Contains(str As String, value As String, comparisonType As StringComparison) As Boolean
        If str Is Nothing Then Throw New ArgumentNullException(NameOf(str))

        Return str.IndexOf(value, comparisonType) <> -1
    End Function

    <Extension>
    Public Function Replace(str As String, oldValue As String, newValue As String, comparisonType As StringComparison) As String
        If str Is Nothing Then Throw New ArgumentNullException(NameOf(str))
        If comparisonType <> StringComparison.Ordinal Then Throw New PlatformNotSupportedException("String.Replace on .NET Framework only supports StringComparison.Ordinal.")

        Return str.Replace(oldValue, newValue)
    End Function

    <Extension>
    Public Function Split(str As String, separator As Char, comparisonType As StringComparison) As String()
        If str Is Nothing Then Throw New ArgumentNullException(NameOf(str))
        If comparisonType <> StringComparison.Ordinal Then Throw New PlatformNotSupportedException("String.Split on .NET Framework only supports StringComparison.Ordinal.")

        Return str.Split(separator)
    End Function

#End Region
#End If

End Module
