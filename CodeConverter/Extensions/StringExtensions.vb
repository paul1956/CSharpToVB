' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices

Public Module StringExtensions

    <Extension()>
    Public Function ContainsAny(s As String, comparisonType As StringComparison, ParamArray StringArray() As String) As Boolean
        If String.IsNullOrWhiteSpace(s) Then
            Return False
        End If
        If StringArray Is Nothing OrElse StringArray.Length = 0 Then
            Return False
        End If
        For Each str As String In StringArray
            If s.IndexOf(str, comparisonType) >= 0 Then
                Return True
            End If
        Next
        Return False
    End Function

    ' String isn't IEnumerable<char> in the current Portable profile.
    <Extension>
    Public Function Last(arg As String) As Char
        If String.IsNullOrEmpty(arg) Then
            Return CChar(vbNullChar)
        End If
        Return arg(arg.Length - 1)
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
