' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Compare Text
Option Explicit On
Option Infer Off
Option Strict On

Imports System.Runtime.CompilerServices

Imports Microsoft.CodeAnalysis

Public Module StringExtensions

    Private Enum TitleCaseState
        First_Character
        Upper_Case_Word
        Title_Case_Word
    End Enum

    <Extension()>
    Public Function Contains(ByVal s As String, ByVal StringList() As String, ByVal comparisonType As StringComparison) As Boolean
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
    Public Function Count(ByVal value As String, ByVal ch As Char) As Integer
        Return value.Count(Function(c As Char) c = ch)
    End Function

    <Extension()>
    Public Function IsEmptyNullOrWhitespace(ByVal StringToCheck As String) As Boolean
        If StringToCheck Is Nothing Then
            Return True
        End If
        Return StringToCheck.Trim.Length = 0
    End Function

    <Extension()>
    Public Function IsNotEmptyNullOrWhitespace(ByVal StringToCheck As String) As Boolean
        Return Not IsEmptyNullOrWhitespace(StringToCheck)
    End Function

    <Extension>
    Public Function Join(source As IEnumerable(Of String), separator As String) As String
        If source Is Nothing Then
            Throw New ArgumentNullException(NameOf(source))
        End If

        If separator Is Nothing Then
            Throw New ArgumentNullException(NameOf(separator))
        End If

        Return String.Join(separator, source)
    End Function

    <Extension()>
    Public Function Left(ByVal str As String, ByVal Length As Integer) As String
        Return str.Substring(0, Math.Min(Length, str.Length))
    End Function

    <Extension()>
    Public Function Right(ByVal str As String, ByVal Length As Integer) As String
        Return str.Substring(Math.Max(str.Length, Length) - Length)
    End Function

End Module