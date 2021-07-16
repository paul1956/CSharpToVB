' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices

Public Module EnumerableExtensions

    Private Function ContainsTypeName(inStr As String, target As String, comparison As StringComparison) As Boolean
        Dim s As String = inStr.Split(".").LastOrDefault()
        Return s.Equals(target, comparison)
    End Function

    <Extension>
    Friend Function Contains(Of T)(sequence As IEnumerable(Of T), predicate As Func(Of T, Boolean)) As Boolean
        Return sequence.Any(predicate)
    End Function

    ''' <summary>
    ''' This special Contains function compares a list of strings with additional functionality
    ''' </summary>
    ''' <param name="sequence">The IEnumerable(Of String) that is searched</param>
    ''' <param name="target">The string being com</param>
    ''' <param name="comparison"></param>
    ''' <returns></returns>
    <Extension>
    Friend Function Contains(sequence As IEnumerable(Of String), target As String, compareTypeOnly As Boolean, comparison As StringComparison) As Boolean
        If sequence Is Nothing Then Return False
        Dim predicate1 As Func(Of String, Boolean) = Function(inStr As String) As Boolean
                                                         If compareTypeOnly Then
                                                             Return ContainsTypeName(inStr, target, comparison)
                                                         End If
                                                         Return inStr.Equals(target, comparison)
                                                     End Function
        Return sequence.Any(predicate1)
    End Function

    ''' <summary>
    ''' This special FindAll function compares a list of strings with additional functionality
    ''' </summary>
    ''' <param name="sequence">The IEnumerable(Of String) that is searched</param>
    ''' <param name="target">The string being com</param>
    ''' <param name="compareTypeOnly"></param>
    ''' <param name="comparison"></param>
    ''' <returns></returns>
    <Extension>
    Friend Function FindAll(sequence As List(Of String), target As String, compareTypeOnly As Boolean, comparison As StringComparison) As List(Of String)
        If sequence Is Nothing Then Return New List(Of String)
        Dim results As New List(Of String)
        Dim predicate1 As Func(Of String, Boolean) = Function(inStr As String) As Boolean
                                                         If compareTypeOnly Then
                                                             Return ContainsTypeName(inStr, target, comparison)
                                                         End If
                                                         Return inStr.Equals(target, comparison)
                                                     End Function

        For Each s As String In sequence
            If predicate1(s) Then
                results.Add(s)
            End If
        Next
        Return results
    End Function

End Module
