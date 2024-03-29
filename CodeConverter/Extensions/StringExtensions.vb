﻿' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports System.Text
Imports VB = Microsoft.CodeAnalysis.VisualBasic

Public Module StringExtensions

    <Extension>
    Friend Function ConvertCondition(condition As String) As String
        Return condition.
                Replace("==", "=", StringComparison.Ordinal).
                Replace("!=", "Not ", StringComparison.Ordinal).
                Replace("&&", "And", StringComparison.Ordinal).
                Replace("||", "Or", StringComparison.Ordinal).
                Replace("!", "Not ", StringComparison.Ordinal).
                Replace("false", "False", StringComparison.Ordinal).
                Replace("true", "True", StringComparison.Ordinal).
                Replace("  ", " ", StringComparison.Ordinal).
                Replace("//", " ' ", StringComparison.Ordinal)
    End Function

    <Extension>
    Friend Function GetSafeVbName(expressionString As String) As String
        Dim expressionBuilder As New StringBuilder
        For Each e As IndexClass(Of Char) In expressionString.
                                                Replace(".", "Dot_", StringComparison.Ordinal).
                                                Replace(",", "Comma_", StringComparison.Ordinal).
                                                Replace("""", "Quote", StringComparison.Ordinal).
                                                Replace("[", "OpenBracket_", StringComparison.Ordinal).
                                                Replace("]", "CloseBracket", StringComparison.Ordinal).
                                                Replace("(", "OpenParen_", StringComparison.Ordinal).
                                                Replace(")", "CloseParen", StringComparison.Ordinal).
                                                Replace(" ", "_", StringComparison.Ordinal).WithIndex
            If e.IsFirst AndAlso Not VB.SyntaxFacts.IsIdentifierStartCharacter(e.Value) Then
                expressionBuilder.Append($"_")
            End If
            If VB.SyntaxFacts.IsIdentifierPartCharacter(e.Value) Then
                expressionBuilder.Append(e.Value)
            Else
                expressionBuilder.Append("_"c)
            End If
        Next
        Return expressionBuilder.ToString.TrimEnd("_"c)
    End Function

    ' String isn't IEnumerable<char> in the current Portable profile.
    <Extension>
    Friend Function Last(arg As String) As Char
        If String.IsNullOrEmpty(arg) Then
            Return CChar(vbNullChar)
        End If
        Return arg(arg.Length - 1)
    End Function

    <Extension>
    Friend Function RemoveAll(input As String, ParamArray stringsToBeRemoved() As String) As String
        For Each s As String In stringsToBeRemoved
            input = input.Replace(s, "", StringComparison.Ordinal)
        Next
        Return input
    End Function

    <Extension>
    Friend Function RemoveAll(input As String, stringsToBeRemoved As String) As String
        Return input.Replace(stringsToBeRemoved, "", StringComparison.Ordinal)
    End Function

    <Extension>
    Friend Function RemoveBrackets(input As String) As String
        Return input.Replace("["c, "", StringComparison.Ordinal).
                     Replace("]"c, "", StringComparison.Ordinal)
    End Function

    <Extension>
    Friend Function WithoutLeadingSystemDot(input As String) As String
        If Not input.StartsWith("System.", StringComparison.Ordinal) Then
            Return input
        End If
        Return input.Substring("System.".Length)
    End Function

End Module
