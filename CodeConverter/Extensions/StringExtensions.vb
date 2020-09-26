' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports System.Text

Imports VB = Microsoft.CodeAnalysis.VisualBasic

Namespace CSharpToVBConverter

    Public Module StringExtensions

        <Extension>
        Friend Function ContainsAny(s As String, comparisonType As StringComparison, ParamArray StringArray() As String) As Boolean
            If String.IsNullOrWhiteSpace(s) Then
                Return False
            End If
            If StringArray Is Nothing OrElse StringArray.Length = 0 Then
                Return False
            End If
            For Each str As String In StringArray
                If s.Contains(str, comparisonType) Then
                    Return True
                End If
            Next
            Return False
        End Function

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
        Friend Function GetSafeVBName(exprsssionString As String) As String
            Dim ExpressionBuilder As New StringBuilder
            For Each e As IndexClass(Of Char) In exprsssionString.
                                                    Replace(".", "Dot_", StringComparison.Ordinal).
                                                    Replace(",", "Comma_", StringComparison.Ordinal).
                                                    Replace("""", "Quote", StringComparison.Ordinal).
                                                    Replace("[", "OpenBracket_", StringComparison.Ordinal).
                                                    Replace("]", "CloseBracket", StringComparison.Ordinal).
                                                    Replace("(", "OpenParen_", StringComparison.Ordinal).
                                                    Replace(")", "CloseParen", StringComparison.Ordinal).
                                                    Replace(" ", "_", StringComparison.Ordinal).WithIndex
                If e.IsFirst AndAlso Not VB.SyntaxFacts.IsIdentifierStartCharacter(e.Value) Then
                    ExpressionBuilder.Append($"_")
                End If
                If VB.SyntaxFacts.IsIdentifierPartCharacter(e.Value) Then
                    ExpressionBuilder.Append(e.Value)
                Else
                    ExpressionBuilder.Append("_"c)
                End If
            Next
            Return ExpressionBuilder.ToString.TrimEnd("_"c)
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
        Friend Function RemoveAll(input As String, ParamArray StringsToBeRemoved() As String) As String
            For Each s As String In StringsToBeRemoved
                input = input.Replace(s, "", StringComparison.Ordinal)
            Next
            Return input
        End Function

        <Extension>
        Friend Function RemoveAll(input As String, StringsToBeRemoved As String) As String
            Return input.Replace(StringsToBeRemoved, "", StringComparison.Ordinal)
        End Function

        <Extension>
        Friend Function RemoveBrackets(input As String) As String
            Return input.Replace("["c, "", StringComparison.Ordinal).
                         Replace("]"c, "", StringComparison.Ordinal)
        End Function

        <Extension>
        Friend Function TrimStart(input As String, TrimString As String) As String
            If Not input.StartsWith(TrimString, StringComparison.OrdinalIgnoreCase) Then
                Return input
            End If
            Return input.Substring(TrimString.Length)
        End Function

        <Extension>
        Friend Function WithoutLeadingSystemDot(input As String) As String
            If Not input.StartsWith("System.", StringComparison.Ordinal) Then
                Return input
            End If
            Return input.Substring("System.".Length)
        End Function

#If NET48 Then

        #Region "Workarounds so that CA1307 is enabled until https://github.com/dotnet/roslyn-analyzers/issues/2581 is fixed"

                <Extension>
                Friend Function Contains(str As String, value As String, comparisonType As StringComparison) As Boolean
                    If str Is Nothing Then Throw New ArgumentNullException(NameOf(str))

                    Return str.IndexOf(value, comparisonType) <> -1
                End Function

                <Extension>
                Friend Function Replace(str As String, oldValue As String, newValue As String, comparisonType As StringComparison) As String
                    If str Is Nothing Then Throw New ArgumentNullException(NameOf(str))
                    If comparisonType <> StringComparison.Ordinal Then Throw New PlatformNotSupportedException("String.Replace on .NET Framework only supports StringComparison.Ordinal.")

                    Return str.Replace(oldValue, newValue)
                End Function

                <Extension>
                Friend Function Split(str As String, separator As Char, comparisonType As StringComparison) As String()
                    If str Is Nothing Then Throw New ArgumentNullException(NameOf(str))
                    If comparisonType <> StringComparison.Ordinal Then Throw New PlatformNotSupportedException("String.Split on .NET Framework only supports StringComparison.Ordinal.")

                    Return str.Split(separator)
                End Function

        #End Region

#End If

    End Module
End Namespace
