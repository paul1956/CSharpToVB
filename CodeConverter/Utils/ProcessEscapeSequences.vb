' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices

Public Module ProcessEscapeSequences

    Friend Function ConvertCSharpEscapes(TokenString As String) As String
        Dim _Buffer As String
        Try
            'Dim unescape1 As String = RegexParser.Unescape(TokenString)
            _Buffer = TokenString.
                        Replace("\r\n", "{vbCrLf}", StringComparison.Ordinal).
                        Replace("\'", "'", StringComparison.Ordinal).
                        Replace("\0", "{ChrW(0)}", StringComparison.Ordinal).
                        Replace("\a", "{&H7}", StringComparison.Ordinal).
                        Replace("\c", "{vbCr}", StringComparison.Ordinal).
                        Replace("\b", "{ChrW(&H8)}", StringComparison.Ordinal).
                        Replace("\f", "{ChrW(12)}", StringComparison.Ordinal).
                        Replace("\n", "{vbLf}", StringComparison.Ordinal).
                        Replace("\t", "{vbTab}", StringComparison.Ordinal).
                        Replace("\=", "=", StringComparison.Ordinal).
                        Replace("\,", ",", StringComparison.Ordinal).
                        Replace("\""", Quote, StringComparison.Ordinal).
                        Replace("\\", "\", StringComparison.Ordinal).
                        Replace(Quote, DoubleQuote, StringComparison.Ordinal).NormalizeLineEndings
            _Buffer = _Buffer.Replace("{", "{{", StringComparison.Ordinal).
                                Replace("}", "}}", StringComparison.Ordinal)
            If _Buffer.Contains(UnicodeOpenQuote, StringComparison.Ordinal) Then
                _Buffer = _Buffer.ConverUnicodeQuotes(UnicodeOpenQuote)
            End If
            If _Buffer.Contains(UnicodeCloseQuote, StringComparison.Ordinal) Then
                _Buffer = _Buffer.ConverUnicodeQuotes(UnicodeCloseQuote)
            End If
            If _Buffer.Contains(UnicodeFullWidthQuoationMark, StringComparison.Ordinal) Then
                _Buffer = _Buffer.ConverUnicodeQuotes(UnicodeFullWidthQuoationMark)
            End If

            TokenString = _Buffer
        Catch ex As OperationCanceledException
            Throw
        Catch ex As Exception
            Stop
        End Try
        Return TokenString
    End Function

    <Extension>
    Friend Function ConverUnicodeQuotes(TokenString As String, UnicodeQuote As String) As String
        TokenString = TokenString.Replace(UnicodeQuote & UnicodeQuote, ChrW(0), StringComparison.Ordinal)
        TokenString = TokenString.Replace(UnicodeQuote, UnicodeQuote & UnicodeQuote, StringComparison.Ordinal)
        Return TokenString.Replace(ChrW(0), UnicodeQuote & UnicodeQuote, StringComparison.Ordinal)
    End Function

End Module
