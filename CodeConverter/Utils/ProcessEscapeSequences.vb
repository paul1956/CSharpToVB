' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

Imports System.Runtime.CompilerServices

Public Module ProcessEscapeSequences

    Friend Function ConvertCSharpEscapes(TokenString As String) As String
        Dim _Buffer As String
        Try
            'Dim unescape1 As String = RegexParser.Unescape(TokenString)
            _Buffer = TokenString.
                        Replace("\r\n", "{vbCrLf}", StringComparison.InvariantCulture).
                        Replace("\'", "'", StringComparison.InvariantCulture).
                        Replace("\0", "{ChrW(0)}", StringComparison.InvariantCulture).
                        Replace("\a", "{&H7}", StringComparison.InvariantCulture).
                        Replace("\c", "{vbCr}", StringComparison.InvariantCulture).
                        Replace("\b", "{ChrW(&H8)}", StringComparison.InvariantCulture).
                        Replace("\f", "{ChrW(12)}", StringComparison.InvariantCulture).
                        Replace("\n", "{vbLf}", StringComparison.InvariantCulture).
                        Replace("\t", "{vbTab}", StringComparison.InvariantCulture).
                        Replace("\=", "=", StringComparison.InvariantCulture).
                        Replace("\,", ",", StringComparison.InvariantCulture).
                        Replace("\""", Quote, StringComparison.InvariantCulture).
                        Replace("\\", "\", StringComparison.InvariantCulture).
                        Replace(Quote, DoubleQuote, StringComparison.InvariantCulture).NormalizeLineEndings
            _Buffer = _Buffer.Replace("{", "{{", StringComparison.InvariantCulture).
                                Replace("}", "}}", StringComparison.InvariantCulture)
            If _Buffer.Contains(UnicodeOpenQuote, StringComparison.InvariantCulture) Then
                _Buffer = _Buffer.ConverUnicodeQuotes(UnicodeOpenQuote)
            End If
            If _Buffer.Contains(UnicodeCloseQuote, StringComparison.InvariantCulture) Then
                _Buffer = _Buffer.ConverUnicodeQuotes(UnicodeCloseQuote)
            End If
            If _Buffer.Contains(UnicodeFullWidthQuoationMark, StringComparison.InvariantCulture) Then
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
        TokenString = TokenString.Replace(UnicodeQuote & UnicodeQuote, ChrW(0), StringComparison.InvariantCulture)
        TokenString = TokenString.Replace(UnicodeQuote, UnicodeQuote & UnicodeQuote, StringComparison.InvariantCulture)
        Return TokenString.Replace(ChrW(0), UnicodeQuote & UnicodeQuote, StringComparison.InvariantCulture)
    End Function

End Module
