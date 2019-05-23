Option Explicit On
Option Infer Off
Option Strict On

Imports System.Runtime.CompilerServices

Public Module ProcessEscapeSequences

    Public Function ConvertCSharpEscapes(TokenString As String) As String
        Dim _Buffer As String
        Try
            'Dim unescape1 As String = RegexParser.Unescape(TokenString)
            _Buffer = TokenString.
                        Replace("\r\n", "{vbCrLf}", StringComparison.Ordinal).
                        Replace("\'", "'").
                        Replace("\0", "{ChrW(0)}").
                        Replace("\a", "{&H7}", StringComparison.Ordinal).
                        Replace("\c", "{vbCr}", StringComparison.Ordinal).
                        Replace("\b", "{ChrW(&H8)}", StringComparison.Ordinal).
                        Replace("\f", "{ChrW(12)}", StringComparison.Ordinal).
                        Replace("\n", "{vbLf}", StringComparison.Ordinal).
                        Replace("\t", "{vbTab}", StringComparison.Ordinal).
                        Replace("\=", "=").
                        Replace("\,", ",").
                        Replace("\""", Quote).
                        Replace("\\", "\").
                        Replace(Quote, DoubleQuote).NormalizeLineEndings
            _Buffer = _Buffer.Replace("{", "{{").Replace("}", "}}")
            If _Buffer.Contains(UnicodeOpenQuote) Then
                _Buffer = _Buffer.ConverUnicodeQuotes(UnicodeOpenQuote)
            End If
            If _Buffer.Contains(UnicodeCloseQuote) Then
                _Buffer = _Buffer.ConverUnicodeQuotes(UnicodeCloseQuote)
            End If
            If _Buffer.Contains(UnicodeFullWidthQuoationMark) Then
                _Buffer = _Buffer.ConverUnicodeQuotes(UnicodeFullWidthQuoationMark)
            End If

            TokenString = _Buffer
        Catch ex As Exception
            Stop
        End Try
        Return TokenString
    End Function

    <Extension>
    Public Function ConverUnicodeQuotes(TokenString As String, UnicodeQuote As String) As String
        TokenString = TokenString.Replace(UnicodeQuote & UnicodeQuote, ChrW(0))
        TokenString = TokenString.Replace(UnicodeQuote, UnicodeQuote & UnicodeQuote)
        Return TokenString.Replace(ChrW(0), UnicodeQuote & UnicodeQuote)
    End Function

End Module