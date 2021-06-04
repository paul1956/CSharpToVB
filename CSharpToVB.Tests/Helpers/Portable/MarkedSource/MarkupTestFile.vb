' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.Collections.Immutable
Imports System.Runtime.InteropServices
Imports System.Text
Imports System.Text.RegularExpressions
Imports Microsoft.CodeAnalysis.Text

Namespace Helpers.Portable.MarkedSource

    '' <summary>
    '' To aid with testing, we define a special type of text file that can encode additional
    '' information in it.  This prevents a test writer from having to carry around multiple sources
    '' of information that must be reconstituted.  For example, instead of having to keep around the
    '' contents of a file *and* and the location of the cursor, the tester can just provide a
    '' string with the "$" character in it.  This allows for easy creation of "FIT" tests where all
    '' that needs to be provided are strings that encode every bit of state necessary in the string
    '' itself.
    ''
    '' The current set of encoded features we support are:
    ''
    '' $$ - The position in the file.  There can be at most one of these.
    ''
    '' [| ... |] - A span of text in the file.  There can be many of these and they can be nested
    '' and/or overlap the $ position.
    ''
    '' {|Name: ... |} A span of text in the file annotated with an identifier.  There can be many of
    '' these, including ones with the same name.
    ''
    '' Additional encoded features can be added on a case by case basis.
    '' </summary>
    Public Module MarkupTestFile

        Private Const PositionString As String = "$$"
        Private Const SpanStartString As String = "[|"
        Private Const SpanEndString As String = "|]"
        Private Const NamedSpanEndString As String = "|}"

        Private ReadOnly s_namedSpanStartRegex As New Regex("\{\| ([-_.A-Za-z0-9\+]+) \:",
            RegexOptions.Multiline Or RegexOptions.IgnorePatternWhitespace)

        Private Sub Parse(
            input As String, <Out> ByRef output As String, <Out> ByRef position As Integer?, <Out> ByRef spans As IDictionary(Of String, List(Of TextSpan)))
            position = Nothing
            Dim tempSpans As New Dictionary(Of String, List(Of TextSpan))()

            Dim outputBuilder As New StringBuilder()

            Dim currentIndexInInput As Integer = 0

            ' A stack of span starts along with their associated annotation name.  [||] spans simply
            ' have empty string for their annotation name.
            Dim spanStartStack As New Stack(Of Tuple(Of Integer, String))()

            While True
                Dim matches As New List(Of Tuple(Of Integer, String))()
                AddMatch(input, PositionString, currentIndexInInput, matches)
                AddMatch(input, SpanStartString, currentIndexInInput, matches)
                AddMatch(input, SpanEndString, currentIndexInInput, matches)
                AddMatch(input, NamedSpanEndString, currentIndexInInput, matches)

                Dim namedSpanStartMatch As Match = s_namedSpanStartRegex.Match(input, currentIndexInInput)
                If namedSpanStartMatch.Success Then
                    matches.Add(Tuple.Create(namedSpanStartMatch.Index, namedSpanStartMatch.Value))
                End If

                If matches.Count = 0 Then
                    ' No more markup to process.
                    Exit While
                End If
            End While

            If spanStartStack.Count > 0 Then
                Throw New ArgumentException(String.Format(Globalization.CultureInfo.CurrentCulture, "Saw {0} without matching {1}", SpanStartString, SpanEndString))
            End If

            ' Append the remainder of the string.
            outputBuilder.Append(input.Substring(currentIndexInInput))
            output = outputBuilder.ToString()
            spans = tempSpans.ToDictionary(Function(kvp As KeyValuePair(Of String, List(Of TextSpan)))
                                               Return kvp.Key
                                           End Function,
                                           Function(kvp As KeyValuePair(Of String, List(Of TextSpan)))
                                               Return kvp.Value
                                           End Function)
        End Sub

        Private Function GetOrAdd(Of TK, TV)(dictionary As IDictionary(Of TK, TV), key As TK, [function] As Func(Of TK, TV)) As TV
            Dim value As TV = Nothing
            If Not dictionary.TryGetValue(key, value) Then
                value = [function](key)
                dictionary.Add(key, value)
            End If

            Return value
        End Function

        Private Sub AddMatch(input As String, value As String, currentIndex As Integer, matches As List(Of Tuple(Of Integer, String)))
            Dim index As Integer = input.IndexOf(value, currentIndex, StringComparison.Ordinal)
            If index >= 0 Then
                matches.Add(Tuple.Create(index, value))
            End If
        End Sub

        Private Sub GetPositionAndSpans(input As String, <Out> ByRef output As String, <Out> ByRef cursorPositionOpt As Integer?, <Out> ByRef spans As ImmutableArray(Of TextSpan))
            Dim mDictionary As IDictionary(Of String, List(Of TextSpan)) = Nothing
            Parse(input, output, cursorPositionOpt, mDictionary)

            Dim builder As List(Of TextSpan) = GetOrAdd(mDictionary, String.Empty, Function(__ As String) New List(Of TextSpan))
            spans = ImmutableArray.Create(builder.ToArray)
        End Sub

        Public Sub GetPositionAndSpans(input As String, <Out> ByRef output As String, <Out> ByRef cursorPositionOpt As Integer?, <Out> ByRef spans As IDictionary(Of String, ImmutableArray(Of TextSpan)))
            If input Is Nothing Then
                Throw New ArgumentNullException(NameOf(input))
            End If

            Dim mDictionary As IDictionary(Of String, List(Of TextSpan)) = Nothing
            Parse(input, output, cursorPositionOpt, mDictionary)
            spans = mDictionary.ToDictionary(Function(kvp As KeyValuePair(Of String, List(Of TextSpan)))
                                                 Return kvp.Key
                                             End Function,
                                             Function(kvp As KeyValuePair(Of String, List(Of TextSpan)))
                                                 Return ImmutableArray.Create(kvp.Value.ToArray)
                                             End Function)
        End Sub

        Public Sub GetSpans(input As String, <Out> ByRef output As String, <Out> ByRef spans As ImmutableArray(Of TextSpan))
            Dim pos As Integer? = Nothing
            GetPositionAndSpans(input, output, pos, spans)
        End Sub

    End Module
End Namespace
