' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Collections.Immutable
Imports System.Runtime.InteropServices
Imports System.Text
Imports System.Text.RegularExpressions

Imports Microsoft.CodeAnalysis.Text

Namespace Roslyn.Test.Utilities

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
        Private Const NamedSpanStartString As String = "{|"
        Private Const NamedSpanEndString As String = "|}"

        Private ReadOnly s_namedSpanStartRegex As Regex = New Regex("\{\| ([-_.A-Za-z0-9\+]+) \:",
            RegexOptions.Multiline Or RegexOptions.IgnorePatternWhitespace)

        Private Sub Parse(
            input As String, <Out> ByRef output As String, <Out> ByRef position As Integer?, <Out> ByRef spans As IDictionary(Of String, List(Of TextSpan)))
            position = Nothing
            Dim tempSpans As New Dictionary(Of String, List(Of TextSpan))()

            Dim outputBuilder As New StringBuilder()

            Dim currentIndexInInput As Integer = 0
            Dim inputOutputOffset As Integer = 0

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
                Dim orderedMatches As List(Of Tuple(Of Integer, String)) = SortMatches(matches)
                If orderedMatches.Count >= 2 AndAlso
                spanStartStack.Count > 0 AndAlso
                matches(0).Item1 = matches(1).Item1 - 1 Then
                    ' We have a slight ambiguity with cases like these:
                    '
                    ' [|]    [|}
                    '
                    ' Is it starting a new match, or ending an existing match.  As a workaround, we
                    ' special case these and consider it ending a match if we have something on the
                    ' stack already.
                    If (matches(0).Item2 = SpanStartString AndAlso matches(1).Item2 = SpanEndString AndAlso spanStartStack.Peek().Item2.Length = 0) OrElse
                    (matches(0).Item2 = SpanStartString AndAlso matches(1).Item2 = NamedSpanEndString AndAlso spanStartStack.Peek().Item2.Length <> 0) Then
                        orderedMatches.RemoveAt(0)
                    End If
                End If

                ' Order the matches by their index
                Dim firstMatch As Tuple(Of Integer, String) = orderedMatches.First()

                Dim matchIndexInInput As Integer = firstMatch.Item1
                Dim matchString As String = firstMatch.Item2

                Dim matchIndexInOutput As Integer = matchIndexInInput - inputOutputOffset
                outputBuilder.Append(input.Substring(currentIndexInInput, matchIndexInInput - currentIndexInInput))

                currentIndexInInput = matchIndexInInput + matchString.Length
                inputOutputOffset += matchString.Length

                Select Case matchString.Substring(0, 2)
                    Case PositionString
                        If position.HasValue Then
                            Throw New ArgumentException(String.Format(Globalization.CultureInfo.CurrentCulture, "Saw multiple occurrences of {0}", PositionString))
                        End If

                        position = matchIndexInOutput

                    Case SpanStartString
                        spanStartStack.Push(Tuple.Create(matchIndexInOutput, String.Empty))

                    Case SpanEndString
                        If spanStartStack.Count = 0 Then
                            Throw New ArgumentException(String.Format(Globalization.CultureInfo.CurrentCulture, "Saw {0} without matching {1}", SpanEndString, SpanStartString))
                        End If

                        If spanStartStack.Peek().Item2.Length > 0 Then
                            Throw New ArgumentException(String.Format(Globalization.CultureInfo.CurrentCulture, "Saw {0} without matching {1}", NamedSpanStartString, NamedSpanEndString))
                        End If

                        PopSpan(spanStartStack, tempSpans, matchIndexInOutput)

                    Case NamedSpanStartString
                        Dim name As String = namedSpanStartMatch.Groups(1).Value
                        spanStartStack.Push(Tuple.Create(matchIndexInOutput, name))

                    Case NamedSpanEndString
                        If spanStartStack.Count = 0 Then
                            Throw New ArgumentException(String.Format(Globalization.CultureInfo.CurrentCulture, "Saw {0} without matching {1}", NamedSpanEndString, NamedSpanStartString))
                        End If

                        If spanStartStack.Peek().Item2.Length = 0 Then
                            Throw New ArgumentException(String.Format(Globalization.CultureInfo.CurrentCulture, "Saw {0} without matching {1}", SpanStartString, SpanEndString))
                        End If

                        PopSpan(spanStartStack, tempSpans, matchIndexInOutput)
                    Case Else
                        Throw New InvalidOperationException()
                End Select
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

        Private Function SortMatches(matches As List(Of Tuple(Of Integer, String))) As List(Of Tuple(Of Integer, String))
            Dim sortedMatches As New List(Of Tuple(Of Integer, String))
            Select Case matches.Count
                Case 0
                Case 1
                    sortedMatches.Add(matches(0))
                Case Else

            End Select
            sortedMatches.Add(matches(0))

            Dim max As Integer = matches.Count - 1
            For loop1 As Integer = 1 To max
                For loop2 As Integer = 0 To max - loop1
                    If sortedMatches(loop2).Item1 > sortedMatches(loop2 + 1).Item1 Then
                        Dim tmp As Tuple(Of Integer, String) = sortedMatches(loop2)
                        sortedMatches(loop2) = sortedMatches(loop2 + 1)
                        sortedMatches(loop2 + 1) = tmp
                    End If
                Next loop2
            Next loop1
            Return sortedMatches
        End Function

        Private Function GetOrAdd(Of K, V)(Dictionary As IDictionary(Of K, V), key As K, [function] As Func(Of K, V)) As V
            Dim value As V = Nothing
            If Not Dictionary.TryGetValue(key, value) Then
                value = [function](key)
                Dictionary.Add(key, value)
            End If

            Return value
        End Function

        Private Sub PopSpan(
            spanStartStack As Stack(Of Tuple(Of Integer, String)),
            spans As IDictionary(Of String, List(Of TextSpan)),
            finalIndex As Integer)
            Dim spanStartTuple As Tuple(Of Integer, String) = spanStartStack.Pop()

            Dim span As TextSpan = TextSpan.FromBounds(spanStartTuple.Item1, finalIndex)
            GetOrAdd(spans, spanStartTuple.Item2, Function(__ As String) New List(Of TextSpan)).Add(span)
        End Sub

        Private Sub AddMatch(input As String, value As String, currentIndex As Integer, matches As List(Of Tuple(Of Integer, String)))
            Dim index As Integer = input.IndexOf(value, currentIndex, StringComparison.Ordinal)
            If index >= 0 Then
                matches.Add(Tuple.Create(index, value))
            End If
        End Sub

        Private Sub GetPositionAndSpans(
            input As String, <Out> ByRef output As String, <Out> ByRef cursorPositionOpt As Integer?, <Out> ByRef spans As ImmutableArray(Of TextSpan))
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

        Public Sub GetSpans(input As String, <Out> ByRef output As String, <Out> ByRef spans As IDictionary(Of String, ImmutableArray(Of TextSpan)))
            Dim cursorPositionOpt As Integer?
            Call GetPositionAndSpans(input, output, cursorPositionOpt, spans)
        End Sub

        Public Sub GetPositionAndSpans(input As String, <Out> ByRef output As String, <Out> ByRef cursorPosition As Integer, <Out> ByRef spans As ImmutableArray(Of TextSpan))
            Dim pos As Integer? = Nothing
            GetPositionAndSpans(input, output, pos, spans)
            cursorPosition = pos.Value
        End Sub

        Public Sub GetPosition(input As String, <Out> ByRef output As String, <Out> ByRef cursorPosition As Integer?)
            Dim spans As ImmutableArray(Of TextSpan) = Nothing
            Call GetPositionAndSpans(input, output, cursorPosition, spans)
        End Sub

        Public Sub GetPosition(input As String, <Out> ByRef output As String, <Out> ByRef cursorPosition As Integer)
            Dim spans As ImmutableArray(Of TextSpan) = Nothing
            Call GetPositionAndSpans(input, output, cursorPosition, spans)
        End Sub

        Public Sub GetPositionAndSpan(input As String, <Out> ByRef output As String, <Out> ByRef cursorPosition As Integer?, <Out> ByRef textSpan As TextSpan?)
            Dim spans As ImmutableArray(Of TextSpan) = Nothing
            GetPositionAndSpans(input, output, cursorPosition, spans)
            textSpan = If(spans.Length = 0, Nothing, CType(spans.[Single](), TextSpan?))
        End Sub

        Public Sub GetPositionAndSpan(input As String, <Out> ByRef output As String, <Out> ByRef cursorPosition As Integer, <Out> ByRef textSpan As TextSpan)
            Dim spans As ImmutableArray(Of TextSpan) = Nothing
            GetPositionAndSpans(input, output, cursorPosition, spans)
            textSpan = spans.[Single]()
        End Sub

        Public Sub GetSpans(input As String, <Out> ByRef output As String, <Out> ByRef spans As ImmutableArray(Of TextSpan))
            Dim pos As Integer? = Nothing
            GetPositionAndSpans(input, output, pos, spans)
        End Sub

        Public Sub GetSpan(input As String, <Out> ByRef output As String, <Out> ByRef textSpan As TextSpan)
            Dim spans As ImmutableArray(Of TextSpan) = Nothing
            GetSpans(input, output, spans)
            textSpan = spans.[Single]()
        End Sub

    End Module
End Namespace
