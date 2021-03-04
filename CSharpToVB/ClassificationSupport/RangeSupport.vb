' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Classification
Imports Microsoft.CodeAnalysis.Formatting
Imports Microsoft.CodeAnalysis.Text

Public Module RangeSupport

    <Extension>
    Private Function AdjustAdditiveSpans(spans As IEnumerable(Of ClassifiedSpan)) As List(Of ClassifiedSpan)
        Dim newSpans As New List(Of ClassifiedSpan)
        If Not spans.Any Then Return newSpans
        Dim currentSpan As ClassifiedSpan = spans(0)
        Dim i As Integer = 0
        While i <= spans.Count - 1
            Try
                ' ReSharper disable once PossibleMultipleEnumeration
                Dim nextSpan As ClassifiedSpan = spans.GetForwardItem(i, 1)
                If ClassificationTypeNames.AdditiveTypeNames.Contains(currentSpan.ClassificationType) Then
                    i += 1
                    currentSpan = nextSpan
                    Continue While
                End If
                Dim newSpan As ClassifiedSpan

                If i = spans.Count - 1 Then
                    If spans(i).TextSpan.End < currentSpan.TextSpan.End Then
                        newSpans.Add(currentSpan)
                    Else
                        newSpans.Add(spans(i))
                    End If
                    Exit While
                End If
                If currentSpan.TextSpan.Length = 0 OrElse
                currentSpan.TextSpan.End <= nextSpan.TextSpan.Start Then
                    newSpans.Add(currentSpan)
                    currentSpan = nextSpan
                    i += 1
                    Continue While
                End If
                newSpan = New ClassifiedSpan(currentSpan.ClassificationType, New TextSpan(currentSpan.TextSpan.Start, nextSpan.TextSpan.Start - currentSpan.TextSpan.Start))
                newSpans.Add(newSpan)
                newSpans.Add(nextSpan)
                Dim length As Integer = currentSpan.TextSpan.Length - (newSpan.TextSpan.Length + nextSpan.TextSpan.Length)
                If length < 0 Then
                    currentSpan = nextSpan
                Else
                    currentSpan = New ClassifiedSpan(currentSpan.ClassificationType, New TextSpan(nextSpan.TextSpan.End, length))
                End If
                i += 1
            Catch ex As Exception
                Stop
            End Try
        End While
        Return newSpans
    End Function

    Private Iterator Function FillGaps(text As SourceText, ranges As IEnumerable(Of Range)) As IEnumerable(Of Range)
        Const whitespaceClassification As String = Nothing

        Dim current As Integer = 0
        Dim previous As Range = Nothing

        For Each range As Range In ranges
            Dim start As Integer = range.TextSpan.Start
            If start > current Then
                Yield New Range(whitespaceClassification, TextSpan.FromBounds(current, start), text)
            End If

            If previous Is Nothing OrElse range.TextSpan <> previous.TextSpan Then
                Yield range
            End If

            previous = range
            current = range.TextSpan.End
        Next

        If current < text.Length Then
            Yield New Range(whitespaceClassification, TextSpan.FromBounds(current, text.Length), text)
        End If
    End Function

    <Extension>
    Friend Function GetForwardItem(Of T)(listOfT As IEnumerable(Of T), index As Integer, lookAhead As Integer) As T
        Dim finalIndex As Integer = index + lookAhead
        If finalIndex < 0 Then
            Return Nothing
        End If
        Return If(finalIndex < listOfT.Count, listOfT(finalIndex), Nothing)
    End Function

    Public Function GetClassifiedRanges(sourceCode As String, language As String) As IEnumerable(Of Range)
        Using workspace As New AdhocWorkspace()
            Dim solution As Solution = workspace.CurrentSolution
            Dim document As Document
            If language = LanguageNames.CSharp Then
                Dim project As Project = solution.AddProject("projectName", "assemblyName", LanguageNames.CSharp)
                document = project.AddDocument("name.cs", sourceCode)
            Else
                Dim project As Project = solution.AddProject("projectName", "assemblyName", LanguageNames.VisualBasic)
                document = project.AddDocument("name.vb", sourceCode)
            End If

            document = Formatter.FormatAsync(document).Result
            Dim text As SourceText = document.GetTextAsync().Result
            Dim classifiedSpans As List(Of ClassifiedSpan) = Classifier.GetClassifiedSpansAsync(document, TextSpan.FromBounds(0, text.Length)).Result.AdjustAdditiveSpans()
            Dim ranges As IEnumerable(Of Range) = From span As ClassifiedSpan In classifiedSpans
                                                  Select New Range(span, text.GetSubText(span.TextSpan).ToString())
            ' Whitespace isn't classified so fill in ranges for whitespace.
            Return FillGaps(text, ranges)
        End Using
    End Function

End Module
