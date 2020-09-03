' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Text

Imports DiffPlex
Imports DiffPlex.DiffBuilder
Imports DiffPlex.DiffBuilder.Model

Imports Xunit

Namespace Roslyn.Test.Utilities

    '' <summary>
    '' Assert style type to deal with the lack of features in xUnit's Assert type
    '' </summary>
    Public Module AssertEx

        '' <summary>
        '' Asserts that two strings are equal, and prints the difference between the two if they are not.
        '' </summary>
        '' <param name="expected">The expected string. This is presented as the "baseline/before" side of the difference.</param>
        '' <param name="actual">The actual string. This is presented as the changed or "after" side in the difference.</param>
        '' <param name="message">The message to precede the difference, if the values are not equal.</param>
        Public Sub EqualOrDiff(expected As String, actual As String, Optional message As String = Nothing)
            If expected = actual Then
                Return
            End If

            Dim diffBuilder As InlineDiffBuilder = New InlineDiffBuilder(New Differ())
            Dim diff As DiffPaneModel = diffBuilder.BuildDiffModel(expected, actual, ignoreWhitespace:=False)
            Dim messageBuilder As StringBuilder = New StringBuilder()
            messageBuilder.AppendLine(
                If(String.IsNullOrEmpty(message),
                "Actual and expected values differ. Expected shown in baseline of difference:", message))

            For Each line As DiffPiece In diff.Lines
                Select Case line.Type
                    Case ChangeType.Inserted
                        messageBuilder.Append("+"c)
                    Case ChangeType.Deleted
                        messageBuilder.Append("-"c)
                    Case Else
                        messageBuilder.Append(" "c)
                End Select

                messageBuilder.AppendLine(line.Text)
            Next

            Assert.[True](False, messageBuilder.ToString())
        End Sub

    End Module
End Namespace
