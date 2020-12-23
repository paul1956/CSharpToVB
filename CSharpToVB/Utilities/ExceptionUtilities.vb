' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Diagnostics.CodeAnalysis
Imports System.Globalization
Imports System.Text

Friend Module ExceptionUtilities

    <ExcludeFromCodeCoverage>
    Friend Function GetExceptionsAsString(Exceptions As IReadOnlyList(Of Exception)) As String
        If Exceptions Is Nothing OrElse Not Exceptions.Any Then
            Return String.Empty
        End If

        Dim builder As New StringBuilder()
        For index As Integer = 0 To Exceptions.Count - 1
            builder.AppendFormat(CultureInfo.InvariantCulture, "----- Exception {0} Of {1} -----" & Environment.NewLine, index + 1, Exceptions.Count)
            builder.AppendLine(Exceptions(index).ToString())
        Next index
        Return builder.ToString()
    End Function

End Module
