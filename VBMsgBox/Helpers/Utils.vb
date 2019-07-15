' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

Imports System.Globalization

Public Class Utils

    'Summary: Retrieves a resource string and formats it by replacing placeholders
    '         with params. For example if the unformatted string is
    '         "Hello, {0}" then GetString("StringID", "World") will return "Hello, World"
    '         This one is exposed because I have to be able to get at localized error
    '         strings from the MY template
    '  Param: ID - Identifier for the string to be retrieved
    '  Param: Args - An array of params used to replace placeholders.
    'Returns: The resource string if found or an error message string
    '*****************************************************************************
    Public Shared Function GetResourceString(ByVal ResourceKey As String, ByVal ParamArray Args() As String) As String

        Debug.Assert(Not ResourceKey = "", "ResourceKey is missing")
        Debug.Assert(Not Args Is Nothing, "No Args")

        Dim UnformattedString As String = Nothing
        Dim FormattedString As String = Nothing
        Try
            'Get unformatted string which may have place holders ie "Hello, {0}. How is {1}?"
            UnformattedString = GetResourceString(ResourceKey)

            'Replace placeholders with items from the passed in array
            '[688666] - changing CurrentUICulture to CurrentCulture due to FxCop warning CA1305.
            '           The guideline seems to be to use CurrentUICulture when pulling things out
            '           of a ResourceManager, and to use CurrentCulture for any type of formatting
            '           on a value that will be displayed to the user.  Technically we should
            '           probably modify the above overload of GetResourceString to use CurrentUICulture,
            '           but we don't want to change it at this point because it's been that way forever.
            '           We can change it later if the need ever arises.
            FormattedString = String.Format(Threading.Thread.CurrentThread.CurrentCulture, UnformattedString, Args)

            'Rethrow hosting exceptions
        Catch ex As StackOverflowException
            Throw ex
        Catch ex As OutOfMemoryException
            Throw ex
        Catch ex As Threading.ThreadAbortException
            Throw ex
        Catch ex As Exception
            Debug.Fail("Unable to get and format string for ResourceKey: " & ResourceKey)
        Finally
            Debug.Assert(Not UnformattedString = "", "Unable to get string for ResourceKey: " & ResourceKey)
            Debug.Assert(Not FormattedString = "", "Unable to format string for ResourceKey: " & ResourceKey)
        End Try

        'Return the string if we have one otherwise return a default error message
        If Not FormattedString = "" Then
            Return FormattedString
        Else
            Return UnformattedString 'will contain an error string from the attempt to load via the GetResourceString() overload we call internally
        End If
    End Function

    Friend Shared Function GetCultureInfo() As CultureInfo
        Return Threading.Thread.CurrentThread.CurrentCulture
    End Function

End Class
