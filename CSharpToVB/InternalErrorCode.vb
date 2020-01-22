' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

''' <summary>
''' Values for ErrorCode/ERRID that are used internally by the compiler but are not exposed.
''' </summary>
Friend Module InternalErrorCode

    ''' <summary>
    ''' The code has yet to be determined.
    ''' </summary>
    Public Const Unknown As Integer = -1

    ''' <summary>
    ''' The code was lazily determined and does not need to be reported.
    ''' </summary>
    Public Const Void As Integer = -2

End Module
