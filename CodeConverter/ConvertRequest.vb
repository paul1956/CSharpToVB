﻿' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.Threading

Imports ManageProgressBar

#If NETCOREAPP Then

#Else

Imports ReportProgress = System.Object

#End If

Public Class ConvertRequest

    Public Sub New(mSkipAutoGenerated As Boolean, mProgressBar As ReportProgress, mCancelToken As CancellationToken)
        _ProgressBar = mProgressBar
        _SkipAutoGenerated = mSkipAutoGenerated
        _CancelToken = mCancelToken
    End Sub

    Public Property CancelToken As CancellationToken
    Public ReadOnly Property ProgressBar As ReportProgress
    Public ReadOnly Property SkipAutoGenerated As Boolean
    Public Property SourceCode As String

End Class
