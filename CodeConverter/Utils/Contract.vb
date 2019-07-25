' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

Imports System.Diagnostics.CodeAnalysis
Public Module Contract

    ''' <summary>
    ''' Equivalent to Debug.Assert.
    '''
    ''' DevDiv 867813 covers removing this completely at a future date
    ''' </summary>
    <Conditional("DEBUG"), DebuggerHidden>
    <ExcludeFromCodeCoverage>
    Public Sub Requires(condition As Boolean, Optional message As String = Nothing)
        Debug.Assert(condition, message)
    End Sub

    ''' <summary>
    ''' Equivalent to Debug.Assert.
    '''
    ''' DevDiv 867813 covers removing this completely at a future date
    ''' </summary>
    <Conditional("DEBUG")>
    <ExcludeFromCodeCoverage>
    Public Sub Assume(condition As Boolean, Optional message As String = Nothing)
        If String.IsNullOrEmpty(message) Then
            Debug.Assert(condition)
        Else
            Debug.Assert(condition, message)
        End If
    End Sub

    ''' <summary>
    ''' Throws a non-accessible exception if the provided value is null.  This method executes in
    ''' all builds
    ''' </summary>
    <ExcludeFromCodeCoverage>
    Public Sub ThrowIfNull(Of T As Class)(value As T, Optional message As String = Nothing)
        If value Is Nothing Then
            message = If(message, "Unexpected Null")
            Fail(message)
        End If
    End Sub

    ''' <summary>
    ''' Throws a non-accessible exception if the provided value is false.  This method executes
    ''' in all builds
    ''' </summary>
    <ExcludeFromCodeCoverage>
    Public Sub ThrowIfFalse(condition As Boolean, Optional message As String = Nothing)
        If Not condition Then
            message = If(message, "Unexpected false")
            Fail(message)
        End If
    End Sub

    ''' <summary>
    ''' Throws a non-accessible exception if the provided value is true. This method executes in
    ''' all builds.
    ''' </summary>
    <ExcludeFromCodeCoverage>
    Public Sub ThrowIfTrue(condition As Boolean, Optional message As String = Nothing)
        If condition Then
            message = If(message, "Unexpected true")
            Fail(message)
        End If
    End Sub

    <DebuggerHidden>
    <ExcludeFromCodeCoverage>
    Public Sub Fail(Optional message As String = "Unexpected")
        Throw New InvalidOperationException(message)
    End Sub

    <DebuggerHidden>
    <ExcludeFromCodeCoverage>
    Public Function FailWithReturn(Of T)(Optional message As String = "Unexpected") As T
        Throw New InvalidOperationException(message)
    End Function

    <ExcludeFromCodeCoverage>
    Public Sub InvalidEnumValue(Of T)(value As T)
        Fail(String.Format("Invalid Enumeration value {0}", value))
    End Sub

End Module
