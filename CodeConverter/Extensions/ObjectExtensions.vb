' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices

Public Module ObjectExtensions

#Region "TypeSwitch on Func<T>"

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TResult)(objType As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
        If objType Is Nothing Then
            Throw New ArgumentNullException(NameOf(objType))
        ElseIf matchFunc1 Is Nothing Then
            Throw New ArgumentNullException(NameOf(matchFunc1))
        ElseIf matchFunc2 Is Nothing Then
            Throw New ArgumentNullException(NameOf(matchFunc2))
        ElseIf matchFunc3 Is Nothing Then
            Throw New ArgumentNullException(NameOf(matchFunc3))
        ElseIf matchFunc4 Is Nothing Then
            Throw New ArgumentNullException(NameOf(matchFunc4))
        ElseIf matchFunc5 Is Nothing Then
            Throw New ArgumentNullException(NameOf(matchFunc5))
        End If
        If TypeOf objType Is TDerivedType1 Then
            Return matchFunc1(DirectCast(objType, TDerivedType1))
        ElseIf TypeOf objType Is TDerivedType2 Then
            Return matchFunc2(DirectCast(objType, TDerivedType2))
        ElseIf TypeOf objType Is TDerivedType3 Then
            Return matchFunc3(DirectCast(objType, TDerivedType3))
        ElseIf TypeOf objType Is TDerivedType4 Then
            Return matchFunc4(DirectCast(objType, TDerivedType4))
        ElseIf TypeOf objType Is TDerivedType5 Then
            Return matchFunc5(DirectCast(objType, TDerivedType5))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(objType)
        Else
            Return CType(Nothing, TResult)
        End If
    End Function

#End Region

End Module
