' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices

Namespace Extensions
    Friend Module TypeSwitchExtensions

#Region "TypeSwitch on Action"

        <Extension>
        Friend Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), matchAction2 As Action(Of TDerivedType2), matchAction3 As Action(Of TDerivedType3), Optional defaultAction As Action(Of TBaseType) = Nothing)
            If TypeOf obj Is TDerivedType1 Then
                matchAction1(DirectCast(obj, TDerivedType1))
            ElseIf TypeOf obj Is TDerivedType2 Then
                matchAction2(DirectCast(obj, TDerivedType2))
            ElseIf TypeOf obj Is TDerivedType3 Then
                matchAction3(DirectCast(obj, TDerivedType3))
            ElseIf defaultAction IsNot Nothing Then
                defaultAction(obj)
            End If
        End Sub

#End Region

#Region "TypeSwitch on Func<T>"

        <Extension>
        Friend Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
            If TypeOf obj Is TDerivedType1 Then
                Return matchFunc1(DirectCast(obj, TDerivedType1))
            ElseIf TypeOf obj Is TDerivedType2 Then
                Return matchFunc2(DirectCast(obj, TDerivedType2))
            ElseIf TypeOf obj Is TDerivedType3 Then
                Return matchFunc3(DirectCast(obj, TDerivedType3))
            ElseIf defaultFunc IsNot Nothing Then
                Return defaultFunc(obj)
            Else
                Return Nothing
            End If
        End Function

        <Extension>
        Friend Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
            TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
            Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
            If TypeOf obj Is TDerivedType1 Then
                Return matchFunc1(DirectCast(obj, TDerivedType1))
            ElseIf TypeOf obj Is TDerivedType2 Then
                Return matchFunc2(DirectCast(obj, TDerivedType2))
            ElseIf TypeOf obj Is TDerivedType3 Then
                Return matchFunc3(DirectCast(obj, TDerivedType3))
            ElseIf TypeOf obj Is TDerivedType4 Then
                Return matchFunc4(DirectCast(obj, TDerivedType4))
            ElseIf TypeOf obj Is TDerivedType5 Then
                Return matchFunc5(DirectCast(obj, TDerivedType5))
            ElseIf defaultFunc IsNot Nothing Then
                Return defaultFunc(obj)
            Else
                Return Nothing
            End If
        End Function

        <Extension>
        Friend Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
            TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
            matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
            If TypeOf obj Is TDerivedType1 Then
                Return matchFunc1(DirectCast(obj, TDerivedType1))
            ElseIf TypeOf obj Is TDerivedType2 Then
                Return matchFunc2(DirectCast(obj, TDerivedType2))
            ElseIf TypeOf obj Is TDerivedType3 Then
                Return matchFunc3(DirectCast(obj, TDerivedType3))
            ElseIf TypeOf obj Is TDerivedType4 Then
                Return matchFunc4(DirectCast(obj, TDerivedType4))
            ElseIf TypeOf obj Is TDerivedType5 Then
                Return matchFunc5(DirectCast(obj, TDerivedType5))
            ElseIf TypeOf obj Is TDerivedType6 Then
                Return matchFunc6(DirectCast(obj, TDerivedType6))
            ElseIf TypeOf obj Is TDerivedType7 Then
                Return matchFunc7(DirectCast(obj, TDerivedType7))
            ElseIf defaultFunc IsNot Nothing Then
                Return defaultFunc(obj)
            Else
                Return Nothing
            End If
        End Function

#End Region

    End Module
End Namespace
