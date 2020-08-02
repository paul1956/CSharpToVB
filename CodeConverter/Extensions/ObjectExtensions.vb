' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices

Public Module ObjectExtensions

#Region "TypeSwitch on Action"

    <Extension>
    Public Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), Optional defaultAction As Action(Of TBaseType) = Nothing)
        If TypeOf obj Is TDerivedType1 Then
            matchAction1(DirectCast(obj, TDerivedType1))
        ElseIf defaultAction IsNot Nothing Then
            defaultAction(obj)
        End If
    End Sub

    <Extension>
    Public Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), matchAction2 As Action(Of TDerivedType2), Optional defaultAction As Action(Of TBaseType) = Nothing)
        If TypeOf obj Is TDerivedType1 Then
            matchAction1(DirectCast(obj, TDerivedType1))
        ElseIf TypeOf obj Is TDerivedType2 Then
            matchAction2(DirectCast(obj, TDerivedType2))
        ElseIf defaultAction IsNot Nothing Then
            defaultAction(obj)
        End If
    End Sub

    <Extension>
    Public Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), matchAction2 As Action(Of TDerivedType2), matchAction3 As Action(Of TDerivedType3), Optional defaultAction As Action(Of TBaseType) = Nothing)
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

    <Extension>
    Public Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), matchAction2 As Action(Of TDerivedType2), matchAction3 As Action(Of TDerivedType3), matchAction4 As Action(Of TDerivedType4), Optional defaultAction As Action(Of TBaseType) = Nothing)
        If TypeOf obj Is TDerivedType1 Then
            matchAction1(DirectCast(obj, TDerivedType1))
        ElseIf TypeOf obj Is TDerivedType2 Then
            matchAction2(DirectCast(obj, TDerivedType2))
        ElseIf TypeOf obj Is TDerivedType3 Then
            matchAction3(DirectCast(obj, TDerivedType3))
        ElseIf TypeOf obj Is TDerivedType4 Then
            matchAction4(DirectCast(obj, TDerivedType4))
        ElseIf defaultAction IsNot Nothing Then
            defaultAction(obj)
        End If
    End Sub

    <Extension>
    Public Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), matchAction2 As Action(Of TDerivedType2), matchAction3 As Action(Of TDerivedType3), matchAction4 As Action(Of TDerivedType4), matchAction5 As Action(Of TDerivedType5),
        Optional defaultAction As Action(Of TBaseType) = Nothing)
        If TypeOf obj Is TDerivedType1 Then
            matchAction1(DirectCast(obj, TDerivedType1))
        ElseIf TypeOf obj Is TDerivedType2 Then
            matchAction2(DirectCast(obj, TDerivedType2))
        ElseIf TypeOf obj Is TDerivedType3 Then
            matchAction3(DirectCast(obj, TDerivedType3))
        ElseIf TypeOf obj Is TDerivedType4 Then
            matchAction4(DirectCast(obj, TDerivedType4))
        ElseIf TypeOf obj Is TDerivedType5 Then
            matchAction5(DirectCast(obj, TDerivedType5))
        ElseIf defaultAction IsNot Nothing Then
            defaultAction(obj)
        End If
    End Sub

    <Extension>
    Public Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), matchAction2 As Action(Of TDerivedType2), matchAction3 As Action(Of TDerivedType3), matchAction4 As Action(Of TDerivedType4), matchAction5 As Action(Of TDerivedType5),
        matchAction6 As Action(Of TDerivedType6), Optional defaultAction As Action(Of TBaseType) = Nothing)
        If TypeOf obj Is TDerivedType1 Then
            matchAction1(DirectCast(obj, TDerivedType1))
        ElseIf TypeOf obj Is TDerivedType2 Then
            matchAction2(DirectCast(obj, TDerivedType2))
        ElseIf TypeOf obj Is TDerivedType3 Then
            matchAction3(DirectCast(obj, TDerivedType3))
        ElseIf TypeOf obj Is TDerivedType4 Then
            matchAction4(DirectCast(obj, TDerivedType4))
        ElseIf TypeOf obj Is TDerivedType5 Then
            matchAction5(DirectCast(obj, TDerivedType5))
        ElseIf TypeOf obj Is TDerivedType6 Then
            matchAction6(DirectCast(obj, TDerivedType6))
        ElseIf defaultAction IsNot Nothing Then
            defaultAction(obj)
        End If
    End Sub

    <Extension>
    Public Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), matchAction2 As Action(Of TDerivedType2), matchAction3 As Action(Of TDerivedType3), matchAction4 As Action(Of TDerivedType4), matchAction5 As Action(Of TDerivedType5),
        matchAction6 As Action(Of TDerivedType6), matchAction7 As Action(Of TDerivedType7), Optional defaultAction As Action(Of TBaseType) = Nothing)
        If TypeOf obj Is TDerivedType1 Then
            matchAction1(DirectCast(obj, TDerivedType1))
        ElseIf TypeOf obj Is TDerivedType2 Then
            matchAction2(DirectCast(obj, TDerivedType2))
        ElseIf TypeOf obj Is TDerivedType3 Then
            matchAction3(DirectCast(obj, TDerivedType3))
        ElseIf TypeOf obj Is TDerivedType4 Then
            matchAction4(DirectCast(obj, TDerivedType4))
        ElseIf TypeOf obj Is TDerivedType5 Then
            matchAction5(DirectCast(obj, TDerivedType5))
        ElseIf TypeOf obj Is TDerivedType6 Then
            matchAction6(DirectCast(obj, TDerivedType6))
        ElseIf TypeOf obj Is TDerivedType7 Then
            matchAction7(DirectCast(obj, TDerivedType7))
        ElseIf defaultAction IsNot Nothing Then
            defaultAction(obj)
        End If
    End Sub

    <Extension>
    Public Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), matchAction2 As Action(Of TDerivedType2), matchAction3 As Action(Of TDerivedType3), matchAction4 As Action(Of TDerivedType4), matchAction5 As Action(Of TDerivedType5),
        matchAction6 As Action(Of TDerivedType6), matchAction7 As Action(Of TDerivedType7), matchAction8 As Action(Of TDerivedType8), Optional defaultAction As Action(Of TBaseType) = Nothing)
        If TypeOf obj Is TDerivedType1 Then
            matchAction1(DirectCast(obj, TDerivedType1))
        ElseIf TypeOf obj Is TDerivedType2 Then
            matchAction2(DirectCast(obj, TDerivedType2))
        ElseIf TypeOf obj Is TDerivedType3 Then
            matchAction3(DirectCast(obj, TDerivedType3))
        ElseIf TypeOf obj Is TDerivedType4 Then
            matchAction4(DirectCast(obj, TDerivedType4))
        ElseIf TypeOf obj Is TDerivedType5 Then
            matchAction5(DirectCast(obj, TDerivedType5))
        ElseIf TypeOf obj Is TDerivedType6 Then
            matchAction6(DirectCast(obj, TDerivedType6))
        ElseIf TypeOf obj Is TDerivedType7 Then
            matchAction7(DirectCast(obj, TDerivedType7))
        ElseIf TypeOf obj Is TDerivedType8 Then
            matchAction8(DirectCast(obj, TDerivedType8))
        ElseIf defaultAction IsNot Nothing Then
            defaultAction(obj)
        End If
    End Sub

    <Extension>
    Public Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), matchAction2 As Action(Of TDerivedType2), matchAction3 As Action(Of TDerivedType3), matchAction4 As Action(Of TDerivedType4), matchAction5 As Action(Of TDerivedType5),
        matchAction6 As Action(Of TDerivedType6), matchAction7 As Action(Of TDerivedType7), matchAction8 As Action(Of TDerivedType8), matchAction9 As Action(Of TDerivedType9), Optional defaultAction As Action(Of TBaseType) = Nothing)
        If TypeOf obj Is TDerivedType1 Then
            matchAction1(DirectCast(obj, TDerivedType1))
        ElseIf TypeOf obj Is TDerivedType2 Then
            matchAction2(DirectCast(obj, TDerivedType2))
        ElseIf TypeOf obj Is TDerivedType3 Then
            matchAction3(DirectCast(obj, TDerivedType3))
        ElseIf TypeOf obj Is TDerivedType4 Then
            matchAction4(DirectCast(obj, TDerivedType4))
        ElseIf TypeOf obj Is TDerivedType5 Then
            matchAction5(DirectCast(obj, TDerivedType5))
        ElseIf TypeOf obj Is TDerivedType6 Then
            matchAction6(DirectCast(obj, TDerivedType6))
        ElseIf TypeOf obj Is TDerivedType7 Then
            matchAction7(DirectCast(obj, TDerivedType7))
        ElseIf TypeOf obj Is TDerivedType8 Then
            matchAction8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            matchAction9(DirectCast(obj, TDerivedType9))
        ElseIf defaultAction IsNot Nothing Then
            defaultAction(obj)
        End If
    End Sub

    <Extension>
    Public Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), matchAction2 As Action(Of TDerivedType2), matchAction3 As Action(Of TDerivedType3), matchAction4 As Action(Of TDerivedType4), matchAction5 As Action(Of TDerivedType5),
        matchAction6 As Action(Of TDerivedType6), matchAction7 As Action(Of TDerivedType7), matchAction8 As Action(Of TDerivedType8), matchAction9 As Action(Of TDerivedType9), matchAction10 As Action(Of TDerivedType10), Optional defaultAction As Action(Of TBaseType) = Nothing)
        If TypeOf obj Is TDerivedType1 Then
            matchAction1(DirectCast(obj, TDerivedType1))
        ElseIf TypeOf obj Is TDerivedType2 Then
            matchAction2(DirectCast(obj, TDerivedType2))
        ElseIf TypeOf obj Is TDerivedType3 Then
            matchAction3(DirectCast(obj, TDerivedType3))
        ElseIf TypeOf obj Is TDerivedType4 Then
            matchAction4(DirectCast(obj, TDerivedType4))
        ElseIf TypeOf obj Is TDerivedType5 Then
            matchAction5(DirectCast(obj, TDerivedType5))
        ElseIf TypeOf obj Is TDerivedType6 Then
            matchAction6(DirectCast(obj, TDerivedType6))
        ElseIf TypeOf obj Is TDerivedType7 Then
            matchAction7(DirectCast(obj, TDerivedType7))
        ElseIf TypeOf obj Is TDerivedType8 Then
            matchAction8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            matchAction9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            matchAction10(DirectCast(obj, TDerivedType10))
        ElseIf defaultAction IsNot Nothing Then
            defaultAction(obj)
        End If
    End Sub

    <Extension>
    Public Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), matchAction2 As Action(Of TDerivedType2), matchAction3 As Action(Of TDerivedType3), matchAction4 As Action(Of TDerivedType4), matchAction5 As Action(Of TDerivedType5),
        matchAction6 As Action(Of TDerivedType6), matchAction7 As Action(Of TDerivedType7), matchAction8 As Action(Of TDerivedType8), matchAction9 As Action(Of TDerivedType9), matchAction10 As Action(Of TDerivedType10), matchAction11 As Action(Of TDerivedType11),
        Optional defaultAction As Action(Of TBaseType) = Nothing)
        If TypeOf obj Is TDerivedType1 Then
            matchAction1(DirectCast(obj, TDerivedType1))
        ElseIf TypeOf obj Is TDerivedType2 Then
            matchAction2(DirectCast(obj, TDerivedType2))
        ElseIf TypeOf obj Is TDerivedType3 Then
            matchAction3(DirectCast(obj, TDerivedType3))
        ElseIf TypeOf obj Is TDerivedType4 Then
            matchAction4(DirectCast(obj, TDerivedType4))
        ElseIf TypeOf obj Is TDerivedType5 Then
            matchAction5(DirectCast(obj, TDerivedType5))
        ElseIf TypeOf obj Is TDerivedType6 Then
            matchAction6(DirectCast(obj, TDerivedType6))
        ElseIf TypeOf obj Is TDerivedType7 Then
            matchAction7(DirectCast(obj, TDerivedType7))
        ElseIf TypeOf obj Is TDerivedType8 Then
            matchAction8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            matchAction9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            matchAction10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            matchAction11(DirectCast(obj, TDerivedType11))
        ElseIf defaultAction IsNot Nothing Then
            defaultAction(obj)
        End If
    End Sub

    <Extension>
    Public Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), matchAction2 As Action(Of TDerivedType2), matchAction3 As Action(Of TDerivedType3), matchAction4 As Action(Of TDerivedType4), matchAction5 As Action(Of TDerivedType5),
        matchAction6 As Action(Of TDerivedType6), matchAction7 As Action(Of TDerivedType7), matchAction8 As Action(Of TDerivedType8), matchAction9 As Action(Of TDerivedType9), matchAction10 As Action(Of TDerivedType10), matchAction11 As Action(Of TDerivedType11),
        matchAction12 As Action(Of TDerivedType12), Optional defaultAction As Action(Of TBaseType) = Nothing)
        If TypeOf obj Is TDerivedType1 Then
            matchAction1(DirectCast(obj, TDerivedType1))
        ElseIf TypeOf obj Is TDerivedType2 Then
            matchAction2(DirectCast(obj, TDerivedType2))
        ElseIf TypeOf obj Is TDerivedType3 Then
            matchAction3(DirectCast(obj, TDerivedType3))
        ElseIf TypeOf obj Is TDerivedType4 Then
            matchAction4(DirectCast(obj, TDerivedType4))
        ElseIf TypeOf obj Is TDerivedType5 Then
            matchAction5(DirectCast(obj, TDerivedType5))
        ElseIf TypeOf obj Is TDerivedType6 Then
            matchAction6(DirectCast(obj, TDerivedType6))
        ElseIf TypeOf obj Is TDerivedType7 Then
            matchAction7(DirectCast(obj, TDerivedType7))
        ElseIf TypeOf obj Is TDerivedType8 Then
            matchAction8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            matchAction9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            matchAction10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            matchAction11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            matchAction12(DirectCast(obj, TDerivedType12))
        ElseIf defaultAction IsNot Nothing Then
            defaultAction(obj)
        End If
    End Sub

    <Extension>
    Public Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), matchAction2 As Action(Of TDerivedType2), matchAction3 As Action(Of TDerivedType3), matchAction4 As Action(Of TDerivedType4), matchAction5 As Action(Of TDerivedType5),
        matchAction6 As Action(Of TDerivedType6), matchAction7 As Action(Of TDerivedType7), matchAction8 As Action(Of TDerivedType8), matchAction9 As Action(Of TDerivedType9), matchAction10 As Action(Of TDerivedType10), matchAction11 As Action(Of TDerivedType11),
        matchAction12 As Action(Of TDerivedType12), matchAction13 As Action(Of TDerivedType13), Optional defaultAction As Action(Of TBaseType) = Nothing)
        If TypeOf obj Is TDerivedType1 Then
            matchAction1(DirectCast(obj, TDerivedType1))
        ElseIf TypeOf obj Is TDerivedType2 Then
            matchAction2(DirectCast(obj, TDerivedType2))
        ElseIf TypeOf obj Is TDerivedType3 Then
            matchAction3(DirectCast(obj, TDerivedType3))
        ElseIf TypeOf obj Is TDerivedType4 Then
            matchAction4(DirectCast(obj, TDerivedType4))
        ElseIf TypeOf obj Is TDerivedType5 Then
            matchAction5(DirectCast(obj, TDerivedType5))
        ElseIf TypeOf obj Is TDerivedType6 Then
            matchAction6(DirectCast(obj, TDerivedType6))
        ElseIf TypeOf obj Is TDerivedType7 Then
            matchAction7(DirectCast(obj, TDerivedType7))
        ElseIf TypeOf obj Is TDerivedType8 Then
            matchAction8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            matchAction9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            matchAction10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            matchAction11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            matchAction12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            matchAction13(DirectCast(obj, TDerivedType13))
        ElseIf defaultAction IsNot Nothing Then
            defaultAction(obj)
        End If
    End Sub

    <Extension>
    Public Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), matchAction2 As Action(Of TDerivedType2), matchAction3 As Action(Of TDerivedType3), matchAction4 As Action(Of TDerivedType4), matchAction5 As Action(Of TDerivedType5),
        matchAction6 As Action(Of TDerivedType6), matchAction7 As Action(Of TDerivedType7), matchAction8 As Action(Of TDerivedType8), matchAction9 As Action(Of TDerivedType9), matchAction10 As Action(Of TDerivedType10), matchAction11 As Action(Of TDerivedType11),
        matchAction12 As Action(Of TDerivedType12), matchAction13 As Action(Of TDerivedType13), matchAction14 As Action(Of TDerivedType14), Optional defaultAction As Action(Of TBaseType) = Nothing)
        If TypeOf obj Is TDerivedType1 Then
            matchAction1(DirectCast(obj, TDerivedType1))
        ElseIf TypeOf obj Is TDerivedType2 Then
            matchAction2(DirectCast(obj, TDerivedType2))
        ElseIf TypeOf obj Is TDerivedType3 Then
            matchAction3(DirectCast(obj, TDerivedType3))
        ElseIf TypeOf obj Is TDerivedType4 Then
            matchAction4(DirectCast(obj, TDerivedType4))
        ElseIf TypeOf obj Is TDerivedType5 Then
            matchAction5(DirectCast(obj, TDerivedType5))
        ElseIf TypeOf obj Is TDerivedType6 Then
            matchAction6(DirectCast(obj, TDerivedType6))
        ElseIf TypeOf obj Is TDerivedType7 Then
            matchAction7(DirectCast(obj, TDerivedType7))
        ElseIf TypeOf obj Is TDerivedType8 Then
            matchAction8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            matchAction9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            matchAction10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            matchAction11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            matchAction12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            matchAction13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            matchAction14(DirectCast(obj, TDerivedType14))
        ElseIf defaultAction IsNot Nothing Then
            defaultAction(obj)
        End If
    End Sub

    <Extension>
    Public Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), matchAction2 As Action(Of TDerivedType2), matchAction3 As Action(Of TDerivedType3), matchAction4 As Action(Of TDerivedType4), matchAction5 As Action(Of TDerivedType5),
        matchAction6 As Action(Of TDerivedType6), matchAction7 As Action(Of TDerivedType7), matchAction8 As Action(Of TDerivedType8), matchAction9 As Action(Of TDerivedType9), matchAction10 As Action(Of TDerivedType10), matchAction11 As Action(Of TDerivedType11),
        matchAction12 As Action(Of TDerivedType12), matchAction13 As Action(Of TDerivedType13), matchAction14 As Action(Of TDerivedType14), matchAction15 As Action(Of TDerivedType15), Optional defaultAction As Action(Of TBaseType) = Nothing)
        If TypeOf obj Is TDerivedType1 Then
            matchAction1(DirectCast(obj, TDerivedType1))
        ElseIf TypeOf obj Is TDerivedType2 Then
            matchAction2(DirectCast(obj, TDerivedType2))
        ElseIf TypeOf obj Is TDerivedType3 Then
            matchAction3(DirectCast(obj, TDerivedType3))
        ElseIf TypeOf obj Is TDerivedType4 Then
            matchAction4(DirectCast(obj, TDerivedType4))
        ElseIf TypeOf obj Is TDerivedType5 Then
            matchAction5(DirectCast(obj, TDerivedType5))
        ElseIf TypeOf obj Is TDerivedType6 Then
            matchAction6(DirectCast(obj, TDerivedType6))
        ElseIf TypeOf obj Is TDerivedType7 Then
            matchAction7(DirectCast(obj, TDerivedType7))
        ElseIf TypeOf obj Is TDerivedType8 Then
            matchAction8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            matchAction9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            matchAction10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            matchAction11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            matchAction12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            matchAction13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            matchAction14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            matchAction15(DirectCast(obj, TDerivedType15))
        ElseIf defaultAction IsNot Nothing Then
            defaultAction(obj)
        End If
    End Sub

    <Extension>
    Public Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), matchAction2 As Action(Of TDerivedType2), matchAction3 As Action(Of TDerivedType3), matchAction4 As Action(Of TDerivedType4), matchAction5 As Action(Of TDerivedType5),
        matchAction6 As Action(Of TDerivedType6), matchAction7 As Action(Of TDerivedType7), matchAction8 As Action(Of TDerivedType8), matchAction9 As Action(Of TDerivedType9), matchAction10 As Action(Of TDerivedType10), matchAction11 As Action(Of TDerivedType11),
        matchAction12 As Action(Of TDerivedType12), matchAction13 As Action(Of TDerivedType13), matchAction14 As Action(Of TDerivedType14), matchAction15 As Action(Of TDerivedType15), matchAction16 As Action(Of TDerivedType16), Optional defaultAction As Action(Of TBaseType) = Nothing)
        If TypeOf obj Is TDerivedType1 Then
            matchAction1(DirectCast(obj, TDerivedType1))
        ElseIf TypeOf obj Is TDerivedType2 Then
            matchAction2(DirectCast(obj, TDerivedType2))
        ElseIf TypeOf obj Is TDerivedType3 Then
            matchAction3(DirectCast(obj, TDerivedType3))
        ElseIf TypeOf obj Is TDerivedType4 Then
            matchAction4(DirectCast(obj, TDerivedType4))
        ElseIf TypeOf obj Is TDerivedType5 Then
            matchAction5(DirectCast(obj, TDerivedType5))
        ElseIf TypeOf obj Is TDerivedType6 Then
            matchAction6(DirectCast(obj, TDerivedType6))
        ElseIf TypeOf obj Is TDerivedType7 Then
            matchAction7(DirectCast(obj, TDerivedType7))
        ElseIf TypeOf obj Is TDerivedType8 Then
            matchAction8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            matchAction9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            matchAction10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            matchAction11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            matchAction12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            matchAction13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            matchAction14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            matchAction15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            matchAction16(DirectCast(obj, TDerivedType16))
        ElseIf defaultAction IsNot Nothing Then
            defaultAction(obj)
        End If
    End Sub

    <Extension>
    Public Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), matchAction2 As Action(Of TDerivedType2), matchAction3 As Action(Of TDerivedType3), matchAction4 As Action(Of TDerivedType4), matchAction5 As Action(Of TDerivedType5),
        matchAction6 As Action(Of TDerivedType6), matchAction7 As Action(Of TDerivedType7), matchAction8 As Action(Of TDerivedType8), matchAction9 As Action(Of TDerivedType9), matchAction10 As Action(Of TDerivedType10), matchAction11 As Action(Of TDerivedType11),
        matchAction12 As Action(Of TDerivedType12), matchAction13 As Action(Of TDerivedType13), matchAction14 As Action(Of TDerivedType14), matchAction15 As Action(Of TDerivedType15), matchAction16 As Action(Of TDerivedType16), matchAction17 As Action(Of TDerivedType17),
        Optional defaultAction As Action(Of TBaseType) = Nothing)
        If TypeOf obj Is TDerivedType1 Then
            matchAction1(DirectCast(obj, TDerivedType1))
        ElseIf TypeOf obj Is TDerivedType2 Then
            matchAction2(DirectCast(obj, TDerivedType2))
        ElseIf TypeOf obj Is TDerivedType3 Then
            matchAction3(DirectCast(obj, TDerivedType3))
        ElseIf TypeOf obj Is TDerivedType4 Then
            matchAction4(DirectCast(obj, TDerivedType4))
        ElseIf TypeOf obj Is TDerivedType5 Then
            matchAction5(DirectCast(obj, TDerivedType5))
        ElseIf TypeOf obj Is TDerivedType6 Then
            matchAction6(DirectCast(obj, TDerivedType6))
        ElseIf TypeOf obj Is TDerivedType7 Then
            matchAction7(DirectCast(obj, TDerivedType7))
        ElseIf TypeOf obj Is TDerivedType8 Then
            matchAction8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            matchAction9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            matchAction10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            matchAction11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            matchAction12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            matchAction13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            matchAction14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            matchAction15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            matchAction16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            matchAction17(DirectCast(obj, TDerivedType17))
        ElseIf defaultAction IsNot Nothing Then
            defaultAction(obj)
        End If
    End Sub

    <Extension>
    Public Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), matchAction2 As Action(Of TDerivedType2), matchAction3 As Action(Of TDerivedType3), matchAction4 As Action(Of TDerivedType4), matchAction5 As Action(Of TDerivedType5),
        matchAction6 As Action(Of TDerivedType6), matchAction7 As Action(Of TDerivedType7), matchAction8 As Action(Of TDerivedType8), matchAction9 As Action(Of TDerivedType9), matchAction10 As Action(Of TDerivedType10), matchAction11 As Action(Of TDerivedType11),
        matchAction12 As Action(Of TDerivedType12), matchAction13 As Action(Of TDerivedType13), matchAction14 As Action(Of TDerivedType14), matchAction15 As Action(Of TDerivedType15), matchAction16 As Action(Of TDerivedType16), matchAction17 As Action(Of TDerivedType17),
        matchAction18 As Action(Of TDerivedType18), Optional defaultAction As Action(Of TBaseType) = Nothing)
        If TypeOf obj Is TDerivedType1 Then
            matchAction1(DirectCast(obj, TDerivedType1))
        ElseIf TypeOf obj Is TDerivedType2 Then
            matchAction2(DirectCast(obj, TDerivedType2))
        ElseIf TypeOf obj Is TDerivedType3 Then
            matchAction3(DirectCast(obj, TDerivedType3))
        ElseIf TypeOf obj Is TDerivedType4 Then
            matchAction4(DirectCast(obj, TDerivedType4))
        ElseIf TypeOf obj Is TDerivedType5 Then
            matchAction5(DirectCast(obj, TDerivedType5))
        ElseIf TypeOf obj Is TDerivedType6 Then
            matchAction6(DirectCast(obj, TDerivedType6))
        ElseIf TypeOf obj Is TDerivedType7 Then
            matchAction7(DirectCast(obj, TDerivedType7))
        ElseIf TypeOf obj Is TDerivedType8 Then
            matchAction8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            matchAction9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            matchAction10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            matchAction11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            matchAction12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            matchAction13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            matchAction14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            matchAction15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            matchAction16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            matchAction17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            matchAction18(DirectCast(obj, TDerivedType18))
        ElseIf defaultAction IsNot Nothing Then
            defaultAction(obj)
        End If
    End Sub

    <Extension>
    Public Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), matchAction2 As Action(Of TDerivedType2), matchAction3 As Action(Of TDerivedType3), matchAction4 As Action(Of TDerivedType4), matchAction5 As Action(Of TDerivedType5),
        matchAction6 As Action(Of TDerivedType6), matchAction7 As Action(Of TDerivedType7), matchAction8 As Action(Of TDerivedType8), matchAction9 As Action(Of TDerivedType9), matchAction10 As Action(Of TDerivedType10), matchAction11 As Action(Of TDerivedType11),
        matchAction12 As Action(Of TDerivedType12), matchAction13 As Action(Of TDerivedType13), matchAction14 As Action(Of TDerivedType14), matchAction15 As Action(Of TDerivedType15), matchAction16 As Action(Of TDerivedType16), matchAction17 As Action(Of TDerivedType17),
        matchAction18 As Action(Of TDerivedType18), matchAction19 As Action(Of TDerivedType19), Optional defaultAction As Action(Of TBaseType) = Nothing)
        If TypeOf obj Is TDerivedType1 Then
            matchAction1(DirectCast(obj, TDerivedType1))
        ElseIf TypeOf obj Is TDerivedType2 Then
            matchAction2(DirectCast(obj, TDerivedType2))
        ElseIf TypeOf obj Is TDerivedType3 Then
            matchAction3(DirectCast(obj, TDerivedType3))
        ElseIf TypeOf obj Is TDerivedType4 Then
            matchAction4(DirectCast(obj, TDerivedType4))
        ElseIf TypeOf obj Is TDerivedType5 Then
            matchAction5(DirectCast(obj, TDerivedType5))
        ElseIf TypeOf obj Is TDerivedType6 Then
            matchAction6(DirectCast(obj, TDerivedType6))
        ElseIf TypeOf obj Is TDerivedType7 Then
            matchAction7(DirectCast(obj, TDerivedType7))
        ElseIf TypeOf obj Is TDerivedType8 Then
            matchAction8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            matchAction9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            matchAction10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            matchAction11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            matchAction12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            matchAction13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            matchAction14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            matchAction15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            matchAction16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            matchAction17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            matchAction18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            matchAction19(DirectCast(obj, TDerivedType19))
        ElseIf defaultAction IsNot Nothing Then
            defaultAction(obj)
        End If
    End Sub

    <Extension>
    Public Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), matchAction2 As Action(Of TDerivedType2), matchAction3 As Action(Of TDerivedType3), matchAction4 As Action(Of TDerivedType4), matchAction5 As Action(Of TDerivedType5),
        matchAction6 As Action(Of TDerivedType6), matchAction7 As Action(Of TDerivedType7), matchAction8 As Action(Of TDerivedType8), matchAction9 As Action(Of TDerivedType9), matchAction10 As Action(Of TDerivedType10), matchAction11 As Action(Of TDerivedType11),
        matchAction12 As Action(Of TDerivedType12), matchAction13 As Action(Of TDerivedType13), matchAction14 As Action(Of TDerivedType14), matchAction15 As Action(Of TDerivedType15), matchAction16 As Action(Of TDerivedType16), matchAction17 As Action(Of TDerivedType17),
        matchAction18 As Action(Of TDerivedType18), matchAction19 As Action(Of TDerivedType19), matchAction20 As Action(Of TDerivedType20), Optional defaultAction As Action(Of TBaseType) = Nothing)
        If TypeOf obj Is TDerivedType1 Then
            matchAction1(DirectCast(obj, TDerivedType1))
        ElseIf TypeOf obj Is TDerivedType2 Then
            matchAction2(DirectCast(obj, TDerivedType2))
        ElseIf TypeOf obj Is TDerivedType3 Then
            matchAction3(DirectCast(obj, TDerivedType3))
        ElseIf TypeOf obj Is TDerivedType4 Then
            matchAction4(DirectCast(obj, TDerivedType4))
        ElseIf TypeOf obj Is TDerivedType5 Then
            matchAction5(DirectCast(obj, TDerivedType5))
        ElseIf TypeOf obj Is TDerivedType6 Then
            matchAction6(DirectCast(obj, TDerivedType6))
        ElseIf TypeOf obj Is TDerivedType7 Then
            matchAction7(DirectCast(obj, TDerivedType7))
        ElseIf TypeOf obj Is TDerivedType8 Then
            matchAction8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            matchAction9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            matchAction10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            matchAction11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            matchAction12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            matchAction13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            matchAction14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            matchAction15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            matchAction16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            matchAction17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            matchAction18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            matchAction19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            matchAction20(DirectCast(obj, TDerivedType20))
        ElseIf defaultAction IsNot Nothing Then
            defaultAction(obj)
        End If
    End Sub

    <Extension>
    Public Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TDerivedType21 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), matchAction2 As Action(Of TDerivedType2), matchAction3 As Action(Of TDerivedType3), matchAction4 As Action(Of TDerivedType4), matchAction5 As Action(Of TDerivedType5),
        matchAction6 As Action(Of TDerivedType6), matchAction7 As Action(Of TDerivedType7), matchAction8 As Action(Of TDerivedType8), matchAction9 As Action(Of TDerivedType9), matchAction10 As Action(Of TDerivedType10), matchAction11 As Action(Of TDerivedType11),
        matchAction12 As Action(Of TDerivedType12), matchAction13 As Action(Of TDerivedType13), matchAction14 As Action(Of TDerivedType14), matchAction15 As Action(Of TDerivedType15), matchAction16 As Action(Of TDerivedType16), matchAction17 As Action(Of TDerivedType17),
        matchAction18 As Action(Of TDerivedType18), matchAction19 As Action(Of TDerivedType19), matchAction20 As Action(Of TDerivedType20), matchAction21 As Action(Of TDerivedType21), Optional defaultAction As Action(Of TBaseType) = Nothing)
        If TypeOf obj Is TDerivedType1 Then
            matchAction1(DirectCast(obj, TDerivedType1))
        ElseIf TypeOf obj Is TDerivedType2 Then
            matchAction2(DirectCast(obj, TDerivedType2))
        ElseIf TypeOf obj Is TDerivedType3 Then
            matchAction3(DirectCast(obj, TDerivedType3))
        ElseIf TypeOf obj Is TDerivedType4 Then
            matchAction4(DirectCast(obj, TDerivedType4))
        ElseIf TypeOf obj Is TDerivedType5 Then
            matchAction5(DirectCast(obj, TDerivedType5))
        ElseIf TypeOf obj Is TDerivedType6 Then
            matchAction6(DirectCast(obj, TDerivedType6))
        ElseIf TypeOf obj Is TDerivedType7 Then
            matchAction7(DirectCast(obj, TDerivedType7))
        ElseIf TypeOf obj Is TDerivedType8 Then
            matchAction8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            matchAction9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            matchAction10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            matchAction11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            matchAction12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            matchAction13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            matchAction14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            matchAction15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            matchAction16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            matchAction17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            matchAction18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            matchAction19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            matchAction20(DirectCast(obj, TDerivedType20))
        ElseIf TypeOf obj Is TDerivedType21 Then
            matchAction21(DirectCast(obj, TDerivedType21))
        ElseIf defaultAction IsNot Nothing Then
            defaultAction(obj)
        End If
    End Sub

    <Extension>
    Public Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TDerivedType21 As TBaseType, TDerivedType22 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), matchAction2 As Action(Of TDerivedType2), matchAction3 As Action(Of TDerivedType3), matchAction4 As Action(Of TDerivedType4), matchAction5 As Action(Of TDerivedType5),
        matchAction6 As Action(Of TDerivedType6), matchAction7 As Action(Of TDerivedType7), matchAction8 As Action(Of TDerivedType8), matchAction9 As Action(Of TDerivedType9), matchAction10 As Action(Of TDerivedType10), matchAction11 As Action(Of TDerivedType11),
        matchAction12 As Action(Of TDerivedType12), matchAction13 As Action(Of TDerivedType13), matchAction14 As Action(Of TDerivedType14), matchAction15 As Action(Of TDerivedType15), matchAction16 As Action(Of TDerivedType16), matchAction17 As Action(Of TDerivedType17),
        matchAction18 As Action(Of TDerivedType18), matchAction19 As Action(Of TDerivedType19), matchAction20 As Action(Of TDerivedType20), matchAction21 As Action(Of TDerivedType21), matchAction22 As Action(Of TDerivedType22), Optional defaultAction As Action(Of TBaseType) = Nothing)
        If TypeOf obj Is TDerivedType1 Then
            matchAction1(DirectCast(obj, TDerivedType1))
        ElseIf TypeOf obj Is TDerivedType2 Then
            matchAction2(DirectCast(obj, TDerivedType2))
        ElseIf TypeOf obj Is TDerivedType3 Then
            matchAction3(DirectCast(obj, TDerivedType3))
        ElseIf TypeOf obj Is TDerivedType4 Then
            matchAction4(DirectCast(obj, TDerivedType4))
        ElseIf TypeOf obj Is TDerivedType5 Then
            matchAction5(DirectCast(obj, TDerivedType5))
        ElseIf TypeOf obj Is TDerivedType6 Then
            matchAction6(DirectCast(obj, TDerivedType6))
        ElseIf TypeOf obj Is TDerivedType7 Then
            matchAction7(DirectCast(obj, TDerivedType7))
        ElseIf TypeOf obj Is TDerivedType8 Then
            matchAction8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            matchAction9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            matchAction10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            matchAction11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            matchAction12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            matchAction13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            matchAction14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            matchAction15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            matchAction16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            matchAction17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            matchAction18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            matchAction19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            matchAction20(DirectCast(obj, TDerivedType20))
        ElseIf TypeOf obj Is TDerivedType21 Then
            matchAction21(DirectCast(obj, TDerivedType21))
        ElseIf TypeOf obj Is TDerivedType22 Then
            matchAction22(DirectCast(obj, TDerivedType22))
        ElseIf defaultAction IsNot Nothing Then
            defaultAction(obj)
        End If
    End Sub

    <Extension>
    Public Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TDerivedType21 As TBaseType, TDerivedType22 As TBaseType, TDerivedType23 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), matchAction2 As Action(Of TDerivedType2), matchAction3 As Action(Of TDerivedType3), matchAction4 As Action(Of TDerivedType4), matchAction5 As Action(Of TDerivedType5),
        matchAction6 As Action(Of TDerivedType6), matchAction7 As Action(Of TDerivedType7), matchAction8 As Action(Of TDerivedType8), matchAction9 As Action(Of TDerivedType9), matchAction10 As Action(Of TDerivedType10), matchAction11 As Action(Of TDerivedType11),
        matchAction12 As Action(Of TDerivedType12), matchAction13 As Action(Of TDerivedType13), matchAction14 As Action(Of TDerivedType14), matchAction15 As Action(Of TDerivedType15), matchAction16 As Action(Of TDerivedType16), matchAction17 As Action(Of TDerivedType17),
        matchAction18 As Action(Of TDerivedType18), matchAction19 As Action(Of TDerivedType19), matchAction20 As Action(Of TDerivedType20), matchAction21 As Action(Of TDerivedType21), matchAction22 As Action(Of TDerivedType22), matchAction23 As Action(Of TDerivedType23),
        Optional defaultAction As Action(Of TBaseType) = Nothing)
        If TypeOf obj Is TDerivedType1 Then
            matchAction1(DirectCast(obj, TDerivedType1))
        ElseIf TypeOf obj Is TDerivedType2 Then
            matchAction2(DirectCast(obj, TDerivedType2))
        ElseIf TypeOf obj Is TDerivedType3 Then
            matchAction3(DirectCast(obj, TDerivedType3))
        ElseIf TypeOf obj Is TDerivedType4 Then
            matchAction4(DirectCast(obj, TDerivedType4))
        ElseIf TypeOf obj Is TDerivedType5 Then
            matchAction5(DirectCast(obj, TDerivedType5))
        ElseIf TypeOf obj Is TDerivedType6 Then
            matchAction6(DirectCast(obj, TDerivedType6))
        ElseIf TypeOf obj Is TDerivedType7 Then
            matchAction7(DirectCast(obj, TDerivedType7))
        ElseIf TypeOf obj Is TDerivedType8 Then
            matchAction8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            matchAction9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            matchAction10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            matchAction11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            matchAction12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            matchAction13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            matchAction14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            matchAction15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            matchAction16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            matchAction17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            matchAction18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            matchAction19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            matchAction20(DirectCast(obj, TDerivedType20))
        ElseIf TypeOf obj Is TDerivedType21 Then
            matchAction21(DirectCast(obj, TDerivedType21))
        ElseIf TypeOf obj Is TDerivedType22 Then
            matchAction22(DirectCast(obj, TDerivedType22))
        ElseIf TypeOf obj Is TDerivedType23 Then
            matchAction23(DirectCast(obj, TDerivedType23))
        ElseIf defaultAction IsNot Nothing Then
            defaultAction(obj)
        End If
    End Sub

    <Extension>
    Public Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TDerivedType21 As TBaseType, TDerivedType22 As TBaseType, TDerivedType23 As TBaseType,
        TDerivedType24 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), matchAction2 As Action(Of TDerivedType2), matchAction3 As Action(Of TDerivedType3), matchAction4 As Action(Of TDerivedType4), matchAction5 As Action(Of TDerivedType5),
        matchAction6 As Action(Of TDerivedType6), matchAction7 As Action(Of TDerivedType7), matchAction8 As Action(Of TDerivedType8), matchAction9 As Action(Of TDerivedType9), matchAction10 As Action(Of TDerivedType10), matchAction11 As Action(Of TDerivedType11),
        matchAction12 As Action(Of TDerivedType12), matchAction13 As Action(Of TDerivedType13), matchAction14 As Action(Of TDerivedType14), matchAction15 As Action(Of TDerivedType15), matchAction16 As Action(Of TDerivedType16), matchAction17 As Action(Of TDerivedType17),
        matchAction18 As Action(Of TDerivedType18), matchAction19 As Action(Of TDerivedType19), matchAction20 As Action(Of TDerivedType20), matchAction21 As Action(Of TDerivedType21), matchAction22 As Action(Of TDerivedType22), matchAction23 As Action(Of TDerivedType23),
        matchAction24 As Action(Of TDerivedType24), Optional defaultAction As Action(Of TBaseType) = Nothing)
        If TypeOf obj Is TDerivedType1 Then
            matchAction1(DirectCast(obj, TDerivedType1))
        ElseIf TypeOf obj Is TDerivedType2 Then
            matchAction2(DirectCast(obj, TDerivedType2))
        ElseIf TypeOf obj Is TDerivedType3 Then
            matchAction3(DirectCast(obj, TDerivedType3))
        ElseIf TypeOf obj Is TDerivedType4 Then
            matchAction4(DirectCast(obj, TDerivedType4))
        ElseIf TypeOf obj Is TDerivedType5 Then
            matchAction5(DirectCast(obj, TDerivedType5))
        ElseIf TypeOf obj Is TDerivedType6 Then
            matchAction6(DirectCast(obj, TDerivedType6))
        ElseIf TypeOf obj Is TDerivedType7 Then
            matchAction7(DirectCast(obj, TDerivedType7))
        ElseIf TypeOf obj Is TDerivedType8 Then
            matchAction8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            matchAction9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            matchAction10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            matchAction11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            matchAction12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            matchAction13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            matchAction14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            matchAction15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            matchAction16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            matchAction17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            matchAction18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            matchAction19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            matchAction20(DirectCast(obj, TDerivedType20))
        ElseIf TypeOf obj Is TDerivedType21 Then
            matchAction21(DirectCast(obj, TDerivedType21))
        ElseIf TypeOf obj Is TDerivedType22 Then
            matchAction22(DirectCast(obj, TDerivedType22))
        ElseIf TypeOf obj Is TDerivedType23 Then
            matchAction23(DirectCast(obj, TDerivedType23))
        ElseIf TypeOf obj Is TDerivedType24 Then
            matchAction24(DirectCast(obj, TDerivedType24))
        ElseIf defaultAction IsNot Nothing Then
            defaultAction(obj)
        End If
    End Sub

    <Extension>
    Public Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TDerivedType21 As TBaseType, TDerivedType22 As TBaseType, TDerivedType23 As TBaseType,
        TDerivedType24 As TBaseType, TDerivedType25 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), matchAction2 As Action(Of TDerivedType2), matchAction3 As Action(Of TDerivedType3), matchAction4 As Action(Of TDerivedType4), matchAction5 As Action(Of TDerivedType5),
        matchAction6 As Action(Of TDerivedType6), matchAction7 As Action(Of TDerivedType7), matchAction8 As Action(Of TDerivedType8), matchAction9 As Action(Of TDerivedType9), matchAction10 As Action(Of TDerivedType10), matchAction11 As Action(Of TDerivedType11),
        matchAction12 As Action(Of TDerivedType12), matchAction13 As Action(Of TDerivedType13), matchAction14 As Action(Of TDerivedType14), matchAction15 As Action(Of TDerivedType15), matchAction16 As Action(Of TDerivedType16), matchAction17 As Action(Of TDerivedType17),
        matchAction18 As Action(Of TDerivedType18), matchAction19 As Action(Of TDerivedType19), matchAction20 As Action(Of TDerivedType20), matchAction21 As Action(Of TDerivedType21), matchAction22 As Action(Of TDerivedType22), matchAction23 As Action(Of TDerivedType23),
        matchAction24 As Action(Of TDerivedType24), matchAction25 As Action(Of TDerivedType25), Optional defaultAction As Action(Of TBaseType) = Nothing)
        If TypeOf obj Is TDerivedType1 Then
            matchAction1(DirectCast(obj, TDerivedType1))
        ElseIf TypeOf obj Is TDerivedType2 Then
            matchAction2(DirectCast(obj, TDerivedType2))
        ElseIf TypeOf obj Is TDerivedType3 Then
            matchAction3(DirectCast(obj, TDerivedType3))
        ElseIf TypeOf obj Is TDerivedType4 Then
            matchAction4(DirectCast(obj, TDerivedType4))
        ElseIf TypeOf obj Is TDerivedType5 Then
            matchAction5(DirectCast(obj, TDerivedType5))
        ElseIf TypeOf obj Is TDerivedType6 Then
            matchAction6(DirectCast(obj, TDerivedType6))
        ElseIf TypeOf obj Is TDerivedType7 Then
            matchAction7(DirectCast(obj, TDerivedType7))
        ElseIf TypeOf obj Is TDerivedType8 Then
            matchAction8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            matchAction9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            matchAction10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            matchAction11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            matchAction12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            matchAction13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            matchAction14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            matchAction15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            matchAction16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            matchAction17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            matchAction18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            matchAction19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            matchAction20(DirectCast(obj, TDerivedType20))
        ElseIf TypeOf obj Is TDerivedType21 Then
            matchAction21(DirectCast(obj, TDerivedType21))
        ElseIf TypeOf obj Is TDerivedType22 Then
            matchAction22(DirectCast(obj, TDerivedType22))
        ElseIf TypeOf obj Is TDerivedType23 Then
            matchAction23(DirectCast(obj, TDerivedType23))
        ElseIf TypeOf obj Is TDerivedType24 Then
            matchAction24(DirectCast(obj, TDerivedType24))
        ElseIf TypeOf obj Is TDerivedType25 Then
            matchAction25(DirectCast(obj, TDerivedType25))
        ElseIf defaultAction IsNot Nothing Then
            defaultAction(obj)
        End If
    End Sub

    <Extension>
    Public Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TDerivedType21 As TBaseType, TDerivedType22 As TBaseType, TDerivedType23 As TBaseType,
        TDerivedType24 As TBaseType, TDerivedType25 As TBaseType, TDerivedType26 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), matchAction2 As Action(Of TDerivedType2), matchAction3 As Action(Of TDerivedType3), matchAction4 As Action(Of TDerivedType4), matchAction5 As Action(Of TDerivedType5),
        matchAction6 As Action(Of TDerivedType6), matchAction7 As Action(Of TDerivedType7), matchAction8 As Action(Of TDerivedType8), matchAction9 As Action(Of TDerivedType9), matchAction10 As Action(Of TDerivedType10), matchAction11 As Action(Of TDerivedType11),
        matchAction12 As Action(Of TDerivedType12), matchAction13 As Action(Of TDerivedType13), matchAction14 As Action(Of TDerivedType14), matchAction15 As Action(Of TDerivedType15), matchAction16 As Action(Of TDerivedType16), matchAction17 As Action(Of TDerivedType17),
        matchAction18 As Action(Of TDerivedType18), matchAction19 As Action(Of TDerivedType19), matchAction20 As Action(Of TDerivedType20), matchAction21 As Action(Of TDerivedType21), matchAction22 As Action(Of TDerivedType22), matchAction23 As Action(Of TDerivedType23),
        matchAction24 As Action(Of TDerivedType24), matchAction25 As Action(Of TDerivedType25), matchAction26 As Action(Of TDerivedType26), Optional defaultAction As Action(Of TBaseType) = Nothing)
        If TypeOf obj Is TDerivedType1 Then
            matchAction1(DirectCast(obj, TDerivedType1))
        ElseIf TypeOf obj Is TDerivedType2 Then
            matchAction2(DirectCast(obj, TDerivedType2))
        ElseIf TypeOf obj Is TDerivedType3 Then
            matchAction3(DirectCast(obj, TDerivedType3))
        ElseIf TypeOf obj Is TDerivedType4 Then
            matchAction4(DirectCast(obj, TDerivedType4))
        ElseIf TypeOf obj Is TDerivedType5 Then
            matchAction5(DirectCast(obj, TDerivedType5))
        ElseIf TypeOf obj Is TDerivedType6 Then
            matchAction6(DirectCast(obj, TDerivedType6))
        ElseIf TypeOf obj Is TDerivedType7 Then
            matchAction7(DirectCast(obj, TDerivedType7))
        ElseIf TypeOf obj Is TDerivedType8 Then
            matchAction8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            matchAction9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            matchAction10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            matchAction11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            matchAction12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            matchAction13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            matchAction14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            matchAction15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            matchAction16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            matchAction17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            matchAction18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            matchAction19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            matchAction20(DirectCast(obj, TDerivedType20))
        ElseIf TypeOf obj Is TDerivedType21 Then
            matchAction21(DirectCast(obj, TDerivedType21))
        ElseIf TypeOf obj Is TDerivedType22 Then
            matchAction22(DirectCast(obj, TDerivedType22))
        ElseIf TypeOf obj Is TDerivedType23 Then
            matchAction23(DirectCast(obj, TDerivedType23))
        ElseIf TypeOf obj Is TDerivedType24 Then
            matchAction24(DirectCast(obj, TDerivedType24))
        ElseIf TypeOf obj Is TDerivedType25 Then
            matchAction25(DirectCast(obj, TDerivedType25))
        ElseIf TypeOf obj Is TDerivedType26 Then
            matchAction26(DirectCast(obj, TDerivedType26))
        ElseIf defaultAction IsNot Nothing Then
            defaultAction(obj)
        End If
    End Sub

    <Extension>
    Public Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TDerivedType21 As TBaseType, TDerivedType22 As TBaseType, TDerivedType23 As TBaseType,
        TDerivedType24 As TBaseType, TDerivedType25 As TBaseType, TDerivedType26 As TBaseType, TDerivedType27 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), matchAction2 As Action(Of TDerivedType2), matchAction3 As Action(Of TDerivedType3), matchAction4 As Action(Of TDerivedType4), matchAction5 As Action(Of TDerivedType5),
        matchAction6 As Action(Of TDerivedType6), matchAction7 As Action(Of TDerivedType7), matchAction8 As Action(Of TDerivedType8), matchAction9 As Action(Of TDerivedType9), matchAction10 As Action(Of TDerivedType10), matchAction11 As Action(Of TDerivedType11),
        matchAction12 As Action(Of TDerivedType12), matchAction13 As Action(Of TDerivedType13), matchAction14 As Action(Of TDerivedType14), matchAction15 As Action(Of TDerivedType15), matchAction16 As Action(Of TDerivedType16), matchAction17 As Action(Of TDerivedType17),
        matchAction18 As Action(Of TDerivedType18), matchAction19 As Action(Of TDerivedType19), matchAction20 As Action(Of TDerivedType20), matchAction21 As Action(Of TDerivedType21), matchAction22 As Action(Of TDerivedType22), matchAction23 As Action(Of TDerivedType23),
        matchAction24 As Action(Of TDerivedType24), matchAction25 As Action(Of TDerivedType25), matchAction26 As Action(Of TDerivedType26), matchAction27 As Action(Of TDerivedType27), Optional defaultAction As Action(Of TBaseType) = Nothing)
        If TypeOf obj Is TDerivedType1 Then
            matchAction1(DirectCast(obj, TDerivedType1))
        ElseIf TypeOf obj Is TDerivedType2 Then
            matchAction2(DirectCast(obj, TDerivedType2))
        ElseIf TypeOf obj Is TDerivedType3 Then
            matchAction3(DirectCast(obj, TDerivedType3))
        ElseIf TypeOf obj Is TDerivedType4 Then
            matchAction4(DirectCast(obj, TDerivedType4))
        ElseIf TypeOf obj Is TDerivedType5 Then
            matchAction5(DirectCast(obj, TDerivedType5))
        ElseIf TypeOf obj Is TDerivedType6 Then
            matchAction6(DirectCast(obj, TDerivedType6))
        ElseIf TypeOf obj Is TDerivedType7 Then
            matchAction7(DirectCast(obj, TDerivedType7))
        ElseIf TypeOf obj Is TDerivedType8 Then
            matchAction8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            matchAction9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            matchAction10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            matchAction11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            matchAction12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            matchAction13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            matchAction14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            matchAction15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            matchAction16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            matchAction17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            matchAction18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            matchAction19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            matchAction20(DirectCast(obj, TDerivedType20))
        ElseIf TypeOf obj Is TDerivedType21 Then
            matchAction21(DirectCast(obj, TDerivedType21))
        ElseIf TypeOf obj Is TDerivedType22 Then
            matchAction22(DirectCast(obj, TDerivedType22))
        ElseIf TypeOf obj Is TDerivedType23 Then
            matchAction23(DirectCast(obj, TDerivedType23))
        ElseIf TypeOf obj Is TDerivedType24 Then
            matchAction24(DirectCast(obj, TDerivedType24))
        ElseIf TypeOf obj Is TDerivedType25 Then
            matchAction25(DirectCast(obj, TDerivedType25))
        ElseIf TypeOf obj Is TDerivedType26 Then
            matchAction26(DirectCast(obj, TDerivedType26))
        ElseIf TypeOf obj Is TDerivedType27 Then
            matchAction27(DirectCast(obj, TDerivedType27))
        ElseIf defaultAction IsNot Nothing Then
            defaultAction(obj)
        End If
    End Sub

    <Extension>
    Public Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TDerivedType21 As TBaseType, TDerivedType22 As TBaseType, TDerivedType23 As TBaseType,
        TDerivedType24 As TBaseType, TDerivedType25 As TBaseType, TDerivedType26 As TBaseType, TDerivedType27 As TBaseType, TDerivedType28 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), matchAction2 As Action(Of TDerivedType2), matchAction3 As Action(Of TDerivedType3), matchAction4 As Action(Of TDerivedType4), matchAction5 As Action(Of TDerivedType5),
        matchAction6 As Action(Of TDerivedType6), matchAction7 As Action(Of TDerivedType7), matchAction8 As Action(Of TDerivedType8), matchAction9 As Action(Of TDerivedType9), matchAction10 As Action(Of TDerivedType10), matchAction11 As Action(Of TDerivedType11),
        matchAction12 As Action(Of TDerivedType12), matchAction13 As Action(Of TDerivedType13), matchAction14 As Action(Of TDerivedType14), matchAction15 As Action(Of TDerivedType15), matchAction16 As Action(Of TDerivedType16), matchAction17 As Action(Of TDerivedType17),
        matchAction18 As Action(Of TDerivedType18), matchAction19 As Action(Of TDerivedType19), matchAction20 As Action(Of TDerivedType20), matchAction21 As Action(Of TDerivedType21), matchAction22 As Action(Of TDerivedType22), matchAction23 As Action(Of TDerivedType23),
        matchAction24 As Action(Of TDerivedType24), matchAction25 As Action(Of TDerivedType25), matchAction26 As Action(Of TDerivedType26), matchAction27 As Action(Of TDerivedType27), matchAction28 As Action(Of TDerivedType28), Optional defaultAction As Action(Of TBaseType) = Nothing)
        If TypeOf obj Is TDerivedType1 Then
            matchAction1(DirectCast(obj, TDerivedType1))
        ElseIf TypeOf obj Is TDerivedType2 Then
            matchAction2(DirectCast(obj, TDerivedType2))
        ElseIf TypeOf obj Is TDerivedType3 Then
            matchAction3(DirectCast(obj, TDerivedType3))
        ElseIf TypeOf obj Is TDerivedType4 Then
            matchAction4(DirectCast(obj, TDerivedType4))
        ElseIf TypeOf obj Is TDerivedType5 Then
            matchAction5(DirectCast(obj, TDerivedType5))
        ElseIf TypeOf obj Is TDerivedType6 Then
            matchAction6(DirectCast(obj, TDerivedType6))
        ElseIf TypeOf obj Is TDerivedType7 Then
            matchAction7(DirectCast(obj, TDerivedType7))
        ElseIf TypeOf obj Is TDerivedType8 Then
            matchAction8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            matchAction9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            matchAction10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            matchAction11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            matchAction12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            matchAction13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            matchAction14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            matchAction15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            matchAction16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            matchAction17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            matchAction18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            matchAction19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            matchAction20(DirectCast(obj, TDerivedType20))
        ElseIf TypeOf obj Is TDerivedType21 Then
            matchAction21(DirectCast(obj, TDerivedType21))
        ElseIf TypeOf obj Is TDerivedType22 Then
            matchAction22(DirectCast(obj, TDerivedType22))
        ElseIf TypeOf obj Is TDerivedType23 Then
            matchAction23(DirectCast(obj, TDerivedType23))
        ElseIf TypeOf obj Is TDerivedType24 Then
            matchAction24(DirectCast(obj, TDerivedType24))
        ElseIf TypeOf obj Is TDerivedType25 Then
            matchAction25(DirectCast(obj, TDerivedType25))
        ElseIf TypeOf obj Is TDerivedType26 Then
            matchAction26(DirectCast(obj, TDerivedType26))
        ElseIf TypeOf obj Is TDerivedType27 Then
            matchAction27(DirectCast(obj, TDerivedType27))
        ElseIf TypeOf obj Is TDerivedType28 Then
            matchAction28(DirectCast(obj, TDerivedType28))
        ElseIf defaultAction IsNot Nothing Then
            defaultAction(obj)
        End If
    End Sub

    <Extension>
    Public Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TDerivedType21 As TBaseType, TDerivedType22 As TBaseType, TDerivedType23 As TBaseType,
        TDerivedType24 As TBaseType, TDerivedType25 As TBaseType, TDerivedType26 As TBaseType, TDerivedType27 As TBaseType, TDerivedType28 As TBaseType, TDerivedType29 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), matchAction2 As Action(Of TDerivedType2), matchAction3 As Action(Of TDerivedType3), matchAction4 As Action(Of TDerivedType4), matchAction5 As Action(Of TDerivedType5),
        matchAction6 As Action(Of TDerivedType6), matchAction7 As Action(Of TDerivedType7), matchAction8 As Action(Of TDerivedType8), matchAction9 As Action(Of TDerivedType9), matchAction10 As Action(Of TDerivedType10), matchAction11 As Action(Of TDerivedType11),
        matchAction12 As Action(Of TDerivedType12), matchAction13 As Action(Of TDerivedType13), matchAction14 As Action(Of TDerivedType14), matchAction15 As Action(Of TDerivedType15), matchAction16 As Action(Of TDerivedType16), matchAction17 As Action(Of TDerivedType17),
        matchAction18 As Action(Of TDerivedType18), matchAction19 As Action(Of TDerivedType19), matchAction20 As Action(Of TDerivedType20), matchAction21 As Action(Of TDerivedType21), matchAction22 As Action(Of TDerivedType22), matchAction23 As Action(Of TDerivedType23),
        matchAction24 As Action(Of TDerivedType24), matchAction25 As Action(Of TDerivedType25), matchAction26 As Action(Of TDerivedType26), matchAction27 As Action(Of TDerivedType27), matchAction28 As Action(Of TDerivedType28), matchAction29 As Action(Of TDerivedType29),
        Optional defaultAction As Action(Of TBaseType) = Nothing)
        If TypeOf obj Is TDerivedType1 Then
            matchAction1(DirectCast(obj, TDerivedType1))
        ElseIf TypeOf obj Is TDerivedType2 Then
            matchAction2(DirectCast(obj, TDerivedType2))
        ElseIf TypeOf obj Is TDerivedType3 Then
            matchAction3(DirectCast(obj, TDerivedType3))
        ElseIf TypeOf obj Is TDerivedType4 Then
            matchAction4(DirectCast(obj, TDerivedType4))
        ElseIf TypeOf obj Is TDerivedType5 Then
            matchAction5(DirectCast(obj, TDerivedType5))
        ElseIf TypeOf obj Is TDerivedType6 Then
            matchAction6(DirectCast(obj, TDerivedType6))
        ElseIf TypeOf obj Is TDerivedType7 Then
            matchAction7(DirectCast(obj, TDerivedType7))
        ElseIf TypeOf obj Is TDerivedType8 Then
            matchAction8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            matchAction9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            matchAction10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            matchAction11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            matchAction12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            matchAction13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            matchAction14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            matchAction15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            matchAction16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            matchAction17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            matchAction18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            matchAction19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            matchAction20(DirectCast(obj, TDerivedType20))
        ElseIf TypeOf obj Is TDerivedType21 Then
            matchAction21(DirectCast(obj, TDerivedType21))
        ElseIf TypeOf obj Is TDerivedType22 Then
            matchAction22(DirectCast(obj, TDerivedType22))
        ElseIf TypeOf obj Is TDerivedType23 Then
            matchAction23(DirectCast(obj, TDerivedType23))
        ElseIf TypeOf obj Is TDerivedType24 Then
            matchAction24(DirectCast(obj, TDerivedType24))
        ElseIf TypeOf obj Is TDerivedType25 Then
            matchAction25(DirectCast(obj, TDerivedType25))
        ElseIf TypeOf obj Is TDerivedType26 Then
            matchAction26(DirectCast(obj, TDerivedType26))
        ElseIf TypeOf obj Is TDerivedType27 Then
            matchAction27(DirectCast(obj, TDerivedType27))
        ElseIf TypeOf obj Is TDerivedType28 Then
            matchAction28(DirectCast(obj, TDerivedType28))
        ElseIf TypeOf obj Is TDerivedType29 Then
            matchAction29(DirectCast(obj, TDerivedType29))
        ElseIf defaultAction IsNot Nothing Then
            defaultAction(obj)
        End If
    End Sub

    <Extension>
    Public Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TDerivedType21 As TBaseType, TDerivedType22 As TBaseType, TDerivedType23 As TBaseType,
        TDerivedType24 As TBaseType, TDerivedType25 As TBaseType, TDerivedType26 As TBaseType, TDerivedType27 As TBaseType, TDerivedType28 As TBaseType, TDerivedType29 As TBaseType,
        TDerivedType30 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), matchAction2 As Action(Of TDerivedType2), matchAction3 As Action(Of TDerivedType3), matchAction4 As Action(Of TDerivedType4), matchAction5 As Action(Of TDerivedType5),
        matchAction6 As Action(Of TDerivedType6), matchAction7 As Action(Of TDerivedType7), matchAction8 As Action(Of TDerivedType8), matchAction9 As Action(Of TDerivedType9), matchAction10 As Action(Of TDerivedType10), matchAction11 As Action(Of TDerivedType11),
        matchAction12 As Action(Of TDerivedType12), matchAction13 As Action(Of TDerivedType13), matchAction14 As Action(Of TDerivedType14), matchAction15 As Action(Of TDerivedType15), matchAction16 As Action(Of TDerivedType16), matchAction17 As Action(Of TDerivedType17),
        matchAction18 As Action(Of TDerivedType18), matchAction19 As Action(Of TDerivedType19), matchAction20 As Action(Of TDerivedType20), matchAction21 As Action(Of TDerivedType21), matchAction22 As Action(Of TDerivedType22), matchAction23 As Action(Of TDerivedType23),
        matchAction24 As Action(Of TDerivedType24), matchAction25 As Action(Of TDerivedType25), matchAction26 As Action(Of TDerivedType26), matchAction27 As Action(Of TDerivedType27), matchAction28 As Action(Of TDerivedType28), matchAction29 As Action(Of TDerivedType29),
        matchAction30 As Action(Of TDerivedType30), Optional defaultAction As Action(Of TBaseType) = Nothing)
        If TypeOf obj Is TDerivedType1 Then
            matchAction1(DirectCast(obj, TDerivedType1))
        ElseIf TypeOf obj Is TDerivedType2 Then
            matchAction2(DirectCast(obj, TDerivedType2))
        ElseIf TypeOf obj Is TDerivedType3 Then
            matchAction3(DirectCast(obj, TDerivedType3))
        ElseIf TypeOf obj Is TDerivedType4 Then
            matchAction4(DirectCast(obj, TDerivedType4))
        ElseIf TypeOf obj Is TDerivedType5 Then
            matchAction5(DirectCast(obj, TDerivedType5))
        ElseIf TypeOf obj Is TDerivedType6 Then
            matchAction6(DirectCast(obj, TDerivedType6))
        ElseIf TypeOf obj Is TDerivedType7 Then
            matchAction7(DirectCast(obj, TDerivedType7))
        ElseIf TypeOf obj Is TDerivedType8 Then
            matchAction8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            matchAction9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            matchAction10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            matchAction11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            matchAction12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            matchAction13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            matchAction14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            matchAction15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            matchAction16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            matchAction17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            matchAction18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            matchAction19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            matchAction20(DirectCast(obj, TDerivedType20))
        ElseIf TypeOf obj Is TDerivedType21 Then
            matchAction21(DirectCast(obj, TDerivedType21))
        ElseIf TypeOf obj Is TDerivedType22 Then
            matchAction22(DirectCast(obj, TDerivedType22))
        ElseIf TypeOf obj Is TDerivedType23 Then
            matchAction23(DirectCast(obj, TDerivedType23))
        ElseIf TypeOf obj Is TDerivedType24 Then
            matchAction24(DirectCast(obj, TDerivedType24))
        ElseIf TypeOf obj Is TDerivedType25 Then
            matchAction25(DirectCast(obj, TDerivedType25))
        ElseIf TypeOf obj Is TDerivedType26 Then
            matchAction26(DirectCast(obj, TDerivedType26))
        ElseIf TypeOf obj Is TDerivedType27 Then
            matchAction27(DirectCast(obj, TDerivedType27))
        ElseIf TypeOf obj Is TDerivedType28 Then
            matchAction28(DirectCast(obj, TDerivedType28))
        ElseIf TypeOf obj Is TDerivedType29 Then
            matchAction29(DirectCast(obj, TDerivedType29))
        ElseIf TypeOf obj Is TDerivedType30 Then
            matchAction30(DirectCast(obj, TDerivedType30))
        ElseIf defaultAction IsNot Nothing Then
            defaultAction(obj)
        End If
    End Sub

    <Extension>
    Public Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TDerivedType21 As TBaseType, TDerivedType22 As TBaseType, TDerivedType23 As TBaseType,
        TDerivedType24 As TBaseType, TDerivedType25 As TBaseType, TDerivedType26 As TBaseType, TDerivedType27 As TBaseType, TDerivedType28 As TBaseType, TDerivedType29 As TBaseType,
        TDerivedType30 As TBaseType, TDerivedType31 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), matchAction2 As Action(Of TDerivedType2), matchAction3 As Action(Of TDerivedType3), matchAction4 As Action(Of TDerivedType4), matchAction5 As Action(Of TDerivedType5),
        matchAction6 As Action(Of TDerivedType6), matchAction7 As Action(Of TDerivedType7), matchAction8 As Action(Of TDerivedType8), matchAction9 As Action(Of TDerivedType9), matchAction10 As Action(Of TDerivedType10), matchAction11 As Action(Of TDerivedType11),
        matchAction12 As Action(Of TDerivedType12), matchAction13 As Action(Of TDerivedType13), matchAction14 As Action(Of TDerivedType14), matchAction15 As Action(Of TDerivedType15), matchAction16 As Action(Of TDerivedType16), matchAction17 As Action(Of TDerivedType17),
        matchAction18 As Action(Of TDerivedType18), matchAction19 As Action(Of TDerivedType19), matchAction20 As Action(Of TDerivedType20), matchAction21 As Action(Of TDerivedType21), matchAction22 As Action(Of TDerivedType22), matchAction23 As Action(Of TDerivedType23),
        matchAction24 As Action(Of TDerivedType24), matchAction25 As Action(Of TDerivedType25), matchAction26 As Action(Of TDerivedType26), matchAction27 As Action(Of TDerivedType27), matchAction28 As Action(Of TDerivedType28), matchAction29 As Action(Of TDerivedType29),
        matchAction30 As Action(Of TDerivedType30), matchAction31 As Action(Of TDerivedType31), Optional defaultAction As Action(Of TBaseType) = Nothing)
        If TypeOf obj Is TDerivedType1 Then
            matchAction1(DirectCast(obj, TDerivedType1))
        ElseIf TypeOf obj Is TDerivedType2 Then
            matchAction2(DirectCast(obj, TDerivedType2))
        ElseIf TypeOf obj Is TDerivedType3 Then
            matchAction3(DirectCast(obj, TDerivedType3))
        ElseIf TypeOf obj Is TDerivedType4 Then
            matchAction4(DirectCast(obj, TDerivedType4))
        ElseIf TypeOf obj Is TDerivedType5 Then
            matchAction5(DirectCast(obj, TDerivedType5))
        ElseIf TypeOf obj Is TDerivedType6 Then
            matchAction6(DirectCast(obj, TDerivedType6))
        ElseIf TypeOf obj Is TDerivedType7 Then
            matchAction7(DirectCast(obj, TDerivedType7))
        ElseIf TypeOf obj Is TDerivedType8 Then
            matchAction8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            matchAction9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            matchAction10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            matchAction11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            matchAction12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            matchAction13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            matchAction14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            matchAction15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            matchAction16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            matchAction17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            matchAction18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            matchAction19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            matchAction20(DirectCast(obj, TDerivedType20))
        ElseIf TypeOf obj Is TDerivedType21 Then
            matchAction21(DirectCast(obj, TDerivedType21))
        ElseIf TypeOf obj Is TDerivedType22 Then
            matchAction22(DirectCast(obj, TDerivedType22))
        ElseIf TypeOf obj Is TDerivedType23 Then
            matchAction23(DirectCast(obj, TDerivedType23))
        ElseIf TypeOf obj Is TDerivedType24 Then
            matchAction24(DirectCast(obj, TDerivedType24))
        ElseIf TypeOf obj Is TDerivedType25 Then
            matchAction25(DirectCast(obj, TDerivedType25))
        ElseIf TypeOf obj Is TDerivedType26 Then
            matchAction26(DirectCast(obj, TDerivedType26))
        ElseIf TypeOf obj Is TDerivedType27 Then
            matchAction27(DirectCast(obj, TDerivedType27))
        ElseIf TypeOf obj Is TDerivedType28 Then
            matchAction28(DirectCast(obj, TDerivedType28))
        ElseIf TypeOf obj Is TDerivedType29 Then
            matchAction29(DirectCast(obj, TDerivedType29))
        ElseIf TypeOf obj Is TDerivedType30 Then
            matchAction30(DirectCast(obj, TDerivedType30))
        ElseIf TypeOf obj Is TDerivedType31 Then
            matchAction31(DirectCast(obj, TDerivedType31))
        ElseIf defaultAction IsNot Nothing Then
            defaultAction(obj)
        End If
    End Sub

    <Extension>
    Public Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TDerivedType21 As TBaseType, TDerivedType22 As TBaseType, TDerivedType23 As TBaseType,
        TDerivedType24 As TBaseType, TDerivedType25 As TBaseType, TDerivedType26 As TBaseType, TDerivedType27 As TBaseType, TDerivedType28 As TBaseType, TDerivedType29 As TBaseType,
        TDerivedType30 As TBaseType, TDerivedType31 As TBaseType, TDerivedType32 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), matchAction2 As Action(Of TDerivedType2), matchAction3 As Action(Of TDerivedType3), matchAction4 As Action(Of TDerivedType4), matchAction5 As Action(Of TDerivedType5),
        matchAction6 As Action(Of TDerivedType6), matchAction7 As Action(Of TDerivedType7), matchAction8 As Action(Of TDerivedType8), matchAction9 As Action(Of TDerivedType9), matchAction10 As Action(Of TDerivedType10), matchAction11 As Action(Of TDerivedType11),
        matchAction12 As Action(Of TDerivedType12), matchAction13 As Action(Of TDerivedType13), matchAction14 As Action(Of TDerivedType14), matchAction15 As Action(Of TDerivedType15), matchAction16 As Action(Of TDerivedType16), matchAction17 As Action(Of TDerivedType17),
        matchAction18 As Action(Of TDerivedType18), matchAction19 As Action(Of TDerivedType19), matchAction20 As Action(Of TDerivedType20), matchAction21 As Action(Of TDerivedType21), matchAction22 As Action(Of TDerivedType22), matchAction23 As Action(Of TDerivedType23),
        matchAction24 As Action(Of TDerivedType24), matchAction25 As Action(Of TDerivedType25), matchAction26 As Action(Of TDerivedType26), matchAction27 As Action(Of TDerivedType27), matchAction28 As Action(Of TDerivedType28), matchAction29 As Action(Of TDerivedType29),
        matchAction30 As Action(Of TDerivedType30), matchAction31 As Action(Of TDerivedType31), matchAction32 As Action(Of TDerivedType32), Optional defaultAction As Action(Of TBaseType) = Nothing)
        If TypeOf obj Is TDerivedType1 Then
            matchAction1(DirectCast(obj, TDerivedType1))
        ElseIf TypeOf obj Is TDerivedType2 Then
            matchAction2(DirectCast(obj, TDerivedType2))
        ElseIf TypeOf obj Is TDerivedType3 Then
            matchAction3(DirectCast(obj, TDerivedType3))
        ElseIf TypeOf obj Is TDerivedType4 Then
            matchAction4(DirectCast(obj, TDerivedType4))
        ElseIf TypeOf obj Is TDerivedType5 Then
            matchAction5(DirectCast(obj, TDerivedType5))
        ElseIf TypeOf obj Is TDerivedType6 Then
            matchAction6(DirectCast(obj, TDerivedType6))
        ElseIf TypeOf obj Is TDerivedType7 Then
            matchAction7(DirectCast(obj, TDerivedType7))
        ElseIf TypeOf obj Is TDerivedType8 Then
            matchAction8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            matchAction9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            matchAction10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            matchAction11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            matchAction12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            matchAction13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            matchAction14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            matchAction15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            matchAction16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            matchAction17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            matchAction18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            matchAction19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            matchAction20(DirectCast(obj, TDerivedType20))
        ElseIf TypeOf obj Is TDerivedType21 Then
            matchAction21(DirectCast(obj, TDerivedType21))
        ElseIf TypeOf obj Is TDerivedType22 Then
            matchAction22(DirectCast(obj, TDerivedType22))
        ElseIf TypeOf obj Is TDerivedType23 Then
            matchAction23(DirectCast(obj, TDerivedType23))
        ElseIf TypeOf obj Is TDerivedType24 Then
            matchAction24(DirectCast(obj, TDerivedType24))
        ElseIf TypeOf obj Is TDerivedType25 Then
            matchAction25(DirectCast(obj, TDerivedType25))
        ElseIf TypeOf obj Is TDerivedType26 Then
            matchAction26(DirectCast(obj, TDerivedType26))
        ElseIf TypeOf obj Is TDerivedType27 Then
            matchAction27(DirectCast(obj, TDerivedType27))
        ElseIf TypeOf obj Is TDerivedType28 Then
            matchAction28(DirectCast(obj, TDerivedType28))
        ElseIf TypeOf obj Is TDerivedType29 Then
            matchAction29(DirectCast(obj, TDerivedType29))
        ElseIf TypeOf obj Is TDerivedType30 Then
            matchAction30(DirectCast(obj, TDerivedType30))
        ElseIf TypeOf obj Is TDerivedType31 Then
            matchAction31(DirectCast(obj, TDerivedType31))
        ElseIf TypeOf obj Is TDerivedType32 Then
            matchAction32(DirectCast(obj, TDerivedType32))
        ElseIf defaultAction IsNot Nothing Then
            defaultAction(obj)
        End If
    End Sub

    <Extension>
    Public Sub TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TDerivedType21 As TBaseType, TDerivedType22 As TBaseType, TDerivedType23 As TBaseType,
        TDerivedType24 As TBaseType, TDerivedType25 As TBaseType, TDerivedType26 As TBaseType, TDerivedType27 As TBaseType, TDerivedType28 As TBaseType, TDerivedType29 As TBaseType,
        TDerivedType30 As TBaseType, TDerivedType31 As TBaseType, TDerivedType32 As TBaseType, TDerivedType33 As TBaseType)(obj As TBaseType, matchAction1 As Action(Of TDerivedType1), matchAction2 As Action(Of TDerivedType2), matchAction3 As Action(Of TDerivedType3), matchAction4 As Action(Of TDerivedType4), matchAction5 As Action(Of TDerivedType5),
        matchAction6 As Action(Of TDerivedType6), matchAction7 As Action(Of TDerivedType7), matchAction8 As Action(Of TDerivedType8), matchAction9 As Action(Of TDerivedType9), matchAction10 As Action(Of TDerivedType10), matchAction11 As Action(Of TDerivedType11),
        matchAction12 As Action(Of TDerivedType12), matchAction13 As Action(Of TDerivedType13), matchAction14 As Action(Of TDerivedType14), matchAction15 As Action(Of TDerivedType15), matchAction16 As Action(Of TDerivedType16), matchAction17 As Action(Of TDerivedType17),
        matchAction18 As Action(Of TDerivedType18), matchAction19 As Action(Of TDerivedType19), matchAction20 As Action(Of TDerivedType20), matchAction21 As Action(Of TDerivedType21), matchAction22 As Action(Of TDerivedType22), matchAction23 As Action(Of TDerivedType23),
        matchAction24 As Action(Of TDerivedType24), matchAction25 As Action(Of TDerivedType25), matchAction26 As Action(Of TDerivedType26), matchAction27 As Action(Of TDerivedType27), matchAction28 As Action(Of TDerivedType28), matchAction29 As Action(Of TDerivedType29),
        matchAction30 As Action(Of TDerivedType30), matchAction31 As Action(Of TDerivedType31), matchAction32 As Action(Of TDerivedType32), matchAction33 As Action(Of TDerivedType33), Optional defaultAction As Action(Of TBaseType) = Nothing)
        If TypeOf obj Is TDerivedType1 Then
            matchAction1(DirectCast(obj, TDerivedType1))
        ElseIf TypeOf obj Is TDerivedType2 Then
            matchAction2(DirectCast(obj, TDerivedType2))
        ElseIf TypeOf obj Is TDerivedType3 Then
            matchAction3(DirectCast(obj, TDerivedType3))
        ElseIf TypeOf obj Is TDerivedType4 Then
            matchAction4(DirectCast(obj, TDerivedType4))
        ElseIf TypeOf obj Is TDerivedType5 Then
            matchAction5(DirectCast(obj, TDerivedType5))
        ElseIf TypeOf obj Is TDerivedType6 Then
            matchAction6(DirectCast(obj, TDerivedType6))
        ElseIf TypeOf obj Is TDerivedType7 Then
            matchAction7(DirectCast(obj, TDerivedType7))
        ElseIf TypeOf obj Is TDerivedType8 Then
            matchAction8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            matchAction9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            matchAction10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            matchAction11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            matchAction12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            matchAction13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            matchAction14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            matchAction15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            matchAction16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            matchAction17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            matchAction18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            matchAction19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            matchAction20(DirectCast(obj, TDerivedType20))
        ElseIf TypeOf obj Is TDerivedType21 Then
            matchAction21(DirectCast(obj, TDerivedType21))
        ElseIf TypeOf obj Is TDerivedType22 Then
            matchAction22(DirectCast(obj, TDerivedType22))
        ElseIf TypeOf obj Is TDerivedType23 Then
            matchAction23(DirectCast(obj, TDerivedType23))
        ElseIf TypeOf obj Is TDerivedType24 Then
            matchAction24(DirectCast(obj, TDerivedType24))
        ElseIf TypeOf obj Is TDerivedType25 Then
            matchAction25(DirectCast(obj, TDerivedType25))
        ElseIf TypeOf obj Is TDerivedType26 Then
            matchAction26(DirectCast(obj, TDerivedType26))
        ElseIf TypeOf obj Is TDerivedType27 Then
            matchAction27(DirectCast(obj, TDerivedType27))
        ElseIf TypeOf obj Is TDerivedType28 Then
            matchAction28(DirectCast(obj, TDerivedType28))
        ElseIf TypeOf obj Is TDerivedType29 Then
            matchAction29(DirectCast(obj, TDerivedType29))
        ElseIf TypeOf obj Is TDerivedType30 Then
            matchAction30(DirectCast(obj, TDerivedType30))
        ElseIf TypeOf obj Is TDerivedType31 Then
            matchAction31(DirectCast(obj, TDerivedType31))
        ElseIf TypeOf obj Is TDerivedType32 Then
            matchAction32(DirectCast(obj, TDerivedType32))
        ElseIf TypeOf obj Is TDerivedType33 Then
            matchAction33(DirectCast(obj, TDerivedType33))
        ElseIf defaultAction IsNot Nothing Then
            defaultAction(obj)
        End If
    End Sub

#End Region

#Region "TypeSwitch on Func<T>"

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
        If TypeOf obj Is TDerivedType1 Then
            Return matchFunc1(DirectCast(obj, TDerivedType1))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
        If TypeOf obj Is TDerivedType1 Then
            Return matchFunc1(DirectCast(obj, TDerivedType1))
        ElseIf TypeOf obj Is TDerivedType2 Then
            Return matchFunc2(DirectCast(obj, TDerivedType2))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
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
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
        If TypeOf obj Is TDerivedType1 Then
            Return matchFunc1(DirectCast(obj, TDerivedType1))
        ElseIf TypeOf obj Is TDerivedType2 Then
            Return matchFunc2(DirectCast(obj, TDerivedType2))
        ElseIf TypeOf obj Is TDerivedType3 Then
            Return matchFunc3(DirectCast(obj, TDerivedType3))
        ElseIf TypeOf obj Is TDerivedType4 Then
            Return matchFunc4(DirectCast(obj, TDerivedType4))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
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
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
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
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
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

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), matchFunc8 As Func(Of TDerivedType8, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
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
        ElseIf TypeOf obj Is TDerivedType8 Then
            Return matchFunc8(DirectCast(obj, TDerivedType8))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), matchFunc8 As Func(Of TDerivedType8, TResult), matchFunc9 As Func(Of TDerivedType9, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
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
        ElseIf TypeOf obj Is TDerivedType8 Then
            Return matchFunc8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            Return matchFunc9(DirectCast(obj, TDerivedType9))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), matchFunc8 As Func(Of TDerivedType8, TResult), matchFunc9 As Func(Of TDerivedType9, TResult), matchFunc10 As Func(Of TDerivedType10, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
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
        ElseIf TypeOf obj Is TDerivedType8 Then
            Return matchFunc8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            Return matchFunc9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            Return matchFunc10(DirectCast(obj, TDerivedType10))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), matchFunc8 As Func(Of TDerivedType8, TResult), matchFunc9 As Func(Of TDerivedType9, TResult), matchFunc10 As Func(Of TDerivedType10, TResult), matchFunc11 As Func(Of TDerivedType11, TResult),
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
        ElseIf TypeOf obj Is TDerivedType6 Then
            Return matchFunc6(DirectCast(obj, TDerivedType6))
        ElseIf TypeOf obj Is TDerivedType7 Then
            Return matchFunc7(DirectCast(obj, TDerivedType7))
        ElseIf TypeOf obj Is TDerivedType8 Then
            Return matchFunc8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            Return matchFunc9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            Return matchFunc10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            Return matchFunc11(DirectCast(obj, TDerivedType11))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), matchFunc8 As Func(Of TDerivedType8, TResult), matchFunc9 As Func(Of TDerivedType9, TResult), matchFunc10 As Func(Of TDerivedType10, TResult), matchFunc11 As Func(Of TDerivedType11, TResult),
        matchFunc12 As Func(Of TDerivedType12, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
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
        ElseIf TypeOf obj Is TDerivedType8 Then
            Return matchFunc8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            Return matchFunc9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            Return matchFunc10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            Return matchFunc11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            Return matchFunc12(DirectCast(obj, TDerivedType12))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), matchFunc8 As Func(Of TDerivedType8, TResult), matchFunc9 As Func(Of TDerivedType9, TResult), matchFunc10 As Func(Of TDerivedType10, TResult), matchFunc11 As Func(Of TDerivedType11, TResult),
        matchFunc12 As Func(Of TDerivedType12, TResult), matchFunc13 As Func(Of TDerivedType13, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
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
        ElseIf TypeOf obj Is TDerivedType8 Then
            Return matchFunc8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            Return matchFunc9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            Return matchFunc10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            Return matchFunc11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            Return matchFunc12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            Return matchFunc13(DirectCast(obj, TDerivedType13))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), matchFunc8 As Func(Of TDerivedType8, TResult), matchFunc9 As Func(Of TDerivedType9, TResult), matchFunc10 As Func(Of TDerivedType10, TResult), matchFunc11 As Func(Of TDerivedType11, TResult),
        matchFunc12 As Func(Of TDerivedType12, TResult), matchFunc13 As Func(Of TDerivedType13, TResult), matchFunc14 As Func(Of TDerivedType14, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
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
        ElseIf TypeOf obj Is TDerivedType8 Then
            Return matchFunc8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            Return matchFunc9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            Return matchFunc10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            Return matchFunc11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            Return matchFunc12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            Return matchFunc13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            Return matchFunc14(DirectCast(obj, TDerivedType14))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), matchFunc8 As Func(Of TDerivedType8, TResult), matchFunc9 As Func(Of TDerivedType9, TResult), matchFunc10 As Func(Of TDerivedType10, TResult), matchFunc11 As Func(Of TDerivedType11, TResult),
        matchFunc12 As Func(Of TDerivedType12, TResult), matchFunc13 As Func(Of TDerivedType13, TResult), matchFunc14 As Func(Of TDerivedType14, TResult), matchFunc15 As Func(Of TDerivedType15, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
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
        ElseIf TypeOf obj Is TDerivedType8 Then
            Return matchFunc8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            Return matchFunc9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            Return matchFunc10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            Return matchFunc11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            Return matchFunc12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            Return matchFunc13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            Return matchFunc14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            Return matchFunc15(DirectCast(obj, TDerivedType15))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), matchFunc8 As Func(Of TDerivedType8, TResult), matchFunc9 As Func(Of TDerivedType9, TResult), matchFunc10 As Func(Of TDerivedType10, TResult), matchFunc11 As Func(Of TDerivedType11, TResult),
        matchFunc12 As Func(Of TDerivedType12, TResult), matchFunc13 As Func(Of TDerivedType13, TResult), matchFunc14 As Func(Of TDerivedType14, TResult), matchFunc15 As Func(Of TDerivedType15, TResult), matchFunc16 As Func(Of TDerivedType16, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
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
        ElseIf TypeOf obj Is TDerivedType8 Then
            Return matchFunc8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            Return matchFunc9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            Return matchFunc10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            Return matchFunc11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            Return matchFunc12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            Return matchFunc13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            Return matchFunc14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            Return matchFunc15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            Return matchFunc16(DirectCast(obj, TDerivedType16))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), matchFunc8 As Func(Of TDerivedType8, TResult), matchFunc9 As Func(Of TDerivedType9, TResult), matchFunc10 As Func(Of TDerivedType10, TResult), matchFunc11 As Func(Of TDerivedType11, TResult),
        matchFunc12 As Func(Of TDerivedType12, TResult), matchFunc13 As Func(Of TDerivedType13, TResult), matchFunc14 As Func(Of TDerivedType14, TResult), matchFunc15 As Func(Of TDerivedType15, TResult), matchFunc16 As Func(Of TDerivedType16, TResult), matchFunc17 As Func(Of TDerivedType17, TResult),
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
        ElseIf TypeOf obj Is TDerivedType6 Then
            Return matchFunc6(DirectCast(obj, TDerivedType6))
        ElseIf TypeOf obj Is TDerivedType7 Then
            Return matchFunc7(DirectCast(obj, TDerivedType7))
        ElseIf TypeOf obj Is TDerivedType8 Then
            Return matchFunc8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            Return matchFunc9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            Return matchFunc10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            Return matchFunc11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            Return matchFunc12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            Return matchFunc13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            Return matchFunc14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            Return matchFunc15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            Return matchFunc16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            Return matchFunc17(DirectCast(obj, TDerivedType17))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), matchFunc8 As Func(Of TDerivedType8, TResult), matchFunc9 As Func(Of TDerivedType9, TResult), matchFunc10 As Func(Of TDerivedType10, TResult), matchFunc11 As Func(Of TDerivedType11, TResult),
        matchFunc12 As Func(Of TDerivedType12, TResult), matchFunc13 As Func(Of TDerivedType13, TResult), matchFunc14 As Func(Of TDerivedType14, TResult), matchFunc15 As Func(Of TDerivedType15, TResult), matchFunc16 As Func(Of TDerivedType16, TResult), matchFunc17 As Func(Of TDerivedType17, TResult),
        matchFunc18 As Func(Of TDerivedType18, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
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
        ElseIf TypeOf obj Is TDerivedType8 Then
            Return matchFunc8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            Return matchFunc9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            Return matchFunc10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            Return matchFunc11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            Return matchFunc12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            Return matchFunc13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            Return matchFunc14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            Return matchFunc15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            Return matchFunc16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            Return matchFunc17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            Return matchFunc18(DirectCast(obj, TDerivedType18))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), matchFunc8 As Func(Of TDerivedType8, TResult), matchFunc9 As Func(Of TDerivedType9, TResult), matchFunc10 As Func(Of TDerivedType10, TResult), matchFunc11 As Func(Of TDerivedType11, TResult),
        matchFunc12 As Func(Of TDerivedType12, TResult), matchFunc13 As Func(Of TDerivedType13, TResult), matchFunc14 As Func(Of TDerivedType14, TResult), matchFunc15 As Func(Of TDerivedType15, TResult), matchFunc16 As Func(Of TDerivedType16, TResult), matchFunc17 As Func(Of TDerivedType17, TResult),
        matchFunc18 As Func(Of TDerivedType18, TResult), matchFunc19 As Func(Of TDerivedType19, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
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
        ElseIf TypeOf obj Is TDerivedType8 Then
            Return matchFunc8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            Return matchFunc9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            Return matchFunc10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            Return matchFunc11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            Return matchFunc12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            Return matchFunc13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            Return matchFunc14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            Return matchFunc15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            Return matchFunc16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            Return matchFunc17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            Return matchFunc18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            Return matchFunc19(DirectCast(obj, TDerivedType19))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), matchFunc8 As Func(Of TDerivedType8, TResult), matchFunc9 As Func(Of TDerivedType9, TResult), matchFunc10 As Func(Of TDerivedType10, TResult), matchFunc11 As Func(Of TDerivedType11, TResult),
        matchFunc12 As Func(Of TDerivedType12, TResult), matchFunc13 As Func(Of TDerivedType13, TResult), matchFunc14 As Func(Of TDerivedType14, TResult), matchFunc15 As Func(Of TDerivedType15, TResult), matchFunc16 As Func(Of TDerivedType16, TResult), matchFunc17 As Func(Of TDerivedType17, TResult),
        matchFunc18 As Func(Of TDerivedType18, TResult), matchFunc19 As Func(Of TDerivedType19, TResult), matchFunc20 As Func(Of TDerivedType20, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
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
        ElseIf TypeOf obj Is TDerivedType8 Then
            Return matchFunc8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            Return matchFunc9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            Return matchFunc10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            Return matchFunc11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            Return matchFunc12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            Return matchFunc13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            Return matchFunc14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            Return matchFunc15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            Return matchFunc16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            Return matchFunc17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            Return matchFunc18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            Return matchFunc19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            Return matchFunc20(DirectCast(obj, TDerivedType20))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TDerivedType21 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), matchFunc8 As Func(Of TDerivedType8, TResult), matchFunc9 As Func(Of TDerivedType9, TResult), matchFunc10 As Func(Of TDerivedType10, TResult), matchFunc11 As Func(Of TDerivedType11, TResult),
        matchFunc12 As Func(Of TDerivedType12, TResult), matchFunc13 As Func(Of TDerivedType13, TResult), matchFunc14 As Func(Of TDerivedType14, TResult), matchFunc15 As Func(Of TDerivedType15, TResult), matchFunc16 As Func(Of TDerivedType16, TResult), matchFunc17 As Func(Of TDerivedType17, TResult),
        matchFunc18 As Func(Of TDerivedType18, TResult), matchFunc19 As Func(Of TDerivedType19, TResult), matchFunc20 As Func(Of TDerivedType20, TResult), matchFunc21 As Func(Of TDerivedType21, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
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
        ElseIf TypeOf obj Is TDerivedType8 Then
            Return matchFunc8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            Return matchFunc9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            Return matchFunc10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            Return matchFunc11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            Return matchFunc12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            Return matchFunc13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            Return matchFunc14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            Return matchFunc15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            Return matchFunc16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            Return matchFunc17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            Return matchFunc18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            Return matchFunc19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            Return matchFunc20(DirectCast(obj, TDerivedType20))
        ElseIf TypeOf obj Is TDerivedType21 Then
            Return matchFunc21(DirectCast(obj, TDerivedType21))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TDerivedType21 As TBaseType, TDerivedType22 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), matchFunc8 As Func(Of TDerivedType8, TResult), matchFunc9 As Func(Of TDerivedType9, TResult), matchFunc10 As Func(Of TDerivedType10, TResult), matchFunc11 As Func(Of TDerivedType11, TResult),
        matchFunc12 As Func(Of TDerivedType12, TResult), matchFunc13 As Func(Of TDerivedType13, TResult), matchFunc14 As Func(Of TDerivedType14, TResult), matchFunc15 As Func(Of TDerivedType15, TResult), matchFunc16 As Func(Of TDerivedType16, TResult), matchFunc17 As Func(Of TDerivedType17, TResult),
        matchFunc18 As Func(Of TDerivedType18, TResult), matchFunc19 As Func(Of TDerivedType19, TResult), matchFunc20 As Func(Of TDerivedType20, TResult), matchFunc21 As Func(Of TDerivedType21, TResult), matchFunc22 As Func(Of TDerivedType22, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
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
        ElseIf TypeOf obj Is TDerivedType8 Then
            Return matchFunc8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            Return matchFunc9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            Return matchFunc10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            Return matchFunc11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            Return matchFunc12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            Return matchFunc13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            Return matchFunc14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            Return matchFunc15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            Return matchFunc16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            Return matchFunc17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            Return matchFunc18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            Return matchFunc19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            Return matchFunc20(DirectCast(obj, TDerivedType20))
        ElseIf TypeOf obj Is TDerivedType21 Then
            Return matchFunc21(DirectCast(obj, TDerivedType21))
        ElseIf TypeOf obj Is TDerivedType22 Then
            Return matchFunc22(DirectCast(obj, TDerivedType22))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TDerivedType21 As TBaseType, TDerivedType22 As TBaseType, TDerivedType23 As TBaseType,
        TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), matchFunc8 As Func(Of TDerivedType8, TResult), matchFunc9 As Func(Of TDerivedType9, TResult), matchFunc10 As Func(Of TDerivedType10, TResult), matchFunc11 As Func(Of TDerivedType11, TResult),
        matchFunc12 As Func(Of TDerivedType12, TResult), matchFunc13 As Func(Of TDerivedType13, TResult), matchFunc14 As Func(Of TDerivedType14, TResult), matchFunc15 As Func(Of TDerivedType15, TResult), matchFunc16 As Func(Of TDerivedType16, TResult), matchFunc17 As Func(Of TDerivedType17, TResult),
        matchFunc18 As Func(Of TDerivedType18, TResult), matchFunc19 As Func(Of TDerivedType19, TResult), matchFunc20 As Func(Of TDerivedType20, TResult), matchFunc21 As Func(Of TDerivedType21, TResult), matchFunc22 As Func(Of TDerivedType22, TResult), matchFunc23 As Func(Of TDerivedType23, TResult),
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
        ElseIf TypeOf obj Is TDerivedType6 Then
            Return matchFunc6(DirectCast(obj, TDerivedType6))
        ElseIf TypeOf obj Is TDerivedType7 Then
            Return matchFunc7(DirectCast(obj, TDerivedType7))
        ElseIf TypeOf obj Is TDerivedType8 Then
            Return matchFunc8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            Return matchFunc9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            Return matchFunc10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            Return matchFunc11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            Return matchFunc12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            Return matchFunc13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            Return matchFunc14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            Return matchFunc15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            Return matchFunc16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            Return matchFunc17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            Return matchFunc18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            Return matchFunc19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            Return matchFunc20(DirectCast(obj, TDerivedType20))
        ElseIf TypeOf obj Is TDerivedType21 Then
            Return matchFunc21(DirectCast(obj, TDerivedType21))
        ElseIf TypeOf obj Is TDerivedType22 Then
            Return matchFunc22(DirectCast(obj, TDerivedType22))
        ElseIf TypeOf obj Is TDerivedType23 Then
            Return matchFunc23(DirectCast(obj, TDerivedType23))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TDerivedType21 As TBaseType, TDerivedType22 As TBaseType, TDerivedType23 As TBaseType,
        TDerivedType24 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), matchFunc8 As Func(Of TDerivedType8, TResult), matchFunc9 As Func(Of TDerivedType9, TResult), matchFunc10 As Func(Of TDerivedType10, TResult), matchFunc11 As Func(Of TDerivedType11, TResult),
        matchFunc12 As Func(Of TDerivedType12, TResult), matchFunc13 As Func(Of TDerivedType13, TResult), matchFunc14 As Func(Of TDerivedType14, TResult), matchFunc15 As Func(Of TDerivedType15, TResult), matchFunc16 As Func(Of TDerivedType16, TResult), matchFunc17 As Func(Of TDerivedType17, TResult),
        matchFunc18 As Func(Of TDerivedType18, TResult), matchFunc19 As Func(Of TDerivedType19, TResult), matchFunc20 As Func(Of TDerivedType20, TResult), matchFunc21 As Func(Of TDerivedType21, TResult), matchFunc22 As Func(Of TDerivedType22, TResult), matchFunc23 As Func(Of TDerivedType23, TResult),
        matchFunc24 As Func(Of TDerivedType24, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
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
        ElseIf TypeOf obj Is TDerivedType8 Then
            Return matchFunc8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            Return matchFunc9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            Return matchFunc10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            Return matchFunc11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            Return matchFunc12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            Return matchFunc13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            Return matchFunc14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            Return matchFunc15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            Return matchFunc16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            Return matchFunc17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            Return matchFunc18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            Return matchFunc19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            Return matchFunc20(DirectCast(obj, TDerivedType20))
        ElseIf TypeOf obj Is TDerivedType21 Then
            Return matchFunc21(DirectCast(obj, TDerivedType21))
        ElseIf TypeOf obj Is TDerivedType22 Then
            Return matchFunc22(DirectCast(obj, TDerivedType22))
        ElseIf TypeOf obj Is TDerivedType23 Then
            Return matchFunc23(DirectCast(obj, TDerivedType23))
        ElseIf TypeOf obj Is TDerivedType24 Then
            Return matchFunc24(DirectCast(obj, TDerivedType24))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TDerivedType21 As TBaseType, TDerivedType22 As TBaseType, TDerivedType23 As TBaseType,
        TDerivedType24 As TBaseType, TDerivedType25 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), matchFunc8 As Func(Of TDerivedType8, TResult), matchFunc9 As Func(Of TDerivedType9, TResult), matchFunc10 As Func(Of TDerivedType10, TResult), matchFunc11 As Func(Of TDerivedType11, TResult),
        matchFunc12 As Func(Of TDerivedType12, TResult), matchFunc13 As Func(Of TDerivedType13, TResult), matchFunc14 As Func(Of TDerivedType14, TResult), matchFunc15 As Func(Of TDerivedType15, TResult), matchFunc16 As Func(Of TDerivedType16, TResult), matchFunc17 As Func(Of TDerivedType17, TResult),
        matchFunc18 As Func(Of TDerivedType18, TResult), matchFunc19 As Func(Of TDerivedType19, TResult), matchFunc20 As Func(Of TDerivedType20, TResult), matchFunc21 As Func(Of TDerivedType21, TResult), matchFunc22 As Func(Of TDerivedType22, TResult), matchFunc23 As Func(Of TDerivedType23, TResult),
        matchFunc24 As Func(Of TDerivedType24, TResult), matchFunc25 As Func(Of TDerivedType25, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
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
        ElseIf TypeOf obj Is TDerivedType8 Then
            Return matchFunc8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            Return matchFunc9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            Return matchFunc10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            Return matchFunc11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            Return matchFunc12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            Return matchFunc13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            Return matchFunc14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            Return matchFunc15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            Return matchFunc16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            Return matchFunc17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            Return matchFunc18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            Return matchFunc19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            Return matchFunc20(DirectCast(obj, TDerivedType20))
        ElseIf TypeOf obj Is TDerivedType21 Then
            Return matchFunc21(DirectCast(obj, TDerivedType21))
        ElseIf TypeOf obj Is TDerivedType22 Then
            Return matchFunc22(DirectCast(obj, TDerivedType22))
        ElseIf TypeOf obj Is TDerivedType23 Then
            Return matchFunc23(DirectCast(obj, TDerivedType23))
        ElseIf TypeOf obj Is TDerivedType24 Then
            Return matchFunc24(DirectCast(obj, TDerivedType24))
        ElseIf TypeOf obj Is TDerivedType25 Then
            Return matchFunc25(DirectCast(obj, TDerivedType25))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TDerivedType21 As TBaseType, TDerivedType22 As TBaseType, TDerivedType23 As TBaseType,
        TDerivedType24 As TBaseType, TDerivedType25 As TBaseType, TDerivedType26 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), matchFunc8 As Func(Of TDerivedType8, TResult), matchFunc9 As Func(Of TDerivedType9, TResult), matchFunc10 As Func(Of TDerivedType10, TResult), matchFunc11 As Func(Of TDerivedType11, TResult),
        matchFunc12 As Func(Of TDerivedType12, TResult), matchFunc13 As Func(Of TDerivedType13, TResult), matchFunc14 As Func(Of TDerivedType14, TResult), matchFunc15 As Func(Of TDerivedType15, TResult), matchFunc16 As Func(Of TDerivedType16, TResult), matchFunc17 As Func(Of TDerivedType17, TResult),
        matchFunc18 As Func(Of TDerivedType18, TResult), matchFunc19 As Func(Of TDerivedType19, TResult), matchFunc20 As Func(Of TDerivedType20, TResult), matchFunc21 As Func(Of TDerivedType21, TResult), matchFunc22 As Func(Of TDerivedType22, TResult), matchFunc23 As Func(Of TDerivedType23, TResult),
        matchFunc24 As Func(Of TDerivedType24, TResult), matchFunc25 As Func(Of TDerivedType25, TResult), matchFunc26 As Func(Of TDerivedType26, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
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
        ElseIf TypeOf obj Is TDerivedType8 Then
            Return matchFunc8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            Return matchFunc9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            Return matchFunc10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            Return matchFunc11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            Return matchFunc12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            Return matchFunc13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            Return matchFunc14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            Return matchFunc15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            Return matchFunc16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            Return matchFunc17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            Return matchFunc18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            Return matchFunc19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            Return matchFunc20(DirectCast(obj, TDerivedType20))
        ElseIf TypeOf obj Is TDerivedType21 Then
            Return matchFunc21(DirectCast(obj, TDerivedType21))
        ElseIf TypeOf obj Is TDerivedType22 Then
            Return matchFunc22(DirectCast(obj, TDerivedType22))
        ElseIf TypeOf obj Is TDerivedType23 Then
            Return matchFunc23(DirectCast(obj, TDerivedType23))
        ElseIf TypeOf obj Is TDerivedType24 Then
            Return matchFunc24(DirectCast(obj, TDerivedType24))
        ElseIf TypeOf obj Is TDerivedType25 Then
            Return matchFunc25(DirectCast(obj, TDerivedType25))
        ElseIf TypeOf obj Is TDerivedType26 Then
            Return matchFunc26(DirectCast(obj, TDerivedType26))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TDerivedType21 As TBaseType, TDerivedType22 As TBaseType, TDerivedType23 As TBaseType,
        TDerivedType24 As TBaseType, TDerivedType25 As TBaseType, TDerivedType26 As TBaseType, TDerivedType27 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), matchFunc8 As Func(Of TDerivedType8, TResult), matchFunc9 As Func(Of TDerivedType9, TResult), matchFunc10 As Func(Of TDerivedType10, TResult), matchFunc11 As Func(Of TDerivedType11, TResult),
        matchFunc12 As Func(Of TDerivedType12, TResult), matchFunc13 As Func(Of TDerivedType13, TResult), matchFunc14 As Func(Of TDerivedType14, TResult), matchFunc15 As Func(Of TDerivedType15, TResult), matchFunc16 As Func(Of TDerivedType16, TResult), matchFunc17 As Func(Of TDerivedType17, TResult),
        matchFunc18 As Func(Of TDerivedType18, TResult), matchFunc19 As Func(Of TDerivedType19, TResult), matchFunc20 As Func(Of TDerivedType20, TResult), matchFunc21 As Func(Of TDerivedType21, TResult), matchFunc22 As Func(Of TDerivedType22, TResult), matchFunc23 As Func(Of TDerivedType23, TResult),
        matchFunc24 As Func(Of TDerivedType24, TResult), matchFunc25 As Func(Of TDerivedType25, TResult), matchFunc26 As Func(Of TDerivedType26, TResult), matchFunc27 As Func(Of TDerivedType27, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
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
        ElseIf TypeOf obj Is TDerivedType8 Then
            Return matchFunc8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            Return matchFunc9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            Return matchFunc10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            Return matchFunc11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            Return matchFunc12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            Return matchFunc13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            Return matchFunc14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            Return matchFunc15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            Return matchFunc16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            Return matchFunc17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            Return matchFunc18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            Return matchFunc19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            Return matchFunc20(DirectCast(obj, TDerivedType20))
        ElseIf TypeOf obj Is TDerivedType21 Then
            Return matchFunc21(DirectCast(obj, TDerivedType21))
        ElseIf TypeOf obj Is TDerivedType22 Then
            Return matchFunc22(DirectCast(obj, TDerivedType22))
        ElseIf TypeOf obj Is TDerivedType23 Then
            Return matchFunc23(DirectCast(obj, TDerivedType23))
        ElseIf TypeOf obj Is TDerivedType24 Then
            Return matchFunc24(DirectCast(obj, TDerivedType24))
        ElseIf TypeOf obj Is TDerivedType25 Then
            Return matchFunc25(DirectCast(obj, TDerivedType25))
        ElseIf TypeOf obj Is TDerivedType26 Then
            Return matchFunc26(DirectCast(obj, TDerivedType26))
        ElseIf TypeOf obj Is TDerivedType27 Then
            Return matchFunc27(DirectCast(obj, TDerivedType27))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TDerivedType21 As TBaseType, TDerivedType22 As TBaseType, TDerivedType23 As TBaseType,
        TDerivedType24 As TBaseType, TDerivedType25 As TBaseType, TDerivedType26 As TBaseType, TDerivedType27 As TBaseType, TDerivedType28 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), matchFunc8 As Func(Of TDerivedType8, TResult), matchFunc9 As Func(Of TDerivedType9, TResult), matchFunc10 As Func(Of TDerivedType10, TResult), matchFunc11 As Func(Of TDerivedType11, TResult),
        matchFunc12 As Func(Of TDerivedType12, TResult), matchFunc13 As Func(Of TDerivedType13, TResult), matchFunc14 As Func(Of TDerivedType14, TResult), matchFunc15 As Func(Of TDerivedType15, TResult), matchFunc16 As Func(Of TDerivedType16, TResult), matchFunc17 As Func(Of TDerivedType17, TResult),
        matchFunc18 As Func(Of TDerivedType18, TResult), matchFunc19 As Func(Of TDerivedType19, TResult), matchFunc20 As Func(Of TDerivedType20, TResult), matchFunc21 As Func(Of TDerivedType21, TResult), matchFunc22 As Func(Of TDerivedType22, TResult), matchFunc23 As Func(Of TDerivedType23, TResult),
        matchFunc24 As Func(Of TDerivedType24, TResult), matchFunc25 As Func(Of TDerivedType25, TResult), matchFunc26 As Func(Of TDerivedType26, TResult), matchFunc27 As Func(Of TDerivedType27, TResult), matchFunc28 As Func(Of TDerivedType28, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
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
        ElseIf TypeOf obj Is TDerivedType8 Then
            Return matchFunc8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            Return matchFunc9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            Return matchFunc10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            Return matchFunc11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            Return matchFunc12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            Return matchFunc13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            Return matchFunc14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            Return matchFunc15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            Return matchFunc16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            Return matchFunc17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            Return matchFunc18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            Return matchFunc19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            Return matchFunc20(DirectCast(obj, TDerivedType20))
        ElseIf TypeOf obj Is TDerivedType21 Then
            Return matchFunc21(DirectCast(obj, TDerivedType21))
        ElseIf TypeOf obj Is TDerivedType22 Then
            Return matchFunc22(DirectCast(obj, TDerivedType22))
        ElseIf TypeOf obj Is TDerivedType23 Then
            Return matchFunc23(DirectCast(obj, TDerivedType23))
        ElseIf TypeOf obj Is TDerivedType24 Then
            Return matchFunc24(DirectCast(obj, TDerivedType24))
        ElseIf TypeOf obj Is TDerivedType25 Then
            Return matchFunc25(DirectCast(obj, TDerivedType25))
        ElseIf TypeOf obj Is TDerivedType26 Then
            Return matchFunc26(DirectCast(obj, TDerivedType26))
        ElseIf TypeOf obj Is TDerivedType27 Then
            Return matchFunc27(DirectCast(obj, TDerivedType27))
        ElseIf TypeOf obj Is TDerivedType28 Then
            Return matchFunc28(DirectCast(obj, TDerivedType28))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TDerivedType21 As TBaseType, TDerivedType22 As TBaseType, TDerivedType23 As TBaseType,
        TDerivedType24 As TBaseType, TDerivedType25 As TBaseType, TDerivedType26 As TBaseType, TDerivedType27 As TBaseType, TDerivedType28 As TBaseType, TDerivedType29 As TBaseType,
        TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), matchFunc8 As Func(Of TDerivedType8, TResult), matchFunc9 As Func(Of TDerivedType9, TResult), matchFunc10 As Func(Of TDerivedType10, TResult), matchFunc11 As Func(Of TDerivedType11, TResult),
        matchFunc12 As Func(Of TDerivedType12, TResult), matchFunc13 As Func(Of TDerivedType13, TResult), matchFunc14 As Func(Of TDerivedType14, TResult), matchFunc15 As Func(Of TDerivedType15, TResult), matchFunc16 As Func(Of TDerivedType16, TResult), matchFunc17 As Func(Of TDerivedType17, TResult),
        matchFunc18 As Func(Of TDerivedType18, TResult), matchFunc19 As Func(Of TDerivedType19, TResult), matchFunc20 As Func(Of TDerivedType20, TResult), matchFunc21 As Func(Of TDerivedType21, TResult), matchFunc22 As Func(Of TDerivedType22, TResult), matchFunc23 As Func(Of TDerivedType23, TResult),
        matchFunc24 As Func(Of TDerivedType24, TResult), matchFunc25 As Func(Of TDerivedType25, TResult), matchFunc26 As Func(Of TDerivedType26, TResult), matchFunc27 As Func(Of TDerivedType27, TResult), matchFunc28 As Func(Of TDerivedType28, TResult), matchFunc29 As Func(Of TDerivedType29, TResult),
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
        ElseIf TypeOf obj Is TDerivedType6 Then
            Return matchFunc6(DirectCast(obj, TDerivedType6))
        ElseIf TypeOf obj Is TDerivedType7 Then
            Return matchFunc7(DirectCast(obj, TDerivedType7))
        ElseIf TypeOf obj Is TDerivedType8 Then
            Return matchFunc8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            Return matchFunc9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            Return matchFunc10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            Return matchFunc11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            Return matchFunc12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            Return matchFunc13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            Return matchFunc14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            Return matchFunc15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            Return matchFunc16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            Return matchFunc17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            Return matchFunc18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            Return matchFunc19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            Return matchFunc20(DirectCast(obj, TDerivedType20))
        ElseIf TypeOf obj Is TDerivedType21 Then
            Return matchFunc21(DirectCast(obj, TDerivedType21))
        ElseIf TypeOf obj Is TDerivedType22 Then
            Return matchFunc22(DirectCast(obj, TDerivedType22))
        ElseIf TypeOf obj Is TDerivedType23 Then
            Return matchFunc23(DirectCast(obj, TDerivedType23))
        ElseIf TypeOf obj Is TDerivedType24 Then
            Return matchFunc24(DirectCast(obj, TDerivedType24))
        ElseIf TypeOf obj Is TDerivedType25 Then
            Return matchFunc25(DirectCast(obj, TDerivedType25))
        ElseIf TypeOf obj Is TDerivedType26 Then
            Return matchFunc26(DirectCast(obj, TDerivedType26))
        ElseIf TypeOf obj Is TDerivedType27 Then
            Return matchFunc27(DirectCast(obj, TDerivedType27))
        ElseIf TypeOf obj Is TDerivedType28 Then
            Return matchFunc28(DirectCast(obj, TDerivedType28))
        ElseIf TypeOf obj Is TDerivedType29 Then
            Return matchFunc29(DirectCast(obj, TDerivedType29))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TDerivedType21 As TBaseType, TDerivedType22 As TBaseType, TDerivedType23 As TBaseType,
        TDerivedType24 As TBaseType, TDerivedType25 As TBaseType, TDerivedType26 As TBaseType, TDerivedType27 As TBaseType, TDerivedType28 As TBaseType, TDerivedType29 As TBaseType,
        TDerivedType30 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), matchFunc8 As Func(Of TDerivedType8, TResult), matchFunc9 As Func(Of TDerivedType9, TResult), matchFunc10 As Func(Of TDerivedType10, TResult), matchFunc11 As Func(Of TDerivedType11, TResult),
        matchFunc12 As Func(Of TDerivedType12, TResult), matchFunc13 As Func(Of TDerivedType13, TResult), matchFunc14 As Func(Of TDerivedType14, TResult), matchFunc15 As Func(Of TDerivedType15, TResult), matchFunc16 As Func(Of TDerivedType16, TResult), matchFunc17 As Func(Of TDerivedType17, TResult),
        matchFunc18 As Func(Of TDerivedType18, TResult), matchFunc19 As Func(Of TDerivedType19, TResult), matchFunc20 As Func(Of TDerivedType20, TResult), matchFunc21 As Func(Of TDerivedType21, TResult), matchFunc22 As Func(Of TDerivedType22, TResult), matchFunc23 As Func(Of TDerivedType23, TResult),
        matchFunc24 As Func(Of TDerivedType24, TResult), matchFunc25 As Func(Of TDerivedType25, TResult), matchFunc26 As Func(Of TDerivedType26, TResult), matchFunc27 As Func(Of TDerivedType27, TResult), matchFunc28 As Func(Of TDerivedType28, TResult), matchFunc29 As Func(Of TDerivedType29, TResult),
        matchFunc30 As Func(Of TDerivedType30, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
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
        ElseIf TypeOf obj Is TDerivedType8 Then
            Return matchFunc8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            Return matchFunc9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            Return matchFunc10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            Return matchFunc11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            Return matchFunc12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            Return matchFunc13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            Return matchFunc14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            Return matchFunc15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            Return matchFunc16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            Return matchFunc17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            Return matchFunc18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            Return matchFunc19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            Return matchFunc20(DirectCast(obj, TDerivedType20))
        ElseIf TypeOf obj Is TDerivedType21 Then
            Return matchFunc21(DirectCast(obj, TDerivedType21))
        ElseIf TypeOf obj Is TDerivedType22 Then
            Return matchFunc22(DirectCast(obj, TDerivedType22))
        ElseIf TypeOf obj Is TDerivedType23 Then
            Return matchFunc23(DirectCast(obj, TDerivedType23))
        ElseIf TypeOf obj Is TDerivedType24 Then
            Return matchFunc24(DirectCast(obj, TDerivedType24))
        ElseIf TypeOf obj Is TDerivedType25 Then
            Return matchFunc25(DirectCast(obj, TDerivedType25))
        ElseIf TypeOf obj Is TDerivedType26 Then
            Return matchFunc26(DirectCast(obj, TDerivedType26))
        ElseIf TypeOf obj Is TDerivedType27 Then
            Return matchFunc27(DirectCast(obj, TDerivedType27))
        ElseIf TypeOf obj Is TDerivedType28 Then
            Return matchFunc28(DirectCast(obj, TDerivedType28))
        ElseIf TypeOf obj Is TDerivedType29 Then
            Return matchFunc29(DirectCast(obj, TDerivedType29))
        ElseIf TypeOf obj Is TDerivedType30 Then
            Return matchFunc30(DirectCast(obj, TDerivedType30))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TDerivedType21 As TBaseType, TDerivedType22 As TBaseType, TDerivedType23 As TBaseType,
        TDerivedType24 As TBaseType, TDerivedType25 As TBaseType, TDerivedType26 As TBaseType, TDerivedType27 As TBaseType, TDerivedType28 As TBaseType, TDerivedType29 As TBaseType,
        TDerivedType30 As TBaseType, TDerivedType31 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), matchFunc8 As Func(Of TDerivedType8, TResult), matchFunc9 As Func(Of TDerivedType9, TResult), matchFunc10 As Func(Of TDerivedType10, TResult), matchFunc11 As Func(Of TDerivedType11, TResult),
        matchFunc12 As Func(Of TDerivedType12, TResult), matchFunc13 As Func(Of TDerivedType13, TResult), matchFunc14 As Func(Of TDerivedType14, TResult), matchFunc15 As Func(Of TDerivedType15, TResult), matchFunc16 As Func(Of TDerivedType16, TResult), matchFunc17 As Func(Of TDerivedType17, TResult),
        matchFunc18 As Func(Of TDerivedType18, TResult), matchFunc19 As Func(Of TDerivedType19, TResult), matchFunc20 As Func(Of TDerivedType20, TResult), matchFunc21 As Func(Of TDerivedType21, TResult), matchFunc22 As Func(Of TDerivedType22, TResult), matchFunc23 As Func(Of TDerivedType23, TResult),
        matchFunc24 As Func(Of TDerivedType24, TResult), matchFunc25 As Func(Of TDerivedType25, TResult), matchFunc26 As Func(Of TDerivedType26, TResult), matchFunc27 As Func(Of TDerivedType27, TResult), matchFunc28 As Func(Of TDerivedType28, TResult), matchFunc29 As Func(Of TDerivedType29, TResult),
        matchFunc30 As Func(Of TDerivedType30, TResult), matchFunc31 As Func(Of TDerivedType31, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
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
        ElseIf TypeOf obj Is TDerivedType8 Then
            Return matchFunc8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            Return matchFunc9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            Return matchFunc10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            Return matchFunc11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            Return matchFunc12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            Return matchFunc13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            Return matchFunc14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            Return matchFunc15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            Return matchFunc16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            Return matchFunc17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            Return matchFunc18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            Return matchFunc19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            Return matchFunc20(DirectCast(obj, TDerivedType20))
        ElseIf TypeOf obj Is TDerivedType21 Then
            Return matchFunc21(DirectCast(obj, TDerivedType21))
        ElseIf TypeOf obj Is TDerivedType22 Then
            Return matchFunc22(DirectCast(obj, TDerivedType22))
        ElseIf TypeOf obj Is TDerivedType23 Then
            Return matchFunc23(DirectCast(obj, TDerivedType23))
        ElseIf TypeOf obj Is TDerivedType24 Then
            Return matchFunc24(DirectCast(obj, TDerivedType24))
        ElseIf TypeOf obj Is TDerivedType25 Then
            Return matchFunc25(DirectCast(obj, TDerivedType25))
        ElseIf TypeOf obj Is TDerivedType26 Then
            Return matchFunc26(DirectCast(obj, TDerivedType26))
        ElseIf TypeOf obj Is TDerivedType27 Then
            Return matchFunc27(DirectCast(obj, TDerivedType27))
        ElseIf TypeOf obj Is TDerivedType28 Then
            Return matchFunc28(DirectCast(obj, TDerivedType28))
        ElseIf TypeOf obj Is TDerivedType29 Then
            Return matchFunc29(DirectCast(obj, TDerivedType29))
        ElseIf TypeOf obj Is TDerivedType30 Then
            Return matchFunc30(DirectCast(obj, TDerivedType30))
        ElseIf TypeOf obj Is TDerivedType31 Then
            Return matchFunc31(DirectCast(obj, TDerivedType31))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TDerivedType21 As TBaseType, TDerivedType22 As TBaseType, TDerivedType23 As TBaseType,
        TDerivedType24 As TBaseType, TDerivedType25 As TBaseType, TDerivedType26 As TBaseType, TDerivedType27 As TBaseType, TDerivedType28 As TBaseType, TDerivedType29 As TBaseType,
        TDerivedType30 As TBaseType, TDerivedType31 As TBaseType, TDerivedType32 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), matchFunc8 As Func(Of TDerivedType8, TResult), matchFunc9 As Func(Of TDerivedType9, TResult), matchFunc10 As Func(Of TDerivedType10, TResult), matchFunc11 As Func(Of TDerivedType11, TResult),
        matchFunc12 As Func(Of TDerivedType12, TResult), matchFunc13 As Func(Of TDerivedType13, TResult), matchFunc14 As Func(Of TDerivedType14, TResult), matchFunc15 As Func(Of TDerivedType15, TResult), matchFunc16 As Func(Of TDerivedType16, TResult), matchFunc17 As Func(Of TDerivedType17, TResult),
        matchFunc18 As Func(Of TDerivedType18, TResult), matchFunc19 As Func(Of TDerivedType19, TResult), matchFunc20 As Func(Of TDerivedType20, TResult), matchFunc21 As Func(Of TDerivedType21, TResult), matchFunc22 As Func(Of TDerivedType22, TResult), matchFunc23 As Func(Of TDerivedType23, TResult),
        matchFunc24 As Func(Of TDerivedType24, TResult), matchFunc25 As Func(Of TDerivedType25, TResult), matchFunc26 As Func(Of TDerivedType26, TResult), matchFunc27 As Func(Of TDerivedType27, TResult), matchFunc28 As Func(Of TDerivedType28, TResult), matchFunc29 As Func(Of TDerivedType29, TResult),
        matchFunc30 As Func(Of TDerivedType30, TResult), matchFunc31 As Func(Of TDerivedType31, TResult), matchFunc32 As Func(Of TDerivedType32, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
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
        ElseIf TypeOf obj Is TDerivedType8 Then
            Return matchFunc8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            Return matchFunc9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            Return matchFunc10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            Return matchFunc11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            Return matchFunc12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            Return matchFunc13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            Return matchFunc14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            Return matchFunc15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            Return matchFunc16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            Return matchFunc17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            Return matchFunc18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            Return matchFunc19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            Return matchFunc20(DirectCast(obj, TDerivedType20))
        ElseIf TypeOf obj Is TDerivedType21 Then
            Return matchFunc21(DirectCast(obj, TDerivedType21))
        ElseIf TypeOf obj Is TDerivedType22 Then
            Return matchFunc22(DirectCast(obj, TDerivedType22))
        ElseIf TypeOf obj Is TDerivedType23 Then
            Return matchFunc23(DirectCast(obj, TDerivedType23))
        ElseIf TypeOf obj Is TDerivedType24 Then
            Return matchFunc24(DirectCast(obj, TDerivedType24))
        ElseIf TypeOf obj Is TDerivedType25 Then
            Return matchFunc25(DirectCast(obj, TDerivedType25))
        ElseIf TypeOf obj Is TDerivedType26 Then
            Return matchFunc26(DirectCast(obj, TDerivedType26))
        ElseIf TypeOf obj Is TDerivedType27 Then
            Return matchFunc27(DirectCast(obj, TDerivedType27))
        ElseIf TypeOf obj Is TDerivedType28 Then
            Return matchFunc28(DirectCast(obj, TDerivedType28))
        ElseIf TypeOf obj Is TDerivedType29 Then
            Return matchFunc29(DirectCast(obj, TDerivedType29))
        ElseIf TypeOf obj Is TDerivedType30 Then
            Return matchFunc30(DirectCast(obj, TDerivedType30))
        ElseIf TypeOf obj Is TDerivedType31 Then
            Return matchFunc31(DirectCast(obj, TDerivedType31))
        ElseIf TypeOf obj Is TDerivedType32 Then
            Return matchFunc32(DirectCast(obj, TDerivedType32))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TDerivedType21 As TBaseType, TDerivedType22 As TBaseType, TDerivedType23 As TBaseType,
        TDerivedType24 As TBaseType, TDerivedType25 As TBaseType, TDerivedType26 As TBaseType, TDerivedType27 As TBaseType, TDerivedType28 As TBaseType, TDerivedType29 As TBaseType,
        TDerivedType30 As TBaseType, TDerivedType31 As TBaseType, TDerivedType32 As TBaseType, TDerivedType33 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), matchFunc8 As Func(Of TDerivedType8, TResult), matchFunc9 As Func(Of TDerivedType9, TResult), matchFunc10 As Func(Of TDerivedType10, TResult), matchFunc11 As Func(Of TDerivedType11, TResult),
        matchFunc12 As Func(Of TDerivedType12, TResult), matchFunc13 As Func(Of TDerivedType13, TResult), matchFunc14 As Func(Of TDerivedType14, TResult), matchFunc15 As Func(Of TDerivedType15, TResult), matchFunc16 As Func(Of TDerivedType16, TResult), matchFunc17 As Func(Of TDerivedType17, TResult),
        matchFunc18 As Func(Of TDerivedType18, TResult), matchFunc19 As Func(Of TDerivedType19, TResult), matchFunc20 As Func(Of TDerivedType20, TResult), matchFunc21 As Func(Of TDerivedType21, TResult), matchFunc22 As Func(Of TDerivedType22, TResult), matchFunc23 As Func(Of TDerivedType23, TResult),
        matchFunc24 As Func(Of TDerivedType24, TResult), matchFunc25 As Func(Of TDerivedType25, TResult), matchFunc26 As Func(Of TDerivedType26, TResult), matchFunc27 As Func(Of TDerivedType27, TResult), matchFunc28 As Func(Of TDerivedType28, TResult), matchFunc29 As Func(Of TDerivedType29, TResult),
        matchFunc30 As Func(Of TDerivedType30, TResult), matchFunc31 As Func(Of TDerivedType31, TResult), matchFunc32 As Func(Of TDerivedType32, TResult), matchFunc33 As Func(Of TDerivedType33, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
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
        ElseIf TypeOf obj Is TDerivedType8 Then
            Return matchFunc8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            Return matchFunc9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            Return matchFunc10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            Return matchFunc11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            Return matchFunc12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            Return matchFunc13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            Return matchFunc14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            Return matchFunc15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            Return matchFunc16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            Return matchFunc17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            Return matchFunc18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            Return matchFunc19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            Return matchFunc20(DirectCast(obj, TDerivedType20))
        ElseIf TypeOf obj Is TDerivedType21 Then
            Return matchFunc21(DirectCast(obj, TDerivedType21))
        ElseIf TypeOf obj Is TDerivedType22 Then
            Return matchFunc22(DirectCast(obj, TDerivedType22))
        ElseIf TypeOf obj Is TDerivedType23 Then
            Return matchFunc23(DirectCast(obj, TDerivedType23))
        ElseIf TypeOf obj Is TDerivedType24 Then
            Return matchFunc24(DirectCast(obj, TDerivedType24))
        ElseIf TypeOf obj Is TDerivedType25 Then
            Return matchFunc25(DirectCast(obj, TDerivedType25))
        ElseIf TypeOf obj Is TDerivedType26 Then
            Return matchFunc26(DirectCast(obj, TDerivedType26))
        ElseIf TypeOf obj Is TDerivedType27 Then
            Return matchFunc27(DirectCast(obj, TDerivedType27))
        ElseIf TypeOf obj Is TDerivedType28 Then
            Return matchFunc28(DirectCast(obj, TDerivedType28))
        ElseIf TypeOf obj Is TDerivedType29 Then
            Return matchFunc29(DirectCast(obj, TDerivedType29))
        ElseIf TypeOf obj Is TDerivedType30 Then
            Return matchFunc30(DirectCast(obj, TDerivedType30))
        ElseIf TypeOf obj Is TDerivedType31 Then
            Return matchFunc31(DirectCast(obj, TDerivedType31))
        ElseIf TypeOf obj Is TDerivedType32 Then
            Return matchFunc32(DirectCast(obj, TDerivedType32))
        ElseIf TypeOf obj Is TDerivedType33 Then
            Return matchFunc33(DirectCast(obj, TDerivedType33))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TDerivedType21 As TBaseType, TDerivedType22 As TBaseType, TDerivedType23 As TBaseType,
        TDerivedType24 As TBaseType, TDerivedType25 As TBaseType, TDerivedType26 As TBaseType, TDerivedType27 As TBaseType, TDerivedType28 As TBaseType, TDerivedType29 As TBaseType,
        TDerivedType30 As TBaseType, TDerivedType31 As TBaseType, TDerivedType32 As TBaseType, TDerivedType33 As TBaseType, TDerivedType34 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), matchFunc8 As Func(Of TDerivedType8, TResult), matchFunc9 As Func(Of TDerivedType9, TResult), matchFunc10 As Func(Of TDerivedType10, TResult), matchFunc11 As Func(Of TDerivedType11, TResult),
        matchFunc12 As Func(Of TDerivedType12, TResult), matchFunc13 As Func(Of TDerivedType13, TResult), matchFunc14 As Func(Of TDerivedType14, TResult), matchFunc15 As Func(Of TDerivedType15, TResult), matchFunc16 As Func(Of TDerivedType16, TResult), matchFunc17 As Func(Of TDerivedType17, TResult),
        matchFunc18 As Func(Of TDerivedType18, TResult), matchFunc19 As Func(Of TDerivedType19, TResult), matchFunc20 As Func(Of TDerivedType20, TResult), matchFunc21 As Func(Of TDerivedType21, TResult), matchFunc22 As Func(Of TDerivedType22, TResult), matchFunc23 As Func(Of TDerivedType23, TResult),
        matchFunc24 As Func(Of TDerivedType24, TResult), matchFunc25 As Func(Of TDerivedType25, TResult), matchFunc26 As Func(Of TDerivedType26, TResult), matchFunc27 As Func(Of TDerivedType27, TResult), matchFunc28 As Func(Of TDerivedType28, TResult), matchFunc29 As Func(Of TDerivedType29, TResult),
        matchFunc30 As Func(Of TDerivedType30, TResult), matchFunc31 As Func(Of TDerivedType31, TResult), matchFunc32 As Func(Of TDerivedType32, TResult), matchFunc33 As Func(Of TDerivedType33, TResult), matchFunc34 As Func(Of TDerivedType34, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
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
        ElseIf TypeOf obj Is TDerivedType8 Then
            Return matchFunc8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            Return matchFunc9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            Return matchFunc10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            Return matchFunc11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            Return matchFunc12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            Return matchFunc13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            Return matchFunc14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            Return matchFunc15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            Return matchFunc16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            Return matchFunc17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            Return matchFunc18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            Return matchFunc19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            Return matchFunc20(DirectCast(obj, TDerivedType20))
        ElseIf TypeOf obj Is TDerivedType21 Then
            Return matchFunc21(DirectCast(obj, TDerivedType21))
        ElseIf TypeOf obj Is TDerivedType22 Then
            Return matchFunc22(DirectCast(obj, TDerivedType22))
        ElseIf TypeOf obj Is TDerivedType23 Then
            Return matchFunc23(DirectCast(obj, TDerivedType23))
        ElseIf TypeOf obj Is TDerivedType24 Then
            Return matchFunc24(DirectCast(obj, TDerivedType24))
        ElseIf TypeOf obj Is TDerivedType25 Then
            Return matchFunc25(DirectCast(obj, TDerivedType25))
        ElseIf TypeOf obj Is TDerivedType26 Then
            Return matchFunc26(DirectCast(obj, TDerivedType26))
        ElseIf TypeOf obj Is TDerivedType27 Then
            Return matchFunc27(DirectCast(obj, TDerivedType27))
        ElseIf TypeOf obj Is TDerivedType28 Then
            Return matchFunc28(DirectCast(obj, TDerivedType28))
        ElseIf TypeOf obj Is TDerivedType29 Then
            Return matchFunc29(DirectCast(obj, TDerivedType29))
        ElseIf TypeOf obj Is TDerivedType30 Then
            Return matchFunc30(DirectCast(obj, TDerivedType30))
        ElseIf TypeOf obj Is TDerivedType31 Then
            Return matchFunc31(DirectCast(obj, TDerivedType31))
        ElseIf TypeOf obj Is TDerivedType32 Then
            Return matchFunc32(DirectCast(obj, TDerivedType32))
        ElseIf TypeOf obj Is TDerivedType33 Then
            Return matchFunc33(DirectCast(obj, TDerivedType33))
        ElseIf TypeOf obj Is TDerivedType34 Then
            Return matchFunc34(DirectCast(obj, TDerivedType34))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TDerivedType21 As TBaseType, TDerivedType22 As TBaseType, TDerivedType23 As TBaseType,
        TDerivedType24 As TBaseType, TDerivedType25 As TBaseType, TDerivedType26 As TBaseType, TDerivedType27 As TBaseType, TDerivedType28 As TBaseType, TDerivedType29 As TBaseType,
        TDerivedType30 As TBaseType, TDerivedType31 As TBaseType, TDerivedType32 As TBaseType, TDerivedType33 As TBaseType, TDerivedType34 As TBaseType, TDerivedType35 As TBaseType,
        TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), matchFunc8 As Func(Of TDerivedType8, TResult), matchFunc9 As Func(Of TDerivedType9, TResult), matchFunc10 As Func(Of TDerivedType10, TResult), matchFunc11 As Func(Of TDerivedType11, TResult),
        matchFunc12 As Func(Of TDerivedType12, TResult), matchFunc13 As Func(Of TDerivedType13, TResult), matchFunc14 As Func(Of TDerivedType14, TResult), matchFunc15 As Func(Of TDerivedType15, TResult), matchFunc16 As Func(Of TDerivedType16, TResult), matchFunc17 As Func(Of TDerivedType17, TResult),
        matchFunc18 As Func(Of TDerivedType18, TResult), matchFunc19 As Func(Of TDerivedType19, TResult), matchFunc20 As Func(Of TDerivedType20, TResult), matchFunc21 As Func(Of TDerivedType21, TResult), matchFunc22 As Func(Of TDerivedType22, TResult), matchFunc23 As Func(Of TDerivedType23, TResult),
        matchFunc24 As Func(Of TDerivedType24, TResult), matchFunc25 As Func(Of TDerivedType25, TResult), matchFunc26 As Func(Of TDerivedType26, TResult), matchFunc27 As Func(Of TDerivedType27, TResult), matchFunc28 As Func(Of TDerivedType28, TResult), matchFunc29 As Func(Of TDerivedType29, TResult),
        matchFunc30 As Func(Of TDerivedType30, TResult), matchFunc31 As Func(Of TDerivedType31, TResult), matchFunc32 As Func(Of TDerivedType32, TResult), matchFunc33 As Func(Of TDerivedType33, TResult), matchFunc34 As Func(Of TDerivedType34, TResult), matchFunc35 As Func(Of TDerivedType35, TResult),
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
        ElseIf TypeOf obj Is TDerivedType6 Then
            Return matchFunc6(DirectCast(obj, TDerivedType6))
        ElseIf TypeOf obj Is TDerivedType7 Then
            Return matchFunc7(DirectCast(obj, TDerivedType7))
        ElseIf TypeOf obj Is TDerivedType8 Then
            Return matchFunc8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            Return matchFunc9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            Return matchFunc10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            Return matchFunc11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            Return matchFunc12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            Return matchFunc13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            Return matchFunc14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            Return matchFunc15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            Return matchFunc16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            Return matchFunc17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            Return matchFunc18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            Return matchFunc19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            Return matchFunc20(DirectCast(obj, TDerivedType20))
        ElseIf TypeOf obj Is TDerivedType21 Then
            Return matchFunc21(DirectCast(obj, TDerivedType21))
        ElseIf TypeOf obj Is TDerivedType22 Then
            Return matchFunc22(DirectCast(obj, TDerivedType22))
        ElseIf TypeOf obj Is TDerivedType23 Then
            Return matchFunc23(DirectCast(obj, TDerivedType23))
        ElseIf TypeOf obj Is TDerivedType24 Then
            Return matchFunc24(DirectCast(obj, TDerivedType24))
        ElseIf TypeOf obj Is TDerivedType25 Then
            Return matchFunc25(DirectCast(obj, TDerivedType25))
        ElseIf TypeOf obj Is TDerivedType26 Then
            Return matchFunc26(DirectCast(obj, TDerivedType26))
        ElseIf TypeOf obj Is TDerivedType27 Then
            Return matchFunc27(DirectCast(obj, TDerivedType27))
        ElseIf TypeOf obj Is TDerivedType28 Then
            Return matchFunc28(DirectCast(obj, TDerivedType28))
        ElseIf TypeOf obj Is TDerivedType29 Then
            Return matchFunc29(DirectCast(obj, TDerivedType29))
        ElseIf TypeOf obj Is TDerivedType30 Then
            Return matchFunc30(DirectCast(obj, TDerivedType30))
        ElseIf TypeOf obj Is TDerivedType31 Then
            Return matchFunc31(DirectCast(obj, TDerivedType31))
        ElseIf TypeOf obj Is TDerivedType32 Then
            Return matchFunc32(DirectCast(obj, TDerivedType32))
        ElseIf TypeOf obj Is TDerivedType33 Then
            Return matchFunc33(DirectCast(obj, TDerivedType33))
        ElseIf TypeOf obj Is TDerivedType34 Then
            Return matchFunc34(DirectCast(obj, TDerivedType34))
        ElseIf TypeOf obj Is TDerivedType35 Then
            Return matchFunc35(DirectCast(obj, TDerivedType35))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TDerivedType21 As TBaseType, TDerivedType22 As TBaseType, TDerivedType23 As TBaseType,
        TDerivedType24 As TBaseType, TDerivedType25 As TBaseType, TDerivedType26 As TBaseType, TDerivedType27 As TBaseType, TDerivedType28 As TBaseType, TDerivedType29 As TBaseType,
        TDerivedType30 As TBaseType, TDerivedType31 As TBaseType, TDerivedType32 As TBaseType, TDerivedType33 As TBaseType, TDerivedType34 As TBaseType, TDerivedType35 As TBaseType,
        TDerivedType36 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), matchFunc8 As Func(Of TDerivedType8, TResult), matchFunc9 As Func(Of TDerivedType9, TResult), matchFunc10 As Func(Of TDerivedType10, TResult), matchFunc11 As Func(Of TDerivedType11, TResult),
        matchFunc12 As Func(Of TDerivedType12, TResult), matchFunc13 As Func(Of TDerivedType13, TResult), matchFunc14 As Func(Of TDerivedType14, TResult), matchFunc15 As Func(Of TDerivedType15, TResult), matchFunc16 As Func(Of TDerivedType16, TResult), matchFunc17 As Func(Of TDerivedType17, TResult),
        matchFunc18 As Func(Of TDerivedType18, TResult), matchFunc19 As Func(Of TDerivedType19, TResult), matchFunc20 As Func(Of TDerivedType20, TResult), matchFunc21 As Func(Of TDerivedType21, TResult), matchFunc22 As Func(Of TDerivedType22, TResult), matchFunc23 As Func(Of TDerivedType23, TResult),
        matchFunc24 As Func(Of TDerivedType24, TResult), matchFunc25 As Func(Of TDerivedType25, TResult), matchFunc26 As Func(Of TDerivedType26, TResult), matchFunc27 As Func(Of TDerivedType27, TResult), matchFunc28 As Func(Of TDerivedType28, TResult), matchFunc29 As Func(Of TDerivedType29, TResult),
        matchFunc30 As Func(Of TDerivedType30, TResult), matchFunc31 As Func(Of TDerivedType31, TResult), matchFunc32 As Func(Of TDerivedType32, TResult), matchFunc33 As Func(Of TDerivedType33, TResult), matchFunc34 As Func(Of TDerivedType34, TResult), matchFunc35 As Func(Of TDerivedType35, TResult),
        matchFunc36 As Func(Of TDerivedType36, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
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
        ElseIf TypeOf obj Is TDerivedType8 Then
            Return matchFunc8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            Return matchFunc9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            Return matchFunc10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            Return matchFunc11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            Return matchFunc12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            Return matchFunc13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            Return matchFunc14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            Return matchFunc15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            Return matchFunc16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            Return matchFunc17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            Return matchFunc18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            Return matchFunc19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            Return matchFunc20(DirectCast(obj, TDerivedType20))
        ElseIf TypeOf obj Is TDerivedType21 Then
            Return matchFunc21(DirectCast(obj, TDerivedType21))
        ElseIf TypeOf obj Is TDerivedType22 Then
            Return matchFunc22(DirectCast(obj, TDerivedType22))
        ElseIf TypeOf obj Is TDerivedType23 Then
            Return matchFunc23(DirectCast(obj, TDerivedType23))
        ElseIf TypeOf obj Is TDerivedType24 Then
            Return matchFunc24(DirectCast(obj, TDerivedType24))
        ElseIf TypeOf obj Is TDerivedType25 Then
            Return matchFunc25(DirectCast(obj, TDerivedType25))
        ElseIf TypeOf obj Is TDerivedType26 Then
            Return matchFunc26(DirectCast(obj, TDerivedType26))
        ElseIf TypeOf obj Is TDerivedType27 Then
            Return matchFunc27(DirectCast(obj, TDerivedType27))
        ElseIf TypeOf obj Is TDerivedType28 Then
            Return matchFunc28(DirectCast(obj, TDerivedType28))
        ElseIf TypeOf obj Is TDerivedType29 Then
            Return matchFunc29(DirectCast(obj, TDerivedType29))
        ElseIf TypeOf obj Is TDerivedType30 Then
            Return matchFunc30(DirectCast(obj, TDerivedType30))
        ElseIf TypeOf obj Is TDerivedType31 Then
            Return matchFunc31(DirectCast(obj, TDerivedType31))
        ElseIf TypeOf obj Is TDerivedType32 Then
            Return matchFunc32(DirectCast(obj, TDerivedType32))
        ElseIf TypeOf obj Is TDerivedType33 Then
            Return matchFunc33(DirectCast(obj, TDerivedType33))
        ElseIf TypeOf obj Is TDerivedType34 Then
            Return matchFunc34(DirectCast(obj, TDerivedType34))
        ElseIf TypeOf obj Is TDerivedType35 Then
            Return matchFunc35(DirectCast(obj, TDerivedType35))
        ElseIf TypeOf obj Is TDerivedType36 Then
            Return matchFunc36(DirectCast(obj, TDerivedType36))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TDerivedType21 As TBaseType, TDerivedType22 As TBaseType, TDerivedType23 As TBaseType,
        TDerivedType24 As TBaseType, TDerivedType25 As TBaseType, TDerivedType26 As TBaseType, TDerivedType27 As TBaseType, TDerivedType28 As TBaseType, TDerivedType29 As TBaseType,
        TDerivedType30 As TBaseType, TDerivedType31 As TBaseType, TDerivedType32 As TBaseType, TDerivedType33 As TBaseType, TDerivedType34 As TBaseType, TDerivedType35 As TBaseType,
        TDerivedType36 As TBaseType, TDerivedType37 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), matchFunc8 As Func(Of TDerivedType8, TResult), matchFunc9 As Func(Of TDerivedType9, TResult), matchFunc10 As Func(Of TDerivedType10, TResult), matchFunc11 As Func(Of TDerivedType11, TResult),
        matchFunc12 As Func(Of TDerivedType12, TResult), matchFunc13 As Func(Of TDerivedType13, TResult), matchFunc14 As Func(Of TDerivedType14, TResult), matchFunc15 As Func(Of TDerivedType15, TResult), matchFunc16 As Func(Of TDerivedType16, TResult), matchFunc17 As Func(Of TDerivedType17, TResult),
        matchFunc18 As Func(Of TDerivedType18, TResult), matchFunc19 As Func(Of TDerivedType19, TResult), matchFunc20 As Func(Of TDerivedType20, TResult), matchFunc21 As Func(Of TDerivedType21, TResult), matchFunc22 As Func(Of TDerivedType22, TResult), matchFunc23 As Func(Of TDerivedType23, TResult),
        matchFunc24 As Func(Of TDerivedType24, TResult), matchFunc25 As Func(Of TDerivedType25, TResult), matchFunc26 As Func(Of TDerivedType26, TResult), matchFunc27 As Func(Of TDerivedType27, TResult), matchFunc28 As Func(Of TDerivedType28, TResult), matchFunc29 As Func(Of TDerivedType29, TResult),
        matchFunc30 As Func(Of TDerivedType30, TResult), matchFunc31 As Func(Of TDerivedType31, TResult), matchFunc32 As Func(Of TDerivedType32, TResult), matchFunc33 As Func(Of TDerivedType33, TResult), matchFunc34 As Func(Of TDerivedType34, TResult), matchFunc35 As Func(Of TDerivedType35, TResult),
        matchFunc36 As Func(Of TDerivedType36, TResult), matchFunc37 As Func(Of TDerivedType37, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
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
        ElseIf TypeOf obj Is TDerivedType8 Then
            Return matchFunc8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            Return matchFunc9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            Return matchFunc10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            Return matchFunc11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            Return matchFunc12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            Return matchFunc13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            Return matchFunc14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            Return matchFunc15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            Return matchFunc16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            Return matchFunc17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            Return matchFunc18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            Return matchFunc19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            Return matchFunc20(DirectCast(obj, TDerivedType20))
        ElseIf TypeOf obj Is TDerivedType21 Then
            Return matchFunc21(DirectCast(obj, TDerivedType21))
        ElseIf TypeOf obj Is TDerivedType22 Then
            Return matchFunc22(DirectCast(obj, TDerivedType22))
        ElseIf TypeOf obj Is TDerivedType23 Then
            Return matchFunc23(DirectCast(obj, TDerivedType23))
        ElseIf TypeOf obj Is TDerivedType24 Then
            Return matchFunc24(DirectCast(obj, TDerivedType24))
        ElseIf TypeOf obj Is TDerivedType25 Then
            Return matchFunc25(DirectCast(obj, TDerivedType25))
        ElseIf TypeOf obj Is TDerivedType26 Then
            Return matchFunc26(DirectCast(obj, TDerivedType26))
        ElseIf TypeOf obj Is TDerivedType27 Then
            Return matchFunc27(DirectCast(obj, TDerivedType27))
        ElseIf TypeOf obj Is TDerivedType28 Then
            Return matchFunc28(DirectCast(obj, TDerivedType28))
        ElseIf TypeOf obj Is TDerivedType29 Then
            Return matchFunc29(DirectCast(obj, TDerivedType29))
        ElseIf TypeOf obj Is TDerivedType30 Then
            Return matchFunc30(DirectCast(obj, TDerivedType30))
        ElseIf TypeOf obj Is TDerivedType31 Then
            Return matchFunc31(DirectCast(obj, TDerivedType31))
        ElseIf TypeOf obj Is TDerivedType32 Then
            Return matchFunc32(DirectCast(obj, TDerivedType32))
        ElseIf TypeOf obj Is TDerivedType33 Then
            Return matchFunc33(DirectCast(obj, TDerivedType33))
        ElseIf TypeOf obj Is TDerivedType34 Then
            Return matchFunc34(DirectCast(obj, TDerivedType34))
        ElseIf TypeOf obj Is TDerivedType35 Then
            Return matchFunc35(DirectCast(obj, TDerivedType35))
        ElseIf TypeOf obj Is TDerivedType36 Then
            Return matchFunc36(DirectCast(obj, TDerivedType36))
        ElseIf TypeOf obj Is TDerivedType37 Then
            Return matchFunc37(DirectCast(obj, TDerivedType37))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TDerivedType21 As TBaseType, TDerivedType22 As TBaseType, TDerivedType23 As TBaseType,
        TDerivedType24 As TBaseType, TDerivedType25 As TBaseType, TDerivedType26 As TBaseType, TDerivedType27 As TBaseType, TDerivedType28 As TBaseType, TDerivedType29 As TBaseType,
        TDerivedType30 As TBaseType, TDerivedType31 As TBaseType, TDerivedType32 As TBaseType, TDerivedType33 As TBaseType, TDerivedType34 As TBaseType, TDerivedType35 As TBaseType,
        TDerivedType36 As TBaseType, TDerivedType37 As TBaseType, TDerivedType38 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), matchFunc8 As Func(Of TDerivedType8, TResult), matchFunc9 As Func(Of TDerivedType9, TResult), matchFunc10 As Func(Of TDerivedType10, TResult), matchFunc11 As Func(Of TDerivedType11, TResult),
        matchFunc12 As Func(Of TDerivedType12, TResult), matchFunc13 As Func(Of TDerivedType13, TResult), matchFunc14 As Func(Of TDerivedType14, TResult), matchFunc15 As Func(Of TDerivedType15, TResult), matchFunc16 As Func(Of TDerivedType16, TResult), matchFunc17 As Func(Of TDerivedType17, TResult),
        matchFunc18 As Func(Of TDerivedType18, TResult), matchFunc19 As Func(Of TDerivedType19, TResult), matchFunc20 As Func(Of TDerivedType20, TResult), matchFunc21 As Func(Of TDerivedType21, TResult), matchFunc22 As Func(Of TDerivedType22, TResult), matchFunc23 As Func(Of TDerivedType23, TResult),
        matchFunc24 As Func(Of TDerivedType24, TResult), matchFunc25 As Func(Of TDerivedType25, TResult), matchFunc26 As Func(Of TDerivedType26, TResult), matchFunc27 As Func(Of TDerivedType27, TResult), matchFunc28 As Func(Of TDerivedType28, TResult), matchFunc29 As Func(Of TDerivedType29, TResult),
        matchFunc30 As Func(Of TDerivedType30, TResult), matchFunc31 As Func(Of TDerivedType31, TResult), matchFunc32 As Func(Of TDerivedType32, TResult), matchFunc33 As Func(Of TDerivedType33, TResult), matchFunc34 As Func(Of TDerivedType34, TResult), matchFunc35 As Func(Of TDerivedType35, TResult),
        matchFunc36 As Func(Of TDerivedType36, TResult), matchFunc37 As Func(Of TDerivedType37, TResult), matchFunc38 As Func(Of TDerivedType38, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
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
        ElseIf TypeOf obj Is TDerivedType8 Then
            Return matchFunc8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            Return matchFunc9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            Return matchFunc10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            Return matchFunc11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            Return matchFunc12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            Return matchFunc13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            Return matchFunc14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            Return matchFunc15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            Return matchFunc16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            Return matchFunc17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            Return matchFunc18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            Return matchFunc19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            Return matchFunc20(DirectCast(obj, TDerivedType20))
        ElseIf TypeOf obj Is TDerivedType21 Then
            Return matchFunc21(DirectCast(obj, TDerivedType21))
        ElseIf TypeOf obj Is TDerivedType22 Then
            Return matchFunc22(DirectCast(obj, TDerivedType22))
        ElseIf TypeOf obj Is TDerivedType23 Then
            Return matchFunc23(DirectCast(obj, TDerivedType23))
        ElseIf TypeOf obj Is TDerivedType24 Then
            Return matchFunc24(DirectCast(obj, TDerivedType24))
        ElseIf TypeOf obj Is TDerivedType25 Then
            Return matchFunc25(DirectCast(obj, TDerivedType25))
        ElseIf TypeOf obj Is TDerivedType26 Then
            Return matchFunc26(DirectCast(obj, TDerivedType26))
        ElseIf TypeOf obj Is TDerivedType27 Then
            Return matchFunc27(DirectCast(obj, TDerivedType27))
        ElseIf TypeOf obj Is TDerivedType28 Then
            Return matchFunc28(DirectCast(obj, TDerivedType28))
        ElseIf TypeOf obj Is TDerivedType29 Then
            Return matchFunc29(DirectCast(obj, TDerivedType29))
        ElseIf TypeOf obj Is TDerivedType30 Then
            Return matchFunc30(DirectCast(obj, TDerivedType30))
        ElseIf TypeOf obj Is TDerivedType31 Then
            Return matchFunc31(DirectCast(obj, TDerivedType31))
        ElseIf TypeOf obj Is TDerivedType32 Then
            Return matchFunc32(DirectCast(obj, TDerivedType32))
        ElseIf TypeOf obj Is TDerivedType33 Then
            Return matchFunc33(DirectCast(obj, TDerivedType33))
        ElseIf TypeOf obj Is TDerivedType34 Then
            Return matchFunc34(DirectCast(obj, TDerivedType34))
        ElseIf TypeOf obj Is TDerivedType35 Then
            Return matchFunc35(DirectCast(obj, TDerivedType35))
        ElseIf TypeOf obj Is TDerivedType36 Then
            Return matchFunc36(DirectCast(obj, TDerivedType36))
        ElseIf TypeOf obj Is TDerivedType37 Then
            Return matchFunc37(DirectCast(obj, TDerivedType37))
        ElseIf TypeOf obj Is TDerivedType38 Then
            Return matchFunc38(DirectCast(obj, TDerivedType38))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TDerivedType21 As TBaseType, TDerivedType22 As TBaseType, TDerivedType23 As TBaseType,
        TDerivedType24 As TBaseType, TDerivedType25 As TBaseType, TDerivedType26 As TBaseType, TDerivedType27 As TBaseType, TDerivedType28 As TBaseType, TDerivedType29 As TBaseType,
        TDerivedType30 As TBaseType, TDerivedType31 As TBaseType, TDerivedType32 As TBaseType, TDerivedType33 As TBaseType, TDerivedType34 As TBaseType, TDerivedType35 As TBaseType,
        TDerivedType36 As TBaseType, TDerivedType37 As TBaseType, TDerivedType38 As TBaseType, TDerivedType39 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), matchFunc8 As Func(Of TDerivedType8, TResult), matchFunc9 As Func(Of TDerivedType9, TResult), matchFunc10 As Func(Of TDerivedType10, TResult), matchFunc11 As Func(Of TDerivedType11, TResult),
        matchFunc12 As Func(Of TDerivedType12, TResult), matchFunc13 As Func(Of TDerivedType13, TResult), matchFunc14 As Func(Of TDerivedType14, TResult), matchFunc15 As Func(Of TDerivedType15, TResult), matchFunc16 As Func(Of TDerivedType16, TResult), matchFunc17 As Func(Of TDerivedType17, TResult),
        matchFunc18 As Func(Of TDerivedType18, TResult), matchFunc19 As Func(Of TDerivedType19, TResult), matchFunc20 As Func(Of TDerivedType20, TResult), matchFunc21 As Func(Of TDerivedType21, TResult), matchFunc22 As Func(Of TDerivedType22, TResult), matchFunc23 As Func(Of TDerivedType23, TResult),
        matchFunc24 As Func(Of TDerivedType24, TResult), matchFunc25 As Func(Of TDerivedType25, TResult), matchFunc26 As Func(Of TDerivedType26, TResult), matchFunc27 As Func(Of TDerivedType27, TResult), matchFunc28 As Func(Of TDerivedType28, TResult), matchFunc29 As Func(Of TDerivedType29, TResult),
        matchFunc30 As Func(Of TDerivedType30, TResult), matchFunc31 As Func(Of TDerivedType31, TResult), matchFunc32 As Func(Of TDerivedType32, TResult), matchFunc33 As Func(Of TDerivedType33, TResult), matchFunc34 As Func(Of TDerivedType34, TResult), matchFunc35 As Func(Of TDerivedType35, TResult),
        matchFunc36 As Func(Of TDerivedType36, TResult), matchFunc37 As Func(Of TDerivedType37, TResult), matchFunc38 As Func(Of TDerivedType38, TResult), matchFunc39 As Func(Of TDerivedType39, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
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
        ElseIf TypeOf obj Is TDerivedType8 Then
            Return matchFunc8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            Return matchFunc9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            Return matchFunc10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            Return matchFunc11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            Return matchFunc12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            Return matchFunc13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            Return matchFunc14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            Return matchFunc15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            Return matchFunc16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            Return matchFunc17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            Return matchFunc18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            Return matchFunc19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            Return matchFunc20(DirectCast(obj, TDerivedType20))
        ElseIf TypeOf obj Is TDerivedType21 Then
            Return matchFunc21(DirectCast(obj, TDerivedType21))
        ElseIf TypeOf obj Is TDerivedType22 Then
            Return matchFunc22(DirectCast(obj, TDerivedType22))
        ElseIf TypeOf obj Is TDerivedType23 Then
            Return matchFunc23(DirectCast(obj, TDerivedType23))
        ElseIf TypeOf obj Is TDerivedType24 Then
            Return matchFunc24(DirectCast(obj, TDerivedType24))
        ElseIf TypeOf obj Is TDerivedType25 Then
            Return matchFunc25(DirectCast(obj, TDerivedType25))
        ElseIf TypeOf obj Is TDerivedType26 Then
            Return matchFunc26(DirectCast(obj, TDerivedType26))
        ElseIf TypeOf obj Is TDerivedType27 Then
            Return matchFunc27(DirectCast(obj, TDerivedType27))
        ElseIf TypeOf obj Is TDerivedType28 Then
            Return matchFunc28(DirectCast(obj, TDerivedType28))
        ElseIf TypeOf obj Is TDerivedType29 Then
            Return matchFunc29(DirectCast(obj, TDerivedType29))
        ElseIf TypeOf obj Is TDerivedType30 Then
            Return matchFunc30(DirectCast(obj, TDerivedType30))
        ElseIf TypeOf obj Is TDerivedType31 Then
            Return matchFunc31(DirectCast(obj, TDerivedType31))
        ElseIf TypeOf obj Is TDerivedType32 Then
            Return matchFunc32(DirectCast(obj, TDerivedType32))
        ElseIf TypeOf obj Is TDerivedType33 Then
            Return matchFunc33(DirectCast(obj, TDerivedType33))
        ElseIf TypeOf obj Is TDerivedType34 Then
            Return matchFunc34(DirectCast(obj, TDerivedType34))
        ElseIf TypeOf obj Is TDerivedType35 Then
            Return matchFunc35(DirectCast(obj, TDerivedType35))
        ElseIf TypeOf obj Is TDerivedType36 Then
            Return matchFunc36(DirectCast(obj, TDerivedType36))
        ElseIf TypeOf obj Is TDerivedType37 Then
            Return matchFunc37(DirectCast(obj, TDerivedType37))
        ElseIf TypeOf obj Is TDerivedType38 Then
            Return matchFunc38(DirectCast(obj, TDerivedType38))
        ElseIf TypeOf obj Is TDerivedType39 Then
            Return matchFunc39(DirectCast(obj, TDerivedType39))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

    <Extension>
    Public Function TypeSwitch(Of TBaseType, TDerivedType1 As TBaseType, TDerivedType2 As TBaseType, TDerivedType3 As TBaseType, TDerivedType4 As TBaseType, TDerivedType5 As TBaseType,
        TDerivedType6 As TBaseType, TDerivedType7 As TBaseType, TDerivedType8 As TBaseType, TDerivedType9 As TBaseType, TDerivedType10 As TBaseType, TDerivedType11 As TBaseType,
        TDerivedType12 As TBaseType, TDerivedType13 As TBaseType, TDerivedType14 As TBaseType, TDerivedType15 As TBaseType, TDerivedType16 As TBaseType, TDerivedType17 As TBaseType,
        TDerivedType18 As TBaseType, TDerivedType19 As TBaseType, TDerivedType20 As TBaseType, TDerivedType21 As TBaseType, TDerivedType22 As TBaseType, TDerivedType23 As TBaseType,
        TDerivedType24 As TBaseType, TDerivedType25 As TBaseType, TDerivedType26 As TBaseType, TDerivedType27 As TBaseType, TDerivedType28 As TBaseType, TDerivedType29 As TBaseType,
        TDerivedType30 As TBaseType, TDerivedType31 As TBaseType, TDerivedType32 As TBaseType, TDerivedType33 As TBaseType, TDerivedType34 As TBaseType, TDerivedType35 As TBaseType,
        TDerivedType36 As TBaseType, TDerivedType37 As TBaseType, TDerivedType38 As TBaseType, TDerivedType39 As TBaseType, TDerivedType40 As TBaseType, TResult)(obj As TBaseType, matchFunc1 As Func(Of TDerivedType1, TResult), matchFunc2 As Func(Of TDerivedType2, TResult), matchFunc3 As Func(Of TDerivedType3, TResult), matchFunc4 As Func(Of TDerivedType4, TResult), matchFunc5 As Func(Of TDerivedType5, TResult),
        matchFunc6 As Func(Of TDerivedType6, TResult), matchFunc7 As Func(Of TDerivedType7, TResult), matchFunc8 As Func(Of TDerivedType8, TResult), matchFunc9 As Func(Of TDerivedType9, TResult), matchFunc10 As Func(Of TDerivedType10, TResult), matchFunc11 As Func(Of TDerivedType11, TResult),
        matchFunc12 As Func(Of TDerivedType12, TResult), matchFunc13 As Func(Of TDerivedType13, TResult), matchFunc14 As Func(Of TDerivedType14, TResult), matchFunc15 As Func(Of TDerivedType15, TResult), matchFunc16 As Func(Of TDerivedType16, TResult), matchFunc17 As Func(Of TDerivedType17, TResult),
        matchFunc18 As Func(Of TDerivedType18, TResult), matchFunc19 As Func(Of TDerivedType19, TResult), matchFunc20 As Func(Of TDerivedType20, TResult), matchFunc21 As Func(Of TDerivedType21, TResult), matchFunc22 As Func(Of TDerivedType22, TResult), matchFunc23 As Func(Of TDerivedType23, TResult),
        matchFunc24 As Func(Of TDerivedType24, TResult), matchFunc25 As Func(Of TDerivedType25, TResult), matchFunc26 As Func(Of TDerivedType26, TResult), matchFunc27 As Func(Of TDerivedType27, TResult), matchFunc28 As Func(Of TDerivedType28, TResult), matchFunc29 As Func(Of TDerivedType29, TResult),
        matchFunc30 As Func(Of TDerivedType30, TResult), matchFunc31 As Func(Of TDerivedType31, TResult), matchFunc32 As Func(Of TDerivedType32, TResult), matchFunc33 As Func(Of TDerivedType33, TResult), matchFunc34 As Func(Of TDerivedType34, TResult), matchFunc35 As Func(Of TDerivedType35, TResult),
        matchFunc36 As Func(Of TDerivedType36, TResult), matchFunc37 As Func(Of TDerivedType37, TResult), matchFunc38 As Func(Of TDerivedType38, TResult), matchFunc39 As Func(Of TDerivedType39, TResult), matchFunc40 As Func(Of TDerivedType40, TResult), Optional defaultFunc As Func(Of TBaseType, TResult) = Nothing) As TResult
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
        ElseIf TypeOf obj Is TDerivedType8 Then
            Return matchFunc8(DirectCast(obj, TDerivedType8))
        ElseIf TypeOf obj Is TDerivedType9 Then
            Return matchFunc9(DirectCast(obj, TDerivedType9))
        ElseIf TypeOf obj Is TDerivedType10 Then
            Return matchFunc10(DirectCast(obj, TDerivedType10))
        ElseIf TypeOf obj Is TDerivedType11 Then
            Return matchFunc11(DirectCast(obj, TDerivedType11))
        ElseIf TypeOf obj Is TDerivedType12 Then
            Return matchFunc12(DirectCast(obj, TDerivedType12))
        ElseIf TypeOf obj Is TDerivedType13 Then
            Return matchFunc13(DirectCast(obj, TDerivedType13))
        ElseIf TypeOf obj Is TDerivedType14 Then
            Return matchFunc14(DirectCast(obj, TDerivedType14))
        ElseIf TypeOf obj Is TDerivedType15 Then
            Return matchFunc15(DirectCast(obj, TDerivedType15))
        ElseIf TypeOf obj Is TDerivedType16 Then
            Return matchFunc16(DirectCast(obj, TDerivedType16))
        ElseIf TypeOf obj Is TDerivedType17 Then
            Return matchFunc17(DirectCast(obj, TDerivedType17))
        ElseIf TypeOf obj Is TDerivedType18 Then
            Return matchFunc18(DirectCast(obj, TDerivedType18))
        ElseIf TypeOf obj Is TDerivedType19 Then
            Return matchFunc19(DirectCast(obj, TDerivedType19))
        ElseIf TypeOf obj Is TDerivedType20 Then
            Return matchFunc20(DirectCast(obj, TDerivedType20))
        ElseIf TypeOf obj Is TDerivedType21 Then
            Return matchFunc21(DirectCast(obj, TDerivedType21))
        ElseIf TypeOf obj Is TDerivedType22 Then
            Return matchFunc22(DirectCast(obj, TDerivedType22))
        ElseIf TypeOf obj Is TDerivedType23 Then
            Return matchFunc23(DirectCast(obj, TDerivedType23))
        ElseIf TypeOf obj Is TDerivedType24 Then
            Return matchFunc24(DirectCast(obj, TDerivedType24))
        ElseIf TypeOf obj Is TDerivedType25 Then
            Return matchFunc25(DirectCast(obj, TDerivedType25))
        ElseIf TypeOf obj Is TDerivedType26 Then
            Return matchFunc26(DirectCast(obj, TDerivedType26))
        ElseIf TypeOf obj Is TDerivedType27 Then
            Return matchFunc27(DirectCast(obj, TDerivedType27))
        ElseIf TypeOf obj Is TDerivedType28 Then
            Return matchFunc28(DirectCast(obj, TDerivedType28))
        ElseIf TypeOf obj Is TDerivedType29 Then
            Return matchFunc29(DirectCast(obj, TDerivedType29))
        ElseIf TypeOf obj Is TDerivedType30 Then
            Return matchFunc30(DirectCast(obj, TDerivedType30))
        ElseIf TypeOf obj Is TDerivedType31 Then
            Return matchFunc31(DirectCast(obj, TDerivedType31))
        ElseIf TypeOf obj Is TDerivedType32 Then
            Return matchFunc32(DirectCast(obj, TDerivedType32))
        ElseIf TypeOf obj Is TDerivedType33 Then
            Return matchFunc33(DirectCast(obj, TDerivedType33))
        ElseIf TypeOf obj Is TDerivedType34 Then
            Return matchFunc34(DirectCast(obj, TDerivedType34))
        ElseIf TypeOf obj Is TDerivedType35 Then
            Return matchFunc35(DirectCast(obj, TDerivedType35))
        ElseIf TypeOf obj Is TDerivedType36 Then
            Return matchFunc36(DirectCast(obj, TDerivedType36))
        ElseIf TypeOf obj Is TDerivedType37 Then
            Return matchFunc37(DirectCast(obj, TDerivedType37))
        ElseIf TypeOf obj Is TDerivedType38 Then
            Return matchFunc38(DirectCast(obj, TDerivedType38))
        ElseIf TypeOf obj Is TDerivedType39 Then
            Return matchFunc39(DirectCast(obj, TDerivedType39))
        ElseIf TypeOf obj Is TDerivedType40 Then
            Return matchFunc40(DirectCast(obj, TDerivedType40))
        ElseIf defaultFunc IsNot Nothing Then
            Return defaultFunc(obj)
        Else
            Return Nothing
        End If
    End Function

#End Region

End Module
