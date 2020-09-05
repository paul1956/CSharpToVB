﻿' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Public Structure ProgressReport
    Implements IEquatable(Of ProgressReport)

    Public Sub New(Current As Integer, Maximum As Integer)
        Me.Current = Current
        Me.Maximum = Maximum
    End Sub

    Public ReadOnly Property Current As Integer
    Public ReadOnly Property Maximum As Integer

    Public Overrides Function Equals(obj As Object) As Boolean
        Return (TypeOf obj Is ProgressReport) AndAlso Me.Equals(DirectCast(obj, ProgressReport))
    End Function

    Public Overloads Function Equals(other As ProgressReport) As Boolean Implements IEquatable(Of ProgressReport).Equals
        Return Current = other.Current AndAlso Maximum = other.Maximum
    End Function

#If NET48 Then
    Public Overrides Function GetHashCode() As Integer
        Return (Current, Maximum).GetHashCode()
    End Function
#Else

    Public Overrides Function GetHashCode() As Integer
        Return HashCode.Combine(Current, Maximum)
    End Function

#End If

    Public Shared Operator =(left As ProgressReport, right As ProgressReport) As Boolean
        Return left.Equals(right)
    End Operator

    Public Shared Operator <>(left As ProgressReport, right As ProgressReport) As Boolean
        Return Not left = right
    End Operator

End Structure
