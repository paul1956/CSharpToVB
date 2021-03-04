' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Public Structure ColorDescriptor
    Public Property Foreground As Color
    Public Property Background As Color

    Public Sub New(foreground As Color, background As Color)
        Me.Foreground = foreground
        Me.Background = background
    End Sub

    Public Overrides Function Equals(obj As Object) As Boolean
        If Not (TypeOf obj Is ColorDescriptor) Then
            Return False
        End If

        Dim other As ColorDescriptor = DirectCast(obj, ColorDescriptor)
        Return Me.Foreground.A = other.Foreground.A AndAlso
            Me.Foreground.R = other.Foreground.R AndAlso
            Me.Foreground.G = other.Foreground.G AndAlso
            Me.Foreground.B = other.Foreground.B AndAlso
            Me.Background.A = other.Background.A AndAlso
            Me.Background.R = other.Background.R AndAlso
            Me.Background.G = other.Background.G AndAlso
            Me.Background.B = other.Background.B
    End Function

    Public Overrides Function GetHashCode() As Integer
        Return HashCode.Combine(Me.Foreground, Me.Background)
    End Function

    Public Shared Operator =(left As ColorDescriptor, right As ColorDescriptor) As Boolean
        Return left.Equals(right)
    End Operator

    Public Shared Operator <>(left As ColorDescriptor, right As ColorDescriptor) As Boolean
        Return Not left = right
    End Operator

End Structure
