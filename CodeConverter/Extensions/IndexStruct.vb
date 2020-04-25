' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

#Disable Warning CA1815 ' Override equals and operator equals on value types

Public Structure IndexStruct(Of T)
    Public Property Index As Integer                                                           ' first element has index = 0
    Public Property IsFirst As Boolean
    Public Property IsLast As Boolean
    Public Property MinIndex As Integer
    Public Property Value As T
End Structure

#Enable Warning CA1815 ' Override equals and operator equals on value types
