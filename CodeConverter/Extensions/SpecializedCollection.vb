﻿' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.Runtime.CompilerServices

Partial Public Module SpecializedCollection

    <Extension>
    Public Function SingletonEnumerable(Of T)(value As T) As IEnumerable(Of T)
        Return New SingletonList(Of T)(value)
    End Function

End Module
