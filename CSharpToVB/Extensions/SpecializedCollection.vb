﻿' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.Runtime.CompilerServices

Partial Public Module SpecializedCollection

    <Extension>
    Public Function Last(ByVal StringCollection As Specialized.StringCollection) As String

        Return If(StringCollection Is Nothing OrElse StringCollection.Count = 0, "", StringCollection.Item(StringCollection.Count - 1))
    End Function

End Module
