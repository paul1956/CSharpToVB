' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

Imports System.Diagnostics.CodeAnalysis
Imports System.Runtime.CompilerServices

Imports Microsoft.CodeAnalysis

Public Module MethodKindExtensions

    <ExcludeFromCodeCoverage>
    <Extension>
    Public Function IsPropertyAccessor(kind As MethodKind) As Boolean
        Return kind = MethodKind.PropertyGet OrElse kind = MethodKind.PropertySet
    End Function

End Module