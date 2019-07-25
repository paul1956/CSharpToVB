﻿' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

Imports System.Diagnostics.CodeAnalysis
Imports System.Runtime.CompilerServices

Imports Microsoft.CodeAnalysis

Public Module IParameterSymbolExtensions

    <Extension>
    <ExcludeFromCodeCoverage>
    Public Function IsRefOrOut(symbol As IParameterSymbol) As Boolean
        Return symbol.RefKind <> RefKind.None
    End Function

End Module