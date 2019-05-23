' Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.
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