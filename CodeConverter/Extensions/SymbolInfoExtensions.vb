' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices

Imports Microsoft.CodeAnalysis

Namespace CSharpToVBConverter

    Public Module SymbolInfoExtensions

        <Extension>
        Friend Function GetAnySymbol(info As SymbolInfo) As ISymbol
            Return If(info.Symbol, info.CandidateSymbols.FirstOrDefault())
        End Function

    End Module
End Namespace
