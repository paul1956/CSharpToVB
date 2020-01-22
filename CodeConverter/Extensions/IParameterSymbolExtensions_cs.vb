' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices

Imports Microsoft.CodeAnalysis

Public Module IParameterSymbolExtensions

    <Extension>
    Public Function IsRefOrOut(symbol As IParameterSymbol) As Boolean
        If symbol Is Nothing Then
            Throw New ArgumentNullException(NameOf(symbol))
        End If
        Return symbol.RefKind <> RefKind.None
    End Function

End Module
