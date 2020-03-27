' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices

Imports Microsoft.CodeAnalysis

Public Module TypeInfoExtensions
    <Extension>
    Friend Function IsString(_Typeinfo As TypeInfo) As Boolean
        Dim typeSymbol As ITypeSymbol = _Typeinfo.Type

        If typeSymbol Is Nothing OrElse typeSymbol.IsErrorType Then
            Return False
        End If

        If typeSymbol.ToString.Replace("?", "", StringComparison.OrdinalIgnoreCase).Equals("string", StringComparison.OrdinalIgnoreCase) Then
            Return True
        End If

        Return False
    End Function

End Module
