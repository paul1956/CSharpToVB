' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Collections.Immutable
Imports System.ComponentModel
Imports System.Runtime.CompilerServices
Imports CSharpToVBConverter
Imports Microsoft.CodeAnalysis

Namespace CSharpToVBConverter

    <EditorBrowsable(EditorBrowsableState.Never)>
    Public Module ITypeSymbolExtensions

        <Extension>
        Friend Function IsDelegateType(symbol As ITypeSymbol) As Boolean
            If symbol Is Nothing Then
                Return False
            End If
            Return symbol.TypeKind = TypeKind.Delegate
        End Function

        <Extension>
        Friend Function IsErrorType(symbol As ITypeSymbol) As Boolean
            Return CBool(symbol?.TypeKind = TypeKind.Error)
        End Function

        <Extension>
        Friend Function IsInterfaceType(symbol As ITypeSymbol) As Boolean
            If symbol Is Nothing Then
                Return False
            End If

            Return symbol.TypeKind = TypeKind.Interface
        End Function

    End Module
End Namespace
