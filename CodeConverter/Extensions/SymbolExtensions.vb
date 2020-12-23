' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Collections.Immutable
Imports System.Runtime.CompilerServices
Imports Microsoft.CodeAnalysis

Namespace CSharpToVBConverter
    Friend Module SymbolExtensions

        <Extension>
        Public Function GetParameters(symbol As ISymbol) As ImmutableArray(Of IParameterSymbol)
            If symbol Is Nothing Then
                Throw New ArgumentNullException(NameOf(symbol))
            End If
            Dim method As IMethodSymbol = TryCast(symbol, IMethodSymbol)
            If method IsNot Nothing Then
                Return method.Parameters
            End If
            Dim [property] As IPropertySymbol = TryCast(symbol, IPropertySymbol)
            If [property] IsNot Nothing Then
                Return [property].Parameters
            End If
            Dim ev As IEventSymbol = TryCast(symbol, IEventSymbol)
            If ev IsNot Nothing Then
                Return GetDelegateInvokeMethod(ev.Type).Parameters
            End If
            Return ImmutableArray(Of IParameterSymbol).Empty
        End Function

    End Module
End Namespace
