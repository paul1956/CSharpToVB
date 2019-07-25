' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

Imports System.Diagnostics.CodeAnalysis
Imports Microsoft.CodeAnalysis

Partial Friend Class SymbolEquivalenceComparer

    <ExcludeFromCodeCoverage>
    Friend Class SignatureTypeSymbolEquivalenceComparer
        Implements IEqualityComparer(Of ITypeSymbol)

        Private ReadOnly _symbolEquivalenceComparer As SymbolEquivalenceComparer

        <ExcludeFromCodeCoverage>
        Public Sub New(symbolEquivalenceComparer As SymbolEquivalenceComparer)
            Me._symbolEquivalenceComparer = symbolEquivalenceComparer
        End Sub

        <ExcludeFromCodeCoverage>
        Public Shadows Function Equals(x As ITypeSymbol, y As ITypeSymbol) As Boolean Implements IEqualityComparer(Of ITypeSymbol).Equals
            Return Me.Equals(x, y, Nothing)
        End Function

        <ExcludeFromCodeCoverage>
        Public Shadows Function Equals(x As ITypeSymbol, y As ITypeSymbol, equivalentTypesWithDifferingAssemblies As Dictionary(Of INamedTypeSymbol, INamedTypeSymbol)) As Boolean
            Return Me._symbolEquivalenceComparer.GetEquivalenceVisitor(compareMethodTypeParametersByIndex:=True, objectAndDynamicCompareEqually:=True).AreEquivalent(x, y, equivalentTypesWithDifferingAssemblies)
        End Function

        <ExcludeFromCodeCoverage>
        Public Shadows Function GetHashCode(x As ITypeSymbol) As Integer Implements IEqualityComparer(Of ITypeSymbol).GetHashCode
            Return Me._symbolEquivalenceComparer.GetGetHashCodeVisitor(compareMethodTypeParametersByIndex:=True, objectAndDynamicCompareEqually:=True).GetHashCode(x, currentHash:=0)
        End Function

    End Class

End Class