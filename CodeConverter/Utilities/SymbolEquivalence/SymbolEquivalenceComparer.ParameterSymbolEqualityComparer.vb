' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Diagnostics.CodeAnalysis

Imports Microsoft.CodeAnalysis

Namespace CSharpToVBConverter

    Partial Friend Class SymbolEquivalenceComparer

        <ExcludeFromCodeCoverage>
        Friend Shared Function AreRefKindsEquivalent(rk1 As RefKind, rk2 As RefKind, distinguishRefFromOut As Boolean) As Boolean
            Return If(distinguishRefFromOut, rk1 = rk2, rk1 = RefKind.None = (rk2 = RefKind.None))
        End Function

        <ExcludeFromCodeCoverage>
        Friend Class ParameterSymbolEqualityComparer
            Implements IEqualityComparer(Of IParameterSymbol)

            Private ReadOnly _distinguishRefFromOut As Boolean
            Private ReadOnly _symbolEqualityComparer As SymbolEquivalenceComparer

            Friend Sub New(symbolEqualityComparer As SymbolEquivalenceComparer, distinguishRefFromOut As Boolean)
                _symbolEqualityComparer = symbolEqualityComparer
                _distinguishRefFromOut = distinguishRefFromOut
            End Sub

            Public Shadows Function Equals(x As IParameterSymbol, y As IParameterSymbol, equivalentTypesWithDifferingAssemblies As Dictionary(Of INamedTypeSymbol, INamedTypeSymbol), compareParameterName As Boolean, isCaseSensitive As Boolean) As Boolean
                If ReferenceEquals(x, y) Then
                    Return True
                End If

                If x Is Nothing OrElse y Is Nothing Then
                    Return False
                End If

                Dim nameComparisonCheck As Boolean = True
                If compareParameterName Then
                    nameComparisonCheck = If(isCaseSensitive, x.Name = y.Name, String.Equals(x.Name, y.Name, StringComparison.OrdinalIgnoreCase))
                End If

                ' See the comment in the outer type.  If we're comparing two parameters for
                ' equality, then we want to consider method type parameters by index only.

                Return AreRefKindsEquivalent(x.RefKind, y.RefKind, _distinguishRefFromOut) AndAlso nameComparisonCheck AndAlso _symbolEqualityComparer.GetEquivalenceVisitor().AreEquivalent(x.CustomModifiers, y.CustomModifiers, equivalentTypesWithDifferingAssemblies) AndAlso _symbolEqualityComparer.SignatureTypeEquivalenceComparer.Equals(x.Type, y.Type, equivalentTypesWithDifferingAssemblies)
            End Function

            Public Shadows Function Equals(x As IParameterSymbol, y As IParameterSymbol) As Boolean Implements IEqualityComparer(Of IParameterSymbol).Equals
                Return Me.Equals(x, y, Nothing, False, False)
            End Function

            Public Shadows Function Equals(x As IParameterSymbol, y As IParameterSymbol, compareParameterName As Boolean, isCaseSensitive As Boolean) As Boolean
                Return Me.Equals(x, y, Nothing, compareParameterName, isCaseSensitive)
            End Function

            Public Shadows Function GetHashCode(x As IParameterSymbol) As Integer Implements IEqualityComparer(Of IParameterSymbol).GetHashCode
                If x Is Nothing Then
                    Return 0
                End If

                Return x.GetHashCode
            End Function

        End Class

    End Class

End Namespace
