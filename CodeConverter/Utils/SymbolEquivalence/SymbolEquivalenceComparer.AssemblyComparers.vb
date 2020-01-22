' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Diagnostics.CodeAnalysis

Imports Microsoft.CodeAnalysis

Partial Friend Class SymbolEquivalenceComparer

    <ExcludeFromCodeCoverage>
    Private NotInheritable Class SimpleNameAssemblyComparer
        Implements IEqualityComparer(Of IAssemblySymbol)

        Public Shared ReadOnly Instance As IEqualityComparer(Of IAssemblySymbol) = New SimpleNameAssemblyComparer()

        Public Shadows Function Equals(x As IAssemblySymbol, y As IAssemblySymbol) As Boolean Implements IEqualityComparer(Of IAssemblySymbol).Equals
            Return AssemblyIdentityComparer.SimpleNameComparer.Equals(x.Name, y.Name)
        End Function

        Public Shadows Function GetHashCode(obj As IAssemblySymbol) As Integer Implements IEqualityComparer(Of IAssemblySymbol).GetHashCode
            Return AssemblyIdentityComparer.SimpleNameComparer.GetHashCode(obj.Name)
        End Function

    End Class

End Class
