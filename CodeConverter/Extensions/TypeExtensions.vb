' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports System.Text
Imports Microsoft.CodeAnalysis

Namespace CSharpToVBConverter

    Module TypeExtensions

        ''' <summary>
        ''' Gets all base classes and interfaces.
        ''' </summary>
        ''' <returns>All classes and interfaces.</returns>
        ''' <param name="type">Type.</param>
        <Extension()>
        Friend Iterator Function GetAllBaseClassesAndInterfaces(type As INamedTypeSymbol, Optional includeSuperType As Boolean = False) As IEnumerable(Of INamedTypeSymbol)
            If Not includeSuperType Then
                type = type.BaseType
            End If
            Dim curType As INamedTypeSymbol = type
            While curType IsNot Nothing
                Yield curType
                curType = curType.BaseType
            End While

            For Each inter As INamedTypeSymbol In type.AllInterfaces
                Yield inter
            Next
        End Function

        ''' <summary>
        ''' TODO: Eradicate this in favor of CommonConversions.GetFullyQualifiedNameSyntax
        ''' Gets the full name of the metadata.
        ''' In case symbol is not INamedTypeSymbol it returns raw MetadataName
        ''' Example: Generic type returns T1, T2...
        ''' </summary>
        ''' <returns>The full metadata name.</returns>
        ''' <param name="symbol">Symbol.</param>
        <Extension()>
        Friend Function GetFullMetadataName(symbol As ITypeSymbol) As String
            Dim TempVar1 As Boolean = TypeOf symbol Is IArrayTypeSymbol
            Dim ats As IArrayTypeSymbol = CType(symbol, IArrayTypeSymbol)
            If TempVar1 Then
                Return GetFullMetadataName(ats.ElementType) & "[" & New String(Enumerable.Repeat(","c, ats.Rank - 1).ToArray()) & "]"

            End If
            'This is for compatibility with NR5 reflection name in case of generic types like T1, T2...
            Dim namedTypeSymbol As INamedTypeSymbol = TryCast(symbol, INamedTypeSymbol)
            Return If(namedTypeSymbol IsNot Nothing, GetFullMetadataName(namedTypeSymbol), symbol.MetadataName)
        End Function

        ''' <summary>
        ''' TODO: Eradicate this in favor of CommonConversions.GetFullyQualifiedNameSyntax
        ''' Gets the full MetadataName(ReflectionName in NR5).
        ''' Example: Namespace1.Namespace2.Classs1+NestedClassWithTwoGenericTypes`2+NestedClassWithoutGenerics
        ''' </summary>
        ''' <returns>The full metadata name.</returns>
        ''' <param name="symbol">Symbol.</param>
        <Extension()>
        Friend Function GetFullMetadataName(symbol As INamedTypeSymbol) As String
            Dim fullName As StringBuilder = New StringBuilder(symbol.MetadataName)
            Dim parentType As INamedTypeSymbol = symbol.ContainingType
            While parentType IsNot Nothing
                fullName.Insert(0, "+"c)
                fullName.Insert(0, parentType.MetadataName)
                parentType = parentType.ContainingType
            End While

            Return GetFullMetadataName(symbol.ContainingNamespace, fullName)
        End Function

        <Extension()>
        Friend Function GetFullMetadataName(ns As INamespaceSymbol, Optional sb As StringBuilder = Nothing) As String
            sb = If(sb, New StringBuilder)
            While ns IsNot Nothing AndAlso Not ns.IsGlobalNamespace
                If sb.Length > 0 Then
                    sb.Insert(0, "."c)
                End If
                sb.Insert(0, ns.MetadataName)
                ns = ns.ContainingNamespace
            End While

            Return sb.ToString()
        End Function

    End Module
End Namespace
