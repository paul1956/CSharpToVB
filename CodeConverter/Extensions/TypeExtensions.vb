' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports Microsoft.CodeAnalysis
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBConverter

    Public Module TypeExtensions

        ''' <summary>
        ''' Gets all base classes and interfaces.
        ''' </summary>
        ''' <returns>All classes and interfaces.</returns>
        ''' <param name="type">Type.</param>
        <Extension>
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

        <Extension>
        Friend Function GetElementType(typeSyntax As VBS.TypeSyntax) As VBS.TypeSyntax
            If TypeOf typeSyntax Is VBS.ArrayTypeSyntax Then
                typeSyntax = CType(typeSyntax, VBS.ArrayTypeSyntax).ElementType
            End If
            Return typeSyntax
        End Function

    End Module
End Namespace
