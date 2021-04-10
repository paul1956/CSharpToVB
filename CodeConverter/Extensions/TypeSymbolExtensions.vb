' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.ComponentModel
Imports System.Runtime.CompilerServices
Imports System.Text
Imports Microsoft.CodeAnalysis
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace Extensions

    <EditorBrowsable(EditorBrowsableState.Never)>
    Public Module TypeSymbolExtensions

        <Extension>
        Private Function GetFullMetadataName(ns As INamespaceSymbol) As String
            Dim sb As New StringBuilder
            While ns IsNot Nothing AndAlso Not ns.IsGlobalNamespace
                If sb.Length > 0 Then
                    sb.Insert(0, "."c)
                End If
                sb.Insert(0, ns.MetadataName)
                ns = ns.ContainingNamespace
            End While

            Return sb.ToString()
        End Function

        <Extension>
        Friend Function GetFullyQualifiedNameSyntax(symbol As INamespaceOrTypeSymbol) As VBS.NameSyntax
            Select Case True
                Case TypeOf symbol Is ITypeSymbol
                    Dim typeSyntax As VBS.TypeSyntax = CType(symbol, ITypeSymbol).ConvertToType.GetElementType
                    If TypeOf typeSyntax Is VBS.PredefinedTypeSyntax Then
                        typeSyntax = Factory.IdentifierName($"[{symbol}]")
                    End If
                    Dim nullableType As VBS.NullableTypeSyntax = TryCast(typeSyntax, VBS.NullableTypeSyntax)
                    If nullableType IsNot Nothing Then
                        typeSyntax = nullableType.ElementType
                    End If
                    Return CType(typeSyntax, VBS.NameSyntax)
                Case TypeOf symbol Is INamespaceSymbol
                    Dim ns As INamespaceSymbol = CType(symbol, INamespaceSymbol)
                    Return Factory.ParseName(ns.GetFullMetadataName())
                Case Else
                    Throw New NotImplementedException($"Fully qualified name for {symbol.[GetType]().FullName} not implemented")
            End Select
        End Function

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
