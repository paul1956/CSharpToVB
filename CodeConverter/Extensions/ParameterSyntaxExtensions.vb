' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

Imports System.Runtime.CompilerServices

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

Public Module ParameterSyntaxExtensions

    <Extension>
    Friend Function RemoveModifier(Parameter As ParameterSyntax, ModifierKind As SyntaxKind) As ParameterSyntax
        Dim ReturnTokenList As New SyntaxTokenList
        If Parameter.Modifiers.Any Then
            For Each token As SyntaxToken In Parameter.Modifiers
                If token.IsKind(ModifierKind) Then
                    Continue For
                End If
                ReturnTokenList = ReturnTokenList.Add(token)
            Next
        End If
        Return Parameter.WithModifiers(ReturnTokenList)
    End Function

End Module
