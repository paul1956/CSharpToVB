' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic

Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace Extensions

    Public Module ParameterSyntaxExtensions

        <Extension>
        Friend Function RemoveModifier(parameter As VBS.ParameterSyntax, modifierKind As SyntaxKind) As VBS.ParameterSyntax
            Dim returnTokenList As New SyntaxTokenList
            If parameter.Modifiers.Any Then
                For Each token As SyntaxToken In parameter.Modifiers
                    If token.IsKind(modifierKind) Then
                        Continue For
                    End If
                    returnTokenList = returnTokenList.Add(token)
                Next
            End If
            Return parameter.WithModifiers(returnTokenList)
        End Function

    End Module
End Namespace
