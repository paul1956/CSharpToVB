' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports Microsoft.CodeAnalysis
Imports VB = Microsoft.CodeAnalysis.VisualBasic

Friend Module TokenListExtensions

    <Extension>
    Friend Function Contains(tokens As SyntaxTokenList, kind As CSharp.SyntaxKind) As Boolean
        Return tokens.Contains(Function(m As SyntaxToken) m.IsKind(kind))
    End Function

    <Extension>
    Friend Function Contains(tokens As IEnumerable(Of SyntaxToken), ParamArray kind() As VB.SyntaxKind) As Boolean
        Return tokens.Contains(Function(m As SyntaxToken) m.IsKind(kind))
    End Function

    <Extension>
    Friend Function IndexOf(tokens As IEnumerable(Of SyntaxToken), kind As VB.SyntaxKind) As Integer
        For i As Integer = 0 To tokens.Count - 1
            If tokens(i).IsKind(kind) Then
                Return i
            End If
        Next
        Return -1
    End Function

End Module
