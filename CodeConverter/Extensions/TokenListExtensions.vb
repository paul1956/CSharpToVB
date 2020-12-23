' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports Microsoft.CodeAnalysis
Imports VB = Microsoft.CodeAnalysis.VisualBasic

Namespace CSharpToVBConverter
    Friend Module TokenListExtensions

        <Extension>
        Friend Function Contains(Tokens As SyntaxTokenList, Kind As CSharp.SyntaxKind) As Boolean
            Return Tokens.Contains(Function(m As SyntaxToken) m.IsKind(Kind))
        End Function

        <Extension>
        Friend Function Contains(Tokens As IEnumerable(Of SyntaxToken), ParamArray Kind() As VB.SyntaxKind) As Boolean
            Return Tokens.Contains(Function(m As SyntaxToken) m.IsKind(Kind))
        End Function

        <Extension>
        Friend Function IndexOf(Tokens As IEnumerable(Of SyntaxToken), Kind As VB.SyntaxKind) As Integer
            For i As Integer = 0 To Tokens.Count - 1
                If Tokens(i).IsKind(Kind) Then
                    Return i
                End If
            Next
            Return -1
        End Function

    End Module
End Namespace
