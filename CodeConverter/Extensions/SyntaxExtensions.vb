' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports Microsoft.CodeAnalysis
Imports CS = Microsoft.CodeAnalysis.CSharp

Public Module SyntaxExtensions

    <Extension>
    Friend Function IsParentKind(node As SyntaxNode, kind As CS.SyntaxKind) As Boolean
        Return node IsNot Nothing AndAlso node.Parent.IsKind(kind)
    End Function

    ''' <Summary>
    ''' Creates a new syntax node with all whitespace and end of line trivia replaced with
    ''' regularly formatted trivia
    ''' </Summary>
    ''' <typeparam name="TNode">>The type of the node.</typeparam>
    ''' <param name="node">The node to format.</param>
    ''' <param name="useDefaultCasing"></param>
    ''' <param name="indentation">An optional sequence of whitespace characters that defines a single level of indentation.</param>
    ''' <param name="eol"></param>
    ''' <param name="preserveCRLF"></param>
    ''' <returns></returns>
    <Extension>
    Public Function NormalizeWhitespaceEx(Of TNode As SyntaxNode)(node As TNode,
                                                                  useDefaultCasing As Boolean,
                                                                  Optional indentation As String = "    ",
                                                                  Optional eol As String = vbCrLf,
                                                                  Optional preserveCRLF As Boolean = False
                                                                 ) As TNode
        If node Is Nothing Then
            Throw New ArgumentNullException(NameOf(node))
        End If

        Return DirectCast(SyntaxNormalizer.Normalize(node, indentation, eol, useElasticTrivia:=False, useDefaultCasing, preserveCRLF), TNode)
    End Function

End Module
