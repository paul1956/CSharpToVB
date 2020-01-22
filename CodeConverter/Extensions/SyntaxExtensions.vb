' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBCodeConverter.Util

    Public Module SyntaxExtensions

        <Extension()>
        Public Function IsParentKind(node As SyntaxNode, kind As CS.SyntaxKind) As Boolean
            Return node IsNot Nothing AndAlso node.Parent.IsKind(kind)
        End Function

        ''' <summary>
        ''' Creates a new syntax node with all whitespace and end of line trivia replaced with
        ''' regularly formatted trivia.        ''' </summary>
        ''' <typeparam name="TNode">>The type of the node.</typeparam>
        ''' <param name="node">The node to format.</param>
        ''' <param name="useDefaultCasing"></param>
        ''' <param name="indentation">An optional sequence of whitespace characters that defines a single level of indentation.</param>
        ''' <param name="elasticTrivia"></param>
        ''' <param name="PreserveCRLF"></param>
        ''' <returns></returns>
        <Extension()>
        Public Function NormalizeWhitespaceEx(Of TNode As SyntaxNode)(node As TNode,
                                                                      useDefaultCasing As Boolean,
                                                                      Optional indentation As String = "    ",
                                                                      Optional eol As String = vbCrLf,
                                                                      Optional PreserveCRLF As Boolean = False
                                                                      ) As TNode
            Contracts.Contract.Requires(node IsNot Nothing)
            Return DirectCast(VBS.SyntaxNormalizer.Normalize(node, indentation, eol, useElasticTrivia:=False, useDefaultCasing, PreserveCRLF), TNode)
        End Function

    End Module
End Namespace
