' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.Runtime.CompilerServices

Imports Microsoft.CodeAnalysis

Imports VB = Microsoft.CodeAnalysis.VisualBasic

Module SyntaxFactsExtensions

    Private Function IsIdentifier(token As SyntaxToken) As Boolean
        Return token.RawKind = VB.SyntaxKind.IdentifierToken
    End Function

    ''' <summary>
    ''' Determines if the token represents a reserved or contextual keyword
    ''' </summary>
    ''' <param name="token">The source SyntaxToken.</param>
    ''' <returns>A boolean value True if token is a keyword.</returns>
    <Extension>
    Public Function IsKeyword(token As SyntaxToken) As Boolean
        Return VB.SyntaxFacts.IsKeywordKind(CType(token.RawKind(), VB.SyntaxKind))
    End Function

    Public Function IsNumericLiteral(token As SyntaxToken) As Boolean
        Return token.RawKind = VB.SyntaxKind.DecimalLiteralToken OrElse
                   token.RawKind = VB.SyntaxKind.FloatingLiteralToken OrElse
                   token.RawKind = VB.SyntaxKind.IntegerLiteralToken
    End Function

    Public Function IsNumericLiteralExpression(node As SyntaxNode) As Boolean
        Return If(node Is Nothing, False, node.IsKind(VB.SyntaxKind.NumericLiteralExpression))
    End Function

    ''' <summary>
    ''' Determines if the token  represents a preprocessor keyword
    ''' </summary>
    ''' <param name="token">The source SyntaxToken.</param>
    ''' <returns> A boolean value True if token is a preprocessor keyword.</returns>
    <Extension>
    Public Function IsPreprocessorKeyword(token As SyntaxToken) As Boolean
        Return VB.SyntaxFacts.IsPreprocessorKeyword(CType(token.RawKind, VB.SyntaxKind))
    End Function

    Private Function IsWord(token As SyntaxToken) As Boolean
        Return IsIdentifier(token) _
               OrElse IsKeyword(token) _
               OrElse VB.SyntaxFacts.IsContextualKeyword(CType(token.RawKind, VB.SyntaxKind)) _
               OrElse VB.SyntaxFacts.IsPreprocessorKeyword(CType(token.RawKind, VB.SyntaxKind))
    End Function

    Public Function IsWordOrNumber(token As SyntaxToken) As Boolean
        Return IsWord(token) OrElse IsNumericLiteral(token)
    End Function

End Module