' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports Microsoft.CodeAnalysis

Public Module CSharpSyntaxFactory
    Public ReadOnly CloseBraceToken As SyntaxToken = CSharp.SyntaxFactory.Token(CSharp.SyntaxKind.CloseBraceToken)
    Public ReadOnly ReadOnlyKeyword As SyntaxToken = CSharp.SyntaxFactory.Token(CSharp.SyntaxKind.ReadOnlyKeyword)
    Public ReadOnly CS_SpaceTrivia As SyntaxTrivia = CSharp.SyntaxFactory.Whitespace(" ")
    Public ReadOnly CS_Space As SyntaxTrivia = CSharp.SyntaxFactory.Space
End Module
