' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CSharp

Namespace CSharpToVBConverter
    Public Module CSharpSyntaxFactory
        Public ReadOnly CSCloseBraceToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.CloseBraceToken)
        Public ReadOnly CSReadOnlyKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.ReadOnlyKeyword)
        Public ReadOnly CSEmptySpaceTrivia As SyntaxTrivia = SyntaxFactory.Whitespace(String.Empty)

    End Module
End Namespace
