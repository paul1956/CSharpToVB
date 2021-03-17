' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CSharp

Namespace Utilities
    Public Module CSharpSyntaxFactory
        Public ReadOnly s_csReadOnlyKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.ReadOnlyKeyword)
        Public ReadOnly s_csEmptySpaceTrivia As SyntaxTrivia = SyntaxFactory.Whitespace(String.Empty)

    End Module
End Namespace
