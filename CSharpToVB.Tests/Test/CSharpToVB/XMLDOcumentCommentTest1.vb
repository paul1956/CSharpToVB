' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports CodeConverter.Tests

Imports Xunit

Namespace CSharpToVB.Tests

    <TestClass()> Public Class XMLDocumentCommentTest1
        Inherits ConverterTestBase

        Private Const CsharpCode As String = "// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System.Collections.Immutable;
using Microsoft.CodeAnalysis.CSharp.Extensions;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Diagnostics.PreferFrameworkType;

namespace Microsoft.CodeAnalysis.CSharp.Diagnostics.Analyzers
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    internal class CSharpPreferFrameworkTypeDiagnosticAnalyzer :
        PreferFrameworkTypeDiagnosticAnalyzerBase<SyntaxKind, ExpressionSyntax, PredefinedTypeSyntax>
    {
        protected override ImmutableArray<SyntaxKind> SyntaxKindsOfInterest =>
            ImmutableArray.Create(SyntaxKind.PredefinedType);

        ///<remarks>
        /// every predefined type keyword except `void` can be replaced by its framework type in code.
        ///</remarks>
        protected override bool IsPredefinedTypeReplaceableWithFrameworkType(PredefinedTypeSyntax node) =>
            node.Keyword.Kind() != SyntaxKind.VoidKeyword;

        protected override bool IsInMemberAccessOrCrefReferenceContext(ExpressionSyntax node) =>
            node.IsInMemberAccessContext() || node.InsideCrefReference();

        protected override string GetLanguageName() => LanguageNames.CSharp;
    }
}
"

        Private Const DesiredResult As String = "' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit Off
Option Infer On
Option Strict Off

Imports System.Collections.Immutable
Imports Microsoft.CodeAnalysis.CSharp.Extensions
Imports Microsoft.CodeAnalysis.CSharp.Syntax
Imports Microsoft.CodeAnalysis.Diagnostics
Imports Microsoft.CodeAnalysis.Diagnostics.PreferFrameworkType

Namespace Microsoft.CodeAnalysis.CSharp.Diagnostics.Analyzers

    <DiagnosticAnalyzer(LanguageNames.CSharp)>
    Friend Class CSharpPreferFrameworkTypeDiagnosticAnalyzer
        Inherits PreferFrameworkTypeDiagnosticAnalyzerBase(Of SyntaxKind, ExpressionSyntax, PredefinedTypeSyntax)

        Protected Overrides ReadOnly Property SyntaxKindsOfInterest As ImmutableArray(Of SyntaxKind)
            Get
                Return ImmutableArray.Create(SyntaxKind.PredefinedType)
            End Get
        End Property

        ''' <remarks>
        ''' every predefined type keyword except `void` can be replaced by its framework type in code.
        ''' </remarks>
        Protected Overrides Function IsPredefinedTypeReplaceableWithFrameworkType(node As PredefinedTypeSyntax) As Boolean
            Return node.Keyword.Kind() <> SyntaxKind.VoidKeyword
        End Function

        Protected Overrides Function IsInMemberAccessOrCrefReferenceContext(node As ExpressionSyntax) As Boolean
            Return node.IsInMemberAccessContext() OrElse node.InsideCrefReference()
        End Function

        Protected Overrides Function GetLanguageName() As String
            Return LanguageNames.CSharp
        End Function
    End Class
End Namespace
"

        <Fact>
        Public Shared Sub CSharpToVBMalformedDocumentComments()
            TestConversionCSharpToVisualBasic(CsharpCode, DesiredResult)
        End Sub

    End Class

End Namespace
