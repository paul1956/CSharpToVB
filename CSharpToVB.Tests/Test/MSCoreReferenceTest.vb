' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CSharp
Imports Xunit

Namespace MSCoreReference.Tests
    Public Class MSCoreReferenceTest

        Public Shared ReadOnly Property CodeCoverageEnabled() As Boolean
            Get
                Return False
            End Get
        End Property

        <ConditionalFact(NameOf(CodeCoverageEnabled))>
        <PlatformSpecific(TestPlatforms.Windows)>
        Sub VerifyReferencesExist()
            Dim tree As SyntaxTree = CSharpSyntaxTree.ParseText("using System;
class test : IComparable { }")

            Dim compilation As CSharpCompilation = CSharpCompilation.Create("MyCompilation", syntaxTrees:={tree}, references:=SharedReferences.CSharpReferences)
            Dim lSemanticModel As SemanticModel = compilation.GetSemanticModel(tree)
            Dim InputNode As Syntax.CompilationUnitSyntax = CType(compilation.SyntaxTrees(0).GetRoot, Syntax.CompilationUnitSyntax)
            Dim node As Syntax.ClassDeclarationSyntax = CType(InputNode.Members(0), Syntax.ClassDeclarationSyntax)

            Assert.Equal(node.Kind(), SyntaxKind.ClassDeclaration)
            Dim classOrInterface As Syntax.TypeSyntax = node.BaseList?.Types.FirstOrDefault()?.Type
            Assert.False(classOrInterface Is Nothing)
            Dim Class_SymbolInfo As SymbolInfo = ModelExtensions.GetSymbolInfo(lSemanticModel, classOrInterface)
            Assert.NotNull(Class_SymbolInfo)
            Dim classOrInterfaceSymbol As ISymbol = Class_SymbolInfo.Symbol
            Assert.NotNull(classOrInterfaceSymbol)
        End Sub
    End Class
End Namespace