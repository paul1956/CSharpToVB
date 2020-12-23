' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports CSharpToVBConverter
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CSharp

Imports Xunit

Namespace MSCoreReference.Tests

    Public NotInheritable Class MSCoreReferenceTest

        <Fact>
        Public Shared Sub VerifyReferencesExist()
            Dim tree As SyntaxTree = CSharpSyntaxTree.ParseText("using System; //Not required in VB due to global imports
class test : IComparable { }")

            Dim compilation As CSharpCompilation = CSharpCompilation.Create("MyCompilation", syntaxTrees:={tree}, CSharpReferences("", New List(Of MetadataReference)))
            Dim lSemanticModel As SemanticModel = compilation.GetSemanticModel(tree)
            Dim inputNode As Syntax.CompilationUnitSyntax = CType(compilation.SyntaxTrees(0).GetRoot, Syntax.CompilationUnitSyntax)
            Dim node As Syntax.ClassDeclarationSyntax = CType(inputNode.Members(0), Syntax.ClassDeclarationSyntax)

            Assert.Equal(node.Kind(), SyntaxKind.ClassDeclaration)
            Dim classOrInterface As Syntax.TypeSyntax = node.BaseList?.Types.FirstOrDefault()?.Type
            Assert.False(classOrInterface Is Nothing)
            Dim classSymbolInfo As SymbolInfo = ModelExtensions.GetSymbolInfo(lSemanticModel, classOrInterface)
            Assert.NotNull(classSymbolInfo)
            Dim classOrInterfaceSymbol As ISymbol = classSymbolInfo.Symbol
            Assert.NotNull(classOrInterfaceSymbol)
        End Sub

    End Class

End Namespace
