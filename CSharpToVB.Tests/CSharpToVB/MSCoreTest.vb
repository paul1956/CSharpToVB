Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CSharp
Imports Xunit

Namespace XUnitTestProject1
    Public Class MSCoreTest

        <Fact>
        Sub TestSub()
            Dim tree As SyntaxTree = CSharpSyntaxTree.ParseText("using System;
class test : IComparable { }")

            Dim compilation As CSharpCompilation = CSharpCompilation.Create("MyCompilation", syntaxTrees:={tree}, references:=SharedReferences.References)
            Dim lSemanticModel = compilation.GetSemanticModel(tree)
            Dim InputNode As Syntax.CompilationUnitSyntax = compilation.SyntaxTrees(0).GetRoot
            Dim node As Syntax.ClassDeclarationSyntax = InputNode.Members(0)

            Assert.True(node.Kind() = SyntaxKind.ClassDeclaration)
            Dim classOrInterface As Syntax.TypeSyntax = node.BaseList?.Types.FirstOrDefault()?.Type
            Assert.False(classOrInterface Is Nothing)
            Dim Class_SymbolInfo As SymbolInfo = ModelExtensions.GetSymbolInfo(lSemanticModel, classOrInterface)
            Assert.NotNull(Class_SymbolInfo)
            Dim classOrInterfaceSymbol As ISymbol = Class_SymbolInfo.Symbol
            Assert.NotNull(classOrInterfaceSymbol)
        End Sub
    End Class
End Namespace