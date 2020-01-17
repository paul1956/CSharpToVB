' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.Runtime.CompilerServices

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax

Namespace CSharpToVBCodeConverter.DestVisualBasic
    Friend Module ConversionExtensions

        Private Function MatchesNamespaceOrRoot(arg As SyntaxNode) As Boolean
            Return TypeOf arg Is CSS.NamespaceDeclarationSyntax OrElse TypeOf arg Is CSS.CompilationUnitSyntax
        End Function

        <Extension>
        Public Function HasUsingDirective(tree As CS.CSharpSyntaxTree, FullName As String) As Boolean
            If tree Is Nothing Then
                Throw New ArgumentNullException(NameOf(tree))
            End If
            If String.IsNullOrWhiteSpace(FullName) Then
                Throw New ArgumentNullException(NameOf(FullName))
            End If
            FullName = FullName.Trim()
            Return tree.GetRoot().DescendantNodes(AddressOf MatchesNamespaceOrRoot).OfType(Of CSS.UsingDirectiveSyntax)().Any(Function(u As CSS.UsingDirectiveSyntax) u.Name.ToString().Equals(FullName, StringComparison.OrdinalIgnoreCase))
        End Function

        <Extension>
        Public Iterator Function IndexedSelect(Of T, R)(source As IEnumerable(Of T), transform As Func(Of Integer, T, R)) As IEnumerable(Of R)
            Dim i As Integer = 0
            For Each item As T In source
                Yield transform(i, item)
                i += 1
            Next item
        End Function

    End Module
End Namespace
