' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CSharp
Imports Microsoft.CodeAnalysis.VisualBasic

Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax

Namespace CSharpToVBConverter.CSharpToVBVisitors
    Public Module MethodBodySupport

        Friend Function ContainsLocalFunctionReference(syntax As SyntaxNode, localFunctionSymbol As IMethodSymbol, _semanticModel As SemanticModel) As Boolean
            Return syntax.DescendantNodes().
                                OfType(Of CSS.SimpleNameSyntax)().
                                Any(Function(name As CSS.SimpleNameSyntax) name.Identifier.ValueText = localFunctionSymbol.Name AndAlso
                                SymbolEqualityComparer.Default.Equals(_semanticModel.GetSymbolInfo(name).Symbol, localFunctionSymbol))
        End Function

        <Extension>
        Friend Function GetUniqueVariableNameInScope(node As CSharpSyntaxNode, variableNameBase As String, _usedIdentifiers As Dictionary(Of String, SymbolTableEntry), lSemanticModel As SemanticModel) As String
            Dim isField As Boolean = node.AncestorsAndSelf().OfType(Of CSS.FieldDeclarationSyntax).Any
            Dim reservedNames As New List(Of String) From {
                    "_"
                }
            reservedNames.AddRange(node.DescendantNodesAndSelf().SelectMany(Function(lSyntaxNode As SyntaxNode) lSemanticModel.LookupSymbols(lSyntaxNode.SpanStart).Select(Function(s As ISymbol) s.Name)).Distinct)
            Dim uniqueVariableName As String = EnsureUniqueness(variableNameBase, _usedIdentifiers, reservedNames)
            _usedIdentifiers.Add(uniqueVariableName,
                                  New SymbolTableEntry(uniqueVariableName,
                                                       IsType:=False,
                                                       isField
                                                       )
                                  )
            Return uniqueVariableName
        End Function

    End Module
End Namespace
