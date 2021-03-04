' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices

Imports Microsoft.CodeAnalysis
Imports SupportClasses

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax

Namespace Utilities
    Public Module MethodBodySupport

        Friend Function ContainsLocalFunctionReference(syntax As SyntaxNode, localFunctionSymbol As IMethodSymbol, semanticModel As SemanticModel) As Boolean
            Return syntax.DescendantNodes().
                                OfType(Of CSS.SimpleNameSyntax)().
                                Any(Function(name As CSS.SimpleNameSyntax) name.Identifier.ValueText = localFunctionSymbol.Name AndAlso
                                SymbolEqualityComparer.Default.Equals(semanticModel.GetSymbolInfo(name).Symbol, localFunctionSymbol))
        End Function

        <Extension>
        Friend Function GetUniqueVariableNameInScope(node As CS.CSharpSyntaxNode, variableNameBase As String, usedIdentifiers As Dictionary(Of String, SymbolTableEntry), lSemanticModel As SemanticModel) As String
            Dim isField As Boolean = node.AncestorsAndSelf().OfType(Of CSS.FieldDeclarationSyntax).Any
            Dim reservedNames As New List(Of String) From {
                    "_"
                }
            reservedNames.AddRange(node.DescendantNodesAndSelf().SelectMany(Function(lSyntaxNode As SyntaxNode) lSemanticModel.LookupSymbols(lSyntaxNode.SpanStart).Select(Function(s As ISymbol) s.Name)).Distinct)
            Dim uniqueVariableName As String = EnsureUniqueness(variableNameBase, usedIdentifiers, reservedNames)
            usedIdentifiers.Add(uniqueVariableName,
                                  New SymbolTableEntry(uniqueVariableName,
                                                       isType:=False,
                                                       isField
                                                       )
                                  )
            Return uniqueVariableName
        End Function

    End Module
End Namespace
