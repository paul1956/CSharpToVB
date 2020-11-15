' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CSharp
Imports Microsoft.CodeAnalysis.VisualBasic

Imports CS = Microsoft.CodeAnalysis.CSharp

Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax

Namespace CSharpToVBConverter.ToVisualBasic
    Public Module MethodBodySupport

        Friend Function ContainsLocalFunctionReference(syntax As SyntaxNode, localFunctionSymbol As IMethodSymbol, _semanticModel As SemanticModel) As Boolean
            Return syntax.DescendantNodes().
                                OfType(Of CSS.SimpleNameSyntax)().
                                Any(Function(name As CSS.SimpleNameSyntax) name.identifier.ValueText = localFunctionSymbol.Name AndAlso
                                SymbolEqualityComparer.Default.Equals(_semanticModel.GetSymbolInfo(name).Symbol, localFunctionSymbol))
        End Function

        <Extension>
        Friend Function GetPossibleEventName(expression As CSS.ExpressionSyntax) As String
            Dim ident As CSS.IdentifierNameSyntax = TryCast(expression, CSS.IdentifierNameSyntax)
            If ident IsNot Nothing Then Return ident.identifier.Text

            If TypeOf expression Is CSS.ParenthesizedExpressionSyntax Then
                expression = DirectCast(expression, CSS.ParenthesizedExpressionSyntax).Expression
            Else
                Return Nothing
            End If
            Dim fre As CSS.MemberAccessExpressionSyntax = TryCast(expression, CSS.MemberAccessExpressionSyntax)
            If fre IsNot Nothing AndAlso fre.Expression.IsKind(CS.SyntaxKind.ThisExpression) Then Return fre.Name.identifier.Text
            Return Nothing
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
