' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports System.Runtime.InteropServices
Imports System.Threading

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax

Namespace CSharpToVBCodeConverter.ToVisualBasic

    Module SemanticModelExtensions

        <Extension>
        Friend Function TryInitializeState(model As SemanticModel,
                                           node As SyntaxNode,
                                           CancelToken As CancellationToken,
                                           <Out()> ByRef classOrStructDecl As SyntaxNode,
                                           <Out()> ByRef classOrStructType As INamedTypeSymbol,
                                           <Out()> ByRef interfaceTypes As IEnumerable(Of INamedTypeSymbol)) As Boolean
            Dim NodeTypeIsTypeSyntax As Boolean = TypeOf node Is CSS.TypeSyntax
            Dim interfaceNode As CSS.TypeSyntax = If(NodeTypeIsTypeSyntax, CType(node, CSS.TypeSyntax), Nothing)
            If NodeTypeIsTypeSyntax AndAlso TypeOf interfaceNode.Parent Is CSS.BaseTypeSyntax AndAlso interfaceNode.Parent.IsParentKind(CS.SyntaxKind.BaseList) AndAlso CType(interfaceNode.Parent, CSS.BaseTypeSyntax).Type Is interfaceNode Then
                If interfaceNode.Parent.Parent.IsParentKind(CS.SyntaxKind.ClassDeclaration) OrElse interfaceNode.Parent.Parent.IsParentKind(CS.SyntaxKind.StructDeclaration) Then
                    Dim interfaceSymbolInfo As SymbolInfo = model.GetSymbolInfo(interfaceNode, CancelToken)
                    If interfaceSymbolInfo.CandidateReason <> CandidateReason.WrongArity Then
                        Dim interfaceType As INamedTypeSymbol = TryCast(interfaceSymbolInfo.GetAnySymbol(), INamedTypeSymbol)
                        If interfaceType IsNot Nothing AndAlso interfaceType.TypeKind = Microsoft.CodeAnalysis.TypeKind.Interface Then
                            classOrStructDecl = TryCast(interfaceNode.Parent.Parent.Parent, CSS.TypeDeclarationSyntax)
                            classOrStructType = TryCast(model.GetDeclaredSymbol(classOrStructDecl, CancelToken), INamedTypeSymbol)
                            interfaceTypes = SpecializedCollection.SingletonEnumerable(interfaceType)
                            Return interfaceTypes IsNot Nothing AndAlso classOrStructType IsNot Nothing
                        End If
                    End If
                End If
            End If

            classOrStructDecl = Nothing
            classOrStructType = Nothing
            interfaceTypes = Nothing
            Return False
        End Function

    End Module
End Namespace
