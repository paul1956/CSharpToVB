' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports Microsoft.CodeAnalysis
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBConverter.ToVisualBasic

    Friend NotInheritable Class CommonConversions
        Private ReadOnly _semanticModel As SemanticModel

        Public Sub New(semanticModel1 As SemanticModel)
            _semanticModel = semanticModel1
        End Sub

        Private Shared Function GetFullyQualifiedNameSyntax(symbol As INamespaceOrTypeSymbol, Optional allowGlobalPrefix As Boolean = True) As VBS.NameSyntax
            Select Case True
                Case TypeOf symbol Is ITypeSymbol
                    Dim ts As ITypeSymbol = CType(symbol, ITypeSymbol)
                    Dim nameSyntax1 As VBS.NameSyntax = CType(Factory.ParseTypeName(ts.ConvertToType.ToString), VBS.NameSyntax)
                    If allowGlobalPrefix Then
                        Return nameSyntax1
                    End If
                    Dim globalNameNode As VBS.GlobalNameSyntax = nameSyntax1.DescendantNodes().OfType(Of VBS.GlobalNameSyntax)().FirstOrDefault()
                    If globalNameNode IsNot Nothing Then
                        nameSyntax1 = nameSyntax1.ReplaceNodes(TryCast(globalNameNode.Parent, VBS.QualifiedNameSyntax).Yield(), Function(orig, rewrite) orig.Right)
                    End If

                    Return nameSyntax1
                Case TypeOf symbol Is INamespaceSymbol
                    Dim ns As INamespaceSymbol = CType(symbol, INamespaceSymbol)
                    Return Factory.ParseName(ns.GetFullMetadataName())
                Case Else
                    Throw New NotImplementedException($"Fully qualified name for {symbol.[GetType]().FullName} not implemented")
            End Select
        End Function

        Private Function GetSymbol(syntax As CS.CSharpSyntaxNode) As ISymbol
            Return If(syntax.SyntaxTree Is _semanticModel.SyntaxTree, _semanticModel.GetSymbolInfo(syntax).Symbol, Nothing)
        End Function

        Public Function IsEventHandlerIdentifier(syntax As CS.CSharpSyntaxNode) As Boolean
            Return Me.GetSymbol(syntax).IsKind(SymbolKind.Event)
        End Function

    End Class

End Namespace
