' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports System.Text
Imports CSharpToVBConverter.ToVisualBasic
Imports Microsoft.CodeAnalysis
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBConverter

    Public Module IdentifierSupport

        Friend Sub CreateDesignationName(ListOfVariables As List(Of VBS.ModifiedIdentifierSyntax), ByRef sBuilder As StringBuilder)
            sBuilder.Append("TupleTempVar")
            For j As Integer = 0 To ListOfVariables.Count - 1
                Dim v As VBS.ModifiedIdentifierSyntax = ListOfVariables(j)
                If v.identifier.ValueText = "_" Then
                    sBuilder.Append($"_Discard{j}")
                Else
                    sBuilder.Append($"_{v.identifier.ValueText}")
                End If
            Next
        End Sub

        <Extension>
        Friend Function GetNewUniqueName(ConvertedIdentifier As String, _usedIdentifiers As Dictionary(Of String, SymbolTableEntry), IsType As Boolean, Node As CS.CSharpSyntaxNode, Model As SemanticModel) As String
            If IsType Then
                Return ConvertedIdentifier
            End If

            ConvertedIdentifier = ConvertedIdentifier.RemoveBrackets

            Dim uniqueID As String = GetUniqueVariableNameInScope(Node, ConvertedIdentifier, _usedIdentifiers, Model)
            If VB.SyntaxFacts.GetKeywordKind(uniqueID) = VB.SyntaxKind.None Then
                Return uniqueID
            End If

            Return $"[{uniqueID}]"
        End Function

        Friend Function IsSpecialReservedWord(ID As String) As Boolean
            If ID.Equals("Alias", StringComparison.OrdinalIgnoreCase) OrElse
                    ID.Equals("CType", StringComparison.OrdinalIgnoreCase) OrElse
                    ID.Equals("End", StringComparison.OrdinalIgnoreCase) OrElse
                    ID.Equals("Error", StringComparison.OrdinalIgnoreCase) OrElse
                    ID.Equals("Event", StringComparison.OrdinalIgnoreCase) OrElse
                    ID.Equals("Imports", StringComparison.OrdinalIgnoreCase) OrElse
                    ID.Equals("Module", StringComparison.OrdinalIgnoreCase) OrElse
                    ID.Equals("Option", StringComparison.OrdinalIgnoreCase) OrElse
                    ID.Equals("Optional", StringComparison.OrdinalIgnoreCase) Then
                Return True
            End If
            Return False
        End Function

        ''' <summary>
        ''' If Name is an VB Reserved word surround with []
        ''' </summary>
        ''' <param name="Name"></param>
        Friend Function MakeVBSafeName(Id As String) As String
            If IsSpecialReservedWord(Id) OrElse
                VB.SyntaxFacts.IsKeywordKind(VB.SyntaxFacts.GetKeywordKind(Id)) Then
                Return $"[{Id}]"
            End If
            Return Id
        End Function

        Friend Function ProcessVariableDesignation(node As CSS.ParenthesizedVariableDesignationSyntax) As List(Of VBS.ModifiedIdentifierSyntax)
            Dim vbVariables As New List(Of VBS.ModifiedIdentifierSyntax)
            For Each e As IndexClass(Of CSS.VariableDesignationSyntax) In node.Variables.WithIndex
                Dim vbVariableDeclarator As VBS.ModifiedIdentifierSyntax
                If e.Value.RawKind = CS.SyntaxKind.DiscardDesignation Then
                    vbVariableDeclarator = Factory.ModifiedIdentifier("_")
                Else
                    vbVariableDeclarator = Factory.ModifiedIdentifier(MakeVBSafeName(e.Value.ToString))
                End If
                vbVariables.Add(vbVariableDeclarator)
            Next
            Return vbVariables
        End Function

    End Module

End Namespace
