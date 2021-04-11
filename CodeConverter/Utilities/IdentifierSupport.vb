' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Text
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Public Module IdentifierSupport

    Friend Sub CreateDesignationName(listOfVariables As List(Of VBS.ModifiedIdentifierSyntax), ByRef sBuilder As StringBuilder)
        sBuilder.Append("TupleTempVar")
        For j As Integer = 0 To listOfVariables.Count - 1
            Dim v As VBS.ModifiedIdentifierSyntax = listOfVariables(j)
            If v.Identifier.ValueText = "_" Then
                sBuilder.Append($"_Discard{j}")
            Else
                sBuilder.Append($"_{v.Identifier.ValueText}")
            End If
        Next
    End Sub

    Friend Function IsSpecialReservedWord(id As String) As Boolean
        If id.Equals("Alias", StringComparison.OrdinalIgnoreCase) OrElse
                id.Equals("CType", StringComparison.OrdinalIgnoreCase) OrElse
                id.Equals("End", StringComparison.OrdinalIgnoreCase) OrElse
                id.Equals("Error", StringComparison.OrdinalIgnoreCase) OrElse
                id.Equals("Event", StringComparison.OrdinalIgnoreCase) OrElse
                id.Equals("Imports", StringComparison.OrdinalIgnoreCase) OrElse
                id.Equals("Module", StringComparison.OrdinalIgnoreCase) OrElse
                id.Equals("Option", StringComparison.OrdinalIgnoreCase) OrElse
                id.Equals("Optional", StringComparison.OrdinalIgnoreCase) Then
            Return True
        End If
        Return False
    End Function

    ''' <summary>
    ''' If id is an VB Reserved word surround with []
    ''' </summary>
    ''' <param name="id"></param>
    Friend Function MakeVbSafeName(id As String) As String
        If IsSpecialReservedWord(id) OrElse
            VB.SyntaxFacts.IsKeywordKind(VB.SyntaxFacts.GetKeywordKind(id)) Then
            Return $"[{id}]"
        End If
        Return id
    End Function

    Friend Function ProcessVariableDesignation(node As CSS.ParenthesizedVariableDesignationSyntax) As List(Of VBS.ModifiedIdentifierSyntax)
        Dim vbVariables As New List(Of VBS.ModifiedIdentifierSyntax)
        For Each e As IndexClass(Of CSS.VariableDesignationSyntax) In node.Variables.WithIndex
            Dim vbVariableDeclarator As VBS.ModifiedIdentifierSyntax
            If e.Value.RawKind = CS.SyntaxKind.DiscardDesignation Then
                vbVariableDeclarator = Factory.ModifiedIdentifier("_")
            Else
                vbVariableDeclarator = Factory.ModifiedIdentifier(MakeVbSafeName(e.Value.ToString))
            End If
            vbVariables.Add(vbVariableDeclarator)
        Next
        Return vbVariables
    End Function

End Module
