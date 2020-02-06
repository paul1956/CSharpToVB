' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBCodeConverter

    Public Module IdentifierSupport

        Friend Function GetNewUniqueName(ConvertedIdentifier As String, ident As KeyValuePair(Of String, SymbolTableEntry)) As String
            Dim NewUniqueName As String
            If ident.Value.Name.StartsWith("_", StringComparison.InvariantCulture) Then
                NewUniqueName = ConvertedIdentifier
            Else
                NewUniqueName = If(ConvertedIdentifier.StartsWith("[", StringComparison.InvariantCulture),
                                        ConvertedIdentifier.Replace("[", "_", StringComparison.InvariantCulture).
                                                            Replace("]", "", StringComparison.InvariantCulture),
                                        If(Char.IsLower(CChar(ConvertedIdentifier.Substring(0, 1))),
                                            $"_{ConvertedIdentifier}",
                                            $"{ConvertedIdentifier}_Renamed"))
            End If

            Return NewUniqueName
        End Function

        Friend Function IsSpecialReservedWord(ID As String) As Boolean
            If ID.Equals("Alias", StringComparison.InvariantCultureIgnoreCase) OrElse
                    ID.Equals("CType", StringComparison.InvariantCultureIgnoreCase) OrElse
                    ID.Equals("End", StringComparison.InvariantCultureIgnoreCase) OrElse
                    ID.Equals("Error", StringComparison.InvariantCultureIgnoreCase) OrElse
                    ID.Equals("Event", StringComparison.InvariantCultureIgnoreCase) OrElse
                    ID.Equals("Imports", StringComparison.InvariantCultureIgnoreCase) OrElse
                    ID.Equals("Module", StringComparison.InvariantCultureIgnoreCase) OrElse
                    ID.Equals("Option", StringComparison.InvariantCultureIgnoreCase) OrElse
                    ID.Equals("Optional", StringComparison.InvariantCultureIgnoreCase) Then
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

        Friend Function ProcessVariableDesignation(node As CS.Syntax.ParenthesizedVariableDesignationSyntax) As List(Of VBS.ModifiedIdentifierSyntax)
            Dim Variables As New List(Of VBS.ModifiedIdentifierSyntax)
            For i As Integer = 0 To node.Variables.Count - 1
                Dim VariableDeclarator As VBS.ModifiedIdentifierSyntax
                If node.Variables(i).RawKind = CS.SyntaxKind.DiscardDesignation Then
                    VariableDeclarator = VBFactory.ModifiedIdentifier("_")
                Else
                    VariableDeclarator = VBFactory.ModifiedIdentifier(MakeVBSafeName(node.Variables(i).ToString))
                End If
                Variables.Add(VariableDeclarator)
            Next
            Return Variables
        End Function

    End Module

End Namespace