' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Collections.Immutable
Imports System.Runtime.CompilerServices
Imports CSharpToVBCodeConverter
Imports Microsoft.CodeAnalysis
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Module CSharpNodeExtensions

    <Extension>
    Private Function ExtractConvertedTuple(TupleString As String, Node As CSharp.CSharpSyntaxNode, Model As SemanticModel) As String
        Dim TupleElements As New List(Of String)
        For Each t As String In TupleString.Substring(startIndex:=1, TupleString.Length - 2).Split(","c)
            Dim TuplePart() As String = t.Trim.Split(" "c)
            If TuplePart.Length = 1 Then
                TupleElements.Add(ConvertToType(TuplePart(0)).ToString)
            Else
                Dim Identifier As SyntaxToken = CSharp.SyntaxFactory.Identifier(TuplePart(1))
                TupleElements.Add($"{GenerateSafeVBToken(Identifier, Node, Model).ValueText} As {ConvertToType(TuplePart(0))}")
            End If
        Next
        Return $"({String.Join(", ", TupleElements)})"
    End Function

    <Extension>
    Friend Function ConvertISymbolToNameSyntaxInterfaceName(Node As CSharp.CSharpSyntaxNode, interfaceMethod As ISymbol, Model As SemanticModel) As VBS.NameSyntax
        Dim TypeString As String = interfaceMethod.ContainingSymbol.ToString
        TypeString = TypeString.ConvertTypeArgumentList.
                                RemoveBrackets

        Dim FirstTupleIndex As Integer = TypeString.IndexOf("(Of (", StringComparison.Ordinal)
        If FirstTupleIndex < 0 Then
            Return Factory.ParseName(TypeString)
        End If
        FirstTupleIndex += 4
        Dim Result As String = TypeString.Substring(startIndex:=0, FirstTupleIndex)
        Dim OpenIndex As Integer = FirstTupleIndex
        Dim OpenParenCount As Integer = 0
        Dim CloseIndex As Integer = FirstTupleIndex
        Dim TupleList As New List(Of String)
        While CloseIndex < TypeString.Length - 1
            Select Case TypeString.Chars(CloseIndex)
                Case "("c
                    OpenParenCount += 1
                Case ")"c
                    OpenParenCount -= 1
                    If OpenParenCount = 0 Then
                        Dim TupleString As String = TypeString.Substring(OpenIndex, CloseIndex - OpenIndex + 1)
                        Result &= TupleString.ExtractConvertedTuple(Node, Model)
                        If CloseIndex < TypeString.Length - 2 Then
                            Stop
                        Else
                            Exit While
                        End If
                    End If
                Case Else
            End Select
            CloseIndex += 1
        End While
        Return Factory.ParseName($"{Result})")
    End Function

    <Extension>
    Friend Function GetImplementsClauseForMethod(Node As CSharp.CSharpSyntaxNode, Model As SemanticModel, csMethod As IMethodSymbol, ListOfRequiredInterfaces As ImmutableArray(Of (InterfaceName As INamedTypeSymbol, MethodList As ImmutableArray(Of ISymbol)))) As VBS.ImplementsClauseSyntax
        If Not ListOfRequiredInterfaces.Any Then
            Return Nothing
        End If
        Dim SeparatedList As New List(Of VBS.QualifiedNameSyntax)
        For Each entry As (InterfaceName As INamedTypeSymbol, MethodList As ImmutableArray(Of ISymbol)) In ListOfRequiredInterfaces
            For Each InterfaceMethod As ISymbol In entry.MethodList
                Dim _Right As VBS.SimpleNameSyntax = Nothing
                If TypeOf InterfaceMethod Is IMethodSymbol Then
                    If ImplementsMethodOrProperty(csMethod, CType(InterfaceMethod, IMethodSymbol), _Right) Then
                        SeparatedList.Add(Factory.QualifiedName(ConvertISymbolToNameSyntaxInterfaceName(Node, entry.InterfaceName, Model), _Right))
                        Exit For
                    End If
                End If
            Next
        Next
        If SeparatedList.Count = 0 Then
            Return Nothing
        End If
        Return Factory.ImplementsClause(Factory.SeparatedList(SeparatedList))
    End Function

End Module
