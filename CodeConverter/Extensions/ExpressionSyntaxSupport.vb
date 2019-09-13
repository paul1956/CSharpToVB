' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Strict On
Option Infer Off

Imports System.Runtime.CompilerServices
Imports System.Text

Imports CSharpToVBCodeConverter.Util
Imports CSharpToVBCodeConverter.Visual_Basic.CSharpConverter

Imports Microsoft.CodeAnalysis

Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Public Module ExpressionSyntaxSupport

    Private Function ConvertCSTupleToVBType(CSType As ITypeSymbol) As VBS.TypeSyntax
        Dim TypeString As String = CSType.ToString
        Dim IsArray As Boolean = False
        Dim Nullable As Boolean = False
        If TypeString.EndsWith("?", StringComparison.InvariantCulture) Then
            Nullable = True
            TypeString = TypeString.Substring(0, TypeString.Length - 1).Trim
        End If
        If TypeString.EndsWith("[]", StringComparison.InvariantCulture) Then
            IsArray = True
            TypeString = TypeString.Substring(0, TypeString.Length - 2).Trim
        End If
        TypeString = TypeString.Substring(1, TypeString.Length - 2).Trim

        Dim ElementList As New List(Of String)
        While TypeString.Length > 0
            Dim IndexOfComma As Integer = TypeString.IndexOf(",", StringComparison.InvariantCulture)
            Dim IndexOfLessThan As Integer = TypeString.IndexOf("<", StringComparison.InvariantCulture)

            If IndexOfLessThan = -1 OrElse IndexOfComma = -1 AndAlso IndexOfLessThan > IndexOfComma Then
                Dim Item As String = TypeString.Substring(0, If(IndexOfComma = -1, TypeString.Length, IndexOfComma)).Trim
                Dim SplitOnSpace() As String = Item.Split(" "c)
                Dim VBType As VBS.TypeSyntax = NodesVisitor.ConvertToType(SplitOnSpace(0).Replace("<", "(Of ", StringComparison.InvariantCulture).Replace(">", ")", StringComparison.InvariantCulture).Trim).NormalizeWhitespaceEx(useDefaultCasing:=True)
                If SplitOnSpace.Count = 2 Then
                    ElementList.Add($"{SplitOnSpace(1).Trim} As {VBType.ToFullString}")
                Else
                    ElementList.Add(VBType.ToFullString)
                End If
                TypeString = TypeString.Substring(If(IndexOfComma = -1, TypeString.Length, IndexOfComma + 1)).Trim
            Else
                Dim Item As String = TypeString.Substring(0, Math.Min(IndexOfComma, IndexOfLessThan))
                Dim SplitOnSpace() As String = Item.Split(" "c)
                Dim VariableName As String = String.Empty
                If SplitOnSpace.Count = 2 Then
                    VariableName = SplitOnSpace(1).Trim
                End If
                If SplitOnSpace(0).Trim = "?" Then
                    ElementList.Add("Object")
                Else
                    ElementList.Add($"{VariableName}{NodesVisitor.ConvertToType(SplitOnSpace(0).Replace("<", "(Of ", StringComparison.InvariantCulture).Replace(">", ")", StringComparison.InvariantCulture).Trim).NormalizeWhitespaceEx(useDefaultCasing:=True)}")
                End If
                If IndexOfComma = -1 Then
                    TypeString = String.Empty
                Else
                    TypeString = TypeString.Substring(If(IndexOfComma = -1, TypeString.Length, IndexOfComma + 1)).Trim
                End If
            End If
        End While
        Dim builder As New StringBuilder
        builder.Append("(")
        For i As Integer = 0 To ElementList.Count - 2
            builder.Append(ElementList(i) & ", ")
        Next
        builder.Append(ElementList.Last & ")")
        Dim TupleType As String = builder.ToString & If(IsArray, "()", "") & If(Nullable, "?", "")

        Return VisualBasic.SyntaxFactory.ParseTypeName(TupleType).WithLeadingTrivia(SpaceTrivia)
    End Function

    <Extension>
    Friend Function DetermineType(expression As CSS.ExpressionSyntax, Model As SemanticModel) As (_ITypeSymbol As ITypeSymbol, _Error As Boolean)
        ' If a parameter appears to have a void return type, then just use 'object' instead.
        Try
            If expression IsNot Nothing Then
                Dim typeInfo As TypeInfo = Model.GetTypeInfo(expression)
                Dim symbolInfo As SymbolInfo = Model.GetSymbolInfo(expression)
                If typeInfo.Type IsNot Nothing Then
                    If (typeInfo.Type.IsErrorType) Then
                        Return (Model.Compilation.ObjectType, _Error:=True)
                    ElseIf Equals(typeInfo.Type, Model.Compilation.ObjectType) Then
                        Return (Model.Compilation.ObjectType, _Error:=False)
                    End If
                End If
                Dim symbol As ISymbol = If(typeInfo.Type, symbolInfo.GetAnySymbol())
                If symbol IsNot Nothing Then
                    Dim _type As ITypeSymbol = TryCast(symbol, ITypeSymbol)
                    If _type IsNot Nothing Then
                        If symbol.Kind = SymbolKind.PointerType Then
                            Return (_type, _Error:=True)
                        End If
                        If _type.ToString.Contains("<anonymous type", StringComparison.InvariantCulture) OrElse
                            _type.ToString.StartsWith("(", StringComparison.InvariantCulture) Then
                            Return (_type, _Error:=True)
                        End If
                        Return (_type, _Error:=False)
                    End If
                    Return (symbol.ConvertISymbolToType(Model.Compilation), False)
                End If

            End If
        Catch ex As Exception
            Stop
        End Try
        Return (Model.Compilation.ObjectType, _Error:=True)
    End Function

    <Extension>
    Friend Function DetermineTypeSyntax(expression As CSS.ExpressionSyntax, Model As SemanticModel) As (_ITypeSymbol As VBS.TypeSyntax, _Error As Boolean)
        ' If a parameter appears to have a void return type, then just use 'object' instead.
        Try
            If expression IsNot Nothing Then
                Dim typeInfo As TypeInfo = Model.GetTypeInfo(expression)
                Dim symbolInfo As SymbolInfo = Model.GetSymbolInfo(expression)
                If typeInfo.Type IsNot Nothing Then
                    If (typeInfo.Type.IsErrorType) Then
                        Return (PredefinedTypeObject, _Error:=True)
                    ElseIf Equals(typeInfo.Type, Model.Compilation.ObjectType) Then
                        Return (PredefinedTypeObject, _Error:=False)
                    End If
                End If
                Dim symbol As ISymbol = If(typeInfo.Type, symbolInfo.GetAnySymbol())
                If symbol IsNot Nothing Then
                    Dim _type As ITypeSymbol = TryCast(symbol, ITypeSymbol)
                    If _type IsNot Nothing Then
                        If symbol.Kind = SymbolKind.PointerType Then
                            Return (IntPtrType, _Error:=True)
                        End If
                        If _type.ToString.Contains("<anonymous type", StringComparison.InvariantCulture) Then
                            Return (PredefinedTypeObject, _Error:=True)
                        End If
                        If _type.ToString.StartsWith("(", StringComparison.InvariantCulture) Then
                            Return (ConvertCSTupleToVBType(_type), _Error:=False)
                        End If
                    End If
                    Return (NodesVisitor.ConvertToType(symbol.ConvertISymbolToType(Model.Compilation)), _Error:=False)
                End If

            End If
        Catch ex As Exception
            Stop
        End Try
        Return (PredefinedTypeObject, _Error:=True)
    End Function

End Module
