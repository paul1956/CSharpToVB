' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.Runtime.CompilerServices

Imports CSharpToVBCodeConverter
Imports CSharpToVBCodeConverter.DestVisualBasic
Imports CSharpToVBCodeConverter.Util

Imports Microsoft.CodeAnalysis

Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Public Module ExpressionSyntaxSupport

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
                    ElseIf SymbolEqualityComparer.Default.Equals(typeInfo.Type, Model.Compilation.ObjectType) Then
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
        Catch ex As OperationCanceledException
            Throw
        Catch ex As Exception
            Stop
            Throw
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
                    ElseIf SymbolEqualityComparer.Default.Equals(typeInfo.Type, Model.Compilation.ObjectType) Then
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
                            Return (CSharpConverter.ConvertCSTupleToVBType(_type.ToString), _Error:=False)
                        End If
                    End If
                    Return (ConvertToType(symbol.ConvertISymbolToType(Model.Compilation)), _Error:=False)
                End If

            End If
        Catch ex As OperationCanceledException
            Throw
        Catch ex As Exception
            Stop
        End Try
        Return (PredefinedTypeObject, _Error:=True)
    End Function

End Module