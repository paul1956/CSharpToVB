Option Explicit On
Option Strict On
Option Infer Off

Imports System.Runtime.CompilerServices
Imports IVisualBasicCode.CodeConverter.Util
Imports Microsoft.CodeAnalysis
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax

Public Module ExpressionSyntaxSupport

    <Extension>
    Public Function DetermineType(expression As CSS.ExpressionSyntax, Model As SemanticModel) As (_ITypeSymbol As ITypeSymbol, _Error As Boolean)
        ' If a parameter appears to have a void return type, then just use 'object' instead.
        Try
            If expression IsNot Nothing Then
                Dim typeInfo As TypeInfo = Model.GetTypeInfo(expression)
                Dim symbolInfo As SymbolInfo = Model.GetSymbolInfo(expression)
                If typeInfo.Type IsNot Nothing Then
                    If (typeInfo.Type.IsErrorType) Then
                        Return (Model.Compilation.ObjectType, True)
                    ElseIf Equals(typeInfo.Type, Model.Compilation.ObjectType) Then
                        Return (Model.Compilation.ObjectType, False)
                    End If
                End If
                Dim symbol As ISymbol = If(typeInfo.Type, symbolInfo.GetAnySymbol())
                If symbol IsNot Nothing Then
                    Dim _type As ITypeSymbol = TryCast(symbol, ITypeSymbol)
                    If _type IsNot Nothing Then
                        Return (_type, False)
                    End If
                    Return (symbol.ConvertISymbolToType(Model.Compilation), False)
                End If

            End If
        Catch ex As Exception
            Stop
        End Try
        Return (Model.Compilation.ObjectType, True)
    End Function

End Module