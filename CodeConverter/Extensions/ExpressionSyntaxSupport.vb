' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices

Imports CSharpToVBCodeConverter
Imports CSharpToVBCodeConverter.DestVisualBasic
Imports CSharpToVBCodeConverter.Util

Imports Microsoft.CodeAnalysis

Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Public Module ExpressionSyntaxSupport

    <Extension>
    Friend Function DetermineType(expression As CSS.ExpressionSyntax, Model As SemanticModel) As (_Error As Boolean, _ITypeSymbol As ITypeSymbol)
        ' If a parameter appears to have a void return type, then just use 'object' instead.
        Try
            If expression IsNot Nothing Then
                Dim typeInfo As TypeInfo = Model.GetTypeInfo(expression)
                Dim symbolInfo As SymbolInfo = Model.GetSymbolInfo(expression)
                If typeInfo.Type IsNot Nothing Then
                    If (typeInfo.Type.IsErrorType) Then
                        Return (_Error:=True, Model.Compilation.ObjectType)
                    ElseIf SymbolEqualityComparer.Default.Equals(typeInfo.Type, Model.Compilation.ObjectType) Then
                        Return (_Error:=False, Model.Compilation.ObjectType)
                    End If
                End If
                Dim symbol As ISymbol = If(typeInfo.Type, symbolInfo.GetAnySymbol())
                If symbol IsNot Nothing Then
                    Dim _type As ITypeSymbol = TryCast(symbol, ITypeSymbol)
                    If _type IsNot Nothing Then
                        If symbol.Kind = SymbolKind.PointerType Then
                            Return (_Error:=True, _type)
                        End If
                        If _type.ToString.Contains("<anonymous type", StringComparison.Ordinal) OrElse
                            _type.ToString.StartsWith("(", StringComparison.Ordinal) Then
                            Return (_Error:=True, _type)
                        End If
                        Return (_Error:=False, _type)
                    End If
                    Return (_Error:=False, symbol.ConvertISymbolToType(Model.Compilation))
                End If

            End If
        Catch ex As OperationCanceledException
            Throw
        Catch ex As Exception
            Stop
            Throw
        End Try
        Return (_Error:=True, Model.Compilation.ObjectType)
    End Function

    <Extension>
    Friend Function DetermineTypeSyntax(expression As CSS.ExpressionSyntax, Model As SemanticModel) As (_Error As Boolean, _ITypeSymbol As VBS.TypeSyntax)
        ' If a parameter appears to have a void return type, then just use 'object' instead.
        Try
            If expression IsNot Nothing Then
                Dim typeInfo As TypeInfo = Model.GetTypeInfo(expression)
                Dim symbolInfo As SymbolInfo = Model.GetSymbolInfo(expression)
                If typeInfo.Type IsNot Nothing Then
                    If (typeInfo.Type.IsErrorType) Then
                        Return (_Error:=True, PredefinedTypeObject)
                    ElseIf SymbolEqualityComparer.Default.Equals(typeInfo.Type, Model.Compilation.ObjectType) Then
                        Return (_Error:=False, PredefinedTypeObject)
                    End If
                End If
                Dim symbol As ISymbol = If(typeInfo.Type, symbolInfo.GetAnySymbol())
                If symbol IsNot Nothing Then
                    Dim _type As ITypeSymbol = TryCast(symbol, ITypeSymbol)
                    If _type IsNot Nothing Then
                        If symbol.Kind = SymbolKind.PointerType Then
                            Return (_Error:=True, IntPtrType)
                        End If
                        If _type.ToString.Contains("<anonymous type", StringComparison.Ordinal) Then
                            Return (_Error:=True, PredefinedTypeObject)
                        End If
                        If _type.ToString.StartsWith("(", StringComparison.Ordinal) Then
                            Return (_Error:=False, CSharpConverter.ConvertCSTupleToVBType(_type.ToString))
                        End If
                    End If
                    Return (_Error:=False, ConvertToType(symbol.ConvertISymbolToType(Model.Compilation)))
                End If

            End If
        Catch ex As OperationCanceledException
            Throw
        Catch ex As Exception
            Stop
        End Try
        Return (_Error:=True, PredefinedTypeObject)
    End Function

    <Extension>
    Public Function AdjustExpressionLeadingTrivia(Expression As VBS.ExpressionSyntax) As VBS.ExpressionSyntax
        If Expression Is Nothing Then
            Throw New ArgumentNullException(NameOf(Expression))
        End If

        Dim AfterWhiteSpace As Boolean = False
        Dim AfterEOL As Boolean = False
        Dim NeedLineContinuation As Boolean = True
        Dim NewTriviaList As New List(Of SyntaxTrivia)

        Dim OldTriviaList As New List(Of SyntaxTrivia)
        OldTriviaList.AddRange(Expression.GetLeadingTrivia)
        For Each e As IndexClass(Of SyntaxTrivia) In OldTriviaList.WithIndex
            Dim Trivia As SyntaxTrivia = e.Value
            Dim nextTrivia As SyntaxTrivia = If(Not e.IsLast, OldTriviaList(e.Index + 1), New SyntaxTrivia)
            Select Case Trivia.RawKind
                Case VB.SyntaxKind.WhitespaceTrivia
                    If NeedLineContinuation AndAlso Not nextTrivia.IsNone Then
                        NewTriviaList.Add(SpaceTrivia)
                        NewTriviaList.Add(LineContinuation)
                    End If
                    NewTriviaList.Add(Trivia)
                    AfterEOL = False
                    AfterWhiteSpace = True
                    NeedLineContinuation = False
                Case VB.SyntaxKind.EndOfLineTrivia
                    If AfterEOL Then
                        Continue For
                    End If
                    If nextTrivia.IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                        NeedLineContinuation = True
                    End If
                    NewTriviaList.Add(Trivia)
                    AfterWhiteSpace = False
                    AfterEOL = True
                Case VB.SyntaxKind.CommentTrivia
                    If Not AfterWhiteSpace Then
                        NewTriviaList.Add(SpaceTrivia)
                    End If
                    If NeedLineContinuation Then
                        NewTriviaList.Add(LineContinuation)
                        NeedLineContinuation = False
                    End If
                    NewTriviaList.Add(Trivia)
                    If Not nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                        NewTriviaList.Add(VBEOLTrivia)
                    End If
                Case VB.SyntaxKind.LineContinuationTrivia
                    NewTriviaList.Add(Trivia)
                    AfterEOL = False
                    AfterWhiteSpace = False
                    NeedLineContinuation = False
                Case Else
                    Stop
            End Select
        Next
        Return Expression.WithLeadingTrivia(NewTriviaList)
    End Function

End Module
