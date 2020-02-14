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
        Dim InitialTriviaListUBound As Integer = OldTriviaList.Count - 1
        For i As Integer = 0 To InitialTriviaListUBound
            Dim Trivia As SyntaxTrivia = OldTriviaList(i)
            Dim nextTrivia As SyntaxTrivia = If(i < InitialTriviaListUBound, OldTriviaList(i + 1), New SyntaxTrivia)
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
                    'Case VB.SyntaxKind.DisableWarningDirectiveTrivia, VB.SyntaxKind.EnableWarningDirectiveTrivia
                    '    FirstTrivia = False
                    '    Stop
                    'Case VB.SyntaxKind.IfDirectiveTrivia
                    '    FirstTrivia = False
                    '    FinalLeadingTriviaList.AddRange(DirectiveNotAllowedHere(Trivia))
                    'Case VB.SyntaxKind.DisabledTextTrivia
                    '    FirstTrivia = False
                    '    FinalLeadingTriviaList.AddRange(DirectiveNotAllowedHere(Trivia))
                    'Case VB.SyntaxKind.ElseDirectiveTrivia
                    '    FirstTrivia = False
                    '    FinalLeadingTriviaList.AddRange(DirectiveNotAllowedHere(Trivia))
                    'Case VB.SyntaxKind.EndIfDirectiveTrivia
                    '    FirstTrivia = False
                    '    FinalLeadingTriviaList.AddRange(DirectiveNotAllowedHere(Trivia))
                Case Else
                    Stop
            End Select
        Next
        Return Expression.WithLeadingTrivia(NewTriviaList)
    End Function

End Module