' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports CSharpToVBConverter
Imports CSharpToVBConverter.ToVisualBasic.CSharpConverter
Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBConverter
    Public Module ExpressionSyntaxExtensions

        <Extension>
        Friend Function AdjustExpressionTrivia(Expression As VBS.ExpressionSyntax, AdjustLeading As Boolean) As VBS.ExpressionSyntax
            If Expression Is Nothing Then
                Throw New ArgumentNullException(NameOf(Expression))
            End If

            Dim initialTriviaList As SyntaxTriviaList = Expression.GetLeadingTrivia
            Dim newLeadingTrivia As New SyntaxTriviaList

            If AdjustLeading Then
                For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                    Dim Trivia As SyntaxTrivia = e.Value
                    Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(initialTriviaList, e.Index, LookaheadCount:=1)

                    Select Case Trivia.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            Select Case nextTrivia.RawKind
                                Case VB.SyntaxKind.WhitespaceTrivia
                                    newLeadingTrivia = newLeadingTrivia.Add(If(Trivia.Span.Length > nextTrivia.Span.Length, Trivia, nextTrivia))
                                    e.MoveNext()
                                Case VB.SyntaxKind.LineContinuationTrivia
                                    newLeadingTrivia = newLeadingTrivia.Add(VBSpaceTrivia)
                                    newLeadingTrivia = newLeadingTrivia.Add(LineContinuation)
                                    e.MoveNext()
                                    nextTrivia = GetForwardTriviaOrDefault(initialTriviaList, e.Index, LookaheadCount:=1)
                                    If nextTrivia.IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                                        newLeadingTrivia = newLeadingTrivia.Add(If(Trivia.Span.Length > nextTrivia.Span.Length, Trivia, nextTrivia))
                                        e.MoveNext()
                                    End If
                                Case VB.SyntaxKind.None
                                    newLeadingTrivia = newLeadingTrivia.Add(e.Value)
                                Case VB.SyntaxKind.EndOfLineTrivia
                                Case VB.SyntaxKind.CommentTrivia,
                             VB.SyntaxKind.DocumentationCommentTrivia
                                    newLeadingTrivia = newLeadingTrivia.Add(VBSpaceTrivia)
                                    newLeadingTrivia = newLeadingTrivia.Add(LineContinuation)
                                    newLeadingTrivia = newLeadingTrivia.Add(e.Value)
                                Case Else
                                    If e.Value.IsDirective Then
                                        newLeadingTrivia = newLeadingTrivia.Add(e.Value)
                                    Else
                                        Stop
                                    End If
                            End Select
                        Case VB.SyntaxKind.EndOfLineTrivia
                            If e.IsFirst Then
                                Continue For
                            End If
                            newLeadingTrivia = newLeadingTrivia.Add(Trivia)
                        Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                            Select Case nextTrivia.RawKind
                                Case VB.SyntaxKind.WhitespaceTrivia, VB.SyntaxKind.LineContinuationTrivia
                                    newLeadingTrivia = newLeadingTrivia.Add(Trivia)
                                    e.MoveNext()
                                Case VB.SyntaxKind.None
                                    newLeadingTrivia = newLeadingTrivia.Add(Trivia)
                                    newLeadingTrivia = newLeadingTrivia.Add(VBEOLTrivia)
                                Case VB.SyntaxKind.EndOfLineTrivia
                                    newLeadingTrivia = newLeadingTrivia.Add(Trivia)
                                Case VB.SyntaxKind.EndOfLineTrivia
                                    newLeadingTrivia = newLeadingTrivia.Add(Trivia)
                                Case Else
                                    Stop
                            End Select
                        Case VB.SyntaxKind.LineContinuationTrivia
                            newLeadingTrivia = newLeadingTrivia.Add(Trivia)
                        Case Else
                            If Trivia.IsDirective Then
                                newLeadingTrivia = newLeadingTrivia.Add(Trivia)
                            Else
                                Stop
                            End If
                    End Select
                Next
                Expression = Expression.WithLeadingTrivia(newLeadingTrivia)
            End If

            initialTriviaList = Expression.GetTrailingTrivia
            Dim NewTrailingTrivia As New SyntaxTriviaList

            For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(initialTriviaList, e.Index, LookaheadCount:=1)
                Select Case e.Value.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        Select Case nextTrivia.RawKind
                            Case VB.SyntaxKind.None
                                NewTrailingTrivia = NewTrailingTrivia.Add(e.Value)
                            Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                                NewTrailingTrivia = NewTrailingTrivia.Add(VBSpaceTrivia)
                                NewTrailingTrivia = NewTrailingTrivia.Add(LineContinuation)
                                NewTrailingTrivia = NewTrailingTrivia.Add(e.Value)
                            Case VB.SyntaxKind.WhitespaceTrivia,
                             VB.SyntaxKind.EndOfLineTrivia
                            Case VB.SyntaxKind.LineContinuationTrivia
                                NewTrailingTrivia = NewTrailingTrivia.Add(e.Value)
                            Case Else
                                Stop
                                NewTrailingTrivia = NewTrailingTrivia.Add(e.Value)
                        End Select
                    Case VB.SyntaxKind.EndOfLineTrivia
                        Select Case nextTrivia.RawKind
                            Case VB.SyntaxKind.EndOfLineTrivia
                                ' skip it
                            Case VB.SyntaxKind.None
                                If Not NewTrailingTrivia.Any Then
                                    NewTrailingTrivia = NewTrailingTrivia.Add(VBSpaceTrivia)
                                    NewTrailingTrivia = NewTrailingTrivia.Add(LineContinuation)
                                    NewTrailingTrivia = NewTrailingTrivia.Add(e.Value)
                                End If
                            Case VB.SyntaxKind.WhitespaceTrivia
                                NewTrailingTrivia = NewTrailingTrivia.Add(VBSpaceTrivia)
                                NewTrailingTrivia = NewTrailingTrivia.Add(LineContinuation)
                                NewTrailingTrivia = NewTrailingTrivia.Add(e.Value)
                                e.MoveNext()
                            Case Else
                                Stop
                                NewTrailingTrivia = NewTrailingTrivia.Add(e.Value)
                        End Select
                    Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                        NewTrailingTrivia = NewTrailingTrivia.Add(e.Value)
                        NewTrailingTrivia = NewTrailingTrivia.Add(VBEOLTrivia)
                        If nextTrivia.IsEndOfLine Then
                            e.MoveNext()
                        End If
                    Case VB.SyntaxKind.LineContinuationTrivia
                        NewTrailingTrivia = NewTrailingTrivia.Add(e.Value) ' _
                        If nextTrivia.IsKind(VB.SyntaxKind.WhitespaceTrivia) OrElse
                        nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            e.MoveNext()
                            NewTrailingTrivia = NewTrailingTrivia.Add(e.Value)
                        End If
                    Case Else
                        Stop
                End Select
            Next
            Expression = Expression.WithTrailingTrivia(NewTrailingTrivia)
            'Debug.WriteLine($"Exp Out         :{Expression.ToFullString}")
            Return Expression
        End Function

        <Extension>
        Friend Function ConvertDirectiveCondition(Condition As CSS.ExpressionSyntax) As String
            Return Condition.ToString.ConvertCondition
        End Function

        Friend Function CreateArgList(Of T As VBS.ExpressionSyntax)(ParamArray args() As T) As VBS.ArgumentListSyntax
            Return CreateVbArgList(args)
        End Function

        <Extension>
        Friend Function CreateVbArgList(Of T As VBS.ExpressionSyntax)(argExpressions As IEnumerable(Of T)) As VBS.ArgumentListSyntax
            Return Factory.ArgumentList(Factory.SeparatedList(argExpressions.Select(Function(e) CType(Factory.SimpleArgument(e), VBS.ArgumentSyntax))))
        End Function

        <Extension>
        Friend Function DetermineType(expression As CSS.ExpressionSyntax, Model As SemanticModel) As (_Error As Boolean, _ITypeSymbol As ITypeSymbol)
            ' If a parameter appears to have a void return type, then just use 'object' instead.
            Try
                If expression IsNot Nothing Then
                    Dim typeInfo As TypeInfo = Model.GetTypeInfo(expression)
                    Dim symbolInfo As SymbolInfo = Model.GetSymbolInfo(expression)
                    If typeInfo.Type IsNot Nothing Then
                        If typeInfo.Type.IsErrorType Then
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
                        If typeInfo.Type.IsErrorType Then
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
                                Return (_Error:=False, _type.ToString.ConvertCSStringToName)
                            End If
                        End If

                        Dim typeSymbol As ITypeSymbol = symbol.ConvertISymbolToType(Model.Compilation)
                        Return (_Error:=False, typeSymbol.ConvertToType)
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
        Friend Function GetExpressionBodyStatements(ArrowExpressionClause As CSS.ArrowExpressionClauseSyntax, visitor As NodesVisitor) As SyntaxList(Of VBS.StatementSyntax)
            Dim Statement As VBS.StatementSyntax
            Dim ExpressionBody As VB.VisualBasicSyntaxNode = ArrowExpressionClause.Accept(visitor)
            If TypeOf ExpressionBody Is VBS.TryBlockSyntax Then
                Dim TryBlock As VBS.TryBlockSyntax = CType(ExpressionBody, VBS.TryBlockSyntax)
                Dim StatementList As SyntaxList(Of VBS.StatementSyntax) = ReplaceOneStatementWithMarkedStatements(ArrowExpressionClause, TryBlock.Statements(0))
                For Each e As IndexClass(Of VBS.StatementSyntax) In TryBlock.Statements.WithIndex
                    StatementList = StatementList.Add(e.Value)
                Next
                Return StatementList
            ElseIf TypeOf ExpressionBody Is VBS.AssignmentStatementSyntax OrElse
                        TypeOf ExpressionBody Is VBS.AddRemoveHandlerStatementSyntax OrElse
                        TypeOf ExpressionBody Is VBS.RaiseEventStatementSyntax OrElse
                        TypeOf ExpressionBody Is VBS.ThrowStatementSyntax Then
                Statement = DirectCast(ExpressionBody, VBS.StatementSyntax).WithTrailingEOL(RemoveLastLineContinuation:=True)
            ElseIf ArrowExpressionClause.Parent.IsKind(CS.SyntaxKind.SetAccessorDeclaration) Then
                Statement = Factory.ExpressionStatement(CType(ExpressionBody, VBS.ExpressionSyntax))
            Else
                Statement = Factory.ReturnStatement(DirectCast(ExpressionBody.WithLeadingTrivia(VBSpaceTrivia), VBS.ExpressionSyntax)) _
                                        .WithLeadingTrivia(ExpressionBody.GetLeadingTrivia)
            End If
            Return ReplaceOneStatementWithMarkedStatements(ArrowExpressionClause, Statement.WithTrailingEOL(RemoveLastLineContinuation:=True))
        End Function

        <Extension>
        Friend Function IsReferenceComparison(Expression1 As CSS.ExpressionSyntax, Expression2 As CSS.ExpressionSyntax, Model As SemanticModel) As Boolean
            Dim TypeSymbol1 As (IsRefType As Boolean, IsString As Boolean) = IsReferenceTypeOrString(Expression1, Model)
            Dim TypeSymbol2 As (IsRefType As Boolean, IsString As Boolean) = IsReferenceTypeOrString(Expression2, Model)
            Return TypeSymbol1.IsRefType AndAlso TypeSymbol2.IsRefType AndAlso Not (TypeSymbol1.IsString AndAlso TypeSymbol2.IsString)
        End Function

        <Extension>
        Friend Function IsReferenceTypeOrString(Expression1 As CSS.ExpressionSyntax, Model As SemanticModel) As (IsRefType As Boolean, IsString As Boolean)
            Dim TypeSymbol1 As (_Error As Boolean, _ITypeSymbol As ITypeSymbol) = DetermineType(Expression1, Model)
            If TypeSymbol1._Error Then
                Return (False, False)
            End If
            Return (TypeSymbol1._ITypeSymbol.IsReferenceType, TypeSymbol1._ITypeSymbol.Name = "String")
        End Function

        <Extension>
        Friend Function IsReturnValueDiscarded(node As CSS.ExpressionSyntax) As Boolean
            If TypeOf node.Parent Is CSS.ExpressionStatementSyntax Then
                Dim csExpression As CSS.ExpressionStatementSyntax = CType(node.Parent, CSS.ExpressionStatementSyntax)
                If TypeOf csExpression.Expression Is CSS.AssignmentExpressionSyntax Then
                    Dim AssignmentExpression As CSS.AssignmentExpressionSyntax = CType(csExpression.Expression, CSS.AssignmentExpressionSyntax)
                    If TypeOf AssignmentExpression.Left Is CSS.DeclarationExpressionSyntax OrElse
                     TypeOf AssignmentExpression.Left Is CSS.TupleExpressionSyntax Then
                        Return False
                    End If
                End If
                Return True
            End If

            Return TypeOf node.Parent Is CSS.SimpleLambdaExpressionSyntax OrElse
               TypeOf node.Parent Is CSS.ForStatementSyntax OrElse
               node.Parent.IsParentKind(CS.SyntaxKind.SetAccessorDeclaration)
        End Function

        <Extension>
        Friend Function WithoutLeadingSystemDot(Expression As VBS.ExpressionSyntax) As VBS.ExpressionSyntax

            If Expression.StartsWithSystemDot Then
                Return Factory.ParseExpression(Expression.ToString.Substring("System.".Length)).WithTriviaFrom(Expression)
            End If
            Return Expression
        End Function

    End Module
End Namespace
