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
        Friend Function AdjustExpressionTrivia(Of T As VB.VisualBasicSyntaxNode)(Expression As T, AdjustLeading As Boolean) As T
            If Expression Is Nothing Then
                Throw New ArgumentNullException(NameOf(Expression))
            End If

            Dim initialTriviaList As SyntaxTriviaList = Expression.GetLeadingTrivia
            Dim newLeadingTrivia As New SyntaxTriviaList

            If AdjustLeading Then
                For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                    Dim trivia As SyntaxTrivia = e.Value
                    Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(initialTriviaList, e.index, LookaheadCount:=1)

                    Select Case trivia.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            Select Case nextTrivia.RawKind
                                Case VB.SyntaxKind.WhitespaceTrivia
                                    newLeadingTrivia = newLeadingTrivia.Add(If(trivia.Span.Length > nextTrivia.Span.Length, trivia, nextTrivia))
                                    e.MoveNext()
                                Case VB.SyntaxKind.LineContinuationTrivia
                                    newLeadingTrivia = newLeadingTrivia.Add(Factory.Space)
                                    newLeadingTrivia = newLeadingTrivia.Add(LineContinuation)
                                    e.MoveNext()
                                    nextTrivia = GetForwardTriviaOrDefault(initialTriviaList, e.index, LookaheadCount:=1)
                                    If nextTrivia.IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                                        newLeadingTrivia = newLeadingTrivia.Add(If(trivia.Span.Length > nextTrivia.Span.Length, trivia, nextTrivia))
                                        e.MoveNext()
                                    End If
                                Case VB.SyntaxKind.None
                                    newLeadingTrivia = newLeadingTrivia.Add(e.Value)
                                Case VB.SyntaxKind.EndOfLineTrivia
                                Case VB.SyntaxKind.CommentTrivia,
                             VB.SyntaxKind.DocumentationCommentTrivia
                                    newLeadingTrivia = newLeadingTrivia.Add(Factory.Space)
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
                            newLeadingTrivia = newLeadingTrivia.Add(trivia)
                        Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                            Select Case nextTrivia.RawKind
                                Case VB.SyntaxKind.WhitespaceTrivia, VB.SyntaxKind.LineContinuationTrivia
                                    newLeadingTrivia = newLeadingTrivia.Add(trivia)
                                    e.MoveNext()
                                Case VB.SyntaxKind.None
                                    newLeadingTrivia = newLeadingTrivia.Add(trivia)
                                    newLeadingTrivia = newLeadingTrivia.Add(VBEOLTrivia)
                                Case VB.SyntaxKind.EndOfLineTrivia
                                    newLeadingTrivia = newLeadingTrivia.Add(trivia)
                                Case VB.SyntaxKind.EndOfLineTrivia
                                    newLeadingTrivia = newLeadingTrivia.Add(trivia)
                                Case Else
                                    Stop
                            End Select
                        Case VB.SyntaxKind.LineContinuationTrivia
                            newLeadingTrivia = newLeadingTrivia.Add(trivia)
                        Case Else
                            If trivia.IsDirective Then
                                newLeadingTrivia = newLeadingTrivia.Add(trivia)
                            Else
                                Stop
                            End If
                    End Select
                Next
                Expression = Expression.WithLeadingTrivia(newLeadingTrivia)
            End If

            initialTriviaList = Expression.GetTrailingTrivia
            Dim newTrailingTrivia As New SyntaxTriviaList

            For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(initialTriviaList, e.index, LookaheadCount:=1)
                Select Case e.Value.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        Select Case nextTrivia.RawKind
                            Case VB.SyntaxKind.None
                                newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                            Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                                newTrailingTrivia = newTrailingTrivia.Add(Factory.Space)
                                newTrailingTrivia = newTrailingTrivia.Add(LineContinuation)
                                newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                            Case VB.SyntaxKind.WhitespaceTrivia,
                             VB.SyntaxKind.EndOfLineTrivia
                            Case VB.SyntaxKind.LineContinuationTrivia
                                newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                            Case Else
                                Stop
                                newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                        End Select
                    Case VB.SyntaxKind.EndOfLineTrivia
                        Select Case nextTrivia.RawKind
                            Case VB.SyntaxKind.EndOfLineTrivia
                                ' skip it
                            Case VB.SyntaxKind.None
                                If Not newTrailingTrivia.Any Then
                                    newTrailingTrivia = newTrailingTrivia.Add(Factory.Space)
                                    newTrailingTrivia = newTrailingTrivia.Add(LineContinuation)
                                    newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                                End If
                            Case VB.SyntaxKind.WhitespaceTrivia
                                newTrailingTrivia = newTrailingTrivia.Add(Factory.Space)
                                newTrailingTrivia = newTrailingTrivia.Add(LineContinuation)
                                newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                                e.MoveNext()
                            Case Else
                                Stop
                                newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                        End Select
                    Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                        newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                        newTrailingTrivia = newTrailingTrivia.Add(VBEOLTrivia)
                        If nextTrivia.IsEndOfLine Then
                            e.MoveNext()
                        End If
                    Case VB.SyntaxKind.LineContinuationTrivia
                        newTrailingTrivia = newTrailingTrivia.Add(e.Value) ' _
                        If nextTrivia.IsKind(VB.SyntaxKind.WhitespaceTrivia) OrElse
                        nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            e.MoveNext()
                            newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                        End If
                    Case Else
                        Stop
                End Select
            Next
            Expression = Expression.WithTrailingTrivia(newTrailingTrivia)
            'Debug.WriteLine($"Exp Out         :{Expression.ToFullString}")
            Return Expression
        End Function

        <Extension>
        Friend Function ConvertDirectiveCondition(condition As CSS.ExpressionSyntax) As String
            Return condition.ToString.ConvertCondition
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
                        Dim type As ITypeSymbol = TryCast(symbol, ITypeSymbol)
                        If type IsNot Nothing Then
                            If symbol.Kind = SymbolKind.PointerType Then
                                Return (_Error:=True, type)
                            End If
                            If type.ToString.Contains("<anonymous type", StringComparison.Ordinal) OrElse
                            type.ToString.StartsWith("(", StringComparison.Ordinal) Then
                                Return (_Error:=True, type)
                            End If
                            Return (_Error:=False, type)
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
                        Dim type As ITypeSymbol = TryCast(symbol, ITypeSymbol)
                        If type IsNot Nothing Then
                            If symbol.Kind = SymbolKind.PointerType Then
                                Return (_Error:=True, IntPtrType)
                            End If
                            If type.ToString.Contains("<anonymous type", StringComparison.Ordinal) Then
                                Return (_Error:=True, PredefinedTypeObject)
                            End If
                            If type.ToString.StartsWith("(", StringComparison.Ordinal) Then
                                Return (_Error:=False, type.ToString.ConvertCSStringToName)
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
        Friend Function GetExpressionBodyStatements(ArrowExpressionClause As CSS.ArrowExpressionClauseSyntax, IsReturnVoid As Boolean, visitor As NodesVisitor) As SyntaxList(Of VBS.StatementSyntax)
            Dim statement As VBS.StatementSyntax
            Dim expressionBody As VB.VisualBasicSyntaxNode = ArrowExpressionClause.Accept(visitor)
            Dim leadingComments As SyntaxTriviaList
            leadingComments = expressionBody.GetLeadingTrivia
            expressionBody = expressionBody.WithLeadingTrivia(Factory.Space)
            If TypeOf expressionBody Is VBS.TryBlockSyntax Then
                Dim tryBlock As VBS.TryBlockSyntax = CType(expressionBody, VBS.TryBlockSyntax)
                Dim statementList As SyntaxList(Of VBS.StatementSyntax) = ReplaceOneStatementWithMarkedStatements(ArrowExpressionClause, tryBlock.Statements(0))
                For Each e As IndexClass(Of VBS.StatementSyntax) In tryBlock.Statements.WithIndex
                    statementList = statementList.Add(e.Value)
                Next
                Return statementList
            ElseIf TypeOf expressionBody Is VBS.AssignmentStatementSyntax OrElse
                        TypeOf expressionBody Is VBS.AddRemoveHandlerStatementSyntax OrElse
                        TypeOf expressionBody Is VBS.RaiseEventStatementSyntax OrElse
                        TypeOf expressionBody Is VBS.ThrowStatementSyntax Then
                statement = DirectCast(expressionBody, VBS.StatementSyntax).WithTrailingEOL
            ElseIf ArrowExpressionClause.Parent.IsKind(CS.SyntaxKind.SetAccessorDeclaration) OrElse IsReturnVoid Then
                If TypeOf expressionBody Is VBS.ObjectCreationExpressionSyntax Then
                    statement = FactoryDimStatement("tempVar", Factory.AsNewClause(CType(expressionBody, VBS.NewExpressionSyntax)), Nothing).WithPrependedLeadingTrivia(leadingComments)
                ElseIf TypeOf expressionBody Is VBS.InvocationExpressionSyntax Then
                    statement = Factory.CallStatement(CType(expressionBody, VBS.InvocationExpressionSyntax)).WithPrependedLeadingTrivia(leadingComments)
                Else
                    statement = Factory.ExpressionStatement(CType(expressionBody, VBS.ExpressionSyntax)).WithPrependedLeadingTrivia(leadingComments)
                End If
            Else
                statement = Factory.ReturnStatement(DirectCast(expressionBody.WithLeadingTrivia(Factory.Space), VBS.ExpressionSyntax)) _
                                        .WithLeadingTrivia(leadingComments)
            End If
            Return ReplaceOneStatementWithMarkedStatements(ArrowExpressionClause, statement.WithTrailingEOL)
        End Function

        <Extension>
        Friend Function IsReferenceComparison(Expression1 As CSS.ExpressionSyntax, Expression2 As CSS.ExpressionSyntax, Model As SemanticModel) As Boolean
            Dim typeSymbol1 As (IsRefType As Boolean, IsString As Boolean) = IsReferenceTypeOrString(Expression1, Model)
            Dim typeSymbol2 As (IsRefType As Boolean, IsString As Boolean) = IsReferenceTypeOrString(Expression2, Model)
            Return typeSymbol1.IsRefType AndAlso typeSymbol2.IsRefType AndAlso Not (typeSymbol1.IsString AndAlso typeSymbol2.IsString)
        End Function

        <Extension>
        Friend Function IsReferenceTypeOrString(Expression1 As CSS.ExpressionSyntax, Model As SemanticModel) As (IsRefType As Boolean, IsString As Boolean)
            Dim typeSymbol1 As (_Error As Boolean, _ITypeSymbol As ITypeSymbol) = DetermineType(Expression1, Model)
            If typeSymbol1._Error Then
                Return (False, False)
            End If
            Return (typeSymbol1._ITypeSymbol.IsReferenceType, typeSymbol1._ITypeSymbol.Name = "String")
        End Function

        <Extension>
        Friend Function IsReturnValueDiscarded(node As CSS.ExpressionSyntax) As Boolean
            If TypeOf node.Parent Is CSS.ExpressionStatementSyntax Then
                Dim csExpression As CSS.ExpressionStatementSyntax = CType(node.Parent, CSS.ExpressionStatementSyntax)
                If TypeOf csExpression.Expression Is CSS.AssignmentExpressionSyntax Then
                    Dim assignmentExpression As CSS.AssignmentExpressionSyntax = CType(csExpression.Expression, CSS.AssignmentExpressionSyntax)
                    If TypeOf assignmentExpression.Left Is CSS.DeclarationExpressionSyntax OrElse
                     TypeOf assignmentExpression.Left Is CSS.TupleExpressionSyntax Then
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
