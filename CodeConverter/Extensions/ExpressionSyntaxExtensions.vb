' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports CSharpToVBConverter.CSharpToVBVisitors.CSharpConverter
Imports Microsoft.CodeAnalysis
Imports Utilities

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace Extensions
    Public Module ExpressionSyntaxExtensions

        <Extension>
        Private Function CreateVbArgList(Of T As VBS.ExpressionSyntax)(argExpressions As IEnumerable(Of T)) As VBS.ArgumentListSyntax
            Return Factory.ArgumentList(Factory.SeparatedList(argExpressions.Select(Function(e) CType(Factory.SimpleArgument(e), VBS.ArgumentSyntax))))
        End Function

        <Extension>
        Private Function StartsWithSystemDot(expression As SyntaxNode) As Boolean
            Return expression.ToString.StartsWith("System.", StringComparison.Ordinal)
        End Function

        <Extension>
        Friend Function AdjustExpressionTrivia(Of T As VB.VisualBasicSyntaxNode)(expression As T, adjustLeading As Boolean, directiveNotAllowed As Boolean) As T
            If expression Is Nothing Then
                Throw New ArgumentNullException(NameOf(expression))
            End If

            Dim initialTriviaList As SyntaxTriviaList = expression.GetLeadingTrivia
            Dim newLeadingTrivia As New SyntaxTriviaList

            If adjustLeading Then
                Dim afterEol As Boolean
                For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                    Dim trivia As SyntaxTrivia = e.Value
                    Dim nextTrivia As SyntaxTrivia = initialTriviaList.GetForwardTriviaOrDefault(e.Index, lookaheadCount:=1)

                    Select Case trivia.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            Select Case nextTrivia.RawKind
                                Case VB.SyntaxKind.WhitespaceTrivia
                                    newLeadingTrivia = newLeadingTrivia.Add(If(trivia.Span.Length > nextTrivia.Span.Length, trivia, nextTrivia))
                                    e.MoveNext()
                                Case VB.SyntaxKind.LineContinuationTrivia
                                    newLeadingTrivia = newLeadingTrivia.Add(SpaceTrivia)
                                    newLeadingTrivia = newLeadingTrivia.Add(LineContinuation)
                                    e.MoveNext()
                                    nextTrivia = initialTriviaList.GetForwardTriviaOrDefault(e.Index, lookaheadCount:=1)
                                    If nextTrivia.IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                                        newLeadingTrivia = newLeadingTrivia.Add(If(trivia.Span.Length > nextTrivia.Span.Length, trivia, nextTrivia))
                                        e.MoveNext()
                                    End If
                                Case VB.SyntaxKind.None
                                    newLeadingTrivia = newLeadingTrivia.Add(e.Value)
                                Case VB.SyntaxKind.EndOfLineTrivia
                                Case VB.SyntaxKind.CommentTrivia,
                             VB.SyntaxKind.DocumentationCommentTrivia
                                    newLeadingTrivia = newLeadingTrivia.Add(SpaceTrivia)
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
                                    newLeadingTrivia = newLeadingTrivia.Add(VbEolTrivia)
                                Case VB.SyntaxKind.EndOfLineTrivia
                                    newLeadingTrivia = newLeadingTrivia.Add(trivia)
                                Case VB.SyntaxKind.EndOfLineTrivia
                                    newLeadingTrivia = newLeadingTrivia.Add(trivia)
                                Case Else
                                    Stop
                            End Select
                        Case VB.SyntaxKind.LineContinuationTrivia
                            newLeadingTrivia = newLeadingTrivia.Add(trivia)
                        Case VB.SyntaxKind.DisabledTextTrivia
                            If directiveNotAllowed Then
                                newLeadingTrivia = newLeadingTrivia.AddRange(trivia.DirectiveNotAllowedHere(afterEol))
                            Else
                                newLeadingTrivia = newLeadingTrivia.Add(trivia)
                            End If
                        Case Else
                            If trivia.IsDirective Then
                                If directiveNotAllowed Then
                                    newLeadingTrivia = newLeadingTrivia.AddRange(trivia.DirectiveNotAllowedHere(afterEol))
                                Else
                                    newLeadingTrivia = newLeadingTrivia.Add(trivia)
                                End If
                            Else
                                Stop
                            End If
                    End Select
                Next
                expression = expression.WithLeadingTrivia(newLeadingTrivia)
            End If

            initialTriviaList = expression.GetTrailingTrivia
            Dim newTrailingTrivia As New SyntaxTriviaList

            For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                Dim nextTrivia As SyntaxTrivia = initialTriviaList.GetForwardTriviaOrDefault(e.Index, lookaheadCount:=1)
                Select Case e.Value.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        Select Case nextTrivia.RawKind
                            Case VB.SyntaxKind.None
                                newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                            Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                                newTrailingTrivia = newTrailingTrivia.Add(SpaceTrivia)
                                newTrailingTrivia = newTrailingTrivia.Add(LineContinuation)
                                newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                            Case VB.SyntaxKind.WhitespaceTrivia,
                             VB.SyntaxKind.EndOfLineTrivia
                            Case VB.SyntaxKind.LineContinuationTrivia
                                newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                            Case Else
                                newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                        End Select
                    Case VB.SyntaxKind.EndOfLineTrivia
                        Select Case nextTrivia.RawKind
                            Case VB.SyntaxKind.EndOfLineTrivia
                                ' skip it
                            Case VB.SyntaxKind.None
                                If Not newTrailingTrivia.Any Then
                                    newTrailingTrivia = newTrailingTrivia.Add(SpaceTrivia)
                                    newTrailingTrivia = newTrailingTrivia.Add(LineContinuation)
                                    newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                                End If
                            Case VB.SyntaxKind.WhitespaceTrivia
                                newTrailingTrivia = newTrailingTrivia.Add(SpaceTrivia)
                                newTrailingTrivia = newTrailingTrivia.Add(LineContinuation)
                                newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                                e.MoveNext()
                            Case Else
                                newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                        End Select
                    Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                        newTrailingTrivia = newTrailingTrivia.Add(e.Value)
                        newTrailingTrivia = newTrailingTrivia.Add(VbEolTrivia)
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
            expression = expression.WithTrailingTrivia(newTrailingTrivia)
            'Debug.WriteLine($"Exp Out         :{Expression.ToFullString}")
            Return expression
        End Function

        <Extension>
        Friend Function ConvertDirectiveCondition(condition As CSS.ExpressionSyntax) As String
            Return condition.ToString.ConvertCondition
        End Function

        Friend Function CreateArgList(Of T As VBS.ExpressionSyntax)(ParamArray args() As T) As VBS.ArgumentListSyntax
            Return args.CreateVbArgList()
        End Function

        <Extension>
        Friend Function DetermineType(expression As CSS.ExpressionSyntax, model As SemanticModel) As (_Error As Boolean, _ITypeSymbol As ITypeSymbol)
            ' If a parameter appears to have a void return type, then just use 'object' instead.
            Try
                If expression IsNot Nothing Then
                    Dim typeInfo As TypeInfo = model.GetTypeInfo(expression)
                    Dim symbolInfo As SymbolInfo = model.GetSymbolInfo(expression)
                    If typeInfo.Type IsNot Nothing Then
                        If typeInfo.Type.IsErrorType Then
                            Return (_Error:=True, model.Compilation.ObjectType)
                        ElseIf SymbolEqualityComparer.Default.Equals(typeInfo.Type, model.Compilation.ObjectType) Then
                            Return (_Error:=False, model.Compilation.ObjectType)
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
                        Return (_Error:=False, symbol.ConvertISymbolToType(model.Compilation))
                    End If

                End If
            Catch ex As OperationCanceledException
                Throw
            End Try
            Return (_Error:=True, model.Compilation.ObjectType)
        End Function

        <Extension>
        Friend Function DetermineTypeSyntax(expression As CSS.ExpressionSyntax, model As SemanticModel) As (_Error As Boolean, _ITypeSymbol As VBS.TypeSyntax)
            ' If a parameter appears to have a void return type, then just use 'object' instead.
            Try
                If expression IsNot Nothing Then
                    Dim typeInfo As TypeInfo = model.GetTypeInfo(expression)
                    Dim symbolInfo As SymbolInfo = model.GetSymbolInfo(expression)
                    If typeInfo.Type IsNot Nothing Then
                        If typeInfo.Type.IsErrorType Then
                            Return (_Error:=True, PredefinedTypeObject)
                        ElseIf SymbolEqualityComparer.Default.Equals(typeInfo.Type, model.Compilation.ObjectType) Then
                            Return (_Error:=False, PredefinedTypeObject)
                        ElseIf typeInfo.Type.ToString.StartsWith("System.ValueTuple") Then
                            Return (_Error:=False, Factory.ParseTypeName(typeInfo.Type.ToString.Replace("<", "(Of ").Replace(">", ")")))
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
                                Return (_Error:=False, type.ToString.ConvertCsStringToName)
                            End If
                        End If

                        Dim typeSymbol As ITypeSymbol = symbol.ConvertISymbolToType(model.Compilation)
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
        Friend Function GetExpressionBodyStatements(arrowExpressionClause As CSS.ArrowExpressionClauseSyntax, isReturnVoid As Boolean, visitor As NodesVisitor) As SyntaxList(Of VBS.StatementSyntax)
            Dim statement As VBS.StatementSyntax
            Dim expressionBody As VB.VisualBasicSyntaxNode = arrowExpressionClause.Accept(visitor)
            Dim leadingComments As SyntaxTriviaList
            leadingComments = expressionBody.GetLeadingTrivia
            expressionBody = expressionBody.WithLeadingTrivia(SpaceTrivia)
            If TypeOf expressionBody Is VBS.TryBlockSyntax Then
                Dim tryBlock As VBS.TryBlockSyntax = CType(expressionBody, VBS.TryBlockSyntax)
                Dim statementList As SyntaxList(Of VBS.StatementSyntax) = ReplaceOneStatementWithMarkedStatements(arrowExpressionClause, tryBlock.Statements(0))
                For Each e As IndexClass(Of VBS.StatementSyntax) In tryBlock.Statements.WithIndex
                    statementList = statementList.Add(e.Value)
                Next
                Return statementList
            ElseIf TypeOf expressionBody Is VBS.AssignmentStatementSyntax OrElse
                        TypeOf expressionBody Is VBS.AddRemoveHandlerStatementSyntax OrElse
                        TypeOf expressionBody Is VBS.RaiseEventStatementSyntax OrElse
                        TypeOf expressionBody Is VBS.ThrowStatementSyntax Then
                statement = DirectCast(expressionBody, VBS.StatementSyntax).WithTrailingEol
            ElseIf arrowExpressionClause.Parent.IsKind(CS.SyntaxKind.SetAccessorDeclaration) OrElse isReturnVoid Then
                If TypeOf expressionBody Is VBS.ObjectCreationExpressionSyntax Then
                    statement = FactoryDimStatement("tempVar", Factory.AsNewClause(CType(expressionBody, VBS.NewExpressionSyntax)), Nothing).WithPrependedLeadingTrivia(leadingComments)
                ElseIf TypeOf expressionBody Is VBS.InvocationExpressionSyntax Then
                    statement = Factory.CallStatement(CType(expressionBody, VBS.InvocationExpressionSyntax)).WithPrependedLeadingTrivia(leadingComments)
                Else
                    statement = Factory.ExpressionStatement(CType(expressionBody, VBS.ExpressionSyntax)).WithPrependedLeadingTrivia(leadingComments)
                End If
            Else
                statement = Factory.ReturnStatement(DirectCast(expressionBody.WithLeadingTrivia(SpaceTrivia), VBS.ExpressionSyntax)) _
                                        .WithLeadingTrivia(leadingComments)
            End If
            Return ReplaceOneStatementWithMarkedStatements(arrowExpressionClause, statement.WithTrailingEol)
        End Function

        <Extension>
        Friend Function IsReferenceComparison(expression1 As CSS.ExpressionSyntax, expression2 As CSS.ExpressionSyntax, model As SemanticModel) As Boolean
            Dim typeSymbol1 As (IsRefType As Boolean, IsString As Boolean) = expression1.IsReferenceTypeOrString(model)
            Dim typeSymbol2 As (IsRefType As Boolean, IsString As Boolean) = expression2.IsReferenceTypeOrString(model)
            Return typeSymbol1.IsRefType AndAlso typeSymbol2.IsRefType AndAlso Not (typeSymbol1.IsString OrElse typeSymbol2.IsString)
        End Function

        <Extension>
        Friend Function IsReferenceTypeOrString(expression1 As CSS.ExpressionSyntax, model As SemanticModel) As (IsRefType As Boolean, IsString As Boolean)
            Dim typeSymbol1 As (_Error As Boolean, _ITypeSymbol As ITypeSymbol) = expression1.DetermineType(model)
            If typeSymbol1._Error Then
                Return (False, False)
            End If
            Return (typeSymbol1._ITypeSymbol.IsReferenceType, typeSymbol1._ITypeSymbol.Name = "String")
        End Function

        <Extension>
        Friend Function IsReturnValueDiscarded(node As CSS.ExpressionSyntax) As Boolean
            Dim csExpression As CSS.ExpressionStatementSyntax = TryCast(node.Parent, CSS.ExpressionStatementSyntax)
            If csExpression IsNot Nothing Then
                Dim assignmentExpression As CSS.AssignmentExpressionSyntax = TryCast(csExpression.Expression, CSS.AssignmentExpressionSyntax)
                If assignmentExpression IsNot Nothing Then
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
        Friend Function WithoutLeadingSystemDot(expression As VBS.ExpressionSyntax) As VBS.ExpressionSyntax

            If expression.StartsWithSystemDot Then
                Return Factory.ParseExpression(expression.ToString.Substring("System.".Length)).WithTriviaFrom(expression)
            End If
            Return expression
        End Function

    End Module
End Namespace
