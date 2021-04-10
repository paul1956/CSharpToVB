' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports CSharpToVBConverter.CSharpToVBVisitors.CSharpConverter
Imports Microsoft.CodeAnalysis
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace Extensions
    Friend Module VariableDeclarationExtensions

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
        Private Function WithModifiedNodeTrailingTrivia(Of T As VB.VisualBasicSyntaxNode)(node As T, separatorFollows As Boolean) As T
            Dim afterComment As Boolean = False
            Dim afterLinefeed As Boolean = False
            Dim afterWhiteSpace As Boolean = False
            Dim finalTrailingTrivia As SyntaxTriviaList
            Dim initialTriviaList As SyntaxTriviaList = node.GetTrailingTrivia
            Dim afterLineContinuation As Boolean = False
            For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                Dim trivia As SyntaxTrivia = e.Value
                Dim nextTrivia As SyntaxTrivia = initialTriviaList.GetForwardTriviaOrDefault(e.Index, lookaheadCount:=1)
                Select Case trivia.RawKind
                    Case VB.SyntaxKind.WhitespaceTrivia
                        If nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            Continue For
                        End If

                        If nextTrivia.IsKind(VB.SyntaxKind.CommentTrivia) OrElse
                            nextTrivia.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                        End If
                        finalTrailingTrivia = finalTrailingTrivia.Add(trivia)
                        afterComment = False
                        afterLineContinuation = False
                        afterLinefeed = False
                        afterWhiteSpace = True
                    Case VB.SyntaxKind.EndOfLineTrivia
                        If nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            Continue For
                        End If
                        If Not afterLinefeed Then
                            If Not (afterComment OrElse afterLineContinuation) Then
                                If separatorFollows Then
                                    finalTrailingTrivia = finalTrailingTrivia.AddRange(SpaceLineContinue)
                                End If
                            End If
                            finalTrailingTrivia = finalTrailingTrivia.Add(trivia)
                            afterComment = False
                            afterLineContinuation = False
                            afterLinefeed = True
                            afterWhiteSpace = False
                        End If
                    Case VB.SyntaxKind.CommentTrivia
                        If Not afterWhiteSpace Then
                            finalTrailingTrivia = finalTrailingTrivia.Add(SpaceTrivia)
                        End If
                        If Not afterLineContinuation Then
                            finalTrailingTrivia = finalTrailingTrivia.AddRange({LineContinuation, SpaceTrivia})
                        End If
                        finalTrailingTrivia = finalTrailingTrivia.Add(trivia)
                        If Not nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            finalTrailingTrivia = finalTrailingTrivia.Add(VbEolTrivia)
                            afterLineContinuation = False
                            afterLinefeed = True
                        End If
                        afterComment = True
                        afterWhiteSpace = False
                    Case VB.SyntaxKind.LineContinuationTrivia
                        If finalTrailingTrivia.Last.IsKind(VB.SyntaxKind.LineContinuationTrivia) Then
                            Continue For
                        End If
                        afterWhiteSpace = False
                        afterLineContinuation = True
                        finalTrailingTrivia = finalTrailingTrivia.Add(LineContinuation)
                    Case VB.SyntaxKind.EndIfDirectiveTrivia
                        finalTrailingTrivia = finalTrailingTrivia.Add(trivia)
                        afterComment = False
                        afterLineContinuation = False
                        afterLinefeed = False
                        afterWhiteSpace = False
                End Select
            Next
            Return node.WithTrailingTrivia(finalTrailingTrivia)
        End Function

        ''' <summary>
        ''' Converts C# VariableDeclaration to VB List(Of VariableDeclaratorSyntax)
        ''' </summary>
        ''' <param name="variableDeclaration"></param>
        ''' <param name="visitor"></param>
        ''' <param name="model"></param>
        ''' <param name="isFieldDeclaration"></param>
        ''' <param name="leadingTrivia"></param>
        ''' <returns></returns>
        ''' <remarks>Fix handling of AddressOf where is mistakenly added to DirectCast and C# Pointers</remarks>
        <Extension>
        Friend Function RemodelVariableDeclaration(variableDeclaration As CSS.VariableDeclarationSyntax, visitor As NodesVisitor, model As SemanticModel, isFieldDeclaration As Boolean, ByRef leadingTrivia As SyntaxTriviaList) As SeparatedSyntaxList(Of VBS.VariableDeclaratorSyntax)
            Dim vbType As VBS.TypeSyntax
            Dim declarationType As VB.VisualBasicSyntaxNode = variableDeclaration.Type.Accept(visitor)
            Dim addNullableTypeSpecifier As Boolean = TypeOf variableDeclaration.Type Is CSS.NullableTypeSyntax AndAlso TypeOf declarationType IsNot VBS.NullableTypeSyntax
            Dim typeOrAddressOf As VB.VisualBasicSyntaxNode = declarationType.WithConvertedLeadingTriviaFrom(variableDeclaration.Type)
            Dim typeLeadingTrivia As SyntaxTriviaList = typeOrAddressOf.GetLeadingTrivia

            If typeLeadingTrivia.Any Then
                If typeLeadingTrivia.Last.RawKind = VB.SyntaxKind.WhitespaceTrivia Then
                    typeOrAddressOf = typeOrAddressOf.WithLeadingTrivia(typeLeadingTrivia.Last)
                Else
                    typeOrAddressOf = typeOrAddressOf.WithLeadingTrivia(SpaceTrivia)
                End If
            End If
            If isFieldDeclaration AndAlso typeLeadingTrivia.Count > 1 Then
                Dim stmtWithIssues As CS.CSharpSyntaxNode = GetStatementWithIssues(variableDeclaration)
                If typeLeadingTrivia.ContainsCommentOrDirectiveTrivia AndAlso Not EndsWithSimilarTrivia(stmtWithIssues.GetLeadingTrivia.ConvertTriviaList(), typeLeadingTrivia) Then
                    stmtWithIssues.AddMarker(Factory.EmptyStatement.WithLeadingTrivia(typeLeadingTrivia), StatementHandlingOption.AppendEmptyStatement, allowDuplicates:=True)
                End If
            End If
            Dim csCollectedCommentTrivia As SyntaxTriviaList
            If typeOrAddressOf.IsKind(VB.SyntaxKind.AddressOfExpression) Then
                vbType = IntPtrType
            Else
                vbType = DirectCast(typeOrAddressOf, VBS.TypeSyntax)
            End If
            vbType = vbType.AdjustNodeTrivia(separatorFollows:=True)
            Dim csDeclaratorsWithoutInitializers As New List(Of CSS.VariableDeclaratorSyntax)()
            Dim vbDeclarators As New List(Of VBS.VariableDeclaratorSyntax)

            For Each variable As CSS.VariableDeclaratorSyntax In variableDeclaration.Variables
                If variable.Initializer Is Nothing Then
                    csDeclaratorsWithoutInitializers.Add(variable.WithAppendedTrailingTrivia(csCollectedCommentTrivia))
                    csCollectedCommentTrivia = New SyntaxTriviaList
                    Continue For
                End If
                Dim asClause As VBS.AsClauseSyntax = Nothing
                Dim csInitializer As CSS.ImplicitObjectCreationExpressionSyntax = Nothing
                If variableDeclaration.Type.IsKind(CS.SyntaxKind.RefType) Then
                ElseIf Not variableDeclaration.Type.IsVar Then
                    If TypeOf variable.Initializer.Value Is CSS.ImplicitObjectCreationExpressionSyntax Then
                        'Using client As New HttpClient() With {.BaseAddress = New Uri("https://jsonplaceholder.typicode.com")}
                        'End Using
                        '        Dim nodesOrTokens As New List(Of SyntaxNodeOrToken) From {
                        '               KeyKeyword,
                        '                KeyKeyword,
                        '                KeyKeyword,
                        '                KeyKeyword,
                        '                KeyKeyword
                        '}

                        csInitializer = CType(variable.Initializer.Value, CSS.ImplicitObjectCreationExpressionSyntax)
                        Dim argumentList As VBS.ArgumentListSyntax = CType(variable.ArgumentList?.Accept(visitor), VBS.ArgumentListSyntax)
                        Dim vbSyntaxNode As VB.VisualBasicSyntaxNode = csInitializer.Accept(visitor)
                        Select Case True
                            Case TypeOf vbSyntaxNode Is VBS.ObjectCreationExpressionSyntax
                                asClause = Factory.AsNewClause(Factory.ObjectCreationExpression(Nothing,
                                                                                                vbType,
                                                                                                CType(vbSyntaxNode, VBS.ObjectCreationExpressionSyntax).ArgumentList,
                                                                                                CType(vbSyntaxNode, VBS.ObjectCreationExpressionSyntax).Initializer))
                            Case TypeOf vbSyntaxNode Is VBS.CollectionInitializerSyntax
                                If argumentList IsNot Nothing Then
                                    Stop
                                End If
                                asClause = Factory.AsNewClause(Factory.ObjectCreationExpression(NewKeyword,
                                                                                                Nothing,
                                                                                                vbType,
                                                                                                Nothing,
                                                                                                Factory.ObjectCollectionInitializer(FromKeyword,
                                                                                                                                    CType(vbSyntaxNode, VBS.CollectionInitializerSyntax))))
                        End Select
                    Else
                        asClause = Factory.SimpleAsClause(vbType)
                    End If
                Else
                    ' Get Type from Initializer
                    If variable.Initializer.Value.IsKind(CS.SyntaxKind.AnonymousObjectCreationExpression) Then
                        asClause = Factory.AsNewClause(CType(variable.Initializer.Value.Accept(visitor), VBS.NewExpressionSyntax))
                    ElseIf variable.Initializer.Value.IsKind(CS.SyntaxKind.ImplicitArrayCreationExpression) Then
                    Else
                        Dim resultTuple As (_Error As Boolean, _TypeSyntax As VBS.TypeSyntax) = variable.Initializer.Value.DetermineTypeSyntax(model)
                        If Not resultTuple._Error Then
                            asClause = Factory.SimpleAsClause(resultTuple._TypeSyntax)
                        Else
                            asClause = Nothing
                        End If
                    End If
                End If
                Dim initializerValue As VBS.ExpressionSyntax = Nothing
                If csInitializer Is Nothing Then
                    initializerValue = CType(variable.Initializer.Value.Accept(visitor), VBS.ExpressionSyntax)
                    If initializerValue Is Nothing Then
                        initializerValue = Factory.IdentifierName("HandleRefExpression").WithConvertedTriviaFrom(variable.Initializer.Value)
                    End If
                    If initializerValue.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        leadingTrivia = leadingTrivia.AddRange(initializerValue.GetLeadingTrivia)
                    End If
                End If
                Dim initializer As VBS.EqualsValueSyntax = Nothing
                If Not asClause.IsKind(VB.SyntaxKind.AsNewClause) Then
                    initializer = Factory.EqualsValue(initializerValue.WithLeadingTrivia(SpaceTrivia))
                    If initializer.Value.IsKind(VB.SyntaxKind.ObjectCreationExpression) Then
                        If asClause Is Nothing Then
                            asClause = Factory.AsNewClause(CType(initializerValue, VBS.ObjectCreationExpressionSyntax))
                            initializer = Nothing
                        Else
                            If CType(asClause, VBS.SimpleAsClauseSyntax).Type.ToString = CType(initializerValue, VBS.ObjectCreationExpressionSyntax).Type.ToString Then
                                asClause = Factory.AsNewClause(CType(initializerValue, VBS.ObjectCreationExpressionSyntax))
                                initializer = Nothing
                            End If
                        End If
                    End If
                End If
                ' Get the names last to lead with var jsonWriter = new JsonWriter(stringWriter)
                ' Which should be Dim jsonWriter = new JsonWriter(stringWriter)
                vbDeclarators.Add(
                    Factory.VariableDeclarator(Factory.SingletonSeparatedList(DirectCast(variable.Accept(visitor), VBS.ModifiedIdentifierSyntax)),
                                                asClause,
                                                initializer
                                                ).WithModifiedNodeTrailingTrivia(separatorFollows:=False)
                                )
            Next
            If csDeclaratorsWithoutInitializers.Any Then
                Dim modifiedIdentifierList As New List(Of VBS.ModifiedIdentifierSyntax)
                For Each csVarDeclaration As CSS.VariableDeclaratorSyntax In csDeclaratorsWithoutInitializers
                    Dim dTrailingTrivia As SyntaxTriviaList = csVarDeclaration.GetTrailingTrivia
                    If csVarDeclaration.HasTrailingTrivia And dTrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                        csCollectedCommentTrivia = csCollectedCommentTrivia.AddRange(dTrailingTrivia)
                    End If
                    Dim modifiedIdentifier As VBS.ModifiedIdentifierSyntax = DirectCast(csVarDeclaration.Accept(visitor), VBS.ModifiedIdentifierSyntax)
                    If addNullableTypeSpecifier Then
                        modifiedIdentifier = Factory.ModifiedIdentifier(modifiedIdentifier.Identifier, QuestionToken, modifiedIdentifier.ArrayBounds, modifiedIdentifier.ArrayRankSpecifiers)
                    End If
                    modifiedIdentifierList.Add(modifiedIdentifier.WithTrailingTrivia(SpaceTrivia))
                Next
                Dim varDeclarator As VBS.VariableDeclaratorSyntax = Factory.VariableDeclarator(Factory.SeparatedList(modifiedIdentifierList), asClause:=Factory.SimpleAsClause(vbType), initializer:=Nothing)
                vbDeclarators.Insert(0, varDeclarator.WithTrailingTrivia(csCollectedCommentTrivia.ConvertTriviaList()))
                csCollectedCommentTrivia = New SyntaxTriviaList
            End If
            If csCollectedCommentTrivia.Any Then
                Dim finalTrivia As SyntaxTriviaList = csCollectedCommentTrivia.ConvertTriviaList()
                finalTrivia = finalTrivia.AddRange(vbDeclarators.Last.GetTrailingTrivia)
                Dim tempDeclarator As VBS.VariableDeclaratorSyntax = vbDeclarators.Last.WithTrailingTrivia(finalTrivia)
                vbDeclarators.RemoveAt(vbDeclarators.Count - 1)
                vbDeclarators.Add(tempDeclarator)
            End If
            Return Factory.SeparatedList(vbDeclarators)
        End Function

    End Module

End Namespace
