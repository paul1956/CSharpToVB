' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports CSharpToVBConverter.ToVisualBasic.CSharpConverter
Imports Microsoft.CodeAnalysis
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBConverter.ToVisualBasic
    Friend Module VariableDeclarationExtensions

        ''' <summary>
        ''' Converts C# VariableDeclaration to VB List(Of VariableDeclaratorSyntax)
        ''' </summary>
        ''' <param name="variableDeclaration"></param>
        ''' <param name="Visitor"></param>
        ''' <param name="Model"></param>
        ''' <param name="IsFieldDeclaration"></param>
        ''' <param name="leadingTrivia"></param>
        ''' <returns></returns>
        ''' <remarks>Fix handling of AddressOf where is mistakenly added to DirectCast and C# Pointers</remarks>
        <Extension>
        Friend Function RemodelVariableDeclaration(variableDeclaration As CSS.VariableDeclarationSyntax, Visitor As NodesVisitor, Model As SemanticModel, IsFieldDeclaration As Boolean, ByRef leadingTrivia As SyntaxTriviaList) As SeparatedSyntaxList(Of VBS.VariableDeclaratorSyntax)
            Dim vbType As VBS.TypeSyntax
            Dim declarationType As VB.VisualBasicSyntaxNode = variableDeclaration.Type.Accept(Visitor)
            Dim typeOrAddressOf As VB.VisualBasicSyntaxNode = declarationType.WithConvertedLeadingTriviaFrom(variableDeclaration.Type)
            Dim typeLeadingTrivia As SyntaxTriviaList = typeOrAddressOf.GetLeadingTrivia

            If typeLeadingTrivia.Any Then
                If typeLeadingTrivia.Last.RawKind = VB.SyntaxKind.WhitespaceTrivia Then
                    typeOrAddressOf = typeOrAddressOf.WithLeadingTrivia(typeLeadingTrivia.Last)
                Else
                    typeOrAddressOf = typeOrAddressOf.WithLeadingTrivia(Factory.Space)
                End If
            End If
            If IsFieldDeclaration AndAlso typeLeadingTrivia.Count > 1 Then
                Dim stmtWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(variableDeclaration)
                If typeLeadingTrivia.ContainsCommentOrDirectiveTrivia AndAlso Not EndsWithSimilarTrivia(stmtWithIssues.GetLeadingTrivia.ConvertTriviaList(), typeLeadingTrivia) Then
                    stmtWithIssues.AddMarker(Factory.EmptyStatement.WithLeadingTrivia(typeLeadingTrivia), StatementHandlingOption.AppendEmptyStatement, AllowDuplicates:=True)
                End If
            End If
            Dim csCollectedCommentTrivia As SyntaxTriviaList
            If typeOrAddressOf.IsKind(VB.SyntaxKind.AddressOfExpression) Then
                vbType = IntPtrType
            Else
                vbType = DirectCast(typeOrAddressOf, VBS.TypeSyntax)
            End If
            vbType = vbType.AdjustNodeTrivia(SeparatorFollows:=True)
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
                        Dim argumentList As VBS.ArgumentListSyntax = CType(variable.ArgumentList?.Accept(Visitor), VBS.ArgumentListSyntax)
                        Dim vbSyntaxNode As VB.VisualBasicSyntaxNode = csInitializer.Accept(Visitor)
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
                        asClause = Factory.AsNewClause(CType(variable.Initializer.Value.Accept(Visitor), VBS.NewExpressionSyntax))
                    ElseIf variable.Initializer.Value.IsKind(CS.SyntaxKind.ImplicitArrayCreationExpression) Then
                    Else
                        Dim resultTuple As (_Error As Boolean, _TypeSyntax As VBS.TypeSyntax) = variable.Initializer.Value.DetermineTypeSyntax(Model)
                        If Not resultTuple._Error Then
                            asClause = Factory.SimpleAsClause(resultTuple._TypeSyntax)
                        Else
                            asClause = Nothing
                        End If
                    End If
                End If
                Dim initializerValue As VBS.ExpressionSyntax = Nothing
                If csInitializer Is Nothing Then
                    initializerValue = CType(variable.Initializer.Value.Accept(Visitor), VBS.ExpressionSyntax)
                    If initializerValue Is Nothing Then
                        initializerValue = Factory.IdentifierName("HandleRefExpression").WithConvertedTriviaFrom(variable.Initializer.Value)
                    End If
                    If initializerValue.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        leadingTrivia = leadingTrivia.AddRange(initializerValue.GetLeadingTrivia)
                    End If
                End If
                Dim initializer As VBS.EqualsValueSyntax = Nothing
                If Not asClause.IsKind(VB.SyntaxKind.AsNewClause) Then
                    initializer = Factory.EqualsValue(initializerValue.WithLeadingTrivia(Factory.Space))
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
                    Factory.VariableDeclarator(Factory.SingletonSeparatedList(DirectCast(variable.Accept(Visitor), VBS.ModifiedIdentifierSyntax)),
                                                asClause,
                                                initializer
                                                ).WithModifiedNodeTrailingTrivia(SeparatorFollows:=False)
                                )
            Next
            If csDeclaratorsWithoutInitializers.Any Then
                Dim modifiedIdentifierList As New List(Of VBS.ModifiedIdentifierSyntax)
                For Each csVarDeclaration As CSS.VariableDeclaratorSyntax In csDeclaratorsWithoutInitializers
                    Dim dTrailingTrivia As SyntaxTriviaList = csVarDeclaration.GetTrailingTrivia
                    If csVarDeclaration.HasTrailingTrivia And dTrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                        csCollectedCommentTrivia = csCollectedCommentTrivia.AddRange(dTrailingTrivia)
                    End If
                    modifiedIdentifierList.Add(DirectCast(csVarDeclaration.Accept(Visitor), VBS.ModifiedIdentifierSyntax).WithTrailingTrivia(Factory.Space))
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
