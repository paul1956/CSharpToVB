' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports CSharpToVBCodeConverter.ToVisualBasic
Imports CSharpToVBCodeConverter.ToVisualBasic.CSharpConverter
Imports Microsoft.CodeAnalysis
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBCodeConverter.ToVisualBasic

    Module VariableDeclarationExtensions

        ''' <summary>
        ''' Converts C# VariableDeclaration to VB List(Of VariableDeclaratorSyntax)
        ''' </summary>
        ''' <param name="variableDeclaration"></param>
        ''' <param name="Visitor"></param>
        ''' <param name="Model"></param>
        ''' <param name="IsFieldDeclaration"></param>
        ''' <param name="LeadingTrivia"></param>
        ''' <returns></returns>
        ''' <remarks>Fix handling of AddressOf where is mistakenly added to DirectCast and C# Pointers</remarks>
        <Extension>
        Friend Function RemodelVariableDeclaration(variableDeclaration As CSS.VariableDeclarationSyntax, Visitor As NodesVisitor, Model As SemanticModel, IsFieldDeclaration As Boolean, ByRef LeadingTrivia As SyntaxTriviaList) As SeparatedSyntaxList(Of VBS.VariableDeclaratorSyntax)
            Dim vbType As VBS.TypeSyntax
            Dim declarationType As VB.VisualBasicSyntaxNode = variableDeclaration.Type.Accept(Visitor)
            Dim typeOrAddressOf As VB.VisualBasicSyntaxNode = declarationType.WithConvertedLeadingTriviaFrom(variableDeclaration.Type)
            Dim typeLeadingTrivia As SyntaxTriviaList = typeOrAddressOf.GetLeadingTrivia

            If typeLeadingTrivia.Any Then
                If typeLeadingTrivia.Last.RawKind = VB.SyntaxKind.WhitespaceTrivia Then
                    typeOrAddressOf = typeOrAddressOf.WithLeadingTrivia(typeLeadingTrivia.Last)
                Else
                    typeOrAddressOf = typeOrAddressOf.WithLeadingTrivia(VBSpaceTrivia)
                End If
            End If
            If IsFieldDeclaration AndAlso typeLeadingTrivia.Count > 1 Then
                Dim statementWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(variableDeclaration)
                If typeLeadingTrivia.ContainsCommentOrDirectiveTrivia AndAlso Not EndsWithSimilarTrivia(statementWithIssues.GetLeadingTrivia.ConvertTriviaList(), typeLeadingTrivia) Then
                    statementWithIssues.AddMarker(Factory.EmptyStatement.WithLeadingTrivia(typeLeadingTrivia), StatementHandlingOption.AppendEmptyStatement, AllowDuplicates:=True)
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

            For Each v As CSS.VariableDeclaratorSyntax In variableDeclaration.Variables
                If v.Initializer Is Nothing Then
                    csDeclaratorsWithoutInitializers.Add(v.WithAppendedTrailingTrivia(csCollectedCommentTrivia))
                    csCollectedCommentTrivia = New SyntaxTriviaList
                    Continue For
                End If
                Dim asClause As VBS.AsClauseSyntax = Nothing
                If variableDeclaration.Type.IsKind(CS.SyntaxKind.RefType) Then
                ElseIf Not variableDeclaration.Type.IsVar Then
                    asClause = Factory.SimpleAsClause(vbType)
                Else
                    ' Get Type from Initializer
                    If v.Initializer.Value.IsKind(CS.SyntaxKind.AnonymousObjectCreationExpression) Then
                        asClause = Factory.AsNewClause(CType(v.Initializer.Value.Accept(Visitor), VBS.NewExpressionSyntax))
                    ElseIf v.Initializer.Value.IsKind(CS.SyntaxKind.ImplicitArrayCreationExpression) Then
                    Else
                        Dim resultTuple As (_Error As Boolean, _TypeSyntax As VBS.TypeSyntax) = v.Initializer.Value.DetermineTypeSyntax(Model)
                        If Not resultTuple._Error Then
                            asClause = Factory.SimpleAsClause(resultTuple._TypeSyntax)
                        Else
                            asClause = Nothing
                        End If
                    End If
                End If
                Dim initializerValue As VBS.ExpressionSyntax = CType(v.Initializer.Value.Accept(Visitor), VBS.ExpressionSyntax)
                If initializerValue Is Nothing Then
                    initializerValue = Factory.IdentifierName("HandleRefExpression").WithConvertedTriviaFrom(v.Initializer.Value)
                End If
                If initializerValue.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    LeadingTrivia = LeadingTrivia.AddRange(initializerValue.GetLeadingTrivia)
                End If
                Dim initializer As VBS.EqualsValueSyntax = Nothing
                If Not asClause.IsKind(VB.SyntaxKind.AsNewClause) Then
                    initializer = Factory.EqualsValue(initializerValue.WithLeadingTrivia(VBSpaceTrivia))
                    If initializer.Value.IsKind(VB.SyntaxKind.ObjectCreationExpression) Then
                        If asClause IsNot Nothing AndAlso CType(asClause, VBS.SimpleAsClauseSyntax).Type.ToString = CType(initializerValue, VBS.ObjectCreationExpressionSyntax).Type.ToString Then
                            asClause = Factory.AsNewClause(CType(initializerValue, VBS.ObjectCreationExpressionSyntax))
                            initializer = Nothing
                        End If
                    End If
                End If
                ' Get the names last to lead with var jsonWriter = new JsonWriter(stringWriter)
                ' Which should be Dim jsonWriter = new JsonWriter(stringWriter)
                vbDeclarators.Add(
                    Factory.VariableDeclarator(Factory.SingletonSeparatedList(DirectCast(v.Accept(Visitor), VBS.ModifiedIdentifierSyntax)),
                                                asClause,
                                                initializer
                                                ).WithModifiedNodeTrailingTrivia(SeparatorFollows:=False)
                                )
            Next
            If csDeclaratorsWithoutInitializers.Any Then
                Dim ModifiedIdentifierList As New List(Of VBS.ModifiedIdentifierSyntax)
                For Each csVarDeclaration As CSS.VariableDeclaratorSyntax In csDeclaratorsWithoutInitializers
                    Dim dTrailingTrivia As SyntaxTriviaList = csVarDeclaration.GetTrailingTrivia
                    If csVarDeclaration.HasTrailingTrivia And dTrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                        csCollectedCommentTrivia = csCollectedCommentTrivia.AddRange(dTrailingTrivia)
                    End If
                    ModifiedIdentifierList.Add(DirectCast(csVarDeclaration.Accept(Visitor), VBS.ModifiedIdentifierSyntax).WithTrailingTrivia(VBSpaceTrivia))
                Next
                Dim varDeclarator As VBS.VariableDeclaratorSyntax = Factory.VariableDeclarator(Factory.SeparatedList(ModifiedIdentifierList), asClause:=Factory.SimpleAsClause(vbType), initializer:=Nothing)
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
