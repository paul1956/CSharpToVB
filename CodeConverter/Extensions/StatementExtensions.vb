' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports System.Runtime.InteropServices
Imports CSharpToVBConverter.ToVisualBasic
Imports Microsoft.CodeAnalysis
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBConverter

    Public Module StatementExtensions

        <Extension>
        Friend Function ContainsName(ImportList As List(Of VBS.ImportsStatementSyntax), ImportName As String) As Boolean
            For Each importToCheck As VBS.ImportsStatementSyntax In ImportList
                For Each clause As VBS.ImportsClauseSyntax In importToCheck.ImportsClauses
                    If clause.ToString = ImportName Then
                        Return True
                    End If
                Next
            Next
            Return False
        End Function

        <Extension>
        Friend Function IsSimpleStatement(statement As CSS.StatementSyntax) As Boolean
            Return TypeOf statement Is CSS.ExpressionStatementSyntax OrElse
                    TypeOf statement Is CSS.BreakStatementSyntax OrElse
                    TypeOf statement Is CSS.ContinueStatementSyntax OrElse
                    TypeOf statement Is CSS.ReturnStatementSyntax OrElse
                    TypeOf statement Is CSS.YieldStatementSyntax OrElse
                    TypeOf statement Is CSS.ThrowStatementSyntax
        End Function

        <Extension>
        Friend Function RestructureArguments(VB_Node As VBS.StatementSyntax, csArgumentList As CSS.ArgumentListSyntax) As VBS.StatementSyntax
            If Not csArgumentList.ContainsConditionalDirective Then
                Return VB_Node
            End If

            If TypeOf VB_Node Is VBS.ExpressionStatementSyntax Then
                Dim exprStmt As VBS.ExpressionStatementSyntax = DirectCast(VB_Node, VBS.ExpressionStatementSyntax)
                Dim invocationExpr As VBS.InvocationExpressionSyntax = DirectCast(exprStmt.Expression, VBS.InvocationExpressionSyntax)
                If invocationExpr.ArgumentList.Arguments.Count = 0 Then
                    Return VB_Node
                End If
                Dim statementLeadingTrivia As New SyntaxTriviaList
                Dim statementTrailingTrivia As New SyntaxTriviaList
                For Each e As IndexClass(Of CSS.ArgumentSyntax) In csArgumentList.Arguments.WithIndex
                    Dim newArgumentLeadingTrivia As SyntaxTriviaList = RelocateDirectiveDisabledTrivia(Factory.TriviaList(e.Value.GetLeadingTrivia.ConvertTriviaList()), statementLeadingTrivia, RemoveEOL:=True)
                    Dim newArgumentTrailingTrivia As SyntaxTriviaList = RelocateDirectiveDisabledTrivia(Factory.TriviaList(e.Value.GetTrailingTrivia.ConvertTriviaList()), statementTrailingTrivia, RemoveEOL:=False)
                    exprStmt = exprStmt.ReplaceNode(
                                            invocationExpr.ArgumentList.Arguments(e.index),
                                            invocationExpr.ArgumentList.Arguments(e.index).With(newArgumentLeadingTrivia, newArgumentTrailingTrivia)
                                            )
                Next
                statementTrailingTrivia.AddRange(CollectConvertedTokenTrivia(csArgumentList.CloseParenToken, GetLeading:=True, GetTrailing:=True))
                If statementTrailingTrivia.Any AndAlso statementTrailingTrivia(0).IsDirective Then
                    statementTrailingTrivia = statementTrailingTrivia.Insert(0, VBEOLTrivia)
                End If
                Return exprStmt.WithPrependedLeadingTrivia(statementLeadingTrivia).
                                            WithMergedTrailingTrivia(statementTrailingTrivia)
            Else
                Stop
                Return VB_Node
            End If
        End Function

        <Extension>
        Friend Function RestructureAttributesAndModifiers(statement As VBS.StatementSyntax, HasAttributes As Boolean, HasModifiers As Boolean) As VBS.StatementSyntax
            If Not (HasAttributes OrElse HasModifiers) Then
                Return statement
            End If

            Dim attributeLists As New List(Of VBS.AttributeListSyntax)
            Dim keywordLeadingTrivia As SyntaxTriviaList
            Dim newAttributeLeadingTrivia As SyntaxTriviaList
            Dim newModifiers As New SyntaxTokenList
            Dim statementLeadingTrivia As SyntaxTriviaList
            Dim statementTrailingTrivia As SyntaxTriviaList

            If TypeOf statement Is VBS.ClassStatementSyntax Then
                Dim classStatement As VBS.ClassStatementSyntax = DirectCast(statement, VBS.ClassStatementSyntax)
                If HasAttributes Then
                    RestructureAttributeList(classStatement.AttributeLists, attributeLists, newAttributeLeadingTrivia, statementLeadingTrivia, statementTrailingTrivia)
                    classStatement = classStatement.WithAttributeLists(Factory.List(attributeLists))
                End If

                If HasModifiers Then
                    For Each e As IndexClass(Of SyntaxToken) In classStatement.Modifiers.WithIndex
                        newModifiers = newModifiers.Add(RestructureModifier(e.Value, e.index, Not statementLeadingTrivia.ContainsCommentOrDirectiveTrivia, statementLeadingTrivia, statementTrailingTrivia))
                    Next
                    classStatement = classStatement.WithModifiers(newModifiers)
                End If

                keywordLeadingTrivia = RelocateDirectiveDisabledTrivia(classStatement.ClassKeyword.LeadingTrivia, statementTrailingTrivia, RemoveEOL:=False)
                Dim newClassKeyword As SyntaxToken = classStatement.ClassKeyword.WithLeadingTrivia(keywordLeadingTrivia)
                Dim newKeywordLeadingTrivia As SyntaxTriviaList = RelocateLeadingCommentTrivia(newClassKeyword.LeadingTrivia, statementLeadingTrivia)
                Return classStatement.ReplaceToken(classStatement.ClassKeyword, newClassKeyword.WithLeadingTrivia(newKeywordLeadingTrivia)).
                                                                        WithLeadingTrivia(statementLeadingTrivia).
                                                                        WithAppendedTrailingTrivia(statementTrailingTrivia)
            End If

            If TypeOf statement Is VBS.EnumStatementSyntax Then
                Dim enumStmt As VBS.EnumStatementSyntax = DirectCast(statement, VBS.EnumStatementSyntax)

                If HasAttributes Then
                    RestructureAttributeList(enumStmt.AttributeLists, attributeLists, newAttributeLeadingTrivia, statementLeadingTrivia, statementTrailingTrivia)
                    enumStmt = enumStmt.WithAttributeLists(Factory.List(attributeLists))
                End If

                If HasModifiers Then
                    For index As Integer = 0 To enumStmt.Modifiers.Count - 1
                        newModifiers = newModifiers.Add(RestructureModifier(enumStmt.Modifiers(index), index, Not statementLeadingTrivia.ContainsCommentOrDirectiveTrivia, statementLeadingTrivia, statementTrailingTrivia))
                    Next
                    enumStmt = enumStmt.WithModifiers(newModifiers)
                End If

                keywordLeadingTrivia = RelocateDirectiveDisabledTrivia(enumStmt.EnumKeyword.LeadingTrivia, statementTrailingTrivia, RemoveEOL:=False)
                Dim newEnumKeyword As SyntaxToken = enumStmt.EnumKeyword.WithLeadingTrivia(keywordLeadingTrivia)
                Return enumStmt.ReplaceToken(enumStmt.EnumKeyword, newEnumKeyword).
                                                                        WithLeadingTrivia(statementLeadingTrivia).
                                                                        WithAppendedTrailingTrivia(statementTrailingTrivia)
            End If

            If TypeOf statement Is VBS.FieldDeclarationSyntax Then
                Dim fieldStmt As VBS.FieldDeclarationSyntax = DirectCast(statement, VBS.FieldDeclarationSyntax)

                If HasAttributes Then
                    RestructureAttributeList(fieldStmt.AttributeLists, attributeLists, newAttributeLeadingTrivia, statementLeadingTrivia, statementTrailingTrivia)
                    fieldStmt = fieldStmt.WithAttributeLists(Factory.List(attributeLists))
                End If

                If HasModifiers Then
                    For index As Integer = 0 To fieldStmt.Modifiers.Count - 1
                        newModifiers = newModifiers.Add(RestructureModifier(fieldStmt.Modifiers(index), index, Not statementLeadingTrivia.ContainsCommentOrDirectiveTrivia, statementLeadingTrivia, statementTrailingTrivia))
                    Next
                    fieldStmt = fieldStmt.WithModifiers(newModifiers)
                End If
                statementTrailingTrivia.AddRange(fieldStmt.GetTrailingTrivia)
                Return fieldStmt.With(statementLeadingTrivia, statementTrailingTrivia)
            End If

            If TypeOf statement Is VBS.InterfaceStatementSyntax Then
                Dim interfaceStmt As VBS.InterfaceStatementSyntax = DirectCast(statement, VBS.InterfaceStatementSyntax)

                If HasAttributes Then
                    RestructureAttributeList(interfaceStmt.AttributeLists, attributeLists, newAttributeLeadingTrivia, statementLeadingTrivia, statementTrailingTrivia)
                    interfaceStmt = interfaceStmt.WithAttributeLists(Factory.List(attributeLists)).WithLeadingTrivia(statementLeadingTrivia)
                End If

                If HasModifiers Then
                    For index As Integer = 0 To interfaceStmt.Modifiers.Count - 1
                        newModifiers = newModifiers.Add(RestructureModifier(interfaceStmt.Modifiers(index), index, Not statementLeadingTrivia.ContainsCommentOrDirectiveTrivia, statementLeadingTrivia, statementTrailingTrivia))
                    Next
                    interfaceStmt = interfaceStmt.WithModifiers(newModifiers)
                End If

                keywordLeadingTrivia = RelocateDirectiveDisabledTrivia(interfaceStmt.InterfaceKeyword.LeadingTrivia, statementTrailingTrivia, RemoveEOL:=False)
                Dim interfaceKeyword As SyntaxToken = interfaceStmt.InterfaceKeyword.WithLeadingTrivia(keywordLeadingTrivia)
                Return interfaceStmt.ReplaceToken(interfaceStmt.InterfaceKeyword, interfaceKeyword).
                                                                    WithLeadingTrivia(statementLeadingTrivia).
                                                                    WithAppendedTrailingTrivia(statementTrailingTrivia)
            End If

            If TypeOf statement Is VBS.MethodStatementSyntax Then
                Dim methodStmt As VBS.MethodStatementSyntax = DirectCast(statement, VBS.MethodStatementSyntax)

                If HasAttributes Then
                    RestructureAttributeList(methodStmt.AttributeLists, attributeLists, newAttributeLeadingTrivia, statementLeadingTrivia, statementTrailingTrivia)
                    methodStmt = methodStmt.WithAttributeLists(Factory.List(attributeLists))
                End If

                If HasModifiers Then
                    For index As Integer = 0 To methodStmt.Modifiers.Count - 1
                        newModifiers = newModifiers.Add(RestructureModifier(methodStmt.Modifiers(index), index, Not statementLeadingTrivia.ContainsCommentOrDirectiveTrivia, statementLeadingTrivia, statementTrailingTrivia))
                    Next
                    methodStmt = methodStmt.WithModifiers(newModifiers)
                End If

                keywordLeadingTrivia = RelocateDirectiveDisabledTrivia(methodStmt.SubOrFunctionKeyword.LeadingTrivia, statementTrailingTrivia, RemoveEOL:=False)
                Dim newSubOrFunctionKeyword As SyntaxToken = methodStmt.SubOrFunctionKeyword.WithLeadingTrivia(keywordLeadingTrivia)
                Return methodStmt.ReplaceToken(
                                methodStmt.SubOrFunctionKeyword,
                                newSubOrFunctionKeyword
                                ).
                            WithLeadingTrivia(statementLeadingTrivia).
                            WithAppendedTrailingTrivia(statementTrailingTrivia)
            End If

            If TypeOf statement Is VBS.ModuleStatementSyntax Then
                Dim moduleStatement As VBS.ModuleStatementSyntax = DirectCast(statement, VBS.ModuleStatementSyntax)

                If HasAttributes Then
                    RestructureAttributeList(moduleStatement.AttributeLists, attributeLists, newAttributeLeadingTrivia, statementLeadingTrivia, statementTrailingTrivia)
                    moduleStatement = moduleStatement.WithAttributeLists(Factory.List(attributeLists))
                End If

                If HasModifiers Then
                    For index As Integer = 0 To moduleStatement.Modifiers.Count - 1
                        newModifiers = newModifiers.Add(RestructureModifier(moduleStatement.Modifiers(index), index, Not statementLeadingTrivia.ContainsCommentOrDirectiveTrivia, statementLeadingTrivia, statementTrailingTrivia))
                    Next
                    moduleStatement = moduleStatement.WithModifiers(newModifiers)
                End If

                keywordLeadingTrivia = RelocateDirectiveDisabledTrivia(moduleStatement.ModuleKeyword.LeadingTrivia, statementTrailingTrivia, RemoveEOL:=False)
                Dim newModuleKeyword As SyntaxToken = moduleStatement.ModuleKeyword.WithLeadingTrivia(keywordLeadingTrivia)
                Return moduleStatement.ReplaceToken(moduleStatement.ModuleKeyword, newModuleKeyword).
                                                                        WithLeadingTrivia(statementLeadingTrivia).
                                                                        WithAppendedTrailingTrivia(statementTrailingTrivia)
            End If

            If TypeOf statement Is VBS.PropertyBlockSyntax Then
                Dim propertyBlock As VBS.PropertyBlockSyntax = DirectCast(statement, VBS.PropertyBlockSyntax)
                Dim propertyStmt As VBS.PropertyStatementSyntax = propertyBlock.PropertyStatement

                If HasAttributes Then
                    RestructureAttributeList(propertyStmt.AttributeLists, attributeLists, newAttributeLeadingTrivia, statementLeadingTrivia, statementTrailingTrivia)
                    propertyStmt = propertyStmt.WithAttributeLists(Factory.List(attributeLists))
                End If

                If HasModifiers Then
                    For index As Integer = 0 To propertyStmt.Modifiers.Count - 1
                        newModifiers = newModifiers.Add(RestructureModifier(propertyStmt.Modifiers(index), index, Not statementLeadingTrivia.ContainsCommentOrDirectiveTrivia, statementLeadingTrivia, statementTrailingTrivia))
                    Next
                    propertyStmt = propertyStmt.WithModifiers(newModifiers)
                End If
                keywordLeadingTrivia = RelocateDirectiveDisabledTrivia(propertyStmt.PropertyKeyword.LeadingTrivia, statementTrailingTrivia, RemoveEOL:=False)
                Dim newPropertyKeyword As SyntaxToken = propertyStmt.PropertyKeyword.WithLeadingTrivia(keywordLeadingTrivia)
                Dim newPropertyBlock As VBS.PropertyBlockSyntax = propertyBlock.WithPropertyStatement(propertyStmt.ReplaceToken(propertyStmt.PropertyKeyword, newPropertyKeyword).WithLeadingTrivia(statementLeadingTrivia))
                If Not newPropertyBlock.GetTrailingTrivia.Last.IsEndOfLine Then
                    statementTrailingTrivia.Add(VBEOLTrivia)
                End If
                Return newPropertyBlock.WithAppendedTrailingTrivia(statementTrailingTrivia)
            End If

            If TypeOf statement Is VBS.StructureStatementSyntax Then
                Dim structureStmt As VBS.StructureStatementSyntax = DirectCast(statement, VBS.StructureStatementSyntax)

                If HasAttributes Then
                    RestructureAttributeList(structureStmt.AttributeLists, attributeLists, newAttributeLeadingTrivia, statementLeadingTrivia, statementTrailingTrivia)
                    structureStmt = structureStmt.WithAttributeLists(Factory.List(attributeLists))
                End If

                If HasModifiers Then
                    For index As Integer = 0 To structureStmt.Modifiers.Count - 1
                        newModifiers = newModifiers.Add(RestructureModifier(structureStmt.Modifiers(index), index, Not statementLeadingTrivia.ContainsCommentOrDirectiveTrivia, statementLeadingTrivia, statementTrailingTrivia))
                    Next
                    structureStmt = structureStmt.WithModifiers(newModifiers)
                End If

                keywordLeadingTrivia = RelocateDirectiveDisabledTrivia(structureStmt.StructureKeyword.LeadingTrivia, statementTrailingTrivia, RemoveEOL:=True)
                Dim newStructureKeyword As SyntaxToken = structureStmt.StructureKeyword.WithLeadingTrivia(keywordLeadingTrivia)
                Return structureStmt.ReplaceToken(structureStmt.StructureKeyword, newStructureKeyword).
                                                                         WithLeadingTrivia(statementLeadingTrivia).
                                                                         WithAppendedTrailingTrivia(statementTrailingTrivia)
            End If
            If TypeOf statement Is VBS.SubNewStatementSyntax Then
                Dim subNewStmt As VBS.SubNewStatementSyntax = DirectCast(statement, VBS.SubNewStatementSyntax)

                If HasAttributes Then
                    RestructureAttributeList(subNewStmt.AttributeLists, attributeLists, newAttributeLeadingTrivia, statementLeadingTrivia, statementTrailingTrivia)
                    subNewStmt = subNewStmt.WithAttributeLists(Factory.List(attributeLists))
                End If

                If HasModifiers Then
                    For index As Integer = 0 To subNewStmt.Modifiers.Count - 1
                        newModifiers = newModifiers.Add(RestructureModifier(subNewStmt.Modifiers(index), index, Not statementLeadingTrivia.ContainsCommentOrDirectiveTrivia, statementLeadingTrivia, statementTrailingTrivia))
                    Next
                    subNewStmt = subNewStmt.WithModifiers(newModifiers)
                End If

                keywordLeadingTrivia = RelocateDirectiveDisabledTrivia(subNewStmt.SubKeyword.LeadingTrivia, statementTrailingTrivia, RemoveEOL:=False)
                Dim newSubKeyword As SyntaxToken = subNewStmt.SubKeyword.WithLeadingTrivia(keywordLeadingTrivia)
                Return subNewStmt.ReplaceToken(subNewStmt.SubKeyword, newSubKeyword).
                                                                        WithLeadingTrivia(statementLeadingTrivia).
                                                                        WithAppendedTrailingTrivia(statementTrailingTrivia)
            End If

            Throw UnreachableException
            Return statement
        End Function

        <Extension>
        Friend Function TryUnpackExpression(statementSyntax As VBS.StatementSyntax, <Out> ByRef expression As VBS.ExpressionSyntax) As Boolean
            If TypeOf statementSyntax Is VBS.ReturnStatementSyntax Then
                expression = DirectCast(statementSyntax, VBS.ReturnStatementSyntax).Expression
            ElseIf TypeOf statementSyntax Is VBS.YieldStatementSyntax Then
                expression = DirectCast(statementSyntax, VBS.YieldStatementSyntax).Expression
            Else
                expression = Nothing
            End If

            Return expression IsNot Nothing
        End Function

        <Extension>
        Friend Function WithoutLastLineContinuation(statement As VBS.StatementSyntax) As VBS.StatementSyntax
            If statement Is Nothing Then
                Throw New ArgumentNullException(NameOf(statement))
            End If
            Return statement.WithTrailingTrivia(statement.GetTrailingTrivia.WithoutLastLineContinuation)
        End Function

    End Module
End Namespace
