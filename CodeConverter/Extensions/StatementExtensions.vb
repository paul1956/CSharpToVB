﻿' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports System.Runtime.InteropServices
Imports Microsoft.CodeAnalysis
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Public Module StatementExtensions

    <Extension>
    Private Function ContainsAny(s As String, comparisonType As StringComparison, ParamArray stringArray() As String) As Boolean
        If String.IsNullOrWhiteSpace(s) Then
            Return False
        End If
        If stringArray Is Nothing OrElse stringArray.Length = 0 Then
            Return False
        End If
        For Each str As String In stringArray
            If s.Contains(str, comparisonType) Then
                Return True
            End If
        Next
        Return False
    End Function

    <Extension>
    Private Function ContainsConditionalDirective(argumentList As CSS.ArgumentListSyntax) As Boolean
        If argumentList.Arguments.Count = 0 Then
            Return False
        End If
        For Each e As IndexClass(Of CSS.ArgumentSyntax) In argumentList.Arguments.WithIndex
            For Each t As SyntaxTrivia In e.Value.GetLeadingTrivia
                Select Case t.RawKind
                    Case CS.SyntaxKind.DisabledTextTrivia,
                         CS.SyntaxKind.ElifDirectiveTrivia,
                         CS.SyntaxKind.ElseDirectiveTrivia,
                         CS.SyntaxKind.EndIfDirectiveTrivia,
                         CS.SyntaxKind.IfDirectiveTrivia
                        Return True
                    Case CS.SyntaxKind.NullableDirectiveTrivia
                        Return False
                    Case CS.SyntaxKind.EndOfLineTrivia,
                         CS.SyntaxKind.SingleLineCommentTrivia,
                         CS.SyntaxKind.WhitespaceTrivia
                        ' ignore
                    Case Else
                        Debug.WriteLine($"Unknown TriviaKind {CS.CSharpExtensions.Kind(t)} in ContainsConditionalDirective")
                        Stop
                End Select
            Next
        Next
        Return False
    End Function

    Private Sub RestructureAttributeList(vbAttributeLists As SyntaxList(Of VBS.AttributeListSyntax), attributeLists As List(Of VBS.AttributeListSyntax), ByRef newAttributeLeadingTrivia As SyntaxTriviaList, ByRef statementLeadingTrivia As SyntaxTriviaList, ByRef statementTrailingTrivia As SyntaxTriviaList)
        Dim foundDirective As Boolean = False
        Dim isTheoryOrInlineData As Boolean
        For Each e As IndexClass(Of VBS.AttributeListSyntax) In vbAttributeLists.WithIndex
            Dim attributeList As VBS.AttributeListSyntax = e.Value.RemoveExtraLeadingEol
            isTheoryOrInlineData = attributeList.Attributes.FirstOrDefault.ToString.ContainsAny(StringComparison.OrdinalIgnoreCase, "Theory", "InlineData")
            If e.IsFirst Then
                statementLeadingTrivia = statementLeadingTrivia.AddRange(attributeList.GetLeadingTrivia)
                If statementLeadingTrivia.Any AndAlso statementLeadingTrivia.Last.IsWhitespaceOrEndOfLine Then
                    newAttributeLeadingTrivia = newAttributeLeadingTrivia.Add(attributeList.GetLeadingTrivia.Last)
                Else
                    newAttributeLeadingTrivia = newAttributeLeadingTrivia.Add(SpaceTrivia)
                End If
            Else
                RelocateAttributeDirectiveDisabledTrivia(e.Value.GetLeadingTrivia, foundDirective, isTheoryOrInlineData, statementLeadingTrivia, statementTrailingTrivia)
            End If
            Dim newAttributeTrailingTrivia As SyntaxTriviaList = RelocateDirectiveDisabledTrivia(e.Value.GetTrailingTrivia, statementTrailingTrivia, removeEol:=False)
            attributeLists.Add(attributeList.With(newAttributeLeadingTrivia, newAttributeTrailingTrivia))
        Next
    End Sub

    <Extension>
    Friend Function ContainsName(importList As List(Of VBS.ImportsStatementSyntax), importName As String) As Boolean
        For Each importToCheck As VBS.ImportsStatementSyntax In importList
            For Each clause As VBS.ImportsClauseSyntax In importToCheck.ImportsClauses
                If clause.ToString = importName Then
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
    Friend Function RestructureArguments(vbNode As VBS.StatementSyntax, csArgumentList As CSS.ArgumentListSyntax) As VBS.StatementSyntax
        If Not csArgumentList.ContainsConditionalDirective Then
            Return vbNode
        End If

        If TypeOf vbNode Is VBS.ExpressionStatementSyntax Then
            Dim exprStmt As VBS.ExpressionStatementSyntax = DirectCast(vbNode, VBS.ExpressionStatementSyntax)
            Dim invocationExpr As VBS.InvocationExpressionSyntax = DirectCast(exprStmt.Expression, VBS.InvocationExpressionSyntax)
            If invocationExpr.ArgumentList.Arguments.Count = 0 Then
                Return vbNode
            End If
            Dim statementLeadingTrivia As New SyntaxTriviaList
            Dim statementTrailingTrivia As New SyntaxTriviaList
            For Each e As IndexClass(Of CSS.ArgumentSyntax) In csArgumentList.Arguments.WithIndex
                Dim newArgumentLeadingTrivia As SyntaxTriviaList = RelocateDirectiveDisabledTrivia(Factory.TriviaList(e.Value.GetLeadingTrivia.ConvertTriviaList()), statementLeadingTrivia, removeEol:=True)
                Dim newArgumentTrailingTrivia As SyntaxTriviaList = RelocateDirectiveDisabledTrivia(Factory.TriviaList(e.Value.GetTrailingTrivia.ConvertTriviaList()), statementTrailingTrivia, removeEol:=False)
                exprStmt = exprStmt.ReplaceNode(
                                        invocationExpr.ArgumentList.Arguments(e.Index),
                                        invocationExpr.ArgumentList.Arguments(e.Index).With(newArgumentLeadingTrivia, newArgumentTrailingTrivia)
                                        )
            Next
            statementTrailingTrivia.AddRange(csArgumentList.CloseParenToken.CollectConvertedTokenTrivia(getLeading:=True, getTrailing:=True))
            If statementTrailingTrivia.Any AndAlso statementTrailingTrivia(0).IsDirective Then
                statementTrailingTrivia = statementTrailingTrivia.Insert(0, VbEolTrivia)
            End If
            Return exprStmt.WithPrependedLeadingTrivia(statementLeadingTrivia).
                                        WithMergedTrailingTrivia(statementTrailingTrivia)
        Else
            Return vbNode
        End If
    End Function

    <Extension>
    Friend Function RestructureAttributesAndModifiers(statement As VBS.StatementSyntax, hasAttributes As Boolean, hasModifiers As Boolean) As VBS.StatementSyntax
        If Not (hasAttributes OrElse hasModifiers) Then
            Return statement
        End If

        Dim attributeLists As New List(Of VBS.AttributeListSyntax)
        Dim keywordLeadingTrivia As SyntaxTriviaList
        Dim newAttributeLeadingTrivia As SyntaxTriviaList
        Dim newModifiers As New SyntaxTokenList
        Dim statementLeadingTrivia As SyntaxTriviaList
        Dim statementTrailingTrivia As SyntaxTriviaList

        Dim classStatement As VBS.ClassStatementSyntax = TryCast(statement, VBS.ClassStatementSyntax)
        If classStatement IsNot Nothing Then
            If hasAttributes Then
                RestructureAttributeList(classStatement.AttributeLists, attributeLists, newAttributeLeadingTrivia, statementLeadingTrivia, statementTrailingTrivia)
                classStatement = classStatement.WithAttributeLists(Factory.List(attributeLists))
            End If

            If hasModifiers Then
                For Each e As IndexClass(Of SyntaxToken) In classStatement.Modifiers.WithIndex
                    newModifiers = newModifiers.Add(e.Value.RestructureModifier(e.Index, Not statementLeadingTrivia.ContainsCommentOrDirectiveTrivia, statementLeadingTrivia, statementTrailingTrivia))
                Next
                classStatement = classStatement.WithModifiers(newModifiers)
            End If

            keywordLeadingTrivia = RelocateDirectiveDisabledTrivia(classStatement.ClassKeyword.LeadingTrivia, statementTrailingTrivia, removeEol:=False)
            Dim newClassKeyword As SyntaxToken = classStatement.ClassKeyword.WithLeadingTrivia(keywordLeadingTrivia)
            Dim newKeywordLeadingTrivia As SyntaxTriviaList = RelocateLeadingCommentTrivia(newClassKeyword.LeadingTrivia, statementLeadingTrivia)
            Return classStatement.ReplaceToken(classStatement.ClassKeyword, newClassKeyword.WithLeadingTrivia(newKeywordLeadingTrivia)).
                WithLeadingTrivia(statementLeadingTrivia).
                WithAppendedTrailingTrivia(statementTrailingTrivia)
        End If

        Dim enumStmt As VBS.EnumStatementSyntax = TryCast(statement, VBS.EnumStatementSyntax)
        If enumStmt IsNot Nothing Then

            If hasAttributes Then
                RestructureAttributeList(enumStmt.AttributeLists, attributeLists, newAttributeLeadingTrivia, statementLeadingTrivia, statementTrailingTrivia)
                enumStmt = enumStmt.WithAttributeLists(Factory.List(attributeLists))
            End If

            If hasModifiers Then
                For index As Integer = 0 To enumStmt.Modifiers.Count - 1
                    newModifiers = newModifiers.Add(enumStmt.Modifiers(index).RestructureModifier(index, Not statementLeadingTrivia.ContainsCommentOrDirectiveTrivia, statementLeadingTrivia, statementTrailingTrivia))
                Next
                enumStmt = enumStmt.WithModifiers(newModifiers)
            End If

            keywordLeadingTrivia = RelocateDirectiveDisabledTrivia(enumStmt.EnumKeyword.LeadingTrivia, statementTrailingTrivia, removeEol:=False)
            Dim newEnumKeyword As SyntaxToken = enumStmt.EnumKeyword.WithLeadingTrivia(keywordLeadingTrivia)
            Return enumStmt.ReplaceToken(enumStmt.EnumKeyword, newEnumKeyword).
                WithLeadingTrivia(statementLeadingTrivia).
                WithAppendedTrailingTrivia(statementTrailingTrivia)
        End If

        Dim fieldStmt As VBS.FieldDeclarationSyntax = TryCast(statement, VBS.FieldDeclarationSyntax)
        If fieldStmt IsNot Nothing Then

            If hasAttributes Then
                RestructureAttributeList(fieldStmt.AttributeLists, attributeLists, newAttributeLeadingTrivia, statementLeadingTrivia, statementTrailingTrivia)
                fieldStmt = fieldStmt.WithAttributeLists(Factory.List(attributeLists))
            End If

            If hasModifiers Then
                For index As Integer = 0 To fieldStmt.Modifiers.Count - 1
                    newModifiers = newModifiers.Add(fieldStmt.Modifiers(index).RestructureModifier(index, Not statementLeadingTrivia.ContainsCommentOrDirectiveTrivia, statementLeadingTrivia, statementTrailingTrivia))
                Next
                fieldStmt = fieldStmt.WithModifiers(newModifiers)
            End If
            statementTrailingTrivia.AddRange(fieldStmt.GetTrailingTrivia)
            Return fieldStmt.With(statementLeadingTrivia, statementTrailingTrivia)
        End If

        Dim interfaceStmt As VBS.InterfaceStatementSyntax = TryCast(statement, VBS.InterfaceStatementSyntax)
        If interfaceStmt IsNot Nothing Then

            If hasAttributes Then
                RestructureAttributeList(interfaceStmt.AttributeLists, attributeLists, newAttributeLeadingTrivia, statementLeadingTrivia, statementTrailingTrivia)
                interfaceStmt = interfaceStmt.WithAttributeLists(Factory.List(attributeLists)).WithLeadingTrivia(statementLeadingTrivia)
            End If

            If hasModifiers Then
                For index As Integer = 0 To interfaceStmt.Modifiers.Count - 1
                    newModifiers = newModifiers.Add(interfaceStmt.Modifiers(index).RestructureModifier(index, Not statementLeadingTrivia.ContainsCommentOrDirectiveTrivia, statementLeadingTrivia, statementTrailingTrivia))
                Next
                interfaceStmt = interfaceStmt.WithModifiers(newModifiers)
            End If

            keywordLeadingTrivia = RelocateDirectiveDisabledTrivia(interfaceStmt.InterfaceKeyword.LeadingTrivia, statementTrailingTrivia, removeEol:=False)
            Dim interfaceKeyword As SyntaxToken = interfaceStmt.InterfaceKeyword.WithLeadingTrivia(keywordLeadingTrivia)
            Return interfaceStmt.ReplaceToken(interfaceStmt.InterfaceKeyword, interfaceKeyword).
                WithLeadingTrivia(statementLeadingTrivia).
                WithAppendedTrailingTrivia(statementTrailingTrivia)
        End If

        Dim methodStmt As VBS.MethodStatementSyntax = TryCast(statement, VBS.MethodStatementSyntax)
        If methodStmt IsNot Nothing Then

            If hasAttributes Then
                RestructureAttributeList(methodStmt.AttributeLists, attributeLists, newAttributeLeadingTrivia, statementLeadingTrivia, statementTrailingTrivia)
                methodStmt = methodStmt.WithAttributeLists(Factory.List(attributeLists))
            End If

            If hasModifiers Then
                For index As Integer = 0 To methodStmt.Modifiers.Count - 1
                    newModifiers = newModifiers.Add(methodStmt.Modifiers(index).RestructureModifier(index, Not statementLeadingTrivia.ContainsCommentOrDirectiveTrivia, statementLeadingTrivia, statementTrailingTrivia))
                Next
                methodStmt = methodStmt.WithModifiers(newModifiers)
            End If

            keywordLeadingTrivia = RelocateDirectiveDisabledTrivia(methodStmt.SubOrFunctionKeyword.LeadingTrivia, statementTrailingTrivia, removeEol:=False)
            Dim newSubOrFunctionKeyword As SyntaxToken = methodStmt.SubOrFunctionKeyword.WithLeadingTrivia(keywordLeadingTrivia)
            Return methodStmt.ReplaceToken(
                methodStmt.SubOrFunctionKeyword,
                newSubOrFunctionKeyword
                ).
                WithLeadingTrivia(statementLeadingTrivia).
                WithAppendedTrailingTrivia(statementTrailingTrivia)
        End If

        Dim moduleStatement As VBS.ModuleStatementSyntax = TryCast(statement, VBS.ModuleStatementSyntax)
        If moduleStatement IsNot Nothing Then

            If hasAttributes Then
                RestructureAttributeList(moduleStatement.AttributeLists, attributeLists, newAttributeLeadingTrivia, statementLeadingTrivia, statementTrailingTrivia)
                moduleStatement = moduleStatement.WithAttributeLists(Factory.List(attributeLists))
            End If

            If hasModifiers Then
                For index As Integer = 0 To moduleStatement.Modifiers.Count - 1
                    newModifiers = newModifiers.Add(moduleStatement.Modifiers(index).RestructureModifier(index, Not statementLeadingTrivia.ContainsCommentOrDirectiveTrivia, statementLeadingTrivia, statementTrailingTrivia))
                Next
                moduleStatement = moduleStatement.WithModifiers(newModifiers)
            End If

            keywordLeadingTrivia = RelocateDirectiveDisabledTrivia(moduleStatement.ModuleKeyword.LeadingTrivia, statementTrailingTrivia, removeEol:=False)
            Dim newModuleKeyword As SyntaxToken = moduleStatement.ModuleKeyword.WithLeadingTrivia(keywordLeadingTrivia)
            Return moduleStatement.ReplaceToken(moduleStatement.ModuleKeyword, newModuleKeyword).
                WithLeadingTrivia(statementLeadingTrivia).
                WithAppendedTrailingTrivia(statementTrailingTrivia)
        End If

        Dim propertyBlock As VBS.PropertyBlockSyntax = TryCast(statement, VBS.PropertyBlockSyntax)
        If propertyBlock IsNot Nothing Then
            Dim propertyStmt As VBS.PropertyStatementSyntax = propertyBlock.PropertyStatement

            If hasAttributes Then
                RestructureAttributeList(propertyStmt.AttributeLists, attributeLists, newAttributeLeadingTrivia, statementLeadingTrivia, statementTrailingTrivia)
                propertyStmt = propertyStmt.WithAttributeLists(Factory.List(attributeLists))
            End If

            If hasModifiers Then
                For index As Integer = 0 To propertyStmt.Modifiers.Count - 1
                    newModifiers = newModifiers.Add(propertyStmt.Modifiers(index).RestructureModifier(index, Not statementLeadingTrivia.ContainsCommentOrDirectiveTrivia, statementLeadingTrivia, statementTrailingTrivia))
                Next
                propertyStmt = propertyStmt.WithModifiers(newModifiers)
            End If
            keywordLeadingTrivia = RelocateDirectiveDisabledTrivia(propertyStmt.PropertyKeyword.LeadingTrivia, statementTrailingTrivia, removeEol:=False)
            Dim newPropertyKeyword As SyntaxToken = propertyStmt.PropertyKeyword.WithLeadingTrivia(keywordLeadingTrivia)
            Dim newPropertyBlock As VBS.PropertyBlockSyntax = propertyBlock.WithPropertyStatement(propertyStmt.ReplaceToken(propertyStmt.PropertyKeyword, newPropertyKeyword).WithLeadingTrivia(statementLeadingTrivia))
            If Not newPropertyBlock.GetTrailingTrivia.Last.IsEndOfLine Then
                statementTrailingTrivia.Add(VbEolTrivia)
            End If
            Return newPropertyBlock.WithAppendedTrailingTrivia(statementTrailingTrivia)
        End If

        Dim structureStmt As VBS.StructureStatementSyntax = TryCast(statement, VBS.StructureStatementSyntax)
        If structureStmt IsNot Nothing Then
            If hasAttributes Then
                RestructureAttributeList(structureStmt.AttributeLists, attributeLists, newAttributeLeadingTrivia, statementLeadingTrivia, statementTrailingTrivia)
                structureStmt = structureStmt.WithAttributeLists(Factory.List(attributeLists))
            End If

            If hasModifiers Then
                For index As Integer = 0 To structureStmt.Modifiers.Count - 1
                    newModifiers = newModifiers.Add(structureStmt.Modifiers(index).RestructureModifier(index, Not statementLeadingTrivia.ContainsCommentOrDirectiveTrivia, statementLeadingTrivia, statementTrailingTrivia))
                Next
                structureStmt = structureStmt.WithModifiers(newModifiers)
            End If

            keywordLeadingTrivia = RelocateDirectiveDisabledTrivia(structureStmt.StructureKeyword.LeadingTrivia, statementTrailingTrivia, removeEol:=True)
            Dim newStructureKeyword As SyntaxToken = structureStmt.StructureKeyword.WithLeadingTrivia(keywordLeadingTrivia)
            Return structureStmt.ReplaceToken(structureStmt.StructureKeyword, newStructureKeyword).
                WithLeadingTrivia(statementLeadingTrivia).
                WithAppendedTrailingTrivia(statementTrailingTrivia)
        End If
        Dim subNewStmt As VBS.SubNewStatementSyntax = TryCast(statement, VBS.SubNewStatementSyntax)
        If subNewStmt IsNot Nothing Then
            If hasAttributes Then
                RestructureAttributeList(subNewStmt.AttributeLists, attributeLists, newAttributeLeadingTrivia, statementLeadingTrivia, statementTrailingTrivia)
                subNewStmt = subNewStmt.WithAttributeLists(Factory.List(attributeLists))
            End If

            If hasModifiers Then
                For index As Integer = 0 To subNewStmt.Modifiers.Count - 1
                    newModifiers = newModifiers.Add(subNewStmt.Modifiers(index).RestructureModifier(index, Not statementLeadingTrivia.ContainsCommentOrDirectiveTrivia, statementLeadingTrivia, statementTrailingTrivia))
                Next
                subNewStmt = subNewStmt.WithModifiers(newModifiers)
            End If

            keywordLeadingTrivia = RelocateDirectiveDisabledTrivia(subNewStmt.SubKeyword.LeadingTrivia, statementTrailingTrivia, removeEol:=False)
            Dim newSubKeyword As SyntaxToken = subNewStmt.SubKeyword.WithLeadingTrivia(keywordLeadingTrivia)
            Return subNewStmt.ReplaceToken(subNewStmt.SubKeyword, newSubKeyword).
                WithLeadingTrivia(statementLeadingTrivia).
                WithAppendedTrailingTrivia(statementTrailingTrivia)
        End If

        Throw UnreachableException
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
