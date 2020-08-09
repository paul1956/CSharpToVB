﻿' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports System.Runtime.InteropServices
Imports CSharpToVBCodeConverter.ToVisualBasic
Imports Microsoft.CodeAnalysis
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Public Module StatementExtensions

    <Extension>
    Friend Function RestructureArguments(VB_Node As VBS.StatementSyntax, csArgumentList As CSS.ArgumentListSyntax) As VBS.StatementSyntax
        If Not csArgumentList.ContainsConditionalDirective Then
            Return VB_Node
        End If

        Dim StatementLeadingTrivia As New List(Of SyntaxTrivia)
        Dim StatementTrailingTrivia As New List(Of SyntaxTrivia)
        If TypeOf VB_Node Is VBS.ExpressionStatementSyntax Then
            Dim ExpressionStatement As VBS.ExpressionStatementSyntax = DirectCast(VB_Node, VBS.ExpressionStatementSyntax)
            Dim Expr As VBS.InvocationExpressionSyntax = DirectCast(ExpressionStatement.Expression, VBS.InvocationExpressionSyntax)
            If Expr.ArgumentList.Arguments.Count = 0 Then
                Return VB_Node
            End If
            For Each e As IndexClass(Of CSS.ArgumentSyntax) In csArgumentList.Arguments.WithIndex
                Dim newArgumentLeadingTrivia As SyntaxTriviaList = RelocateDirectiveDisabledTrivia(VBFactory.TriviaList(ConvertTrivia(e.Value.GetLeadingTrivia)), StatementLeadingTrivia, RemoveEOL:=True)
                Dim newArgumentTrailingTrivia As SyntaxTriviaList = RelocateDirectiveDisabledTrivia(VBFactory.TriviaList(ConvertTrivia(e.Value.GetTrailingTrivia)), StatementTrailingTrivia, RemoveEOL:=False)
                ExpressionStatement = ExpressionStatement.ReplaceNode(
                                            Expr.ArgumentList.Arguments(e.Index),
                                            Expr.ArgumentList.Arguments(e.Index).With(newArgumentLeadingTrivia, newArgumentTrailingTrivia)
                                            )
            Next
            StatementTrailingTrivia.AddRange(ConvertTrivia(csArgumentList.CloseParenToken.LeadingTrivia))
            StatementTrailingTrivia.AddRange(ConvertTrivia(csArgumentList.CloseParenToken.TrailingTrivia))
            If StatementTrailingTrivia.Any AndAlso StatementTrailingTrivia(0).IsDirective Then
                StatementTrailingTrivia.Insert(0, VBEOLTrivia)
            End If
            Return ExpressionStatement.WithPrependedLeadingTrivia(StatementLeadingTrivia).
                                            WithMergedTrailingTrivia(StatementTrailingTrivia)
        Else
            Stop
            Return VB_Node
        End If
    End Function

    <Extension>
    Friend Function RestructureAttributesAndModifiers(Statement As VBS.StatementSyntax, HasAttributes As Boolean, HasModifiers As Boolean) As VBS.StatementSyntax
        If Not (HasAttributes OrElse HasModifiers) Then
            Return Statement
        End If

        Dim attributeLists As New List(Of VBS.AttributeListSyntax)
        Dim keywordLeadingTrivia As SyntaxTriviaList
        Dim newAttributeLeadingTrivia As SyntaxTriviaList
        Dim newModifiers As New SyntaxTokenList
        Dim statementLeadingTrivia As New List(Of SyntaxTrivia)
        Dim statementTrailingTrivia As New List(Of SyntaxTrivia)

        If TypeOf Statement Is VBS.ClassStatementSyntax Then
            Dim classStatement As VBS.ClassStatementSyntax = DirectCast(Statement, VBS.ClassStatementSyntax)
            If HasAttributes Then
                RestructureAttributeList(classStatement.AttributeLists, attributeLists, newAttributeLeadingTrivia, statementLeadingTrivia, statementTrailingTrivia)
                classStatement = classStatement.WithAttributeLists(VBFactory.List(attributeLists))
            End If

            If HasModifiers Then
                For Each e As IndexClass(Of SyntaxToken) In classStatement.Modifiers.WithIndex
                    newModifiers = newModifiers.Add(RestructureModifier(e.Value, e.Index, Not statementLeadingTrivia.ContainsCommentOrDirectiveTrivia, statementLeadingTrivia, statementTrailingTrivia))
                Next
                For index As Integer = 0 To classStatement.Modifiers.Count - 1
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

        If TypeOf Statement Is VBS.EnumStatementSyntax Then
            Dim EnumStatement As VBS.EnumStatementSyntax = DirectCast(Statement, VBS.EnumStatementSyntax)

            If HasAttributes Then
                RestructureAttributeList(EnumStatement.AttributeLists, attributeLists, newAttributeLeadingTrivia, statementLeadingTrivia, statementTrailingTrivia)
                EnumStatement = EnumStatement.WithAttributeLists(VBFactory.List(attributeLists))
            End If

            If HasModifiers Then
                For index As Integer = 0 To EnumStatement.Modifiers.Count - 1
                    newModifiers = newModifiers.Add(RestructureModifier(EnumStatement.Modifiers(index), index, Not statementLeadingTrivia.ContainsCommentOrDirectiveTrivia, statementLeadingTrivia, statementTrailingTrivia))
                Next
                EnumStatement = EnumStatement.WithModifiers(newModifiers)
            End If

            keywordLeadingTrivia = RelocateDirectiveDisabledTrivia(EnumStatement.EnumKeyword.LeadingTrivia, statementTrailingTrivia, RemoveEOL:=False)
            Dim NewEnumKeyword As SyntaxToken = EnumStatement.EnumKeyword.WithLeadingTrivia(keywordLeadingTrivia)
            Return EnumStatement.ReplaceToken(EnumStatement.EnumKeyword, NewEnumKeyword).
                                                                        WithLeadingTrivia(statementLeadingTrivia).
                                                                        WithAppendedTrailingTrivia(statementTrailingTrivia)
        End If

        If TypeOf Statement Is VBS.FieldDeclarationSyntax Then
            Dim FieldStatement As VBS.FieldDeclarationSyntax = DirectCast(Statement, VBS.FieldDeclarationSyntax)

            If HasAttributes Then
                RestructureAttributeList(FieldStatement.AttributeLists, attributeLists, newAttributeLeadingTrivia, statementLeadingTrivia, statementTrailingTrivia)
                FieldStatement = FieldStatement.WithAttributeLists(VBFactory.List(attributeLists))
            End If

            If HasModifiers Then
                For index As Integer = 0 To FieldStatement.Modifiers.Count - 1
                    newModifiers = newModifiers.Add(RestructureModifier(FieldStatement.Modifiers(index), index, Not statementLeadingTrivia.ContainsCommentOrDirectiveTrivia, statementLeadingTrivia, statementTrailingTrivia))
                Next
                FieldStatement = FieldStatement.WithModifiers(newModifiers)
            End If
            statementTrailingTrivia.AddRange(FieldStatement.GetTrailingTrivia)
            Return FieldStatement.With(statementLeadingTrivia, statementTrailingTrivia)
        End If

        If TypeOf Statement Is VBS.InterfaceStatementSyntax Then
            Dim InterfaceStatement As VBS.InterfaceStatementSyntax = DirectCast(Statement, VBS.InterfaceStatementSyntax)

            If HasAttributes Then
                RestructureAttributeList(InterfaceStatement.AttributeLists, attributeLists, newAttributeLeadingTrivia, statementLeadingTrivia, statementTrailingTrivia)
                InterfaceStatement = InterfaceStatement.WithAttributeLists(VBFactory.List(attributeLists)).WithLeadingTrivia(statementLeadingTrivia)
            End If

            If HasModifiers Then
                For index As Integer = 0 To InterfaceStatement.Modifiers.Count - 1
                    newModifiers = newModifiers.Add(RestructureModifier(InterfaceStatement.Modifiers(index), index, Not statementLeadingTrivia.ContainsCommentOrDirectiveTrivia, statementLeadingTrivia, statementTrailingTrivia))
                Next
                InterfaceStatement = InterfaceStatement.WithModifiers(newModifiers)
            End If

            keywordLeadingTrivia = RelocateDirectiveDisabledTrivia(InterfaceStatement.InterfaceKeyword.LeadingTrivia, statementTrailingTrivia, RemoveEOL:=False)
            Dim InterfaceKeyword As SyntaxToken = InterfaceStatement.InterfaceKeyword.WithLeadingTrivia(keywordLeadingTrivia)
            Return InterfaceStatement.ReplaceToken(InterfaceStatement.InterfaceKeyword, InterfaceKeyword).
                                                                    WithLeadingTrivia(statementLeadingTrivia).
                                                                    WithAppendedTrailingTrivia(statementTrailingTrivia)
        End If

        If TypeOf Statement Is VBS.MethodStatementSyntax Then
            Dim MethodStatement As VBS.MethodStatementSyntax = DirectCast(Statement, VBS.MethodStatementSyntax)

            If HasAttributes Then
                RestructureAttributeList(MethodStatement.AttributeLists, attributeLists, newAttributeLeadingTrivia, statementLeadingTrivia, statementTrailingTrivia)
                MethodStatement = MethodStatement.WithAttributeLists(VBFactory.List(attributeLists))
            End If

            If HasModifiers Then
                For index As Integer = 0 To MethodStatement.Modifiers.Count - 1
                    newModifiers = newModifiers.Add(RestructureModifier(MethodStatement.Modifiers(index), index, Not statementLeadingTrivia.ContainsCommentOrDirectiveTrivia, statementLeadingTrivia, statementTrailingTrivia))
                Next
                MethodStatement = MethodStatement.WithModifiers(newModifiers)
            End If

            keywordLeadingTrivia = RelocateDirectiveDisabledTrivia(MethodStatement.SubOrFunctionKeyword.LeadingTrivia, statementTrailingTrivia, RemoveEOL:=False)
            Dim NewSubOrFunctionKeyword As SyntaxToken = MethodStatement.SubOrFunctionKeyword.WithLeadingTrivia(keywordLeadingTrivia)
            Return MethodStatement.ReplaceToken(
                                MethodStatement.SubOrFunctionKeyword,
                                NewSubOrFunctionKeyword
                                ).
                            WithLeadingTrivia(statementLeadingTrivia).
                            WithAppendedTrailingTrivia(statementTrailingTrivia)
        End If

        If TypeOf Statement Is VBS.ModuleStatementSyntax Then
            Dim moduleStatement As VBS.ModuleStatementSyntax = DirectCast(Statement, VBS.ModuleStatementSyntax)

            If HasAttributes Then
                RestructureAttributeList(moduleStatement.AttributeLists, attributeLists, newAttributeLeadingTrivia, statementLeadingTrivia, statementTrailingTrivia)
                moduleStatement = moduleStatement.WithAttributeLists(VBFactory.List(attributeLists))
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

        If TypeOf Statement Is VBS.PropertyBlockSyntax Then
            Dim PropertyBlock As VBS.PropertyBlockSyntax = DirectCast(Statement, VBS.PropertyBlockSyntax)
            Dim PropertyStatement As VBS.PropertyStatementSyntax = PropertyBlock.PropertyStatement

            If HasAttributes Then
                RestructureAttributeList(PropertyStatement.AttributeLists, attributeLists, newAttributeLeadingTrivia, statementLeadingTrivia, statementTrailingTrivia)
                PropertyStatement = PropertyStatement.WithAttributeLists(VBFactory.List(attributeLists))
            End If

            If HasModifiers Then
                For index As Integer = 0 To PropertyStatement.Modifiers.Count - 1
                    newModifiers = newModifiers.Add(RestructureModifier(PropertyStatement.Modifiers(index), index, Not statementLeadingTrivia.ContainsCommentOrDirectiveTrivia, statementLeadingTrivia, statementTrailingTrivia))
                Next
                PropertyStatement = PropertyStatement.WithModifiers(newModifiers)
            End If
            keywordLeadingTrivia = RelocateDirectiveDisabledTrivia(PropertyStatement.PropertyKeyword.LeadingTrivia, statementTrailingTrivia, RemoveEOL:=False)
            Dim NewPropertyKeyword As SyntaxToken = PropertyStatement.PropertyKeyword.WithLeadingTrivia(keywordLeadingTrivia)
            Dim NewPropertyStatement As VBS.PropertyStatementSyntax = PropertyStatement.ReplaceToken(PropertyStatement.PropertyKeyword, NewPropertyKeyword)
            Dim NewPropertyBlock As VBS.PropertyBlockSyntax = PropertyBlock.WithPropertyStatement(NewPropertyStatement.WithLeadingTrivia(statementLeadingTrivia))
            If Not NewPropertyBlock.GetTrailingTrivia.Last.IsEndOfLine Then
                statementTrailingTrivia.Add(VBEOLTrivia)
            End If
            Return NewPropertyBlock.WithAppendedTrailingTrivia(statementTrailingTrivia)
        End If

        If TypeOf Statement Is VBS.StructureStatementSyntax Then
            Dim StructureStatement As VBS.StructureStatementSyntax = DirectCast(Statement, VBS.StructureStatementSyntax)

            If HasAttributes Then
                RestructureAttributeList(StructureStatement.AttributeLists, attributeLists, newAttributeLeadingTrivia, statementLeadingTrivia, statementTrailingTrivia)
                StructureStatement = StructureStatement.WithAttributeLists(VBFactory.List(attributeLists))
            End If

            If HasModifiers Then
                For index As Integer = 0 To StructureStatement.Modifiers.Count - 1
                    newModifiers = newModifiers.Add(RestructureModifier(StructureStatement.Modifiers(index), index, Not statementLeadingTrivia.ContainsCommentOrDirectiveTrivia, statementLeadingTrivia, statementTrailingTrivia))
                Next
                StructureStatement = StructureStatement.WithModifiers(newModifiers)
            End If

            keywordLeadingTrivia = RelocateDirectiveDisabledTrivia(StructureStatement.StructureKeyword.LeadingTrivia, statementTrailingTrivia, RemoveEOL:=True)
            Dim NewStructureKeyword As SyntaxToken = StructureStatement.StructureKeyword.WithLeadingTrivia(keywordLeadingTrivia)
            Return StructureStatement.ReplaceToken(StructureStatement.StructureKeyword, NewStructureKeyword).
                                                                         WithLeadingTrivia(statementLeadingTrivia).
                                                                         WithAppendedTrailingTrivia(statementTrailingTrivia)
        End If
        If TypeOf Statement Is VBS.SubNewStatementSyntax Then
            Dim SubNewStatement As VBS.SubNewStatementSyntax = DirectCast(Statement, VBS.SubNewStatementSyntax)

            If HasAttributes Then
                RestructureAttributeList(SubNewStatement.AttributeLists, attributeLists, newAttributeLeadingTrivia, statementLeadingTrivia, statementTrailingTrivia)
                SubNewStatement = SubNewStatement.WithAttributeLists(VBFactory.List(attributeLists))
            End If

            If HasModifiers Then
                For index As Integer = 0 To SubNewStatement.Modifiers.Count - 1
                    newModifiers = newModifiers.Add(RestructureModifier(SubNewStatement.Modifiers(index), index, Not statementLeadingTrivia.ContainsCommentOrDirectiveTrivia, statementLeadingTrivia, statementTrailingTrivia))
                Next
                SubNewStatement = SubNewStatement.WithModifiers(newModifiers)
            End If

            keywordLeadingTrivia = RelocateDirectiveDisabledTrivia(SubNewStatement.SubKeyword.LeadingTrivia, statementTrailingTrivia, RemoveEOL:=False)
            Dim newSubKeyword As SyntaxToken = SubNewStatement.SubKeyword.WithLeadingTrivia(keywordLeadingTrivia)
            Return SubNewStatement.ReplaceToken(SubNewStatement.SubKeyword, newSubKeyword).
                                                                        WithLeadingTrivia(statementLeadingTrivia).
                                                                        WithAppendedTrailingTrivia(statementTrailingTrivia)
        End If

        Throw UnreachableException
        Return Statement
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
    Public Function RemoveLineContinuation(Statement As VBS.StatementSyntax) As VBS.StatementSyntax
        If Statement Is Nothing Then
            Throw New ArgumentNullException(NameOf(Statement))
        End If
        Return Statement.WithTrailingTrivia(Statement.GetTrailingTrivia.RemoveLineContinuation)
    End Function

End Module