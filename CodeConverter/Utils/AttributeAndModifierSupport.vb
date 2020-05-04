' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.Runtime.CompilerServices

Imports CSharpToVBCodeConverter.Util

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBCodeConverter.DestVisualBasic

    Public Module AttributeAndModifierSupport

        Public Enum TokenContext
            [Global]
            InterfaceOrModule
            [Class]
            Member
            VariableOrConst
            Local
            [New]
            [Readonly]
            XMLComment
            Struct
            [Property]
            LocalFunction
        End Enum

        Private Function ConvertModifier(m As SyntaxToken, IsModule As Boolean, context As TokenContext, ByRef FoundVisibility As Boolean) As SyntaxToken
            Dim Token As SyntaxToken = ConvertModifierTokenKind(CS.CSharpExtensions.Kind(m), IsModule, context, FoundVisibility)
            If Token.IsKind(VB.SyntaxKind.EmptyToken) Then
                Return EmptyToken.WithConvertedLeadingTriviaFrom(m)
            End If
            Return Token.WithConvertedTriviaFrom(m)
        End Function

        Private Iterator Function ConvertModifiersCore(csModifiers As IEnumerable(Of SyntaxToken), IsModule As Boolean, Context As TokenContext) As IEnumerable(Of SyntaxToken)
            Dim FoundVisibility As Boolean = False
            Dim LeadingTrivia As New List(Of SyntaxTrivia)
            Dim FirstModifier As Boolean = True
            If csModifiers.Any Then
                LeadingTrivia.AddRange(ConvertTrivia(csModifiers(0).LeadingTrivia))
            End If

            If Context <> TokenContext.Local AndAlso Context <> TokenContext.InterfaceOrModule AndAlso Context <> TokenContext.Class Then
                Dim visibility As Boolean = False
                For Each token As SyntaxToken In csModifiers
                    If IsVisibility(token, Context) Then
                        visibility = True
                        Exit For
                    End If
                Next

                If Not visibility AndAlso Context = TokenContext.Member Then
                    Dim DefaultVisibility As SyntaxToken = CSharpDefaultVisibility(Context)
                    If FirstModifier Then
                        Yield DefaultVisibility.WithLeadingTrivia(LeadingTrivia)
                        LeadingTrivia.Clear()
                        LeadingTrivia.Add(SpaceTrivia)
                        FirstModifier = False
                    Else
                        Yield DefaultVisibility
                    End If
                    FoundVisibility = Not DefaultVisibility.IsKind(VB.SyntaxKind.EmptyToken)
                End If
            End If
            For Each e As IndexClass(Of SyntaxToken) In csModifiers.WithIndex
                Dim csModifier As SyntaxToken = e.Value
                If e.IsFirst AndAlso Not FirstModifier Then
                    csModifier = csModifier.WithLeadingTrivia(CSSpaceTrivia)
                End If
                Dim VB_Modifier As SyntaxToken = ConvertModifier(csModifier, IsModule, Context, FoundVisibility)
                Dim TrailingTrivia As New List(Of SyntaxTrivia)

                ' If there is only empty Token then attach leading trivia to it otherwise ignore
                If VB_Modifier.IsKind(VB.SyntaxKind.EmptyToken) Then
                    If FirstModifier Then
                        If Not VB_Modifier.HasLeadingTrivia Then
                            Continue For
                        End If

                        If VB_Modifier.LeadingTrivia.Count > 1 Then
                            FirstModifier = False
                            LeadingTrivia.Clear()
                            Yield VB_Modifier.WithTrailingTrivia()
                            Continue For
                        End If
                    ElseIf e.IsLast Then
                        If LeadingTrivia.Any AndAlso Not LeadingTrivia.Last.IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                            FirstModifier = False
                            Yield VB_Modifier.WithPrependedLeadingTrivia(LeadingTrivia).WithTrailingTrivia()
                            LeadingTrivia.Clear()
                            Continue For
                        End If
                    End If
                    If VB_Modifier.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        FirstModifier = False
                        LeadingTrivia.Clear()
                        Yield VB_Modifier
                    End If
                    Continue For
                End If

                If IgnoreInContext(csModifier, Context) Then
                    If FirstModifier Then
                        VB_Modifier = VB_Modifier.WithLeadingTrivia(LeadingTrivia)
                        FirstModifier = False
                        LeadingTrivia.Clear()
                    End If
                    Yield VB_Modifier
                    LeadingTrivia.Clear()
                    Continue For
                End If
                If FirstModifier Then
                    VB_Modifier = VB_Modifier.WithLeadingTrivia(LeadingTrivia)
                    FirstModifier = False
                    LeadingTrivia.Clear()
                    Yield VB_Modifier
                    Continue For
                End If
                VB_Modifier = VB_Modifier.WithPrependedLeadingTrivia(LeadingTrivia)
                If Not (VB_Modifier.IsKind(VB.SyntaxKind.None) OrElse VB_Modifier.IsKind(VB.SyntaxKind.EmptyToken)) Then
                    LeadingTrivia.Clear()
                    LeadingTrivia.Add(SpaceTrivia)
                    ModifyTrailingTrivia(VB_Modifier.LeadingTrivia, TrailingTrivia)
                    ModifyTrailingTrivia(VB_Modifier.TrailingTrivia, TrailingTrivia)
                    Yield VB_Modifier.With(LeadingTrivia, TrailingTrivia)
                End If
            Next
        End Function

        Private Function CSharpDefaultVisibility(context As TokenContext) As SyntaxToken
            Select Case context
                Case TokenContext.Global
                    Return FriendKeyword
                Case TokenContext.Local, TokenContext.Member, TokenContext.VariableOrConst
                    Return PrivateKeyword
                Case TokenContext.New, TokenContext.Property
                    Return EmptyToken
            End Select

            Throw New ArgumentOutOfRangeException(NameOf(context))
        End Function

        Private Function IgnoreInContext(m As SyntaxToken, context As TokenContext) As Boolean
            Select Case context
                Case TokenContext.InterfaceOrModule
                    Return m.IsKind(CS.SyntaxKind.PublicKeyword) OrElse m.IsKind(CS.SyntaxKind.StaticKeyword)
                Case TokenContext.Class
                    Return m.IsKind(CS.SyntaxKind.StaticKeyword)
            End Select

            Return False
        End Function

        Private Function IsVisibility(token As SyntaxToken, context As TokenContext) As Boolean
            Return token.IsKind(CS.SyntaxKind.PublicKeyword, CS.SyntaxKind.InternalKeyword, CS.SyntaxKind.ProtectedKeyword, CS.SyntaxKind.PrivateKeyword) OrElse
                    (context = TokenContext.VariableOrConst AndAlso token.IsKind(CS.SyntaxKind.ConstKeyword))
        End Function

        Private Sub ModifyTrailingTrivia(VB_ModifierTrivia As SyntaxTriviaList, ByRef TrailingTrivia As List(Of SyntaxTrivia))
            For Each t As SyntaxTrivia In VB_ModifierTrivia
                Select Case t.RawKind
                    Case VB.SyntaxKind.None
                    Case VB.SyntaxKind.WhitespaceTrivia
                        TrailingTrivia.Add(SpaceTrivia)
                    Case VB.SyntaxKind.EndOfLineTrivia
                        TrailingTrivia.Add(SpaceTrivia)
                    Case VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.ElseDirectiveTrivia, VB.SyntaxKind.ElseIfDirectiveTrivia,
                         VB.SyntaxKind.DisabledTextTrivia, VB.SyntaxKind.EndIfDirectiveTrivia
                        TrailingTrivia.Add(t)
                    Case VB.SyntaxKind.CommentTrivia
                        TrailingTrivia.Add(t)
                    Case VB.SyntaxKind.DocumentationCommentTrivia
                        TrailingTrivia.Add(t)
                    Case Else
                        Stop
                End Select
            Next
        End Sub

        Private Function RestructureModifier(NodeModifier As SyntaxToken, i As Integer, ByRef AttributesNotFound As Boolean, ByRef StatementLeadingTrivia As List(Of SyntaxTrivia), ByRef StatementTrailingTrivia As List(Of SyntaxTrivia)) As SyntaxToken
            If (Not AttributesNotFound) AndAlso NodeModifier.RawKind = VB.SyntaxKind.EmptyToken AndAlso NodeModifier.HasLeadingTrivia AndAlso NodeModifier.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                StatementTrailingTrivia.Add(VBEOLTrivia)
                StatementTrailingTrivia.AddRange(NodeModifier.LeadingTrivia)
                NodeModifier = NodeModifier.WithLeadingTrivia(SpaceTrivia)
            Else
                NodeModifier = NodeModifier.WithLeadingTrivia(RestructureModifierLeadingTrivia(AttributesNotFound, NodeModifier, i, StatementLeadingTrivia, StatementTrailingTrivia))
            End If
            If NodeModifier.TrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                StatementLeadingTrivia.AddRange(RelocateDirectiveDisabledTrivia(NodeModifier.TrailingTrivia, StatementTrailingTrivia, RemoveEOL:=False))
                If StatementLeadingTrivia.Any AndAlso StatementLeadingTrivia.Last.RawKind <> VB.SyntaxKind.EndOfLineTrivia Then
                    StatementLeadingTrivia.Add(VBEOLTrivia)
                End If
                Return NodeModifier.WithTrailingTrivia(SpaceTrivia)
            End If
            Return NodeModifier.WithTrailingTrivia(RelocateDirectiveDisabledTrivia(NodeModifier.TrailingTrivia, StatementTrailingTrivia, RemoveEOL:=True))
        End Function

        Private Function RestructureModifierLeadingTrivia(ByRef LeadingTriviaNotHandled As Boolean, Modifier As SyntaxToken, i As Integer, ByRef StatementLeadingTrivia As List(Of SyntaxTrivia), ByRef StatementTrailingTrivia As List(Of SyntaxTrivia)) As SyntaxTriviaList
            Dim NewModifierLeadingTrivia As New SyntaxTriviaList
            If LeadingTriviaNotHandled Then
                If i = 0 Then
                    StatementLeadingTrivia.AddRange(Modifier.LeadingTrivia)
                Else
                    NewModifierLeadingTrivia = RelocateDirectiveDisabledTrivia(Modifier.LeadingTrivia, StatementLeadingTrivia, RemoveEOL:=True)
                End If
            Else
                If i = 0 Then
                    If Modifier.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        StatementTrailingTrivia.Add(VBEOLTrivia)
                    End If
                    StatementTrailingTrivia.AddRange(Modifier.LeadingTrivia)
                Else
                    NewModifierLeadingTrivia = RelocateDirectiveDisabledTrivia(Modifier.LeadingTrivia, StatementTrailingTrivia, RemoveEOL:=True)
                End If

            End If
            LeadingTriviaNotHandled = StatementLeadingTrivia.Count = 0
            Return NewModifierLeadingTrivia
        End Function

        Public Function ConvertModifiers(CSModifiers As SyntaxTokenList, IsModule As Boolean, Optional Context As TokenContext = TokenContext.[Global]) As List(Of SyntaxToken)
            Return ConvertModifiersCore(CSModifiers, IsModule, Context).ToList
        End Function

        Public Function ConvertModifierTokenKind(t As CS.SyntaxKind, IsModule As Boolean, context As TokenContext, ByRef FoundVisibility As Boolean) As SyntaxToken
            Select Case t
                Case CS.SyntaxKind.None
                    Return EmptyToken
                Case CS.SyntaxKind.PublicKeyword
                    FoundVisibility = True
                    Return PublicKeyword
                Case CS.SyntaxKind.PrivateKeyword
                    If FoundVisibility Then
                        Return EmptyToken
                    End If
                    FoundVisibility = True
                    Return PrivateKeyword
                Case CS.SyntaxKind.InternalKeyword
                    FoundVisibility = True
                    Return FriendKeyword
                Case CS.SyntaxKind.ProtectedKeyword
                    Return ProtectedKeyword
                Case CS.SyntaxKind.ReadOnlyKeyword
                    Return VisualBasicSyntaxFactory.ReadOnlyKeyword
                Case CS.SyntaxKind.OverrideKeyword
                    Return OverridesKeyword
                Case CS.SyntaxKind.VirtualKeyword
                    Return OverridableKeyword
                Case CS.SyntaxKind.InKeyword
                    Return ByValKeyword
                Case CS.SyntaxKind.OutKeyword
                    Return ByRefKeyword
                Case CS.SyntaxKind.PartialKeyword
                    Return PartialKeyword
                Case CS.SyntaxKind.AsyncKeyword
                    Return AsyncKeyword
                Case CS.SyntaxKind.NewKeyword
                    Return ShadowsKeyword
                Case CS.SyntaxKind.ParamsKeyword
                    Return ParamArrayKeyword
                Case CS.SyntaxKind.AwaitKeyword
                    Return AwaitKeyword
                Case CS.SyntaxKind.RefKeyword
                    If context = TokenContext.Struct Then
                        Return EmptyToken
                    End If
                    Return ByRefKeyword

                ' Context Specific
                Case CS.SyntaxKind.AbstractKeyword
                    Return If(context = TokenContext.[Global] OrElse context = TokenContext.Class, MustInheritKeyword, MustOverrideKeyword)
                Case CS.SyntaxKind.ConstKeyword
                    If context = TokenContext.Readonly Then
                        Return VisualBasicSyntaxFactory.ReadOnlyKeyword
                    End If
                    Return ConstKeyword
                Case CS.SyntaxKind.SealedKeyword
                    Return If(context = TokenContext.[Global] OrElse context = TokenContext.Class, NotInheritableKeyword, NotOverridableKeyword)
                Case CS.SyntaxKind.StaticKeyword
                    If IsModule Then
                        If context = TokenContext.VariableOrConst Then
                            If FoundVisibility Then
                                Return EmptyToken
                            End If
                            Return PrivateKeyword
                        End If
                        Return EmptyToken
                    End If
                    If context = TokenContext.InterfaceOrModule Then
                        Return NotInheritableKeyword
                    End If
                    If context = TokenContext.LocalFunction Then
                        Return EmptyToken
                    End If
                    Return SharedKeyword
                Case CS.SyntaxKind.ThisKeyword
                    Return MeKeyword
                Case CS.SyntaxKind.BaseKeyword
                    Return MyBaseKeyword

                    ' unsupported start here
                Case CS.SyntaxKind.ExternKeyword
                    Return EmptyToken
                Case CS.SyntaxKind.FixedKeyword
                    Return EmptyToken
                Case CS.SyntaxKind.UnsafeKeyword
                    Return EmptyToken
                Case CS.SyntaxKind.VolatileKeyword
                    Return EmptyToken
            End Select

            Throw New NotSupportedException($"Modifier.Kind {t} is not supported!")
        End Function

        <Extension>
        Public Function RestructureAttributesAndModifiers(Statement As VBS.StatementSyntax, HasAttributes As Boolean, HasModifiers As Boolean) As VBS.StatementSyntax
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

        Private Function RestructureAttributeList(vbAttributeLists As SyntaxList(Of VBS.AttributeListSyntax), AttributeLists As List(Of VBS.AttributeListSyntax), ByRef NewAttributeLeadingTrivia As SyntaxTriviaList, ByRef StatementLeadingTrivia As List(Of SyntaxTrivia), ByRef StatementTrailingTrivia As List(Of SyntaxTrivia)) As Boolean
            Dim foundDirective As Boolean = False
            Dim foundTheory As Boolean = False
            Dim isTheoryOrInlineData As Boolean
            For Each e As IndexClass(Of VBS.AttributeListSyntax) In vbAttributeLists.WithIndex
                Dim attributeList As VBS.AttributeListSyntax = e.Value
                isTheoryOrInlineData = attributeList.Attributes.FirstOrDefault.ToString.Contains({"Theory", "InlineData"}, StringComparison.OrdinalIgnoreCase)
                If isTheoryOrInlineData Then
                    foundTheory = True
                End If
                Dim NewAttributLeadingTrivia As New SyntaxTriviaList
                If e.IsFirst Then
                    StatementLeadingTrivia.AddRange(attributeList.GetLeadingTrivia)
                    If StatementLeadingTrivia.Any AndAlso StatementLeadingTrivia.Last.IsWhitespaceOrEndOfLine Then
                        NewAttributeLeadingTrivia = NewAttributeLeadingTrivia.Add(attributeList.GetLeadingTrivia.Last)
                    Else
                        NewAttributeLeadingTrivia = NewAttributeLeadingTrivia.Add(SpaceTrivia)
                    End If
                Else
                    RelocateAttributeDirectiveDisabledTrivia(e.Value.GetLeadingTrivia, foundDirective, isTheoryOrInlineData, StatementLeadingTrivia, StatementTrailingTrivia)
                End If
                Dim newAttributeTrailingTrivia As SyntaxTriviaList = RelocateDirectiveDisabledTrivia(e.Value.GetTrailingTrivia, StatementTrailingTrivia, RemoveEOL:=False)
                AttributeLists.Add(attributeList.With(NewAttributeLeadingTrivia, newAttributeTrailingTrivia))
            Next
            Return foundTheory
        End Function

    End Module
End Namespace
