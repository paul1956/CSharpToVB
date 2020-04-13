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
        End Enum

        Private Function ConvertModifier(m As SyntaxToken, IsModule As Boolean, context As TokenContext, ByRef FoundVisibility As Boolean) As SyntaxToken
            Dim Token As SyntaxToken = ConvertModifierTokenKind(CS.CSharpExtensions.Kind(m), IsModule, context, FoundVisibility)
            If Token.IsKind(VB.SyntaxKind.EmptyToken) Then
                Return EmptyToken.WithConvertedLeadingTriviaFrom(m)
            End If
            Return Token.WithConvertedTriviaFrom(m)
        End Function

        Private Iterator Function ConvertModifiersCore(CS_Modifiers As IEnumerable(Of SyntaxToken), IsModule As Boolean, Context As TokenContext) As IEnumerable(Of SyntaxToken)
            Dim FoundVisibility As Boolean = False
            Dim LeadingTrivia As New List(Of SyntaxTrivia)
            Dim FirstModifier As Boolean = True
            If CS_Modifiers.Any Then
                LeadingTrivia.AddRange(ConvertTrivia(CS_Modifiers(0).LeadingTrivia))
            End If

            If Context <> TokenContext.Local AndAlso Context <> TokenContext.InterfaceOrModule AndAlso Context <> TokenContext.Class Then
                Dim visibility As Boolean = False
                For Each token As SyntaxToken In CS_Modifiers
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
            For i As Integer = 0 To CS_Modifiers.Count - 1
                Dim CS_Modifier As SyntaxToken = CS_Modifiers(i)
                If i = 0 AndAlso Not FirstModifier Then
                    CS_Modifier = CS_Modifier.WithLeadingTrivia(CSSpaceTrivia)
                End If
                Dim VB_Modifier As SyntaxToken = ConvertModifier(CS_Modifier, IsModule, Context, FoundVisibility)
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
                    ElseIf i = CS_Modifiers.Count - 1 Then
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

                If IgnoreInContext(CS_Modifier, Context) Then
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
        Public Function RestructureAttributesAndModifiers(VBStatementNode As VBS.StatementSyntax, HasAttributes As Boolean, HasModifiers As Boolean) As VBS.StatementSyntax
            If Not (HasAttributes OrElse HasModifiers) Then
                Return VBStatementNode
            End If

            Dim AttributeLists As New List(Of VBS.AttributeListSyntax)
            Dim KeywordLeadingTrivia As SyntaxTriviaList
            Dim NewAttributeLeadingTrivia As SyntaxTriviaList
            Dim NewModifiers As New SyntaxTokenList
            Dim StatementLeadingTrivia As New List(Of SyntaxTrivia)
            Dim StatementTrailingTrivia As New List(Of SyntaxTrivia)

            If TypeOf VBStatementNode Is VBS.ClassStatementSyntax Then
                Dim ClassStatement As VBS.ClassStatementSyntax = DirectCast(VBStatementNode, VBS.ClassStatementSyntax)
                If HasAttributes Then
                    RestructureAttributeList(ClassStatement.AttributeLists, AttributeLists, NewAttributeLeadingTrivia, StatementLeadingTrivia, StatementTrailingTrivia)
                    ClassStatement = ClassStatement.WithAttributeLists(VBFactory.List(AttributeLists))
                End If

                If HasModifiers Then
                    For i As Integer = 0 To ClassStatement.Modifiers.Count - 1
                        NewModifiers = NewModifiers.Add(RestructureModifier(ClassStatement.Modifiers(i), i, Not StatementLeadingTrivia.ContainsCommentOrDirectiveTrivia, StatementLeadingTrivia, StatementTrailingTrivia))
                    Next
                    ClassStatement = ClassStatement.WithModifiers(NewModifiers)
                End If

                KeywordLeadingTrivia = RelocateDirectiveDisabledTrivia(ClassStatement.ClassKeyword.LeadingTrivia, StatementTrailingTrivia, RemoveEOL:=False)
                Dim NewClassKeyword As SyntaxToken = ClassStatement.ClassKeyword.WithLeadingTrivia(KeywordLeadingTrivia)
                Dim NewKeywordLeadingTrivia As SyntaxTriviaList = RelocateLeadingCommentTrivia(NewClassKeyword.LeadingTrivia, StatementLeadingTrivia)
                Return ClassStatement.ReplaceToken(ClassStatement.ClassKeyword, NewClassKeyword.WithLeadingTrivia(NewKeywordLeadingTrivia)).
                                                                        WithLeadingTrivia(StatementLeadingTrivia).
                                                                        WithAppendedTrailingTrivia(StatementTrailingTrivia)
            End If

            If TypeOf VBStatementNode Is VBS.EnumStatementSyntax Then
                Dim EnumStatement As VBS.EnumStatementSyntax = DirectCast(VBStatementNode, VBS.EnumStatementSyntax)

                If HasAttributes Then
                    RestructureAttributeList(EnumStatement.AttributeLists, AttributeLists, NewAttributeLeadingTrivia, StatementLeadingTrivia, StatementTrailingTrivia)
                    EnumStatement = EnumStatement.WithAttributeLists(VBFactory.List(AttributeLists))
                End If

                If HasModifiers Then
                    For i As Integer = 0 To EnumStatement.Modifiers.Count - 1
                        NewModifiers = NewModifiers.Add(RestructureModifier(EnumStatement.Modifiers(i), i, Not StatementLeadingTrivia.ContainsCommentOrDirectiveTrivia, StatementLeadingTrivia, StatementTrailingTrivia))
                    Next
                    EnumStatement = EnumStatement.WithModifiers(NewModifiers)
                End If

                KeywordLeadingTrivia = RelocateDirectiveDisabledTrivia(EnumStatement.EnumKeyword.LeadingTrivia, StatementTrailingTrivia, RemoveEOL:=False)
                Dim NewEnumKeyword As SyntaxToken = EnumStatement.EnumKeyword.WithLeadingTrivia(KeywordLeadingTrivia)
                Return EnumStatement.ReplaceToken(EnumStatement.EnumKeyword, NewEnumKeyword).
                                                                        WithLeadingTrivia(StatementLeadingTrivia).
                                                                        WithAppendedTrailingTrivia(StatementTrailingTrivia)
            End If

            If TypeOf VBStatementNode Is VBS.FieldDeclarationSyntax Then
                Dim FieldStatement As VBS.FieldDeclarationSyntax = DirectCast(VBStatementNode, VBS.FieldDeclarationSyntax)

                If HasAttributes Then
                    RestructureAttributeList(FieldStatement.AttributeLists, AttributeLists, NewAttributeLeadingTrivia, StatementLeadingTrivia, StatementTrailingTrivia)
                    FieldStatement = FieldStatement.WithAttributeLists(VBFactory.List(AttributeLists))
                End If

                If HasModifiers Then
                    For i As Integer = 0 To FieldStatement.Modifiers.Count - 1
                        NewModifiers = NewModifiers.Add(RestructureModifier(FieldStatement.Modifiers(i), i, Not StatementLeadingTrivia.ContainsCommentOrDirectiveTrivia, StatementLeadingTrivia, StatementTrailingTrivia))
                    Next
                    FieldStatement = FieldStatement.WithModifiers(NewModifiers)
                End If
                StatementTrailingTrivia.AddRange(FieldStatement.GetTrailingTrivia)
                Return FieldStatement.With(StatementLeadingTrivia, StatementTrailingTrivia)
            End If

            If TypeOf VBStatementNode Is VBS.InterfaceStatementSyntax Then
                Dim InterfaceStatement As VBS.InterfaceStatementSyntax = DirectCast(VBStatementNode, VBS.InterfaceStatementSyntax)

                If HasAttributes Then
                    RestructureAttributeList(InterfaceStatement.AttributeLists, AttributeLists, NewAttributeLeadingTrivia, StatementLeadingTrivia, StatementTrailingTrivia)
                    InterfaceStatement = InterfaceStatement.WithAttributeLists(VBFactory.List(AttributeLists)).WithLeadingTrivia(StatementLeadingTrivia)
                End If

                If HasModifiers Then
                    For i As Integer = 0 To InterfaceStatement.Modifiers.Count - 1
                        NewModifiers = NewModifiers.Add(RestructureModifier(InterfaceStatement.Modifiers(i), i, Not StatementLeadingTrivia.ContainsCommentOrDirectiveTrivia, StatementLeadingTrivia, StatementTrailingTrivia))
                    Next
                    InterfaceStatement = InterfaceStatement.WithModifiers(NewModifiers)
                End If

                KeywordLeadingTrivia = RelocateDirectiveDisabledTrivia(InterfaceStatement.InterfaceKeyword.LeadingTrivia, StatementTrailingTrivia, RemoveEOL:=False)
                Dim InterfaceKeyword As SyntaxToken = InterfaceStatement.InterfaceKeyword.WithLeadingTrivia(KeywordLeadingTrivia)
                Return InterfaceStatement.ReplaceToken(InterfaceStatement.InterfaceKeyword, InterfaceKeyword).
                                                                    WithLeadingTrivia(StatementLeadingTrivia).
                                                                    WithAppendedTrailingTrivia(StatementTrailingTrivia)
            End If

            If TypeOf VBStatementNode Is VBS.MethodStatementSyntax Then
                Dim MethodStatement As VBS.MethodStatementSyntax = DirectCast(VBStatementNode, VBS.MethodStatementSyntax)

                If HasAttributes Then
                    RestructureAttributeList(MethodStatement.AttributeLists, AttributeLists, NewAttributeLeadingTrivia, StatementLeadingTrivia, StatementTrailingTrivia)
                    MethodStatement = MethodStatement.WithAttributeLists(VBFactory.List(AttributeLists))
                End If

                If HasModifiers Then
                    For i As Integer = 0 To MethodStatement.Modifiers.Count - 1
                        NewModifiers = NewModifiers.Add(RestructureModifier(MethodStatement.Modifiers(i), i, Not StatementLeadingTrivia.ContainsCommentOrDirectiveTrivia, StatementLeadingTrivia, StatementTrailingTrivia))
                    Next
                    MethodStatement = MethodStatement.WithModifiers(NewModifiers)
                End If

                KeywordLeadingTrivia = RelocateDirectiveDisabledTrivia(MethodStatement.SubOrFunctionKeyword.LeadingTrivia, StatementTrailingTrivia, RemoveEOL:=False)
                Dim NewSubOrFunctionKeyword As SyntaxToken = MethodStatement.SubOrFunctionKeyword.WithLeadingTrivia(KeywordLeadingTrivia)
                Return MethodStatement.ReplaceToken(MethodStatement.SubOrFunctionKeyword, NewSubOrFunctionKeyword).
                                                                        WithLeadingTrivia(StatementLeadingTrivia).
                                                                        WithAppendedTrailingTrivia(StatementTrailingTrivia)
            End If

            If TypeOf VBStatementNode Is VBS.ModuleStatementSyntax Then
                Dim ModuleStatement As VBS.ModuleStatementSyntax = DirectCast(VBStatementNode, VBS.ModuleStatementSyntax)

                If HasAttributes Then
                    RestructureAttributeList(ModuleStatement.AttributeLists, AttributeLists, NewAttributeLeadingTrivia, StatementLeadingTrivia, StatementTrailingTrivia)
                    ModuleStatement = ModuleStatement.WithAttributeLists(VBFactory.List(AttributeLists))
                End If

                If HasModifiers Then
                    For i As Integer = 0 To ModuleStatement.Modifiers.Count - 1
                        NewModifiers = NewModifiers.Add(RestructureModifier(ModuleStatement.Modifiers(i), i, Not StatementLeadingTrivia.ContainsCommentOrDirectiveTrivia, StatementLeadingTrivia, StatementTrailingTrivia))
                    Next
                    ModuleStatement = ModuleStatement.WithModifiers(NewModifiers)
                End If

                KeywordLeadingTrivia = RelocateDirectiveDisabledTrivia(ModuleStatement.ModuleKeyword.LeadingTrivia, StatementTrailingTrivia, RemoveEOL:=False)
                Dim NewModuleKeyword As SyntaxToken = ModuleStatement.ModuleKeyword.WithLeadingTrivia(KeywordLeadingTrivia)
                Return ModuleStatement.ReplaceToken(ModuleStatement.ModuleKeyword, NewModuleKeyword).
                                                                        WithLeadingTrivia(StatementLeadingTrivia).
                                                                        WithAppendedTrailingTrivia(StatementTrailingTrivia)
            End If

            If TypeOf VBStatementNode Is VBS.PropertyBlockSyntax Then
                Dim PropertyBlock As VBS.PropertyBlockSyntax = DirectCast(VBStatementNode, VBS.PropertyBlockSyntax)
                Dim PropertyStatement As VBS.PropertyStatementSyntax = PropertyBlock.PropertyStatement

                If HasAttributes Then
                    RestructureAttributeList(PropertyStatement.AttributeLists, AttributeLists, NewAttributeLeadingTrivia, StatementLeadingTrivia, StatementTrailingTrivia)
                    PropertyStatement = PropertyStatement.WithAttributeLists(VBFactory.List(AttributeLists))
                End If

                If HasModifiers Then
                    For i As Integer = 0 To PropertyStatement.Modifiers.Count - 1
                        NewModifiers = NewModifiers.Add(RestructureModifier(PropertyStatement.Modifiers(i), i, Not StatementLeadingTrivia.ContainsCommentOrDirectiveTrivia, StatementLeadingTrivia, StatementTrailingTrivia))
                    Next
                    PropertyStatement = PropertyStatement.WithModifiers(NewModifiers)
                End If
                KeywordLeadingTrivia = RelocateDirectiveDisabledTrivia(PropertyStatement.PropertyKeyword.LeadingTrivia, StatementTrailingTrivia, RemoveEOL:=False)
                Dim NewPropertyKeyword As SyntaxToken = PropertyStatement.PropertyKeyword.WithLeadingTrivia(KeywordLeadingTrivia)
                Dim NewPropertyStatement As VBS.PropertyStatementSyntax = PropertyStatement.ReplaceToken(PropertyStatement.PropertyKeyword, NewPropertyKeyword)
                Dim NewPropertyBlock As VBS.PropertyBlockSyntax = PropertyBlock.WithPropertyStatement(NewPropertyStatement.WithLeadingTrivia(StatementLeadingTrivia))
                If Not NewPropertyBlock.GetTrailingTrivia.Last.IsEndOfLine Then
                    StatementTrailingTrivia.Add(VBEOLTrivia)
                End If
                Return NewPropertyBlock.WithAppendedTrailingTrivia(StatementTrailingTrivia)
            End If

            If TypeOf VBStatementNode Is VBS.StructureStatementSyntax Then
                Dim StructureStatement As VBS.StructureStatementSyntax = DirectCast(VBStatementNode, VBS.StructureStatementSyntax)

                If HasAttributes Then
                    RestructureAttributeList(StructureStatement.AttributeLists, AttributeLists, NewAttributeLeadingTrivia, StatementLeadingTrivia, StatementTrailingTrivia)
                    StructureStatement = StructureStatement.WithAttributeLists(VBFactory.List(AttributeLists))
                End If

                If HasModifiers Then
                    For i As Integer = 0 To StructureStatement.Modifiers.Count - 1
                        NewModifiers = NewModifiers.Add(RestructureModifier(StructureStatement.Modifiers(i), i, Not StatementLeadingTrivia.ContainsCommentOrDirectiveTrivia, StatementLeadingTrivia, StatementTrailingTrivia))
                    Next
                    StructureStatement = StructureStatement.WithModifiers(NewModifiers)
                End If

                KeywordLeadingTrivia = RelocateDirectiveDisabledTrivia(StructureStatement.StructureKeyword.LeadingTrivia, StatementTrailingTrivia, RemoveEOL:=True)
                Dim NewStructureKeyword As SyntaxToken = StructureStatement.StructureKeyword.WithLeadingTrivia(KeywordLeadingTrivia)
                Return StructureStatement.ReplaceToken(StructureStatement.StructureKeyword, NewStructureKeyword).
                                                                         WithLeadingTrivia(StatementLeadingTrivia).
                                                                         WithAppendedTrailingTrivia(StatementTrailingTrivia)
            End If
            If TypeOf VBStatementNode Is VBS.SubNewStatementSyntax Then
                Dim SubNewStatement As VBS.SubNewStatementSyntax = DirectCast(VBStatementNode, VBS.SubNewStatementSyntax)

                If HasAttributes Then
                    RestructureAttributeList(SubNewStatement.AttributeLists, AttributeLists, NewAttributeLeadingTrivia, StatementLeadingTrivia, StatementTrailingTrivia)
                    SubNewStatement = SubNewStatement.WithAttributeLists(VBFactory.List(AttributeLists))
                End If

                If HasModifiers Then
                    For i As Integer = 0 To SubNewStatement.Modifiers.Count - 1
                        NewModifiers = NewModifiers.Add(RestructureModifier(SubNewStatement.Modifiers(i), i, Not StatementLeadingTrivia.ContainsCommentOrDirectiveTrivia, StatementLeadingTrivia, StatementTrailingTrivia))
                    Next
                    SubNewStatement = SubNewStatement.WithModifiers(NewModifiers)
                End If

                KeywordLeadingTrivia = RelocateDirectiveDisabledTrivia(SubNewStatement.SubKeyword.LeadingTrivia, StatementTrailingTrivia, RemoveEOL:=False)
                Dim NewSubKeyword As SyntaxToken = SubNewStatement.SubKeyword.WithLeadingTrivia(KeywordLeadingTrivia)
                Return SubNewStatement.ReplaceToken(SubNewStatement.SubKeyword, NewSubKeyword).
                                                                        WithLeadingTrivia(StatementLeadingTrivia).
                                                                        WithAppendedTrailingTrivia(StatementTrailingTrivia)
            End If

            Throw UnreachableException
            Return VBStatementNode
        End Function

        Private Function RestructureAttributeList(CS_AttributeLists1 As SyntaxList(Of VBS.AttributeListSyntax), AttributeLists As List(Of VBS.AttributeListSyntax), ByRef NewAttributeLeadingTrivia As SyntaxTriviaList, ByRef StatementLeadingTrivia As List(Of SyntaxTrivia), ByRef StatementTrailingTrivia As List(Of SyntaxTrivia)) As Boolean
            Dim FoundDirective As Boolean = False
            Dim FoundTheory As Boolean = False
            Dim IsTheoryOrInlineData As Boolean
            For i As Integer = 0 To CS_AttributeLists1.Count - 1
                Dim AttributeList As VBS.AttributeListSyntax = CS_AttributeLists1(i)
                IsTheoryOrInlineData = AttributeList.Attributes.FirstOrDefault.ToString.Contains({"Theory", "InlineData"}, StringComparison.OrdinalIgnoreCase)
                If IsTheoryOrInlineData Then
                    FoundTheory = True
                End If
                Dim NewAttributLeadingTrivia As New SyntaxTriviaList
                If i = 0 Then
                    StatementLeadingTrivia.AddRange(AttributeList.GetLeadingTrivia)
                    If StatementLeadingTrivia.Any AndAlso StatementLeadingTrivia.Last.IsWhitespaceOrEndOfLine Then
                        NewAttributeLeadingTrivia = NewAttributeLeadingTrivia.Add(AttributeList.GetLeadingTrivia.Last)
                    Else
                        NewAttributeLeadingTrivia = NewAttributeLeadingTrivia.Add(SpaceTrivia)
                    End If
                Else
                    RelocateAttributeDirectiveDisabledTrivia(CS_AttributeLists1(i).GetLeadingTrivia, FoundDirective, IsTheoryOrInlineData, StatementLeadingTrivia, StatementTrailingTrivia)
                End If
                Dim NewAttributeTrailingTrivia As SyntaxTriviaList = RelocateDirectiveDisabledTrivia(CS_AttributeLists1(i).GetTrailingTrivia, StatementTrailingTrivia, RemoveEOL:=False)
                AttributeLists.Add(AttributeList.With(NewAttributeLeadingTrivia, NewAttributeTrailingTrivia))
            Next
            Return FoundTheory
        End Function

    End Module
End Namespace
