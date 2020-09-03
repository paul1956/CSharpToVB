' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBCodeConverter.ToVisualBasic

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

        Private Iterator Function ConvertModifiersCore(csModifiers As IEnumerable(Of SyntaxToken), IsModule As Boolean, Context As TokenContext) As IEnumerable(Of SyntaxToken)
            Dim FoundVisibility As Boolean = False
            Dim newLeadingTrivia As New SyntaxTriviaList
            Dim FirstModifier As Boolean = True
            If csModifiers.Any Then
                newLeadingTrivia = newLeadingTrivia.AddRange(csModifiers(0).LeadingTrivia.ConvertTriviaList())
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
                        Yield DefaultVisibility.WithLeadingTrivia(newLeadingTrivia)
                        newLeadingTrivia = New SyntaxTriviaList
                        newLeadingTrivia = newLeadingTrivia.Add(VBSpaceTrivia)
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
                Dim VB_Modifier As SyntaxToken = csModifier.ConvertModifier(IsModule, Context, FoundVisibility)
                Dim newTrailingTrivia As New SyntaxTriviaList

                ' If there is only empty Token then attach leading trivia to it otherwise ignore
                If VB_Modifier.IsKind(VB.SyntaxKind.EmptyToken) Then
                    If FirstModifier Then
                        If Not VB_Modifier.HasLeadingTrivia Then
                            Continue For
                        End If

                        If VB_Modifier.LeadingTrivia.Count > 1 Then
                            FirstModifier = False
                            newLeadingTrivia = New SyntaxTriviaList
                            Yield VB_Modifier.WithTrailingTrivia()
                            Continue For
                        End If
                    ElseIf e.IsLast Then
                        If newLeadingTrivia.Any AndAlso Not newLeadingTrivia.Last.IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                            FirstModifier = False
                            Yield VB_Modifier.WithPrependedLeadingTrivia(newLeadingTrivia).WithTrailingTrivia()
                            newLeadingTrivia = New SyntaxTriviaList
                            Continue For
                        End If
                    End If
                    If VB_Modifier.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        FirstModifier = False
                        newLeadingTrivia = New SyntaxTriviaList
                        Yield VB_Modifier
                    End If
                    Continue For
                End If

                If IgnoreInContext(csModifier, Context) Then
                    If FirstModifier Then
                        VB_Modifier = VB_Modifier.WithLeadingTrivia(newLeadingTrivia)
                        FirstModifier = False
                        newLeadingTrivia = New SyntaxTriviaList
                    End If
                    Yield VB_Modifier
                    newLeadingTrivia = New SyntaxTriviaList
                    Continue For
                End If
                If FirstModifier Then
                    VB_Modifier = VB_Modifier.WithLeadingTrivia(newLeadingTrivia)
                    FirstModifier = False
                    newLeadingTrivia = New SyntaxTriviaList
                    Yield VB_Modifier
                    Continue For
                End If
                VB_Modifier = VB_Modifier.WithPrependedLeadingTrivia(newLeadingTrivia)
                If Not (VB_Modifier.IsKind(VB.SyntaxKind.None) OrElse VB_Modifier.IsKind(VB.SyntaxKind.EmptyToken)) Then
                    newLeadingTrivia = New SyntaxTriviaList
                    newLeadingTrivia = newLeadingTrivia.Add(VBSpaceTrivia)
                    ModifyTrailingTrivia(VB_Modifier.LeadingTrivia, newTrailingTrivia)
                    ModifyTrailingTrivia(VB_Modifier.TrailingTrivia, newTrailingTrivia)
                    Yield VB_Modifier.With(newLeadingTrivia, newTrailingTrivia)
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

        Friend Function RestructureAttributeList(vbAttributeLists As SyntaxList(Of VBS.AttributeListSyntax), AttributeLists As List(Of VBS.AttributeListSyntax), ByRef NewAttributeLeadingTrivia As SyntaxTriviaList, ByRef StatementLeadingTrivia As SyntaxTriviaList, ByRef StatementTrailingTrivia As SyntaxTriviaList) As Boolean
            Dim foundDirective As Boolean = False
            Dim foundTheory As Boolean = False
            Dim isTheoryOrInlineData As Boolean
            For Each e As IndexClass(Of VBS.AttributeListSyntax) In vbAttributeLists.WithIndex
                Dim attributeList As VBS.AttributeListSyntax = e.Value.RemoveExtraLeadingEOL
                isTheoryOrInlineData = attributeList.Attributes.FirstOrDefault.ToString.ContainsAny(StringComparison.OrdinalIgnoreCase, "Theory", "InlineData")
                If isTheoryOrInlineData Then
                    foundTheory = True
                End If
                Dim NewAttributLeadingTrivia As New SyntaxTriviaList
                If e.IsFirst Then
                    StatementLeadingTrivia = StatementLeadingTrivia.AddRange(attributeList.GetLeadingTrivia)
                    If StatementLeadingTrivia.Any AndAlso StatementLeadingTrivia.Last.IsWhitespaceOrEndOfLine Then
                        NewAttributeLeadingTrivia = NewAttributeLeadingTrivia.Add(attributeList.GetLeadingTrivia.Last)
                    Else
                        NewAttributeLeadingTrivia = NewAttributeLeadingTrivia.Add(VBSpaceTrivia)
                    End If
                Else
                    RelocateAttributeDirectiveDisabledTrivia(e.Value.GetLeadingTrivia, foundDirective, isTheoryOrInlineData, StatementLeadingTrivia, StatementTrailingTrivia)
                End If
                Dim newAttributeTrailingTrivia As SyntaxTriviaList = RelocateDirectiveDisabledTrivia(e.Value.GetTrailingTrivia, StatementTrailingTrivia, RemoveEOL:=False)
                AttributeLists.Add(attributeList.With(NewAttributeLeadingTrivia, newAttributeTrailingTrivia))
            Next
            Return foundTheory
        End Function

        Public Function ConvertModifiers(CSModifiers As SyntaxTokenList, IsModule As Boolean, Optional Context As TokenContext = TokenContext.Global) As List(Of SyntaxToken)
            Return ConvertModifiersCore(CSModifiers, IsModule, Context).ToList
        End Function

    End Module
End Namespace
