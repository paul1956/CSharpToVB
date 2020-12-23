' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBConverter.ToVisualBasic

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

        Friend Iterator Function ConvertModifiers(csModifiers As IEnumerable(Of SyntaxToken), IsModule As Boolean, Context As TokenContext) As IEnumerable(Of SyntaxToken)
            Dim foundVisibility As Boolean = False
            Dim newLeadingTrivia As New SyntaxTriviaList
            Dim firstModifier As Boolean = True
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
                    Dim defaultVisibility As SyntaxToken = CSharpDefaultVisibility(Context)
                    If firstModifier Then
                        Yield defaultVisibility.WithLeadingTrivia(newLeadingTrivia)
                        newLeadingTrivia = New SyntaxTriviaList
                        newLeadingTrivia = newLeadingTrivia.Add(Factory.Space)
                        firstModifier = False
                    Else
                        Yield defaultVisibility
                    End If
                    foundVisibility = Not defaultVisibility.IsKind(VB.SyntaxKind.EmptyToken)
                End If
            End If
            For Each e As IndexClass(Of SyntaxToken) In csModifiers.WithIndex
                Dim csModifier As SyntaxToken = e.Value
                If e.IsFirst AndAlso Not firstModifier Then
                    csModifier = csModifier.WithLeadingTrivia(CS.SyntaxFactory.Space)
                End If
                Dim vbModifier As SyntaxToken = csModifier.ConvertModifier(IsModule, Context, foundVisibility)
                Dim newTrailingTrivia As New SyntaxTriviaList

                ' If there is only empty Token then attach leading trivia to it otherwise ignore
                If vbModifier.IsKind(VB.SyntaxKind.EmptyToken) Then
                    If firstModifier Then
                        If Not vbModifier.HasLeadingTrivia Then
                            Continue For
                        End If

                        If vbModifier.LeadingTrivia.Count > 1 Then
                            firstModifier = False
                            newLeadingTrivia = New SyntaxTriviaList
                            Yield vbModifier.WithTrailingTrivia()
                            Continue For
                        End If
                    ElseIf e.IsLast Then
                        If newLeadingTrivia.Any AndAlso Not newLeadingTrivia.Last.IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                            firstModifier = False
                            Yield vbModifier.WithPrependedLeadingTrivia(newLeadingTrivia).WithTrailingTrivia()
                            newLeadingTrivia = New SyntaxTriviaList
                            Continue For
                        End If
                    End If
                    If vbModifier.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        firstModifier = False
                        newLeadingTrivia = New SyntaxTriviaList
                        Yield vbModifier
                    End If
                    Continue For
                End If

                If IgnoreInContext(csModifier, Context) Then
                    If firstModifier Then
                        vbModifier = vbModifier.WithLeadingTrivia(newLeadingTrivia)
                        firstModifier = False
                        newLeadingTrivia = New SyntaxTriviaList
                    End If
                    Yield vbModifier
                    newLeadingTrivia = New SyntaxTriviaList
                    Continue For
                End If
                If firstModifier Then
                    vbModifier = vbModifier.WithLeadingTrivia(newLeadingTrivia)
                    firstModifier = False
                    newLeadingTrivia = New SyntaxTriviaList
                    Yield vbModifier
                    Continue For
                End If
                vbModifier = vbModifier.WithPrependedLeadingTrivia(newLeadingTrivia)
                If Not (vbModifier.IsKind(VB.SyntaxKind.None) OrElse vbModifier.IsKind(VB.SyntaxKind.EmptyToken)) Then
                    newLeadingTrivia = New SyntaxTriviaList
                    newLeadingTrivia = newLeadingTrivia.Add(Factory.Space)
                    ModifyTrailingTrivia(vbModifier.LeadingTrivia, newTrailingTrivia)
                    ModifyTrailingTrivia(vbModifier.TrailingTrivia, newTrailingTrivia)
                    Yield vbModifier.With(newLeadingTrivia, newTrailingTrivia)
                End If
            Next

        End Function

        Friend Function RestructureAttributeList(vbAttributeLists As SyntaxList(Of VBS.AttributeListSyntax), attributeLists As List(Of VBS.AttributeListSyntax), ByRef NewAttributeLeadingTrivia As SyntaxTriviaList, ByRef statementLeadingTrivia As SyntaxTriviaList, ByRef statementTrailingTrivia As SyntaxTriviaList) As Boolean
            Dim foundDirective As Boolean = False
            Dim foundTheory As Boolean = False
            Dim isTheoryOrInlineData As Boolean
            For Each e As IndexClass(Of VBS.AttributeListSyntax) In vbAttributeLists.WithIndex
                Dim attributeList As VBS.AttributeListSyntax = e.Value.RemoveExtraLeadingEOL
                isTheoryOrInlineData = attributeList.attributes.FirstOrDefault.ToString.ContainsAny(StringComparison.OrdinalIgnoreCase, "Theory", "InlineData")
                If isTheoryOrInlineData Then
                    foundTheory = True
                End If
                Dim newAttributLeadingTrivia As New SyntaxTriviaList
                If e.IsFirst Then
                    statementLeadingTrivia = statementLeadingTrivia.AddRange(attributeList.GetLeadingTrivia)
                    If statementLeadingTrivia.Any AndAlso statementLeadingTrivia.Last.IsWhitespaceOrEndOfLine Then
                        NewAttributeLeadingTrivia = NewAttributeLeadingTrivia.Add(attributeList.GetLeadingTrivia.Last)
                    Else
                        NewAttributeLeadingTrivia = NewAttributeLeadingTrivia.Add(Factory.Space)
                    End If
                Else
                    RelocateAttributeDirectiveDisabledTrivia(e.Value.GetLeadingTrivia, foundDirective, isTheoryOrInlineData, statementLeadingTrivia, statementTrailingTrivia)
                End If
                Dim newAttributeTrailingTrivia As SyntaxTriviaList = RelocateDirectiveDisabledTrivia(e.Value.GetTrailingTrivia, statementTrailingTrivia, RemoveEOL:=False)
                attributeLists.Add(attributeList.With(NewAttributeLeadingTrivia, newAttributeTrailingTrivia))
            Next
            Return foundTheory
        End Function

    End Module
End Namespace
