' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports Extensions
Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace Utilities

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
            XmlComment
            Struct
            [Property]
            LocalFunction
        End Enum

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
        Private Function ConvertModifier(csToken As SyntaxToken, isModule As Boolean, context As TokenContext, ByRef foundVisibility As Boolean) As SyntaxToken
            Dim token As SyntaxToken = CS.CSharpExtensions.Kind(csToken).GetVisibilityKeyword(isModule, context, foundVisibility)
            If token.IsKind(VB.SyntaxKind.EmptyToken) Then
                Return EmptyToken.WithConvertedLeadingTriviaFrom(csToken)
            End If
            Return token.WithConvertedTriviaFrom(csToken)
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

        Friend Iterator Function ConvertModifiers(csModifiers As IEnumerable(Of SyntaxToken), isModule As Boolean, context As TokenContext) As IEnumerable(Of SyntaxToken)
            Dim foundVisibility As Boolean = False
            Dim newLeadingTrivia As New SyntaxTriviaList
            Dim firstModifier As Boolean = True
            If csModifiers.Any Then
                newLeadingTrivia = newLeadingTrivia.AddRange(csModifiers(0).LeadingTrivia.ConvertTriviaList())
            End If

            If context <> TokenContext.Local AndAlso context <> TokenContext.InterfaceOrModule AndAlso context <> TokenContext.Class Then
                Dim visibility As Boolean = False
                For Each token As SyntaxToken In csModifiers
                    If IsVisibility(token, context) Then
                        visibility = True
                        Exit For
                    End If
                Next

                If Not visibility AndAlso context = TokenContext.Member Then
                    Dim defaultVisibility As SyntaxToken = CSharpDefaultVisibility(context)
                    If firstModifier Then
                        Yield defaultVisibility.WithLeadingTrivia(newLeadingTrivia)
                        newLeadingTrivia = New SyntaxTriviaList
                        newLeadingTrivia = newLeadingTrivia.Add(SpaceTrivia)
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
                Dim vbModifier As SyntaxToken = csModifier.ConvertModifier(isModule, context, foundVisibility)
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

                If IgnoreInContext(csModifier, context) Then
                    If firstModifier Then
                        vbModifier = vbModifier.WithLeadingTrivia(newLeadingTrivia)
                        firstModifier = False
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
                    newLeadingTrivia = newLeadingTrivia.Add(SpaceTrivia)
                    ModifyTrailingTrivia(vbModifier.LeadingTrivia, newTrailingTrivia)
                    ModifyTrailingTrivia(vbModifier.TrailingTrivia, newTrailingTrivia)
                    Yield vbModifier.With(newLeadingTrivia, newTrailingTrivia)
                End If
            Next

        End Function

        Friend Function RestructureAttributeList(vbAttributeLists As SyntaxList(Of VBS.AttributeListSyntax), attributeLists As List(Of VBS.AttributeListSyntax), ByRef newAttributeLeadingTrivia As SyntaxTriviaList, ByRef statementLeadingTrivia As SyntaxTriviaList, ByRef statementTrailingTrivia As SyntaxTriviaList) As Boolean
            Dim foundDirective As Boolean = False
            Dim foundTheory As Boolean = False
            Dim isTheoryOrInlineData As Boolean
            For Each e As IndexClass(Of VBS.AttributeListSyntax) In vbAttributeLists.WithIndex
                Dim attributeList As VBS.AttributeListSyntax = e.Value.RemoveExtraLeadingEol
                isTheoryOrInlineData = attributeList.Attributes.FirstOrDefault.ToString.ContainsAny(StringComparison.OrdinalIgnoreCase, "Theory", "InlineData")
                If isTheoryOrInlineData Then
                    foundTheory = True
                End If
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
            Return foundTheory
        End Function

    End Module
End Namespace
