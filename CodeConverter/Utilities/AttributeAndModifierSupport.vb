' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports Extensions
Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports VB = Microsoft.CodeAnalysis.VisualBasic

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

    <Extension>
    Private Function GetVisibilityKeyword(t As CS.SyntaxKind, isModule As Boolean, context As TokenContext, ByRef foundVisibility As Boolean) As SyntaxToken
        Select Case t
            Case CS.SyntaxKind.None
                Return EmptyToken
            Case CS.SyntaxKind.PublicKeyword
                foundVisibility = True
                Return PublicKeyword
            Case CS.SyntaxKind.PrivateKeyword
                If foundVisibility Then
                    Return EmptyToken
                End If
                foundVisibility = True
                Return PrivateKeyword
            Case CS.SyntaxKind.InternalKeyword
                foundVisibility = True
                Return FriendKeyword
            Case CS.SyntaxKind.ProtectedKeyword
                Return ProtectedKeyword
            Case CS.SyntaxKind.ReadOnlyKeyword
                If context = TokenContext.Struct Then
                    Return EmptyToken
                End If
                Return ReadOnlyKeyword
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
                Return If(context = TokenContext.Global OrElse context = TokenContext.Class, MustInheritKeyword, MustOverrideKeyword)
            Case CS.SyntaxKind.ConstKeyword
                If context = TokenContext.Readonly Then
                    Return ReadOnlyKeyword
                End If
                Return ConstKeyword
            Case CS.SyntaxKind.SealedKeyword
                Return If(context = TokenContext.Global OrElse context = TokenContext.Class, NotInheritableKeyword, NotOverridableKeyword)
            Case CS.SyntaxKind.StaticKeyword
                If isModule Then
                    If context = TokenContext.VariableOrConst Then
                        If foundVisibility Then
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
                If context = TokenContext.Global Then
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

End Module
