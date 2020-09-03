' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports Microsoft.CodeAnalysis
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports VB = Microsoft.CodeAnalysis.VisualBasic

Namespace CSharpToVBCodeConverter.ToVisualBasic
    Module SyntaxKindExtensions

        <Extension>
        Friend Function ConvertModifierKindToVBKeyword(t As CS.SyntaxKind, IsModule As Boolean, context As TokenContext, ByRef FoundVisibility As Boolean) As SyntaxToken
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

        <Extension>
        Friend Function ConvertOperatorDeclarationToken(lSyntaxKind As CS.SyntaxKind, firstParameterIsString As Boolean) As SyntaxToken
            Select Case lSyntaxKind
                Case CS.SyntaxKind.AmpersandToken
                    Return AndKeyword
                Case CS.SyntaxKind.AsteriskEqualsToken
                    Return AsteriskEqualsToken
                Case CS.SyntaxKind.AsteriskToken
                    Return AsteriskToken
                Case CS.SyntaxKind.BarToken
                    Return OrKeyword
                Case CS.SyntaxKind.CaretToken
                    Return XorKeyword
                Case CS.SyntaxKind.EqualsEqualsToken
                    Return EqualsToken
                Case CS.SyntaxKind.EqualsGreaterThanToken
                    Return GreaterThanEqualsToken
                Case CS.SyntaxKind.ExclamationEqualsToken
                    Return LessThanGreaterThanToken
                Case CS.SyntaxKind.ExclamationToken
                    Return NotKeyword
                Case CS.SyntaxKind.GreaterThanEqualsToken
                    Return GreaterThanEqualsToken
                Case CS.SyntaxKind.GreaterThanToken
                    Return GreaterThanToken
                Case CS.SyntaxKind.GreaterThanGreaterThanToken
                    Return GreaterThanGreaterThanToken
                Case CS.SyntaxKind.LessThanToken
                    Return LessThanToken
                Case CS.SyntaxKind.LessThanLessThanEqualsToken
                    Return LessThanLessThanEqualsToken
                Case CS.SyntaxKind.LessThanLessThanToken
                    Return LessThanLessThanToken
                Case CS.SyntaxKind.LessThanEqualsToken
                    Return LessThanEqualsToken
                Case CS.SyntaxKind.MinusToken
                    Return MinusToken
                Case CS.SyntaxKind.PlusToken
                    Return If(firstParameterIsString, AmpersandToken, PlusToken)
                Case CS.SyntaxKind.SlashToken
                    Return SlashToken
                Case CS.SyntaxKind.TildeToken
                    Return NotKeyword
                Case CS.SyntaxKind.FalseKeyword
                    Return IsFalse
                Case CS.SyntaxKind.TrueKeyword
                    Return IsTrueKeyword
            End Select
            Throw New NotSupportedException($"Assignment Operator {lSyntaxKind} is not supported")
        End Function

        <Extension>
        Friend Function MatchesKind(Kind As VB.SyntaxKind, ParamArray Kinds() As VB.SyntaxKind) As Boolean
            Return Kinds.Contains(Kind)
        End Function


    End Module
End Namespace
