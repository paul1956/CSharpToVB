﻿' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports Microsoft.CodeAnalysis
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports VB = Microsoft.CodeAnalysis.VisualBasic

Friend Module SyntaxKindExtensions

    <Extension>
    Friend Function GetComparisonOperatorToken(lSyntaxKind As CS.SyntaxKind, firstParameterIsString As Boolean) As SyntaxToken
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

    ''' <summary>
    '''Get VB Expression Operator Token Kind from VB Expression Kind
    ''' </summary>
    ''' <param name="op"></param>
    ''' <returns></returns>
    ''' <param name="isReferenceType"></param>
    ''' <param name="IsIntegerMathOperation"></param>
    <Extension>
    Friend Function GetOperatorToken(op As VB.SyntaxKind, isReferenceType As Boolean, Optional ByRef IsIntegerMathOperation As Boolean = False) As SyntaxToken
        IsIntegerMathOperation = False
        Select Case op
            Case VB.SyntaxKind.EqualsExpression
                If isReferenceType Then
                    Return IsKeyword
                End If
                Return EqualsToken
            Case VB.SyntaxKind.NotEqualsExpression
                If isReferenceType Then
                    Return IsNotKeyword
                End If
                Return LessThanGreaterThanToken
            Case VB.SyntaxKind.GreaterThanExpression
                Return GreaterThanToken
            Case VB.SyntaxKind.GreaterThanOrEqualExpression
                Return GreaterThanEqualsToken
            Case VB.SyntaxKind.LessThanExpression
                Return LessThanToken
            Case VB.SyntaxKind.LessThanOrEqualExpression
                Return LessThanEqualsToken
            Case VB.SyntaxKind.OrExpression
                IsIntegerMathOperation = True
                Return OrKeyword
            Case VB.SyntaxKind.OrElseExpression
                Return OrElseKeyword
            Case VB.SyntaxKind.AndExpression
                IsIntegerMathOperation = True
                Return AndKeyword
            Case VB.SyntaxKind.AndAlsoExpression
                Return AndAlsoKeyword
            Case VB.SyntaxKind.AddExpression
                IsIntegerMathOperation = True
                Return PlusToken
            Case VB.SyntaxKind.ConcatenateExpression
                Return AmpersandToken
            Case VB.SyntaxKind.ConcatenateAssignmentStatement
                Return AmpersandEqualsToken
            Case VB.SyntaxKind.SubtractExpression
                IsIntegerMathOperation = True
                Return MinusToken
            Case VB.SyntaxKind.MultiplyExpression
                IsIntegerMathOperation = True
                Return AsteriskToken
            Case VB.SyntaxKind.DivideExpression
                IsIntegerMathOperation = True
                Return SlashToken
            Case VB.SyntaxKind.ModuloExpression
                IsIntegerMathOperation = True
                Return ModKeyword
            Case VB.SyntaxKind.SimpleAssignmentStatement
                Return EqualsToken
            Case VB.SyntaxKind.LeftShiftAssignmentStatement
                IsIntegerMathOperation = True
                Return LessThanLessThanEqualsToken
            Case VB.SyntaxKind.RightShiftAssignmentStatement
                Return GreaterThanGreaterThanEqualsToken
            Case VB.SyntaxKind.AddAssignmentStatement
                IsIntegerMathOperation = True
                Return PlusEqualsToken
            Case VB.SyntaxKind.SubtractAssignmentStatement
                IsIntegerMathOperation = True
                Return MinusEqualsToken
            Case VB.SyntaxKind.MultiplyAssignmentStatement
                IsIntegerMathOperation = True
                Return AsteriskEqualsToken
            Case VB.SyntaxKind.DivideAssignmentStatement
                IsIntegerMathOperation = True
                Return SlashEqualsToken
            Case VB.SyntaxKind.UnaryPlusExpression
                IsIntegerMathOperation = True
                Return PlusToken
            Case VB.SyntaxKind.UnaryMinusExpression
                IsIntegerMathOperation = True
                Return MinusToken
            Case VB.SyntaxKind.NotExpression
                Return NotKeyword
            Case VB.SyntaxKind.RightShiftExpression
                IsIntegerMathOperation = True
                Return GreaterThanGreaterThanToken
            Case VB.SyntaxKind.LeftShiftExpression
                IsIntegerMathOperation = True
                Return LessThanLessThanToken
            Case VB.SyntaxKind.AddressOfExpression
                Return AddressOfKeyword
            Case VB.SyntaxKind.ExclusiveOrExpression
                IsIntegerMathOperation = True
                Return XorKeyword
        End Select

        Throw New ArgumentOutOfRangeException($"op = {op}")
    End Function

    <Extension>
    Friend Function GetPredefinedType(t As VB.SyntaxKind) As SyntaxToken

        Select Case t
                ' built-in types
            Case VB.SyntaxKind.BooleanKeyword
                Return BooleanKeyword
            Case VB.SyntaxKind.ByteKeyword
                Return ByteKeyword
            Case VB.SyntaxKind.SByteKeyword
                Return SByteKeyword
            Case VB.SyntaxKind.ShortKeyword
                Return ShortKeyword
            Case VB.SyntaxKind.UShortKeyword
                Return UShortKeyword
            Case VB.SyntaxKind.IntegerKeyword
                Return IntegerKeyword
            Case VB.SyntaxKind.UIntegerKeyword
                Return UIntegerKeyword
            Case VB.SyntaxKind.LongKeyword
                Return LongKeyword
            Case VB.SyntaxKind.ULongKeyword
                Return ULongKeyword
            Case VB.SyntaxKind.DoubleKeyword
                Return DoubleKeyword
            Case VB.SyntaxKind.DecimalKeyword
                Return DecimalKeyword
            Case VB.SyntaxKind.StringKeyword
                Return StringKeyword
            Case VB.SyntaxKind.CharKeyword
                Return CharKeyword
            Case VB.SyntaxKind.ObjectKeyword
                Return ObjectKeyword
        End Select

        Throw New NotSupportedException($"Type.Kind {t} is not supported!")
    End Function

    <Extension>
    Friend Function GetTypeToken(t As CS.SyntaxKind, Optional context As TokenContext = TokenContext.Global) As SyntaxToken
        Select Case t
            Case CS.SyntaxKind.None
                Return EmptyToken
                ' built-in types
            Case CS.SyntaxKind.BoolKeyword
                Return BooleanKeyword
            Case CS.SyntaxKind.ByteKeyword
                Return ByteKeyword
            Case CS.SyntaxKind.SByteKeyword
                Return SByteKeyword
            Case CS.SyntaxKind.ShortKeyword
                Return ShortKeyword
            Case CS.SyntaxKind.UShortKeyword
                Return UShortKeyword
            Case CS.SyntaxKind.IntKeyword
                Return IntegerKeyword
            Case CS.SyntaxKind.UIntKeyword
                Return UIntegerKeyword
            Case CS.SyntaxKind.LongKeyword
                Return LongKeyword
            Case CS.SyntaxKind.ULongKeyword
                Return ULongKeyword
            Case CS.SyntaxKind.DoubleKeyword
                Return DoubleKeyword
            Case CS.SyntaxKind.FloatKeyword
                Return SingleKeyword
            Case CS.SyntaxKind.DecimalKeyword
                Return DecimalKeyword
            Case CS.SyntaxKind.StringKeyword
                Return StringKeyword
            Case CS.SyntaxKind.CharKeyword
                Return CharKeyword
            Case CS.SyntaxKind.VoidKeyword
                ' not supported
                If context = TokenContext.XmlComment Then
                    Return NothingKeyword
                End If
                Return EmptyToken
            Case CS.SyntaxKind.ObjectKeyword
                Return ObjectKeyword
        End Select

        Throw New NotSupportedException($"Type.Kind {t} is not supported!")
    End Function

    <Extension>
    Friend Function GetExpressionKind(t As CS.SyntaxKind) As VB.SyntaxKind
        Select Case t
            Case CS.SyntaxKind.None
                Return VB.SyntaxKind.None
            Case CS.SyntaxKind.AddExpression
                Return VB.SyntaxKind.AddExpression
            Case CS.SyntaxKind.SubtractExpression
                Return VB.SyntaxKind.SubtractExpression
            Case CS.SyntaxKind.MultiplyExpression
                Return VB.SyntaxKind.MultiplyExpression
            Case CS.SyntaxKind.DivideExpression
                Return VB.SyntaxKind.DivideExpression
            Case CS.SyntaxKind.ModuloExpression
                Return VB.SyntaxKind.ModuloExpression
            Case CS.SyntaxKind.LeftShiftExpression
                Return VB.SyntaxKind.LeftShiftExpression
            Case CS.SyntaxKind.RightShiftExpression
                Return VB.SyntaxKind.RightShiftExpression
            Case CS.SyntaxKind.LogicalOrExpression
                Return VB.SyntaxKind.OrElseExpression
            Case CS.SyntaxKind.LogicalAndExpression
                Return VB.SyntaxKind.AndAlsoExpression
            Case CS.SyntaxKind.BitwiseOrExpression
                Return VB.SyntaxKind.OrExpression
            Case CS.SyntaxKind.BitwiseAndExpression
                Return VB.SyntaxKind.AndExpression
            Case CS.SyntaxKind.ExclusiveOrExpression
                Return VB.SyntaxKind.ExclusiveOrExpression
            Case CS.SyntaxKind.EqualsExpression
                Return VB.SyntaxKind.EqualsExpression
            Case CS.SyntaxKind.NotEqualsExpression
                Return VB.SyntaxKind.NotEqualsExpression
            Case CS.SyntaxKind.LessThanExpression
                Return VB.SyntaxKind.LessThanExpression
            Case CS.SyntaxKind.LessThanOrEqualExpression
                Return VB.SyntaxKind.LessThanOrEqualExpression
            Case CS.SyntaxKind.GreaterThanExpression
                Return VB.SyntaxKind.GreaterThanExpression
            Case CS.SyntaxKind.GreaterThanOrEqualExpression
                Return VB.SyntaxKind.GreaterThanOrEqualExpression
            Case CS.SyntaxKind.SimpleAssignmentExpression
                Return VB.SyntaxKind.SimpleAssignmentStatement
            Case CS.SyntaxKind.AddAssignmentExpression
                Return VB.SyntaxKind.AddAssignmentStatement
            Case CS.SyntaxKind.SubtractAssignmentExpression
                Return VB.SyntaxKind.SubtractAssignmentStatement
            Case CS.SyntaxKind.MultiplyAssignmentExpression
                Return VB.SyntaxKind.MultiplyAssignmentStatement
            Case CS.SyntaxKind.DivideAssignmentExpression
                Return VB.SyntaxKind.DivideAssignmentStatement
            Case CS.SyntaxKind.ModuloAssignmentExpression
                Return VB.SyntaxKind.ModuloExpression
            Case CS.SyntaxKind.AndAssignmentExpression
                Return VB.SyntaxKind.AndExpression
            Case CS.SyntaxKind.ExclusiveOrAssignmentExpression
                Return VB.SyntaxKind.ExclusiveOrExpression
            Case CS.SyntaxKind.OrAssignmentExpression
                Return VB.SyntaxKind.OrExpression
            Case CS.SyntaxKind.LeftShiftAssignmentExpression
                Return VB.SyntaxKind.LeftShiftAssignmentStatement
            Case CS.SyntaxKind.RightShiftAssignmentExpression
                Return VB.SyntaxKind.RightShiftAssignmentStatement
            Case CS.SyntaxKind.UnaryPlusExpression
                Return VB.SyntaxKind.UnaryPlusExpression
            Case CS.SyntaxKind.UnaryMinusExpression
                Return VB.SyntaxKind.UnaryMinusExpression
            Case CS.SyntaxKind.BitwiseNotExpression
                Return VB.SyntaxKind.NotExpression
            Case CS.SyntaxKind.LogicalNotExpression
                Return VB.SyntaxKind.NotExpression
            Case CS.SyntaxKind.PreIncrementExpression
                Return VB.SyntaxKind.AddAssignmentStatement
            Case CS.SyntaxKind.PreDecrementExpression
                Return VB.SyntaxKind.SubtractAssignmentStatement
            Case CS.SyntaxKind.PostIncrementExpression
                Return VB.SyntaxKind.AddAssignmentStatement
            Case CS.SyntaxKind.PostDecrementExpression
                Return VB.SyntaxKind.SubtractAssignmentStatement
            Case CS.SyntaxKind.PlusPlusToken
                Return VB.SyntaxKind.PlusToken
            Case CS.SyntaxKind.MinusMinusToken
                Return VB.SyntaxKind.MinusToken
            Case CS.SyntaxKind.IsExpression
                Return VB.SyntaxKind.IsExpression
            Case CS.SyntaxKind.AddressOfExpression
                Return VB.SyntaxKind.AddressOfExpression
            Case CS.SyntaxKind.OrPattern
                Return VB.SyntaxKind.OrExpression
            Case CS.SyntaxKind.AndPattern
                Return VB.SyntaxKind.AndExpression
                ' Remainder of list are so caller don't crash but are not directly supported by VB
            Case CS.SyntaxKind.PointerIndirectionExpression
                Return CType(CS.SyntaxKind.PointerIndirectionExpression, VB.SyntaxKind)
            Case CS.SyntaxKind.CoalesceExpression
                Return CType(CS.SyntaxKind.CoalesceExpression, VB.SyntaxKind)
            Case CS.SyntaxKind.AsExpression
                Return CType(CS.SyntaxKind.AsExpression, VB.SyntaxKind)
            Case CS.SyntaxKind.IndexExpression
                Return CType(CS.SyntaxKind.IndexExpression, VB.SyntaxKind)
        End Select
        Throw New NotSupportedException($"Expression.Kind {t} is not supported!")
    End Function

End Module
