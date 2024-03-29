﻿' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports CSharpToVBConverter.CSharpToVBVisitors.CSharpConverter
Imports Microsoft.CodeAnalysis
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Public Module ConvertLiteralSupport

    Private Function Binary(value As Byte) As String
        Return $"&B{Convert.ToString(value, 2).PadLeft(8, "0"c)}"
    End Function

    Private Function Binary(value As SByte) As String
        Return $"&B{Convert.ToString(value, 2).PadLeft(8, "0"c)}"
    End Function

    Private Function Binary(value As Short) As String
        Return $"&B{Convert.ToString(value, 2).PadLeft(16, "0"c)}"
    End Function

    Private Function Binary(value As UShort) As String
        Return $"&B{Convert.ToString(value, 2).PadLeft(16, "0"c)}"
    End Function

    Private Function Binary(value As Integer) As String
        Return $"&B{Convert.ToString(value, 2).PadLeft(32, "0"c)}"
    End Function

    Private Function Binary(value As UInteger) As String
        Return $"&B{Convert.ToString(value, 2).PadLeft(32, "0"c)}UI"
    End Function

    Private Function Binary(value As Long) As String
        Return $"&B{Convert.ToString(value, 2).PadLeft(64, "0"c)}"
    End Function

    <Extension>
    Private Function ConvertUnicodeQuotes(tokenString As String, unicodeQuote As String) As String
        tokenString = tokenString.Replace(unicodeQuote & unicodeQuote, ChrW(0), StringComparison.Ordinal)
        tokenString = tokenString.Replace(unicodeQuote, unicodeQuote & unicodeQuote, StringComparison.Ordinal)
        Return tokenString.Replace(ChrW(0), unicodeQuote & unicodeQuote, StringComparison.Ordinal)
    End Function

    Private Function GetTypeCharacters(tokenAString As String) As String
        Dim typeChars As String = ""
        For i As Integer = tokenAString.Length - 1 To 0 Step -1
            If Char.IsLetter(tokenAString.Chars(i)) Then
                typeChars = tokenAString.Chars(i) & typeChars
            Else
                Exit For
            End If
        Next

        Return typeChars
    End Function

    Private Function TranslateTypeCharacter(token As SyntaxToken) As String

        Dim tokenAString As String = token.ToString
        Dim typeChars As String = GetTypeCharacters(tokenAString)

        If typeChars.Length = 0 Then
            Return tokenAString
        End If
        Dim newType As String
        Select Case typeChars.ToUpperInvariant
            Case "F"
                newType = "F"
            Case "M"
                newType = "D"
            Case "D"
                newType = "R"
            Case "S"
                newType = "S"
            Case "L"
                newType = "L"
            Case "U", "UL", "LU"
                newType = "UL"
            Case Else
                Throw UnreachableException()
        End Select
        Return tokenAString.Replace(typeChars, newType, StringComparison.OrdinalIgnoreCase)
    End Function

    Friend Function ConvertCSharpEscapes(tokenString As String) As String
        Dim buffer As String
        Try
            buffer = tokenString.
                    Replace("\r\n", "{vbCrLf}", StringComparison.Ordinal).
                    Replace("\'", "'", StringComparison.Ordinal).
                    Replace("\0", "{ChrW(0)}", StringComparison.Ordinal).
                    Replace("\a", "{&H7}", StringComparison.Ordinal).
                    Replace("\c", "{vbCr}", StringComparison.Ordinal).
                    Replace("\b", "{ChrW(&H8)}", StringComparison.Ordinal).
                    Replace("\f", "{ChrW(12)}", StringComparison.Ordinal).
                    Replace("\n", "{vbLf}", StringComparison.Ordinal).
                    Replace("\t", "{vbTab}", StringComparison.Ordinal).
                    Replace("\=", "=", StringComparison.Ordinal).
                    Replace("\,", ",", StringComparison.Ordinal).
                    Replace("\""", Quote, StringComparison.Ordinal).
                    Replace("\\", "\", StringComparison.Ordinal).
                    Replace(Quote, DoubleQuote, StringComparison.Ordinal).NormalizeLineEndings
            If buffer.Contains(UnicodeOpenQuote, StringComparison.Ordinal) Then
                buffer = buffer.ConvertUnicodeQuotes(UnicodeOpenQuote)
            End If
            If buffer.Contains(UnicodeCloseQuote, StringComparison.Ordinal) Then
                buffer = buffer.ConvertUnicodeQuotes(UnicodeCloseQuote)
            End If
            If buffer.Contains(UnicodeFullWidthQuotationMark, StringComparison.Ordinal) Then
                buffer = buffer.ConvertUnicodeQuotes(UnicodeFullWidthQuotationMark)
            End If

            tokenString = buffer
        Catch ex As OperationCanceledException
            Throw
        Catch ex As Exception
            Throw
        End Try
        Return tokenString
    End Function

    Friend Function GetLiteralExpression(value As Object, token As SyntaxToken, nodesVisitor As NodesVisitor) As VBS.ExpressionSyntax
        Select Case token.RawKind
            Case CS.SyntaxKind.NumericLiteralToken
                Dim tokenToString As String = token.ToString
                If tokenToString.StartsWith("0x", StringComparison.OrdinalIgnoreCase) Then
                    Dim hexValueString As String = $"&H{tokenToString.Substring(2)}".Replace("ul", "", StringComparison.OrdinalIgnoreCase).Replace("u", "", StringComparison.OrdinalIgnoreCase).Replace("l", "", StringComparison.OrdinalIgnoreCase)
                    If TypeOf value Is Integer Then Return Factory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, Factory.Literal(hexValueString, CInt(value)))
                    If TypeOf value Is SByte Then Return Factory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, Factory.Literal(hexValueString, CSByte(value)))
                    If TypeOf value Is Short Then Return Factory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, Factory.Literal(hexValueString, CShort(value)))
                    If TypeOf value Is UShort Then Return Factory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, Factory.Literal(hexValueString, CUShort(value)))
                    If TypeOf value Is UInteger Then Return Factory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, Factory.Literal(hexValueString & "UI", CUInt(value)))
                    If TypeOf value Is Long Then Return Factory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, Factory.Literal(hexValueString, CLng(value)))
                    If TypeOf value Is ULong Then Return Factory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, Factory.Literal(hexValueString & "UL", CULng(value)))
                    Throw UnreachableException
                End If
                If tokenToString.StartsWith("0b", StringComparison.OrdinalIgnoreCase) Then
                    If TypeOf value Is Integer Then Return Factory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, Factory.Literal($"{Binary(CInt(value))}", CInt(value)))
                    If TypeOf value Is Byte Then Return Factory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, Factory.Literal($"{Binary(CByte(value))}", CByte(value)))
                    If TypeOf value Is SByte Then Return Factory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, Factory.Literal($"{Binary(CSByte(value))}", CSByte(value)))
                    If TypeOf value Is Short Then Return Factory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, Factory.Literal($"{Binary(CShort(value))}", CShort(value)))
                    If TypeOf value Is UShort Then Return Factory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, Factory.Literal($"{Binary(CUShort(value))}", CUShort(value)))
                    If TypeOf value Is UInteger Then Return Factory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, Factory.Literal($"{Binary(CUInt(value))}", CUInt(value)))
                    If TypeOf value Is Long Then Return Factory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, Factory.Literal($"{Binary(CLng(value))}", CLng(value)))
                    If TypeOf value Is ULong Then Return Factory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, Factory.Literal($"{Binary(CType(CULng(value), Long))}UL", CULng(value)))
                    Throw UnreachableException
                End If

                Dim valueText As String = TranslateTypeCharacter(token)
                If TypeOf value Is Integer Then Return Factory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, Factory.Literal(valueText, CInt(value)))
                If TypeOf value Is Byte Then Return Factory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, Factory.Literal(valueText, CByte(value)))
                If TypeOf value Is SByte Then Return Factory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, Factory.Literal(valueText, CSByte(value)))
                If TypeOf value Is Short Then Return Factory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, Factory.Literal(valueText, CShort(value)))
                If TypeOf value Is UShort Then Return Factory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, Factory.Literal(valueText, CUShort(value)))
                If TypeOf value Is UInteger Then Return Factory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, Factory.Literal(valueText, CUInt(value)))
                If TypeOf value Is Long Then Return Factory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, Factory.Literal(valueText, CLng(value)))
                If TypeOf value Is ULong Then
                    If GetTypeCharacters(valueText).Length = 0 AndAlso CULng(value) >= Long.MaxValue Then
                        valueText &= "UL"
                    End If
                    Return Factory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, Factory.Literal(valueText, CULng(value)))
                End If
                If TypeOf value Is Single Then Return Factory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, Factory.Literal(valueText, CSng(value)))
                If TypeOf value Is Double Then Return Factory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, Factory.Literal(valueText, CDbl(value)))
                If TypeOf value Is Decimal Then Return Factory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, Factory.Literal(valueText, CDec(value)))
                Throw UnreachableException
            Case CS.SyntaxKind.StringLiteralToken
                Dim strValue As String = TryCast(value, String)
                If strValue IsNot Nothing Then
                    If strValue.Contains("\"c, StringComparison.Ordinal) Then
                        strValue = ConvertCSharpEscapes(strValue)
                    End If
                    If strValue.Contains(UnicodeOpenQuote, StringComparison.Ordinal) Then
                        strValue = strValue.ConvertUnicodeQuotes(UnicodeOpenQuote)
                    End If
                    If strValue.Contains(UnicodeCloseQuote, StringComparison.Ordinal) Then
                        strValue = strValue.ConvertUnicodeQuotes(UnicodeCloseQuote)
                    End If
                    If strValue.Contains(UnicodeFullWidthQuotationMark, StringComparison.Ordinal) Then
                        strValue = strValue.ConvertUnicodeQuotes(UnicodeFullWidthQuotationMark)
                    End If
                    If strValue.Length = 0 Then
                        Return DoubleQuoteExpression
                    End If
                    Select Case strValue
                        Case vbCrLf
                            Return Factory.ParseExpression("Microsoft.VisualBasic.vbNewLine")
                        Case vbCr
                            Return Factory.ParseExpression("Microsoft.VisualBasic.vbCr")
                        Case vbLf
                            Return Factory.ParseExpression("Microsoft.VisualBasic.vbLf")
                        Case vbVerticalTab
                            Return Factory.ParseExpression("Microsoft.VisualBasic.vbVerticalTab")
                        Case vbBack
                            Return Factory.ParseExpression("Microsoft.VisualBasic.vbBack")
                        Case vbFormFeed
                            Return Factory.ParseExpression("Microsoft.VisualBasic.vbFormFeed")
                    End Select
                    Return Factory.LiteralExpression(VB.SyntaxKind.StringLiteralExpression, Factory.Literal(strValue))
                End If
            Case CS.SyntaxKind.FalseKeyword
                Return Factory.FalseLiteralExpression(FalseKeyword)
            Case CS.SyntaxKind.NullKeyword
                Return NothingExpression
            Case CS.SyntaxKind.TrueKeyword
                Return Factory.TrueLiteralExpression(TrueKeyword)
            Case CS.SyntaxKind.CharacterLiteralToken
                If AscW(CChar(value)) = &H201C Then
                    Return Factory.LiteralExpression(VB.SyntaxKind.CharacterLiteralExpression, Factory.Literal($"{UnicodeOpenQuote}{UnicodeOpenQuote}"))
                End If
                If AscW(CChar(value)) = &H201D Then
                    Return Factory.LiteralExpression(VB.SyntaxKind.CharacterLiteralExpression, Factory.Literal($"{UnicodeCloseQuote}{UnicodeCloseQuote}"))
                End If
                If token.Text.StartsWith("'\u", StringComparison.OrdinalIgnoreCase) Then
                    Return Factory.ParseExpression($"ChrW(&H{token.Text.RemoveAll("'").Substring(2)})")
                End If
                Return Factory.LiteralExpression(VB.SyntaxKind.CharacterLiteralExpression, Factory.Literal(CChar(value)))
            Case CS.SyntaxKind.DefaultKeyword
                Dim methodStatement As CSS.MethodDeclarationSyntax = token.Parent.GetAncestor(Of CSS.MethodDeclarationSyntax)
                Dim returnType As VBS.TypeSyntax
                If token.Parent.GetAncestor(Of CSS.ReturnStatementSyntax) IsNot Nothing OrElse
                    token.Parent.GetAncestor(Of CSS.ArrowExpressionClauseSyntax) IsNot Nothing Then
                    Dim propertyDeclaration As CSS.PropertyDeclarationSyntax = token.Parent.GetAncestor(Of CSS.PropertyDeclarationSyntax)
                    If propertyDeclaration IsNot Nothing Then
                        returnType = DirectCast(propertyDeclaration.Type.Accept(nodesVisitor), VBS.TypeSyntax)
                        Return Factory.CTypeExpression(NothingExpression, returnType)
                    End If
                    If methodStatement IsNot Nothing Then
                        returnType = DirectCast(methodStatement.ReturnType.Accept(nodesVisitor), VBS.TypeSyntax)
                        Return Factory.CTypeExpression(NothingExpression, returnType)
                    End If
                    Dim operatorStatement As CSS.ConversionOperatorDeclarationSyntax = token.Parent.GetAncestor(Of CSS.ConversionOperatorDeclarationSyntax)
                    If operatorStatement IsNot Nothing Then
                        returnType = DirectCast(operatorStatement.Type.Accept(nodesVisitor), VBS.TypeSyntax)
                        Return Factory.CTypeExpression(NothingExpression, returnType)
                    End If
                    Throw UnreachableException
                End If
                Dim equalsValue As CSS.EqualsValueClauseSyntax = token.Parent.GetAncestor(Of CSS.EqualsValueClauseSyntax)
                If equalsValue IsNot Nothing Then
                    Dim parameter As CSS.ParameterSyntax = token.Parent.GetAncestor(Of CSS.ParameterSyntax)
                    If parameter IsNot Nothing Then
                        returnType = DirectCast(parameter.Type.Accept(nodesVisitor), VBS.TypeSyntax)
                        Return Factory.CTypeExpression(NothingExpression, returnType)
                    End If
                    Throw UnreachableException
                End If
                Dim assignmentExpression As CSS.AssignmentExpressionSyntax = token.Parent.GetAncestor(Of CSS.AssignmentExpressionSyntax)
                If assignmentExpression IsNot Nothing Then
                    If methodStatement IsNot Nothing Then
                        If TypeOf assignmentExpression.Left Is CSS.ThisExpressionSyntax Then
                            Dim classAncestor As CSS.ClassDeclarationSyntax = token.Parent.GetAncestor(Of CSS.ClassDeclarationSyntax)
                            If classAncestor IsNot Nothing Then
                                Return Factory.CTypeExpression(NothingExpression, Factory.ParseTypeName(classAncestor.Identifier.ValueText))
                            End If
                            Dim structAncestor As CSS.StructDeclarationSyntax = token.Parent.GetAncestor(Of CSS.StructDeclarationSyntax)
                            If structAncestor IsNot Nothing Then
                                Return Factory.CTypeExpression(NothingExpression, Factory.ParseTypeName(structAncestor.Identifier.ValueText))
                            End If
                            Return NothingExpression
                        End If

                        If assignmentExpression.Left.RawKind = CS.SyntaxKind.PointerIndirectionExpression OrElse
                            TypeOf assignmentExpression.Left Is CSS.DeclarationExpressionSyntax Then
                            Return NothingExpression
                        End If

                        Dim tuple As CSS.TupleExpressionSyntax = TryCast(assignmentExpression.Left, CSS.TupleExpressionSyntax)
                        If tuple IsNot Nothing Then
                            'Dim parent As SyntaxNode = assignmentExpression.Parent
                            ' This could be better if I could figure out the Type
                            Return NothingExpression
                        End If

                        Dim idString As String
                        If TypeOf assignmentExpression.Left Is CSS.IdentifierNameSyntax Then
                            idString = DirectCast(assignmentExpression.Left, CSS.IdentifierNameSyntax).Identifier.ValueText
                        ElseIf TypeOf assignmentExpression.Left Is CSS.MemberAccessExpressionSyntax Then
                            idString = DirectCast(assignmentExpression.Left, CSS.MemberAccessExpressionSyntax).Name.ToString
                        ElseIf TypeOf assignmentExpression.Left Is CSS.ElementAccessExpressionSyntax Then
                            idString = DirectCast(assignmentExpression.Left, CSS.ElementAccessExpressionSyntax).Expression.ToString
                        Else
                            Throw UnreachableException()
                        End If
                        For Each p As CSS.ParameterSyntax In methodStatement.ParameterList.Parameters
                            If p.Identifier.ValueText = idString Then
                                returnType = DirectCast(p.Type.Accept(nodesVisitor), VBS.TypeSyntax)
                                Return Factory.CTypeExpression(NothingExpression, returnType)
                            End If
                        Next
                        Return NothingExpression
                    End If
                End If
                If token.Parent.GetAncestor(Of CSS.ConstructorDeclarationSyntax) IsNot Nothing Then
                    Return NothingExpression
                End If
                If token.Parent.GetAncestor(Of CSS.ArgumentSyntax) IsNot Nothing Then
                    Return NothingExpression
                End If
                Return NothingExpression
            Case CS.SyntaxKind.ArgListKeyword
                Return Factory.IdentifierName("__ArgList")
        End Select
        Return NothingExpression
    End Function

End Module
