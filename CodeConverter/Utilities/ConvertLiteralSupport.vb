' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports CSharpToVBConverter.ToVisualBasic.CSharpConverter
Imports Microsoft.CodeAnalysis
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBConverter

    Public Module ConvertLiteralSupport

        Private Function Binary(Value As Byte) As String
            Return $"&B{Convert.ToString(Value, toBase:=2).PadLeft(totalWidth:=8, "0"c)}"
        End Function

        Private Function Binary(Value As SByte) As String
            Return $"&B{Convert.ToString(Value, toBase:=2).PadLeft(totalWidth:=8, "0"c)}"
        End Function

        Private Function Binary(Value As Short) As String
            Return $"&B{Convert.ToString(Value, toBase:=2).PadLeft(totalWidth:=16, "0"c)}"
        End Function

        Private Function Binary(Value As UShort) As String
            Return $"&B{Convert.ToString(Value, toBase:=2).PadLeft(totalWidth:=16, "0"c)}"
        End Function

        Private Function Binary(Value As Integer) As String
            Return $"&B{Convert.ToString(Value, toBase:=2).PadLeft(totalWidth:=32, "0"c)}"
        End Function

        Private Function Binary(Value As UInteger) As String
            Return $"&B{Convert.ToString(Value, toBase:=2).PadLeft(totalWidth:=32, "0"c)}UI"
        End Function

        Private Function Binary(Value As Long) As String
            Return $"&B{Convert.ToString(Value, toBase:=2).PadLeft(totalWidth:=64, "0"c)}"
        End Function

        Private Function GetTypeCharacters(TokenAString As String) As String
            Dim typeChars As String = ""
            For i As Integer = TokenAString.Length - 1 To 0 Step -1
                If Char.IsLetter(TokenAString.Chars(i)) Then
                    typeChars = TokenAString.Chars(i) & typeChars
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
            Dim newType As String = ""
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
                    Stop
            End Select
            Return tokenAString.Replace(typeChars, newType, StringComparison.OrdinalIgnoreCase)
        End Function

        Friend Function ConvertCSharpEscapes(TokenString As String) As String
            Dim buffer As String
            Try
                'Dim unescape1 As String = RegexParser.Unescape(TokenString)
                buffer = TokenString.
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
                ' TODO Remove comment
                '_Buffer = _Buffer.Replace("{", "{{", StringComparison.Ordinal).
                '                    Replace("}", "}}", StringComparison.Ordinal)
                If buffer.Contains(UnicodeOpenQuote, StringComparison.Ordinal) Then
                    buffer = buffer.ConverUnicodeQuotes(UnicodeOpenQuote)
                End If
                If buffer.Contains(UnicodeCloseQuote, StringComparison.Ordinal) Then
                    buffer = buffer.ConverUnicodeQuotes(UnicodeCloseQuote)
                End If
                If buffer.Contains(UnicodeFullWidthQuoationMark, StringComparison.Ordinal) Then
                    buffer = buffer.ConverUnicodeQuotes(UnicodeFullWidthQuoationMark)
                End If

                TokenString = buffer
            Catch ex As OperationCanceledException
                Throw
            Catch ex As Exception
                Stop
                Throw
            End Try
            Return TokenString
        End Function

        <Extension>
        Friend Function ConverUnicodeQuotes(TokenString As String, UnicodeQuote As String) As String
            TokenString = TokenString.Replace(UnicodeQuote & UnicodeQuote, ChrW(0), StringComparison.Ordinal)
            TokenString = TokenString.Replace(UnicodeQuote, UnicodeQuote & UnicodeQuote, StringComparison.Ordinal)
            Return TokenString.Replace(ChrW(0), UnicodeQuote & UnicodeQuote, StringComparison.Ordinal)
        End Function

        Friend Function GetLiteralExpression(value As Object, Token As SyntaxToken, _NodesVisitor As NodesVisitor) As VBS.ExpressionSyntax
            Select Case Token.RawKind
                Case CS.SyntaxKind.NumericLiteralToken
                    Dim tokenToString As String = Token.ToString
                    If tokenToString.StartsWith("0x", StringComparison.OrdinalIgnoreCase) Then
                        Dim hexValueString As String = $"&H{tokenToString.Substring(startIndex:=2)}".Replace("ul", "", StringComparison.OrdinalIgnoreCase).Replace("u", "", StringComparison.OrdinalIgnoreCase).Replace("l", "", StringComparison.OrdinalIgnoreCase)
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

                    Dim valueText As String = TranslateTypeCharacter(Token)
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
                    If TypeOf value Is String Then
                        Dim strValue As String = DirectCast(value, String)
                        If strValue.Contains("\", StringComparison.Ordinal) Then
                            strValue = ConvertCSharpEscapes(strValue)
                        End If
                        If strValue.Contains(UnicodeOpenQuote, StringComparison.Ordinal) Then
                            strValue = strValue.ConverUnicodeQuotes(UnicodeOpenQuote)
                        End If
                        If strValue.Contains(UnicodeCloseQuote, StringComparison.Ordinal) Then
                            strValue = strValue.ConverUnicodeQuotes(UnicodeCloseQuote)
                        End If
                        If strValue.Contains(UnicodeFullWidthQuoationMark, StringComparison.Ordinal) Then
                            strValue = strValue.ConverUnicodeQuotes(UnicodeFullWidthQuoationMark)
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
                        Return Factory.literalExpression(VB.SyntaxKind.CharacterLiteralExpression, Factory.Literal($"{UnicodeOpenQuote}{UnicodeOpenQuote}"))
                    End If
                    If AscW(CChar(value)) = &H201D Then
                        Return Factory.literalExpression(VB.SyntaxKind.CharacterLiteralExpression, Factory.Literal($"{UnicodeCloseQuote}{UnicodeCloseQuote}"))
                    End If
                    If Token.Text.StartsWith("'\u", StringComparison.OrdinalIgnoreCase) Then
                        Return Factory.ParseExpression($"ChrW(&H{Token.Text.RemoveAll("'").Substring(startIndex:=2)})")
                    End If
                    Return Factory.literalExpression(VB.SyntaxKind.CharacterLiteralExpression, Factory.Literal(CChar(value)))
                Case CS.SyntaxKind.DefaultKeyword
                    Dim methodStatement As CSS.MethodDeclarationSyntax = Token.Parent.GetAncestor(Of CSS.MethodDeclarationSyntax)
                    Dim returnType As VBS.TypeSyntax
                    If Token.Parent.GetAncestor(Of CSS.ReturnStatementSyntax) IsNot Nothing OrElse
                        Token.Parent.GetAncestor(Of CSS.ArrowExpressionClauseSyntax) IsNot Nothing Then
                        Dim propertyDeclaration As CSS.PropertyDeclarationSyntax = Token.Parent.GetAncestor(Of CSS.PropertyDeclarationSyntax)
                        If propertyDeclaration IsNot Nothing Then
                            returnType = DirectCast(propertyDeclaration.Type.Accept(_NodesVisitor), VBS.TypeSyntax)
                            Return Factory.CTypeExpression(NothingExpression, returnType)
                        End If
                        If methodStatement IsNot Nothing Then
                            returnType = DirectCast(methodStatement.ReturnType.Accept(_NodesVisitor), VBS.TypeSyntax)
                            Return Factory.CTypeExpression(NothingExpression, returnType)
                        End If
                        Dim operatorStatement As CSS.ConversionOperatorDeclarationSyntax = Token.Parent.GetAncestor(Of CSS.ConversionOperatorDeclarationSyntax)
                        If operatorStatement IsNot Nothing Then
                            returnType = DirectCast(operatorStatement.Type.Accept(_NodesVisitor), VBS.TypeSyntax)
                            Return Factory.CTypeExpression(NothingExpression, returnType)
                        End If
                        Throw UnreachableException
                    End If
                    Dim equalsValue As CSS.EqualsValueClauseSyntax = Token.Parent.GetAncestor(Of CSS.EqualsValueClauseSyntax)
                    If equalsValue IsNot Nothing Then
                        Dim parameter As CSS.ParameterSyntax = Token.Parent.GetAncestor(Of CSS.ParameterSyntax)
                        If parameter IsNot Nothing Then
                            returnType = DirectCast(parameter.Type.Accept(_NodesVisitor), VBS.TypeSyntax)
                            Return Factory.CTypeExpression(NothingExpression, returnType)
                        End If
                        Throw UnreachableException
                    End If
                    Dim assignmentExpression As CSS.AssignmentExpressionSyntax = Token.Parent.GetAncestor(Of CSS.AssignmentExpressionSyntax)
                    If assignmentExpression IsNot Nothing Then
                        If methodStatement IsNot Nothing Then
                            If TypeOf assignmentExpression.Left Is CSS.ThisExpressionSyntax Then
                                Dim classAncestor As CSS.ClassDeclarationSyntax = Token.Parent.GetAncestor(Of CSS.ClassDeclarationSyntax)
                                If classAncestor IsNot Nothing Then
                                    Return Factory.CTypeExpression(NothingExpression, Factory.ParseTypeName(classAncestor.Identifier.ValueText))
                                End If
                                Dim structAncestor As CSS.StructDeclarationSyntax = Token.Parent.GetAncestor(Of CSS.StructDeclarationSyntax)
                                If structAncestor IsNot Nothing Then
                                    Return Factory.CTypeExpression(NothingExpression, Factory.ParseTypeName(structAncestor.Identifier.ValueText))
                                End If
                                Stop
                                Return NothingExpression
                            End If

                            If assignmentExpression.Left.RawKind = CS.SyntaxKind.PointerIndirectionExpression OrElse
                                TypeOf assignmentExpression.Left Is CSS.DeclarationExpressionSyntax Then
                                Return NothingExpression
                            End If

                            If TypeOf assignmentExpression.Left Is CSS.TupleExpressionSyntax Then
                                Dim tuple As CSS.TupleExpressionSyntax = DirectCast(assignmentExpression.Left, CSS.TupleExpressionSyntax)
                                Dim parent As SyntaxNode = assignmentExpression.Parent
                                ' This could be better if I could figure out the Type
                                Return NothingExpression
                            End If

                            Dim idString As String = ""
                            If TypeOf assignmentExpression.Left Is CSS.IdentifierNameSyntax Then
                                idString = DirectCast(assignmentExpression.Left, CSS.IdentifierNameSyntax).Identifier.ValueText
                            ElseIf TypeOf assignmentExpression.Left Is CSS.MemberAccessExpressionSyntax Then
                                idString = DirectCast(assignmentExpression.Left, CSS.MemberAccessExpressionSyntax).Name.ToString
                            ElseIf TypeOf assignmentExpression.Left Is CSS.ElementAccessExpressionSyntax Then
                                idString = DirectCast(assignmentExpression.Left, CSS.ElementAccessExpressionSyntax).Expression.ToString
                            Else
                                Stop
                            End If
                            For Each p As CSS.ParameterSyntax In methodStatement.ParameterList.Parameters
                                If p.Identifier.ValueText = idString Then
                                    returnType = DirectCast(p.Type.Accept(_NodesVisitor), VBS.TypeSyntax)
                                    Return Factory.CTypeExpression(NothingExpression, returnType)
                                End If
                            Next
                            Return NothingExpression
                        End If
                    End If
                    If Token.Parent.GetAncestor(Of CSS.ConstructorDeclarationSyntax) IsNot Nothing Then
                        Return NothingExpression
                    End If
                    If Token.Parent.GetAncestor(Of CSS.ArgumentSyntax) IsNot Nothing Then
                        Return NothingExpression
                    End If
                    Return NothingExpression
                Case CS.SyntaxKind.ArgListKeyword
                    Return Factory.IdentifierName("__Arglist")
            End Select
            Stop
            Return NothingExpression
        End Function

    End Module
End Namespace
