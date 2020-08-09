' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices

Imports CSharpToVBCodeConverter.ToVisualBasic.CSharpConverter

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Public Module ConvertLiteralSupport

    Private Function Binary(Value As Byte) As String
        Return $"&B{Convert.ToString(Value, 2).PadLeft(8, "0"c)}"
    End Function

    Private Function Binary(Value As SByte) As String
        Return $"&B{Convert.ToString(Value, 2).PadLeft(8, "0"c)}"
    End Function

    Private Function Binary(Value As Short) As String
        Return $"&B{Convert.ToString(Value, 2).PadLeft(16, "0"c)}"
    End Function

    Private Function Binary(Value As UShort) As String
        Return $"&B{Convert.ToString(Value, 2).PadLeft(16, "0"c)}"
    End Function

    Private Function Binary(Value As Integer) As String
        Return $"&B{Convert.ToString(Value, 2).PadLeft(32, "0"c)}"
    End Function

    Private Function Binary(Value As UInteger) As String
        Return $"&B{Convert.ToString(Value, 2).PadLeft(32, "0"c)}UI"
    End Function

    Private Function Binary(Value As Long) As String
        Return $"&B{Convert.ToString(Value, 2).PadLeft(64, "0"c)}"
    End Function

    Friend Function ConvertCSharpEscapes(TokenString As String) As String
        Dim _Buffer As String
        Try
            'Dim unescape1 As String = RegexParser.Unescape(TokenString)
            _Buffer = TokenString.
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
            _Buffer = _Buffer.Replace("{", "{{", StringComparison.Ordinal).
                                Replace("}", "}}", StringComparison.Ordinal)
            If _Buffer.Contains(UnicodeOpenQuote, StringComparison.Ordinal) Then
                _Buffer = _Buffer.ConverUnicodeQuotes(UnicodeOpenQuote)
            End If
            If _Buffer.Contains(UnicodeCloseQuote, StringComparison.Ordinal) Then
                _Buffer = _Buffer.ConverUnicodeQuotes(UnicodeCloseQuote)
            End If
            If _Buffer.Contains(UnicodeFullWidthQuoationMark, StringComparison.Ordinal) Then
                _Buffer = _Buffer.ConverUnicodeQuotes(UnicodeFullWidthQuoationMark)
            End If

            TokenString = _Buffer
        Catch ex As OperationCanceledException
            Throw
        Catch ex As Exception
            Stop
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
                Dim TokenToString As String = Token.ToString
                If TokenToString.StartsWith("0x", StringComparison.OrdinalIgnoreCase) Then
                    Dim HEXValueString As String = $"&H{TokenToString.Substring(2)}".Replace("ul", "", StringComparison.OrdinalIgnoreCase).Replace("u", "", StringComparison.OrdinalIgnoreCase).Replace("l", "", StringComparison.OrdinalIgnoreCase)
                    If TypeOf value Is Integer Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal(HEXValueString, CInt(value)))
                    If TypeOf value Is SByte Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal(HEXValueString, CSByte(value)))
                    If TypeOf value Is Short Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal(HEXValueString, CShort(value)))
                    If TypeOf value Is UShort Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal(HEXValueString, CUShort(value)))
                    If TypeOf value Is UInteger Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal(HEXValueString & "UI", CUInt(value)))
                    If TypeOf value Is Long Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal(HEXValueString, CLng(value)))
                    If TypeOf value Is ULong Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal(HEXValueString & "UL", CULng(value)))
                    Throw UnreachableException
                End If
                If TokenToString.StartsWith("0b", StringComparison.OrdinalIgnoreCase) Then
                    If TypeOf value Is Integer Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal($"{Binary(CInt(value))}", CInt(value)))
                    If TypeOf value Is Byte Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal($"{Binary(CByte(value))}", CByte(value)))
                    If TypeOf value Is SByte Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal($"{Binary(CSByte(value))}", CSByte(value)))
                    If TypeOf value Is Short Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal($"{Binary(CShort(value))}", CShort(value)))
                    If TypeOf value Is UShort Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal($"{Binary(CUShort(value))}", CUShort(value)))
                    If TypeOf value Is UInteger Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal($"{Binary(CUInt(value))}", CUInt(value)))
                    If TypeOf value Is Long Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal($"{Binary(CLng(value))}", CLng(value)))
                    If TypeOf value Is ULong Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal($"{Binary(CType(CULng(value), Long))}UL", CULng(value)))
                    Throw UnreachableException
                End If

                If TypeOf value Is Integer Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal(CInt(value)))
                If TypeOf value Is Byte Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal(CByte(value)))
                If TypeOf value Is SByte Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal(CSByte(value)))
                If TypeOf value Is Short Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal(CShort(value)))
                If TypeOf value Is UShort Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal(CUShort(value)))
                If TypeOf value Is UInteger Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal(CUInt(value)))
                If TypeOf value Is Long Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal(CLng(value)))
                If TypeOf value Is ULong Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal(CULng(value)))
                If TypeOf value Is Single Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal(CSng(value)))
                If TypeOf value Is Double Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal(CDbl(value)))
                If TypeOf value Is Decimal Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal(CDec(value)))
                Throw UnreachableException
            Case CS.SyntaxKind.StringLiteralToken
                If TypeOf value Is String Then
                    Dim StrValue As String = DirectCast(value, String)
                    If StrValue.Contains("\", StringComparison.Ordinal) Then
                        StrValue = ConvertCSharpEscapes(StrValue)
                    End If
                    If StrValue.Contains(UnicodeOpenQuote, StringComparison.Ordinal) Then
                        StrValue = StrValue.ConverUnicodeQuotes(UnicodeOpenQuote)
                    End If
                    If StrValue.Contains(UnicodeCloseQuote, StringComparison.Ordinal) Then
                        StrValue = StrValue.ConverUnicodeQuotes(UnicodeCloseQuote)
                    End If
                    If StrValue.Contains(UnicodeFullWidthQuoationMark, StringComparison.Ordinal) Then
                        StrValue = StrValue.ConverUnicodeQuotes(UnicodeFullWidthQuoationMark)
                    End If
                    Return VBFactory.LiteralExpression(VB.SyntaxKind.StringLiteralExpression, VBFactory.Literal(StrValue))
                End If
            Case CS.SyntaxKind.FalseKeyword
                Return VBFactory.FalseLiteralExpression(FalseKeyword)
            Case CS.SyntaxKind.NullKeyword
                Return NothingExpression
            Case CS.SyntaxKind.TrueKeyword
                Return VBFactory.TrueLiteralExpression(TrueKeyword)
            Case CS.SyntaxKind.CharacterLiteralToken
                If AscW(CChar(value)) = &H201C Then
                    Return VBFactory.LiteralExpression(VB.SyntaxKind.CharacterLiteralExpression, VBFactory.Literal($"{UnicodeOpenQuote}{UnicodeOpenQuote}"))
                End If
                If AscW(CChar(value)) = &H201D Then
                    Return VBFactory.LiteralExpression(VB.SyntaxKind.CharacterLiteralExpression, VBFactory.Literal($"{UnicodeCloseQuote}{UnicodeCloseQuote}"))
                End If
                If Token.Text.StartsWith("'\u", StringComparison.OrdinalIgnoreCase) Then
                    Return VBFactory.ParseExpression($"ChrW(&H{Token.Text.Replace("'", "", StringComparison.Ordinal).Substring(2)})")
                End If
                Return VBFactory.LiteralExpression(VB.SyntaxKind.CharacterLiteralExpression, VBFactory.Literal(CChar(value)))
            Case CS.SyntaxKind.DefaultKeyword
                Dim ReturnType As VBS.TypeSyntax
                Dim MethodStatement As CSS.MethodDeclarationSyntax = Token.Parent.GetAncestor(Of CSS.MethodDeclarationSyntax)
                If Token.Parent.GetAncestor(Of CSS.ReturnStatementSyntax) IsNot Nothing OrElse
                        Token.Parent.GetAncestor(Of CSS.ArrowExpressionClauseSyntax) IsNot Nothing Then
                    Dim PropertyDeclaration As CSS.PropertyDeclarationSyntax = Token.Parent.GetAncestor(Of CSS.PropertyDeclarationSyntax)
                    If PropertyDeclaration IsNot Nothing Then
                        ReturnType = DirectCast(PropertyDeclaration.Type.Accept(_NodesVisitor), VBS.TypeSyntax)
                        Return VBFactory.CTypeExpression(NothingExpression, ReturnType)
                    End If
                    If MethodStatement IsNot Nothing Then
                        ReturnType = DirectCast(MethodStatement.ReturnType.Accept(_NodesVisitor), VBS.TypeSyntax)
                        Return VBFactory.CTypeExpression(NothingExpression, ReturnType)
                    End If
                    Dim OperatorStatement As CSS.ConversionOperatorDeclarationSyntax = Token.Parent.GetAncestor(Of CSS.ConversionOperatorDeclarationSyntax)
                    If OperatorStatement IsNot Nothing Then
                        ReturnType = DirectCast(OperatorStatement.Type.Accept(_NodesVisitor), VBS.TypeSyntax)
                        Return VBFactory.CTypeExpression(NothingExpression, ReturnType)
                    End If
                    Throw UnreachableException
                End If
                Dim EqualsValue As CSS.EqualsValueClauseSyntax = Token.Parent.GetAncestor(Of CSS.EqualsValueClauseSyntax)
                If EqualsValue IsNot Nothing Then
                    Dim Parameter As CSS.ParameterSyntax = Token.Parent.GetAncestor(Of CSS.ParameterSyntax)
                    If Parameter IsNot Nothing Then
                        ReturnType = DirectCast(Parameter.Type.Accept(_NodesVisitor), VBS.TypeSyntax)
                        Return VBFactory.CTypeExpression(NothingExpression, ReturnType)
                    End If
                    Throw UnreachableException
                End If
                Dim AssignmentExpression As CSS.AssignmentExpressionSyntax = Token.Parent.GetAncestor(Of CSS.AssignmentExpressionSyntax)
                If AssignmentExpression IsNot Nothing Then
                    If MethodStatement IsNot Nothing Then
                        If TypeOf AssignmentExpression.Left Is CSS.ThisExpressionSyntax Then
                            Dim ClassAncestor As CSS.ClassDeclarationSyntax = Token.Parent.GetAncestor(Of CSS.ClassDeclarationSyntax)
                            If ClassAncestor IsNot Nothing Then
                                Return VBFactory.CTypeExpression(NothingExpression, VBFactory.ParseTypeName(ClassAncestor.Identifier.ValueText))
                            End If
                            Dim StructAncestor As CSS.StructDeclarationSyntax = Token.Parent.GetAncestor(Of CSS.StructDeclarationSyntax)
                            If StructAncestor IsNot Nothing Then
                                Return VBFactory.CTypeExpression(NothingExpression, VBFactory.ParseTypeName(StructAncestor.Identifier.ValueText))
                            End If
                            Stop
                            Return NothingExpression
                        End If

                        If AssignmentExpression.Left.RawKind = CS.SyntaxKind.PointerIndirectionExpression OrElse
                                TypeOf AssignmentExpression.Left Is CSS.DeclarationExpressionSyntax Then
                            Return NothingExpression
                        End If

                        If TypeOf AssignmentExpression.Left Is CSS.TupleExpressionSyntax Then
                            Dim Tuple As CSS.TupleExpressionSyntax = DirectCast(AssignmentExpression.Left, CSS.TupleExpressionSyntax)
                            Dim Parent As SyntaxNode = AssignmentExpression.Parent
                            ' This could be better if I could figure out the Type
                            Return NothingExpression
                        End If

                        Dim IDString As String = ""
                        If TypeOf AssignmentExpression.Left Is CSS.IdentifierNameSyntax Then
                            IDString = DirectCast(AssignmentExpression.Left, CSS.IdentifierNameSyntax).Identifier.ValueText
                        ElseIf TypeOf AssignmentExpression.Left Is CSS.MemberAccessExpressionSyntax Then
                            IDString = DirectCast(AssignmentExpression.Left, CSS.MemberAccessExpressionSyntax).Name.ToString
                        ElseIf TypeOf AssignmentExpression.Left Is CSS.ElementAccessExpressionSyntax Then
                            IDString = DirectCast(AssignmentExpression.Left, CSS.ElementAccessExpressionSyntax).Expression.ToString
                        Else
                            Stop
                        End If
                        For Each P As CSS.ParameterSyntax In MethodStatement.ParameterList.Parameters
                            If P.Identifier.ValueText = IDString Then
                                ReturnType = DirectCast(P.Type.Accept(_NodesVisitor), VBS.TypeSyntax)
                                Return VBFactory.CTypeExpression(NothingExpression, ReturnType)
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
                Return VBFactory.IdentifierName("__Arglist")
        End Select
        Stop
        Return NothingExpression
    End Function

End Module
