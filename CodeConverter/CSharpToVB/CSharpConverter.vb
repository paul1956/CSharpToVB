Option Explicit On
Option Infer Off
Option Strict On

Imports IVisualBasicCode.CodeConverter.Util

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace IVisualBasicCode.CodeConverter.Visual_Basic

    Partial Public Class CSharpConverter

        Private Shared Function Binary(Value As Byte) As String
            Return $"&B{System.Convert.ToString(Value, 2).PadLeft(8, "0"c)}"
        End Function

        Private Shared Function Binary(Value As SByte) As String
            Return $"&B{System.Convert.ToString(Value, 2).PadLeft(8, "0"c)}"
        End Function

        Private Shared Function Binary(Value As Short) As String
            Return $"&B{System.Convert.ToString(Value, 2).PadLeft(16, "0"c)}"
        End Function

        Private Shared Function Binary(Value As UShort) As String
            Return $"&B{System.Convert.ToString(Value, 2).PadLeft(16, "0"c)}"
        End Function

        Private Shared Function Binary(Value As Integer) As String
            Return $"&B{System.Convert.ToString(Value, 2).PadLeft(32, "0"c)}"
        End Function

        Private Shared Function Binary(Value As UInteger) As String
            Return $"&B{System.Convert.ToString(Value, 2).PadLeft(32, "0"c)}UI"
        End Function

        Private Shared Function Binary(Value As Long) As String
            Return $"&B{System.Convert.ToString(Value, 2).PadLeft(64, "0"c)}"
        End Function

        ''' <summary>
        ''' Returns Safe VB Name
        ''' </summary>
        ''' <param name="id">Original Name</param>
        ''' <param name="IsQualifiedName">True if name is part of a Qualified Name and should not be renamed</param>
        ''' <param name="IsTypeName"></param>
        ''' <returns></returns>
        Private Shared Function GenerateSafeVBToken(id As SyntaxToken, IsQualifiedName As Boolean, Optional IsTypeName As Boolean = False) As SyntaxToken
            If id.Language <> "C#" Then
                Stop
            End If
            If id.HasTrailingTrivia AndAlso id.TrailingTrivia(0).Language <> "C#" Then
                Stop
            End If
            If id.HasLeadingTrivia AndAlso id.LeadingTrivia(0).Language <> "C#" Then
                Stop
            End If
            Dim keywordKind As VB.SyntaxKind = VB.SyntaxFacts.GetKeywordKind(id.ValueText)
            If VB.SyntaxFacts.IsKeywordKind(keywordKind) Then
                Return MakeIdentifierUnique(id, BracketNeeded:=True, QualifiedNameOrTypeName:=IsQualifiedName)
            End If
            If Not IsTypeName Then
                If VB.SyntaxFacts.IsPredefinedType(keywordKind) Then
                    Return MakeIdentifierUnique(id, BracketNeeded:=True, QualifiedNameOrTypeName:=IsQualifiedName)
                End If
            End If

            If id.Parent?.IsParentKind(CS.SyntaxKind.Parameter) Then
                Dim Param As CSS.ParameterSyntax = DirectCast(id.Parent.Parent, CSS.ParameterSyntax)
                Dim MethodDeclaration As CSS.MethodDeclarationSyntax = TryCast(Param.Parent?.Parent, CSS.MethodDeclarationSyntax)
                IsQualifiedName = If(MethodDeclaration IsNot Nothing AndAlso String.Compare(MethodDeclaration.Identifier.ValueText, id.ValueText, ignoreCase:=True) <> 0, False, True)
                IsQualifiedName = IsQualifiedName Or String.Compare(Param.Type.ToString, id.ValueText, ignoreCase:=False) = 0
            End If

            'If id.IsParentKind(CS.SyntaxKind.Parameter) Then
            '    Dim MethodDeclaration As CSS.MethodDeclarationSyntax = TryCast(id.Parent.Parent?.Parent, CSS.MethodDeclarationSyntax)
            '    IsQualifiedName = If(MethodDeclaration IsNot Nothing AndAlso String.Compare(MethodDeclaration.Identifier.ValueText, id.ValueText, ignoreCase:=True) = 0, False, True)
            'End If
            Return MakeIdentifierUnique(id, BracketNeeded:=False, QualifiedNameOrTypeName:=IsQualifiedName)
        End Function

        Private Shared Function GetLiteralExpression(value As Object, Token As SyntaxToken, _NodesVisitor As NodesVisitor) As VBS.ExpressionSyntax
            Select Case Token.RawKind
                Case CS.SyntaxKind.NumericLiteralToken
                    Dim TokenToString As String = Token.ToString
                    If TokenToString.StartsWith("0x") Then
                        Dim HEXValueString As String = $"&H{TokenToString.Substring(2)}".Replace("ul", "", StringComparison.OrdinalIgnoreCase).Replace("u", "", StringComparison.OrdinalIgnoreCase).Replace("l", "", StringComparison.OrdinalIgnoreCase)
                        If TypeOf value Is Integer Then Return VB.SyntaxFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VB.SyntaxFactory.Literal(HEXValueString, CInt(value)))
                        If TypeOf value Is SByte Then Return VB.SyntaxFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VB.SyntaxFactory.Literal(HEXValueString, CSByte(value)))
                        If TypeOf value Is Short Then Return VB.SyntaxFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VB.SyntaxFactory.Literal(HEXValueString, CShort(value)))
                        If TypeOf value Is UShort Then Return VB.SyntaxFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VB.SyntaxFactory.Literal(HEXValueString, CUShort(value)))
                        If TypeOf value Is UInteger Then Return VB.SyntaxFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VB.SyntaxFactory.Literal(HEXValueString & "UI", CUInt(value)))
                        If TypeOf value Is Long Then Return VB.SyntaxFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VB.SyntaxFactory.Literal(HEXValueString, CLng(value)))
                        If TypeOf value Is ULong Then Return VB.SyntaxFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VB.SyntaxFactory.Literal(HEXValueString & "UL", CULng(value)))
                    ElseIf TokenToString.StartsWith("0b") Then
                        If TypeOf value Is Integer Then Return VB.SyntaxFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VB.SyntaxFactory.Literal(($"{Binary(CInt(value))}"), CInt(value)))
                        If TypeOf value Is Byte Then Return VB.SyntaxFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VB.SyntaxFactory.Literal(($"{Binary(CByte(value))}"), CByte(value)))
                        If TypeOf value Is SByte Then Return VB.SyntaxFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VB.SyntaxFactory.Literal($"{Binary(CSByte(value))}", CSByte(value)))
                        If TypeOf value Is Short Then Return VB.SyntaxFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VB.SyntaxFactory.Literal($"{Binary(CShort(value))}", CShort(value)))
                        If TypeOf value Is UShort Then Return VB.SyntaxFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VB.SyntaxFactory.Literal($"{Binary(CUShort(value))}", CUShort(value)))
                        If TypeOf value Is UInteger Then Return VB.SyntaxFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VB.SyntaxFactory.Literal($"{Binary(CUInt(value))}", CUInt(value)))
                        If TypeOf value Is Long Then Return VB.SyntaxFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VB.SyntaxFactory.Literal($"{Binary(CLng(value))}", CLng(value)))
                        If TypeOf value Is ULong Then Return VB.SyntaxFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VB.SyntaxFactory.Literal($"{Binary(CType(CULng(value), Long))}UL", CULng(value)))
                    End If

                    If TypeOf value Is Integer Then Return VB.SyntaxFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VB.SyntaxFactory.Literal(CInt(value)))
                    If TypeOf value Is Byte Then Return VB.SyntaxFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VB.SyntaxFactory.Literal(CByte(value)))
                    If TypeOf value Is SByte Then Return VB.SyntaxFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VB.SyntaxFactory.Literal(CSByte(value)))
                    If TypeOf value Is Short Then Return VB.SyntaxFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VB.SyntaxFactory.Literal(CShort(value)))
                    If TypeOf value Is UShort Then Return VB.SyntaxFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VB.SyntaxFactory.Literal(CUShort(value)))
                    If TypeOf value Is UInteger Then Return VB.SyntaxFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VB.SyntaxFactory.Literal(CUInt(value)))
                    If TypeOf value Is Long Then Return VB.SyntaxFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VB.SyntaxFactory.Literal(CLng(value)))
                    If TypeOf value Is ULong Then Return VB.SyntaxFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VB.SyntaxFactory.Literal(CULng(value)))
                    If TypeOf value Is Single Then Return VB.SyntaxFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VB.SyntaxFactory.Literal(CSng(value)))
                    If TypeOf value Is Double Then Return VB.SyntaxFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VB.SyntaxFactory.Literal(CDbl(value)))
                    If TypeOf value Is Decimal Then Return VB.SyntaxFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VB.SyntaxFactory.Literal(CDec(value)))
                Case CS.SyntaxKind.StringLiteralToken
                    If TypeOf value Is String Then
                        Dim StrValue As String = DirectCast(value, String)
                        If StrValue.Contains("\") Then
                            StrValue = ConvertCSharpEscapes(StrValue)
                        End If
                        If StrValue.Contains(UnicodeOpenQuote) Then
                            StrValue = StrValue.ConverUnicodeQuotes(UnicodeOpenQuote)
                        End If
                        If StrValue.Contains(UnicodeCloseQuote) Then
                            StrValue = StrValue.ConverUnicodeQuotes(UnicodeCloseQuote)
                        End If
                        If StrValue.Contains(UnicodeFullWidthQuoationMark) Then
                            StrValue = StrValue.ConverUnicodeQuotes(UnicodeFullWidthQuoationMark)
                        End If
                        Return VB.SyntaxFactory.LiteralExpression(VB.SyntaxKind.StringLiteralExpression, VB.SyntaxFactory.Literal(StrValue))
                    End If
                Case CS.SyntaxKind.FalseKeyword
                    Return VB.SyntaxFactory.FalseLiteralExpression(FalseKeyword)
                Case CS.SyntaxKind.NullKeyword
                    Return NothingExpression
                Case CS.SyntaxKind.TrueKeyword
                    Return VB.SyntaxFactory.TrueLiteralExpression(TrueKeyword)
                Case CS.SyntaxKind.CharacterLiteralToken
                    If AscW(CChar(value)) = &H201C Then
                        Return VB.SyntaxFactory.LiteralExpression(VB.SyntaxKind.CharacterLiteralExpression, VB.SyntaxFactory.Literal($"{UnicodeOpenQuote}{UnicodeOpenQuote}"))
                    End If
                    If AscW(CChar(value)) = &H201D Then
                        Return VB.SyntaxFactory.LiteralExpression(VB.SyntaxKind.CharacterLiteralExpression, VB.SyntaxFactory.Literal($"{UnicodeCloseQuote}{UnicodeCloseQuote}"))
                    End If
                    If Token.Text.StartsWith("'\u") Then
                        Return VB.SyntaxFactory.ParseExpression($"ChrW(&H{Token.Text.Replace("'", "").Substring(2)})")
                    End If
                    Return VB.SyntaxFactory.LiteralExpression(VB.SyntaxKind.CharacterLiteralExpression, VB.SyntaxFactory.Literal(CChar(value)))
                Case CS.SyntaxKind.DefaultKeyword
                    Dim ReturnType As VBS.TypeSyntax
                    Dim MethodStatement As CSS.MethodDeclarationSyntax = Token.Parent.GetAncestor(Of CSS.MethodDeclarationSyntax)
                    If Token.Parent.GetAncestor(Of CSS.ReturnStatementSyntax) IsNot Nothing OrElse
                        Token.Parent.GetAncestor(Of CSS.ArrowExpressionClauseSyntax) IsNot Nothing Then
                        Dim PropertyDeclaration As CSS.PropertyDeclarationSyntax = Token.Parent.GetAncestor(Of CSS.PropertyDeclarationSyntax)
                        If PropertyDeclaration IsNot Nothing Then
                            ReturnType = DirectCast(PropertyDeclaration.Type.Accept(_NodesVisitor), VBS.TypeSyntax)
                            Return VB.SyntaxFactory.CTypeExpression(NothingExpression, ReturnType)
                        End If
                        If MethodStatement IsNot Nothing Then
                            ReturnType = DirectCast(MethodStatement.ReturnType.Accept(_NodesVisitor), VBS.TypeSyntax)
                            Return VB.SyntaxFactory.CTypeExpression(NothingExpression, ReturnType)
                        End If
                        Dim OperatorStatement As CSS.ConversionOperatorDeclarationSyntax = Token.Parent.GetAncestor(Of CSS.ConversionOperatorDeclarationSyntax)
                        If OperatorStatement IsNot Nothing Then
                            ReturnType = DirectCast(OperatorStatement.Type.Accept(_NodesVisitor), VBS.TypeSyntax)
                            Return VB.SyntaxFactory.CTypeExpression(NothingExpression, ReturnType)
                        End If
                    End If
                    Dim EqualsValue As CSS.EqualsValueClauseSyntax = Token.Parent.GetAncestor(Of CSS.EqualsValueClauseSyntax)
                    If EqualsValue IsNot Nothing Then
                        Dim Parameter As CSS.ParameterSyntax = Token.Parent.GetAncestor(Of CSS.ParameterSyntax)
                        If Parameter IsNot Nothing Then
                            ReturnType = DirectCast(Parameter.Type.Accept(_NodesVisitor), VBS.TypeSyntax)
                            Return VB.SyntaxFactory.CTypeExpression(NothingExpression, ReturnType)
                        End If
                    End If
                    Dim AssignmentExpression As CSS.AssignmentExpressionSyntax = Token.Parent.GetAncestor(Of CSS.AssignmentExpressionSyntax)
                    If AssignmentExpression IsNot Nothing Then
                        If MethodStatement IsNot Nothing Then
                            If TypeOf AssignmentExpression.Left Is CSS.ThisExpressionSyntax Then
                                Dim ClassAncestor As CSS.ClassDeclarationSyntax = Token.Parent.GetAncestor(Of CSS.ClassDeclarationSyntax)
                                If ClassAncestor IsNot Nothing Then
                                    Return VB.SyntaxFactory.CTypeExpression(NothingExpression, VB.SyntaxFactory.ParseTypeName(ClassAncestor.Identifier.ValueText))
                                End If
                                Dim StructAncestor As CSS.StructDeclarationSyntax = Token.Parent.GetAncestor(Of CSS.StructDeclarationSyntax)
                                If StructAncestor IsNot Nothing Then
                                    Return VB.SyntaxFactory.CTypeExpression(NothingExpression, VB.SyntaxFactory.ParseTypeName(StructAncestor.Identifier.ValueText))
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
                                    Return VB.SyntaxFactory.CTypeExpression(NothingExpression, ReturnType)
                                End If
                            Next
                            Return NothingExpression
                        End If
                    End If
                    If Token.Parent.GetAncestor(Of CSS.ConstructorDeclarationSyntax) IsNot Nothing Then
                        Return NothingExpression
                    End If
                    Return NothingExpression
                    If Token.Parent.GetAncestor(Of CSS.ArgumentSyntax) IsNot Nothing Then
                        Return NothingExpression
                    End If
                Case CS.SyntaxKind.ArgListKeyword
                    Return VB.SyntaxFactory.IdentifierName("__Arglist")
            End Select
            Stop
            Return NothingExpression
        End Function

        Private Shared Function MakeIdentifierUnique(id As SyntaxToken, BracketNeeded As Boolean, QualifiedNameOrTypeName As Boolean) As SyntaxToken
            Dim ConvertedIdentifier As String = If(BracketNeeded, $"[{id.ValueText}]", id.ValueText)
            If ConvertedIdentifier = "_" Then
                ConvertedIdentifier = "underscore"
            End If
            ' Don't Change Qualified Names
            If QualifiedNameOrTypeName Then
                If Not UsedIdentifiers.ContainsKey(ConvertedIdentifier) Then
                    UsedIdentifiers.Add(ConvertedIdentifier, New SymbolTableEntry(ConvertedIdentifier, True))
                End If
                Return VB.SyntaxFactory.Identifier(UsedIdentifiers(ConvertedIdentifier).Name).WithConvertedTriviaFrom(id)
            End If
            If UsedIdentifiers.ContainsKey(ConvertedIdentifier) Then
                ' We have a case sensitive exact match so just return it
                Return VB.SyntaxFactory.Identifier(UsedIdentifiers(ConvertedIdentifier).Name).WithConvertedTriviaFrom(id)
            End If
            For Each ident As KeyValuePair(Of String, SymbolTableEntry) In UsedIdentifiers
                If String.Compare(ident.Key, ConvertedIdentifier, ignoreCase:=False) = 0 Then
                    ' We have an exact match keep looking
                    Continue For
                End If
                If String.Compare(ident.Key, ConvertedIdentifier, ignoreCase:=True) = 0 Then
                    ' If we are here we have seen the variable in a different case so fix it
                    If UsedIdentifiers(ident.Key).IsType Then
                        UsedIdentifiers.Add(ConvertedIdentifier, New SymbolTableEntry(_Name:=ConvertedIdentifier, _IsType:=False))
                    Else
                        Dim NewUniqueName As String = If(ConvertedIdentifier.StartsWith("["), ConvertedIdentifier.Replace("[", "").Replace("]", "_Renamed"), $"{ConvertedIdentifier}_Renamed")
                        UsedIdentifiers.Add(ConvertedIdentifier, New SymbolTableEntry(_Name:=NewUniqueName, _IsType:=QualifiedNameOrTypeName))
                    End If
                    Return VB.SyntaxFactory.Identifier(UsedIdentifiers(ConvertedIdentifier).Name).WithConvertedTriviaFrom(id)
                End If
            Next
            UsedIdentifiers.Add(ConvertedIdentifier, New SymbolTableEntry(ConvertedIdentifier, QualifiedNameOrTypeName))
            Return VB.SyntaxFactory.Identifier(ConvertedIdentifier)
        End Function

        ''' <summary>
        ''' CHECK!!!!!!!!
        ''' </summary>
        ''' <param name="node"></param>
        ''' <returns></returns>
        ''' <remarks>Fix handling of AddressOf where is mistakenly added to DirectCast and C# Pointers</remarks>
        Private Shared Function RemodelVariableDeclaration(VariableDeclaration As CSS.VariableDeclarationSyntax, _NodesVisitor As NodesVisitor, IsFieldDeclaration As Boolean, ByRef LeadingTrivia As List(Of SyntaxTrivia)) As SeparatedSyntaxList(Of VBS.VariableDeclaratorSyntax)
            Dim type As VBS.TypeSyntax
            Dim DeclarationType As VB.VisualBasicSyntaxNode = VariableDeclaration.Type.Accept(_NodesVisitor)
            Dim TypeOrAddressOf As VB.VisualBasicSyntaxNode = DeclarationType.WithConvertedLeadingTriviaFrom(VariableDeclaration.Type)
            Dim TypeLeadingTrivia As SyntaxTriviaList = TypeOrAddressOf.GetLeadingTrivia

            If TypeLeadingTrivia.Count > 0 Then
                If TypeLeadingTrivia.Last.RawKind = VB.SyntaxKind.WhitespaceTrivia Then
                    TypeOrAddressOf = TypeOrAddressOf.WithLeadingTrivia(TypeLeadingTrivia.Last)
                Else
                    TypeOrAddressOf = TypeOrAddressOf.WithLeadingTrivia(SpaceTrivia)
                End If
            End If
            If IsFieldDeclaration AndAlso TypeLeadingTrivia.Count > 1 Then
                Dim StatementWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(VariableDeclaration)
                If TypeLeadingTrivia.ContainsCommentOrDirectiveTrivia AndAlso Not TriviaIsIdentical(TypeLeadingTrivia, ConvertTrivia(StatementWithIssues.GetLeadingTrivia).ToList) Then
                    StatementWithIssues.AddMarker(VB.SyntaxFactory.EmptyStatement.WithLeadingTrivia(TypeLeadingTrivia), StatementHandlingOption.AppendEmptyStatement, AllowDuplicates:=True)
                End If
            End If
            Dim CS_CollectedCommentTrivia As New List(Of SyntaxTrivia)
            If TypeOrAddressOf.IsKind(VB.SyntaxKind.AddressOfExpression) Then
                type = VB.SyntaxFactory.ParseTypeName(DirectCast(TypeOrAddressOf, VBS.UnaryExpressionSyntax).Operand.ToString)
                CS_CollectedCommentTrivia.Add(CS.SyntaxFactory.Comment(" TODO TASK: VB has no direct equivalent to C# Pointer Variables"))
            Else
                type = DirectCast(TypeOrAddressOf, VBS.TypeSyntax)
            End If
            type = CType(type.WithModifiedNodeTrivia(SeparatorFollows:=True), VBS.TypeSyntax)
            Dim declaratorsWithoutInitializers As New List(Of CSS.VariableDeclaratorSyntax)()
            Dim declarators As New List(Of VBS.VariableDeclaratorSyntax)
            For i As Integer = 0 To VariableDeclaration.Variables.Count - 1
                Dim v As CSS.VariableDeclaratorSyntax = VariableDeclaration.Variables(i)
                If v.Initializer Is Nothing Then
                    declaratorsWithoutInitializers.Add(v.WithAppendedTrailingTrivia(CS_CollectedCommentTrivia))
                    CS_CollectedCommentTrivia.Clear()
                    Continue For
                Else
                    Dim AsClause As VBS.SimpleAsClauseSyntax = If(VariableDeclaration.Type.IsVar OrElse VariableDeclaration.Type.IsKind(CS.SyntaxKind.RefType), Nothing, VB.SyntaxFactory.SimpleAsClause(type))
                    Dim Value As VBS.ExpressionSyntax = DirectCast(v.Initializer.Value.Accept(_NodesVisitor), VBS.ExpressionSyntax)
                    If Value Is Nothing Then
                        Value = VB.SyntaxFactory.IdentifierName("HandleRefExpression").WithConvertedTriviaFrom(v.Initializer.Value)
                    End If
                    If Value.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        LeadingTrivia.AddRange(Value.GetLeadingTrivia)
                    End If
                    Dim Initializer As VBS.EqualsValueSyntax = VB.SyntaxFactory.EqualsValue(Value.WithLeadingTrivia(SpaceTrivia))
                    ' Get the names last to lead with var jsonWriter = new JsonWriter(stringWriter)
                    ' Which should be Dim jsonWriter_Renamed = new JsonWriter(stringWriter)
                    Dim Names As SeparatedSyntaxList(Of VBS.ModifiedIdentifierSyntax) = VB.SyntaxFactory.SingletonSeparatedList(DirectCast(v.Accept(_NodesVisitor), VBS.ModifiedIdentifierSyntax))
                    Dim Declator As VBS.VariableDeclaratorSyntax = VB.SyntaxFactory.VariableDeclarator(
                                                                                                Names,
                                                                                                AsClause,
                                                                                                Initializer
                                                                                                )
                    If Declator.HasTrailingTrivia Then
                        Dim FoundEOL As Boolean = False
                        Dim NonCommentTrailingTrivia As New List(Of SyntaxTrivia)
                        For Each t As SyntaxTrivia In Declator.GetTrailingTrivia
                            Select Case t.RawKind
                                Case VB.SyntaxKind.EndOfLineTrivia
                                    FoundEOL = True
                                Case VB.SyntaxKind.CommentTrivia
                                    CS_CollectedCommentTrivia.Add(CS.SyntaxFactory.Comment(t.ToString.Replace("'", "//")))
                                Case VB.SyntaxKind.WhitespaceTrivia
                                    NonCommentTrailingTrivia.Add(t)
                                Case Else
                                    ' Directives are ignored but the results are converted. Disabled Text is deleted
                                    'Stop
                            End Select
                        Next
                        If FoundEOL Then
                            CS_CollectedCommentTrivia.Add(CS.SyntaxFactory.EndOfLine(vbLf))
                            Declator = Declator.WithTrailingTrivia(ConvertTrivia(CS_CollectedCommentTrivia))
                            CS_CollectedCommentTrivia.Clear()
                        Else
                            Declator = Declator.WithTrailingTrivia(NonCommentTrailingTrivia)
                        End If
                        If i = VariableDeclaration.Variables.Count - 1 Then
                            If Not Declator.HasTrailingTrivia OrElse Not Declator.GetTrailingTrivia.Last.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                Declator = Declator.WithAppendedTrailingTrivia(VB_EOLTrivia)
                            End If
                        End If
                    End If
                    declarators.Add(Declator)
                End If
            Next
            If declaratorsWithoutInitializers.Count > 0 Then
                Dim ModifiedIdentifierList As New List(Of VBS.ModifiedIdentifierSyntax)
                For Each d As CSS.VariableDeclaratorSyntax In declaratorsWithoutInitializers
                    Dim dTrailingTrivia As SyntaxTriviaList = d.GetTrailingTrivia
                    If d.HasTrailingTrivia And dTrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                        CS_CollectedCommentTrivia.AddRange(dTrailingTrivia)
                    End If
                    ModifiedIdentifierList.Add(DirectCast(d.Accept(_NodesVisitor), VBS.ModifiedIdentifierSyntax).WithTrailingTrivia(SpaceTrivia))
                Next
                Dim VariableDeclarator As VBS.VariableDeclaratorSyntax = VB.SyntaxFactory.VariableDeclarator(VB.SyntaxFactory.SeparatedList(ModifiedIdentifierList), asClause:=VB.SyntaxFactory.SimpleAsClause(type), initializer:=Nothing)
                declarators.Insert(0, VariableDeclarator.WithTrailingTrivia(ConvertTrivia(CS_CollectedCommentTrivia)))
                CS_CollectedCommentTrivia.Clear()
            End If
            If CS_CollectedCommentTrivia.Any Then
                Dim FinalTrivia As New List(Of SyntaxTrivia)
                FinalTrivia.AddRange(ConvertTrivia(CS_CollectedCommentTrivia))
                FinalTrivia.AddRange(declarators.Last.GetTrailingTrivia)
                Dim TempDeclarator As VBS.VariableDeclaratorSyntax = declarators.Last.WithTrailingTrivia(FinalTrivia)
                declarators.RemoveAt(declarators.Count - 1)
                declarators.Add(TempDeclarator)
            End If
            Return VB.SyntaxFactory.SeparatedList(declarators)
        End Function

        ''' <summary>
        ''' Entry Point for converting source, used in testing and applications
        ''' </summary>
        ''' <param name="SourceTree"></param>
        ''' <param name="_SkipAutoGenerated"></param>
        ''' <param name="_SemanticModel"></param>
        ''' <returns></returns>
        Public Shared Function Convert(SourceTree As CS.CSharpSyntaxNode, _SkipAutoGenerated As Boolean, _SemanticModel As SemanticModel) As VB.VisualBasicSyntaxNode
            Dim visualBasicSyntaxNode1 As VB.VisualBasicSyntaxNode
            If OriginalRequest Is Nothing Then
                OriginalRequest = New ConvertRequest(ConvertRequest.CS_To_VB, _SkipAutoGenerated, Sub() Return, _ProgressBar:=Nothing)
            End If
            SyncLock ThisLock
                ClearMarker()
                UsedIdentifierStack.Push(UsedIdentifiers)
                UsedIdentifiers.Clear()
                visualBasicSyntaxNode1 = SourceTree.Accept(New NodesVisitor(_SemanticModel))
                If UsedIdentifierStack.Count > 0 Then
                    UsedIdentifiers = DirectCast(UsedIdentifierStack.Pop, Dictionary(Of String, SymbolTableEntry))
                End If
            End SyncLock
            Return visualBasicSyntaxNode1
        End Function

    End Class

End Namespace