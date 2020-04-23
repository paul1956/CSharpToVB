' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.Threading

Imports CSharpToVBCodeConverter.Util

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBCodeConverter.DestVisualBasic

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

        Private Shared Function GetLiteralExpression(value As Object, Token As SyntaxToken, _NodesVisitor As NodesVisitor) As VBS.ExpressionSyntax
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
                    ElseIf TokenToString.StartsWith("0b", StringComparison.OrdinalIgnoreCase) Then
                        If TypeOf value Is Integer Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal(($"{Binary(CInt(value))}"), CInt(value)))
                        If TypeOf value Is Byte Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal(($"{Binary(CByte(value))}"), CByte(value)))
                        If TypeOf value Is SByte Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal($"{Binary(CSByte(value))}", CSByte(value)))
                        If TypeOf value Is Short Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal($"{Binary(CShort(value))}", CShort(value)))
                        If TypeOf value Is UShort Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal($"{Binary(CUShort(value))}", CUShort(value)))
                        If TypeOf value Is UInteger Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal($"{Binary(CUInt(value))}", CUInt(value)))
                        If TypeOf value Is Long Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal($"{Binary(CLng(value))}", CLng(value)))
                        If TypeOf value Is ULong Then Return VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal($"{Binary(CType(CULng(value), Long))}UL", CULng(value)))
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
                    End If
                    Dim EqualsValue As CSS.EqualsValueClauseSyntax = Token.Parent.GetAncestor(Of CSS.EqualsValueClauseSyntax)
                    If EqualsValue IsNot Nothing Then
                        Dim Parameter As CSS.ParameterSyntax = Token.Parent.GetAncestor(Of CSS.ParameterSyntax)
                        If Parameter IsNot Nothing Then
                            ReturnType = DirectCast(Parameter.Type.Accept(_NodesVisitor), VBS.TypeSyntax)
                            Return VBFactory.CTypeExpression(NothingExpression, ReturnType)
                        End If
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
                    Return NothingExpression
                    If Token.Parent.GetAncestor(Of CSS.ArgumentSyntax) IsNot Nothing Then
                        Return NothingExpression
                    End If
                Case CS.SyntaxKind.ArgListKeyword
                    Return VBFactory.IdentifierName("__Arglist")
            End Select
            Stop
            Return NothingExpression
        End Function

        ''' <summary>
        ''' Converts C# VariableDeclaration to VB List(Of VariableDeclaratorSyntax)
        ''' </summary>
        ''' <param name="variableDeclaration"></param>
        ''' <param name="Visitor"></param>
        ''' <param name="Model"></param>
        ''' <param name="IsFieldDeclaration"></param>
        ''' <param name="LeadingTrivia"></param>
        ''' <returns></returns>
        ''' <remarks>Fix handling of AddressOf where is mistakenly added to DirectCast and C# Pointers</remarks>
        Private Shared Function RemodelVariableDeclaration(variableDeclaration As CSS.VariableDeclarationSyntax, Visitor As NodesVisitor, Model As SemanticModel, IsFieldDeclaration As Boolean, ByRef LeadingTrivia As List(Of SyntaxTrivia)) As SeparatedSyntaxList(Of VBS.VariableDeclaratorSyntax)
            Dim vbType As VBS.TypeSyntax
            Dim declarationType As VB.VisualBasicSyntaxNode = variableDeclaration.Type.Accept(Visitor)
            Dim typeOrAddressOf As VB.VisualBasicSyntaxNode = declarationType.WithConvertedLeadingTriviaFrom(variableDeclaration.Type)
            Dim typeLeadingTrivia As SyntaxTriviaList = typeOrAddressOf.GetLeadingTrivia

            If typeLeadingTrivia.Any Then
                If typeLeadingTrivia.Last.RawKind = VB.SyntaxKind.WhitespaceTrivia Then
                    typeOrAddressOf = typeOrAddressOf.WithLeadingTrivia(typeLeadingTrivia.Last)
                Else
                    typeOrAddressOf = typeOrAddressOf.WithLeadingTrivia(SpaceTrivia)
                End If
            End If
            If IsFieldDeclaration AndAlso typeLeadingTrivia.Count > 1 Then
                Dim statementWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(variableDeclaration)
                If typeLeadingTrivia.ContainsCommentOrDirectiveTrivia AndAlso Not TriviaIsIdentical(typeLeadingTrivia, ConvertTrivia(statementWithIssues.GetLeadingTrivia).ToList) Then
                    statementWithIssues.AddMarker(VBFactory.EmptyStatement.WithLeadingTrivia(typeLeadingTrivia), StatementHandlingOption.AppendEmptyStatement, AllowDuplicates:=True)
                End If
            End If
            Dim csCollectedCommentTrivia As New List(Of SyntaxTrivia)
            If typeOrAddressOf.IsKind(VB.SyntaxKind.AddressOfExpression) Then
                vbType = IntPtrType
            Else
                vbType = DirectCast(typeOrAddressOf, VBS.TypeSyntax)
            End If
            vbType = vbType.WithModifiedNodeTrivia(SeparatorFollows:=True)
            Dim csDeclaratorsWithoutInitializers As New List(Of CSS.VariableDeclaratorSyntax)()
            Dim vbDeclarators As New List(Of VBS.VariableDeclaratorSyntax)

            For Each v As CSS.VariableDeclaratorSyntax In variableDeclaration.Variables
                If v.Initializer Is Nothing Then
                    csDeclaratorsWithoutInitializers.Add(v.WithAppendedTrailingTrivia(csCollectedCommentTrivia))
                    csCollectedCommentTrivia.Clear()
                    Continue For
                End If
                Dim asClause As VBS.AsClauseSyntax = Nothing
                If variableDeclaration.Type.IsKind(CS.SyntaxKind.RefType) Then
                ElseIf Not variableDeclaration.Type.IsVar Then
                    asClause = VBFactory.SimpleAsClause(vbType)
                Else
                    ' Get Type from Initializer
                    If v.Initializer.Value.IsKind(CS.SyntaxKind.AnonymousObjectCreationExpression) Then
                        asClause = VBFactory.AsNewClause(CType(v.Initializer.Value.Accept(Visitor), VBS.NewExpressionSyntax))
                    ElseIf v.Initializer.Value.IsKind(CS.SyntaxKind.ImplicitArrayCreationExpression) Then
                    Else
                        Dim resultTuple As (_Error As Boolean, _TypeSyntax As VBS.TypeSyntax) = DetermineTypeSyntax(v.Initializer.Value, Model)
                        If Not resultTuple._Error Then
                            asClause = VBFactory.SimpleAsClause(resultTuple._TypeSyntax)
                        Else
                            asClause = Nothing
                        End If
                    End If
                End If
                Dim initializerValue As VBS.ExpressionSyntax = DirectCast(v.Initializer.Value.Accept(Visitor), VBS.ExpressionSyntax)
                If initializerValue Is Nothing Then
                    initializerValue = VBFactory.IdentifierName("HandleRefExpression").WithConvertedTriviaFrom(v.Initializer.Value)
                End If
                If initializerValue.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    LeadingTrivia.AddRange(initializerValue.GetLeadingTrivia)
                End If
                Dim initializer As VBS.EqualsValueSyntax = Nothing
                If Not asClause.IsKind(VB.SyntaxKind.AsNewClause) Then
                    initializer = VBFactory.EqualsValue(initializerValue.WithLeadingTrivia(SpaceTrivia))
                    If initializer.Value.IsKind(VB.SyntaxKind.ObjectCreationExpression) Then
                        If asClause IsNot Nothing AndAlso CType(asClause, VBS.SimpleAsClauseSyntax).Type.ToString = CType(initializerValue, VBS.ObjectCreationExpressionSyntax).Type.ToString Then
                            asClause = VBFactory.AsNewClause(CType(initializerValue, VBS.ObjectCreationExpressionSyntax))
                            initializer = Nothing
                        End If
                    End If
                End If
                ' Get the names last to lead with var jsonWriter = new JsonWriter(stringWriter)
                ' Which should be Dim jsonWriter_Renamed = new JsonWriter(stringWriter)
                vbDeclarators.Add(
                    VBFactory.VariableDeclarator(VBFactory.SingletonSeparatedList(DirectCast(v.Accept(Visitor), VBS.ModifiedIdentifierSyntax)),
                                                asClause,
                                                initializer
                                                ).WithModifiedNodeTrailingTrivia(SeparatorFollows:=False)
                                )
            Next
            If csDeclaratorsWithoutInitializers.Any Then
                Dim ModifiedIdentifierList As New List(Of VBS.ModifiedIdentifierSyntax)
                For Each csVarDeclaration As CSS.VariableDeclaratorSyntax In csDeclaratorsWithoutInitializers
                    Dim dTrailingTrivia As SyntaxTriviaList = csVarDeclaration.GetTrailingTrivia
                    If csVarDeclaration.HasTrailingTrivia And dTrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                        csCollectedCommentTrivia.AddRange(dTrailingTrivia)
                    End If
                    ModifiedIdentifierList.Add(DirectCast(csVarDeclaration.Accept(Visitor), VBS.ModifiedIdentifierSyntax).WithTrailingTrivia(SpaceTrivia))
                Next
                Dim varDeclarator As VBS.VariableDeclaratorSyntax = VBFactory.VariableDeclarator(VBFactory.SeparatedList(ModifiedIdentifierList), asClause:=VBFactory.SimpleAsClause(vbType), initializer:=Nothing)
                vbDeclarators.Insert(0, varDeclarator.WithTrailingTrivia(ConvertTrivia(csCollectedCommentTrivia)))
                csCollectedCommentTrivia.Clear()
            End If
            If csCollectedCommentTrivia.Any Then
                Dim finalTrivia As New List(Of SyntaxTrivia)
                finalTrivia.AddRange(ConvertTrivia(csCollectedCommentTrivia))
                finalTrivia.AddRange(vbDeclarators.Last.GetTrailingTrivia)
                Dim tempDeclarator As VBS.VariableDeclaratorSyntax = vbDeclarators.Last.WithTrailingTrivia(finalTrivia)
                vbDeclarators.RemoveAt(vbDeclarators.Count - 1)
                vbDeclarators.Add(tempDeclarator)
            End If
            Return VBFactory.SeparatedList(vbDeclarators)
        End Function

        ''' <summary>
        ''' Entry Point for converting source and new applications
        ''' </summary>
        ''' <param name="SourceTree"></param>
        ''' <param name="SkipAutoGenerated"></param>
        ''' <param name="DefaultVBOptions"></param>
        ''' <param name="pSemanticModel"></param>
        ''' <returns></returns>
        Public Shared Function Convert(SourceTree As CS.CSharpSyntaxNode, SkipAutoGenerated As Boolean, DefaultVBOptions As DefaultVBOptions, pSemanticModel As SemanticModel, ReportException As Action(Of Exception), Progress As IProgress(Of ProgressReport), CancelToken As CancellationToken) As VB.VisualBasicSyntaxNode
            Dim visualBasicSyntaxNode1 As VB.VisualBasicSyntaxNode
            s_originalRequest = New ConvertRequest(SkipAutoGenerated, Progress, CancelToken)
            SyncLock s_thisLock
                ClearMarker()
                s_usedStacks.Push(s_usedIdentifiers)
                s_usedIdentifiers.Clear()
                visualBasicSyntaxNode1 = SourceTree?.Accept(New NodesVisitor(pSemanticModel, DefaultVBOptions, ReportException))
                If s_usedStacks.Count > 0 Then
                    s_usedIdentifiers = DirectCast(s_usedStacks.Pop, Dictionary(Of String, SymbolTableEntry))
                End If
            End SyncLock
            Return visualBasicSyntaxNode1
        End Function

        ''' <summary>
        ''' Entry Point for converting source, used in testing and legacy applications
        ''' </summary>
        ''' <param name="SourceTree"></param>
        ''' <param name="SkipAutoGenerated"></param>
        ''' <param name="pSemanticModel"></param>
        ''' <returns></returns>
        <Obsolete("Don't use this routine any more. Use the new one instead to specify what 'Options' to include in output, use 'New DefaultVBOptions' to get the legacy behavior.")>
        Public Shared Function Convert(SourceTree As CS.CSharpSyntaxNode, SkipAutoGenerated As Boolean, pSemanticModel As SemanticModel, ReportException As Action(Of Exception), Progress As IProgress(Of ProgressReport), CancelToken As CancellationToken) As VB.VisualBasicSyntaxNode
            Return Convert(SourceTree, SkipAutoGenerated, New DefaultVBOptions, pSemanticModel, ReportException, Progress, CancelToken)
        End Function

    End Class

End Namespace
