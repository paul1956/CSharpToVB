' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports CSharpToVBCodeConverter.DestVisualBasic
Imports CSharpToVBCodeConverter.Util

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBCodeConverter

    Public Module TypeConverterSupport

        Private Function MakeIdentifierUnique(id As SyntaxToken, BracketNeeded As Boolean, QualifiedNameOrTypeName As Boolean) As SyntaxToken
            Dim ConvertedIdentifier As String = If(BracketNeeded, $"[{id.ValueText}]", id.ValueText)
            If ConvertedIdentifier = "_" Then
                ConvertedIdentifier = "underscore"
            End If
            ' Don't Change Qualified Names
            If QualifiedNameOrTypeName Then
                If Not s_usedIdentifiers.ContainsKey(ConvertedIdentifier) Then
                    s_usedIdentifiers.Add(ConvertedIdentifier, New SymbolTableEntry(ConvertedIdentifier, True))
                End If
                Return VBFactory.Identifier(s_usedIdentifiers(ConvertedIdentifier).Name).WithConvertedTriviaFrom(id)
            End If
            If s_usedIdentifiers.ContainsKey(ConvertedIdentifier) Then
                ' We have a case sensitive exact match so just return it
                Return VBFactory.Identifier(s_usedIdentifiers(ConvertedIdentifier).Name).WithConvertedTriviaFrom(id)
            End If
            Dim IsFieldIdentifier As Boolean = False
            If TypeOf id.Parent Is CSS.VariableDeclaratorSyntax Then
                Dim VarDecl As CSS.VariableDeclaratorSyntax = CType(id.Parent, CSS.VariableDeclaratorSyntax)
                Dim FieldDeclatationOrNothing As CSS.FieldDeclarationSyntax = VarDecl.FirstAncestorOrSelf(Of CSS.FieldDeclarationSyntax)
                If Char.IsLower(CChar(ConvertedIdentifier.Left(1))) AndAlso FieldDeclatationOrNothing IsNot Nothing Then
                    If FieldDeclatationOrNothing.Modifiers.Any AndAlso FieldDeclatationOrNothing.Modifiers.Contains(Function(t As SyntaxToken) t.IsKind(CS.SyntaxKind.PrivateKeyword)) Then
                        IsFieldIdentifier = True
                    End If
                End If
            End If
            For Each ident As KeyValuePair(Of String, SymbolTableEntry) In s_usedIdentifiers
                If String.Compare(ident.Key, ConvertedIdentifier, ignoreCase:=False, Globalization.CultureInfo.InvariantCulture) = 0 Then
                    ' We have an exact match keep looking
                    Continue For
                End If
                If String.Compare(ident.Key, ConvertedIdentifier, ignoreCase:=True, Globalization.CultureInfo.InvariantCulture) = 0 Then
                    ' If we are here we have seen the variable in a different case so fix it
                    Dim NewUniqueName As String = GetNewUniqueName(ConvertedIdentifier, ident)
                    If s_usedIdentifiers(ident.Key).IsType Then
                        s_usedIdentifiers.Add(ConvertedIdentifier, New SymbolTableEntry(Name:=NewUniqueName, IsType:=False))
                    Else
                        s_usedIdentifiers.Add(ConvertedIdentifier, New SymbolTableEntry(Name:=NewUniqueName, IsType:=QualifiedNameOrTypeName))
                    End If
                    Return VBFactory.Identifier(s_usedIdentifiers(ConvertedIdentifier).Name).WithConvertedTriviaFrom(id)
                End If
            Next
            Dim _ConvertedIdentifier As String = $"{If(IsFieldIdentifier, "_", "")}{ConvertedIdentifier}"
            s_usedIdentifiers.Add(ConvertedIdentifier, New SymbolTableEntry(_ConvertedIdentifier, QualifiedNameOrTypeName))
            Return VBFactory.Identifier(_ConvertedIdentifier)
        End Function

        Friend Function ConvertToType(PossibleTupleType As ITypeSymbol) As VBS.TypeSyntax
            If PossibleTupleType.IsKind(SymbolKind.ArrayType) Then
                Dim ElementType As VBS.TypeSyntax = ConvertToType(DirectCast(PossibleTupleType, IArrayTypeSymbol).ElementType)
                If TypeOf ElementType Is VBS.ArrayTypeSyntax Then
                    Return ElementType
                End If
                Return VBFactory.ArrayType(ElementType)
            End If
            If PossibleTupleType.IsTupleType Then
                Dim TupleElementList As New List(Of VBS.TupleElementSyntax)
                For Each TupleElement As IFieldSymbol In DirectCast(PossibleTupleType, INamedTypeSymbol).TupleElements
                    TupleElementList.Add(CSharpConverter.ConvertToTupleElement(TupleElement))
                Next
                Return VBFactory.TupleType(TupleElementList.ToArray)
            End If
            If PossibleTupleType.Name = "Tuple" Then
                Dim TupleElementList As New List(Of VBS.TypeSyntax)
                For Each TupleElement As ITypeSymbol In DirectCast(PossibleTupleType, INamedTypeSymbol).TypeArguments
                    TupleElementList.Add(ConvertToType(TupleElement))
                Next
                Return VBFactory.GenericName("Tuple", VBFactory.TypeArgumentList(VBFactory.SeparatedList(TupleElementList)))
            End If
            Dim PossibleName As String = PossibleTupleType.ToString.Trim
            Dim StartIndex As Integer = PossibleName.IndexOf("<", StringComparison.Ordinal)
            If StartIndex > 0 Then
                Dim IndexOfLastGreaterThan As Integer = PossibleName.LastIndexOf(">", StringComparison.Ordinal)
                Dim Name As String = PossibleName.Substring(0, StartIndex)
                Dim PossibleTypes As String = PossibleName.Substring(StartIndex + 1, IndexOfLastGreaterThan - StartIndex - 1)
                If PossibleTupleType.ToString.StartsWith("System.Func", StringComparison.Ordinal) Then
                    Dim DictionaryTypeElement As New List(Of VBS.TypeSyntax)
                    While PossibleTypes.Length > 0
                        Dim EndIndex As Integer
                        ' Tuple
                        If PossibleTypes.StartsWith("(", StringComparison.Ordinal) Then
                            ' Tuple
                            EndIndex = PossibleTypes.LastIndexOf(")", StringComparison.Ordinal)
                            DictionaryTypeElement.Add(CSharpConverter.ConvertCSTupleToVBType(PossibleTypes.Substring(0, EndIndex + 1).Trim))
                            EndIndex += 1
                        Else
                            ' Type
                            EndIndex = PossibleTypes.IndexOf(",", StringComparison.Ordinal)
                            Dim FirstLessThan As Integer = PossibleTypes.IndexOf("<", StringComparison.Ordinal)
                            EndIndex = If(EndIndex = -1 OrElse (FirstLessThan <> -1 AndAlso FirstLessThan < EndIndex), PossibleTypes.Length, EndIndex)
                            DictionaryTypeElement.Add(ConvertToType(PossibleTypes.Substring(0, EndIndex) _
                                                                                     .Replace("<", "(Of ", StringComparison.Ordinal) _
                                                                                     .Replace(">", ")", StringComparison.Ordinal).Trim))
                        End If
                        If EndIndex + 1 < PossibleTypes.Length Then
                            PossibleTypes = PossibleTypes.Substring(EndIndex + 1).Trim
                        Else
                            Exit While
                        End If
                    End While
                    Return VBFactory.GenericName(Name, VBFactory.TypeArgumentList(VBFactory.SeparatedList(DictionaryTypeElement)))
                End If
                ' Could be dictionary or List
                If TypeOf PossibleTupleType Is INamedTypeSymbol AndAlso PossibleName.Contains(",", StringComparison.Ordinal) Then
                    Dim NamedType As INamedTypeSymbol = CType(PossibleTupleType, INamedTypeSymbol)
                    Dim DictionaryTypeElement As New List(Of VBS.TypeSyntax)
                    If Not NamedType.TypeArguments.Any Then
                        Return PredefinedTypeObject
                    End If
                    For Each Element As ITypeSymbol In NamedType.TypeArguments
                        DictionaryTypeElement.Add(ConvertToType(Element))
                    Next
                    Return VBFactory.GenericName(Name,
                                                     VBFactory.TypeArgumentList(OpenParenToken,
                                                                                OfKeyword.WithTrailingTrivia(SpaceTrivia),
                                                                                VBFactory.SeparatedList(DictionaryTypeElement),
                                                                                CloseParenToken
                                                                                )
                                                    )
                End If
            End If
            Return ConvertToType(PossibleName)
        End Function

        Friend Function ConvertToType(TypeAsCSString As String, Optional AllowArray As Boolean = True) As VBS.TypeSyntax
            Dim typeString As String = TypeAsCSString.Trim
            Dim arrayRank As String = ""
            If typeString.EndsWith("]", StringComparison.OrdinalIgnoreCase) Then
                Dim IndexOfBracket As Integer
                Dim foundclosedBracker As Boolean = True
                For IndexOfBracket = typeString.Length - 2 To 0 Step -1
                    Select Case typeString.Chars(IndexOfBracket)
                        Case "]"c
                            foundclosedBracker = True
                        Case "["c
                            foundclosedBracker = False
                        Case Else
                            If Not foundclosedBracker Then
                                IndexOfBracket += 1
                                Exit For
                            End If
                    End Select
                Next
                If AllowArray Then
                    arrayRank = typeString.Substring(IndexOfBracket).
                                        Replace("[", "(", StringComparison.Ordinal).
                                        Replace("]", ")", StringComparison.Ordinal)
                End If
                typeString = typeString.Substring(0, IndexOfBracket)
            End If

            If typeString.Contains("<", StringComparison.Ordinal) Then
                    typeString = typeString.Replace("<", "(Of ", StringComparison.Ordinal).
                                        Replace(">", ")", StringComparison.Ordinal)
                End If
                Dim IndexOf As Integer = typeString.IndexOf("(Of ", StringComparison.OrdinalIgnoreCase)
            If IndexOf >= 0 Then
                Dim Name As String = typeString.Substring(0, IndexOf)
                typeString = typeString.Substring(IndexOf + 3)
                Dim IndexOfLastCloseParen As Integer = typeString.LastIndexOf(")", StringComparison.OrdinalIgnoreCase)
                typeString = typeString.Substring(0, IndexOfLastCloseParen)
                Dim TypeList As New List(Of VBS.TypeSyntax)
                Dim PossibleTypes As String = typeString.Trim
                While PossibleTypes.Length > 0
                    Dim EndIndex As Integer
                    ' Type
                    EndIndex = PossibleTypes.IndexOf(",", StringComparison.Ordinal)
                    Dim FirstLessThan As Integer = PossibleTypes.IndexOf("(", StringComparison.Ordinal)
                    If EndIndex = -1 OrElse FirstLessThan = -1 Then
                        EndIndex = PossibleTypes.Length
                    ElseIf EndIndex > FirstLessThan Then
                        Dim OpenParenCount As Integer = 0
                        For currentIndex As Integer = FirstLessThan To PossibleTypes.Length - 1
                            Select Case PossibleTypes.Substring(currentIndex, 1)
                                Case "("
                                    OpenParenCount += 1
                                Case ")"
                                    OpenParenCount -= 1
                                    EndIndex = currentIndex + 1
                                Case ","
                                    If OpenParenCount = 0 Then
                                        EndIndex = currentIndex
                                        Exit For
                                    End If
                            End Select
                        Next
                    End If
                    TypeList.Add(ConvertToType(PossibleTypes.Substring(0, EndIndex)).WithLeadingTrivia(SpaceTrivia))
                    If EndIndex + 1 < PossibleTypes.Length Then
                        PossibleTypes = PossibleTypes.Substring(EndIndex + 1).Trim
                    Else
                        Exit While
                    End If
                End While
                Dim TypeArguemntList As VBS.TypeArgumentListSyntax = VBFactory.TypeArgumentList(VBFactory.SeparatedList(TypeList))
                Return VBFactory.GenericName(Name, TypeArguemntList)
            End If
            If typeString.EndsWith("*", StringComparison.OrdinalIgnoreCase) Then
                Return IntPtrType
            End If
            Select Case typeString.ToUpperInvariant
                Case "BYTE"
                    typeString = "Byte"
                Case "SBYTE"
                    typeString = "SByte"
                Case "INT", "INTEGER"
                    typeString = "Integer"
                Case "UINT", "UINTEGER"
                    typeString = "UInteger"
                Case "SHORT"
                    typeString = "Short"
                Case "USHORT"
                    typeString = "UShort"
                Case "LONG"
                    typeString = "Long"
                Case "ULONG"
                    typeString = "ULong"
                Case "FLOAT"
                    typeString = "Single"
                Case "DOUBLE"
                    typeString = "Double"
                Case "CHAR"
                    typeString = "Char"
                Case "BOOL", "BOOLEAN"
                    typeString = "Boolean"
                Case "OBJECT", "VAR"
                    typeString = "Object"
                Case "STRING"
                    typeString = "String"
                Case "DECIMAL"
                    typeString = "Decimal"
                Case "DATETIME"
                    typeString = "DateTime"
                Case "DATE"
                    typeString = "Date"
                Case "?", "_"
                    typeString = "Object"
                Case Else
                    If typeString.Contains("[", StringComparison.OrdinalIgnoreCase) Then
                        typeString = typeString.
                                        Replace("[", "(", StringComparison.Ordinal).
                                        Replace("]", ")", StringComparison.Ordinal)
                    End If
                    typeString = MakeVBSafeName(typeString)
            End Select
            Return VBFactory.ParseTypeName($"{typeString}{arrayRank}")
        End Function

        ''' <summary>
        ''' Returns Safe VB Name with QualifiedName and TypeName both false
        ''' </summary>
        ''' <param name="id"></param>
        ''' <returns></returns>
        Public Function GenerateSafeVBToken(id As SyntaxToken) As SyntaxToken
            Return GenerateSafeVBToken(id, IsQualifiedName:=False, IsTypeName:=False)
        End Function

        ''' <summary>
        ''' Returns Safe VB Name
        ''' </summary>
        ''' <param name="id">Original Name</param>
        ''' <param name="IsQualifiedName">True if name is part of a Qualified Name and should not be renamed</param>
        ''' <param name="IsTypeName"></param>
        ''' <returns></returns>
        Public Function GenerateSafeVBToken(id As SyntaxToken, IsQualifiedName As Boolean, IsTypeName As Boolean) As SyntaxToken
            Debug.Assert(id.Language = "C#", $"In {NameOf(GenerateSafeVBToken)} Language of Token is not C#")
            Debug.Assert((Not id.HasLeadingTrivia) OrElse id.LeadingTrivia(0).Language = "C#", $"In {NameOf(GenerateSafeVBToken)} Language of ID Token leading trivia is not VB")
            Debug.Assert((Not id.HasTrailingTrivia) OrElse id.TrailingTrivia(0).Language = "C#", $"In {NameOf(GenerateSafeVBToken)} Language of ID Token trailing trivia is not VB")

            Dim keywordKind As VB.SyntaxKind = VB.SyntaxFacts.GetKeywordKind(id.ValueText)
            If VB.SyntaxFacts.IsKeywordKind(keywordKind) Then
                Dim bracketNeeded As Boolean = True
                If keywordKind = VB.SyntaxKind.REMKeyword Then
                    bracketNeeded = True
                ElseIf TypeOf id.Parent?.Parent Is CSS.MemberAccessExpressionSyntax Then
                    bracketNeeded = CType(id.Parent?.Parent, CSS.MemberAccessExpressionSyntax).Expression.ToString.Equals(id.ToString, StringComparison.Ordinal)
                End If
                Return MakeIdentifierUnique(id, bracketNeeded, QualifiedNameOrTypeName:=IsQualifiedName)
            End If
            If IsTypeName Then
                IsQualifiedName = True
            Else
                If VB.SyntaxFacts.IsPredefinedType(keywordKind) Then
                    Return MakeIdentifierUnique(id, BracketNeeded:=True, QualifiedNameOrTypeName:=IsQualifiedName)
                End If
            End If

            If id.Parent?.IsParentKind(CS.SyntaxKind.Parameter) Then
                Dim Param As CSS.ParameterSyntax = DirectCast(id.Parent.Parent, CSS.ParameterSyntax)
                Dim MethodDeclaration As CSS.MethodDeclarationSyntax = TryCast(Param.Parent?.Parent, CSS.MethodDeclarationSyntax)
                IsQualifiedName = MethodDeclaration Is Nothing OrElse String.Compare(MethodDeclaration.Identifier.ValueText, id.ValueText, ignoreCase:=True, Globalization.CultureInfo.InvariantCulture) = 0
                IsQualifiedName = IsQualifiedName Or String.Compare(Param.Type.ToString, id.ValueText, ignoreCase:=False, Globalization.CultureInfo.InvariantCulture) = 0
            End If
            Return MakeIdentifierUnique(id, BracketNeeded:=False, QualifiedNameOrTypeName:=IsQualifiedName)
        End Function

    End Module
End Namespace
