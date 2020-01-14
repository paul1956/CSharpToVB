' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

Imports CSharpToVBCodeConverter.Util

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory

Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBCodeConverter

    Public Module TypeConverterSupport

        Private Function ConvertToTupleElement(TupleElement As IFieldSymbol) As TupleElementSyntax
            If TupleElement.Type Is Nothing Then
                Return VBFactory.NamedTupleElement(TupleElement.Name.ToString(Globalization.CultureInfo.InvariantCulture))
            End If
            Dim AsClause As SimpleAsClauseSyntax = VBFactory.SimpleAsClause(ConvertToType(TupleElement.Type))
            Return VBFactory.NamedTupleElement(VBFactory.Identifier(AddBracketsIfRequired(TupleElement.Name)), AsClause)
        End Function

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
                    If FieldDeclatationOrNothing.Modifiers.Count > 0 AndAlso FieldDeclatationOrNothing.Modifiers.Contains(Function(t As SyntaxToken) t.IsKind(CS.SyntaxKind.PrivateKeyword)) Then
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
                    If s_usedIdentifiers(ident.Key).IsType Then
                        s_usedIdentifiers.Add(ConvertedIdentifier, New SymbolTableEntry(Name:=ConvertedIdentifier, IsType:=False))
                    Else
                        Dim NewUniqueName As String
                        If ident.Value.Name.StartsWith("_", StringComparison.InvariantCulture) Then
                            NewUniqueName = ConvertedIdentifier
                        Else
                            NewUniqueName = If(ConvertedIdentifier.StartsWith("[", StringComparison.InvariantCulture),
                                                    ConvertedIdentifier.Replace("[", "_", StringComparison.InvariantCulture).
                                                                        Replace("]", "", StringComparison.InvariantCulture),
                                                    If(Char.IsLower(CChar(ConvertedIdentifier.Substring(0, 1))),
                                                        $"_{ConvertedIdentifier}",
                                                        $"{ConvertedIdentifier}_Renamed"))
                        End If
                        s_usedIdentifiers.Add(ConvertedIdentifier, New SymbolTableEntry(Name:=NewUniqueName, IsType:=QualifiedNameOrTypeName))
                    End If
                    Return VBFactory.Identifier(s_usedIdentifiers(ConvertedIdentifier).Name).WithConvertedTriviaFrom(id)
                End If
            Next
            Dim _ConvertedIdentifier As String = $"{If(IsFieldIdentifier, "_", "")}{ConvertedIdentifier}"
            s_usedIdentifiers.Add(ConvertedIdentifier, New SymbolTableEntry(_ConvertedIdentifier, QualifiedNameOrTypeName))
            Return VBFactory.Identifier(_ConvertedIdentifier)
        End Function

        Friend Function AddBracketsIfRequired(Id As String) As String
            If IsSpecialReservedWord(Id) OrElse VB.SyntaxFacts.IsKeywordKind(VB.SyntaxFacts.GetKeywordKind(Id)) Then
                Return $"[{Id}]"
            End If
            Return Id
        End Function

        Friend Function ConvertToType(PossibleTupleType As ITypeSymbol) As TypeSyntax
            If PossibleTupleType.IsKind(SymbolKind.ArrayType) Then
                Dim ElementType As TypeSyntax = ConvertToType(DirectCast(PossibleTupleType, IArrayTypeSymbol).ElementType)
                If TypeOf ElementType Is ArrayTypeSyntax Then
                    Return ElementType
                End If
                Return VBFactory.ArrayType(ElementType)
            End If
            If PossibleTupleType.IsTupleType Then
                Dim TupleElementList As New List(Of TupleElementSyntax)
                For Each TupleElement As IFieldSymbol In DirectCast(PossibleTupleType, INamedTypeSymbol).TupleElements
                    TupleElementList.Add(ConvertToTupleElement(TupleElement))
                Next
                Return VBFactory.TupleType(TupleElementList.ToArray)
            End If
            If PossibleTupleType.Name = "Tuple" Then
                Dim TupleElementList As New List(Of TypeSyntax)
                For Each TupleElement As ITypeSymbol In DirectCast(PossibleTupleType, INamedTypeSymbol).TypeArguments
                    TupleElementList.Add(ConvertToType(TupleElement))
                Next
                Return VBFactory.GenericName("Tuple", VBFactory.TypeArgumentList(VBFactory.SeparatedList(TupleElementList)))
            End If
            Dim PossibleName As String = PossibleTupleType.ToString.Trim
            Dim StartIndex As Integer = PossibleName.IndexOf("<", StringComparison.InvariantCulture)
            If StartIndex > 0 Then
                Dim IndexOfLastGreaterThan As Integer = PossibleName.LastIndexOf(">", StringComparison.InvariantCulture)
                Dim Name As String = PossibleName.Substring(0, StartIndex)
                Dim PossibleTypes As String = PossibleName.Substring(StartIndex + 1, IndexOfLastGreaterThan - StartIndex - 1)
                If PossibleTupleType.ToString.StartsWith("System.Func", StringComparison.InvariantCulture) Then
                    Dim DictionaryTypeElement As New List(Of TypeSyntax)
                    While PossibleTypes.Length > 0
                        Dim EndIndex As Integer
                        ' Tuple
                        If PossibleTypes.StartsWith("(", StringComparison.InvariantCulture) Then
                            ' Tuple
                            EndIndex = PossibleTypes.LastIndexOf(")", StringComparison.InvariantCulture)
                            DictionaryTypeElement.Add(CovertStringToTupleType(PossibleTypes.Substring(0, EndIndex + 1).Trim))
                            EndIndex += 1
                        Else
                            ' Type
                            EndIndex = PossibleTypes.IndexOf(",", StringComparison.InvariantCulture)
                            Dim FirstLessThan As Integer = PossibleTypes.IndexOf("<", StringComparison.InvariantCulture)
                            EndIndex = If(EndIndex = -1 OrElse (FirstLessThan <> -1 AndAlso FirstLessThan < EndIndex), PossibleTypes.Length, EndIndex)
                            DictionaryTypeElement.Add(ConvertToType(PossibleTypes.Substring(0, EndIndex) _
                                                                                     .Replace("<", "(Of ", StringComparison.InvariantCulture) _
                                                                                     .Replace(">", ")", StringComparison.InvariantCulture).Trim))
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
                If TypeOf PossibleTupleType Is INamedTypeSymbol AndAlso PossibleName.Contains(",", StringComparison.InvariantCulture) Then
                    Dim NamedType As INamedTypeSymbol = CType(PossibleTupleType, INamedTypeSymbol)
                    Dim DictionaryTypeElement As New List(Of TypeSyntax)
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
            Return ConvertToType(PossibleName.Replace("<", "(Of ", StringComparison.InvariantCulture).Replace(">", ")", StringComparison.InvariantCulture))
        End Function

        Friend Function ConvertToType(_TypeString As String) As VBS.TypeSyntax
            Dim TypeString As String = _TypeString.Trim
            Dim IndexOf As Integer = TypeString.IndexOf("(Of ", StringComparison.InvariantCulture)
            If IndexOf >= 0 Then
                Dim Name As String = TypeString.Substring(0, IndexOf)
                TypeString = TypeString.Substring(IndexOf + 3)
                Dim IndexOfLastCloseParen As Integer = TypeString.LastIndexOf(")", StringComparison.InvariantCulture)
                TypeString = TypeString.Substring(0, IndexOfLastCloseParen)
                Dim TypeList As New List(Of VBS.TypeSyntax)
                Dim PossibleTypes As String = TypeString.Trim
                While PossibleTypes.Length > 0
                    Dim EndIndex As Integer
                    ' Type
                    EndIndex = PossibleTypes.IndexOf(",", StringComparison.InvariantCulture)
                    Dim FirstLessThan As Integer = PossibleTypes.IndexOf("(", StringComparison.InvariantCulture)
                    If EndIndex = -1 OrElse FirstLessThan = -1 Then
                        EndIndex = PossibleTypes.Length
                    ElseIf EndIndex > FirstLessThan Then
                        Dim OpenParenCount As Integer = 0
                        For i As Integer = FirstLessThan To PossibleTypes.Length - 1
                            Select Case PossibleTypes.Substring(i, 1)
                                Case "("
                                    OpenParenCount += 1
                                Case ")"
                                    OpenParenCount -= 1
                                    EndIndex = i + 1
                                Case ","
                                    If OpenParenCount = 0 Then
                                        EndIndex = i
                                        Exit For
                                    End If
                            End Select
                        Next
                    End If
                    TypeList.Add(ConvertToType(PossibleTypes.Substring(0, EndIndex).Trim))
                    If EndIndex + 1 < PossibleTypes.Length Then
                        PossibleTypes = PossibleTypes.Substring(EndIndex + 1).Trim
                    Else
                        Exit While
                    End If
                End While
                Dim TypeArguemntList As VBS.TypeArgumentListSyntax = VBFactory.TypeArgumentList(VBFactory.SeparatedList(TypeList))
                Return VBFactory.GenericName(Name, TypeArguemntList)
            End If
            Select Case TypeString.ToUpperInvariant
                Case "BYTE"
                    Return PredefinedTypeByte
                Case "SBYTE"
                    Return PredefinedTypeSByte
                Case "INT"
                    Return PredefinedTypeInteger
                Case "UINT"
                    Return PredefinedTypeUInteger
                Case "SHORT"
                    Return PredefinedTypeShort
                Case "USHORT"
                    Return PredefinedTypeUShort
                Case "LONG"
                    Return PredefinedTypeLong
                Case "ULONG"
                    Return PredefinedTypeULong
                Case "FLOAT"
                    Return PredefinedTypeSingle
                Case "DOUBLE"
                    Return PredefinedTypeDouble
                Case "CHAR"
                    Return PredefinedTypeChar
                Case "BOOL"
                    Return PredefinedTypeBoolean
                Case "OBJECT", "VAR"
                    Return PredefinedTypeObject
                Case "STRING"
                    Return PredefinedTypeString
                Case "DECIMAL"
                    Return PredefinedTypeDecimal
                Case "DATETIME"
                    Return PredefinedTypeDate
                Case "?", "_"
                    Return PredefinedTypeObject
                Case Else
                    Return VBFactory.ParseTypeName(AddBracketsIfRequired(TypeString.
                                                                             Replace("[", "(", StringComparison.InvariantCulture).
                                                                             Replace("]", ")", StringComparison.InvariantCulture)))
            End Select
        End Function

        Friend Function CovertStringToTupleType(TupleString As String) As TypeSyntax
            Dim TupleElements As New List(Of TupleElementSyntax)
            For Each t As String In TupleString.Substring(1, TupleString.Length - 2).Split(","c)
                Dim TuplePart() As String = t.Trim.Split(" "c)
                If TuplePart.Length = 1 Then
                    Dim typedTupleElementSyntax1 As TypedTupleElementSyntax = VBFactory.TypedTupleElement(ConvertToType(TuplePart(0)))
                    TupleElements.Add(typedTupleElementSyntax1)
                Else
                    Dim Identifier As SyntaxToken = CS.SyntaxFactory.Identifier(TuplePart(1))
                    Dim namedTupleElementSyntax1 As NamedTupleElementSyntax = VBFactory.NamedTupleElement(GenerateSafeVBToken(Identifier, IsQualifiedName:=False, IsTypeName:=False), VBFactory.SimpleAsClause(ConvertToType(TuplePart(0))))
                    TupleElements.Add(namedTupleElementSyntax1)
                End If
            Next
            Return VBFactory.TupleType(TupleElements.ToArray)
        End Function

        Friend Function IsSpecialReservedWord(ID As String) As Boolean
            If ID.Equals("Alias", StringComparison.InvariantCultureIgnoreCase) OrElse
                    ID.Equals("CType", StringComparison.InvariantCultureIgnoreCase) OrElse
                    ID.Equals("End", StringComparison.InvariantCultureIgnoreCase) OrElse
                    ID.Equals("Error", StringComparison.InvariantCultureIgnoreCase) OrElse
                    ID.Equals("Event", StringComparison.InvariantCultureIgnoreCase) OrElse
                    ID.Equals("Imports", StringComparison.InvariantCultureIgnoreCase) OrElse
                    ID.Equals("Module", StringComparison.InvariantCultureIgnoreCase) OrElse
                    ID.Equals("Option", StringComparison.InvariantCultureIgnoreCase) OrElse
                    ID.Equals("Optional", StringComparison.InvariantCultureIgnoreCase) Then
                Return True
            End If
            Return False
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
                Return MakeIdentifierUnique(id, BracketNeeded:=True, QualifiedNameOrTypeName:=IsQualifiedName)
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
                IsQualifiedName = If(MethodDeclaration IsNot Nothing AndAlso String.Compare(MethodDeclaration.Identifier.ValueText, id.ValueText, ignoreCase:=True, Globalization.CultureInfo.InvariantCulture) <> 0, False, True)
                IsQualifiedName = IsQualifiedName Or String.Compare(Param.Type.ToString, id.ValueText, ignoreCase:=False, Globalization.CultureInfo.InvariantCulture) = 0
            End If
            Return MakeIdentifierUnique(id, BracketNeeded:=False, QualifiedNameOrTypeName:=IsQualifiedName)
        End Function

    End Module
End Namespace
