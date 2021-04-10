' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports System.Text
Imports Extensions
Imports Microsoft.CodeAnalysis
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic

Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax


Public Module TypeConverterSupport

    Private Const StrIDictionary As String = "IDictionary"

    Private Const StrIEnumerable As String = "IEnumerable"

    Private Const StrIEnumerableOf As String = "IEnumerable(Of "

    <Extension>
    Private Function ActionType(compilation As Compilation) As INamedTypeSymbol
        Return compilation.GetTypeByMetadataName(GetType(Action).FullName)
    End Function

    <Extension>
    Private Function ConvertIFieldToTupleElement(tupleElement As IFieldSymbol) As VBS.TupleElementSyntax
        If tupleElement.Type Is Nothing Then
            Return Factory.NamedTupleElement(tupleElement.Name.ToString(Globalization.CultureInfo.InvariantCulture))
        End If
        Dim asClause As VBS.SimpleAsClauseSyntax = Factory.SimpleAsClause(AsKeyword.With(SpaceTrivia, SpaceTrivia), New SyntaxList(Of VBS.AttributeListSyntax), tupleElement.Type.ConvertToType)
        Return Factory.NamedTupleElement(Factory.Identifier(MakeVbSafeName(tupleElement.Name)), asClause)
    End Function

    <Extension>
    Private Function ConvertTypeITypeSymbolFromInterfaceToType(expressionConvertedType As ITypeSymbol) As VBS.TypeSyntax

        If Not expressionConvertedType.AllInterfaces.Any Then
            If expressionConvertedType.ToString.EndsWith("IArityEnumerable", StringComparison.Ordinal) Then
                Return PredefinedTypeInteger
            End If
            Return PredefinedTypeObject
        End If
        For Each namedType As INamedTypeSymbol In expressionConvertedType.AllInterfaces
            Dim index As Integer = namedType.ToString.IndexOf(StrIEnumerableOf, StringComparison.Ordinal)
            Dim newType As String
            If index > 0 Then
                newType = namedType.ToString.Substring(index + StrIEnumerableOf.Length)
                Return Factory.ParseName(newType)
            End If
            index = namedType.ToString.IndexOf(StrIDictionary, StringComparison.Ordinal)
            If index > 0 Then
                Return namedType.ConvertToType
            End If
            index = namedType.ToString.IndexOf(StrIEnumerable, StringComparison.Ordinal)
            If index > 0 Then
                Return namedType.ConvertToType
            End If
        Next

        Dim index1 As Integer = expressionConvertedType.ToString.IndexOf(StrIEnumerableOf, StringComparison.Ordinal)
        If index1 > 0 Then
            Return Factory.ParseName(expressionConvertedType.ToString.Substring(index1 + StrIEnumerableOf.Length))
        End If
        Return Nothing
    End Function

    ''' <summary>
    ''' Extract String
    ''' </summary>
    ''' <param name="csNamedTypeString">Source String</param>
    ''' <param name="commaIndex">End index or -1 if no end</param>
    ''' <param name="currentIndex">Start of Substring</param>
    ''' <returns>Substring from StartIndex to CommaIndex or end if CommandIndex = -1</returns>
    <Extension>
    Private Function ExtractSafeName(csNamedTypeString As String, commaIndex As Integer, currentIndex As Integer) As String
        Dim length As Integer
        Dim start As Integer = currentIndex
        If commaIndex < 0 Then
            length = csNamedTypeString.Length - currentIndex - 1
            start = currentIndex + 1
        Else
            length = commaIndex - currentIndex
        End If
        If length <= 0 Then
            Return ""
        End If
        Return MakeVbSafeName(csNamedTypeString.Substring(start, length).Trim)
    End Function

    Private Function ExtractTupleWithName(sourceString As String, includeName As Boolean, typeStringBuilder As StringBuilder, elementList As List(Of String), currentIndex As Integer) As Integer
        sourceString = sourceString.Substring(0, sourceString.Length - 1)
        If currentIndex < sourceString.Length - 1 AndAlso sourceString.Chars(currentIndex) <> "," Then
            currentIndex += 1
        End If
        Dim commaIndex As Integer = sourceString.IndexOf(",", currentIndex, StringComparison.OrdinalIgnoreCase)
        Dim typePart As String = ConvertToType(typeStringBuilder.ToString).ToString
        Dim name As String = ""
        If includeName Then
            name = sourceString.ExtractSafeName(commaIndex, currentIndex)
            currentIndex = If(commaIndex = -1, sourceString.Length - 1, commaIndex)
        End If
        If name.Any Then
            elementList.Add($"{name} As {typePart}")
        Else
            elementList.Add(typePart)
        End If
        Return currentIndex
    End Function

    <Extension>
    Private Function IsKind(kind As VB.SyntaxKind, ParamArray kinds() As VB.SyntaxKind) As Boolean
        Return kinds.Contains(kind)
    End Function

    <Extension>
    Friend Function ConvertCsStringToName(csNamedTypeStringIn As String) As VBS.TypeSyntax
        Dim csNamedTypeString As String = csNamedTypeStringIn
        Dim isArray As Boolean = False
        Dim nullable As Boolean = False
        If csNamedTypeString.EndsWith("?", StringComparison.Ordinal) Then
            nullable = True
            csNamedTypeString = csNamedTypeString.Substring(0, csNamedTypeString.Length - 1).Trim
        End If
        If csNamedTypeString.EndsWith("[]", StringComparison.Ordinal) Then
            isArray = True
            csNamedTypeString = csNamedTypeString.Substring(0, csNamedTypeString.Length - 2).Trim
        End If

        Dim elementList As List(Of String) = csNamedTypeString.ConvertTypeTupleToTypeStrings(includeName:=True)
        Dim builder As New StringBuilder
        builder.Append("("c)
        For Each e As IndexClass(Of String) In elementList.WithIndex
            If e.IsLast Then Exit For
            builder.Append($"{e.Value}, ")
        Next
        builder.Append(elementList.Last & ")")
        Return Factory.ParseTypeName($"{builder}{If(isArray, "()", "")}{If(nullable, "?", "")}").WithLeadingTrivia(SpaceTrivia)
    End Function

    <Extension>
    Friend Function ConvertISymbolToType(symbol As ISymbol, compilation As Compilation, Optional extensionUsedAsInstance As Boolean = False) As ITypeSymbol
        If compilation Is Nothing Then
            Throw New ArgumentNullException(NameOf(compilation))
        End If
        Dim type As ITypeSymbol = TryCast(symbol, ITypeSymbol)
        If type IsNot Nothing Then
            Return type
        End If
        Dim method As IMethodSymbol = TryCast(symbol, IMethodSymbol)
        If method Is Nothing OrElse method.Parameters.Any(Function(p As IParameterSymbol) p.RefKind <> RefKind.None) Then
            ' Otherwise, just default to object.
            Return compilation.ObjectType
        End If
        ' Convert the symbol to Func<...> or Action<...>
        If method.ReturnsVoid Then
            Dim count As Integer = If(extensionUsedAsInstance, method.Parameters.Length - 1, method.Parameters.Length)
            Dim skip As Integer = If(extensionUsedAsInstance, 1, 0)
            count = Math.Max(0, count)
            If count = 0 Then
                ' Action
                Return compilation.ActionType()
            Else
                ' Action<TArg1, ..., TArgN>
                Dim actionName As String = "System.Action`" & count
                Dim actionType As INamedTypeSymbol = compilation.GetTypeByMetadataName(actionName)

                If actionType IsNot Nothing Then
                    Dim types() As ITypeSymbol = method.Parameters.
                Skip(skip).
                Select(Function(p As IParameterSymbol) If(p.Type, compilation.GetSpecialType(SpecialType.System_Object))).
                ToArray()
                    Return actionType.Construct(types)
                End If
            End If
        Else
            ' Func<TArg1,...,TArgN,TReturn>
            '
            ' +1 for the return type.
            Dim count As Integer = If(extensionUsedAsInstance, method.Parameters.Length - 1, method.Parameters.Length)
            Dim skip As Integer = If(extensionUsedAsInstance, 1, 0)
            Dim functionName As String = "System.Func`" & (count + 1)
            Dim functionType As INamedTypeSymbol = compilation.GetTypeByMetadataName(functionName)

            If functionType IsNot Nothing Then
                Try
                    Dim csTypes() As ITypeSymbol = method.Parameters.
                Skip(skip).Select(Function(p As IParameterSymbol) If(p.Type.IsErrorType, compilation.GetSpecialType(SpecialType.System_Object), p.Type)).
                Concat({method.ReturnType}).
                Select(Function(t As ITypeSymbol) If(t Is Nothing OrElse t.IsErrorType, compilation.GetSpecialType(SpecialType.System_Object), t)).
                ToArray()
                    Return functionType.Construct(csTypes)
                Catch ex As OperationCanceledException
                    Throw
                Catch ex As Exception
                    Stop
                End Try
            End If
        End If

        ' Otherwise, just default to object.
        Return compilation.ObjectType
    End Function

    <Extension>
    Friend Function ConvertITypeSymbolToType(iTypeSymbol As ITypeSymbol) As VBS.TypeSyntax
        Dim type As VBS.TypeSyntax = iTypeSymbol.ConvertToType
        If type.IsKind(VB.SyntaxKind.ArrayType) Then
            If DirectCast(iTypeSymbol, IArrayTypeSymbol).ElementType.TypeKind = TypeKind.Array Then
                Return type.NormalizeWhitespace
            End If
            Dim elementType As VBS.TypeSyntax = DirectCast(type, VBS.ArrayTypeSyntax).ElementType
            If elementType.ToString.Any Then
                Return elementType.NormalizeWhitespace
            End If
            Return Nothing
        End If
        Dim qualifiedNameSyntax As VBS.QualifiedNameSyntax = TryCast(type, VBS.QualifiedNameSyntax)
        Dim genericName As VBS.GenericNameSyntax
        If qualifiedNameSyntax IsNot Nothing Then
            Dim rightNode As VBS.SimpleNameSyntax = qualifiedNameSyntax.Right
            If TypeOf rightNode Is VBS.IdentifierNameSyntax Then
                Return iTypeSymbol.ConvertTypeITypeSymbolFromInterfaceToType()
            End If
            genericName = DirectCast(rightNode, VBS.GenericNameSyntax)
            If genericName.TypeArgumentList.Arguments.Count = 1 Then
                Return genericName.TypeArgumentList.Arguments(0)
            Else
                Return Factory.ParseTypeName(genericName.TypeArgumentList.Arguments.ToString & ")")
            End If
        End If
        genericName = TryCast(type, VBS.GenericNameSyntax)
        If genericName IsNot Nothing Then
            If genericName.TypeArgumentList.Arguments.Count = 1 Then
                Return type
            Else
                Return iTypeSymbol.ConvertTypeITypeSymbolFromInterfaceToType()
            End If
        End If
        If TypeOf type Is VBS.IdentifierNameSyntax Then
            Return Factory.ParseTypeName(type.ToString)
        End If
        Return Nothing
    End Function

    Friend Function ConvertSimpleTypeToType(typeString As String) As VBS.TypeSyntax
        Dim isNullable As Boolean = typeString.Length > 1 AndAlso typeString.EndsWith("?", StringComparison.InvariantCultureIgnoreCase)
        typeString = typeString.TrimEnd("?"c)
        Dim returnType As VBS.TypeSyntax
        Select Case typeString.ToUpperInvariant.TrimEnd("?"c)
            Case "OBJECT", ""
                returnType = PredefinedTypeObject
            Case "BOOL", "BOOLEAN"
                returnType = PredefinedTypeBoolean
            Case "BYTE"
                returnType = PredefinedTypeByte
            Case "CHAR"
                returnType = PredefinedTypeChar
            Case "DECIMAL"
                returnType = PredefinedTypeDecimal
            Case "DOUBLE"
                returnType = PredefinedTypeDouble
            Case "INT", "Integer"
                returnType = PredefinedTypeInteger
            Case "SBYTE"
                returnType = PredefinedTypeSByte
            Case "SHORT"
                returnType = PredefinedTypeShort
            Case "UINT"
                returnType = PredefinedTypeUInteger
            Case "ULONG"
                returnType = PredefinedTypeULong
            Case "USHORT"
                returnType = PredefinedTypeUShort
            Case "STRING"
                returnType = PredefinedTypeString
            Case Else
                If typeString.EndsWith("*", StringComparison.OrdinalIgnoreCase) Then
                    Return IntPtrType
                End If
                isNullable = False
                returnType = Factory.ParseTypeName(MakeVbSafeName(typeString))
        End Select
        If isNullable Then
            Return Factory.NullableType(returnType)
        End If
        Return returnType
    End Function

    <Extension>
    Friend Function ConvertToType(possibleTupleType As ITypeSymbol) As VBS.TypeSyntax
        If possibleTupleType.IsKind(SymbolKind.ArrayType) Then
            Dim elementType As VBS.TypeSyntax = DirectCast(possibleTupleType, IArrayTypeSymbol).ElementType.ConvertToType
            If TypeOf elementType Is VBS.ArrayTypeSyntax Then
                Return elementType
            End If
            Return Factory.ArrayType(elementType)
        End If
        If possibleTupleType.IsTupleType Then
            Dim tupleElementList As New List(Of VBS.TupleElementSyntax)
            For Each tupleElement As IFieldSymbol In DirectCast(possibleTupleType, INamedTypeSymbol).TupleElements
                tupleElementList.Add(tupleElement.ConvertIFieldToTupleElement)
            Next
            Return Factory.TupleType(tupleElementList.ToArray)
        End If
        If possibleTupleType.Name = "Tuple" Then
            Dim tupleElementList As New List(Of VBS.TypeSyntax)
            For Each tupleElement As ITypeSymbol In DirectCast(possibleTupleType, INamedTypeSymbol).TypeArguments
                tupleElementList.Add(tupleElement.ConvertToType)
            Next
            Return Factory.GenericName("Tuple", FactoryTypeArgumentList(tupleElementList))
        End If
        Return ConvertToType(possibleTupleType.ToString)
    End Function

    Friend Function ConvertToType(typeAsCsString As String, Optional allowArray As Boolean = True) As VBS.TypeSyntax
        Dim typeString As String = typeAsCsString.Trim
        Dim isNullable As Boolean = typeString.Last = "?"c AndAlso Not s_referenceTypes.Contains(typeString.RemoveBrackets.TrimEnd("?"c), StringComparer.OrdinalIgnoreCase)
        If Not isNullable Then
            typeString = typeString.TrimEnd("?"c)
        End If
        Dim retType As VBS.TypeSyntax = PredefinedTypeObject
        Try
            Dim arrayRank As String = ""
            Dim isArray As Boolean = typeString.EndsWith("]", StringComparison.OrdinalIgnoreCase)

            If isArray Then
                Dim indexOfBracket As Integer
                Dim foundClosedBracket As Boolean = True
                For indexOfBracket = typeString.Length - 2 To 0 Step -1
                    Select Case typeString.Chars(indexOfBracket)
                        Case "]"c
                            foundClosedBracket = True
                        Case "["c
                            foundClosedBracket = False
                        Case Else
                            If Not foundClosedBracket Then
                                indexOfBracket += 1
                                Exit For
                            End If
                    End Select
                Next
                If allowArray AndAlso isArray Then
                    arrayRank = typeString.Substring(indexOfBracket).
                                        Replace("[", "(", StringComparison.Ordinal).
                                        Replace("]", ")", StringComparison.Ordinal).
                                        Replace("*", "", StringComparison.Ordinal)
                End If
                typeString = typeString.Substring(0, indexOfBracket)
            End If

            If typeString.Contains("<", StringComparison.Ordinal) Then
                typeString = typeString.Replace("<", "(Of ", StringComparison.OrdinalIgnoreCase).
                                        Replace(">", ")", StringComparison.OrdinalIgnoreCase)
            End If
            Dim indexOf As Integer = typeString.IndexOf("(Of ", StringComparison.OrdinalIgnoreCase)
            If indexOf >= 0 Then
                If typeString.StartsWith("System.", StringComparison.Ordinal) Then
                    typeString = typeString.WithoutLeadingSystemDot
                    indexOf -= 7
                End If
                Dim name As String = typeString.Substring(0, length:=indexOf)
                typeString = typeString.Substring(indexOf + 3).Trim
                Dim openParenCount As Integer
                Dim indexOfTypeEnd As Integer
                openParenCount = 1
                For indexOfTypeEnd = 0 To typeString.Length - 1
                    Select Case typeString.Chars(indexOfTypeEnd)
                        Case "("c
                            openParenCount += 1
                        Case ")"c
                            openParenCount -= 1
                            If openParenCount = 0 Then
                                Exit For
                            End If
                    End Select
                Next
                Dim possibleTypes As String = typeString.Substring(0, indexOfTypeEnd).Trim
                If indexOfTypeEnd < typeString.Length - 2 Then
                    If typeString.Chars(indexOfTypeEnd + 1) = "." Then
                        typeString = typeString.Substring(indexOfTypeEnd + 2)
                    End If
                End If
                Dim typeList As New List(Of VBS.TypeSyntax)
                While possibleTypes.Any
                    Dim endIndex As Integer = 0
                    openParenCount = 0
                    If possibleTypes.Chars(0) = "(" Then
                        For currentIndex As Integer = 0 To possibleTypes.Length - 1
                            Select Case possibleTypes.Chars(currentIndex)
                                Case "("c
                                    openParenCount += 1
                                Case ")"c
                                    openParenCount -= 1
                                    endIndex = currentIndex + 1
                                    If openParenCount = 0 Then
                                        Exit For
                                    End If
                            End Select
                        Next
                        Dim baseType As String = possibleTypes.Substring(0, endIndex)
                        While endIndex + 1 < possibleTypes.Length AndAlso possibleTypes.Chars(endIndex) = "["
                            If allowArray Then
                                baseType &= "[]"
                            End If
                            endIndex += 2
                        End While
                        typeList.Add(baseType.ConvertCsStringToName)
                        possibleTypes = possibleTypes.Substring(endIndex).Trim
                        If possibleTypes.Any Then
                            possibleTypes = possibleTypes.TrimStart(","c).Trim
                            Continue While
                        Else
                            Exit While
                        End If
                    End If

                    endIndex = possibleTypes.IndexOf(",", StringComparison.Ordinal)
                    Dim firstLessThan As Integer = possibleTypes.IndexOf("(", StringComparison.Ordinal)
                    If endIndex = -1 AndAlso firstLessThan = -1 Then
                        endIndex = possibleTypes.Length
                    ElseIf endIndex = -1 OrElse endIndex > firstLessThan Then
                        firstLessThan = 0
                        Dim currentIndex As Integer
                        For currentIndex = firstLessThan To possibleTypes.Length - 1
                            Select Case possibleTypes.Chars(currentIndex)
                                Case "("c, "["c
                                    openParenCount += 1
                                Case "]"c, ")"c
                                    openParenCount -= 1
                                    If currentIndex = possibleTypes.Length - 1 Then
                                        endIndex = currentIndex + 1
                                    End If
                                Case ","c
                                    If openParenCount = 0 Then
                                        endIndex = currentIndex
                                        Exit For
                                    End If
                            End Select
                        Next
                        If endIndex = -1 OrElse currentIndex > endIndex Then
                            endIndex = currentIndex
                        End If
                    End If
                    Dim argument As String = possibleTypes.Substring(0, endIndex)

                    typeList.Add(ConvertToType(argument))
                    If endIndex + 1 < possibleTypes.Length Then
                        possibleTypes = possibleTypes.Substring(endIndex + 1).Trim
                    Else
                        Exit While
                    End If
                End While

                Dim genericNameSyntax As VBS.GenericNameSyntax = Factory.GenericName(name, FactoryTypeArgumentList(typeList))
                If typeString.Any Then

                End If
                If isNullable Then
                    Return Factory.NullableType(genericNameSyntax)
                End If
                Return genericNameSyntax
            ElseIf typeAsCsString.Chars(0) = "(" Then
                Return typeAsCsString.ConvertCsStringToName
            End If
            retType = ConvertSimpleTypeToType(typeString)
            If arrayRank.Any Then
                retType = Factory.ParseTypeName($"{retType}{arrayRank}")
            End If
        Catch ex As Exception
        End Try
        Return retType
    End Function

    <Extension>
    Friend Function ConvertTypeTupleToTypeStrings(cSharpNamedTypeString As String, includeName As Boolean) As List(Of String)
        If cSharpNamedTypeString(0) <> "(" OrElse
                cSharpNamedTypeString.Last <> ")" Then
            Stop
        End If
        Dim currentChar As String
        Dim openLt As Integer
        Dim commaIndex As Integer
        Dim openParen As Integer
        Dim typeString As New StringBuilder
        Dim elementList As New List(Of String)

        For currentIndex As Integer = 1 To cSharpNamedTypeString.Length - 2
            currentChar = cSharpNamedTypeString(currentIndex)
            Select Case currentChar
                Case "<"
                    openLt = 1
                    typeString.Append(currentChar)
                    While openLt <> 0
                        currentIndex += 1
                        currentChar = cSharpNamedTypeString(currentIndex)
                        Select Case currentChar
                            Case ">"
                                openLt -= 1
                                typeString.Append(">"c)
                                If openLt = 0 Then
                                    Exit While
                                End If
                            Case "<"
                                openLt += 1
                                typeString.Append("<"c)
                            Case "["
                                typeString.Append("("c)
                            Case "]"
                                typeString.Append(")"c)
                            Case Else
                                typeString.Append(currentChar)
                        End Select
                    End While
                    If currentIndex + 1 >= cSharpNamedTypeString.Length - 1 OrElse Not ".[?".Contains(cSharpNamedTypeString(currentIndex + 1), StringComparison.OrdinalIgnoreCase) Then
                        currentIndex = ExtractTupleWithName(cSharpNamedTypeString, includeName, typeString, elementList, currentIndex)
                        commaIndex = cSharpNamedTypeString.IndexOf(",", currentIndex, StringComparison.OrdinalIgnoreCase)
                        typeString.Clear()
                        If commaIndex < 0 Then
                            Exit For
                        End If
                        ' ReSharper disable once RedundantAssignment
                        currentIndex = commaIndex + 1
                    End If
                Case " " ' variable name
                    currentIndex = ExtractTupleWithName(cSharpNamedTypeString, includeName, typeString, elementList, currentIndex)
                    commaIndex = cSharpNamedTypeString.IndexOf(",", currentIndex, StringComparison.OrdinalIgnoreCase)
                    typeString.Clear()
                    If commaIndex < 0 Then
                        Exit For
                    End If
                    ' ReSharper disable once RedundantAssignment
                    currentIndex = commaIndex + 1
                Case ","
                    elementList.Add(ConvertToType(typeString.ToString).ToString)
                    typeString.Clear()
                    ' ReSharper disable once RedundantAssignment
                    currentIndex += If(cSharpNamedTypeString(currentIndex + 1) = " ", 1, 0)
                Case ")"
                    currentIndex = ExtractTupleWithName(cSharpNamedTypeString, includeName, typeString, elementList, currentIndex)
                    If cSharpNamedTypeString.IndexOf(",", currentIndex + 1, StringComparison.OrdinalIgnoreCase) = -1 Then
                        Exit For
                    End If
                    Continue For
                Case "("
                    Dim isSubTuple As Boolean = typeString.Length = 0
                    openParen += 1
                    typeString.Append("("c)
                    While openParen <> 0
                        currentIndex += 1
                        currentChar = cSharpNamedTypeString(currentIndex)
                        Select Case currentChar
                            Case ")"
                                openParen -= 1
                                typeString.Append(")"c)
                                If openParen = 0 Then
                                    Exit While
                                End If
                            Case " " ' variable name
                                If openParen > 0 Then
                                    typeString.Append(currentChar)
                                Else
                                    currentIndex = ExtractTupleWithName(cSharpNamedTypeString, includeName, typeString, elementList, currentIndex)
                                End If
                            Case "("
                                openParen += 1
                                typeString.Append("("c)
                            Case Else
                                typeString.Append(currentChar)
                        End Select
                    End While
                    If isSubTuple Then
                        Dim subTupleList As New List(Of String)
                        subTupleList.AddRange(typeString.ToString.ConvertTypeTupleToTypeStrings(includeName))
                        elementList.Add($"({subTupleList.ToArray.JoinLines(", ")})")
                        typeString.Clear()
                    Else
                        elementList.Add(ConvertToType(typeString.ToString).ToString)
                        typeString.Clear()
                    End If
                    commaIndex = cSharpNamedTypeString.IndexOf(",", currentIndex + 1, StringComparison.OrdinalIgnoreCase)
                    ' ReSharper disable once RedundantAssignment
                    currentIndex += 2
                    If commaIndex < 0 Then
                        Exit For
                    End If
                Case Else
                    typeString.Append(currentChar)
            End Select
        Next
        If typeString.Length > 0 Then
            elementList.Add(ConvertToType(typeString.ToString).ToString)
        End If
        Return elementList
    End Function

    ''' <summary>
    ''' Returns Safe VB Name with QualifiedName and TypeName both false
    ''' </summary>
    ''' <param name="id"></param>
    ''' <returns></returns>
    ''' <param name="node"></param><param name="model"></param>
    ''' <param name="usedIdentifiers"></param>
    Friend Function GenerateSafeVbToken(id As SyntaxToken, node As CS.CSharpSyntaxNode, model As SemanticModel, usedIdentifiers As Dictionary(Of String, SymbolTableEntry)) As SyntaxToken
        Return GenerateSafeVbToken(id, node, model, usedIdentifiers, isQualifiedName:=False, isTypeName:=False)
    End Function

    ''' <summary>
    ''' Returns Safe VB Name
    ''' </summary>
    ''' <param name="id">Original Variable Name</param>
    ''' <param name="node"></param>
    ''' <param name="model"></param>
    ''' <param name="usedIdentifiers"></param>
    ''' <param name="isQualifiedName">True if name is part of a Qualified Name and should not be renamed</param>
    ''' <returns></returns>
    ''' <param name="isTypeName"></param>
    Friend Function GenerateSafeVbToken(id As SyntaxToken, node As CS.CSharpSyntaxNode, model As SemanticModel, usedIdentifiers As Dictionary(Of String, SymbolTableEntry), isQualifiedName As Boolean, isTypeName As Boolean) As SyntaxToken
        If node Is Nothing Then
            Throw New ArgumentNullException(NameOf(node))
        End If

        Dim keywordKind As VB.SyntaxKind = VB.SyntaxFacts.GetKeywordKind(id.ValueText)
        If isTypeName Then
            isQualifiedName = True
        Else
            If VB.SyntaxFacts.IsPredefinedType(keywordKind) Then
                Return id.MakeIdentifierUnique(node, usedIdentifiers, model, isBracketNeeded:=True, isQualifiedNameOrTypeName:=isQualifiedName)
            End If
        End If

        Dim idParent As SyntaxNode = id.Parent
        If VB.SyntaxFacts.IsKeywordKind(keywordKind) OrElse id.ValueText = "Yield" Then
            Dim bracketNeeded As Boolean = True
            If idParent Is Nothing OrElse
               keywordKind.IsKind(VB.SyntaxKind.REMKeyword, VB.SyntaxKind.DelegateKeyword) OrElse
               id.Text.Chars(0) = "@" Then
                ' Do nothing
            ElseIf TypeOf idParent.Parent Is CSS.MemberAccessExpressionSyntax Then
                Dim memberAccessExpression As CSS.MemberAccessExpressionSyntax = CType(idParent?.Parent, CSS.MemberAccessExpressionSyntax)
                If TypeOf memberAccessExpression.Expression IsNot CSS.GenericNameSyntax Then
                    bracketNeeded = memberAccessExpression.Expression.ToString.Equals(id.ToString, StringComparison.Ordinal)
                End If
                ' ReSharper disable once VBUseFirstInstead
            ElseIf idParent.AncestorsAndSelf().OfType(Of CSS.UsingDirectiveSyntax).FirstOrDefault().IsKind(CS.SyntaxKind.UsingDirective) Then
                id = Factory.Token(keywordKind).WithTriviaFrom(id)
                bracketNeeded = False
            End If
            Return id.MakeIdentifierUnique(node, usedIdentifiers, model, bracketNeeded, isQualifiedNameOrTypeName:=isQualifiedName)
        End If

        If (Not isQualifiedName) AndAlso idParent?.IsParentKind(CS.SyntaxKind.Parameter) Then
            Dim param As CSS.ParameterSyntax = DirectCast(idParent.Parent, CSS.ParameterSyntax)
            Dim methodDeclaration As CSS.MethodDeclarationSyntax = TryCast(param.Parent?.Parent, CSS.MethodDeclarationSyntax)
            isQualifiedName = methodDeclaration Is Nothing OrElse
                              String.Compare(methodDeclaration.Identifier.ValueText, id.ValueText,
                                             ignoreCase:=True, Globalization.CultureInfo.InvariantCulture) = 0 OrElse
                              String.Compare(param.Type.ToString, id.ValueText, ignoreCase:=False,
                                             Globalization.CultureInfo.InvariantCulture) = 0
        End If
        Return id.MakeIdentifierUnique(node, usedIdentifiers, model, isBracketNeeded:=False, isQualifiedNameOrTypeName:=isQualifiedName)
    End Function

End Module
