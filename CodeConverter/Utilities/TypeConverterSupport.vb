' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports System.Text
Imports CSharpToVBConverter.ToVisualBasic

Imports Microsoft.CodeAnalysis
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic

Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBConverter

    Public Module TypeConverterSupport

        Private Const StrIDictionary As String = "IDictionary"

        Private Const StrIEnumerable As String = "IEnumerable"

        Private Const StrIEnumerableOf As String = "IEnumerable(Of "

        <Extension>
        Private Function ActionType(compilation As Compilation) As INamedTypeSymbol
            Return compilation.GetTypeByMetadataName(GetType(Action).FullName)
        End Function

        ''' <summary>
        ''' Extract String
        ''' </summary>
        ''' <param name="CSNamedTypeString">Source String</param>
        ''' <param name="CommaIndex">End index or -1 if no end</param>
        ''' <param name="CurrentIndex">Start of Substring</param>
        ''' <returns>Substring from StartIndex to CommaIndex or end if CommanIndex = -1</returns>
        <Extension>
        Private Function ExtractSafeName(CSNamedTypeString As String, CommaIndex As Integer, CurrentIndex As Integer) As String
            Dim length As Integer
            Dim start As Integer = CurrentIndex
            If CommaIndex < 0 Then
                length = CSNamedTypeString.Length - CurrentIndex - 1
                start = CurrentIndex + 1
            Else
                length = CommaIndex - CurrentIndex
            End If
            If length <= 0 Then
                Return ""
            End If
            Return MakeVBSafeName(CSNamedTypeString.Substring(start, length).Trim)
        End Function

        Private Function ExtractTupleWithName(SourceString As String, IncludeName As Boolean, typeStringBuilder As StringBuilder, ElementList As List(Of String), currentIndex As Integer) As Integer
            SourceString = SourceString.Substring(0, SourceString.Length - 1)
            If currentIndex < SourceString.Length - 1 AndAlso SourceString.Chars(currentIndex) <> "," Then
                currentIndex += 1
            End If
            Dim commaIndex As Integer = SourceString.IndexOf(",", currentIndex, StringComparison.OrdinalIgnoreCase)
            Dim typePart As String = ConvertToType(typeStringBuilder.ToString).ToString
            Dim name As String = ""
            If IncludeName Then
                name = SourceString.ExtractSafeName(commaIndex, currentIndex)
                currentIndex = If(commaIndex = -1, SourceString.Length - 1, commaIndex)
            End If
            If name.Any Then
                ElementList.Add($"{name} As {typePart}")
            Else
                ElementList.Add(typePart)
            End If
            Return currentIndex
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
        Friend Function ConvertITypeSymbolToType(_ITypeSymbol As ITypeSymbol) As VBS.TypeSyntax
            Dim type As VBS.TypeSyntax = _ITypeSymbol.ConvertToType
            If type.IsKind(VB.SyntaxKind.ArrayType) Then
                If DirectCast(_ITypeSymbol, IArrayTypeSymbol).ElementType.TypeKind = Microsoft.CodeAnalysis.TypeKind.Array Then
                    Return type.NormalizeWhitespace
                End If
                Dim elementType As VBS.TypeSyntax = DirectCast(type, VBS.ArrayTypeSyntax).ElementType
                If elementType.ToString.Any Then
                    Return elementType.NormalizeWhitespace
                End If
                Return Nothing
            End If
            If TypeOf type Is VBS.QualifiedNameSyntax Then
                Dim rightNode As VBS.SimpleNameSyntax = DirectCast(type, VBS.QualifiedNameSyntax).Right
                If TypeOf rightNode Is VBS.IdentifierNameSyntax Then
                    Return ConvertTypeITypeSymbolFromInterfaceToType(_ITypeSymbol)
                End If
                Dim genericName As VBS.GenericNameSyntax = DirectCast(rightNode, VBS.GenericNameSyntax)
                If genericName.TypeArgumentList.Arguments.Count = 1 Then
                    Return genericName.TypeArgumentList.Arguments(0)
                Else
                    Return Factory.ParseTypeName(genericName.TypeArgumentList.Arguments.ToString & ")")
                End If
            End If
            If TypeOf type Is VBS.GenericNameSyntax Then
                Dim genericdName As VBS.GenericNameSyntax = CType(type, VBS.GenericNameSyntax)
                If genericdName.TypeArgumentList.Arguments.Count = 1 Then
                    Return type
                Else
                    Return ConvertTypeITypeSymbolFromInterfaceToType(_ITypeSymbol)
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
                    returnType = Factory.ParseTypeName(MakeVBSafeName(typeString))
            End Select
            If isNullable Then
                Return Factory.NullableType(returnType)
            End If
            Return returnType
        End Function

        Friend Function ConvertStringToTypeArgumentList(TypeString As String) As VBS.TypeArgumentListSyntax
            Dim commaIndex As Integer
            Dim currentIndex As Integer
            Dim openParenCount As Integer = 0
            Dim possibleTypes As String = TypeString
            Dim typeList As New List(Of VBS.TypeSyntax)
            While possibleTypes.Any
                If possibleTypes.Chars(0) = "(" Then
                    Dim typeStrings As New List(Of String)
                    For currentIndex = 0 To possibleTypes.Length - 1
                        Select Case possibleTypes.Substring(currentIndex, 1)
                            Case "("
                                openParenCount += 1
                            Case ")"
                                openParenCount -= 1
                                commaIndex = currentIndex + 1
                            Case ","
                                If openParenCount = 0 Then
                                    commaIndex = currentIndex
                                    Exit For
                                End If
                        End Select
                    Next
                    typeList.Add(ConvertToType(possibleTypes.Substring(0, commaIndex)).WithLeadingTrivia(Factory.Space))
                    If commaIndex + 1 < possibleTypes.Length Then
                        possibleTypes = possibleTypes.Substring(commaIndex + 1).Trim
                    Else
                        Exit While
                    End If
                Else
                    While possibleTypes.Any
                        ' Type
                        commaIndex = possibleTypes.IndexOf(",", StringComparison.Ordinal)
                        Dim firstOpenParen As Integer = possibleTypes.IndexOf("(", StringComparison.Ordinal)
                        If commaIndex = -1 OrElse firstOpenParen = -1 Then
                            commaIndex = possibleTypes.Length
                        ElseIf commaIndex > firstOpenParen Then
                            openParenCount = 0
                            For currentIndex = firstOpenParen To possibleTypes.Length - 1
                                Select Case possibleTypes.Substring(currentIndex, 1)
                                    Case "("
                                        openParenCount += 1
                                    Case ")"
                                        openParenCount -= 1
                                        commaIndex = currentIndex + 1
                                    Case ","
                                        If openParenCount = 0 Then
                                            commaIndex = currentIndex
                                            Exit For
                                        End If
                                End Select
                            Next
                        End If
                        typeList.Add(ConvertToType(possibleTypes.Substring(0, commaIndex)).WithLeadingTrivia(Factory.Space))
                        If commaIndex + 1 < possibleTypes.Length Then
                            possibleTypes = possibleTypes.Substring(commaIndex + 1).Trim
                        Else
                            possibleTypes = ""
                            Exit While
                        End If
                    End While
                End If
            End While
            Dim typeArguemntList As VBS.TypeArgumentListSyntax = FactoryTypeArgumentList(typeList)
            Return FactoryTypeArgumentList(typeList)

        End Function

        <Extension>
        Friend Function ConvertToType(PossibleTupleType As ITypeSymbol) As VBS.TypeSyntax
            If PossibleTupleType.IsKind(SymbolKind.ArrayType) Then
                Dim elementType As VBS.TypeSyntax = DirectCast(PossibleTupleType, IArrayTypeSymbol).ElementType.ConvertToType
                If TypeOf elementType Is VBS.ArrayTypeSyntax Then
                    Return elementType
                End If
                Return Factory.ArrayType(elementType)
            End If
            If PossibleTupleType.IsTupleType Then
                Dim tupleElementList As New List(Of VBS.TupleElementSyntax)
                For Each tupleElement As IFieldSymbol In DirectCast(PossibleTupleType, INamedTypeSymbol).TupleElements
                    tupleElementList.Add(tupleElement.ConvertIFieldToTupleElement)
                Next
                Return Factory.TupleType(tupleElementList.ToArray)
            End If
            If PossibleTupleType.Name = "Tuple" Then
                Dim tupleElementList As New List(Of VBS.TypeSyntax)
                For Each tupleElement As ITypeSymbol In DirectCast(PossibleTupleType, INamedTypeSymbol).TypeArguments
                    tupleElementList.Add(tupleElement.ConvertToType)
                Next
                Return Factory.GenericName("Tuple", FactoryTypeArgumentList(tupleElementList))
            End If
            Return ConvertToType(PossibleTupleType.ToString)
        End Function

        Friend Function ConvertToType(TypeAsCSString As String, Optional AllowArray As Boolean = True) As VBS.TypeSyntax
            Dim typeString As String = TypeAsCSString.Trim
            Dim isNullable As Boolean = typeString.Last = "?"c
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
                    If AllowArray AndAlso isArray Then
                        arrayRank = typeString.Substring(indexOfBracket).
                                            Replace("[", "(", StringComparison.Ordinal).
                                            Replace("]", ")", StringComparison.Ordinal)
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
                            Case Else
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
                                If AllowArray Then
                                    baseType &= "[]"
                                End If
                                endIndex += 2
                            End While
                            typeList.Add(baseType.ConvertCSStringToName)
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
                ElseIf TypeAsCSString.Chars(0) = "(" Then
                    Return TypeAsCSString.ConvertCSStringToName
                End If
                retType = ConvertSimpleTypeToType(typeString)
                If arrayRank.Any Then
                    Factory.ParseTypeName($"{retType}{arrayRank}")
                End If
            Catch ex As Exception
                Stop
            End Try
            Return retType
        End Function

        <Extension>
        Friend Function ConvertCSStringToName(CSNamedTypeStringIn As String) As VBS.TypeSyntax
            Dim csNamedTypeString As String = CSNamedTypeStringIn
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

            Dim elementList As List(Of String) = csNamedTypeString.ConvertTypeTupleToTypeStrings(IncludeName:=True)
            Dim builder As New StringBuilder
            builder.Append("("c)
            For Each e As IndexClass(Of String) In elementList.WithIndex
                If e.IsLast Then Exit For
                builder.Append($"{e.Value}, ")
            Next
            builder.Append(elementList.Last & ")")
            Return Factory.ParseTypeName($"{builder}{If(isArray, "()", "")}{If(nullable, "?", "")}").WithLeadingTrivia(Factory.Space)
        End Function

        <Extension>
        Friend Function ConvertTypeITypeSymbolFromInterfaceToType(expressionConvertedType As ITypeSymbol) As VBS.TypeSyntax

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

        <Extension>
        Friend Function ConvertIFieldToTupleElement(TupleElement As IFieldSymbol) As VBS.TupleElementSyntax
            If TupleElement.Type Is Nothing Then
                Return Factory.NamedTupleElement(TupleElement.Name.ToString(Globalization.CultureInfo.InvariantCulture))
            End If
            Dim asClause As VBS.SimpleAsClauseSyntax = Factory.SimpleAsClause(AsKeyword.With(Factory.Space, Factory.Space), New SyntaxList(Of VBS.AttributeListSyntax), TupleElement.Type.ConvertToType)
            Return Factory.NamedTupleElement(Factory.Identifier(MakeVBSafeName(TupleElement.Name)), asClause)
        End Function

        <Extension>
        Friend Function ConvertTypeTupleToTypeStrings(CSharpNamedTypeString As String, IncludeName As Boolean) As List(Of String)
            If CSharpNamedTypeString(0) <> "(" OrElse
                    CSharpNamedTypeString.Last <> ")" Then
                Stop
            End If
            Dim currentChar As String
            Dim openLT As Integer
            Dim commaIndex As Integer
            Dim openParen As Integer
            Dim typeString As New StringBuilder
            Dim elementList As New List(Of String)

            For currentIndex As Integer = 1 To CSharpNamedTypeString.Length - 2
                currentChar = CSharpNamedTypeString(currentIndex)
                Select Case currentChar
                    Case "<"
                        openLT = 1
                        typeString.Append(currentChar)
                        While openLT <> 0
                            currentIndex += 1
                            currentChar = CSharpNamedTypeString(currentIndex)
                            Select Case currentChar
                                Case ">"
                                    openLT -= 1
                                    typeString.Append(">"c)
                                    If openLT = 0 Then
                                        Exit While
                                    End If
                                Case "<"
                                    openLT += 1
                                    typeString.Append("<"c)
                                Case "["
                                    typeString.Append("("c)
                                Case "]"
                                    typeString.Append(")"c)
                                Case Else
                                    typeString.Append(currentChar)
                            End Select
                        End While
                        If currentIndex + 1 >= CSharpNamedTypeString.Length - 1 OrElse Not ".[?".Contains(CSharpNamedTypeString(currentIndex + 1), StringComparison.OrdinalIgnoreCase) Then
                            currentIndex = ExtractTupleWithName(CSharpNamedTypeString, IncludeName, typeString, elementList, currentIndex)
                            commaIndex = CSharpNamedTypeString.IndexOf(",", currentIndex, StringComparison.OrdinalIgnoreCase)
                            typeString.Clear()
                            If commaIndex < 0 Then
                                Exit For
                            End If
                            currentIndex = commaIndex + 1
                        End If
                    Case " " ' variable name
                        currentIndex = ExtractTupleWithName(CSharpNamedTypeString, IncludeName, typeString, elementList, currentIndex)
                        commaIndex = CSharpNamedTypeString.IndexOf(",", currentIndex, StringComparison.OrdinalIgnoreCase)
                        typeString.Clear()
                        If commaIndex < 0 Then
                            Exit For
                        End If
                        currentIndex = commaIndex + 1
                    Case ","
                        elementList.Add(ConvertToType(typeString.ToString).ToString)
                        typeString.Clear()
                        currentIndex += If(CSharpNamedTypeString(currentIndex + 1) = " ", 1, 0)
                    Case ")"
                        currentIndex = ExtractTupleWithName(CSharpNamedTypeString, IncludeName, typeString, elementList, currentIndex)
                        Dim typePart As String = ConvertToType(typeString.ToString).ToString
                        If CSharpNamedTypeString.IndexOf(",", currentIndex + 1, StringComparison.OrdinalIgnoreCase) = -1 Then
                            Exit For
                        End If
                        Continue For
                    Case "("
                        Dim isSubTuple As Boolean = typeString.Length = 0
                        openParen += 1
                        typeString.Append("("c)
                        While openParen <> 0
                            currentIndex += 1
                            currentChar = CSharpNamedTypeString(currentIndex)
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
                                        currentIndex = ExtractTupleWithName(CSharpNamedTypeString, IncludeName, typeString, elementList, currentIndex)
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
                            subTupleList.AddRange(typeString.ToString.ConvertTypeTupleToTypeStrings(IncludeName))
                            elementList.Add($"({subTupleList.ToArray.JoinLines(", ")})")
                            typeString.Clear()
                        Else
                            elementList.Add(ConvertToType(typeString.ToString).ToString)
                            typeString.Clear()
                        End If
                        commaIndex = CSharpNamedTypeString.IndexOf(",", currentIndex + 1, StringComparison.OrdinalIgnoreCase)
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
        ''' <param name="Node"></param><param name="usedIdentifiers"></param>
        ''' <param name="Model"></param>
        Friend Function GenerateSafeVBToken(id As SyntaxToken, Node As CS.CSharpSyntaxNode, usedIdentifiers As Dictionary(Of String, SymbolTableEntry), Model As SemanticModel) As SyntaxToken
            Return GenerateSafeVBToken(id, Node, Model, usedIdentifiers, IsQualifiedName:=False, IsTypeName:=False)
        End Function

        ''' <summary>
        ''' Returns Safe VB Name
        ''' </summary>
        ''' <param name="id">Original Variable Name</param>
        ''' <param name="Node"></param>
        ''' <param name="Model"></param>
        ''' <param name="usedIdentifiers"></param>
        ''' <param name="IsQualifiedName">True if name is part of a Qualified Name and should not be renamed</param>
        ''' <returns></returns>
        ''' <param name="IsTypeName"></param>
        Friend Function GenerateSafeVBToken(id As SyntaxToken, Node As CS.CSharpSyntaxNode, Model As SemanticModel, usedIdentifiers As Dictionary(Of String, SymbolTableEntry), IsQualifiedName As Boolean, IsTypeName As Boolean) As SyntaxToken
            If Node Is Nothing Then
                Throw New ArgumentNullException(NameOf(Node))
            End If

            Dim keywordKind As VB.SyntaxKind = VB.SyntaxFacts.GetKeywordKind(id.ValueText)
            If IsTypeName Then
                IsQualifiedName = True
            Else
                If VB.SyntaxFacts.IsPredefinedType(keywordKind) Then
                    Return id.MakeIdentifierUnique(Node, usedIdentifiers, Model, IsBracketNeeded:=True, IsQualifiedNameOrTypeName:=IsQualifiedName)
                End If
            End If

            If VB.SyntaxFacts.IsKeywordKind(keywordKind) OrElse id.ValueText = "Yield" Then
                Dim bracketNeeded As Boolean = True
                If keywordKind.IsKind(VB.SyntaxKind.REMKeyword, VB.SyntaxKind.DelegateKeyword) OrElse id.Text.Chars(0) = "@" Then
                ElseIf id.Parent Is Nothing Then
                ElseIf TypeOf id.Parent.Parent Is CSS.MemberAccessExpressionSyntax Then
                    Dim memberAccessExpression As CSS.MemberAccessExpressionSyntax = CType(id.Parent?.Parent, CSS.MemberAccessExpressionSyntax)
                    If TypeOf memberAccessExpression.Expression IsNot CSS.GenericNameSyntax Then
                        bracketNeeded = memberAccessExpression.Expression.ToString.Equals(id.ToString, StringComparison.Ordinal)
                    End If
                ElseIf id.Parent.AncestorsAndSelf().OfType(Of CSS.UsingDirectiveSyntax).FirstOrDefault().IsKind(CS.SyntaxKind.UsingDirective) Then
                    id = Factory.Token(keywordKind).WithTriviaFrom(id)
                    bracketNeeded = False
                End If
                Return id.MakeIdentifierUnique(Node, usedIdentifiers, Model, bracketNeeded, IsQualifiedNameOrTypeName:=IsQualifiedName)
            End If

            If id.Parent?.IsParentKind(CS.SyntaxKind.Parameter) Then
                Dim param As CSS.ParameterSyntax = DirectCast(id.Parent.Parent, CSS.ParameterSyntax)
                Dim methodDeclaration As CSS.MethodDeclarationSyntax = TryCast(param.Parent?.Parent, CSS.MethodDeclarationSyntax)
                IsQualifiedName = methodDeclaration Is Nothing OrElse String.Compare(methodDeclaration.Identifier.ValueText, id.ValueText, ignoreCase:=True, Globalization.CultureInfo.InvariantCulture) = 0
                IsQualifiedName = IsQualifiedName Or String.Compare(param.Type.ToString, id.ValueText, ignoreCase:=False, Globalization.CultureInfo.InvariantCulture) = 0
            End If
            Return id.MakeIdentifierUnique(Node, usedIdentifiers, Model, IsBracketNeeded:=False, IsQualifiedNameOrTypeName:=IsQualifiedName)
        End Function

    End Module
End Namespace
