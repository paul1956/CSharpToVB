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
        ''' <param name="CommaIndex">End Index or -1 if no end</param>
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
            Dim TypePart As String = ConvertToType(typeStringBuilder.ToString).ToString
            Dim name As String = ""
            If IncludeName Then
                name = SourceString.ExtractSafeName(commaIndex, currentIndex)
                currentIndex = If(commaIndex = -1, SourceString.Length - 1, commaIndex)
            End If
            If name.Any Then
                ElementList.Add($"{name} As {TypePart}")
            Else
                ElementList.Add(TypePart)
            End If
            Return currentIndex
        End Function

        <Extension>
        Friend Function ConvertISymbolToType(symbol As ISymbol, compilation As Compilation, Optional extensionUsedAsInstance As Boolean = False) As ITypeSymbol
            If compilation Is Nothing Then
                Throw New ArgumentNullException(NameOf(compilation))
            End If
            Dim _type As ITypeSymbol = TryCast(symbol, ITypeSymbol)
            If _type IsNot Nothing Then
                Return _type
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
                    Dim _ActionType As INamedTypeSymbol = compilation.GetTypeByMetadataName(actionName)

                    If _ActionType IsNot Nothing Then
                        Dim types() As ITypeSymbol = method.Parameters.
                    Skip(skip).
                    Select(Function(p As IParameterSymbol) If(p.Type, compilation.GetSpecialType(SpecialType.System_Object))).
                    ToArray()
                        Return _ActionType.Construct(types)
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
                        Dim CSharpTypes() As ITypeSymbol = method.Parameters.
                    Skip(skip).Select(Function(p As IParameterSymbol) If(p.Type.IsErrorType, compilation.GetSpecialType(SpecialType.System_Object), p.Type)).
                    Concat({method.ReturnType}).
                    Select(Function(t As ITypeSymbol) If(t Is Nothing OrElse t.IsErrorType, compilation.GetSpecialType(SpecialType.System_Object), t)).
                    ToArray()
                        Return functionType.Construct(CSharpTypes)
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
            Dim _TypeSyntax As VBS.TypeSyntax = _ITypeSymbol.ConvertToType
            If _TypeSyntax.IsKind(VB.SyntaxKind.ArrayType) Then
                If DirectCast(_ITypeSymbol, IArrayTypeSymbol).ElementType.TypeKind = Microsoft.CodeAnalysis.TypeKind.Array Then
                    Return _TypeSyntax.NormalizeWhitespace
                End If
                Dim elementType As VBS.TypeSyntax = DirectCast(_TypeSyntax, VBS.ArrayTypeSyntax).ElementType
                If elementType.ToString.Any Then
                    Return elementType.NormalizeWhitespace
                End If
                Return Nothing
            End If
            If TypeOf _TypeSyntax Is VBS.QualifiedNameSyntax Then
                Dim Right As VBS.SimpleNameSyntax = DirectCast(_TypeSyntax, VBS.QualifiedNameSyntax).Right
                If TypeOf Right Is VBS.IdentifierNameSyntax Then
                    Dim TypeSyntax As VBS.TypeSyntax = ConvertTypeITypeSymbolFromInterfaceToType(_ITypeSymbol)
                    Return TypeSyntax
                End If
                Dim GenericdName As VBS.GenericNameSyntax = DirectCast(Right, VBS.GenericNameSyntax)
                If GenericdName.TypeArgumentList.Arguments.Count = 1 Then
                    Return GenericdName.TypeArgumentList.Arguments(0)
                Else
                    Return Factory.ParseTypeName(GenericdName.TypeArgumentList.Arguments.ToString & ")")
                End If
            End If
            If TypeOf _TypeSyntax Is VBS.GenericNameSyntax Then
                Dim GenericdName As VBS.GenericNameSyntax = CType(_TypeSyntax, VBS.GenericNameSyntax)
                If GenericdName.TypeArgumentList.Arguments.Count = 1 Then
                    Return _TypeSyntax
                Else
                    Return ConvertTypeITypeSymbolFromInterfaceToType(_ITypeSymbol)
                End If
            End If
            If TypeOf _TypeSyntax Is VBS.IdentifierNameSyntax Then
                Return Factory.ParseTypeName(_TypeSyntax.ToString)
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
            Dim PossibleTypes As String = TypeString
            Dim TypeList As New List(Of VBS.TypeSyntax)
            While PossibleTypes.Any
                If PossibleTypes.Chars(0) = "(" Then
                    Dim typeStrings As New List(Of String)
                    For currentIndex = 0 To PossibleTypes.Length - 1
                        Select Case PossibleTypes.Substring(currentIndex, 1)
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
                    TypeList.Add(ConvertToType(PossibleTypes.Substring(0, commaIndex)).WithLeadingTrivia(VBSpaceTrivia))
                    If commaIndex + 1 < PossibleTypes.Length Then
                        PossibleTypes = PossibleTypes.Substring(commaIndex + 1).Trim
                    Else
                        Exit While
                    End If
                Else
                    While PossibleTypes.Any
                        ' Type
                        commaIndex = PossibleTypes.IndexOf(",", StringComparison.Ordinal)
                        Dim FirstLessOpenParen As Integer = PossibleTypes.IndexOf("(", StringComparison.Ordinal)
                        If commaIndex = -1 OrElse FirstLessOpenParen = -1 Then
                            commaIndex = PossibleTypes.Length
                        ElseIf commaIndex > FirstLessOpenParen Then
                            openParenCount = 0
                            For currentIndex = FirstLessOpenParen To PossibleTypes.Length - 1
                                Select Case PossibleTypes.Substring(currentIndex, 1)
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
                        TypeList.Add(ConvertToType(PossibleTypes.Substring(0, commaIndex)).WithLeadingTrivia(VBSpaceTrivia))
                        If commaIndex + 1 < PossibleTypes.Length Then
                            PossibleTypes = PossibleTypes.Substring(commaIndex + 1).Trim
                        Else
                            PossibleTypes = ""
                            Exit While
                        End If
                    End While
                End If
            End While
            Dim TypeArguemntList As VBS.TypeArgumentListSyntax = FactoryTypeArgumentList(TypeList)
            Return FactoryTypeArgumentList(TypeList)

        End Function

        <Extension>
        Friend Function ConvertToType(PossibleTupleType As ITypeSymbol) As VBS.TypeSyntax
            If PossibleTupleType.IsKind(SymbolKind.ArrayType) Then
                Dim ElementType As VBS.TypeSyntax = DirectCast(PossibleTupleType, IArrayTypeSymbol).ElementType.ConvertToType
                If TypeOf ElementType Is VBS.ArrayTypeSyntax Then
                    Return ElementType
                End If
                Return Factory.ArrayType(ElementType)
            End If
            If PossibleTupleType.IsTupleType Then
                Dim TupleElementList As New List(Of VBS.TupleElementSyntax)
                For Each TupleElement As IFieldSymbol In DirectCast(PossibleTupleType, INamedTypeSymbol).TupleElements
                    TupleElementList.Add(TupleElement.ConvertIFieldToTupleElement)
                Next
                Return Factory.TupleType(TupleElementList.ToArray)
            End If
            If PossibleTupleType.Name = "Tuple" Then
                Dim TupleElementList As New List(Of VBS.TypeSyntax)
                For Each TupleElement As ITypeSymbol In DirectCast(PossibleTupleType, INamedTypeSymbol).TypeArguments
                    TupleElementList.Add(TupleElement.ConvertToType)
                Next
                Return Factory.GenericName("Tuple", FactoryTypeArgumentList(TupleElementList))
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
                    Dim IndexOfBracket As Integer
                    Dim foundClosedBracket As Boolean = True
                    For IndexOfBracket = typeString.Length - 2 To 0 Step -1
                        Select Case typeString.Chars(IndexOfBracket)
                            Case "]"c
                                foundClosedBracket = True
                            Case "["c
                                foundClosedBracket = False
                            Case Else
                                If Not foundClosedBracket Then
                                    IndexOfBracket += 1
                                    Exit For
                                End If
                        End Select
                    Next
                    If AllowArray AndAlso isArray Then
                        arrayRank = typeString.Substring(IndexOfBracket).
                                            Replace("[", "(", StringComparison.Ordinal).
                                            Replace("]", ")", StringComparison.Ordinal)
                    End If
                    typeString = typeString.Substring(0, IndexOfBracket)
                End If

                If typeString.Contains("<", StringComparison.Ordinal) Then
                    typeString = typeString.Replace("<", "(Of ", StringComparison.OrdinalIgnoreCase).
                                            Replace(">", ")", StringComparison.OrdinalIgnoreCase)
                End If
                Dim IndexOf As Integer = typeString.IndexOf("(Of ", StringComparison.OrdinalIgnoreCase)
                If IndexOf >= 0 Then
                    If typeString.StartsWith("System.", StringComparison.Ordinal) Then
                        typeString = typeString.WithoutLeadingSystemDot
                        IndexOf -= 7
                    End If
                    Dim Name As String = typeString.Substring(0, length:=IndexOf)
                    typeString = typeString.Substring(IndexOf + 3)
                    Dim IndexOfLastCloseParen As Integer = typeString.LastIndexOf(")", StringComparison.OrdinalIgnoreCase)
                    typeString = typeString.Substring(0, IndexOfLastCloseParen)
                    Dim TypeList As New List(Of VBS.TypeSyntax)
                    Dim PossibleTypes As String = typeString.Trim
                    While PossibleTypes.Any
                        Dim EndIndex As Integer = 0
                        Dim OpenParenCount As Integer = 0
                        If PossibleTypes.Chars(0) = "(" Then
                            For currentIndex As Integer = 0 To PossibleTypes.Length - 1
                                Select Case PossibleTypes.Chars(currentIndex)
                                    Case "("c
                                        OpenParenCount += 1
                                    Case ")"c
                                        OpenParenCount -= 1
                                        EndIndex = currentIndex + 1
                                        If OpenParenCount = 0 Then
                                            Exit For
                                        End If
                                End Select
                            Next
                            Dim baseType As String = PossibleTypes.Substring(0, EndIndex)
                            While EndIndex + 1 < PossibleTypes.Length AndAlso PossibleTypes.Chars(EndIndex) = "["
                                If AllowArray Then
                                    baseType &= "[]"
                                End If
                                EndIndex += 2
                            End While
                            TypeList.Add(baseType.ConvertCSStringToName)
                            PossibleTypes = PossibleTypes.Substring(EndIndex).Trim
                            If PossibleTypes.Any Then
                                PossibleTypes = PossibleTypes.TrimStart(","c).Trim
                                Continue While
                            Else
                                Exit While
                            End If
                        End If

                        EndIndex = PossibleTypes.IndexOf(",", StringComparison.Ordinal)
                        Dim FirstLessThan As Integer = PossibleTypes.IndexOf("(", StringComparison.Ordinal)
                        If EndIndex = -1 AndAlso FirstLessThan = -1 Then
                            EndIndex = PossibleTypes.Length
                        ElseIf EndIndex = -1 OrElse EndIndex > FirstLessThan Then
                            FirstLessThan = 0
                            For currentIndex As Integer = FirstLessThan To PossibleTypes.Length - 1
                                Select Case PossibleTypes.Chars(currentIndex)
                                    Case "("c, "["c
                                        OpenParenCount += 1
                                    Case ")"c
                                        OpenParenCount -= 1
                                        EndIndex = currentIndex + 1
                                    Case "]"c
                                        OpenParenCount -= 1
                                        If currentIndex = PossibleTypes.Length - 1 Then
                                            EndIndex = currentIndex + 1
                                        End If
                                    Case ","c
                                        If OpenParenCount = 0 Then
                                            EndIndex = currentIndex
                                            Exit For
                                        End If
                                End Select
                            Next
                        End If
                        Dim argument As String = PossibleTypes.Substring(0, EndIndex)
                        TypeList.Add(ConvertToType(argument))
                        If EndIndex + 1 < PossibleTypes.Length Then
                            PossibleTypes = PossibleTypes.Substring(EndIndex + 1).Trim
                        Else
                            Exit While
                        End If
                    End While
                    Dim TypeArguemntList As VBS.TypeArgumentListSyntax = FactoryTypeArgumentList(TypeList)

                    Dim genericNameSyntax As VBS.GenericNameSyntax = Factory.GenericName(Name, TypeArguemntList)

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
            Dim CSNamedTypeString As String = CSNamedTypeStringIn
            Dim IsArray As Boolean = False
            Dim Nullable As Boolean = False
            If CSNamedTypeString.EndsWith("?", StringComparison.Ordinal) Then
                Nullable = True
                CSNamedTypeString = CSNamedTypeString.Substring(0, CSNamedTypeString.Length - 1).Trim
            End If
            If CSNamedTypeString.EndsWith("[]", StringComparison.Ordinal) Then
                IsArray = True
                CSNamedTypeString = CSNamedTypeString.Substring(0, CSNamedTypeString.Length - 2).Trim
            End If

            Dim ElementList As List(Of String) = CSNamedTypeString.ConvertTypeTupleToTypeStrings(IncludeName:=True)
            Dim builder As New StringBuilder
            builder.Append("("c)
            For Each e As IndexClass(Of String) In ElementList.WithIndex
                If e.IsLast Then Exit For
                builder.Append($"{e.Value}, ")
            Next
            builder.Append(ElementList.Last & ")")
            Dim TupleType As String = builder.ToString & If(IsArray, "()", "") & If(Nullable, "?", "")

            Return Factory.ParseTypeName(TupleType).WithLeadingTrivia(VBSpaceTrivia)
        End Function

        <Extension>
        Friend Function ConvertTypeITypeSymbolFromInterfaceToType(expressionConvertedType As ITypeSymbol) As VBS.TypeSyntax

            If Not expressionConvertedType.AllInterfaces.Any Then
                If expressionConvertedType.ToString.EndsWith("IArityEnumerable", StringComparison.Ordinal) Then
                    Return PredefinedTypeInteger
                End If
                Return PredefinedTypeObject
            End If
            For Each NamedType As INamedTypeSymbol In expressionConvertedType.AllInterfaces
                Dim index As Integer = NamedType.ToString.IndexOf(StrIEnumerableOf, StringComparison.Ordinal)
                Dim NewType As String
                If index > 0 Then
                    NewType = NamedType.ToString.Substring(index + StrIEnumerableOf.Length)
                    Return Factory.ParseName(NewType)
                End If
                index = NamedType.ToString.IndexOf(StrIDictionary, StringComparison.Ordinal)
                If index > 0 Then
                    Return NamedType.ConvertToType
                End If
                index = NamedType.ToString.IndexOf(StrIEnumerable, StringComparison.Ordinal)
                If index > 0 Then
                    Return NamedType.ConvertToType
                End If
            Next

            Dim index1 As Integer = expressionConvertedType.ToString.IndexOf(StrIEnumerableOf, StringComparison.Ordinal)
            If index1 > 0 Then
                Dim NewType As String = expressionConvertedType.ToString.Substring(index1 + StrIEnumerableOf.Length)
                Return Factory.ParseName(NewType)
            End If
            Return Nothing
        End Function

        <Extension>
        Friend Function ConvertIFieldToTupleElement(TupleElement As IFieldSymbol) As VBS.TupleElementSyntax
            If TupleElement.Type Is Nothing Then
                Return Factory.NamedTupleElement(TupleElement.Name.ToString(Globalization.CultureInfo.InvariantCulture))
            End If
            Dim AsClause As VBS.SimpleAsClauseSyntax = Factory.SimpleAsClause(AsKeyword.With(VBSpaceTrivia, VBSpaceTrivia), New SyntaxList(Of VBS.AttributeListSyntax), TupleElement.Type.ConvertToType)
            Return Factory.NamedTupleElement(Factory.Identifier(MakeVBSafeName(TupleElement.Name)), AsClause)
        End Function

        <Extension>
        Friend Function ConvertTypeToTypeArgumentList(TypeString As String) As String
            Dim Ret As String = ""
            Dim IndexOf As Integer = TypeString.IndexOf("<", StringComparison.OrdinalIgnoreCase)
            If IndexOf >= 0 Then
                Ret &= $"{TypeString.Substring(0, IndexOf).WithoutLeadingSystemDot}(Of "

                TypeString = TypeString.Substring(IndexOf + 1)
                Dim IndexOfLastCloseBracket As Integer = TypeString.LastIndexOf(">", StringComparison.OrdinalIgnoreCase)
                TypeString = TypeString.Substring(0, IndexOfLastCloseBracket)
                Dim PossibleTypes As String = TypeString.Trim
                While PossibleTypes.Any
                    Dim EndIndex As Integer
                    ' Type
                    EndIndex = PossibleTypes.IndexOf(",", StringComparison.Ordinal)
                    Dim FirstLessThan As Integer = PossibleTypes.IndexOf("<", StringComparison.Ordinal)
                    If EndIndex = -1 AndAlso FirstLessThan = -1 Then
                        Ret &= $"{ConvertSimpleTypeToType(PossibleTypes).ToString.TrimEnd(","c)})"
                        PossibleTypes = ""
                    ElseIf EndIndex > FirstLessThan Then
                        Ret &= $"{ConvertSimpleTypeToType(PossibleTypes.Substring(0, EndIndex))}, "
                        PossibleTypes = TypeString.Substring(EndIndex + 1)
                    Else
                        Stop
                    End If
                    If Not PossibleTypes.Any Then
                        Exit While
                    End If
                End While
                Return Ret.ToString.TrimEnd(","c)
            End If
            Return ""
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
            Dim ElementList As New List(Of String)

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
                            currentIndex = ExtractTupleWithName(CSharpNamedTypeString, IncludeName, typeString, ElementList, currentIndex)
                            commaIndex = CSharpNamedTypeString.IndexOf(",", currentIndex, StringComparison.OrdinalIgnoreCase)
                            typeString.Clear()
                            If commaIndex < 0 Then
                                Exit For
                            End If
                            currentIndex = commaIndex + 1
                        End If
                    Case " " ' variable name
                        currentIndex = ExtractTupleWithName(CSharpNamedTypeString, IncludeName, typeString, ElementList, currentIndex)
                        commaIndex = CSharpNamedTypeString.IndexOf(",", currentIndex, StringComparison.OrdinalIgnoreCase)
                        typeString.Clear()
                        If commaIndex < 0 Then
                            Exit For
                        End If
                        currentIndex = commaIndex + 1
                    Case ","
                        ElementList.Add(ConvertToType(typeString.ToString).ToString)
                        typeString.Clear()
                        currentIndex += If(CSharpNamedTypeString(currentIndex + 1) = " ", 1, 0)
                    Case ")"
                        currentIndex = ExtractTupleWithName(CSharpNamedTypeString, IncludeName, typeString, ElementList, currentIndex)
                        Dim TypePart As String = ConvertToType(typeString.ToString).ToString
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
                                        currentIndex = ExtractTupleWithName(CSharpNamedTypeString, IncludeName, typeString, ElementList, currentIndex)
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
                            ElementList.Add($"({subTupleList.ToArray.JoinLines(", ")})")
                            typeString.Clear()
                        Else
                            ElementList.Add(ConvertToType(typeString.ToString).ToString)
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
                Dim TypePart As String = ConvertToType(typeString.ToString).ToString
                ElementList.Add(TypePart)
            End If
            Return ElementList
        End Function

        ''' <summary>
        ''' Returns Safe VB Name with QualifiedName and TypeName both false
        ''' </summary>
        ''' <param name="id"></param>
        ''' <returns></returns>
        ''' <param name="Node"></param><param name="Model"></param>
        Friend Function GenerateSafeVBToken(id As SyntaxToken, Node As CS.CSharpSyntaxNode, Model As SemanticModel) As SyntaxToken
            Return GenerateSafeVBToken(id, Node, Model, IsQualifiedName:=False, IsTypeName:=False)
        End Function

        ''' <summary>
        ''' Returns Safe VB Name
        ''' </summary>
        ''' <param name="id">Original Variable Name</param>
        ''' <param name="Node"></param>
        ''' <param name="Model"></param>
        ''' <param name="IsQualifiedName">True if name is part of a Qualified Name and should not be renamed</param>
        ''' <param name="IsTypeName"></param>
        ''' <returns></returns>
        Friend Function GenerateSafeVBToken(id As SyntaxToken, Node As CS.CSharpSyntaxNode, Model As SemanticModel, IsQualifiedName As Boolean, IsTypeName As Boolean) As SyntaxToken
            If Node Is Nothing Then
                Throw New ArgumentNullException(NameOf(Node))
            End If

            Dim keywordKind As VB.SyntaxKind = VB.SyntaxFacts.GetKeywordKind(id.ValueText)
            If IsTypeName Then
                IsQualifiedName = True
            Else
                If VB.SyntaxFacts.IsPredefinedType(keywordKind) Then
                    Return id.MakeIdentifierUnique(Node, Model, IsBracketNeeded:=True, IsQualifiedName)
                End If
            End If

            If VB.SyntaxFacts.IsKeywordKind(keywordKind) Then
                Dim bracketNeeded As Boolean = True
                If keywordKind.IsKind(VB.SyntaxKind.REMKeyword, VB.SyntaxKind.DelegateKeyword) OrElse id.Text.Chars(0) = "@" Then
                    bracketNeeded = True
                ElseIf id.Parent Is Nothing Then
                    bracketNeeded = True
                ElseIf TypeOf id.Parent.Parent Is CSS.MemberAccessExpressionSyntax Then
                    bracketNeeded = CType(id.Parent?.Parent, CSS.MemberAccessExpressionSyntax).Expression.ToString.Equals(id.ToString, StringComparison.Ordinal)
                ElseIf id.Parent.AncestorsAndSelf().OfType(Of CSS.UsingDirectiveSyntax).FirstOrDefault().IsKind(CS.SyntaxKind.UsingDirective) Then
                    id = Factory.Token(keywordKind).WithTriviaFrom(id)
                    bracketNeeded = False
                End If
                Return id.MakeIdentifierUnique(Node, Model, bracketNeeded, IsQualifiedNameOrTypeName:=IsQualifiedName)
            End If

            If id.Parent?.IsParentKind(CS.SyntaxKind.Parameter) Then
                Dim Param As CSS.ParameterSyntax = DirectCast(id.Parent.Parent, CSS.ParameterSyntax)
                Dim MethodDeclaration As CSS.MethodDeclarationSyntax = TryCast(Param.Parent?.Parent, CSS.MethodDeclarationSyntax)
                IsQualifiedName = MethodDeclaration Is Nothing OrElse String.Compare(MethodDeclaration.Identifier.ValueText, id.ValueText, ignoreCase:=True, Globalization.CultureInfo.InvariantCulture) = 0
                IsQualifiedName = IsQualifiedName Or String.Compare(Param.Type.ToString, id.ValueText, ignoreCase:=False, Globalization.CultureInfo.InvariantCulture) = 0
            End If
            Return id.MakeIdentifierUnique(Node, Model, IsBracketNeeded:=False, IsQualifiedName)
        End Function

    End Module
End Namespace
