' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Text
Imports CSharpToVBConverter

Public Module ClassificationNameStrings
#Region "Special names"
    Public Const ThemeDefaultColor As String = "default"
    Public Const ThemeErrorColor As String = "error"
    Public Const NumericLiteral As String = "number"
    Public Const String_VerbatimLiteral As String = "string - verbatim"
    Public Const StringLiteral As String = "string"
    Public Const FunctionKeyword As String = "function"
#End Region

    Public Const Comment As String = "comment"
    Public Const ExcludedCode As String = "excluded code"
    Public Const Identifier As String = "identifier"
    Public Const Keyword As String = "keyword"
    Public Const Keyword_Control As String = "keyword - control"
    Public Const [Operator] As String = "operator"
    Public Const Operator_Overloaded As String = "operator - overloaded"
    Public Const PreprocessorKeyword As String = "preprocessor keyword"
    Public Const Whitespace As String = "whitespace"
    Public Const Text As String = "text"

    Public Const StaticSymbol As String = "static symbol"

    Public Const PreprocessorText As String = "preprocessor text"
    Public Const Punctuation As String = "punctuation"
    Public Const String_EscapeCharacter As String = "string - escape character"

    Public Const ClassName As String = "class name"
    Public Const DelegateName As String = "delegate name"
    Public Const EnumName As String = "enum name"
    Public Const InterfaceName As String = "interface name"
    Public Const ModuleName As String = "module name"
    Public Const StructName As String = "struct name"
    Public Const TypeParameterName As String = "type parameter name"

    Public Const FieldName As String = "field name"
    Public Const EnumMemberName As String = "enum member name"
    Public Const ConstantName As String = "constant name"
    Public Const LocalName As String = "local name"
    Public Const ParameterName As String = "parameter name"
    Public Const MethodName As String = "method name"
    Public Const ExtensionMethodName As String = "extension method name"
    Public Const PropertyName As String = "property name"
    Public Const EventName As String = "event name"
    Public Const NamespaceName As String = "namespace name"
    Public Const LabelName As String = "label name"

    Public Const Regex_Comment As String = "regex - comment"
    Public Const Regex_CharacterClass As String = "regex - character class"
    Public Const Regex_Anchor As String = "regex - anchor"
    Public Const Regex_Quantifier As String = "regex - quantifier"
    Public Const Regex_Grouping As String = "regex - grouping"
    Public Const Regex_Alternation As String = "regex - alternation"
    Public Const Regex_Text As String = "regex - text"
    Public Const Regex_SelfEscapedCharacter As String = "regex - self escaped character"
    Public Const Regex_OtherEscape As String = "regex - other escape"

    Public Const XmlDocComment_AttributeName As String = "xml doc comment - attribute name"
    Public Const XmlDocComment_AttributeQuotes As String = "xml doc comment - attribute quotes"
    Public Const XmlDocComment_AttributeValue As String = "xml doc comment - attribute value"
    Public Const XmlDocComment_CDataSection As String = "xml doc comment - cdata section"
    Public Const XmlDocComment_Comment As String = "xml doc comment - comment"
    Public Const XmlDocComment_Delimiter As String = "xml doc comment - delimiter"
    Public Const XmlDocComment_EntityReference As String = "xml doc comment - entity reference"
    Public Const XmlDocComment_Name As String = "xml doc comment - name"
    Public Const XmlDocComment_ProcessingInstruction As String = "xml doc comment - processing instruction"
    Public Const XmlDocComment_Text As String = "xml doc comment - text"

    Public Const XmlLiteral_AttributeName As String = "xml literal - attribute name"
    Public Const XmlLiteral_AttributeQuotes As String = "xml literal - attribute quotes"
    Public Const XmlLiteral_AttributeValue As String = "xml literal - attribute value"
    Public Const XmlLiteral_CDataSection As String = "xml literal - cdata section"
    Public Const XmlLiteral_Comment As String = "xml literal - comment"
    Public Const XmlLiteral_Delimiter As String = "xml literal - delimiter"
    Public Const XmlLiteral_EmbeddedExpression As String = "xml literal - embedded expression"
    Public Const XmlLiteral_EntityReference As String = "xml literal - entity reference"
    Public Const XmlLiteral_Name As String = "xml literal - name"
    Public Const XmlLiteral_ProcessingInstruction As String = "xml literal - processing instruction"
    Public Const XmlLiteral_Text As String = "xml literal - text"

    Public Function ClassificationNameToString(ClassificationName As String) As String
        Dim name As New StringBuilder
        Select Case ClassificationName
            Case "FunctionKeyword"
                Return "function"
            Case "NumericLiteral"
                Return "number"
            Case "[Operator]"
                Return "operator"
            Case "ThemeDefaultColor"
                Return "default"
            Case "ThemeErrorColor"
                Return "error"
        End Select
        If ClassificationName.EndsWith("Literal", StringComparison.InvariantCulture) Then
            ClassificationName = ClassificationName.Replace("Literal", "", StringComparison.InvariantCulture)
        End If
        For Each c As IndexClass(Of Char) In ClassificationName.AsEnumerable.WithIndex
            If c.IsFirst Then
                name.Append(Char.ToLower(c.Value))
            Else
                If Char.IsUpper(c.Value) Then
                    name.Append(" "c)
                End If
                If c.Value = "_" Then
                    name.Append(" -")
                Else
                    name.Append(Char.ToLower(c.Value))
                End If
            End If
        Next
        Return name.ToString
    End Function

    Public Function ClassificationStringToName(ClassificationString As String) As String
        Dim name As New StringBuilder
        Select Case ClassificationString
            Case "default"
                Return "ThemeDefaultColor"
            Case "error"
                Return "ThemeErrorColor"
            Case "function"
                Return "FunctionKeyword"
            Case "number"
                Return "NumericLiteral"
            Case "operator"
                Return "[Operator]"
            Case "string - verbatim"
                Return "String_VerbatimLiteral"
            Case "string"
                Return "StringLiteral"
        End Select
        Dim nameSplit() As String = ClassificationString.Split(" "c)
        For Each element As String In nameSplit
            If element = "-" Then
                name.Append("_"c)
            Else
                name.Append($"{Char.ToUpperInvariant(element.Chars(0))}{element.Substring(1)}")
            End If
        Next
        Return name.ToString
    End Function
End Module
