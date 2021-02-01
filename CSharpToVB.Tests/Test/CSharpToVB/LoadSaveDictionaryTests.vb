' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Drawing
Imports System.IO
Imports CSharpToVBApp
Imports CSharpToVBConverter

Imports Xunit

Namespace DictionaryLoadSave.Tests

    <TestClass()> Public NotInheritable Class LoadSaveDictionaryTests

        Private ReadOnly _testThemeMappingDictionary As New Dictionary(Of String, ColorDescriptor)(StringComparer.OrdinalIgnoreCase) From {
                        {ThemeDefaultColor, New ColorDescriptor(Color.White, Color.FromArgb(30, 30, 30))},
                        {ThemeErrorColor, New ColorDescriptor(Color.FromArgb(0, 128, 128), Color.FromArgb(18, 32, 42))}}

        Private ReadOnly _resultDictionary As New Dictionary(Of String, ColorDescriptor)(StringComparer.OrdinalIgnoreCase)

        <Fact>
        Public Sub VBDictionaryWriteTest()
            Dim filePath As String = Path.Combine(Path.GetTempPath, "TestColorDictionary.csv")
            WriteColorDictionaryToFile(filePath, _testThemeMappingDictionary)
            LoadColorDictionaryFromFile(filePath, _resultDictionary)
            Assert.Equal(_testThemeMappingDictionary.Count, _resultDictionary.Count)
            Assert.True(_resultDictionary.ContainsKey(ThemeDefaultColor))
            Assert.True(_testThemeMappingDictionary(ThemeDefaultColor).Equals(_resultDictionary(ThemeDefaultColor)))
            Assert.True(_resultDictionary.ContainsKey(ThemeErrorColor))
            Assert.True(_testThemeMappingDictionary(ThemeErrorColor) = _resultDictionary(ThemeErrorColor))
            File.Delete(filePath)
        End Sub

        <Fact>
        Public Sub ClassificationStringToNameTest()
            Assert.Equal(NameOf(ThemeDefaultColor), ClassificationStringToName("default"))
            Assert.Equal(NameOf(ThemeErrorColor), ClassificationStringToName("error"))
            Assert.Equal(NameOf(NumericLiteral), ClassificationStringToName("number"))
            Assert.Equal(NameOf(String_VerbatimLiteral), ClassificationStringToName("string - verbatim"))
            Assert.Equal(NameOf(StringLiteral), ClassificationStringToName("string"))

            Assert.Equal(NameOf(Comment), ClassificationStringToName("comment"))
            Assert.Equal(NameOf(ExcludedCode), ClassificationStringToName("excluded code"))
            Assert.Equal(NameOf(Identifier), ClassificationStringToName("identifier"))
            Assert.Equal(NameOf(Keyword), ClassificationStringToName("keyword"))
            Assert.Equal(NameOf(ClassificationNameStrings.FunctionKeyword), ClassificationStringToName("Function"))
            Assert.Equal(NameOf(Keyword_Control), ClassificationStringToName("keyword - control"))
            Assert.Equal($"[{NameOf([Operator])}]", ClassificationStringToName("operator"))
            Assert.Equal(NameOf(XmlDocComment_EntityReference), ClassificationStringToName("xml doc comment - entity reference"))

        End Sub
        <Fact>
        Public Sub ClassificationNameToStringTest()
            Assert.Equal(ClassificationNameToString(NameOf(ThemeDefaultColor)), "default")
            Assert.Equal(ClassificationNameToString(NameOf(ThemeErrorColor)), "error")
            Assert.Equal(ClassificationNameToString(NameOf(NumericLiteral)), "number")
            Assert.Equal(ClassificationNameToString(NameOf(String_VerbatimLiteral)), "string - verbatim")
            Assert.Equal(ClassificationNameToString(NameOf(StringLiteral)), "string")

            Assert.Equal(ClassificationNameToString(NameOf(Comment)), "comment")
            Assert.Equal(ClassificationNameToString(NameOf(ExcludedCode)), "excluded code")
            Assert.Equal(ClassificationNameToString(NameOf(Identifier)), "identifier")
            Assert.Equal(ClassificationNameToString(NameOf(Keyword)), "keyword")
            Assert.Equal(ClassificationNameToString(NameOf(ClassificationNameStrings.FunctionKeyword)), "function")
            Assert.Equal(ClassificationNameToString(NameOf(Keyword_Control)), "keyword - control")
            Assert.Equal(ClassificationNameToString(NameOf([Operator])), "operator")

        End Sub

        <Fact>
        Public Shared Sub VBTestRenoveNewLine()
            Dim originalString As String = "This is a 2 Line
String"
            Assert.Equal(originalString.WithoutNewLines, "This is a 2 LineString")
        End Sub

    End Class

End Namespace
