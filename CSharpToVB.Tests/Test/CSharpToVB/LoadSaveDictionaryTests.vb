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

        Private ReadOnly _testThemeMappingDictionary As New Dictionary(Of String, (ForeGround As Color, Background As Color))(StringComparer.OrdinalIgnoreCase) From {
                        {"default", (Color.White, Color.FromArgb(30, 30, 30))},
                        {"class name", (Color.FromArgb(0, 128, 128), Color.FromArgb(18, 32, 42))}}

        Private ReadOnly _resultDictionary As New Dictionary(Of String, (ForeGround As Color, Background As Color))(StringComparer.OrdinalIgnoreCase)

        <Fact>
        Public Sub VBDictionaryWriteTest()
            Dim filePath As String = Path.Combine(Path.GetTempPath, "TestColorDictionary.csv")
            WriteColorDictionaryToFile(filePath, _testThemeMappingDictionary)
            LoadColorDictionaryFromFile(filePath, _resultDictionary)
            Assert.Equal(_testThemeMappingDictionary.Count, _resultDictionary.Count)
            Assert.Equal(_testThemeMappingDictionary("default").ForeGround.ToArgb, _resultDictionary("default").ForeGround.ToArgb)
            Assert.Equal(_testThemeMappingDictionary("default").Background.ToArgb, _resultDictionary("default").Background.ToArgb)
            Assert.Equal(_testThemeMappingDictionary("class name"), _resultDictionary("class name"))
            Assert.Equal(_testThemeMappingDictionary("class name").ForeGround.ToArgb, _resultDictionary("class name").ForeGround.ToArgb)
            Assert.Equal(_testThemeMappingDictionary("class name").Background.ToArgb, _resultDictionary("class name").Background.ToArgb)
            File.Delete(filePath)
        End Sub

        <Fact>
        Public Shared Sub VBTestRenoveNewLine()
            Dim originalString As String = "This is a 2 Line
String"
            Assert.Equal(originalString.WithoutNewLines, "This is a 2 LineString")
        End Sub

    End Class

End Namespace
