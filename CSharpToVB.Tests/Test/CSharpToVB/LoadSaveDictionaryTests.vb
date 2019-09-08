' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports CSharpToVBApp

Imports Xunit

Namespace DictionaryLoadSave.Tests

    <TestClass()> Public NotInheritable Class LoadSaveDictionaryTests

        <Fact>
        Public Shared Sub VBDictionaryWriteTest()
            Dim filePath As String = IO.Path.Combine(FileIO.SpecialDirectories.MyDocuments, "ColorDictionary.csv")
            ColorSelector.WriteColorDictionaryToFile(filePath)
        End Sub

        <Fact>
        Public Shared Sub VBTestRenoveNewLine()
            Dim OriginalString As String = "This is a 2 Line
String"
            Dim ResultlString As String = "This is a 2 LineString"
            Assert.Equal(OriginalString.WithoutNewLines, ResultlString)
        End Sub

    End Class

End Namespace
