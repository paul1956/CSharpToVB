Imports CSharpToVBApp
Imports Microsoft.CodeAnalysis
Imports Xunit

Namespace CSharpToVB.Tests

    <TestClass()> Public Class LoadSaveDictionaryTests
        Dim LastFileProcessed As String
        ReadOnly ListOfFiles As New List(Of String)
        Dim MaxPathLength As Integer = 0
        Public Function GetMaxPathLength(PathWithFileName As String, TargetDirectory As String, LanguageExtension As String, DontCare() As MetadataReference) As Boolean
            ' Do not delete the next line of the parameter
            Me.LastFileProcessed = PathWithFileName
            Me.ListOfFiles.Add(Me.LastFileProcessed)
            If PathWithFileName.Length > Me.MaxPathLength Then
                Me.MaxPathLength = PathWithFileName.Length
            End If
            Return True
        End Function

        <Fact>
        Public Shared Sub VB_DictionaryWriteTest()
            Dim filePath As String = IO.Path.Combine(FileIO.SpecialDirectories.MyDocuments, "ColorDictionary.csv")
            ColorSelector.WriteColorDictionaryToFile(filePath)
        End Sub

        <Fact>
        Public Sub VB_MaxPathLength()
            Dim FilesProcessed As Long = 0
            Dim LastFileNameWithPath As String = ""
            Dim Condition As Boolean = ProcessDirectory(GetRoslynRootDirectory(), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:=LastFileNameWithPath, SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.GetMaxPathLength)
            Assert.True(Me.MaxPathLength = 210, $"MaxPathLength of 210 <> {Me.MaxPathLength}")
        End Sub

        <Fact>
        Public Shared Sub VB_TestRenoveNewLine()
            Dim OriginalString As String = "This is a 2 Line
String"
            Dim ResultlString As String = "This is a 2 LineString"
            Assert.True(OriginalString.WithoutNewLines = ResultlString)
        End Sub
    End Class

End Namespace