' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports CSharpToVBApp

Imports Microsoft.CodeAnalysis

Imports Xunit

Namespace ConvertDirectory.Tests

    <TestClass()> Public Class CountFilesTest
        Private ReadOnly LastFileProcessed As String

        Public Shared Function CountFile(PathWithFileName As String, TargetDirectory As String, LanguageExtension As String, DontCare() As MetadataReference) As Boolean
            ' Do not delete the parameter LanguageExtension it is needed by other versions of this routine
            Return True
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Sub VB_CountAll()
            Dim FilesProcessed As Long = 0
            Dim SourceDirectory As String = GetRoslynRootDirectory()
            Assert.True(ProcessDirectory(SourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, TotalFilesToProcess:=0, ProcessFile:=AddressOf CountFile), $"Failing file {Me.LastFileProcessed}")
            Assert.Equal(GetFileCount(SourceDirectory, "cs", SkipBinAndObjFolders:=True, SkipTestResourceFiles:=True), FilesProcessed)
        End Sub

    End Class

End Namespace