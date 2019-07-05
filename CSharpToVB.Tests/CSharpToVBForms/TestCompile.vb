Imports System.IO

Imports CSharpToVBApp

Imports IVisualBasicCode.CodeConverter
Imports IVisualBasicCode.CodeConverter.ConversionResult

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Emit

Imports Xunit

Namespace CSharpToVB.Tests

    <TestClass()> Public Class TestCompile
        Private LastFileProcessed As String

        Public Shared Function CountFile(PathWithFileName As String, TargetDirectory As String, LanguageExtension As String, DontCare() As MetadataReference) As Boolean
            ' Do not delete the parameter LanguageExtension it is needed by other versions of this routine
            Return True
        End Function

        <Fact>
        Public Sub ConvertDirectoryCodeStyle()

            Dim FilesProcessed As Long = 0

            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "CodeStyle"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Sub ConvertDirectoryCompilersCore()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "Core"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Fact>
        Public Sub ConvertDirectoryCompilersCSharpCSC()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "CSC"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Fact>
        Public Sub ConvertDirectoryCompilersCSharpCSharpAnalyzerDriver()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "CSharpAnalyzerDriver"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Timeout(100000)>
        <Fact>
        Public Sub ConvertDirectoryCompilersCSharpPortable()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "Portable"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Sub ConvertDirectoryCompilersCSharpTestCommandLine()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "Test", "CommandLine"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Sub ConvertDirectoryCompilersCSharpTestEmit()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "Test", "Emit"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Sub ConvertDirectoryCompilersCSharpTestSemantic()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "Test", "Semantic"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Sub ConvertDirectoryCompilersCSharpTestSyntax()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "Test", "Syntax"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Fact>
        Public Sub ConvertDirectoryCompilersCSharpTestWinRT()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "Test", "WinRT"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Fact>
        Public Sub ConvertDirectoryCompilersExtension()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "Extension"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Fact>
        Public Sub ConvertDirectoryCompilersRealParserTests()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "RealParserTests"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Fact>
        Public Sub ConvertDirectoryCompilersServerVBCSCompiler()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "Server", "VBCSCompiler"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Fact>
        Public Sub ConvertDirectoryCompilersServerVBCSCompilerTests()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "Server", "VBCSCompilerTests"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Fact>
        Public Sub ConvertDirectoryCompilersShared()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "Shared"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Fact>
        Public Sub ConvertDirectoryCompilersTest()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "Test"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Sub ConvertDirectoryCompilersVisualStudio()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "VisualBasic"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Fact>
        Public Sub ConvertDirectoryDependencies()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Dependencies"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Fact>
        Public Sub ConvertDirectoryDeployment()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Deployment"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Sub ConvertDirectoryEditorFeatures()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Sub ConvertDirectoryExpressionEvaluator()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "ExpressionEvaluator"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Sub ConvertDirectoryFeatures()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Features"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Sub ConvertDirectoryInteractive()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Interactive"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Fact>
        Public Sub ConvertDirectoryNuGet()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "NuGet"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Sub ConvertDirectoryScripting()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Scripting"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Fact>
        Public Sub ConvertDirectorySetup()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Setup"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Sub ConvertDirectoryTest()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Test"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Sub ConvertDirectoryTools()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Tools"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Sub ConvertDirectoryVisualStudio()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "VisualStudio"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Fact>
        Public Sub ConvertDirectoryWorkspaces()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        Public Function ProcessFile(PathWithFileName As String, TargetDirectory As String, LanguageExtension As String, OptionalReferences() As MetadataReference) As Boolean
            ' Save to TargetDirectory not supported
            Assert.True(TargetDirectory.IsEmptyNullOrWhitespace)
            ' Do not delete the next line or the parameter it is needed by other versions of this routine
            Me.LastFileProcessed = PathWithFileName
            Using fs As FileStream = File.OpenRead(PathWithFileName)
                Dim RequestToConvert As ConvertRequest = New ConvertRequest(ConvertRequest.CS_To_VB, True, Sub() Return, _ProgressBar:=Nothing) With {
                                                                .SourceCode = GetFileTextFromStream(fs)
                                                                }
                Dim ResultOfConversion As ConversionResult = ConvertInputRequest(RequestToConvert, If(OptionalReferences, References.ToArray))
                If ResultOfConversion.ResultStatus = ResultTriState.Failure Then
                    Return False
                End If
                Dim CompileResult As EmitResult = CompileVisualBasicString(StringToBeCompiled:=ResultOfConversion.ConvertedCode, ErrorsToBeIgnored, DiagnosticSeverity.Error, ResultOfConversion:=ResultOfConversion)
                If ResultOfConversion.FilteredListOfFailures.Count > 0 Then
                    Dim Msg As String = ResultOfConversion.FilteredListOfFailures(0).GetMessage
                    Throw New ApplicationException($"{PathWithFileName} failed to compile with error :{vbCrLf}{Msg}")
                    Return False
                End If
            End Using
            Return True
        End Function

        <Fact>
        Public Sub VB_CountAll()
            Dim FilesProcessed As Long = 0
            Dim SourceDirectory As String = GetRoslynRootDirectory()
            Assert.True(ProcessDirectory(SourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, TotalFilesToProcess:=0, ProcessFile:=AddressOf CountFile), $"Failing file {Me.LastFileProcessed}")
            Assert.Equal(GetFileCount(SourceDirectory, "cs", SkipBinAndObjFolders:=True, SkipTestResourceFiles:=True), FilesProcessed)
        End Sub

    End Class

End Namespace