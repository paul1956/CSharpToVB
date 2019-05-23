Imports System.IO
Imports CSharpToVBApp
Imports IVisualBasicCode.CodeConverter
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Emit
Imports Xunit
Namespace CSharpToVB.Tests
    <TestClass()> Public Class TestCompile
        Private LastFileProcessed As String

        Public Shared Function CountFile(PathWithFileName As String, TargetDirectory As String, LanguageExtension As String, TotalFilesToProcess As Long, DontCare() As MetadataReference) As Boolean
            ' Do not delete the parameter LanguageExtension it is needed by other versions of this routine
            Return True
        End Function

        <Fact>
        Public Sub ConvertDirectoryCodeStyle()
            Const sourceDirectory As String = RoslynRootDirectory & "\CodeStyle"

            Dim FilesProcessed As Long = 0

            Assert.True(ProcessDirectory(sourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Sub ConvertDirectoryCompilersCore()
            Const sourceDirectory As String = RoslynRootDirectory & "\Compilers\Core"
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(sourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Fact>
        Public Sub ConvertDirectoryCompilersCSharpCSC()
            Const sourceDirectory As String = RoslynRootDirectory & "\Compilers\CSharp\CSC"
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(sourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Fact>
        Public Sub ConvertDirectoryCompilersCSharpCSharpAnalyzerDriver()
            Const sourceDirectory As String = RoslynRootDirectory & "\Compilers\CSharp\CSharpAnalyzerDriver"
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(sourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Timeout(100000)>
        <Fact>
        Public Sub ConvertDirectoryCompilersCSharpPortable()
            Const sourceDirectory As String = RoslynRootDirectory & "\Compilers\CSharp\Portable"
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(sourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Sub ConvertDirectoryCompilersCSharpTestCommandLine()
            Const sourceDirectory As String = RoslynRootDirectory & "\Compilers\CSharp\Test\CommandLine"
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(sourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Sub ConvertDirectoryCompilersCSharpTestEmit()
            Const sourceDirectory As String = RoslynRootDirectory & "\Compilers\CSharp\Test\Emit"
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(sourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Sub ConvertDirectoryCompilersCSharpTestSemantic()
            Const sourceDirectory As String = RoslynRootDirectory & "\Compilers\CSharp\Test\Semantic"
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(sourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Sub ConvertDirectoryCompilersCSharpTestSyntax()
            Const sourceDirectory As String = RoslynRootDirectory & "\Compilers\CSharp\Test\Syntax"
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(sourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Fact>
        Public Sub ConvertDirectoryCompilersCSharpTestWinRT()
            Const sourceDirectory As String = RoslynRootDirectory & "\Compilers\CSharp\Test\WinRT"
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(sourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Fact>
        Public Sub ConvertDirectoryCompilersExtension()
            Const sourceDirectory As String = RoslynRootDirectory & "\Compilers\Extension"
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(sourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Fact>
        Public Sub ConvertDirectoryCompilersRealParserTests()
            Const sourceDirectory As String = RoslynRootDirectory & "\Compilers\RealParserTests"
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(sourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Fact>
        Public Sub ConvertDirectoryCompilersServerVBCSCompiler()
            Const sourceDirectory As String = RoslynRootDirectory & "\Compilers\Server\VBCSCompiler"
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(sourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Fact>
        Public Sub ConvertDirectoryCompilersServerVBCSCompilerTests()
            Const sourceDirectory As String = RoslynRootDirectory & "\Compilers\Server\VBCSCompilerTests"
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(sourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Fact>
        Public Sub ConvertDirectoryCompilersShared()
            Const sourceDirectory As String = RoslynRootDirectory & "\Compilers\Shared"
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(sourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Fact>
        Public Sub ConvertDirectoryCompilersTest()
            Const sourceDirectory As String = RoslynRootDirectory & "\Compilers\Test"
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(sourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Sub ConvertDirectoryCompilersVisualStudio()
            Const sourceDirectory As String = RoslynRootDirectory & "\Compilers\VisualBasic"
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(sourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Fact>
        Public Sub ConvertDirectoryDependencies()
            Const sourceDirectory As String = RoslynRootDirectory & "\Dependencies"
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(sourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Fact>
        Public Sub ConvertDirectoryDeployment()
            Const sourceDirectory As String = RoslynRootDirectory & "\Deployment"
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(sourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Sub ConvertDirectoryEditorFeatures()
            Const sourceDirectory As String = RoslynRootDirectory & "\EditorFeatures"
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(sourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Sub ConvertDirectoryExpressionEvaluator()
            Const sourceDirectory As String = RoslynRootDirectory & "\ExpressionEvaluator"
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(sourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Sub ConvertDirectoryFeatures()
            Const sourceDirectory As String = RoslynRootDirectory & "\Features"
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(sourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Sub ConvertDirectoryInteractive()
            Const sourceDirectory As String = RoslynRootDirectory & "\Interactive"
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(sourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Fact>
        Public Sub ConvertDirectoryNuGet()
            Const sourceDirectory As String = RoslynRootDirectory & "\NuGet"
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(sourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Sub ConvertDirectoryScripting()
            Const sourceDirectory As String = RoslynRootDirectory & "\Scripting"
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(sourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Fact>
        Public Sub ConvertDirectorySetup()
            Const sourceDirectory As String = RoslynRootDirectory & "\Setup"
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(sourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Sub ConvertDirectoryTest()
            Const sourceDirectory As String = RoslynRootDirectory & "\Test"
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(sourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Sub ConvertDirectoryTools()
            Const sourceDirectory As String = RoslynRootDirectory & "\Tools"
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(sourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Sub ConvertDirectoryVisualStudio()
            Const sourceDirectory As String = RoslynRootDirectory & "\VisualStudio"
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(sourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Fact>
        Public Sub ConvertDirectoryWorkspaces()
            Const sourceDirectory As String = RoslynRootDirectory & "\Workspaces"
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(sourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        Public Function ProcessFile(PathWithFileName As String, TargetDirectory As String, LanguageExtension As String, TotalFilesToProcess As Long, OptionalReferences() As MetadataReference) As Boolean
            ' Save to TargetDirectory not supported
            Assert.True(TargetDirectory.IsEmptyNullOrWhitespace)
            ' Do not delete the next line or the parameter it is needed by other versions of this routine
            LanguageExtension = ""
            Me.LastFileProcessed = PathWithFileName
            Dim fs As FileStream = File.OpenRead(PathWithFileName)
            Dim RequestToConvert As ConvertRequest = New ConvertRequest("cs2vb", Sub() Return, _ProgressBar:=Nothing) With {
                                                            .SourceCode = GetFileTextFromStream(fs)
                                                            }
            If RequestToConvert.SourceCode.Contains({"< autogenerated", "<auto-generated"}, StringComparison.CurrentCultureIgnoreCase) Then
                Return True
            End If
            Dim ResultOfConversion As ConversionResult = ConvertInputRequest(RequestToConvert, If(OptionalReferences, References()))
            If ResultOfConversion.Success = False Then
                Return False
            End If
            Dim CompileResult As EmitResult = CompileVisualBasicString(StringToBeCompiled:=ResultOfConversion.ConvertedCode, ErrorsToBeIgnored, DiagnosticSeverity.Error, ResultOfConversion:=ResultOfConversion)
            If ResultOfConversion.FilteredListOfFailures.Count > 0 Then
                Dim Msg As String = ResultOfConversion.FilteredListOfFailures(0).GetMessage
                Throw New ApplicationException($"{PathWithFileName} failed to compile with error :{vbCrLf}{Msg}")
                Return False
            End If
            Return True
        End Function
        <Fact>
        Public Sub VB_CountAll()
            Const sourceDirectory As String = RoslynRootDirectory & "\"
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(sourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf CountFile), $"Failing file {Me.LastFileProcessed}")
            Assert.Equal(8745, FilesProcessed)
        End Sub
    End Class
End Namespace
