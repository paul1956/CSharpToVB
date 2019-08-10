﻿' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.IO

Imports CodeConverter.Tests

Imports CSharpToVBApp

Imports IVisualBasicCode.CodeConverter
Imports IVisualBasicCode.CodeConverter.ConversionResult

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Emit

Imports Xunit

Namespace ConvertDirectory.Tests
    ''' <summary>
    ''' Return False to skip test
    ''' </summary>
    <TestClass()> Public Class TestCompile
        Private LastFileProcessed As String
        Public Shared ReadOnly Property EnableRoslynTests() As Boolean
            Get
                Return Directory.Exists(GetRoslynRootDirectory)
            End Get
        End Property

#Disable Warning IDE0060 ' Remove unused parameter
        Public Shared Function CountFile(PathWithFileName As String, TargetDirectory As String, LanguageExtension As String, DontCare() As MetadataReference) As Boolean
#Enable Warning IDE0060 ' Remove unused parameter
            ' Do not delete the parameter LanguageExtension it is needed by other versions of this routine
            Return True
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Sub ConvertDirectoryCodeStyle()

            Dim FilesProcessed As Long = 0

            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "CodeStyle"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Sub ConvertDirectoryCompilersCore()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "Core"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Sub ConvertDirectoryCompilersCSharpCSC()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "CSC"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Sub ConvertDirectoryCompilersCSharpCSharpAnalyzerDriver()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "CSharpAnalyzerDriver"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Timeout(100000)>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Sub ConvertDirectoryCompilersCSharpPortable()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "Portable"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Sub ConvertDirectoryCompilersCSharpTestCommandLine()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "Test", "CommandLine"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Sub ConvertDirectoryCompilersCSharpTestEmit()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "Test", "Emit"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Sub ConvertDirectoryCompilersCSharpTestSemantic()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "Test", "Semantic"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Sub ConvertDirectoryCompilersCSharpTestSyntax()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "Test", "Syntax"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Sub ConvertDirectoryCompilersCSharpTestWinRT()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "Test", "WinRT"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Sub ConvertDirectoryCompilersExtension()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "Extension"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Sub ConvertDirectoryCompilersRealParserTests()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "RealParserTests"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Sub ConvertDirectoryCompilersServerVBCSCompiler()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "Server", "VBCSCompiler"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Sub ConvertDirectoryCompilersServerVBCSCompilerTests()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "Server", "VBCSCompilerTests"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Sub ConvertDirectoryCompilersShared()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "Shared"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Sub ConvertDirectoryCompilersTest()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "Test"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Sub ConvertDirectoryCompilersVisualStudio()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "VisualBasic"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Sub ConvertDirectoryDependencies()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Dependencies"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Sub ConvertDirectoryDeployment()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Deployment"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Sub ConvertDirectoryEditorFeatures()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Sub ConvertDirectoryExpressionEvaluator()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "ExpressionEvaluator"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Sub ConvertDirectoryFeatures()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Features"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Sub ConvertDirectoryInteractive()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Interactive"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Sub ConvertDirectoryNuGet()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "NuGet"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Sub ConvertDirectoryScripting()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Scripting"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Sub ConvertDirectorySetup()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Setup"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Sub ConvertDirectoryTest()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Test"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Sub ConvertDirectoryTools()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "Tools"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Sub ConvertDirectoryVisualStudio()
            Dim FilesProcessed As Long = 0
            Assert.True(ProcessDirectory(Path.Combine(GetRoslynRootDirectory(), "src", "VisualStudio"), TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, LastFileNameWithPath:="", SourceLanguageExtension:="cs", FilesProcessed, 0, ProcessFile:=AddressOf Me.ProcessFile), $"Failing file {Me.LastFileProcessed}")
        End Sub

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
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
                Dim ResultOfConversion As ConversionResult = ConvertInputRequest(RequestToConvert, If(OptionalReferences, CSharpReferences.ToArray))
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

    End Class

End Namespace