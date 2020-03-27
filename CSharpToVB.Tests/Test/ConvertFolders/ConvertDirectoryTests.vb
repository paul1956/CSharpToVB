﻿' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.IO
Imports System.Reflection
Imports System.Threading

Imports CSharpToVBApp

Imports CSharpToVBCodeConverter
Imports CSharpToVBCodeConverter.ConversionResult

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Emit

Imports Xunit

Namespace ConvertDirectory.Tests

    ''' <summary>
    ''' Return False to skip test
    ''' </summary>
    <TestClass()> Public Class TestCompile
        Private _lastFileProcessed As String

        Public Shared ReadOnly Property EnableRoslynTests() As Boolean
            Get
                Return Directory.Exists(GetRoslynRootDirectory)
            End Get
        End Property

        Public Function ProcessFileAsync(PathWithFileName As String, TargetDirectory As String, LanguageExtension As String, CSPreprocessorSymbols As List(Of String), VBPreprocessorSymbols As List(Of KeyValuePair(Of String, Object)), OptionalReferences() As MetadataReference, CancelToken As CancellationToken) As Task(Of Boolean)
            ' Save to TargetDirectory not supported
            Assert.True(String.IsNullOrWhiteSpace(TargetDirectory))
            ' Do not delete the next line or the parameter it is needed by other versions of this routine
            _lastFileProcessed = PathWithFileName
            Using fs As FileStream = File.OpenRead(PathWithFileName)
                Dim RequestToConvert As ConvertRequest = New ConvertRequest(mSkipAutoGenerated:=True, mProgress:=Nothing, mCancelToken:=CancelToken) With {
                                                                .SourceCode = GetFileTextFromStream(fs)
                                                                }
                Dim ResultOfConversion As ConversionResult = ConvertInputRequest(RequestToConvert, CSPreprocessorSymbols, VBPreprocessorSymbols, CSharpReferences(Assembly.Load("System.Windows.Forms").Location, OptionalReferences).ToArray, ReportException:=Nothing, mProgress:=Nothing, CancelToken:=CancellationToken.None)
                If ResultOfConversion.ResultStatus = ResultTriState.Failure Then
                    Return Task.FromResult(False)
                End If
                Dim CompileResult As (CompileSuccess As Boolean, EmitResult As EmitResult) = CompileVisualBasicString(StringToBeCompiled:=ResultOfConversion.ConvertedCode, SeverityToReport:=DiagnosticSeverity.Error, ResultOfConversion:=ResultOfConversion)
                If Not CompileResult.CompileSuccess OrElse ResultOfConversion.GetFilteredListOfFailures().Any Then
                    Dim Msg As String = If(CompileResult.CompileSuccess, ResultOfConversion.GetFilteredListOfFailures()(0).GetMessage, "Fatal Compile error")
                    Throw New ApplicationException($"{PathWithFileName} failed to compile with error :{vbCrLf}{Msg}")
                    Return Task.FromResult(False)
                End If
            End Using
            Return Task.FromResult(True)
        End Function

        Private Async Function TextProcessDirectoryAsync(SourceDirectory As String) As Task(Of Boolean)
            Return Await ProcessDirectoryAsync(SourceDirectory, TargetDirectory:="", MeForm:=Nothing, StopButton:=Nothing, RichTextBoxFileList:=Nothing, SourceLanguageExtension:="cs", New ProcessingStats(""), TotalFilesToProcess:=0, AddressOf ProcessFileAsync, CancellationToken.None)
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryCodeStyleAsync() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "CodeStyle")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryCompilersCore() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "Core")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryCompilersCSharpCSC() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "CSC")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryCompilersCSharpCSharpAnalyzerDriver() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "CSharpAnalyzerDriver")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Timeout(100000)>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryCompilersCSharpPortable() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "Portable")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryCompilersCSharpTestCommandLine() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "Test", "CommandLine")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryCompilersCSharpTestEmit() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "Test", "Emit")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryCompilersCSharpTestSemantic() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "Test", "Semantic")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryCompilersCSharpTestSyntax() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "Test", "Syntax")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryCompilersCSharpTestWinRT() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "Test", "WinRT")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryCompilersExtension() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "Extension")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryCompilersRealParserTests() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "RealParserTests")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryCompilersServerVBCSCompiler() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "Server", "VBCSCompiler")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryCompilersServerVBCSCompilerTests() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "Server", "VBCSCompilerTests")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryCompilersShared() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "Shared")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryCompilersTest() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "Test")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryCompilersVisualStudio() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "VisualBasic")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryDependencies() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Dependencies")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryDeployment() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Deployment")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryEditorFeaturesCore() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "Core")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryEditorFeaturesCoreWpf() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "Core.Wpf")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryEditorFeaturesCSharp() As Task

            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryEditorFeaturesCSharpTest() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp.Test")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryEditorFeaturesCSharpTest2() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp.Test2")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryEditorFeaturesCSharpWpf() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp.Wpf")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryEditorFeaturesTest() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "Test")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryEditorFeaturesTest2() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "Test2")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryEditorFeaturesTestUtilities() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "TestUtilities")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryEditorFeaturesTestUtilities2() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "TestUtilities2")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryEditorFeaturesText() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "Text")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryEditorFeaturesVisualBasic() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "VisualBasic")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryEditorFeaturesVisualBasicTest() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "VisualBasicTest")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryExpressionEvaluator() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "ExpressionEvaluator")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryFeatures() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryInteractive() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Interactive")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryNuGet() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "NuGet")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryScripting() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Scripting")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectorySetup() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Setup")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryTest() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Test")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryTools() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Tools")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryVisualStudio() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "VisualStudio")), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <ConditionalFact(NameOf(EnableRoslynTests))>
        Public Async Function ConvertDirectoryWorkspaces() As Task
            Assert.True(Await TextProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces")), $"Failing file {_lastFileProcessed}")
        End Function

    End Class

End Namespace