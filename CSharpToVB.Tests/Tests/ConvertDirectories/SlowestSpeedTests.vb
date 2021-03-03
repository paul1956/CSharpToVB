' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.IO
Imports Xunit

Namespace Tests.ConvertDirectories

    <TestClass()>
    Public Class SlowestSpeedTests

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function AnalyzersCoreAnalyzersConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slowest Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "Core", "Analyzers")).ConfigureAwait(True), $"Failing file {LastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function AnalyzersCoreCodeFixesConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slowest Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "Core", "CodeFixes")).ConfigureAwait(True), $"Failing file {LastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function CompilersCoreCodeAnalysisTestConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slowest Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "Core", "CodeAnalysisTest")).ConfigureAwait(True), $"Failing file {LastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function CompilersCSharpTestCommandLineConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slowest Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "Test", "CommandLine")).ConfigureAwait(True), $"Failing file {LastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function CompilersCSharpTestSemanticSemanticsConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slowest Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "Test", "Semantic", "Semantics")).ConfigureAwait(True), $"Failing file {LastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function CompilersCSharpTestSyntaxConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slowest Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "Test", "Syntax")).ConfigureAwait(True), $"Failing file {LastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function CompilersTestResourcesCoreConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slowest Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "Test", "Resources", "Core")).ConfigureAwait(True), $"Failing file {LastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function EditorFeaturesCoreWpfConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slowest Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "Core.Wpf")).ConfigureAwait(True), $"Failing file {LastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function EditorFeaturesCSharpTestDiagnosticsConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slowest Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "Diagnostics")).ConfigureAwait(True), $"Failing file {LastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function EditorFeaturesTestUtilitiesConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slowest Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "TestUtilities")).ConfigureAwait(True), $"Failing file {LastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function ExpressionEvaluatorCoreSourceResultProviderConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slowest Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "ExpressionEvaluator", "Core", "Source", "ResultProvider")).ConfigureAwait(True), $"Failing file {LastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function ExpressionEvaluatorCSharpSourceExpressionCompilerConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slowest Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "ExpressionEvaluator", "CSharp", "Source", "ExpressionCompiler")).ConfigureAwait(True), $"Failing file {LastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function ExpressionEvaluatorCSharpTestExpressionCompilerConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slowest Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "ExpressionEvaluator", "CSharp", "Test", "ExpressionCompiler")).ConfigureAwait(True), $"Failing file {LastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function FeaturesCSharpPortableCompletionConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slowest Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "Completion")).ConfigureAwait(True), $"Failing file {LastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function FeaturesLanguageServerProtocolConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slowest Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "LanguageServer", "Protocol")).ConfigureAwait(True), $"Failing file {LastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function VisualStudioCoreImplConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slowest Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "VisualStudio", "Core", "Impl")).ConfigureAwait(True), $"Failing file {LastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function VisualStudioCoreTestNextConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slowest Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "VisualStudio", "Core", "Test.Next")).ConfigureAwait(True), $"Failing file {LastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function VisualStudioCSharpImplConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slowest Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "VisualStudio", "CSharp", "Impl")).ConfigureAwait(True), $"Failing file {LastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function VisualStudioIntegrationTestTestUtilitiesConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slowest Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "VisualStudio", "IntegrationTest", "TestUtilities")).ConfigureAwait(True), $"Failing file {LastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function WorkspacesCoreTestConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slowest Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "CoreTest")).ConfigureAwait(True), $"Failing file {LastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function WorkspacesCSharpPortableCodeGenerationConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slowest Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "CSharp", "Portable", "CodeGeneration")).ConfigureAwait(True), $"Failing file {LastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCoreFormattingConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slowest Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "Core", "Formatting")).ConfigureAwait(True), $"Failing file {LastFileProcessed}")
        End Function

    End Class

End Namespace
