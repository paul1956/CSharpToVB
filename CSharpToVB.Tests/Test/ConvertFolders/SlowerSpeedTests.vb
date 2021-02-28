' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.IO

Imports Xunit

Namespace ConvertDirectory.Tests
    <TestClass()>
    Public Class SlowerSpeedTests

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function CompilersCSharpTestSemanticDiagnosticsConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slower Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "Test", "Semantic", "Diagnostics")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function CompilersServerVBCSCompilerConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slower Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "Server", "VBCSCompiler")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function CompilersServerVBCSCompilerTestsConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slower Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "Server", "VBCSCompilerTests")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function CompilersTestUtilitiesCSharpConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slower Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "Test", "Utilities", "CSharp")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function EditorFeaturesCoreCocoaConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slower Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "Core.Cocoa")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function ExpressionEvaluatorCoreSourceExpressionCompilerConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slower Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "ExpressionEvaluator", "Core", "Source", "ExpressionCompiler")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function ExpressionEvaluatorCoreTestResultProviderConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slower Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "ExpressionEvaluator", "Core", "Test", "ResultProvider")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function FeaturesCSharpPortableCodeRefactoringsConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slower Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "CodeRefactorings")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function FeaturesCSharpPortableConvertLinqConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slower Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "ConvertLinq")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function FeaturesCSharpPortableExtractMethodConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slower Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "ExtractMethod")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function ScriptingCoreConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slower Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Scripting", "Core")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function ToolsExternalAccessFSharpConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slower Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Tools", "ExternalAccess", "FSharp")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function ToolsSourceRunTestsConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slower Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Tools", "Source", "RunTests")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function WorkspacesCoreMSBuildMSBuildConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slower Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "Core", "MSBuild", "MSBuild")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function WorkspacesCoreTestUtilitiesConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slower Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "CoreTestUtilities")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function WorkspacesCSharpPortableSimplificationConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slower Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "CSharp", "Portable", "Simplification")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function WorkspacesRemoteCoreConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slower Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "Remote", "Core")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function WorkspacesRemoteServiceHubServicesConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slower Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "Remote", "ServiceHub", "Services")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCoreExtensionsConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slower Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "Core", "Extensions")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCoreUtilitiesConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slower Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "Core", "Utilities")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCSharpExtensionsConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slower Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "CSharp", "Extensions")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCSharpFormattingConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slower Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "CSharp", "Formatting")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCSharpServicesSyntaxFactsConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slower Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "CSharp", "Services", "SyntaxFacts")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsWorkspaceCSharpExtensionsConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slower Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Workspace", "CSharp", "Extensions")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsWorkspaceCSharpLanguageServicesConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slower Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Workspace", "CSharp", "LanguageServices")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

    End Class

End Namespace
