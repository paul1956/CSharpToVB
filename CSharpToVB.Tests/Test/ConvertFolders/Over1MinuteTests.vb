' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.IO

Imports Xunit

Namespace ConvertDirectory.Tests

    <TestClass()>
    Public Class Over1MinuteTests

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function CompilersCorePortableConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Over 1 Minute Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "Core", "Portable")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function CompilersCSharpPortableConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Over 1 Minute Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "Portable")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function CompilersCSharpTestEmitConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Over 1 Minute Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "Test", "Emit")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function CompilersCSharpTestSymbolConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Over 1 Minute Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "Test", "Symbol")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function CompilersTestCoreConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Over 1 Minute Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "Test", "Core")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function EditorFeaturesCoreConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Over 1 Minute Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "Core")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function EditorFeaturesTestConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Over 1 Minute Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "Test")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function FeaturesCorePortableConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Over 1 Minute Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "Core", "Portable")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function FeaturesCSharpPortableEditAndContinueConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Over 1 Minute Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "EditAndContinue")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function VisualStudioCoreDefConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Over 1 Minute Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "VisualStudio", "Core", "Def")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <SkippableFact>
        Public Async Function WorkspacesCorePortableConvertAsync() As Task
            Skip.IfNot(EnableRoslynTests, "Slowest Test")
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "Core", "Portable")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

    End Class

End Namespace
