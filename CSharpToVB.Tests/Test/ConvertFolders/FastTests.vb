' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.IO

Imports Xunit

Namespace ConvertDirectory.Tests
    <TestClass()>
    Public Class FastTests

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersAddAccessibilityModifiersConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "AddAccessibilityModifiers")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersAddBracesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "AddBraces")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersAddRequiredParenthesesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "AddRequiredParentheses")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersConvertAnonymousTypeToTupleConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "ConvertAnonymousTypeToTuple")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersConvertSwitchStatementToExpressionConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "ConvertSwitchStatementToExpression")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersConvertTypeofToNameofConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "ConvertTypeofToNameof")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersFileHeadersConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "FileHeaders")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersInlineDeclarationConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "InlineDeclaration")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersInvokeDelegateWithConditionalAccessConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "InvokeDelegateWithConditionalAccess")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersMakeLocalFunctionStaticConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "MakeLocalFunctionStatic")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersMakeStructFieldsWritableConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "MakeStructFieldsWritable")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersMatchFolderAndNamespaceConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "MatchFolderAndNamespace")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersMisplacedUsingDirectivesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "MisplacedUsingDirectives")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersNamingStyleConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "NamingStyle")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersOrderModifiersConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "OrderModifiers")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersPopulateSwitchConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "PopulateSwitch")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersQualifyMemberAccessConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "QualifyMemberAccess")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersRemoveConfusingSuppressionConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "RemoveConfusingSuppression")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersRemoveRedundantEqualityConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "RemoveRedundantEquality")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersRemoveUnnecessaryCastConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "RemoveUnnecessaryCast")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersRemoveUnnecessaryDiscardDesignationConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "RemoveUnnecessaryDiscardDesignation")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersRemoveUnnecessaryImportsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "RemoveUnnecessaryImports")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersRemoveUnnecessaryParenthesesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "RemoveUnnecessaryParentheses")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersRemoveUnnecessarySuppressionsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "RemoveUnnecessarySuppressions")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersRemoveUnreachableCodeConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "RemoveUnreachableCode")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersRemoveUnusedMembersConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "RemoveUnusedMembers")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersRemoveUnusedParametersAndValuesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "RemoveUnusedParametersAndValues")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersSimplifyBooleanExpressionConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "SimplifyBooleanExpression")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersSimplifyInterpolationConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "SimplifyInterpolation")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersSimplifyLinqExpressionConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "SimplifyLinqExpression")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersUseAutoPropertyConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "UseAutoProperty")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersUseCoalesceExpressionConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "UseCoalesceExpression")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersUseCollectionInitializerConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "UseCollectionInitializer")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersUseCompoundAssignmentConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "UseCompoundAssignment")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersUseConditionalExpressionConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "UseConditionalExpression")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersUseDeconstructionConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "UseDeconstruction")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersUseDefaultLiteralConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "UseDefaultLiteral")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersUseExpressionBodyConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "UseExpressionBody")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersUseImplicitObjectCreationConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "UseImplicitObjectCreation")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersUseImplicitOrExplicitTypeConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "UseImplicitOrExplicitType")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersUseIndexOrRangeOperatorConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "UseIndexOrRangeOperator")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersUseInferredMemberNameConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "UseInferredMemberName")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersUseIsNullCheckConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "UseIsNullCheck")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersUseLocalFunctionConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "UseLocalFunction")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersUseNullPropagationConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "UseNullPropagation")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersUseObjectInitializerConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "UseObjectInitializer")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersUsePatternCombinatorsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "UsePatternCombinators")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersUsePatternMatchingConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "UsePatternMatching")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersUseSimpleUsingStatementConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "UseSimpleUsingStatement")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersUseThrowExpressionConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "UseThrowExpression")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpAnalyzersValidateFormatStringConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Analyzers", "ValidateFormatString")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesAddAccessibilityModifiersConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "AddAccessibilityModifiers")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesAddBracesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "AddBraces")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesConvertAnonymousTypeToTupleConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "ConvertAnonymousTypeToTuple")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesConvertSwitchStatementToExpressionConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "ConvertSwitchStatementToExpression")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesConvertTypeOfToNameOfConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "ConvertTypeOfToNameOf")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesFileHeadersConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "FileHeaders")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesInlineDeclarationConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "InlineDeclaration")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesInvokeDelegateWithConditionalAccessConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "InvokeDelegateWithConditionalAccess")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesMakeFieldReadonlyConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "MakeFieldReadonly")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesMakeStructFieldsWritableConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "MakeStructFieldsWritable")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesMatchFolderAndNamespaceConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "MatchFolderAndNamespace")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesMisplacedUsingDirectivesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "MisplacedUsingDirectives")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesOrderModifiersConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "OrderModifiers")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesPopulateSwitchConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "PopulateSwitch")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesQualifyMemberAccessConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "QualifyMemberAccess")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesRemoveConfusingSuppressionConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "RemoveConfusingSuppression")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesRemoveUnnecessaryCastConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "RemoveUnnecessaryCast")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesRemoveUnnecessaryDiscardDesignationConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "RemoveUnnecessaryDiscardDesignation")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesRemoveUnnecessaryImportsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "RemoveUnnecessaryImports")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesRemoveUnnecessaryParenthesesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "RemoveUnnecessaryParentheses")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesRemoveUnreachableCodeConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "RemoveUnreachableCode")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesRemoveUnusedMembersConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "RemoveUnusedMembers")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesRemoveUnusedParametersAndValuesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "RemoveUnusedParametersAndValues")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesSimplifyInterpolationConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "SimplifyInterpolation")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesSimplifyLinqExpressionConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "SimplifyLinqExpression")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesUseCollectionInitializerConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "UseCollectionInitializer")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesUseCompoundAssignmentConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "UseCompoundAssignment")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesUseConditionalExpressionConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "UseConditionalExpression")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesUseDeconstructionConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "UseDeconstruction")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesUseDefaultLiteralConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "UseDefaultLiteral")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesUseExpressionBodyConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "UseExpressionBody")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesUseImplicitObjectCreationConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "UseImplicitObjectCreation")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesUseImplicitOrExplicitTypeConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "UseImplicitOrExplicitType")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesUseIndexOrRangeOperatorConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "UseIndexOrRangeOperator")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesUseInferredMemberNameConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "UseInferredMemberName")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesUseIsNullCheckConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "UseIsNullCheck")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesUseNullPropagationConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "UseNullPropagation")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesUseObjectInitializerConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "UseObjectInitializer")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesUsePatternMatchingConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "UsePatternMatching")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpCodeFixesUseSimpleUsingStatementConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "CodeFixes", "UseSimpleUsingStatement")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function AnalyzersCSharpTestsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Analyzers", "CSharp", "Tests")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function CodeStyleCoreAnalyzersConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "CodeStyle", "Core", "Analyzers")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function CodeStyleCoreCodeFixesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "CodeStyle", "Core", "CodeFixes")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function CodeStyleCSharpAnalyzersConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "CodeStyle", "CSharp", "Analyzers")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function CodeStyleCSharpCodeFixesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "CodeStyle", "CSharp", "CodeFixes")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function CodeStyleCSharpTestsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "CodeStyle", "CSharp", "Tests")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function CodeStyleToolsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "CodeStyle", "Tools")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function CompilersCoreAnalyzerDriverConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "Core", "AnalyzerDriver")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function CompilersCoreCommandLineConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "Core", "CommandLine")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function CompilersCoreMSBuildTaskConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "Core", "MSBuildTask")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function CompilersCoreMSBuildTaskTestsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "Core", "MSBuildTaskTests")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function CompilersCSharpcscConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "csc")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function CompilersCSharpCSharpAnalyzerDriverConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "CSharpAnalyzerDriver")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function CompilersCSharpTestIOperationIOperationConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "Test", "IOperation", "IOperation")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function CompilersCSharpTestSemanticFlowAnalysisConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "Test", "Semantic", "FlowAnalysis")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function CompilersCSharpTestSemanticSourceGenerationConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "Test", "Semantic", "SourceGeneration")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function CompilersCSharpTestSemanticUtilitiesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "Test", "Semantic", "Utilities")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function CompilersCSharpTestWinRTConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "CSharp", "Test", "WinRT")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function CompilersExtensionConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "Extension")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function CompilersRealParserTestsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "RealParserTests")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function CompilersSharedConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "Shared")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function CompilersVisualBasicTestSemanticBindingConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "VisualBasic", "Test", "Semantic", "Binding")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function CompilersVisualBasicvbcConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Compilers", "VisualBasic", "vbc")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function DependenciesCodeAnalysisDebuggingConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Dependencies", "CodeAnalysis.Debugging")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function DependenciesCollectionsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Dependencies", "Collections")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function DependenciesPooledObjectsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Dependencies", "PooledObjects")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpAddImportsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp", "AddImports")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpAutomaticCompletionConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp", "AutomaticCompletion")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpBlockCommentEditingConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp", "BlockCommentEditing")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpBraceMatchingConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp", "BraceMatching")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpChangeSignatureConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp", "ChangeSignature")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpCommentSelectionConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp", "CommentSelection")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpCompleteStatementConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp", "CompleteStatement")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpContentTypeConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp", "ContentType")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpDecompiledSourceConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp", "DecompiledSource")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpDocumentationCommentsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp", "DocumentationComments")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpEmbeddedLanguagesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp", "EmbeddedLanguages")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpEncapsulateFieldConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp", "EncapsulateField")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpEndConstructConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp", "EndConstruct")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpEventHookupConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp", "EventHookup")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpExtractInterfaceConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp", "ExtractInterface")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpExtractMethodConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp", "ExtractMethod")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpFindUsagesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp", "FindUsages")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpFixInterpolatedVerbatimStringConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp", "FixInterpolatedVerbatimString")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpFormattingConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp", "Formatting")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpGoToBaseConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp", "GoToBase")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpGoToDefinitionConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp", "GoToDefinition")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpHighlightingKeywordHighlightersConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp", "Highlighting", "KeywordHighlighters")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpLanguageServicesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp", "LanguageServices")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpLineSeparatorsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp", "LineSeparators")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpNavigationBarConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp", "NavigationBar")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpRenameTrackingConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp", "RenameTracking")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpSplitCommentConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp", "SplitComment")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpSplitStringLiteralConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp", "SplitStringLiteral")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTest2RecommendationsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest2", "Recommendations")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestAddAnonymousTypeMemberNameConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "AddAnonymousTypeMemberName")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestAddDebuggerDisplayConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "AddDebuggerDisplay")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestAddFileBannerConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "AddFileBanner")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestAddObsoleteAttributeConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "AddObsoleteAttribute")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestAddParameterConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "AddParameter")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestAddUsingConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "AddUsing")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestAliasAmbiguousTypeConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "AliasAmbiguousType")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestAssignOutParametersConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "AssignOutParameters")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestAutomaticCompletionConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "AutomaticCompletion")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestBlockCommentEditingConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "BlockCommentEditing")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestBraceHighlightingConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "BraceHighlighting")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestBraceMatchingConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "BraceMatching")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestChangeSignatureConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "ChangeSignature")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestClassificationConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "Classification")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestCodeActionsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "CodeActions")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestCodeGenerationConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "CodeGeneration")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestCodeLensConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "CodeLens")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestCodeRefactoringsAddMissingImportsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "CodeRefactorings", "AddMissingImports")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestCodeRefactoringsUseExplicitOrImplicitTypeConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "CodeRefactorings", "UseExplicitOrImplicitType")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestCommentSelectionConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "CommentSelection")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestCompleteStatementConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "CompleteStatement")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestCompletionConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "Completion")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestConflictMarkerResolutionConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "ConflictMarkerResolution")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestConvertAnonymousTypeToClassConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "ConvertAnonymousTypeToClass")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestConvertAutoPropertyToFullPropertyConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "ConvertAutoPropertyToFullProperty")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestConvertBetweenRegularAndVerbatimStringConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "ConvertBetweenRegularAndVerbatimString")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestConvertCastConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "ConvertCast")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestConvertForEachToForConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "ConvertForEachToFor")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestConvertForToForEachConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "ConvertForToForEach")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestConvertToInterpolatedStringConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "ConvertToInterpolatedString")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestConvertTupleToStructConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "ConvertTupleToStruct")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestDebuggingConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "Debugging")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestDecompiledSourceConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "DecompiledSource")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestDisambiguateSameVariableConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "DisambiguateSameVariable")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestDocumentationCommentsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "DocumentationComments")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestEditAndContinueConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "EditAndContinue")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestEmbeddedLanguagesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "EmbeddedLanguages")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestEncapsulateFieldConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "EncapsulateField")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestEventHookupConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "EventHookup")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestExtensionsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "Extensions")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestExtractClassConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "ExtractClass")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestExtractInterfaceConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "ExtractInterface")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestExtractMethodConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "ExtractMethod")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestFixInterpolatedVerbatimStringConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "FixInterpolatedVerbatimString")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestFormattingConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "Formatting")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestFullyQualifyConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "FullyQualify")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestGenerateComparisonOperatorsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "GenerateComparisonOperators")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestGenerateConstructorConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "GenerateConstructor")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestGenerateDefaultConstructorsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "GenerateDefaultConstructors")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestGenerateFromMembersAddConstructorParametersFromMembersConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "GenerateFromMembers", "AddConstructorParametersFromMembers")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestGenerateFromMembersGenerateConstructorFromMembersConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "GenerateFromMembers", "GenerateConstructorFromMembers")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestGenerateFromMembersGenerateEqualsAndGetHashCodeFromMembersConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "GenerateFromMembers", "GenerateEqualsAndGetHashCodeFromMembers")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestGenerateOverridesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "GenerateOverrides")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestGenerateVariableConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "GenerateVariable")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestGoToAdjacentMemberConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "GoToAdjacentMember")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestImplementAbstractClassConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "ImplementAbstractClass")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestImplementInterfaceConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "ImplementInterface")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestInitializeParameterConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "InitializeParameter")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestInlineMethodConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "InlineMethod")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestInteractiveConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "Interactive")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestIntroduceUsingStatementConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "IntroduceUsingStatement")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestIntroduceVariableConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "IntroduceVariable")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestInvertConditionalConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "InvertConditional")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestInvertIfConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "InvertIf")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestInvertLogicalConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "InvertLogical")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestKeywordHighlightingConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "KeywordHighlighting")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestLineSeparatorsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "LineSeparators")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestMakeLocalFunctionStaticConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "MakeLocalFunctionStatic")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestMakeMemberStaticConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "MakeMemberStatic")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestMakeRefStructConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "MakeRefStruct")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestMakeTypeAbstractConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "MakeTypeAbstract")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestMoveDeclarationNearReferenceConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "MoveDeclarationNearReference")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestMoveToNamespaceConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "MoveToNamespace")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestNameTupleElementConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "NameTupleElement")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestNavigateToConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "NavigateTo")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestOrganizingConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "Organizing")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestPullMemberUpConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "PullMemberUp")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestQuickInfoConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "QuickInfo")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestRefactoringHelpersConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "RefactoringHelpers")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestRemoveUnnecessaryCastConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "RemoveUnnecessaryCast")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestRemoveUnusedLocalFunctionConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "RemoveUnusedLocalFunction")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestRemoveUnusedVariableConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "RemoveUnusedVariable")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestRenameConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "Rename")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestReorderParametersConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "ReorderParameters")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestReplaceDefaultLiteralConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "ReplaceDefaultLiteral")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestReplaceDocCommentTextWithTagConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "ReplaceDocCommentTextWithTag")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestReverseForStatementConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "ReverseForStatement")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestSemanticsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "Semantics")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestSignatureHelpConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "SignatureHelp")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestSimplifyThisOrMeConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "SimplifyThisOrMe")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestSimplifyTypeNamesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "SimplifyTypeNames")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestSplitCommentConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "SplitComment")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestSplitOrMergeIfStatementsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "SplitOrMergeIfStatements")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestSplitStringLiteralConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "SplitStringLiteral")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestSquigglesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "Squiggles")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestStructureConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "Structure")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestSuggestionTagsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "SuggestionTags")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestSymbolKeyConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "SymbolKey")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestTextStructureNavigationConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "TextStructureNavigation")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestTodoCommentConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "TodoComment")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestTypeInferrerConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "TypeInferrer")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestUnsealClassConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "UnsealClass")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestUseExplicitTypeForConstConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "UseExplicitTypeForConst")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestUseExpressionBodyForLambdaConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "UseExpressionBodyForLambda")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestUseExpressionBodyRefactoringConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "UseExpressionBody", "Refactoring")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestUseInterpolatedVerbatimStringConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "UseInterpolatedVerbatimString")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestUseNamedArgumentsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "UseNamedArguments")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestUsePatternCombinatorsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "UsePatternCombinators")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestUsePatternMatchingConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "UsePatternMatching")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestUtilitiesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "Utilities")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestWorkspacesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "Workspaces")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTestWrappingConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharpTest", "Wrapping")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpTextStructureNavigationConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp", "TextStructureNavigation")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesCSharpWpfConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "CSharp.Wpf")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesDiagnosticsTestUtilitiesChangeSignatureConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "DiagnosticsTestUtilities", "ChangeSignature")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesDiagnosticsTestUtilitiesCodeActionsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "DiagnosticsTestUtilities", "CodeActions")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesDiagnosticsTestUtilitiesDiagnosticsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "DiagnosticsTestUtilities", "Diagnostics")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesDiagnosticsTestUtilitiesMoveToNamespaceConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "DiagnosticsTestUtilities", "MoveToNamespace")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesDiagnosticsTestUtilitiesMoveTypeConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "DiagnosticsTestUtilities", "MoveType")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesDiagnosticsTestUtilitiesNamingStylesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "DiagnosticsTestUtilities", "NamingStyles")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesDiagnosticsTestUtilitiesSplitCommentsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "DiagnosticsTestUtilities", "SplitComments")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesTextConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "Text")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function EditorFeaturesXunitHookConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "EditorFeatures", "XunitHook")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function ExpressionEvaluatorCoreSourceFunctionResolverConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "ExpressionEvaluator", "Core", "Source", "FunctionResolver")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function ExpressionEvaluatorCoreTestExpressionCompilerConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "ExpressionEvaluator", "Core", "Test", "ExpressionCompiler")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function ExpressionEvaluatorCoreTestFunctionResolverConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "ExpressionEvaluator", "Core", "Test", "FunctionResolver")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function ExpressionEvaluatorCSharpSourceResultProviderConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "ExpressionEvaluator", "CSharp", "Source", "ResultProvider")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function ExpressionEvaluatorCSharpTestResultProviderConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "ExpressionEvaluator", "CSharp", "Test", "ResultProvider")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableAddAnonymousTypeMemberNameConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "AddAnonymousTypeMemberName")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableAddDebuggerDisplayConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "AddDebuggerDisplay")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableAddFileBannerConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "AddFileBanner")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableAddImportConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "AddImport")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableAddMissingReferenceConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "AddMissingReference")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableAddObsoleteAttributeConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "AddObsoleteAttribute")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableAddPackageConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "AddPackage")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableAddParameterConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "AddParameter")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableAliasAmbiguousTypeConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "AliasAmbiguousType")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableAssignOutParametersConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "AssignOutParameters")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableBraceCompletionConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "BraceCompletion")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableChangeSignatureConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "ChangeSignature")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableCodeCleanupConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "CodeCleanup")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableCodeFixesAddExplicitCastConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "CodeFixes", "AddExplicitCast")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableCodeFixesConditionalExpressionInStringInterpolationConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "CodeFixes", "ConditionalExpressionInStringInterpolation")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableCodeFixesConvertToAsyncConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "CodeFixes", "ConvertToAsync")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableCodeFixesFixReturnTypeConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "CodeFixes", "FixReturnType")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableCodeFixesGenerateEnumMemberConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "CodeFixes", "GenerateEnumMember")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableCodeFixesGenerateMethodConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "CodeFixes", "GenerateMethod")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableCodeFixesGenerateTypeConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "CodeFixes", "GenerateType")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableCodeFixesHideBaseConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "CodeFixes", "HideBase")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableCodeFixesIteratorConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "CodeFixes", "Iterator")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableCodeFixesMakeStatementAsynchronousConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "CodeFixes", "MakeStatementAsynchronous")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableCodeFixesNullableConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "CodeFixes", "Nullable")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableCodeFixesRemoveInKeywordConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "CodeFixes", "RemoveInKeyword")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableCodeFixesRemoveNewModifierConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "CodeFixes", "RemoveNewModifier")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableCodeFixesSuppressionConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "CodeFixes", "Suppression")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableCodeFixesUseInterpolatedVerbatimStringConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "CodeFixes", "UseInterpolatedVerbatimString")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableCodeLensConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "CodeLens")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableCommentSelectionConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "CommentSelection")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableConflictMarkerResolutionConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "ConflictMarkerResolution")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableConvertAnonymousTypeToClassConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "ConvertAnonymousTypeToClass")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableConvertAutoPropertyToFullPropertyConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "ConvertAutoPropertyToFullProperty")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableConvertBetweenRegularAndVerbatimStringConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "ConvertBetweenRegularAndVerbatimString")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableConvertCastConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "ConvertCast")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableConvertForEachToForConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "ConvertForEachToFor")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableConvertForToForEachConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "ConvertForToForEach")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableConvertIfToSwitchConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "ConvertIfToSwitch")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableConvertNumericLiteralConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "ConvertNumericLiteral")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableConvertToInterpolatedStringConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "ConvertToInterpolatedString")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableConvertTupleToStructConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "ConvertTupleToStruct")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableDebuggingConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "Debugging")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableDiagnosticsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "Diagnostics")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableDisambiguateSameVariableConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "DisambiguateSameVariable")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableDocumentationCommentsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "DocumentationComments")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableDocumentHighlightingConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "DocumentHighlighting")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableEmbeddedLanguagesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "EmbeddedLanguages")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableEncapsulateFieldConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "EncapsulateField")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableExternalAccessPythiaConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "ExternalAccess", "Pythia")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableExtractInterfaceConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "ExtractInterface")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableFullyQualifyConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "FullyQualify")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableGenerateConstructorConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "GenerateConstructor")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableGenerateConstructorFromMembersConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "GenerateConstructorFromMembers")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableGenerateEqualsAndGetHashCodeFromMembersConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "GenerateEqualsAndGetHashCodeFromMembers")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableGenerateMemberGenerateDefaultConstructorsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "GenerateMember", "GenerateDefaultConstructors")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableGenerateMemberGenerateEnumMemberConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "GenerateMember", "GenerateEnumMember")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableGenerateMemberGenerateParameterizedMemberConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "GenerateMember", "GenerateParameterizedMember")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableGenerateMemberGenerateVariableConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "GenerateMember", "GenerateVariable")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableGenerateTypeConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "GenerateType")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableGenerateVariableConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "GenerateVariable")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableGoToDefinitionConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "GoToDefinition")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableImplementAbstractClassConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "ImplementAbstractClass")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableImplementInterfaceConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "ImplementInterface")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableInitializeParameterConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "InitializeParameter")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableInlineHintsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "InlineHints")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableInternalUtilitiesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "InternalUtilities")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableIntroduceUsingStatementConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "IntroduceUsingStatement")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableIntroduceVariableConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "IntroduceVariable")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableInvertConditionalConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "InvertConditional")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableInvertIfConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "InvertIf")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableInvertLogicalConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "InvertLogical")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableLanguageServicesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "LanguageServices")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableMakeLocalFunctionStaticConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "MakeLocalFunctionStatic")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableMakeMemberStaticConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "MakeMemberStatic")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableMakeMethodAsynchronousConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "MakeMethodAsynchronous")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableMakeMethodSynchronousConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "MakeMethodSynchronous")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableMakeRefStructConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "MakeRefStruct")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableMakeTypeAbstractConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "MakeTypeAbstract")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableMetadataAsSourceConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "MetadataAsSource")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableMoveDeclarationNearReferenceConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "MoveDeclarationNearReference")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableMoveToNamespaceConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "MoveToNamespace")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableNameTupleElementConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "NameTupleElement")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableNavigateToConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "NavigateTo")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableNavigationBarConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "NavigationBar")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableOrganizingConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "Organizing")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableQuickInfoConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "QuickInfo")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableRemoveAsyncModifierConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "RemoveAsyncModifier")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableRemoveUnusedLocalFunctionConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "RemoveUnusedLocalFunction")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableRemoveUnusedVariableConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "RemoveUnusedVariable")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableReplaceDefaultLiteralConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "ReplaceDefaultLiteral")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableReplaceDocCommentTextWithTagConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "ReplaceDocCommentTextWithTag")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableReplaceMethodWithPropertyConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "ReplaceMethodWithProperty")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableReplacePropertyWithMethodsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "ReplacePropertyWithMethods")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableReverseForStatementConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "ReverseForStatement")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableSignatureHelpConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "SignatureHelp")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableSimplifyThisOrMeConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "SimplifyThisOrMe")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableSimplifyTypeNamesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "SimplifyTypeNames")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableSolutionCrawlerConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "SolutionCrawler")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableSpellCheckConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "SpellCheck")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableSplitOrMergeIfStatementsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "SplitOrMergeIfStatements")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableStructureConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "Structure")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableTodoCommentsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "TodoComments")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableUnsealClassConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "UnsealClass")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableUpdateProjectToAllowUnsafeConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "UpdateProjectToAllowUnsafe")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableUpgradeProjectConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "UpgradeProject")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableUseAutoPropertyConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "UseAutoProperty")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableUseExplicitTypeForConstConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "UseExplicitTypeForConst")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableUseExpressionBodyConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "UseExpressionBody")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableUseExpressionBodyForLambdaConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "UseExpressionBodyForLambda")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableUseLocalFunctionConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "UseLocalFunction")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableUseNamedArgumentsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "UseNamedArguments")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableUsePatternCombinatorsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "UsePatternCombinators")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableUsePatternMatchingConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "UsePatternMatching")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesCSharpPortableWrappingConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "CSharp", "Portable", "Wrapping")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesLanguageServerProtocolUnitTestsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "LanguageServer", "ProtocolUnitTests")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function FeaturesLsifGeneratorConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Features", "Lsif", "Generator")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function InteractivecsiConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Interactive", "csi")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function InteractiveHostInteractiveCoreConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Interactive", "Host", "Interactive", "Core")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function InteractiveHostProcessConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Interactive", "HostProcess")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function InteractiveHostTestConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Interactive", "HostTest")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function ScriptingCoreTestConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Scripting", "CoreTest")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function ScriptingCoreTestDesktopConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Scripting", "CoreTest.Desktop")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function ScriptingCoreTestUtilitiesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Scripting", "CoreTestUtilities")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function ScriptingCSharpConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Scripting", "CSharp")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function ScriptingCSharpTestConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Scripting", "CSharpTest")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function ScriptingCSharpTestDesktopConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Scripting", "CSharpTest.Desktop")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function TestDiagnosticsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Test", "Diagnostics")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function TestPdbUtilitiesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Test", "PdbUtilities")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function TestPerfStackDepthTestConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Test", "Perf", "StackDepthTest")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function TestPerftestshelloworldConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Test", "Perf", "tests", "helloworld")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function TestPerfUtilitiesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Test", "Perf", "Utilities")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function TestSharedConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Test", "Shared")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function ToolsAnalyzerRunnerConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Tools", "AnalyzerRunner")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function ToolsBuildActionTelemetryTableConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Tools", "BuildActionTelemetryTable")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function ToolsBuildBossConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Tools", "BuildBoss")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function ToolsBuildValidatorConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Tools", "BuildValidator")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function ToolsExternalAccessApexConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Tools", "ExternalAccess", "Apex")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function ToolsExternalAccessDebuggerConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Tools", "ExternalAccess", "Debugger")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function ToolsExternalAccessFSharpTestConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Tools", "ExternalAccess", "FSharpTest")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function ToolsExternalAccessRazorConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Tools", "ExternalAccess", "Razor")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function ToolsIdeBenchmarksConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Tools", "IdeBenchmarks")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function ToolsIdeCoreBenchmarksConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Tools", "IdeCoreBenchmarks")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function ToolsPrepareTestsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Tools", "PrepareTests")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function ToolsRoslynPublishConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Tools", "RoslynPublish")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function ToolsSourceCompilerGeneratorToolsSourceBoundTreeGeneratorConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Tools", "Source", "CompilerGeneratorTools", "Source", "BoundTreeGenerator")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function ToolsSourceCompilerGeneratorToolsSourceCSharpErrorFactsGeneratorConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Tools", "Source", "CompilerGeneratorTools", "Source", "CSharpErrorFactsGenerator")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function ToolsSourceCompilerGeneratorToolsSourceCSharpSyntaxGeneratorConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Tools", "Source", "CompilerGeneratorTools", "Source", "CSharpSyntaxGenerator")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function ToolsSourceCompilerGeneratorToolsSourceIOperationGeneratorConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Tools", "Source", "CompilerGeneratorTools", "Source", "IOperationGenerator")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function VisualStudioCodeLensConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "VisualStudio", "CodeLens")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function VisualStudioCSharpTestConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "VisualStudio", "CSharp", "Test")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function VisualStudioIntegrationTestIntegrationServiceConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "VisualStudio", "IntegrationTest", "IntegrationService")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function VisualStudioIntegrationTestIntegrationTestsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "VisualStudio", "IntegrationTest", "IntegrationTests")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function VisualStudioIntegrationTestTestSetupConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "VisualStudio", "IntegrationTest", "TestSetup")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function VisualStudioLiveShareImplConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "VisualStudio", "LiveShare", "Impl")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function VisualStudioLiveShareTestConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "VisualStudio", "LiveShare", "Test")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function VisualStudioRazorConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "VisualStudio", "Razor")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function VisualStudioVisualStudioDiagnosticsToolWindowConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "VisualStudio", "VisualStudioDiagnosticsToolWindow")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function VisualStudioXamlImplConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "VisualStudio", "Xaml", "Impl")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesCoreDesktopConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "Core", "Desktop")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesCoreMSBuildHostMefConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "Core", "MSBuild", "Host", "Mef")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesCSharpPortableCaseCorrectionConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "CSharp", "Portable", "CaseCorrection")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesCSharpPortableClassificationConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "CSharp", "Portable", "Classification")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesCSharpPortableCodeCleanupConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "CSharp", "Portable", "CodeCleanup")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesCSharpPortableCodeStyleConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "CSharp", "Portable", "CodeStyle")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesCSharpPortableCompositionConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "CSharp", "Portable", "Composition")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesCSharpPortableDiagnosticsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "CSharp", "Portable", "Diagnostics")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesCSharpPortableEditingConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "CSharp", "Portable", "Editing")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesCSharpPortableEmbeddedLanguagesLanguageServicesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "CSharp", "Portable", "EmbeddedLanguages", "LanguageServices")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesCSharpPortableExtensionsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "CSharp", "Portable", "Extensions")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesCSharpPortableExternalAccessPythiaApiConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "CSharp", "Portable", "ExternalAccess", "Pythia", "Api")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesCSharpPortableFindSymbolsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "CSharp", "Portable", "FindSymbols")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesCSharpPortableFormattingConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "CSharp", "Portable", "Formatting")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesCSharpPortableIndentationConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "CSharp", "Portable", "Indentation")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesCSharpPortableLanguageServicesSemanticsFactsServiceConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "CSharp", "Portable", "LanguageServices", "SemanticsFactsService")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesCSharpPortableLinkedFilesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "CSharp", "Portable", "LinkedFiles")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesCSharpPortableOrganizeImportsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "CSharp", "Portable", "OrganizeImports")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesCSharpPortableRecommendationsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "CSharp", "Portable", "Recommendations")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesCSharpPortableRenameConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "CSharp", "Portable", "Rename")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesCSharpPortableSemanticModelReuseConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "CSharp", "Portable", "SemanticModelReuse")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesCSharpPortableSerializationConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "CSharp", "Portable", "Serialization")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesCSharpPortableWorkspaceLanguageServicesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "CSharp", "Portable", "Workspace", "LanguageServices")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesCSharpTestConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "CSharpTest")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesDesktopTestConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "DesktopTest")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesMSBuildTestConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "MSBuildTest")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesRemoteRazorConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "Remote", "Razor")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesRemoteServiceHubExternalAccessPythiaApiConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "Remote", "ServiceHub", "ExternalAccess", "Pythia", "Api")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesRemoteServiceHubExternalAccessRazorApiConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "Remote", "ServiceHub", "ExternalAccess", "Razor", "Api")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesRemoteServiceHubExternalAccessUnitTestingApiConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "Remote", "ServiceHub", "ExternalAccess", "UnitTesting", "Api")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesRemoteServiceHubHostConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "Remote", "ServiceHub", "Host")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCoreCodeStyleConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "Core", "CodeStyle")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCoreCollectionsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "Core", "Collections")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCoreDiagnosticsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "Core", "Diagnostics")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCoreEditingConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "Core", "Editing")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCoreEmbeddedLanguagesCommonConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "Core", "EmbeddedLanguages", "Common")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCoreEmbeddedLanguagesVirtualCharsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "Core", "EmbeddedLanguages", "VirtualChars")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCoreFadingConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "Core", "Fading")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCoreFlowAnalysisConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "Core", "FlowAnalysis")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCoreHelpersRemoveUnnecessaryImportsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "Core", "Helpers", "RemoveUnnecessaryImports")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCoreLogConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "Core", "Log")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCoreNamingStylesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "Core", "NamingStyles")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCoreObjectPoolsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "Core", "ObjectPools")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCoreOptionsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "Core", "Options")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCoreServicesPrecedenceConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "Core", "Services", "Precedence")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCoreServicesSelectedMembersConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "Core", "Services", "SelectedMembers")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCoreServicesSemanticFactsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "Core", "Services", "SemanticFacts")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCoreServicesSyntaxFactsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "Core", "Services", "SyntaxFacts")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCoreSimplificationConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "Core", "Simplification")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCoreSystemTextConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "Core", "System.Text")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCoreTestHooksConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "Core", "TestHooks")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCSharpCodeStyleConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "CSharp", "CodeStyle")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCSharpEmbeddedLanguagesVirtualCharsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "CSharp", "EmbeddedLanguages", "VirtualChars")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCSharpHelpersRemoveUnnecessaryImportsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "CSharp", "Helpers", "RemoveUnnecessaryImports")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCSharpServicesPrecedenceConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "CSharp", "Services", "Precedence")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCSharpServicesSelectedMembersConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "CSharp", "Services", "SelectedMembers")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCSharpServicesSemanticFactsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "CSharp", "Services", "SemanticFacts")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCSharpSimplificationConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "CSharp", "Simplification")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsCompilerCSharpUtilitiesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Compiler", "CSharp", "Utilities")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsWorkspaceCoreCodeFixesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Workspace", "Core", "CodeFixes")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsWorkspaceCoreEmbeddedLanguagesVirtualCharsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Workspace", "Core", "EmbeddedLanguages", "VirtualChars")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsWorkspaceCoreExtensionsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Workspace", "Core", "Extensions")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsWorkspaceCoreHelpersConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Workspace", "Core", "Helpers")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsWorkspaceCoreLanguageServicesAddImportsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Workspace", "Core", "LanguageServices", "AddImports")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsWorkspaceCoreLanguageServicesCommandLineConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Workspace", "Core", "LanguageServices", "CommandLine")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsWorkspaceCoreLanguageServicesGeneratedCodeRecognitionConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Workspace", "Core", "LanguageServices", "GeneratedCodeRecognition")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsWorkspaceCoreLanguageServicesMoveDeclarationNearReferenceConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Workspace", "Core", "LanguageServices", "MoveDeclarationNearReference")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsWorkspaceCoreLanguageServicesRemoveUnnecessaryImportsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Workspace", "Core", "LanguageServices", "RemoveUnnecessaryImports")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsWorkspaceCoreLanguageServicesReplaceDiscardDeclarationsWithAssignmentsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Workspace", "Core", "LanguageServices", "ReplaceDiscardDeclarationsWithAssignments")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsWorkspaceCoreLanguageServicesSemanticsFactsServiceConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Workspace", "Core", "LanguageServices", "SemanticsFactsService")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsWorkspaceCoreLanguageServicesSymbolDeclarationServiceConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Workspace", "Core", "LanguageServices", "SymbolDeclarationService")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsWorkspaceCoreLanguageServicesSyntaxFactsServiceConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Workspace", "Core", "LanguageServices", "SyntaxFactsService")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsWorkspaceCoreLanguageServicesSyntaxGeneratorInternalExtensionsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Workspace", "Core", "LanguageServices", "SyntaxGeneratorInternalExtensions")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsWorkspaceCoreLanguageServicesTypeInferenceServiceConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Workspace", "Core", "LanguageServices", "TypeInferenceService")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsWorkspaceCoreLogConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Workspace", "Core", "Log")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsWorkspaceCoreRenameAnnotationsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Workspace", "Core", "Rename", "Annotations")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsWorkspaceCoreUtilitiesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Workspace", "Core", "Utilities")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsWorkspaceCoreWorkspaceMefConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Workspace", "Core", "Workspace", "Mef")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsWorkspaceCoreWorkspaceServicesSemanticModelReuseConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Workspace", "Core", "WorkspaceServices", "SemanticModelReuse")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsWorkspaceCSharpEmbeddedLanguagesVirtualCharsConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Workspace", "CSharp", "EmbeddedLanguages", "VirtualChars")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsWorkspaceCSharpLightupConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Workspace", "CSharp", "Lightup")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesSharedUtilitiesAndExtensionsWorkspaceCSharpUtilitiesConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "SharedUtilitiesAndExtensions", "Workspace", "CSharp", "Utilities")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

        <Trait("Category", "SkipWhenLiveUnitTesting")>
        <Fact>
        Public Async Function WorkspacesTestSourceGeneratorConvertAsync() As Task
            Assert.True(Await TestProcessDirectoryAsync(Path.Combine(GetRoslynRootDirectory(), "src", "Workspaces", "TestSourceGenerator")).ConfigureAwait(True), $"Failing file {_lastFileProcessed}")
        End Function

    End Class

End Namespace
