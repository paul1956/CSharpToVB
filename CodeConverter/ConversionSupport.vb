' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Collections.Immutable
Imports System.Threading

Imports CSharpToVBCodeConverter.DestVisualBasic
Imports CSharpToVBCodeConverter.Util

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports VB = Microsoft.CodeAnalysis.VisualBasic

Namespace CSharpToVBCodeConverter
    Public Module ConversionSupport
        Friend ReadOnly s_usedStacks As New Stack(New Dictionary(Of String, SymbolTableEntry)(StringComparer.Ordinal))
        Friend s_implementedMembers As ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))) = (New List(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol)))).ToImmutableArray
        Friend s_implementedMembersStack As New Stack(ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))).Empty)
        Friend s_originalRequest As ConvertRequest
        Friend s_thisLock As New Object
        Friend s_usedIdentifiers As New Dictionary(Of String, SymbolTableEntry)(StringComparer.Ordinal)

        Private Function GetDefaultVersionForLanguage(language As String) As Integer
            If language Is Nothing Then
                Throw New ArgumentNullException(NameOf(language))
            End If
            If language.StartsWith("cs", StringComparison.OrdinalIgnoreCase) Then
                Return CS.LanguageVersion.Latest
            End If
            If language.StartsWith("vb", StringComparison.OrdinalIgnoreCase) Then
                Return VB.LanguageVersion.Latest
            End If
            Throw New ArgumentException($"{language} not supported!")
        End Function

        Public Function ConvertInputRequest(RequestToConvert As ConvertRequest, DefaultVBOptions As DefaultVBOptions, CSPreprocessorSymbols As List(Of String), VBPreprocessorSymbols As List(Of KeyValuePair(Of String, Object)), OptionalReferences() As MetadataReference, ReportException As Action(Of Exception), mProgress As IProgress(Of ProgressReport), CancelToken As CancellationToken) As ConversionResult
            If RequestToConvert Is Nothing Then
                Throw New ArgumentNullException(NameOf(RequestToConvert))
            End If
            Dim fromLanguage As String = LanguageNames.CSharp
            Dim toLanguage As String = LanguageNames.VisualBasic
            Dim fromVersion As Integer = GetDefaultVersionForLanguage("cs")
            Dim toVersion As Integer = GetDefaultVersionForLanguage("vb")
            Dim codeWithOptions As CodeWithOptions = (New CodeWithOptions(RequestToConvert)).SetFromLanguage(fromLanguage, fromVersion).SetToLanguage(toLanguage, toVersion)
            s_originalRequest = RequestToConvert
            SyncLock s_usedStacks
                s_usedStacks.Push(s_usedIdentifiers)
                s_usedIdentifiers.Clear()
            End SyncLock
            If RequestToConvert.SourceCode Is Nothing Then
                Throw New Exception($"{NameOf(RequestToConvert.SourceCode)} should not be Nothing")
            End If
            If OptionalReferences Is Nothing Then
                Throw New ArgumentNullException(NameOf(OptionalReferences))
            End If
            Dim Tree As SyntaxTree = ParseCSharpSource(RequestToConvert.SourceCode, CSPreprocessorSymbols)
            Dim CSharpOptions As CS.CSharpCompilationOptions =
                New CS.CSharpCompilationOptions(
                                outputKind:=Nothing,
                                reportSuppressedDiagnostics:=False,
                                moduleName:=Nothing,
                                mainTypeName:=Nothing,
                                scriptClassName:=Nothing,
                                usings:=Nothing,
                                OptimizationLevel.Debug,
                                checkOverflow:=False,
                                allowUnsafe:=True,
                                cryptoKeyContainer:=Nothing,
                                cryptoKeyFile:=Nothing,
                                cryptoPublicKey:=Nothing,
                                delaySign:=Nothing,
                                Platform.AnyCpu,
                                ReportDiagnostic.Default,
                                warningLevel:=4,
                                specificDiagnosticOptions:=Nothing,
                                concurrentBuild:=True,
                                deterministic:=False,
                                xmlReferenceResolver:=Nothing,
                                sourceReferenceResolver:=Nothing,
                                metadataReferenceResolver:=Nothing,
                                assemblyIdentityComparer:=Nothing,
                                strongNameProvider:=Nothing,
                                publicSign:=False
                                )
            Dim compilation As Compilation =
                CS.CSharpCompilation.Create(assemblyName:=NameOf(Conversion),
                                            {Tree},
                                            OptionalReferences,
                                            CSharpOptions
                                            )
            Try
                Dim SourceTree As CS.CSharpSyntaxNode = DirectCast(Tree.GetRoot, CS.CSharpSyntaxNode)
                If RequestToConvert.SkipAutoGenerated AndAlso
                    SourceTree.SyntaxTree.IsGeneratedCode(Function(t As SyntaxTrivia) As Boolean
                                                              Return t.IsComment OrElse t.IsRegularOrDocComment
                                                          End Function, CancelToken) Then
                    Return New ConversionResult(Array.Empty(Of Exception))
                Else
                    Dim ConvertedNode As VB.VisualBasicSyntaxNode =
                            CSharpConverter.Convert(
                                SourceTree,
                                codeWithOptions.Request.SkipAutoGenerated,
                                DefaultVBOptions,
                                compilation.GetSemanticModel(Tree, ignoreAccessibility:=True),
                                ReportException,
                                mProgress,
                                CancelToken
                                )
                    s_usedStacks.Clear()
                    s_usedIdentifiers.Clear()
                    Return New ConversionResult(ConvertedNode, LanguageNames.CSharp, LanguageNames.VisualBasic, VBPreprocessorSymbols)
                End If
            Catch ex As OperationCanceledException
                Return New ConversionResult(Array.Empty(Of Exception))
            Catch ex As Exception
                Return New ConversionResult(ex)
            End Try
        End Function

        <Obsolete("Don't use this routine any more. Use the new one instead to specify what 'Options' to include in output, use 'New DefaultVBOptions' to get the legacy behavior.")>
        Public Function ConvertInputRequest(RequestToConvert As ConvertRequest, CSPreprocessorSymbols As List(Of String), VBPreprocessorSymbols As List(Of KeyValuePair(Of String, Object)), OptionalReferences() As MetadataReference, ReportException As Action(Of Exception), mProgress As IProgress(Of ProgressReport), CancelToken As CancellationToken) As ConversionResult
            Return ConvertInputRequest(RequestToConvert, New DefaultVBOptions, CSPreprocessorSymbols, VBPreprocessorSymbols, OptionalReferences, ReportException, mProgress, CancelToken)
        End Function

    End Module
End Namespace
