' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

Imports System.Collections.Immutable
Imports System.Threading
Imports System.Windows.Forms
Imports CSharpToVBCodeConverter.Util
Imports CSharpToVBCodeConverter.Visual_Basic
Imports ManageProgressBar
Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports VB = Microsoft.CodeAnalysis.VisualBasic

Namespace CSharpToVBCodeConverter
    Public Module ConversionSupport
        Friend OriginalRequest As ConvertRequest
        Friend ImplementedMembersStack As New Stack(ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))).Empty)
        Friend ImplementedMembers As ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))) = (New List(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol)))).ToImmutableArray
        Friend ReadOnly UsedStacks As New Stack(New Dictionary(Of String, SymbolTableEntry))
        Friend UsedIdentifiers As New Dictionary(Of String, SymbolTableEntry)(StringComparer.Ordinal)
        Friend ThisLock As New Object

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

        Public Function ConvertInputRequest(RequestToConvert As ConvertRequest, OptionalReferences() As MetadataReference, mProgressBar As ReportProgress, CancelToken As CancellationToken) As ConversionResult
            If RequestToConvert Is Nothing Then
                Throw New ArgumentNullException(NameOf(RequestToConvert))
            End If
            Dim fromLanguage As String = LanguageNames.CSharp
            Dim toLanguage As String = LanguageNames.VisualBasic
            Dim fromVersion As Integer = GetDefaultVersionForLanguage("cs")
            Dim toVersion As Integer = GetDefaultVersionForLanguage("vb")
            Dim codeWithOptions As CodeWithOptions = (New CodeWithOptions(RequestToConvert)).SetFromLanguage(fromLanguage, fromVersion).SetToLanguage(toLanguage, toVersion)
            OriginalRequest = RequestToConvert
            SyncLock UsedStacks
                UsedStacks.Push(UsedIdentifiers)
                UsedIdentifiers.Clear()
            End SyncLock
            If RequestToConvert.SourceCode Is Nothing Then
                Throw New Exception($"{NameOf(RequestToConvert.SourceCode)} should not be Nothing")
            End If
            If OptionalReferences Is Nothing Then
                Throw New ArgumentNullException(NameOf(OptionalReferences))
            End If

            Dim CSharpParseOption As CS.CSharpParseOptions = GetCSharpParseOptions()
            Dim Tree As SyntaxTree = ParseCSharpSource(RequestToConvert.SourceCode)
            Dim CSharpOptions As CS.CSharpCompilationOptions = New CS.CSharpCompilationOptions(
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
            Dim compilation As Compilation = CS.CSharpCompilation.Create(
                                                        assemblyName:=NameOf(Conversion),
                                                        syntaxTrees:={Tree},
                                                        OptionalReferences,
                                                        options:=CSharpOptions)
            Try
                Dim SourceTree As CS.CSharpSyntaxNode = DirectCast(Tree.GetRoot, CS.CSharpSyntaxNode)
                If Not SourceTree.SyntaxTree.IsGeneratedCode(Function(t As SyntaxTrivia) As Boolean
                                                                 Return t.IsComment OrElse t.IsRegularOrDocComment
                                                             End Function, CancelToken) Then
                    Dim ConvertedNode As VB.VisualBasicSyntaxNode = CSharpConverter.Convert(
                                    SourceTree,
                                    codeWithOptions.Request.SkipAutoGenerated,
                                    compilation.GetSemanticModel(Tree, ignoreAccessibility:=True),
                                    mProgressBar,
                                    CancelToken
                                    )
                    UsedStacks.Clear()
                    UsedIdentifiers.Clear()
                    Return New ConversionResult(ConvertedNode, LanguageNames.CSharp, LanguageNames.VisualBasic)
                Else
                    Return New ConversionResult(Array.Empty(Of Exception))
                End If
            Catch ex As Exception
                Return New ConversionResult(ex)
            End Try
        End Function

    End Module
End Namespace
