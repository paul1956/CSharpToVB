' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

Imports IVisualBasicCode.CodeConverter.Util
Imports IVisualBasicCode.CodeConverter.Visual_Basic

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports VB = Microsoft.CodeAnalysis.VisualBasic

Namespace IVisualBasicCode.CodeConverter
    Public Module CodeConverter
        Friend OriginalRequest As ConvertRequest
        Friend ReadOnly UsedIdentifierStack As New Stack(New Dictionary(Of String, SymbolTableEntry))
        Friend ThisLock As New Object
        Friend UsedIdentifiers As New Dictionary(Of String, SymbolTableEntry)(StringComparer.Ordinal)

        Private Function GetDefaultVersionForLanguage(language As String) As Integer
            If language Is Nothing Then
                Throw New ArgumentNullException(NameOf(language))
            End If
            If language.StartsWith("cs", StringComparison.OrdinalIgnoreCase) Then
                Return CSharp.LanguageVersion.Latest
            End If
            If language.StartsWith("vb", StringComparison.OrdinalIgnoreCase) Then
                Return VisualBasic.LanguageVersion.Latest
            End If
            Throw New ArgumentException($"{language} not supported!")
        End Function

        Private Function ParseLanguage(language As String) As String
            If language Is Nothing Then
                Throw New ArgumentNullException(NameOf(language))
            End If
            If language.StartsWith("cs", StringComparison.OrdinalIgnoreCase) Then
                Return LanguageNames.CSharp
            End If
            If language.StartsWith("vb", StringComparison.OrdinalIgnoreCase) Then
                Return LanguageNames.VisualBasic
            End If
            Throw New ArgumentException($"{language} not supported!")
        End Function

        Public Function ConvertInputRequest(RequestToConvert As ConvertRequest, OptionalReferences() As MetadataReference) As ConversionResult
            Dim languages As String() = {RequestToConvert.GetSourceExtension, RequestToConvert.GetTargetExtension}
            Debug.Assert(languages.Length = 2)
            Dim fromLanguage As String = ParseLanguage(languages(0))
            Dim toLanguage As String = ParseLanguage(languages(1))
            Dim fromVersion As Integer = GetDefaultVersionForLanguage(languages(0))
            Dim toVersion As Integer = GetDefaultVersionForLanguage(languages(1))
            Dim codeWithOptions As CodeWithOptions = (New CodeWithOptions(RequestToConvert)).SetFromLanguage(fromLanguage, fromVersion).SetToLanguage(toLanguage, toVersion)
            OriginalRequest = RequestToConvert
            UsedIdentifierStack.Push(UsedIdentifiers)
            UsedIdentifiers.Clear()
            If RequestToConvert.SourceCode Is Nothing Then
                Throw New ArgumentNullException(NameOf(RequestToConvert.SourceCode))
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
                                                             End Function, cancellationToken:=Nothing) Then
                    Dim ConvertedNode As VB.VisualBasicSyntaxNode = CSharpConverter.Convert(
                                    SourceTree,
                                    codeWithOptions.Request.SkipAutoGenerated,
                                    compilation.GetSemanticModel(Tree, ignoreAccessibility:=True)
                                    )
                    UsedIdentifierStack.Clear()
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