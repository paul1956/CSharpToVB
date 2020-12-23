' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports VB = Microsoft.CodeAnalysis.VisualBasic

Namespace CSharpToVBConverter

    Public Module ParseUtilities

        Friend Function GetCSharpParseOptions(CSPreprocessorSymbols As List(Of String)) As CS.CSharpParseOptions
            Return New CS.CSharpParseOptions(
                                        CS.LanguageVersion.Latest,
                                        DocumentationMode.Parse,
                                        SourceCodeKind.Script,
                                        CSPreprocessorSymbols)
        End Function

        Friend Function GetVBParseOptions(VBPreprocessorSymbols As List(Of KeyValuePair(Of String, Object))) As VB.VisualBasicParseOptions
            Return New VB.VisualBasicParseOptions(
                                    VB.LanguageVersion.Latest,
                                    DocumentationMode.Diagnose,
                                    SourceCodeKind.Regular,
                                    VBPreprocessorSymbols)
        End Function

        Public Function ParseCSharpSource(SourceText As String, CSPreprocessorSymbols As List(Of String)) As SyntaxTree
            Return CS.SyntaxFactory.ParseSyntaxTree(Text.SourceText.From(SourceText),
                                                    GetCSharpParseOptions(CSPreprocessorSymbols)
                                                   )
        End Function

    End Module
End Namespace
