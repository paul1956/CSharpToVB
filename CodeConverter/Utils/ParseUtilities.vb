' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports VB = Microsoft.CodeAnalysis.VisualBasic

Public Module ParseUtilities

    Public Function GetCSharpParseOptions(CSPreprocessorSymbols As List(Of String)) As CS.CSharpParseOptions
        Return New CS.CSharpParseOptions(
                                        CS.LanguageVersion.Latest,
                                        DocumentationMode.Parse,
                                        SourceCodeKind.Script,
                                        CSPreprocessorSymbols)
    End Function

    Public Function GetVBParseOptions(VBPreprocessorSymbols As List(Of KeyValuePair(Of String, Object))) As VB.VisualBasicParseOptions
        Return New VB.VisualBasicParseOptions(
                                    VB.LanguageVersion.Latest,
                                    DocumentationMode.Diagnose,
                                    SourceCodeKind.Regular,
                                    VBPreprocessorSymbols)
    End Function

    Public Function ParseCSharpSource(SourceText As String, CSPreprocessorSymbols As List(Of String)) As SyntaxTree
        Dim CSharpParseOptions As CS.CSharpParseOptions = GetCSharpParseOptions(CSPreprocessorSymbols)
        Dim ParsedCSharpTree As SyntaxTree = CS.SyntaxFactory.ParseSyntaxTree(
                                                            Text.SourceText.From(SourceText),
                                                            CSharpParseOptions
                                                            )
        Return ParsedCSharpTree
    End Function

End Module
