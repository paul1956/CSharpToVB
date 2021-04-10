' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports VB = Microsoft.CodeAnalysis.VisualBasic

Public Module ParseUtilities

    Private Function GetCSharpParseOptions(csPreprocessorSymbols As List(Of String)) As CS.CSharpParseOptions
        Return New CS.CSharpParseOptions(CS.LanguageVersion.Latest,
                                         DocumentationMode.Parse,
                                         SourceCodeKind.Script,
                                         csPreprocessorSymbols)
    End Function

    Friend Function GetVbParseOptions(vbPreprocessorSymbols As List(Of KeyValuePair(Of String, Object))) As VB.VisualBasicParseOptions
        Return New VB.VisualBasicParseOptions(VB.LanguageVersion.Latest,
                                              DocumentationMode.Diagnose,
                                              SourceCodeKind.Regular,
                                              vbPreprocessorSymbols)
    End Function

    Public Function ParseCSharpSource(sourceText As String, csPreprocessorSymbols As List(Of String)) As SyntaxTree
        Return CS.SyntaxFactory.ParseSyntaxTree(Text.SourceText.From(sourceText),
                                                GetCSharpParseOptions(csPreprocessorSymbols)
                                               )
    End Function

End Module
