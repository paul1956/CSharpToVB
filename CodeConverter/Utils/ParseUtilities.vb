Option Explicit On
Option Infer Off
Option Strict On

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports VB = Microsoft.CodeAnalysis.VisualBasic

Public Module ParseUtilities

    Private ReadOnly PreprocessorSymbols As List(Of String) = New List(Of String) From {
                                                "NETSTANDARD2_0",
                                                "NET46",
                                                "NETCOREAPP2_0"
                                            }

    Public Function GetCSharpParseOptions(Optional CSPreprocessorSymbols As List(Of String) = Nothing) As CS.CSharpParseOptions
        If CSPreprocessorSymbols Is Nothing Then
            CSPreprocessorSymbols = PreprocessorSymbols
        End If
        Return New CS.CSharpParseOptions(
                                        CS.LanguageVersion.Latest,
                                        DocumentationMode.Parse,
                                        SourceCodeKind.Regular,
                                        CSPreprocessorSymbols)
    End Function

    Public Function GetVBParseOptions(Optional VBPreprocessorSymbols As List(Of KeyValuePair(Of String, Object)) = Nothing) As VB.VisualBasicParseOptions
        If VBPreprocessorSymbols Is Nothing Then
            VBPreprocessorSymbols = New List(Of KeyValuePair(Of String, Object))
            For Each Key As String In PreprocessorSymbols
                VBPreprocessorSymbols.Add(New KeyValuePair(Of String, Object)(Key, True))
            Next
        End If
        Return New VB.VisualBasicParseOptions(
                                    VB.LanguageVersion.VisualBasic16,
                                    DocumentationMode.Diagnose,
                                    SourceCodeKind.Regular,
                                    VBPreprocessorSymbols)
    End Function

    Public Function ParseCSharpSource(SourceText As String, Optional CSPreprocessorSymbols As List(Of String) = Nothing) As SyntaxTree
        If CSPreprocessorSymbols Is Nothing Then
            CSPreprocessorSymbols = PreprocessorSymbols
        End If

        Dim CSharpParseOptions As CS.CSharpParseOptions = GetCSharpParseOptions(CSPreprocessorSymbols)
        Dim ParsedCSharpTree As SyntaxTree = CS.SyntaxFactory.ParseSyntaxTree(
                                                            Text.SourceText.From(SourceText),
                                                            CSharpParseOptions
                                                            )
        Return ParsedCSharpTree
    End Function

End Module