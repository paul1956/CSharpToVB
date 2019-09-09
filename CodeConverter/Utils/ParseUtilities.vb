' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

Imports System.IO
Imports System.Threading

Imports CSharpToVBCodeConverter.Util

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

    Public Function GetFileCount(DirPath As String, SourceLanguageExtension As String, SkipBinAndObjFolders As Boolean, SkipTestResourceFiles As Boolean, Optional Depth As Integer = 0) As Long
        Dim TotalFilesToProcess As Long = 0L
        Try
            For Each Subdirectory As String In Directory.GetDirectories(DirPath)
                If SkipTestResourceFiles AndAlso
                        (Subdirectory.EndsWith("Test\Resources", StringComparison.InvariantCultureIgnoreCase) OrElse
                         Subdirectory.EndsWith("Setup\Templates", StringComparison.InvariantCultureIgnoreCase)) Then
                    Continue For
                End If
                If SkipBinAndObjFolders AndAlso (Subdirectory = "bin" OrElse Subdirectory = "obj" OrElse Subdirectory = "g") Then
                    Continue For
                End If
                Dim TempFileCount As Long = GetFileCount(Subdirectory, SourceLanguageExtension, SkipBinAndObjFolders, SkipTestResourceFiles, Depth + 1)
                If Depth > 0 Then
                    TotalFilesToProcess += TempFileCount
                Else
                    TotalFilesToProcess = TempFileCount
                End If
            Next
            For Each File As String In Directory.GetFiles(path:=DirPath, searchPattern:=$"*.{SourceLanguageExtension}")

                If Not ParseCSharpSource(File).GetRoot.SyntaxTree.IsGeneratedCode(Function(t As SyntaxTrivia) As Boolean
                                                                                      Return t.IsComment OrElse t.IsRegularOrDocComment
                                                                                  End Function, CancellationToken.None) Then
                    TotalFilesToProcess += 1
                End If
            Next
        Catch ua As UnauthorizedAccessException
            'Stop
        Catch ex As Exception
            'Stop
        End Try
        Return TotalFilesToProcess
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
