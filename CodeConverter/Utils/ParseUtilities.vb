' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.IO
Imports System.Threading
Imports System.Windows.Forms

Imports CSharpToVBCodeConverter.Util

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports VB = Microsoft.CodeAnalysis.VisualBasic

Public Module ParseUtilities

    Public Function GetCSharpParseOptions(CSPreprocessorSymbols As List(Of String)) As CS.CSharpParseOptions
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
                Application.DoEvents()

                If SkipTestResourceFiles AndAlso
                        (Subdirectory.EndsWith("Test\Resources", StringComparison.OrdinalIgnoreCase) OrElse
                         Subdirectory.EndsWith("Setup\Templates", StringComparison.OrdinalIgnoreCase)) Then
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
                Application.DoEvents()

                If Not ParseCSharpSource(File, New List(Of String)).
                    GetRoot.SyntaxTree.IsGeneratedCode(Function(t As SyntaxTrivia) As Boolean
                                                           Return t.IsComment OrElse t.IsRegularOrDocComment
                                                       End Function, CancellationToken.None) Then
                    TotalFilesToProcess += 1
                End If
            Next
        Catch ex As OperationCanceledException
            Throw
        Catch ua As UnauthorizedAccessException
            ' Ignore
        Catch ex As Exception
            Stop
            Throw
        End Try

        Return TotalFilesToProcess
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
