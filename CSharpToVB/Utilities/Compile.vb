' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Collections.Immutable
Imports System.IO
Imports System.Reflection
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Emit
Imports Microsoft.CodeAnalysis.VisualBasic

Public Module Compile

    Private Function FilterDiagnostics(diags As ImmutableArray(Of Diagnostic), severity As DiagnosticSeverity) As List(Of Diagnostic)
        Dim filteredDiagnostics As New List(Of Diagnostic)
        For Each diag As Diagnostic In diags
            If diag.Location.IsInSource = True AndAlso diag.Severity >= severity Then
                filteredDiagnostics.Add(diag)
            End If
        Next
        Return filteredDiagnostics.OrderBy(Function(x As Diagnostic) x.Location.GetLineSpan.StartLinePosition.Line).ThenBy(Function(x As Diagnostic) x.Location.GetLineSpan.StartLinePosition.Character).ToList
    End Function

    '' Save for future work
    '''' <summary>
    '''' Compile C# code
    '''' </summary>
    '''' <param name="stringToBeCompiled">String To Be Compiled</param>
    '''' <returns>Tuple(CompileSuccess, EmitResult)CompileSuccess is true unless compiler crashes</returns>
    '<ExcludeFromCodeCoverage>
    'Public Function CompileCSharpString(stringToBeCompiled As String) As (Success As Boolean, EmitResult)
    '    If String.IsNullOrEmpty(stringToBeCompiled) Then
    '        Return (True, Nothing)
    '    End If

    '    Dim assemblyName As String = Path.GetRandomFileName()
    '    Dim preprocessorSymbols As ImmutableArray(Of String) = ImmutableArray(Of String).Empty
    '    preprocessorSymbols = preprocessorSymbols.Add(My.Settings.Framework)
    '    Dim options As CSharp.CSharpParseOptions = CSharp.CSharpParseOptions.Default.WithPreprocessorSymbols(preprocessorSymbols).WithKind(SourceCodeKind.Script)
    '    Dim tree As SyntaxTree = CSharp.CSharpSyntaxTree.ParseText(stringToBeCompiled, options)
    '    Dim compilation As CSharp.CSharpCompilation = CSharp.CSharpCompilation.Create(assemblyName, syntaxTrees:={tree}, SharedReferences.CSharpReferences("", New List(Of MetadataReference)))
    '    Dim compileResult As EmitResult = Nothing
    '    Dim compileSuccess As Boolean = False
    '    Using ms As New MemoryStream()
    '        Try
    '            compileResult = compilation.Emit(ms)
    '            compileSuccess = True
    '        Finally
    '            ' Ignore fatal compiler errors
    '        End Try
    '    End Using
    '    Return (compileSuccess, compileResult)
    'End Function

    ''' <summary>
    ''' Compile VB code
    ''' </summary>
    ''' <param name="stringToBeCompiled"></param>
    ''' <param name="severityToReport"></param>
    ''' <param name="resultOfConversion"></param>
    ''' <returns>Tuple(CompileSuccess, EmitResult)CompileSuccess is true unless compiler crashes</returns>
    Public Function CompileVisualBasicString(stringToBeCompiled As String, vbPreprocessorSymbols As List(Of KeyValuePair(Of String, Object)), severityToReport As DiagnosticSeverity, ByRef resultOfConversion As ConversionResult) As (CompileSuccess As Boolean, EmitResult)
        If resultOfConversion Is Nothing Then
            Throw New ArgumentNullException(NameOf(resultOfConversion))
        End If

        If String.IsNullOrWhiteSpace(stringToBeCompiled) Then
            resultOfConversion.SetFilteredListOfFailures(New List(Of Diagnostic))
            resultOfConversion.ResultStatus = ConversionResult.ResultTriState.Success
            Return (True, Nothing)
        End If
        Dim preprocessorSymbols As New List(Of KeyValuePair(Of String, Object))
        preprocessorSymbols.AddRange(AddPredefinedPreprocessorSymbols(OutputKind.DynamicallyLinkedLibrary,
                                                                      vbPreprocessorSymbols))

        Dim parseOptions As New VisualBasicParseOptions(LanguageVersion.Latest,
                                                        DocumentationMode.Diagnose,
                                                        kind:=SourceCodeKind.Regular,
                                                        preprocessorSymbols)
        Dim syntaxTree As SyntaxTree = VisualBasicSyntaxTree.ParseText(text:=stringToBeCompiled, options:=parseOptions)
        Dim assemblyName As String = Path.GetRandomFileName()

        Dim compilationOptions As New VisualBasicCompilationOptions(outputKind:=OutputKind.DynamicallyLinkedLibrary,
                                                                    optionExplicit:=False,
                                                                    optionInfer:=True,
                                                                    optionStrict:=OptionStrict.Off,
                                                                    parseOptions:=parseOptions)
        Dim compilation As VisualBasicCompilation =
            VisualBasicCompilation.Create(assemblyName,
                                          {syntaxTree},
                                          VisualBasicReferences(Assembly.Load("System.Windows.Forms").Location),
                                          compilationOptions
                                          )
        Dim compileResult As EmitResult = Nothing
        Dim compileSuccess As Boolean = False
        Using ms As New MemoryStream()
            Try
                compileResult = compilation.Emit(ms)
                compileSuccess = True
            Catch ex As ObjectDisposedException
                End
            Catch
                ' Ignore fatal compiler errors
            End Try
        End Using
        resultOfConversion.SetFilteredListOfFailures(FilterDiagnostics(diags:=compilation.GetParseDiagnostics(), severity:=severityToReport))
        Return (compileSuccess, compileResult)
    End Function

End Module
