' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.Collections.Immutable
Imports System.IO
Imports System.Reflection

Imports CSharpToVBCodeConverter
Imports CSharpToVBCodeConverter.ConversionResult

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CSharp
Imports Microsoft.CodeAnalysis.Emit
Imports Microsoft.CodeAnalysis.VisualBasic

Public Module Compile

    ''' <summary>
    ''' Compile C# code
    ''' </summary>
    ''' <param name="StringToBeCompiled">String To Be Compiled</param>
    ''' <returns>Tuple(CompileSuccess, EmitResult)CompileSuccess is true unless compiler crashes</returns>
    Public Function CompileCSharpString(StringToBeCompiled As String) As (Success As Boolean, EmitResult)
        If String.IsNullOrEmpty(StringToBeCompiled) Then
            Return (True, Nothing)
        End If

        Dim assemblyName As String = Path.GetRandomFileName()
        Dim tree As SyntaxTree = CSharpSyntaxTree.ParseText(StringToBeCompiled)
        Dim compilation As CSharpCompilation = CSharpCompilation.Create(assemblyName, syntaxTrees:={tree}, CSharpReferences("", New List(Of MetadataReference)))
        Dim CompileResult As EmitResult = Nothing
        Dim CompileSuccess As Boolean = False
        Using ms As MemoryStream = New MemoryStream()
            Try
                CompileResult = compilation.Emit(ms)
                CompileSuccess = True
            Finally
                ' Ignore fatal compiler errors
            End Try
        End Using
        Return (CompileSuccess, CompileResult)
    End Function

    ''' <summary>
    ''' Compile VB code
    ''' </summary>
    ''' <param name="StringToBeCompiled"></param>
    ''' <param name="ErrorsToBeIgnored"></param>
    ''' <param name="SeverityToReport"></param>
    ''' <param name="ResultOfConversion"></param>
    ''' <returns>Tuple(CompileSuccess, EmitResult)CompileSuccess is true unless compiler crashes</returns>
    Public Function CompileVisualBasicString(StringToBeCompiled As String, ErrorsToBeIgnored As List(Of String), SeverityToReport As DiagnosticSeverity, ByRef ResultOfConversion As ConversionResult) As (CompileSuccess As Boolean, EmitResult)
        Contracts.Contract.Requires(ResultOfConversion IsNot Nothing)
        If String.IsNullOrWhiteSpace(StringToBeCompiled) Then
            ResultOfConversion.SetFilteredListOfFailures(New List(Of Diagnostic))
            ResultOfConversion.ResultStatus = ResultTriState.Success
            Return (True, Nothing)
        End If

        Dim PreprocessorSymbols As ImmutableArray(Of KeyValuePair(Of String, Object)) = ImmutableArray(Of KeyValuePair(Of String, Object)).Empty
        PreprocessorSymbols = PreprocessorSymbols.Add(KeyValuePair.Create(Of String, Object)(My.Settings.Framework, True))
        PreprocessorSymbols = AddPredefinedPreprocessorSymbols(OutputKind.DynamicallyLinkedLibrary, PreprocessorSymbols)

        Dim ParseOptions As VisualBasicParseOptions = New VisualBasicParseOptions(
                VisualBasic.LanguageVersion.Latest,
                DocumentationMode.Diagnose,
                kind:=SourceCodeKind.Regular,
                PreprocessorSymbols)
        Dim syntaxTree As SyntaxTree = VisualBasicSyntaxTree.ParseText(text:=StringToBeCompiled, options:=ParseOptions)
        Dim assemblyName As String = Path.GetRandomFileName()

        Dim CompilationOptions As VisualBasicCompilationOptions = New VisualBasicCompilationOptions(
                                                                            outputKind:=OutputKind.DynamicallyLinkedLibrary,
                                                                            optionExplicit:=False,
                                                                            optionInfer:=True,
                                                                            optionStrict:=OptionStrict.Off,
                                                                            parseOptions:=ParseOptions
                                                                            )
        Dim compilation As VisualBasicCompilation = VisualBasicCompilation.Create(
                                                                    assemblyName:=assemblyName,
                                                                    syntaxTrees:={syntaxTree},
                                                                    VisualBasicReferences(Assembly.Load("System.Windows.Forms").Location),
                                                                    options:=CompilationOptions
                                                                                  )

        Dim CompileResult As EmitResult = Nothing
        Dim CompileSuccess As Boolean = False
        Using ms As MemoryStream = New MemoryStream()
            Try
                CompileResult = compilation.Emit(ms)
                CompileSuccess = True
            Finally
                ' Ignore fatal compiler errors
            End Try
        End Using
        ResultOfConversion.SetFilteredListOfFailures(FilterDiagnostics(Diags:=compilation.GetParseDiagnostics(), SeverityToReport, ErrorsToBeIgnored))
        Return (CompileSuccess, CompileResult)
    End Function

    Private Function FilterDiagnostics(Diags As ImmutableArray(Of Diagnostic), Severity As DiagnosticSeverity, ErrorsToBeIgnored As List(Of String)) As List(Of Diagnostic)
        Contracts.Contract.Requires(ErrorsToBeIgnored IsNot Nothing)
        Dim FilteredDiagnostics As New List(Of Diagnostic)
        For Each Diag As Diagnostic In Diags
            If Diag.Location.IsInSource = True AndAlso Diag.Severity >= Severity Then
                If ErrorsToBeIgnored.Contains(Diag.Id.ToString(Globalization.CultureInfo.InvariantCulture), StringComparer.InvariantCultureIgnoreCase) Then
                    Continue For
                End If
                FilteredDiagnostics.Add(Diag)
            End If
        Next
        Return FilteredDiagnostics.OrderBy(Function(x As Diagnostic) x.Location.GetLineSpan.StartLinePosition.Line).ThenBy(Function(x As Diagnostic) x.Location.GetLineSpan.StartLinePosition.Character).ToList
    End Function

End Module
