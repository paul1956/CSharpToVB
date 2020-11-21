' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Collections.Immutable
Imports System.IO
Imports System.Reflection

Imports CSharpToVBConverter
Imports CSharpToVBConverter.ConversionResult

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
        Dim preprocessorSymbols As ImmutableArray(Of String) = ImmutableArray(Of String).Empty
        preprocessorSymbols = preprocessorSymbols.Add(My.Settings.Framework)
        Dim options As CSharpParseOptions = CSharpParseOptions.Default.WithPreprocessorSymbols(preprocessorSymbols).WithKind(SourceCodeKind.Script)
        Dim tree As SyntaxTree = CSharpSyntaxTree.ParseText(StringToBeCompiled, options)
        Dim compilation As CSharpCompilation = CSharpCompilation.Create(assemblyName, syntaxTrees:={tree}, SharedReferences.CSharpReferences("", New List(Of MetadataReference)))
        Dim compileResult As EmitResult = Nothing
        Dim compileSuccess As Boolean = False
        Using ms As New MemoryStream()
            Try
                compileResult = compilation.Emit(ms)
                compileSuccess = True
            Finally
                ' Ignore fatal compiler errors
            End Try
        End Using
        Return (compileSuccess, compileResult)
    End Function

    ''' <summary>
    ''' Compile VB code
    ''' </summary>
    ''' <param name="StringToBeCompiled"></param>
    ''' <param name="SeverityToReport"></param>
    ''' <param name="ResultOfConversion"></param>
    ''' <returns>Tuple(CompileSuccess, EmitResult)CompileSuccess is true unless compiler crashes</returns>
    Public Function CompileVisualBasicString(StringToBeCompiled As String, VBPreprocessorSymbols As List(Of KeyValuePair(Of String, Object)), SeverityToReport As DiagnosticSeverity, ByRef ResultOfConversion As ConversionResult) As (CompileSuccess As Boolean, EmitResult)
        Contracts.Contract.Requires(ResultOfConversion IsNot Nothing)
        If String.IsNullOrWhiteSpace(StringToBeCompiled) Then
            ResultOfConversion.SetFilteredListOfFailures(New List(Of Diagnostic))
            ResultOfConversion.ResultStatus = ResultTriState.Success
            Return (True, Nothing)
        End If
        Dim preprocessorSymbols As New List(Of KeyValuePair(Of String, Object))
        preprocessorSymbols.AddRange(AddPredefinedPreprocessorSymbols(OutputKind.DynamicallyLinkedLibrary,
                                                                      VBPreprocessorSymbols))

        Dim parseOptions As New VisualBasicParseOptions(
                VisualBasic.LanguageVersion.Latest,
                DocumentationMode.Diagnose,
                kind:=SourceCodeKind.Regular,
                preprocessorSymbols)
        Dim syntaxTree As SyntaxTree = VisualBasicSyntaxTree.ParseText(text:=StringToBeCompiled, options:=parseOptions)
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
            Finally
                ' Ignore fatal compiler errors
            End Try
        End Using
        ResultOfConversion.SetFilteredListOfFailures(FilterDiagnostics(Diags:=compilation.GetParseDiagnostics(), Severity:=SeverityToReport))
        Return (compileSuccess, compileResult)
    End Function

    Private Function FilterDiagnostics(Diags As ImmutableArray(Of Diagnostic), Severity As DiagnosticSeverity) As List(Of Diagnostic)
        Dim filteredDiagnostics As New List(Of Diagnostic)
        For Each diag As Diagnostic In Diags
            If diag.Location.IsInSource = True AndAlso diag.Severity >= Severity Then
                filteredDiagnostics.Add(diag)
            End If
        Next
        Return filteredDiagnostics.OrderBy(Function(x As Diagnostic) x.Location.GetLineSpan.StartLinePosition.Line).ThenBy(Function(x As Diagnostic) x.Location.GetLineSpan.StartLinePosition.Character).ToList
    End Function

End Module
