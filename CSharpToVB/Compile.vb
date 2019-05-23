Option Explicit On
Option Infer Off
Option Strict On

Imports System.Collections.Immutable
Imports System.IO
Imports System.Text.RegularExpressions
Imports IVisualBasicCode.CodeConverter
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Emit
Imports Microsoft.CodeAnalysis.VisualBasic

Public Module Compile
    Public Function CompileVisualBasicString(StringToBeCompiled As String, ErrorsToBeIgnored() As String, SeverityToReport As DiagnosticSeverity, ByRef ResultOfConversion As ConversionResult) As EmitResult
        If StringToBeCompiled.IsEmptyNullOrWhitespace Then
            ResultOfConversion.FilteredListOfFailures = New List(Of Diagnostic)
            ResultOfConversion.Success = True
            Return Nothing
        End If

        Dim PreprocessorSymbols As ImmutableArray(Of KeyValuePair(Of String, Object)) = ImmutableArray(Of KeyValuePair(Of String, Object)).Empty
        PreprocessorSymbols = PreprocessorSymbols.Add(New KeyValuePair(Of String, Object)("NETSTANDARD2_0", True))
        PreprocessorSymbols = PreprocessorSymbols.Add(New KeyValuePair(Of String, Object)("NET46", True))
        PreprocessorSymbols = PreprocessorSymbols.Add(New KeyValuePair(Of String, Object)("NETCOREAPP2_0", True))
        PreprocessorSymbols = AddPredefinedPreprocessorSymbols(OutputKind.DynamicallyLinkedLibrary, PreprocessorSymbols)

        Dim ParseOptions As VisualBasicParseOptions = New VisualBasicParseOptions(
                LanguageVersion.VisualBasic16,
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
                                                                    references:=References,
                                                                    options:=CompilationOptions
                                                                                  )

        Dim CompileResult As EmitResult = Nothing
        Using ms As MemoryStream = New MemoryStream()
            Try
                CompileResult = compilation.Emit(ms)
            Catch ex As Exception
                ' Ignore fatal compiler errors
            End Try
        End Using
        ResultOfConversion.FilteredListOfFailures = FilterDiagnostics(Diags:=compilation.GetParseDiagnostics(), Severity:=SeverityToReport, ErrorsToBeIgnored:=ErrorsToBeIgnored)
        Return CompileResult
    End Function

    Private Function FilterDiagnostics(Diags As ImmutableArray(Of Diagnostic), Severity As DiagnosticSeverity, ErrorsToBeIgnored As String()) As List(Of Diagnostic)
        Dim FilteredDiagnostics As New List(Of Diagnostic)
        For Each Diag As Diagnostic In Diags
            If Diag.Location.IsInSource = True AndAlso Diag.Severity >= Severity Then
                If ErrorsToBeIgnored.Contains(Diag.Id.ToString) Then
                    Continue For
                End If
                FilteredDiagnostics.Add(Diag)
            End If
        Next
        Return FilteredDiagnostics.OrderBy(Function(x As Diagnostic) x.Location.GetLineSpan.StartLinePosition.Line).ThenBy(Function(x As Diagnostic) x.Location.GetLineSpan.StartLinePosition.Character).ToList
    End Function

End Module
