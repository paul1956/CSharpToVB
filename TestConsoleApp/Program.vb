Imports System.Collections.Immutable
Imports System.Reflection
Imports CVS_Generator
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic

Module Program

    Public Sub Main()
        Dim source = <![CDATA[
Imports RecordGenerator

Namespace Foo
  Public Class Person
    Sub Main()
        If s_DarkModeColorDictionary.Count <> s_LightModeColorDictionary.Count Then
            Throw New ApplicationException("s_DarkModeColorDictionary.Count <> s_LightModeColorDictionary.Count")
        End If
    End Sub
  End Class

End Namespace]]>.Value

        Dim result = GetGeneratedOutput(source)

        If result.Diagnostics.Length > 0 Then
            Console.WriteLine("Diagnostics:")
            For Each diag In result.Diagnostics
                Console.WriteLine("   " & diag.ToString())
            Next
            Console.WriteLine()
            Console.WriteLine("Output:")
        End If

        Console.WriteLine(result.Output)

    End Sub

    Private Function GetGeneratedOutput(source As String) As (Diagnostics As ImmutableArray(Of Diagnostic), Output As String)

        Dim syntaxTree = VisualBasicSyntaxTree.ParseText(source)

        Dim references As List(Of MetadataReference) = New List(Of MetadataReference)
        Dim assemblies As Assembly() = AppDomain.CurrentDomain.GetAssemblies()
        For Each assembly As Assembly In assemblies
            If Not assembly.IsDynamic Then
                references.Add(MetadataReference.CreateFromFile(assembly.Location))
            End If
        Next

        Dim compilation = VisualBasicCompilation.Create("Foo", New SyntaxTree() {syntaxTree}, references, New VisualBasicCompilationOptions(OutputKind.DynamicallyLinkedLibrary))

        ' TODO: Uncomment these lines if you want to return immediately if the injected program isn't valid _before_ running generators
        '
        'Dim compilationDiagnostics = compilation.GetDiagnostics()
        'If compilationDiagnostics.Any() Then
        '  Return (compilationDiagnostics, "")
        'End If

        Dim generator1 As ISourceGenerator = New SourceGenerator.CsvGenerator

        Dim iaGenerator = {generator1}.ToImmutableArray
        'Dim iaGenerator = New ImmutableArray(Of ISourceGenerator) From {generator1}

        Dim additionalFiles As ImmutableArray(Of AdditionalText) = ImmutableArray.Create(Of AdditionalText)(New AnalyzerAdditionalFile("Assets\DarkModeColorDictionary.csv"), New AnalyzerAdditionalFile("Assets\LightModeColorDictionary.csv"))
        Dim driver = VisualBasicGeneratorDriver.Create(iaGenerator,
                                                       additionalFiles,
                                                       Nothing,
                                                       Nothing)

        Dim outputCompilation As Compilation = Nothing
        Dim generateDiagnostics As ImmutableArray(Of Diagnostic) = Nothing
        driver.RunGeneratorsAndUpdateCompilation(compilation, outputCompilation, generateDiagnostics)

        Return (generateDiagnostics, $"{outputCompilation.SyntaxTrees(1)}{outputCompilation.SyntaxTrees(2)}")

    End Function

End Module
