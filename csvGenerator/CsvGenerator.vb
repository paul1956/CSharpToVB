Imports System.IO
Imports System.Text

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Text

Namespace SourceGenerator

    <Generator(LanguageNames.VisualBasic)>
    Public Class CsvGenerator
        Implements ISourceGenerator

        Public Enum CsvLoadType
            Startup
            OnDemand
        End Enum

        Public Sub Initialize(context As GeneratorInitializationContext) Implements Microsoft.CodeAnalysis.ISourceGenerator.Initialize

        End Sub

        Public Sub Execute(context As GeneratorExecutionContext) Implements ISourceGenerator.Execute
            Dim options As IEnumerable(Of (CsvLoadType, Boolean, AdditionalText)) = GetLoadOptions(context)
            Dim nameCodeSequence As IEnumerable(Of (Name As String, Code As String)) = SourceFilesFromAdditionalFiles(options)
            For Each entry As (name As String, code As String) In nameCodeSequence
                context.AddSource($"Csv_{entry.name}", SourceText.From(entry.code, Encoding.UTF8))
            Next
        End Sub

        ' Guesses type of property for the object from the value of a csv field
        Public Shared Function GetCsvFieldType(exemplar As String) As String
            Dim garbageBoolean As Boolean
            Dim garbageInteger As Integer
            Dim garbageDouble As Double
            Select Case True
                Case Boolean.TryParse(exemplar, garbageBoolean) : Return "Boolean"
                Case Integer.TryParse(exemplar, garbageInteger) : Return "Integer"
                Case Double.TryParse(exemplar, garbageDouble) : Return "Double"
                Case Else : Return "String"
            End Select
        End Function

        ' Examines the header row and the first row in the csv file to gather all header types and names
        ' Also it returns the first row of data, because it must be read to figure out the types,
        ' As the CsvTextFieldParser cannot 'Peek' ahead of one line. If there is no first line,
        ' it consider all properties as strings. The generator returns an empty list of properly
        ' typed objects in such case. If the file is completely empty, an error is generated.
        Public Shared Function ExtractProperties(parser As TextFieldParser) As (Types As String(), Names As String(), Fields As String())

            Dim headerFields() As String = parser.ReadFields()
            If headerFields Is Nothing Then
                Throw New Exception("Empty csv file!")
            End If

            Dim firstLineFields() As String = parser.ReadFields()
            If firstLineFields Is Nothing Then
                Return (Enumerable.Repeat("String", headerFields.Length).ToArray(), headerFields, firstLineFields)
            Else
                Return (firstLineFields.[Select](Function(field) GetCsvFieldType(field)).ToArray(), headerFields.[Select](New Func(Of String, String)(AddressOf StringToValidPropertyName)).ToArray(), firstLineFields)
            End If

        End Function

        ' Adds a partial Module with a color directory containing all the Names and colors for Classification
        Public Shared Function GenerateColorDictionary(ColorDirectoryName As String, csvText As String, loadTime As CsvLoadType, cacheObjects As Boolean) As String

            Dim sb As New StringBuilder(50_000)
            Dim parser As New TextFieldParser(New StringReader(csvText))
            parser.SetDelimiters({","})
            parser.TextFieldType = FieldType.Delimited

            ''' Module Definition
            sb.AppendLine("Partial Public Module ColorSelector")

            sb.Append($"    Friend s_{ColorDirectoryName} As New Dictionary(Of String, ColorDescriptor)(StringComparer.OrdinalIgnoreCase)")
            If loadTime = CsvLoadType.Startup OrElse cacheObjects Then
                sb.AppendLine(" From {")
                Dim tupleTemp As (Types As String(), Names As String(), Fields As String()) = ExtractProperties(parser) : Dim types() As String = tupleTemp.Types, names() As String = tupleTemp.Names, fields() As String = tupleTemp.Fields
                Dim minLen As Integer = Math.Min(types.Length, names.Length)

                ''' Loading data
                Do

                    If fields Is Nothing Then
                        Continue Do
                    End If
                    If fields.Length < minLen Then
                        Throw New Exception("Not enough fields in CSV file.")
                    End If

                    sb.Append($"        {{ClassificationNameStrings.{fields(0).ClassificationStringToName}, New ColorDescriptor(")
                    sb.Append($"Color.FromARGB({fields(1)},{fields(2)},{fields(3)}), ")
                    sb.Append($"Color.FromARGB({fields(4)},{fields(5)},{fields(6)}))}}")

                    fields = parser.ReadFields()
                    If fields IsNot Nothing Then
                        sb.AppendLine(",")
                    Else
                        sb.AppendLine("}")
                    End If
                Loop While fields IsNot Nothing
            End If

            ' Close things
            sb.AppendLine("End Module")

            Return sb.ToString()

        End Function

        Private Shared Function StringToValidPropertyName(s As String) As String
            s = s.Trim()
            s = If(Char.IsLetter(s(0)), Char.ToUpper(s(0)) & s.Substring(1), s)
            s = If(Char.IsDigit(s.Trim()(0)), "_" & s, s)
            s = New String(s.[Select](Function(ch) If(Char.IsDigit(ch) OrElse Char.IsLetter(ch), ch, "_"c)).ToArray())
            Return s
        End Function

        Private Shared Function SourceFilesFromAdditionalFile(loadType As CsvLoadType, cacheObjects As Boolean, file As AdditionalText) As IEnumerable(Of (Name As String, Code As String))
            Dim modeDirectoryName As String = Path.GetFileNameWithoutExtension(file.Path)
            Dim csvText As String = file.GetText().ToString
            Return New(String, String)() {(modeDirectoryName, GenerateColorDictionary(modeDirectoryName, csvText, loadType, cacheObjects))}
        End Function

        Private Shared Function SourceFilesFromAdditionalFiles(pathsData As IEnumerable(Of (LoadType As CsvLoadType, CacheObjects As Boolean, File As AdditionalText))) As IEnumerable(Of (Name As String, Code As String))
            Return pathsData.SelectMany(Function(d) SourceFilesFromAdditionalFile(d.LoadType, d.CacheObjects, d.File))
        End Function

        Private Shared Iterator Function GetLoadOptions(context As GeneratorExecutionContext) As IEnumerable(Of (LoadType As CsvLoadType, CacheObjects As Boolean, File As AdditionalText))
            For Each file As AdditionalText In context.AdditionalFiles
                If Path.GetExtension(file.Path).Equals(".csv", StringComparison.OrdinalIgnoreCase) Then
                    ' are there any options for it?
                    Dim loadTimeString As String = Nothing
                    context.AnalyzerConfigOptions?.GetOptions(file).TryGetValue("build_metadata.additionalfiles.CsvLoadType", loadTimeString)
                    Dim loadType As CsvLoadType = CsvLoadType.Startup
                    [Enum].TryParse(loadTimeString, ignoreCase:=True, loadType)
                    Dim cacheObjectsString As String = Nothing
                    context.AnalyzerConfigOptions?.GetOptions(file).TryGetValue("build_metadata.additionalfiles.CacheObjects", cacheObjectsString)
                    Dim cacheObjects As Boolean = Nothing
                    Dim unused As Boolean = Boolean.TryParse(cacheObjectsString, cacheObjects)
                    Yield (loadType, cacheObjects, file)
                End If
            Next
        End Function

    End Class

End Namespace
