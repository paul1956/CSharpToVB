' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Collections.Immutable
Imports System.Reflection
Imports System.Threading
Imports ConvertDirectory.Tests
Imports CSharpToVBConverter
Imports CSharpToVBConverter.ToVisualBasic

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CSharp
Imports Microsoft.CodeAnalysis.CSharp.Formatting
Imports Microsoft.CodeAnalysis.Formatting
Imports Microsoft.CodeAnalysis.Options
Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.VisualBasic

Namespace CodeConverter.Tests

    Public Class ConverterTestBase

        Private Const OptionHeaderText As String = "Option Explicit Off
Option Infer On
Option Strict Off

"

        ' Do not remove Version
        Public Property Version As Integer = 1

        Private Shared Sub CSharpWorkspaceSetup(text As String, ByRef workspace As TestWorkspace, ByRef doc As Document, Optional parseOptions As CSharpParseOptions = Nothing)
            workspace = New TestWorkspace()
            Dim projectId As ProjectId = ProjectId.CreateNewId()
            Dim documentId As DocumentId = DocumentId.CreateNewId(projectId)
            If parseOptions Is Nothing Then
                parseOptions = New CSharpParseOptions(
                    languageVersion:=CSharp.LanguageVersion.Default,
                    documentationMode:=DocumentationMode.Diagnose Or DocumentationMode.Parse,
                    kind:=SourceCodeKind.Regular,
                    preprocessorSymbols:=ImmutableArray.Create("DEBUG", "TEST")
                )
            End If
            workspace.Options.WithChangedOption([option]:=CSharpFormattingOptions.NewLinesForBracesInControlBlocks, value:=False)
            workspace.Open(ProjectInfo.Create(
                id:=projectId,
                version:=VersionStamp.Create(),
                name:="TestProject",
                assemblyName:="TestProject",
                language:=LanguageNames.CSharp,
                filePath:=Nothing,
                outputFilePath:=Nothing,
                compilationOptions:=New CSharpCompilationOptions(
                    OutputKind.DynamicallyLinkedLibrary,
                    False,
                    moduleName:="",
                    mainTypeName:="",
                    "Script",
                    {"System", "System.Collections.Generic", "System.Linq"},
                    OptimizationLevel.Debug,
                    checkOverflow:=False,
                    allowUnsafe:=True
                    ),
                parseOptions:=parseOptions,
                documents:={
                    DocumentInfo.Create(
                        id:=documentId,
                        name:="a.cs",
                        folders:=Nothing,
                        sourceCodeKind:=SourceCodeKind.Regular,
                        loader:=TextLoader.From(textAndVersion:=TextAndVersion.Create(text:=SourceText.From(text), version:=VersionStamp.Create()))
                    )
                },
                projectReferences:=Nothing,
                metadataReferences:=CSharpReferences(Assembly.Load("System.Windows.Forms").Location, Nothing))
                )
            doc = workspace.CurrentSolution.GetProject(projectId).GetDocument(documentId)
        End Sub

        Private Shared Function FindFirstDifferenceColumn(DesiredLine As String, ActualLine As String) As (ColumnIndex As Integer, Character As String)
            Dim minLength As Integer = Math.Min(DesiredLine.Length, ActualLine.Length) - 1
            For index As Integer = 0 To minLength
                If Not DesiredLine.Chars(index).Equals(ActualLine.Chars(index)) Then
                    Return (index + 1, $"Expected Character ""{DesiredLine.Chars(index)}"", Actual Character ""{ActualLine.Chars(index)}""")
                End If
            Next

            If DesiredLine.Length > ActualLine.Length Then
                Return (minLength + 1, $"Expected Character ""{DesiredLine.Chars(minLength + 1)}"", Actual Character Nothing")
            Else
                Return (minLength + 1, $"Expected Character Nothing, Actual Character ""{ActualLine.Chars(minLength + 1)}""")
            End If
        End Function

        Private Shared Function FindFirstDifferenceLine(DesiredText As String, ActualText As String) As String
            Dim desiredlines() As String = DesiredText.SplitLines()
            Dim actuaLines() As String = ActualText.SplitLines()
            For index As Integer = 0 To Math.Min(desiredlines.GetUpperBound(0), actuaLines.GetUpperBound(0))
                Dim desiredLine As String = desiredlines(index)
                Dim actualLine As String = actuaLines(index)
                If Not desiredLine.Equals(actualLine, StringComparison.CurrentCulture) Then
                    Dim p As (ColumnIndex As Integer, Character As String) = FindFirstDifferenceColumn(desiredLine, actualLine)
                    Return $"{vbCrLf}Expected Line_{index + 1} {desiredLine}{vbCrLf}Actual Line___{index + 1} {actualLine}{vbCrLf}Column {p.ColumnIndex} {p.Character}"
                End If
            Next
            Return "Files identical"
        End Function

        Private Shared Sub VBWorkspaceSetup(ByRef workspace As TestWorkspace, ByRef doc As Document, Optional parseOptions As VisualBasicParseOptions = Nothing)
            workspace = New TestWorkspace()
            If parseOptions Is Nothing Then
                parseOptions = New VisualBasicParseOptions(
                    languageVersion:=VisualBasic.LanguageVersion.VisualBasic16,
                    documentationMode:=DocumentationMode.Diagnose Or DocumentationMode.Parse,
                    kind:=SourceCodeKind.Regular,
                    preprocessorSymbols:=New Dictionary(Of String, Object) From {{"NETSTANDARD2_0", Nothing}}
                )
            End If
            workspace.Options.WithChangedOption([option]:=CSharpFormattingOptions.NewLinesForBracesInControlBlocks, value:=False)
            Dim compilationOptions As VisualBasicCompilationOptions = New VisualBasicCompilationOptions(outputKind:=OutputKind.DynamicallyLinkedLibrary).
                WithRootNamespace(rootNamespace:="TestProject").
                WithGlobalImports(globalImports:=GlobalImport.Parse(NameOf(System), "System.Collections.Generic", "System.Linq", "Microsoft.VisualBasic"))

            Dim projectID As ProjectId = ProjectId.CreateNewId()
            Dim documentID As DocumentId = DocumentId.CreateNewId(projectId:=projectID)
            workspace.Open(ProjectInfo.Create(
                                            projectID,
                                            VersionStamp.Create(),
                                            name:="TestProject",
                                            assemblyName:="TestProject",
                                            LanguageNames.VisualBasic,
                                            filePath:=Nothing,
                                            outputFilePath:=Nothing,
                                            compilationOptions,
                                            parseOptions,
                                            documents:={
                                                DocumentInfo.Create(
                                                    documentID,
                                                    "a.vb",
                                                    Nothing,
                                                    SourceCodeKind.Regular
                                                )
                                            },
                                            projectReferences:=Nothing,
                                            VisualBasicReferences(Assembly.Load("System.Windows.Forms").Location)))
            doc = workspace.CurrentSolution.GetProject(projectID).GetDocument(documentID)
        End Sub

        Protected Shared Function WorkspaceFormat(workspace As Workspace, root As SyntaxNode, spans As IEnumerable(Of TextSpan), pOptionSet As OptionSet, pSourceText As SourceText) As String
            Dim result As IList(Of TextChange) = Formatter.GetFormattedTextChanges(root, spans, workspace, pOptionSet)
            Return pSourceText?.WithChanges(result).ToString()
        End Function

        Friend Shared Sub TestConversionCSharpToVisualBasic(csharpCode As String, DesiredResult As String, Optional csharpOptions As CSharpParseOptions = Nothing, Optional vbOptions As VisualBasicParseOptions = Nothing, Optional IncludeOptions As Boolean = True)
            Dim csharpWorkspace As TestWorkspace = Nothing
            Dim vbWorkspace As TestWorkspace = Nothing
            Dim inputDocument As Document = Nothing
            Dim outputDocument As Document = Nothing

            CSharpWorkspaceSetup(text:=csharpCode, workspace:=csharpWorkspace, doc:=inputDocument, parseOptions:=csharpOptions)
            VBWorkspaceSetup(workspace:=vbWorkspace, doc:=outputDocument, parseOptions:=vbOptions)

            Dim lSemanticModel As SemanticModel = inputDocument.GetSemanticModelAsync().GetAwaiter().GetResult()
            Dim outputNode As SyntaxNode = CType(inputDocument.GetSyntaxRootAsync() _
                                                              .GetAwaiter() _
                                                              .GetResult(),
                                                 CSharpSyntaxNode).DoConversion(lSemanticModel,
                                                                                New DefaultVBOptions,
                                                                                SkipAutoGenerated:=True,
                                                                                ReportException:=Nothing,
                                                                                Progress:=Nothing,
                                                                                CancellationToken.None)

            Dim actualResult As String = outputDocument.WithSyntaxRoot(
                root:=outputNode.NormalizeWhitespaceEx(useDefaultCasing:=True,
                                                       PreserveCRLF:=True)
                                                      ).GetTextAsync().GetAwaiter().GetResult().ToString()

            Using workspace As AdhocWorkspace = New AdhocWorkspace()
                Dim project As Project = workspace.CurrentSolution.AddProject("Project", "Project.dll", LanguageNames.VisualBasic)
                Dim preprocessorSymbols As New List(Of KeyValuePair(Of String, Object)) From {
                    New KeyValuePair(Of String, Object)("NETCOREAPP", True)
                }


                project = project.WithParseOptions(New VisualBasicParseOptions(
                                                        VisualBasic.LanguageVersion.VisualBasic16,
                                                        DocumentationMode.Diagnose,
                                                        kind:=SourceCodeKind.Regular,
                                                        preprocessorSymbols)
                                                   )

                Dim doc As Document = project.AddDocument("Document", actualResult)
                Dim tree As SyntaxTree = doc.GetSyntaxTreeAsync().Result
                Dim spans As IEnumerable(Of TextSpan) = Nothing
                actualResult = WorkspaceFormat(workspace,
                                               tree.GetRootAsync().Result,
                                               spans,
                                               workspace.Options,
                                               doc.GetTextAsync().Result)
            End Using

            If IncludeOptions Then
                actualResult = actualResult.Replace(OptionHeaderText, "", StringComparison.OrdinalIgnoreCase)
            End If

            actualResult = HomogenizeEol(actualResult).TrimEnd()
            DesiredResult = HomogenizeEol(DesiredResult).TrimEnd()
            Using vbTestBase As New VisualBasicFormattingTestBase()
                vbTestBase.AssertFormatSpanAsync(actualResult, DesiredResult)
                Dim errorMessage As String = FindFirstDifferenceLine(DesiredResult, actualResult)
                If errorMessage <> "Files identical" Then
                    Assert.AreEqual(DesiredResult, actualResult, errorMessage)
                End If
            End Using
            csharpWorkspace.Dispose()
            vbWorkspace.Dispose()
        End Sub

    End Class

End Namespace
