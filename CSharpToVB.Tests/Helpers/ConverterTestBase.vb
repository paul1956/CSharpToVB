' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.Collections.Immutable
Imports System.Reflection
Imports System.Threading
Imports ConvertDirectory.Tests
Imports Extensions
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CSharp
Imports Microsoft.CodeAnalysis.CSharp.Formatting
Imports Microsoft.CodeAnalysis.Formatting
Imports Microsoft.CodeAnalysis.Options
Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.VisualBasic
Imports SupportClasses
Imports Tests.CodeConverter
Imports Tests.ConvertDirectories
Imports Utilities

Namespace Helpers

    Public Class ConverterTestBase

        Private Const OptionHeaderText As String = "' To configure or remove Option's included in result, go to Options/Advanced Options...
Option Explicit Off
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

        Private Shared Function FindFirstDifferenceColumn(desiredLine As String, actualLine As String) As (ColumnIndex As Integer, Character As String)
            Dim minLength As Integer = Math.Min(desiredLine.Length, actualLine.Length) - 1
            For index As Integer = 0 To minLength
                If Not desiredLine.Chars(index).Equals(actualLine.Chars(index)) Then
                    Return (index + 1, $"Expected Character ""{desiredLine.Chars(index)}"", Actual Character ""{actualLine.Chars(index)}""")
                End If
            Next

            If desiredLine.Length > actualLine.Length Then
                Return (minLength + 1, $"Expected Character ""{desiredLine.Chars(minLength + 1)}"", Actual Character Nothing")
            Else
                Return (minLength + 1, $"Expected Character Nothing, Actual Character ""{actualLine.Chars(minLength + 1)}""")
            End If
        End Function

        Private Shared Function FindFirstDifferenceLine(desiredText As String, actualText As String) As String
            Dim desiredLines() As String = desiredText.SplitLines()
            Dim actualLines() As String = actualText.SplitLines()
            For index As Integer = 0 To Math.Min(desiredLines.GetUpperBound(0), actualLines.GetUpperBound(0))
                Dim desiredLine As String = desiredLines(index)
                Dim actualLine As String = actualLines(index)
                If Not desiredLine.Equals(actualLine, StringComparison.CurrentCulture) Then
                    Dim p As (ColumnIndex As Integer, Character As String) = FindFirstDifferenceColumn(desiredLine, actualLine)
                    Return $"{vbCrLf}Expected Line_{index + 1} {desiredLine}{vbCrLf}Actual Line___{index + 1} {actualLine}{vbCrLf}Column {p.ColumnIndex} {p.Character}"
                End If
            Next
            Return "Files identical"
        End Function

        Private Shared Sub VbWorkspaceSetup(ByRef workspace As TestWorkspace, ByRef doc As Document,
                                            Optional parseOptions As VisualBasicParseOptions = Nothing)
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

            Dim projectId As ProjectId = ProjectId.CreateNewId()
            Dim documentId As DocumentId = DocumentId.CreateNewId(projectId:=projectId)
            workspace.Open(ProjectInfo.Create(
                                            projectId,
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
                                                    documentId,
                                                    "a.vb",
                                                    Nothing,
                                                    SourceCodeKind.Regular
                                                )
                                            },
                                            projectReferences:=Nothing,
                                            VisualBasicReferences(Assembly.Load("System.Windows.Forms").Location)))
            doc = workspace.CurrentSolution.GetProject(projectId).GetDocument(documentId)
        End Sub

        Protected Shared Function WorkspaceFormat(workspace As Workspace, root As SyntaxNode, spans As IEnumerable(Of TextSpan), pOptionSet As OptionSet, pSourceText As SourceText) As String
            Dim result As IList(Of TextChange) = Formatter.GetFormattedTextChanges(root, spans, workspace, pOptionSet)
            Return pSourceText?.WithChanges(result).ToString()
        End Function

        Friend Shared Sub TestConversionCSharpToVisualBasic(csharpCode As String, desiredResult As String, Optional csharpOptions As CSharpParseOptions = Nothing, Optional vbOptions As VisualBasicParseOptions = Nothing, Optional includeOptions As Boolean = True)
            Dim csharpWorkspace As TestWorkspace = Nothing
            Dim vbWorkspace As TestWorkspace = Nothing
            Dim inputDocument As Document = Nothing
            Dim outputDocument As Document = Nothing

            CSharpWorkspaceSetup(text:=csharpCode, workspace:=csharpWorkspace, doc:=inputDocument, parseOptions:=csharpOptions)
            VbWorkspaceSetup(workspace:=vbWorkspace, doc:=outputDocument, parseOptions:=vbOptions)

            Dim lSemanticModel As SemanticModel = inputDocument.GetSemanticModelAsync().GetAwaiter().GetResult()
            Dim outputNode As SyntaxNode = CType(inputDocument.GetSyntaxRootAsync() _
                                                              .GetAwaiter() _
                                                              .GetResult(),
                                                 CSharpSyntaxNode).DoConversion(lSemanticModel,
                                                                                New DefaultVbOptions,
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

            If includeOptions Then
                actualResult = actualResult.Replace(OptionHeaderText, "", StringComparison.OrdinalIgnoreCase)
            End If

            actualResult = HomogenizeEol(actualResult).TrimEnd()
            desiredResult = HomogenizeEol(desiredResult).TrimEnd()
            Using vbTestBase As New VisualBasicFormattingTestBase()
                vbTestBase.AssertFormatSpanAsync(actualResult, desiredResult)
                Dim errorMessage As String = FindFirstDifferenceLine(desiredResult, actualResult)
                If errorMessage <> "Files identical" Then
                    Assert.AreEqual(desiredResult, actualResult, errorMessage)
                End If
            End Using
            csharpWorkspace.Dispose()
            vbWorkspace.Dispose()
        End Sub

    End Class

End Namespace
