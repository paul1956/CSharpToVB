' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Collections.Immutable
Imports System.Reflection
Imports System.Threading

Imports CSharpToVBCodeConverter.DestVisualBasic
Imports CSharpToVBCodeConverter.Util

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CSharp
Imports Microsoft.CodeAnalysis.CSharp.Formatting
Imports Microsoft.CodeAnalysis.Formatting
Imports Microsoft.CodeAnalysis.Options
Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.VisualBasic

Namespace CodeConverter.Tests

    Public Class ConverterTestBase

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

            Dim ProjectId As ProjectId = ProjectId.CreateNewId()
            Dim DocumentID As DocumentId = DocumentId.CreateNewId(projectId:=ProjectId)
            workspace.Open(ProjectInfo.Create(
                                            id:=ProjectId,
                                            version:=VersionStamp.Create(),
                                            name:="TestProject",
                                            assemblyName:="TestProject",
                                            language:=LanguageNames.VisualBasic,
                                            filePath:=Nothing,
                                            outputFilePath:=Nothing,
                                            compilationOptions:=compilationOptions,
                                            parseOptions:=parseOptions,
                                            documents:={
                                                DocumentInfo.Create(
                                                    DocumentID,
                                                    "a.vb",
                                                    Nothing,
                                                    SourceCodeKind.Regular
                                                )
                                            },
                                            projectReferences:=Nothing,
                                            metadataReferences:=VisualBasicReferences(Assembly.Load("System.Windows.Forms").Location)))
            doc = workspace.CurrentSolution.GetProject(ProjectId).GetDocument(DocumentID)
        End Sub

        Friend Shared Sub TestConversionCSharpToVisualBasic(csharpCode As String, DesiredResult As String, Optional csharpOptions As CSharpParseOptions = Nothing, Optional vbOptions As VisualBasicParseOptions = Nothing)
            Dim csharpWorkspace As TestWorkspace = Nothing
            Dim vbWorkspace As TestWorkspace = Nothing
            Dim inputDocument As Document = Nothing
            Dim outputDocument As Document = Nothing

            CSharpWorkspaceSetup(text:=csharpCode, workspace:=csharpWorkspace, doc:=inputDocument, parseOptions:=csharpOptions)
            VBWorkspaceSetup(workspace:=vbWorkspace, doc:=outputDocument, parseOptions:=vbOptions)

            Dim InputNode As SyntaxNode = inputDocument.GetSyntaxRootAsync().GetAwaiter().GetResult()
            Dim lSemanticModel As SemanticModel = inputDocument.GetSemanticModelAsync().GetAwaiter().GetResult()
            Dim outputNode As SyntaxNode = CSharpConverter.Convert(CType(InputNode, CSharpSyntaxNode), SkipAutoGenerated:=True, New DefaultVBOptions, lSemanticModel, ReportException:=Nothing, Progress:=Nothing, CancellationToken.None)

            Dim ActualResult As String = outputDocument.WithSyntaxRoot(
                root:=outputNode.NormalizeWhitespaceEx(useDefaultCasing:=True,
                                                        PreserveCRLF:=True)
                                                        ).GetTextAsync().GetAwaiter().GetResult().ToString()

            Using Workspace As AdhocWorkspace = New AdhocWorkspace()
                Dim project As Project = Workspace.CurrentSolution.AddProject("Project", "Project.dll", LanguageNames.VisualBasic)
                Dim PreprocessorSymbols As New List(Of KeyValuePair(Of String, Object)) From {
                    New KeyValuePair(Of String, Object)("NETCOREAPP", True)
                }

                Dim ParseOptions As VisualBasicParseOptions = New VisualBasicParseOptions(
                        VisualBasic.LanguageVersion.VisualBasic16,
                        DocumentationMode.Diagnose,
                        kind:=SourceCodeKind.Regular,
                        PreprocessorSymbols)

                project = project.WithParseOptions(ParseOptions)

                Dim _Document As Document = project.AddDocument("Document", ActualResult)
                Dim _SyntaxTree As SyntaxTree = _Document.GetSyntaxTreeAsync().Result

                Dim Options As OptionSet = Workspace.Options

                Dim Root As SyntaxNode = _SyntaxTree.GetRootAsync().Result
                Dim spans As IEnumerable(Of TextSpan) = Nothing
                ActualResult = WorkspaceFormat(Workspace, Root, spans, Options, _Document.GetTextAsync().Result)

            End Using

            ActualResult = HomogenizeEol(ActualResult).TrimEnd()
            DesiredResult = HomogenizeEol(DesiredResult).TrimEnd()
            Dim VBTestBase As New VisualBasicFormattingTestBase()
            Call VBTestBase.AssertFormatSpanAsync(ActualResult, DesiredResult)
            Dim ErrorMessage As String = FindFirstDifferenceLine(DesiredResult, ActualResult)
            If ErrorMessage <> "Files identical" Then
                Assert.AreEqual(DesiredResult, ActualResult, ErrorMessage)
            End If
            VBTestBase.Dispose()
            csharpWorkspace.Dispose()
            vbWorkspace.Dispose()
        End Sub

        Private Shared Function FindFirstDifferenceColumn(DesiredLine As String, ActualLine As String) As (ColumnIndex As Integer, Character As String)
            Dim minLength As Integer = Math.Min(DesiredLine.Length, ActualLine.Length) - 1
            For index As Integer = 0 To minLength
                If Not DesiredLine.Chars(index).Equals(ActualLine.Chars(index)) Then
                    Return (index + 1, $"Expected Character ""{DesiredLine.Substring(index, 1)}"", Actual Character ""{ActualLine.Substring(index, 1)}""")
                End If
            Next
#Disable Warning CC0013 ' Use Ternary operator.
            If DesiredLine.Length > ActualLine.Length Then
#Enable Warning CC0013 ' Use Ternary operator.
                Return (minLength + 1, $"Expected Character ""{DesiredLine.Substring(minLength + 1, 1)}"", Actual Character Nothing")
            Else
                Return (minLength + 1, $"Expected Character Nothing, Actual Character ""{ActualLine.Substring(minLength + 1, 1)}""")
            End If
        End Function

        Private Shared Function FindFirstDifferenceLine(DesiredText As String, ActualText As String) As String
            Dim Desiredlines() As String = DesiredText.SplitLines()
            Dim ActuaLines() As String = ActualText.SplitLines()
            For index As Integer = 0 To Math.Min(Desiredlines.GetUpperBound(0), ActuaLines.GetUpperBound(0))
                Dim DesiredLine As String = Desiredlines(index)
                Dim ActualLine As String = ActuaLines(index)
                If Not DesiredLine.Equals(ActualLine, StringComparison.CurrentCulture) Then
                    Dim p As (ColumnIndex As Integer, Character As String) = FindFirstDifferenceColumn(DesiredLine, ActualLine)
                    Return $"{vbCrLf}Expected Line_{index + 1} {DesiredLine}{vbCrLf}Actual Line___{index + 1} {ActualLine}{vbCrLf}Column {p.ColumnIndex} {p.Character}"
                End If
            Next
            Return "Files identical"
        End Function

        Protected Shared Function WorkspaceFormat(workspace As Workspace, root As SyntaxNode, spans As IEnumerable(Of TextSpan), OptionSet As OptionSet, SourceText As SourceText) As String
            Contracts.Contract.Requires(SourceText IsNot Nothing)
            Dim result As IList(Of TextChange) = Formatter.GetFormattedTextChanges(root, spans, workspace, OptionSet)
            Return SourceText.WithChanges(result).ToString()
        End Function

    End Class

End Namespace
