' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.IO
Imports System.Threading
Imports System.Xml

Imports Buildalyzer
Imports Buildalyzer.Workspaces

Imports CSharpToVBCodeConverter.Util

Imports Microsoft.CodeAnalysis

Imports VBMsgBox

Partial Public Class Form1

    Private Shared Function CSharpReferences(fileReferences As IEnumerable(Of String), projectReferences As IEnumerable(Of String)) As List(Of MetadataReference)
        Dim ReferenceList As New List(Of MetadataReference)
        For Each DLL_Path As String In fileReferences
            If DLL_Path.EndsWith("System.EnterpriseServices.Wrapper.dll", StringComparison.Ordinal) Then
                Continue For
            End If
            If Not File.Exists(DLL_Path) Then
                Continue For
            End If
            ReferenceList.Add(MetadataReference.CreateFromFile(DLL_Path))
        Next
        For Each proj_Path As String In projectReferences
            ReferenceList.Add(MetadataReference.CreateFromFile(proj_Path))
        Next
        Return ReferenceList
    End Function

    Private Shared Async Function GetAnalyzer(sourceProjectNameWithPath As String, manager As IAnalyzerManager) As Task(Of IProjectAnalyzer)
        Return Await Task.Run(Function() manager.GetProject(sourceProjectNameWithPath)).ConfigureAwait(True)
    End Function

    Private Shared Async Function GetManager() As Task(Of AnalyzerManager)
        Return Await Task.Run(Function() New AnalyzerManager()).ConfigureAwait(True)
    End Function

    Private Shared Async Function GetResults(analyzer As ProjectAnalyzer) As Task(Of IAnalyzerResults)
        Return Await Task.Run(Function() analyzer.Build()).ConfigureAwait(True)
    End Function

    Private Shared Function TryGetFramework(TargetFrameworks As List(Of String), ByRef framework As String) As Boolean
        Select Case TargetFrameworks.Count
            Case 0
                Return False
            Case 1
                framework = TargetFrameworks(0)
            Case Else
                Using F As New FrameworkSelectionDialog
                    F.SetFrameworkList(TargetFrameworks)
                    If F.ShowDialog <> DialogResult.OK Then
                        Return False
                    End If
                    framework = F.CurrentFramework
                End Using
        End Select
        Return True
    End Function

    Private Async Function ProcessOneProjectCore(currentProject As Project, sourceProjectNameWithPath As String, projectSavePath As String, Framework As String, References As MetadataReference()) As Task(Of Boolean)
        If _cancellationTokenSource.IsCancellationRequested Then
            Return False
        End If
        Dim xmlDoc As New XmlDocument With {
                        .PreserveWhitespace = True
                    }
        xmlDoc.Load(currentProject.FilePath)
        Dim root As XmlNode = xmlDoc.FirstChild
        If root.Attributes.Count > 0 AndAlso root.Attributes(0).Name.StartsWith("Microsoft.NET.Sdk", StringComparison.OrdinalIgnoreCase) Then
            ConvertProjectFile(projectSavePath, currentProject.FilePath, xmlDoc)
        End If

        Dim FilesProcessed As Integer = 0
        Dim TotalFilesToProcess As Integer = currentProject.Documents.Count
        Dim CSPreprocessorSymbols As New List(Of String) From {
                                    Framework
                                }
        Dim VBPreprocessorSymbols As New List(Of KeyValuePair(Of String, Object)) From {
                                    KeyValuePair.Create(Of String, Object)(Framework, True)
                                }

        For Each currentDocument As Document In currentProject.Documents
            If _cancellationTokenSource.IsCancellationRequested Then
                Return False
            End If
            Dim targetFileWithPath As String = DestinationFilePath(Path.GetDirectoryName(sourceProjectNameWithPath), projectSavePath, currentDocument.FilePath)
            If ParseCSharpSource(currentDocument.GetTextAsync(Nothing).Result.ToString,
                                     CSPreprocessorSymbols).GetRoot.SyntaxTree.IsGeneratedCode(Function(t As SyntaxTrivia) As Boolean
                                                                                                   Return t.IsComment OrElse t.IsRegularOrDocComment
                                                                                               End Function, _cancellationTokenSource.Token) Then
                TotalFilesToProcess -= 1
                FilesConversionProgress.Text = $"Processed {FilesProcessed:N0} of {TotalFilesToProcess:N0} Files"
                Application.DoEvents()
                Continue For
            Else
                FilesProcessed += 1
                ListBoxFileList.Items.Add(New NumberedListItem($"{FilesProcessed.ToString(Globalization.CultureInfo.InvariantCulture),-5} {currentDocument.FilePath}", $"{targetFileWithPath}{Path.DirectorySeparatorChar}{Path.GetFileNameWithoutExtension(currentDocument.Name)}.vb"))
                ListBoxFileList.SelectedIndex = ListBoxFileList.Items.Count - 1
                FilesConversionProgress.Text = $"Processed {FilesProcessed:N0} of {TotalFilesToProcess:N0} Files"
                Application.DoEvents()
            End If
            If Not Await ProcessFileAsync(currentDocument.FilePath, targetFileWithPath, "cs", CSPreprocessorSymbols, VBPreprocessorSymbols, References, _cancellationTokenSource.Token).ConfigureAwait(True) _
                            OrElse _requestToConvert.CancelToken.IsCancellationRequested Then
                Return False
            End If
        Next currentDocument
        Return True
    End Function

    Private Async Function ProcessOneProjectUI(manager As AnalyzerManager, sourceProjectNameWithPath As String, projectSavePath As String, _cancellationTokenSource As CancellationTokenSource) As Task(Of Boolean)
        LabelProgress.Text = "Getting Project Analyzer"
        Dim TaskAnalyzer As Task(Of IProjectAnalyzer) = GetAnalyzer(sourceProjectNameWithPath, manager)
        While Not TaskAnalyzer.IsCompleted
            If _cancellationTokenSource.IsCancellationRequested Then
                Return False
            End If
            Await Task.Delay(100).ConfigureAwait(True)
        End While

        LabelProgress.Text = "Getting Analyzer Results"
        Dim TaskResults As Task(Of IAnalyzerResults) = GetResults(CType(TaskAnalyzer.Result, ProjectAnalyzer))
        While Not TaskResults.IsCompleted
            If _cancellationTokenSource.IsCancellationRequested Then
                Return False
            End If
            Await Task.Delay(100).ConfigureAwait(True)
        End While
        Dim results As IAnalyzerResults = TaskResults.Result
        LabelProgress.Visible = False
        ProgressBar1.Visible = False
        Dim Framework As String = String.Empty
        If TryGetFramework(results.TargetFrameworks.ToList, Framework) Then
            Using workspace As AdhocWorkspace = CType(TaskAnalyzer.Result, ProjectAnalyzer).GetWorkspace()
                If Not workspace.CurrentSolution.Projects.Any Then
                    Return False
                End If
                Dim currentProject As Project = workspace.CurrentSolution.Projects(0)
                Await ProcessOneProjectCore(
                        currentProject,
                        sourceProjectNameWithPath,
                        projectSavePath,
                        Framework,
                        CSharpReferences(
                                        results(Framework).References,
                                        results(Framework).ProjectReferences).ToArray
                                        ).ConfigureAwait(True)
            End Using
        Else
            SetButtonStopAndCursor(Me, ButtonStopConversion, StopButtonVisible:=False)
            MsgBox($"No framework is specified, processing project will terminate!", MsgBoxStyle.Exclamation, "Framework Warning")
        End If

        RichTextBoxConversionInput.Text = ""
        RichTextBoxConversionOutput.Text = ""
        Return True
    End Function

End Class
