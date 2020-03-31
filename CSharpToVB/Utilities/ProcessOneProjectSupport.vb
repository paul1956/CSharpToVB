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
        'SystemReferences
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

    Private Async Function ProcessOneProject(sourceProjectNameWithPath As String, projectSavePath As String, _cancellationTokenSource As CancellationTokenSource) As Task(Of Boolean)
        ListBoxErrorList.Text = ""
        ListBoxFileList.Text = ""
        LabelProgress.Text = "Getting Analyzer Manger"
        LabelProgress.Visible = True
        ProgressBar1.Visible = True
        Dim TaskAnalyzerManager As Task(Of AnalyzerManager) = GetManager()
        While Not TaskAnalyzerManager.IsCompleted

            If _cancellationTokenSource.IsCancellationRequested Then
                LabelProgress.Visible = False
                ProgressBar1.Visible = False
                Return False
            End If
            Await Task.Delay(100).ConfigureAwait(True)
        End While
        Dim manager As AnalyzerManager = TaskAnalyzerManager.Result

        LabelProgress.Text = "Getting Project Analyzer"
        Dim TaskAnalyzer As Task(Of ProjectAnalyzer) = GetAnalyzer(sourceProjectNameWithPath, manager)
        While Not TaskAnalyzer.IsCompleted
            If _cancellationTokenSource.IsCancellationRequested Then
                LabelProgress.Visible = False
                ProgressBar1.Visible = False
                Return False
            End If
            Await Task.Delay(100).ConfigureAwait(True)
        End While
        Dim analyzer As ProjectAnalyzer = TaskAnalyzer.Result

        LabelProgress.Text = "Getting Analyzer Results"
        Dim TaskResults As Task(Of AnalyzerResults) = GetResults(analyzer)
        While Not TaskResults.IsCompleted
            If _cancellationTokenSource.IsCancellationRequested Then
                LabelProgress.Visible = False
                ProgressBar1.Visible = False
                Return False
            End If
            Await Task.Delay(100).ConfigureAwait(True)
        End While
        Dim results As AnalyzerResults = TaskResults.Result

        ProgressBar1.Visible = False
        LabelProgress.Visible = False
        Using workspace As AdhocWorkspace = analyzer.GetWorkspace()
            If Not workspace.CurrentSolution.Projects.Any Then
                Return False
            End If
            Dim currentProject As Project = workspace.CurrentSolution.Projects(0)
            If currentProject.HasDocuments Then
                If _cancellationTokenSource.IsCancellationRequested Then
                    Return False
                End If
                Dim References() As MetadataReference
                Dim FrameWorks As List(Of String) = results.TargetFrameworks.ToList
                Dim Framework As String = FrameWorks(0)
                Select Case FrameWorks.Count
                    Case 0
                        References = SharedReferences.CSharpReferences("", Nothing).ToArray
                        MsgBox($"No framework is specified, processing will terminate!", MsgBoxStyle.Exclamation, "Framework Warning")
                        Return False
                    Case 1
                        References = CSharpReferences(results(Framework).References, results(Framework).ProjectReferences).ToArray
                    Case Else
                        Dim F As New FrameworkSelectionDialog
                        F.SetFrameworkList(FrameWorks)
                        Dim Result As DialogResult = F.ShowDialog
                        Framework = F.CurrentFramework
                        F.Dispose()

                        If Result = DialogResult.OK Then
                            References = CSharpReferences(results(Framework).References, results(Framework).ProjectReferences).ToArray
                        Else
                            Return False
                        End If
                End Select
                Dim xmlDoc As New XmlDocument With {
                                .PreserveWhitespace = True
                            }
                xmlDoc.Load(currentProject.FilePath)
                Dim root As XmlNode = xmlDoc.FirstChild
                If root.Attributes.Count > 0 AndAlso root.Attributes(0).Name.StartsWith("Microsoft.NET.Sdk", StringComparison.OrdinalIgnoreCase) Then
                    ConvertProjectFile(projectSavePath, currentProject.FilePath, xmlDoc)
                End If

                SetButtonStopAndCursor(Me, ButtonStopConversion, StopButtonVisible:=True)
                Dim FilesProcessed As Integer = 0
                Dim TotalFilesToProcess As Integer = currentProject.Documents.Count
                Dim CSPreprocessorSymbols As New List(Of String) From {
                                            Framework
                                        }
                Dim VBPreprocessorSymbols As New List(Of KeyValuePair(Of String, Object)) From {
                                            KeyValuePair.Create(Of String, Object)(Framework, True)
                                        }

                For Each currentDocument As Document In currentProject.Documents
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
            End If
        End Using
        RichTextBoxConversionInput.Text = ""
        RichTextBoxConversionOutput.Text = ""
        Return True
    End Function

    Private Shared Async Function GetSourceFiles(results As AnalyzerResults) As Task(Of String())
        Return Await Task.Run(Function() results.First().SourceFiles()).ConfigureAwait(True)
    End Function

    Private Shared Async Function GetResults(analyzer As ProjectAnalyzer) As Task(Of AnalyzerResults)
        Return Await Task.Run(Function() analyzer.Build()).ConfigureAwait(True)
    End Function

    Private Shared Async Function GetAnalyzer(sourceProjectNameWithPath As String, manager As AnalyzerManager) As Task(Of ProjectAnalyzer)
        Return Await Task.Run(Function() manager.GetProject(sourceProjectNameWithPath)).ConfigureAwait(True)
    End Function

    Private Shared Async Function GetManager() As Task(Of AnalyzerManager)
        Return Await Task.Run(Function() New AnalyzerManager()).ConfigureAwait(True)
    End Function
End Class
