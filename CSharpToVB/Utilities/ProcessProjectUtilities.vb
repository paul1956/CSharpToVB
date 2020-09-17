' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.IO

Imports CSharpToVBConverter

Imports Buildalyzer
Imports Buildalyzer.Workspaces

Imports Microsoft.CodeAnalysis
Imports System.Threading
Imports System.Globalization

Public Module ProcessProjectUtilities

    Friend Function CSharpReferences(fileReferences As IEnumerable(Of String), projectReferences As IEnumerable(Of String)) As List(Of MetadataReference)
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

    ''' <summary>
    ''' If debugger is attached return all frameworks if not ask user to pick one
    ''' </summary>
    ''' <param name="TargetFrameworks"></param>
    ''' <returns></returns>
    Friend Function GetFrameworks(TargetFrameworks As List(Of String)) As List(Of String)
        Select Case TargetFrameworks.Count
            Case 0
                Return New List(Of String)
            Case 1
                Return New List(Of String)({TargetFrameworks(0)})
            Case Else
                Using F As New FrameworkSelectionDialog
                    If Debugger.IsAttached Then
                        Return TargetFrameworks.ToList
                    End If
                    F.SetFrameworkList(TargetFrameworks)
                    If F.ShowDialog <> DialogResult.OK Then
                        Return New List(Of String)
                    End If
                    Return New List(Of String)({F.CurrentFramework})
                End Using
        End Select
    End Function

    Friend Async Function GetManagerAsync(SolutionPath As String) As Task(Of AnalyzerManager)
        If SolutionPath.EndsWith(".sln", StringComparison.OrdinalIgnoreCase) Then
            Return Await Task.Run(Function() New AnalyzerManager(SolutionPath)).ConfigureAwait(True)
        End If
        Return Await Task.Run(Function() New AnalyzerManager()).ConfigureAwait(True)
    End Function

    Friend Async Function GetProjectAnalyzerAsync(sourceProjectNameWithPath As String, manager As IAnalyzerManager) As Task(Of IProjectAnalyzer)
        Return Await Task.Run(Function() manager.GetProject(sourceProjectNameWithPath)).ConfigureAwait(True)
    End Function

    Friend Async Function GetResultsAsync(analyzer As ProjectAnalyzer) As Task(Of IAnalyzerResults)
        Return Await Task.Run(Function() analyzer.Build()).ConfigureAwait(True)
    End Function

    Friend Async Function GetWorkspaceAsync(TaskProjectAnalyzer As IProjectAnalyzer) As Task(Of AdhocWorkspace)
        Return Await Task.Run(Function() CType(TaskProjectAnalyzer, ProjectAnalyzer).GetWorkspace()).ConfigureAwait(True)
    End Function
    ''' <summary>
    ''' Convert 1 Project
    ''' </summary>
    ''' <param name="TaskProjectAnalyzer"></param>
    ''' <param name="SolutionRoot"></param>
    ''' <param name="cancelToken"></param>
    ''' <returns>Error String to be Displayed and list of products processed</returns>
    Friend Async Function ProcessProjectAsync(MainForm As Form1, TaskProjectAnalyzer As IProjectAnalyzer, SolutionRoot As String, processedProjects As Integer, totalProjects As Integer, cancelToken As CancellationTokenSource) As Task(Of (ErrorPrompt As String, ProjectsToBeAdded As List(Of String)))
        Application.DoEvents()
        MainForm.UpdateProgressLabels("Getting Analyzer Results", True)
        Dim TaskResults As Task(Of IAnalyzerResults) = GetResultsAsync(CType(TaskProjectAnalyzer, ProjectAnalyzer))
        While Not TaskResults.IsCompleted
            If cancelToken.IsCancellationRequested Then
                Return ("", New List(Of String))
            End If
            Await Task.Delay(100).ConfigureAwait(True)
        End While
        Dim results As IAnalyzerResults = TaskResults.Result
        MainForm.UpdateProgressLabels("Loading Workspace", True)
        Dim TaskWorkspace As Task(Of AdhocWorkspace) = GetWorkspaceAsync(TaskProjectAnalyzer)
        Dim frameworkList As List(Of String) = GetFrameworks(results.TargetFrameworks.ToList)
        ' Under debugger each framework will be processed
        ' Under production the user will select one framework
        While Not TaskWorkspace.IsCompleted
            If cancelToken.IsCancellationRequested Then
                Return ("", New List(Of String))
            End If
            Await Task.Delay(100).ConfigureAwait(True)
        End While
        If frameworkList.Count = 0 Then
            Return ($"no framework is specified, processing project will terminate!", New List(Of String))
        End If
        Dim projectsToBeAdd As New List(Of String)
        Using workspace As AdhocWorkspace = TaskWorkspace.Result
            MainForm.UpdateProgressLabels("", False)

            If workspace.CurrentSolution.Projects.Count <> 1 Then
                Return ($"of an unexpected number of projects {workspace.CurrentSolution.Projects.Count}, processing project will terminate!", New List(Of String))
            End If
            For Each framework As IndexClass(Of String) In frameworkList.WithIndex
                Dim frameworkMsg As String
                If frameworkList.Count = 1 Then
                    frameworkMsg = $"Framework: {framework.Value}"
                Else
                    frameworkMsg = $"Framework {framework.Index + 1} of {frameworkList.Count}: {framework.Value}"
                End If
                Dim currentProject As Project = workspace.CurrentSolution.Projects(0)
                MainForm.StatusStripCurrentFileName.Text = $"{processedProjects} of {totalProjects} Projects, {frameworkMsg}, {currentProject.FilePath}"
                Application.DoEvents()
                Dim csReferences As MetadataReference() = CSharpReferences(results(framework.Value).References, results(framework.Value).ProjectReferences).ToArray
                Dim taskConvertOneProject As Task(Of Boolean) =
                        ProcessProjectCoreAsync(MainForm,
                                                currentProject,
                                                SolutionRoot,
                                                framework.Value,
                                                csReferences
                                               )
                Dim projectToBeAdd As String = ConvertProjectFile(currentProject.FilePath, SolutionRoot)
                If projectToBeAdd.Length > 0 AndAlso Not projectsToBeAdd.Contains(projectToBeAdd) Then
                    projectsToBeAdd.Add(projectToBeAdd)
                End If
                While Not taskConvertOneProject.IsCompleted
                    If cancelToken.IsCancellationRequested Then
                        Exit For
                    End If
                    Await Task.Delay(100).ConfigureAwait(True)
                End While
                If Not taskConvertOneProject.Result Then
                    Exit For
                End If
            Next
        End Using
        Return ("", projectsToBeAdd)
    End Function

    ''' <summary>
    ''' Converts all the files in a C# project
    ''' </summary>
    ''' <param name="MainForm"></param>
    ''' <param name="currentProject"></param>
    ''' <param name="solutionRoot"></param>
    ''' <param name="Framework"></param>
    ''' <returns>False if failed</returns>
    ''' <param name="References"></param>
    Friend Async Function ProcessProjectCoreAsync(MainForm As Form1, currentProject As Project, solutionRoot As String, Framework As String, References As MetadataReference()) As Task(Of Boolean)
        If MainForm._cancellationTokenSource.IsCancellationRequested Then
            Return False
        End If
        Dim FilesProcessed As Integer = 0
        Dim TotalFilesToProcess As Integer = currentProject.Documents.Count
        Dim convertedFramework As String = FrameworkNameToConstant(Framework)
        Dim CSPreprocessorSymbols As New List(Of String) From {Framework, convertedFramework}

        Dim VBPreprocessorSymbols As New List(Of KeyValuePair(Of String, Object)) From {
                                    KeyValuePair.Create(Of String, Object)(convertedFramework, True)}
        If Not convertedFramework.Equals(Framework, StringComparison.OrdinalIgnoreCase) Then
            VBPreprocessorSymbols.Add(KeyValuePair.Create(Of String, Object)(Framework, True))
        End If

        For Each currentDocument As Document In currentProject.Documents
            If MainForm._cancellationTokenSource.IsCancellationRequested Then
                Return False
            End If
            Dim targetFileWithPath As String = DestinationFilePath(currentDocument.FilePath, solutionRoot)
            FilesProcessed += 1
            MainForm.ListBoxFileList.Items.Add(New NumberedListItem($"{FilesProcessed.ToString(CultureInfo.InvariantCulture),-5} {currentDocument.FilePath}", $"{targetFileWithPath}{Path.DirectorySeparatorChar}{Path.GetFileNameWithoutExtension(currentDocument.Name)}.vb"))
            MainForm.ListBoxFileList.SelectedIndex = MainForm.ListBoxFileList.Items.Count - 1
            MainForm.FilesConversionProgress.Text = $"Processed {FilesProcessed:N0} of {TotalFilesToProcess:N0} Files"
            Application.DoEvents()
            If Not Await ProcessFileAsync(MainForm,
                                          currentDocument.FilePath,
                                          targetFileWithPath,
                                          "cs",
                                          CSPreprocessorSymbols,
                                          VBPreprocessorSymbols,
                                          References,
                                          SkipAutoGenerated:=True,
                                          CancelToken:=MainForm._cancellationTokenSource.Token).ConfigureAwait(True) _
                                                    OrElse MainForm._requestToConvert.CancelToken.IsCancellationRequested Then
                Return False
            End If
        Next
        Return True
    End Function

End Module
