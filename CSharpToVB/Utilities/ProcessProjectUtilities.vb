' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Globalization
Imports System.IO
Imports System.Runtime.CompilerServices
Imports System.Threading
Imports Buildalyzer
Imports Buildalyzer.Workspaces
Imports Extensions
Imports Microsoft.CodeAnalysis

Public Module ProcessProjectUtilities

    Private Function CSharpMetaDataReferences(fileReferences As IEnumerable(Of String), projectReferences As IEnumerable(Of String)) As List(Of MetadataReference)
        Dim referenceList As New List(Of MetadataReference)
        For Each dllPath As String In fileReferences
            If dllPath.EndsWith("System.EnterpriseServices.Wrapper.dll", StringComparison.Ordinal) Then
                Continue For
            End If
            If Not File.Exists(dllPath) Then
                Continue For
            End If
            referenceList.Add(MetadataReference.CreateFromFile(dllPath))
        Next
        For Each projPath As String In projectReferences
            referenceList.Add(MetadataReference.CreateFromFile(projPath))
        Next
        Return referenceList
    End Function

    ''' <summary>
    ''' If debugger is attached return all frameworks if not ask user to pick one
    ''' </summary>
    ''' <param name="targetFrameworks"></param>
    ''' <returns></returns>
    <Extension>
    Friend Function GetFrameworks(mainForm As Form1, targetFrameworks As List(Of String)) As List(Of String)
        Select Case targetFrameworks.Count
            Case 0
                Return New List(Of String)
            Case 1
                Return New List(Of String)({targetFrameworks(0)})
            Case Else
                Dim page As New TaskDialogPage
                For Each s As IndexClass(Of String) In targetFrameworks.WithIndex
                    page.RadioButtons.Add(New TaskDialogRadioButton(s.Value) With
                                          {.Checked = s.IsFirst}
                                         )
                Next
                page.Caption = "Select Framework"
                page.Text = "Selected Framework is use to set 'Framework' #const used for conversion. Only one Framework per project can be converted at a time. Merging multiple Frameworks is not automatically supported"
                page.Buttons.Add(New TaskDialogButton("OK"))
                page.Buttons.Add(New TaskDialogButton("Cancel"))
                page.DefaultButton = page.Buttons(0)
                Dim taskDialogResult As TaskDialogButton = TaskDialog.ShowDialog(My.Forms.Form1.Handle, page, TaskDialogStartupLocation.CenterOwner)

                If taskDialogResult.Text = TaskDialogButton.OK.Text Then
                    Return {page.RadioButtons.Where(Function(b As TaskDialogRadioButton) b.Checked = True).First.Text}.ToList
                Else
                    Return New List(Of String)
                End If
        End Select
    End Function

    Friend Async Function GetManagerAsync(solutionPath As String) As Task(Of AnalyzerManager)
        If solutionPath.EndsWith(".sln", StringComparison.OrdinalIgnoreCase) Then
            Return Await Task.Run(Function() New AnalyzerManager(solutionPath)).ConfigureAwait(True)
        End If
        Return Await Task.Run(Function() New AnalyzerManager()).ConfigureAwait(True)
    End Function

    Friend Async Function GetProjectAnalyzerAsync(sourceProjectNameWithPath As String, manager As IAnalyzerManager) As Task(Of IProjectAnalyzer)
        Return Await Task.Run(Function() manager.GetProject(sourceProjectNameWithPath)).ConfigureAwait(True)
    End Function

    Friend Async Function GetResultsAsync(analyzer As ProjectAnalyzer) As Task(Of IAnalyzerResults)
        Return Await Task.Run(Function() analyzer.Build()).ConfigureAwait(True)
    End Function

    Friend Async Function GetWorkspaceAsync(taskProjectAnalyzer As IProjectAnalyzer) As Task(Of AdhocWorkspace)
        Return Await Task.Run(Function() CType(taskProjectAnalyzer, ProjectAnalyzer).GetWorkspace()).ConfigureAwait(True)
    End Function

    ''' <summary>
    ''' Convert 1 Project
    ''' </summary>
    ''' <param name="taskProjectAnalyzer"></param>
    ''' <param name="solutionRoot"></param>
    ''' <param name="cancelToken"></param>
    ''' <returns>Error String to be Displayed and list of products processed</returns>
    Friend Async Function ProcessProjectAsync(mainForm As Form1, taskProjectAnalyzer As IProjectAnalyzer, solutionRoot As String, processedProjects As Integer, totalProjects As Integer, cancelToken As CancellationTokenSource) As Task(Of (ErrorPrompt As String, ProjectsToBeAdded As List(Of String)))
        Application.DoEvents()
        mainForm.UpdateProgressLabels("Getting Analyzer Results")
        Dim taskResults As Task(Of IAnalyzerResults) = GetResultsAsync(CType(taskProjectAnalyzer, ProjectAnalyzer))
        While Not taskResults.IsCompleted
            If cancelToken.IsCancellationRequested Then
                Return ("", New List(Of String))
            End If
            Await Task.Delay(100).ConfigureAwait(True)
        End While
        Dim results As IAnalyzerResults = taskResults.Result
        mainForm.UpdateProgressLabels("Loading Workspace")
        Dim taskWorkspace As Task(Of AdhocWorkspace) = GetWorkspaceAsync(taskProjectAnalyzer)
        Dim frameworkList As List(Of String) = mainForm.GetFrameworks(results.TargetFrameworks.ToList)
        ' Under debugger each framework will be processed
        ' Under production the user will select one framework
        While Not taskWorkspace.IsCompleted
            If cancelToken.IsCancellationRequested Then
                Return ("", New List(Of String))
            End If
            Await Task.Delay(100).ConfigureAwait(True)
        End While
        If frameworkList.Count = 0 Then
            Return ($"no framework is specified, processing project will terminate!", New List(Of String))
        End If
        Dim projectsToBeAdd As New List(Of String)
        Using workspace As AdhocWorkspace = taskWorkspace.Result
            mainForm.UpdateProgressLabels("")

            If workspace.CurrentSolution.Projects.Count <> 1 Then
                Return ($"of an unexpected number of projects {workspace.CurrentSolution.Projects.Count}, processing project will terminate!", New List(Of String))
            End If
            For Each framework As IndexClass(Of String) In frameworkList.WithIndex
                Dim frameworkMsg As String
                If frameworkList.Count = 1 Then
                    frameworkMsg = $"Framework: {framework.Value}"
                Else
                    frameworkMsg = $"Framework {framework.index + 1} of {frameworkList.Count}: {framework.Value}"
                End If
                Dim currentProject As Project = workspace.CurrentSolution.Projects(0)
                mainForm.StatusStripCurrentFileName.Text = $"{processedProjects} of {totalProjects} Projects, {frameworkMsg}, {currentProject.FilePath}"
                Application.DoEvents()
                Dim csReferences As MetadataReference() = CSharpMetaDataReferences(results(framework.Value).References, results(framework.Value).ProjectReferences).ToArray
                Dim taskConvertOneProject As Task(Of Boolean) =
                        ProcessProjectCoreAsync(mainForm,
                                                currentProject,
                                                solutionRoot,
                                                framework.Value,
                                                csReferences
                                               )
                Dim projectToBeAdd As String = ConvertProjectFile(currentProject.FilePath, solutionRoot)
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
    ''' <param name="mainForm"></param>
    ''' <param name="currentProject"></param>
    ''' <param name="solutionRoot"></param>
    ''' <param name="framework"></param>
    ''' <returns>False if failed</returns>
    ''' <param name="references"></param>
    Friend Async Function ProcessProjectCoreAsync(mainForm As Form1, currentProject As Project, solutionRoot As String, framework As String, references As MetadataReference()) As Task(Of Boolean)
        If mainForm._cancellationTokenSource.IsCancellationRequested Then
            Return False
        End If
        Dim filesProcessed As Integer = 0
        Dim totalFilesToProcess As Integer = currentProject.Documents.Count
        Dim convertedFramework As String = FrameworkNameToConstant(framework)
        Dim csPreprocessorSymbols As New List(Of String) From {framework, convertedFramework}

        Dim vbPreprocessorSymbols As New List(Of KeyValuePair(Of String, Object)) From {
                                    KeyValuePair.Create(Of String, Object)(convertedFramework, True)}
        If Not convertedFramework.Equals(framework, StringComparison.OrdinalIgnoreCase) Then
            vbPreprocessorSymbols.Add(KeyValuePair.Create(Of String, Object)(framework, True))
        End If

        For Each currentDocument As Document In currentProject.Documents
            If mainForm._cancellationTokenSource.IsCancellationRequested Then
                Return False
            End If
            Dim targetFileWithPath As String = DestinationFilePath(currentDocument.FilePath, solutionRoot)
            filesProcessed += 1
            mainForm.FileListListBox.Items.Add(New NumberedListItem($"{filesProcessed.ToString(CultureInfo.InvariantCulture), -5} {currentDocument.FilePath}", $"{targetFileWithPath}{Path.DirectorySeparatorChar}{Path.GetFileNameWithoutExtension(currentDocument.Name)}.vb"))
            mainForm.FileListListBox.SelectedIndex = mainForm.FileListListBox.Items.Count - 1
            mainForm.StatusStripConversionFileProgressLabel.Text = $"Processed {filesProcessed:N0} of {totalFilesToProcess:N0} Files"
            Application.DoEvents()
            If Not Await ProcessFileAsync(mainForm,
                                          currentDocument.FilePath,
                                          targetFileWithPath,
                                          "cs",
                                          csPreprocessorSymbols,
                                          vbPreprocessorSymbols,
                                          references,
                                          skipAutoGenerated:=True,
                                          cancelToken:=mainForm._cancellationTokenSource.Token).ConfigureAwait(True) _
                                                    OrElse mainForm._requestToConvert.CancelToken.IsCancellationRequested Then
                Return False
            End If
        Next
        Return True
    End Function

    Friend Async Sub ProcessProjectOrSolutionAsync(mainForm As Form1, fileName As String)
        mainForm._cancellationTokenSource = New CancellationTokenSource
        Dim saveSolutionRoot As String = mainForm.GetSavePath(fileName, promptIfDirExists:=True).SolutionRoot
        If String.IsNullOrWhiteSpace(saveSolutionRoot) Then
            mainForm.UpdateProgress("")
            MsgBox($"Can't find {saveSolutionRoot}, exiting solution conversion")
            Exit Sub
        End If
        SetButtonStopAndCursor(mainForm, mainForm.ButtonStopConversion, stopButtonVisible:=True)
        mainForm.ErrorListListBox.Items.Clear()
        mainForm.FileListListBox.Items.Clear()
        mainForm.ConversionInput.Clear()
        mainForm.ConversionOutput.Clear()
        mainForm.UpdateProgressLabels($"Getting Analyzer Manger for {fileName}")
        ' Allow user to read
        Await Task.Delay(5000, mainForm._cancellationTokenSource.Token).ConfigureAwait(True)
        Try
            Dim taskAnalyzerManager As Task(Of AnalyzerManager) = GetManagerAsync(fileName)
            While Not taskAnalyzerManager.IsCompleted
                If mainForm._cancellationTokenSource.IsCancellationRequested Then
                    Exit Try
                End If
                Await Task.Delay(100, mainForm._cancellationTokenSource.Token).ConfigureAwait(True)
            End While
            Dim solutionAnalyzerManager As AnalyzerManager = taskAnalyzerManager.Result
            mainForm.UpdateProgress("")

            Dim prompt As String = "Conversion stopped."
            If fileName.EndsWith(".sln", StringComparison.OrdinalIgnoreCase) Then
                mainForm.mnuFileLastSolution.Enabled = True
                My.Settings.LastSolution = fileName
                mainForm.mnuFileLastSolution.Text = $"Last Solution - {fileName}"
                My.Settings.Save()
                Dim totalProjects As Integer = solutionAnalyzerManager.Projects.Count
                Dim processedProjects As Integer = 0
                Dim skipProjects As Boolean = My.Settings.StartFolderConvertFromLastFile
                Dim results As (resultsString As String, projectsToBeAdded As List(Of String)) = ("", New List(Of String))
                For Each proj As KeyValuePair(Of String, IProjectAnalyzer) In solutionAnalyzerManager.Projects
                    Dim projectFile As String = proj.Key
                    If Not projectFile.EndsWith(".csproj", StringComparison.OrdinalIgnoreCase) Then
                        totalProjects -= 1
                        Continue For
                    End If
                    processedProjects += 1

                    If skipProjects AndAlso My.Settings.LastProject.Length > 0 Then
                        If projectFile <> My.Settings.LastProject Then
                            Continue For
                        Else
                            skipProjects = False
                        End If
                    End If
                    mainForm.mnuFileLastProject.Text = $"Last Project - {projectFile}"
                    mainForm.mnuFileLastProject.Enabled = True
                    My.Settings.LastProject = projectFile
                    My.Settings.Save()
                    Application.DoEvents()
                    results = Await ProcessProjectAsync(
                        mainForm,
                        taskProjectAnalyzer:=proj.Value,
                        saveSolutionRoot,
                        processedProjects,
                        totalProjects,
                        mainForm._cancellationTokenSource).ConfigureAwait(True)
                    If results.resultsString.Length = 0 Then
                        If mainForm._cancellationTokenSource.Token.IsCancellationRequested Then
                            prompt = $"Conversion canceled, files in {processedProjects} of {totalProjects} projects completed successfully."
                            Exit For
                        Else
                            prompt = $"Conversion completed, files in {totalProjects} projects completed successfully."
                        End If
                    Else
                        prompt = $"Conversion canceled because {results}, {processedProjects} of {totalProjects} projects completed successfully."
                        Exit For
                    End If
                Next
                ConvertSolutionFile(fileName, saveSolutionRoot, results.projectsToBeAdded)
                If prompt.Length > 0 Then
                    MsgBox(prompt,
                           MsgBoxStyle.OkOnly Or If(prompt.Contains("terminated", StringComparison.OrdinalIgnoreCase), MsgBoxStyle.Critical, MsgBoxStyle.Information) Or MsgBoxStyle.MsgBoxSetForeground,
                           Title:="Convert C# to Visual Basic")
                End If
            Else
                ' Single project
                mainForm.UpdateProgressLabels($"Getting Project Analyzer for {fileName}")
                Dim taskProjectAnalyzer As Task(Of IProjectAnalyzer) = GetProjectAnalyzerAsync(fileName, solutionAnalyzerManager)
                While Not taskProjectAnalyzer.IsCompleted
                    If mainForm._cancellationTokenSource.IsCancellationRequested Then
                        Exit Sub
                    End If
                    Await Task.Delay(100).ConfigureAwait(True)
                End While
                mainForm.UpdateProgressLabels("")
                mainForm.mnuFileLastProject.Text = $"Last Project - {fileName}"
                mainForm.mnuFileLastProject.Enabled = True
                My.Settings.LastProject = fileName
                My.Settings.Save()
                prompt = (Await ProcessProjectAsync(mainForm, taskProjectAnalyzer.Result,
                    saveSolutionRoot,
                    processedProjects:=1,
                    totalProjects:=1,
                    mainForm._cancellationTokenSource).ConfigureAwait(True)).ErrorPrompt

                Dim conversionComplete As Boolean = prompt.Length = 0

                If conversionComplete Then
#Disable Warning CA1308 ' Normalize strings to uppercase
                    prompt = $"{If(mainForm._cancellationTokenSource.Token.IsCancellationRequested, "Conversion canceled", "Conversion completed")}, {mainForm.StatusStripConversionFileProgressLabel.Text.ToLower(CultureInfo.InvariantCulture)} completed successfully."
#Enable Warning CA1308 ' Normalize strings to uppercase
                End If
                MsgBox(prompt,
                       MsgBoxStyle.OkOnly Or If(prompt.Contains("terminated", StringComparison.OrdinalIgnoreCase), MsgBoxStyle.Critical, MsgBoxStyle.Information) Or MsgBoxStyle.MsgBoxSetForeground,
                       Title:="Convert C# to Visual Basic")
                If conversionComplete Then
                    Dim projectSavePath As String = DestinationFilePath(fileName, saveSolutionRoot)
                    If Directory.Exists(projectSavePath) Then
                        Process.Start("explorer.exe", $"/root,{projectSavePath}")
                    End If
                End If
            End If
        Catch ex As ObjectDisposedException
            End
        Finally
            mainForm.UpdateProgress("")
            SetButtonStopAndCursor(mainForm, mainForm.ButtonStopConversion, stopButtonVisible:=False)
        End Try
    End Sub

    <Extension>
    Friend Sub UpdateProgressLabels(mainForm As Form1, progressStr As String)
        If mainForm.InvokeRequired Then
            mainForm.Invoke(Sub()
                                mainForm.UpdateProgress(progressStr)
                            End Sub)
        Else
            mainForm.UpdateProgress(progressStr)
        End If
    End Sub

End Module
