' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.IO

Imports Buildalyzer
Imports Buildalyzer.Workspaces

Imports Microsoft.CodeAnalysis

Public Module ProcessOneProjectUtilities

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

End Module
