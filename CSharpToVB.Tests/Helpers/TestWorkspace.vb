﻿Option Explicit On
Option Infer Off
Option Strict On

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Host
Imports Microsoft.CodeAnalysis.Text

Namespace CodeConverter.Tests

    Friend Class TestWorkspace
        Inherits Workspace

        Private Shared Shadows ReadOnly services As HostServices = Microsoft.CodeAnalysis.Host.Mef.MefHostServices.DefaultHost
        ' MefHostServices.Create(new [] {
        '				typeof(MefHostServices).Assembly,
        '				typeof(Microsoft.CodeAnalysis.CSharp.Formatting.CSharpFormattingOptions).Assembly
        '			});

        Public Sub New(Optional workspaceKind As String = "Test")
            MyBase.New(services, workspaceKind)
            '
            '			foreach (var a in MefHostServices.DefaultAssemblies)
            '			{
            '				Console.WriteLine(a.FullName);
            '			}
        End Sub

        Public Sub ChangeDocument(id As DocumentId, text As SourceText)
            Me.ApplyDocumentTextChanged(id, text)
        End Sub

        Protected Overrides Sub ApplyDocumentTextChanged(id As DocumentId, text As SourceText)
            MyBase.ApplyDocumentTextChanged(id, text)
            Dim document As Document = Me.CurrentSolution.GetDocument(id)
            If document IsNot Nothing Then
                Me.OnDocumentTextChanged(id, text, PreservationMode.PreserveValue)
            End If
        End Sub

        Public Overrides Function CanApplyChange(feature As ApplyChangesKind) As Boolean
            Return True
        End Function

        Public Sub Open(projectInfo As ProjectInfo)
            Dim sInfo As SolutionInfo = SolutionInfo.Create(SolutionId.CreateNewId(), VersionStamp.Create(), Nothing, {projectInfo})
            Me.OnSolutionAdded(sInfo)
        End Sub

    End Class

End Namespace