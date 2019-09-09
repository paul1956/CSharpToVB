' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Formatting
Imports Microsoft.CodeAnalysis.Options
Imports Microsoft.CodeAnalysis.Text

Imports VB = Microsoft.CodeAnalysis.VisualBasic

Namespace CSharpToVBCodeConverter

    Public Class ConversionResult

        '
        ' Summary:
        '     Indicates File Conversion succeeded, failed or wasn't attempted (ignored)
        Public Enum ResultTriState

            '     This file was ignored setting.
            Ignore = -2

            '     Conversion succeeded.
            Success = -1

            '     Conversion failed.
            Failure = 0

        End Enum

        Public Sub New(_ConvertedTree As SyntaxNode, InputLanguage As String, OutputLanguage As String)
            Exceptions = New List(Of Exception)
            SourceLanguage = InputLanguage
            ResultStatus = ResultTriState.Success
            TargetLanguage = OutputLanguage
            Using Workspace As New AdhocWorkspace()
                Dim project As Project = Workspace.CurrentSolution.AddProject("Project", "Project.dll", OutputLanguage)

                Dim VBParseOptions As VB.VisualBasicParseOptions = GetVBParseOptions()

                project = project.WithParseOptions(VBParseOptions)

                Dim _Document As Document = project.AddDocument("Document", _ConvertedTree)
                Dim _SyntaxTree As SyntaxTree = _Document.GetSyntaxTreeAsync().Result

                Dim Root As SyntaxNode = _SyntaxTree.GetRootAsync().Result
                ConvertedCode = WorkspaceFormat(Workspace, Root, spans:=Nothing, Workspace.Options, _Document.GetTextAsync().Result)
                ConvertedTree = DirectCast(Root, VB.VisualBasicSyntaxNode)
            End Using

        End Sub

        Public Sub New(ParamArray exceptions() As Exception)
            ResultStatus = If(exceptions.Any, ResultTriState.Failure, ResultTriState.Ignore)
            Me.Exceptions = exceptions
        End Sub

        Public ReadOnly Property ConvertedCode As String

        Public Property ConvertedTree As VB.VisualBasicSyntaxNode

        Public Property Exceptions As IReadOnlyList(Of Exception)
        Private _FilteredListOfFailures As List(Of Diagnostic)

        Public Function GetFilteredListOfFailures() As List(Of Diagnostic)
            Return _FilteredListOfFailures
        End Function

        Public Sub SetFilteredListOfFailures(AutoPropertyValue As List(Of Diagnostic))
            _FilteredListOfFailures = AutoPropertyValue
        End Sub

        Public Property ResultStatus As ResultTriState

        Public Property SourceLanguage As String

        Public Property TargetLanguage As String

        Protected Shared Function WorkspaceFormat(workspace As Workspace, root As SyntaxNode, spans As IEnumerable(Of TextSpan), _OptionSet As OptionSet, _SourceText As SourceText) As String
            Dim result As IList(Of TextChange) = Formatter.GetFormattedTextChanges(root, spans, workspace, _OptionSet)
            Return _SourceText?.WithChanges(result).ToString()
        End Function

    End Class

End Namespace
