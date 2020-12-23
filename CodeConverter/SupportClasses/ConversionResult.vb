' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Formatting
Imports Microsoft.CodeAnalysis.Options
Imports Microsoft.CodeAnalysis.Text

Imports VB = Microsoft.CodeAnalysis.VisualBasic

Namespace CSharpToVBConverter

    Public Class ConversionResult

        Private _filteredListOfFailures As List(Of Diagnostic)

        Friend Sub New(ConvertedTree As SyntaxNode, InputLanguage As String, OutputLanguage As String, VBPreprocessorSymbols As List(Of KeyValuePair(Of String, Object)))
            Me.Exceptions = New List(Of Exception)
            Me.SourceLanguage = InputLanguage
            Me.ResultStatus = ResultTriState.Success
            Me.TargetLanguage = OutputLanguage
            Using workspace As New AdhocWorkspace()
                Dim project As Project = workspace.CurrentSolution.AddProject("Project", "Project.dll", OutputLanguage).WithParseOptions(GetVBParseOptions(VBPreprocessorSymbols))
                Dim syntaxTree As SyntaxTree = project.AddDocument("Document", ConvertedTree).GetSyntaxTreeAsync().Result
                Dim root As SyntaxNode = syntaxTree.GetRootAsync().GetAwaiter.GetResult
                Try
                    Me.ConvertedCode = WorkspaceFormat(workspace, root, spans:=Nothing, workspace.Options, project.AddDocument("Document", ConvertedTree).GetTextAsync().GetAwaiter.GetResult)
                    Me.ConvertedTree = DirectCast(root, VB.VisualBasicSyntaxNode)
                    Exit Sub
                Catch ex As Exception
                    Stop
                End Try

                Dim tree As SyntaxTree = VB.VisualBasicSyntaxTree.ParseText(root.NormalizeWhitespaceEx(useDefaultCasing:=True).ToFullString)
                Dim root1 As SyntaxNode = tree.GetRootAsync().GetAwaiter.GetResult
                Try
                    Me.ConvertedCode = WorkspaceFormat(workspace, root1, spans:=Nothing, workspace.Options, project.AddDocument("Document", ConvertedTree).GetTextAsync().GetAwaiter.GetResult)
                    Me.ConvertedTree = DirectCast(root1, VB.VisualBasicSyntaxNode)
                Catch ex As Exception
                    Me.ConvertedCode = DirectCast(root, VB.VisualBasicSyntaxNode).ToFullString
                End Try
                Me.ConvertedTree = DirectCast(root1, VB.VisualBasicSyntaxNode)
            End Using
        End Sub

        Friend Sub New(ParamArray exceptions() As Exception)
            Me.ResultStatus = If(exceptions.Any, ResultTriState.Failure, ResultTriState.Ignore)
            Me.Exceptions = exceptions
        End Sub

        ''' <summary>
        ''' Indicates File Conversion succeeded, failed or wasn't attempted (ignored)
        ''' </summary>
        Public Enum ResultTriState

            '     This file was ignored setting.
            Ignore = -2

            '     Conversion succeeded.
            Success = -1

            '     Conversion failed.
            Failure = 0

        End Enum

        Public ReadOnly Property ConvertedCode As String

        Public Property ConvertedTree As VB.VisualBasicSyntaxNode

        Public Property Exceptions As IReadOnlyList(Of Exception)
        Public Property ResultStatus As ResultTriState

        Public Property SourceLanguage As String

        Public Property TargetLanguage As String

        Protected Shared Function WorkspaceFormat(workspace As Workspace, root As SyntaxNode, spans As IEnumerable(Of TextSpan), pOptionSet As OptionSet, pSourceText As SourceText) As String
            Dim result As IList(Of TextChange) = Formatter.GetFormattedTextChanges(root, spans, workspace, pOptionSet)
            Return pSourceText?.WithChanges(result).ToString()
        End Function

        Public Function GetFilteredListOfFailures() As List(Of Diagnostic)
            If _filteredListOfFailures IsNot Nothing Then
                For Each d As Diagnostic In _filteredListOfFailures
                    If d.Id = "BC30689" Then
                        Return New List(Of Diagnostic)
                    End If
                Next
            End If
            Return _filteredListOfFailures
        End Function

        Public Sub SetFilteredListOfFailures(AutoPropertyValue As List(Of Diagnostic))
            _filteredListOfFailures = AutoPropertyValue
        End Sub

    End Class

End Namespace
