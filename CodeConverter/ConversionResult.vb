Option Explicit On
Option Infer Off
Option Strict On

Imports System.Diagnostics.CodeAnalysis

Imports IVisualBasicCode.CodeConverter.Util

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Formatting
Imports Microsoft.CodeAnalysis.Options
Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.VisualBasic
Imports VB = Microsoft.CodeAnalysis.VisualBasic

Namespace IVisualBasicCode.CodeConverter

    Public Class ConversionResult
        Private ReadOnly PreprocessorSymbols As New List(Of KeyValuePair(Of String, Object)) From {
                    New KeyValuePair(Of String, Object)("NETSTANDARD2_0", True),
                    New KeyValuePair(Of String, Object)("NET46", True),
                    New KeyValuePair(Of String, Object)("NETCOREAPP2_0", True)
                }

        Public Sub New(_ConvertedTree As SyntaxNode, InputLanguage As String, OutputLanguage As String)
            Me.Exceptions = New List(Of Exception)
            Me.SourceLanguage = InputLanguage
            Me.Success = True
            Me.TargetLanguage = OutputLanguage
            Using Workspace As New AdhocWorkspace()
                Dim project As Project = Workspace.CurrentSolution.AddProject("Project", "Project.dll", OutputLanguage)

                Dim ParseOptions As VisualBasicParseOptions = New VisualBasicParseOptions(
                        LanguageVersion.VisualBasic16,
                        DocumentationMode.Diagnose,
                        SourceCodeKind.Regular,
                        Me.PreprocessorSymbols)

                project = project.WithParseOptions(ParseOptions)

                Dim _Document As Document = project.AddDocument("Document", _ConvertedTree)
                Dim _SyntaxTree As SyntaxTree = _Document.GetSyntaxTreeAsync().Result

                Dim Root As SyntaxNode = _SyntaxTree.GetRootAsync().Result
                Me.ConvertedCode = WorkspaceFormat(Workspace, Root, spans:=Nothing, Workspace.Options, _Document.GetTextAsync().Result)
                Me.ConvertedTree = DirectCast(Root, VB.VisualBasicSyntaxNode)
            End Using

        End Sub

        Protected Shared Function WorkspaceFormat(workspace As Workspace, root As SyntaxNode, spans As IEnumerable(Of TextSpan), _OptionSet As OptionSet, _SourceText As SourceText) As String
            Dim result As IList(Of TextChange) = Formatter.GetFormattedTextChanges(root, spans, workspace, _OptionSet)
            Return _SourceText.WithChanges(result).ToString()
        End Function

        <ExcludeFromCodeCoverage>
        Public Sub New(ParamArray exceptions() As Exception)
            Me.Success = False
            Me.Exceptions = exceptions
        End Sub

        Public ReadOnly Property ConvertedCode As String
        Public Property ConvertedTree As VB.VisualBasicSyntaxNode
        Public Property Exceptions As IReadOnlyList(Of Exception)
        Public Property FilteredListOfFailures As List(Of Diagnostic)
        Public Property SourceLanguage As String
        Public Property Success As Boolean
        Public Property TargetLanguage As String
    End Class

End Namespace