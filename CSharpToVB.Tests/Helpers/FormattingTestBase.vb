' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Threading

Imports Microsoft.CodeAnalysis.Formatting
Imports Microsoft.CodeAnalysis.Options
Imports Microsoft.CodeAnalysis.Text

Imports Roslyn.Test.Utilities

Imports Xunit

Namespace Microsoft.CodeAnalysis.UnitTests.Formatting

    '<UseExportProvider>
    Public MustInherit Class FormattingTestBase

        Protected Async Function AssertFormatAsync(
            expected As String,
            code As String,
            spans As IEnumerable(Of TextSpan),
            language As String,
            Optional ChangedOptionSet As Dictionary(Of OptionKey, Object) = Nothing,
            Optional treeCompare As Boolean = True,
            Optional parseOptions As ParseOptions = Nothing) As Task
            Using workspace As AdhocWorkspace = New AdhocWorkspace()
                Dim project As Project = workspace.CurrentSolution.AddProject("Project", "Project.dll", language)
                If parseOptions IsNot Nothing Then
                    project = project.WithParseOptions(parseOptions)
                End If

                Dim doc As Document = project.AddDocument("Document", SourceText.From(code))

                Dim tree As SyntaxTree = Await doc.GetSyntaxTreeAsync().ConfigureAwait(False)

                Dim options As OptionSet = workspace.Options
                If ChangedOptionSet IsNot Nothing Then
                    For Each entry As KeyValuePair(Of OptionKey, Object) In ChangedOptionSet
                        options = options.WithChangedOption(entry.Key, entry.Value)
                    Next
                End If

                AssertFormat(workspace,
                             expected,
                             Await tree.GetRootAsync().ConfigureAwait(False),
                             spans,
                             options,
                             Await doc.GetTextAsync().ConfigureAwait(False))

                ' format with node and transform
                Me.AssertFormatWithTransformation(workspace, expected, Await tree.GetRootAsync().ConfigureAwait(False), spans, options, treeCompare, parseOptions)
            End Using
        End Function

        Protected Sub AssertFormatWithTransformation(
            workspace As Workspace, expected As String, root As SyntaxNode, spans As IEnumerable(Of TextSpan), optionSet As OptionSet, Optional treeCompare As Boolean = True, Optional parseOptions As ParseOptions = Nothing)
            Dim newRootNode As SyntaxNode = Formatter.Format(root, spans, workspace, optionSet, CancellationToken.None)

            Assert.Equal(expected, newRootNode.ToFullString())

            ' test doesn't use parsing option. add one if needed later
            Dim newRootNodeFromString As SyntaxNode = Me.ParseCompilation(expected, parseOptions)
            If treeCompare Then
                ' simple check to see whether two nodes are equivalent each other.
                Assert.[True](newRootNodeFromString.IsEquivalentTo(newRootNode))
            End If
        End Sub

        Protected MustOverride Function ParseCompilation(text As String, parseOptions As ParseOptions) As SyntaxNode

        Friend Shared Sub AssertFormat(workspace As Workspace, expected As String, root As SyntaxNode, spans As IEnumerable(Of TextSpan), OptionSet As OptionSet, SourceText As SourceText)
            Dim result As IList(Of TextChange) = Formatter.GetFormattedTextChanges(root, spans, workspace, OptionSet)
            AssertResult(expected, SourceText, result)
        End Sub

        Friend Shared Sub AssertResult(expected As String, sourceText As SourceText, result As IList(Of TextChange))
            Dim actual As String = sourceText.WithChanges(result).ToString()
            EqualOrDiff(expected, actual)
        End Sub

    End Class

End Namespace
