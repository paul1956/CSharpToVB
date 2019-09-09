' Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

Imports System.Collections.Immutable
Imports System.Threading

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Formatting
Imports Microsoft.CodeAnalysis.Options
Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.UnitTests.Formatting
Imports Microsoft.CodeAnalysis.VisualBasic

Imports Roslyn.Test.Utilities

Public NotInheritable Class VisualBasicFormattingTestBase
    Inherits FormattingTestBase
    Implements IDisposable

    Private _ws As Workspace

    Protected ReadOnly Property DefaultWorkspace As Workspace
        Get
            If _ws Is Nothing Then
                _ws = New AdhocWorkspace()
            End If

            Return _ws
        End Get
    End Property

    Private Shared Function StringFromLines(ParamArray lines As String()) As String
        Return String.Join(Environment.NewLine, lines)
    End Function

    Protected Shared Function CreateMethod(ParamArray lines() As String) As String
        Dim adjustedLines As New List(Of String) From {
            "Class C",
            "    Sub Method()"
        }
        adjustedLines.AddRange(lines)
        adjustedLines.Add("    End Sub")
        adjustedLines.Add("End Class")

        Return StringFromLines(adjustedLines.ToArray())
    End Function

    Protected Overloads Function AssertFormatAsync(
        expected As String,
        code As String,
        spans As IEnumerable(Of TextSpan),
        Optional changedOptionSet As Dictionary(Of OptionKey, Object) = Nothing,
        Optional testWithTransformation As Boolean = False,
        Optional experimental As Boolean = False) As Task

        Dim parseOptions As New VisualBasicParseOptions(LanguageVersion.VisualBasic16, DocumentationMode.None)
        If (experimental) Then
            ' There are no experimental features at this time.
            ' parseOptions = parseOptions.WithExperimentalFeatures
        End If

        Return AssertFormatAsync(expected, code, spans, LanguageNames.VisualBasic, changedOptionSet, testWithTransformation, parseOptions)
    End Function

    Protected Function AssertFormatLf2CrLfAsync(code As String, expected As String, Optional optionSet As Dictionary(Of OptionKey, Object) = Nothing) As Task
        If code Is Nothing Then
            Throw New ArgumentNullException(NameOf(code))
        End If
        If expected Is Nothing Then
            Throw New ArgumentNullException(NameOf(expected))
        End If
        code = code.Replace(vbLf, vbCrLf, StringComparison.InvariantCulture)
        expected = expected.Replace(vbLf, vbCrLf, StringComparison.InvariantCulture)

        Return AssertFormatAsync(code, expected, changedOptionSet:=optionSet)
    End Function

    Protected Async Function AssertFormatUsingAllEntryPointsAsync(code As String, expected As String) As Task
        Using workspace As AdhocWorkspace = New AdhocWorkspace()

            Dim project As Project = workspace.CurrentSolution.AddProject("Project", "Project.dll", LanguageNames.VisualBasic)
            Dim document As Document = project.AddDocument("Document", SourceText.From(code))
            Dim syntaxTree As SyntaxTree = Await document.GetSyntaxTreeAsync().ConfigureAwait(False)

            ' Test various entry points into the formatter

            Dim spans As New List(Of TextSpan) From {
                syntaxTree.GetRoot().FullSpan
            }

            Dim changes As IList(Of TextChange) = Formatter.GetFormattedTextChanges(Await syntaxTree.GetRootAsync().ConfigureAwait(False), workspace, cancellationToken:=CancellationToken.None)
            AssertResult(expected, Await document.GetTextAsync().ConfigureAwait(False), changes)

            changes = Formatter.GetFormattedTextChanges(Await syntaxTree.GetRootAsync().ConfigureAwait(False), (Await syntaxTree.GetRootAsync().ConfigureAwait(False)).FullSpan, workspace, cancellationToken:=CancellationToken.None)
            AssertResult(expected, Await document.GetTextAsync().ConfigureAwait(False), changes)

            spans = New List(Of TextSpan) From {
                syntaxTree.GetRoot().FullSpan
            }

            changes = Formatter.GetFormattedTextChanges(Await syntaxTree.GetRootAsync().ConfigureAwait(False), spans, workspace, cancellationToken:=CancellationToken.None)
            AssertResult(expected, Await document.GetTextAsync().ConfigureAwait(False), changes)

            ' format with node and transform
            AssertFormatWithTransformation(workspace, expected, syntaxTree.GetRoot(), spans, Nothing, False)
        End Using
    End Function

    Protected Overrides Function ParseCompilation(text As String, parseOptions As ParseOptions) As SyntaxNode
        Return SyntaxFactory.ParseCompilationUnit(text, options:=DirectCast(parseOptions, VisualBasicParseOptions))
    End Function

    Friend Overloads Function AssertFormatAsync(
        code As String,
        expected As String,
        Optional changedOptionSet As Dictionary(Of OptionKey, Object) = Nothing,
        Optional testWithTransformation As Boolean = False,
        Optional experimental As Boolean = False) As Task
        Return AssertFormatAsync(expected, code, SpecializedCollection.SingletonEnumerable(New TextSpan(0, code.Length)), changedOptionSet, testWithTransformation, experimental:=experimental)
    End Function

    Friend Function AssertFormatSpanAsync(markupCode As String, expected As String) As Task
        Dim code As String = Nothing
        Dim spans As ImmutableArray(Of TextSpan) = Nothing
        MarkupTestFile.GetSpans(markupCode, code, spans)

        Return AssertFormatAsync(expected, code, spans)
    End Function

    Public Sub Dispose() Implements IDisposable.Dispose
        If _ws IsNot Nothing Then
            _ws.Dispose()
        End If
    End Sub

End Class
