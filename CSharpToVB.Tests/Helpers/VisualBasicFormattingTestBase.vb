﻿' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.Collections.Immutable
Imports Helpers.Portable.MarkedSource
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Options
Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.VisualBasic

Namespace Helpers

    Public NotInheritable Class VisualBasicFormattingTestBase
        Inherits FormattingTestBase
        Implements IDisposable

        Private ReadOnly _ws As Workspace

        Protected Overrides Function ParseCompilation(text As String, parseOptions As ParseOptions) As SyntaxNode
            Return SyntaxFactory.ParseCompilationUnit(text, options:=DirectCast(parseOptions, VisualBasicParseOptions))
        End Function

        Private Overloads Function AssertFormatAsync(expected As String,
                                   code As String,
                                   spans As ImmutableArray(Of TextSpan),
                                   Optional changedOptionSet As Dictionary(Of OptionKey, Object) = Nothing,
                                   Optional testWithTransformation As Boolean = False,
                                   Optional experimental As Boolean = False) As Task

            Dim parseOptions As New VisualBasicParseOptions(LanguageVersion.VisualBasic16, DocumentationMode.None)
            If experimental Then
                ' There are no experimental features at this time.
                ' parseOptions = parseOptions.WithExperimentalFeatures
            End If

            Return Me.AssertFormatAsync(expected, code, spans, LanguageNames.VisualBasic, changedOptionSet, testWithTransformation, parseOptions)
        End Function

        Friend Function AssertFormatSpanAsync(markupCode As String, expected As String) As Task
            Dim code As String = Nothing
            Dim spans As ImmutableArray(Of TextSpan) = Nothing
            GetSpans(markupCode, code, spans)

            Return Me.AssertFormatAsync(expected, code, spans)
        End Function

        Public Sub Dispose() Implements IDisposable.Dispose
            If _ws IsNot Nothing Then
                _ws.Dispose()
            End If
        End Sub

    End Class

End Namespace
