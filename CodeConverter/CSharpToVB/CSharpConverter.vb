' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.Threading

Imports CSharpToVBCodeConverter.Utilities

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBCodeConverter.ToVisualBasic

    Partial Public Class CSharpConverter

        ''' <summary>
        ''' Converts C# VariableDeclaration to VB List(Of VariableDeclaratorSyntax)
        ''' </summary>
        ''' <param name="variableDeclaration"></param>
        ''' <param name="Visitor"></param>
        ''' <param name="Model"></param>
        ''' <param name="IsFieldDeclaration"></param>
        ''' <param name="LeadingTrivia"></param>
        ''' <returns></returns>
        ''' <remarks>Fix handling of AddressOf where is mistakenly added to DirectCast and C# Pointers</remarks>
        Private Shared Function RemodelVariableDeclaration(variableDeclaration As CSS.VariableDeclarationSyntax, Visitor As NodesVisitor, Model As SemanticModel, IsFieldDeclaration As Boolean, ByRef LeadingTrivia As List(Of SyntaxTrivia)) As SeparatedSyntaxList(Of VBS.VariableDeclaratorSyntax)
            Dim vbType As VBS.TypeSyntax
            Dim declarationType As VB.VisualBasicSyntaxNode = variableDeclaration.Type.Accept(Visitor)
            Dim typeOrAddressOf As VB.VisualBasicSyntaxNode = declarationType.WithConvertedLeadingTriviaFrom(variableDeclaration.Type)
            Dim typeLeadingTrivia As SyntaxTriviaList = typeOrAddressOf.GetLeadingTrivia

            If typeLeadingTrivia.Any Then
                If typeLeadingTrivia.Last.RawKind = VB.SyntaxKind.WhitespaceTrivia Then
                    typeOrAddressOf = typeOrAddressOf.WithLeadingTrivia(typeLeadingTrivia.Last)
                Else
                    typeOrAddressOf = typeOrAddressOf.WithLeadingTrivia(SpaceTrivia)
                End If
            End If
            If IsFieldDeclaration AndAlso typeLeadingTrivia.Count > 1 Then
                Dim statementWithIssues As CS.CSharpSyntaxNode = GetStatementwithIssues(variableDeclaration)
                If typeLeadingTrivia.ContainsCommentOrDirectiveTrivia AndAlso Not TriviaIsIdentical(typeLeadingTrivia, ConvertTrivia(statementWithIssues.GetLeadingTrivia).ToList) Then
                    statementWithIssues.AddMarker(VBFactory.EmptyStatement.WithLeadingTrivia(typeLeadingTrivia), StatementHandlingOption.AppendEmptyStatement, AllowDuplicates:=True)
                End If
            End If
            Dim csCollectedCommentTrivia As New List(Of SyntaxTrivia)
            If typeOrAddressOf.IsKind(VB.SyntaxKind.AddressOfExpression) Then
                vbType = IntPtrType
            Else
                vbType = DirectCast(typeOrAddressOf, VBS.TypeSyntax)
            End If
            vbType = vbType.WithModifiedNodeTrivia(SeparatorFollows:=True)
            Dim csDeclaratorsWithoutInitializers As New List(Of CSS.VariableDeclaratorSyntax)()
            Dim vbDeclarators As New List(Of VBS.VariableDeclaratorSyntax)

            For Each v As CSS.VariableDeclaratorSyntax In variableDeclaration.Variables
                If v.Initializer Is Nothing Then
                    csDeclaratorsWithoutInitializers.Add(v.WithAppendedTrailingTrivia(csCollectedCommentTrivia))
                    csCollectedCommentTrivia.Clear()
                    Continue For
                End If
                Dim asClause As VBS.AsClauseSyntax = Nothing
                If variableDeclaration.Type.IsKind(CS.SyntaxKind.RefType) Then
                ElseIf Not variableDeclaration.Type.IsVar Then
                    asClause = VBFactory.SimpleAsClause(vbType)
                Else
                    ' Get Type from Initializer
                    If v.Initializer.Value.IsKind(CS.SyntaxKind.AnonymousObjectCreationExpression) Then
                        asClause = VBFactory.AsNewClause(CType(v.Initializer.Value.Accept(Visitor), VBS.NewExpressionSyntax))
                    ElseIf v.Initializer.Value.IsKind(CS.SyntaxKind.ImplicitArrayCreationExpression) Then
                    Else
                        Dim resultTuple As (_Error As Boolean, _TypeSyntax As VBS.TypeSyntax) = DetermineTypeSyntax(v.Initializer.Value, Model)
                        If Not resultTuple._Error Then
                            asClause = VBFactory.SimpleAsClause(resultTuple._TypeSyntax)
                        Else
                            asClause = Nothing
                        End If
                    End If
                End If
                Dim initializerValue As VBS.ExpressionSyntax = DirectCast(v.Initializer.Value.Accept(Visitor), VBS.ExpressionSyntax)
                If initializerValue Is Nothing Then
                    initializerValue = VBFactory.IdentifierName("HandleRefExpression").WithConvertedTriviaFrom(v.Initializer.Value)
                End If
                If initializerValue.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                    LeadingTrivia.AddRange(initializerValue.GetLeadingTrivia)
                End If
                Dim initializer As VBS.EqualsValueSyntax = Nothing
                If Not asClause.IsKind(VB.SyntaxKind.AsNewClause) Then
                    initializer = VBFactory.EqualsValue(initializerValue.WithLeadingTrivia(SpaceTrivia))
                    If initializer.Value.IsKind(VB.SyntaxKind.ObjectCreationExpression) Then
                        If asClause IsNot Nothing AndAlso CType(asClause, VBS.SimpleAsClauseSyntax).Type.ToString = CType(initializerValue, VBS.ObjectCreationExpressionSyntax).Type.ToString Then
                            asClause = VBFactory.AsNewClause(CType(initializerValue, VBS.ObjectCreationExpressionSyntax))
                            initializer = Nothing
                        End If
                    End If
                End If
                ' Get the names last to lead with var jsonWriter = new JsonWriter(stringWriter)
                ' Which should be Dim jsonWriter_Renamed = new JsonWriter(stringWriter)
                vbDeclarators.Add(
                    VBFactory.VariableDeclarator(VBFactory.SingletonSeparatedList(DirectCast(v.Accept(Visitor), VBS.ModifiedIdentifierSyntax)),
                                                asClause,
                                                initializer
                                                ).WithModifiedNodeTrailingTrivia(SeparatorFollows:=False)
                                )
            Next
            If csDeclaratorsWithoutInitializers.Any Then
                Dim ModifiedIdentifierList As New List(Of VBS.ModifiedIdentifierSyntax)
                For Each csVarDeclaration As CSS.VariableDeclaratorSyntax In csDeclaratorsWithoutInitializers
                    Dim dTrailingTrivia As SyntaxTriviaList = csVarDeclaration.GetTrailingTrivia
                    If csVarDeclaration.HasTrailingTrivia And dTrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                        csCollectedCommentTrivia.AddRange(dTrailingTrivia)
                    End If
                    ModifiedIdentifierList.Add(DirectCast(csVarDeclaration.Accept(Visitor), VBS.ModifiedIdentifierSyntax).WithTrailingTrivia(SpaceTrivia))
                Next
                Dim varDeclarator As VBS.VariableDeclaratorSyntax = VBFactory.VariableDeclarator(VBFactory.SeparatedList(ModifiedIdentifierList), asClause:=VBFactory.SimpleAsClause(vbType), initializer:=Nothing)
                vbDeclarators.Insert(0, varDeclarator.WithTrailingTrivia(ConvertTrivia(csCollectedCommentTrivia)))
                csCollectedCommentTrivia.Clear()
            End If
            If csCollectedCommentTrivia.Any Then
                Dim finalTrivia As New List(Of SyntaxTrivia)
                finalTrivia.AddRange(ConvertTrivia(csCollectedCommentTrivia))
                finalTrivia.AddRange(vbDeclarators.Last.GetTrailingTrivia)
                Dim tempDeclarator As VBS.VariableDeclaratorSyntax = vbDeclarators.Last.WithTrailingTrivia(finalTrivia)
                vbDeclarators.RemoveAt(vbDeclarators.Count - 1)
                vbDeclarators.Add(tempDeclarator)
            End If
            Return VBFactory.SeparatedList(vbDeclarators)
        End Function

        ''' <summary>
        ''' Entry Point for converting source and new applications
        ''' </summary>
        ''' <param name="SourceTree"></param>
        ''' <param name="SkipAutoGenerated"></param>
        ''' <param name="DefaultVBOptions"></param>
        ''' <param name="pSemanticModel"></param>
        ''' <returns></returns>
        Public Shared Function Convert(SourceTree As CS.CSharpSyntaxNode, SkipAutoGenerated As Boolean, DefaultVBOptions As DefaultVBOptions, pSemanticModel As SemanticModel, ReportException As Action(Of Exception), Progress As IProgress(Of ProgressReport), CancelToken As CancellationToken) As VB.VisualBasicSyntaxNode
            IgnoredIfDepth = 0
            IfDepth = 0
            Dim visualBasicSyntaxNode1 As VB.VisualBasicSyntaxNode
            s_originalRequest = New ConvertRequest(SkipAutoGenerated, Progress, CancelToken)
            SyncLock s_thisLock
                ClearMarker()
                s_usedStacks.Push(s_usedIdentifiers)
                s_usedIdentifiers.Clear()
                visualBasicSyntaxNode1 = SourceTree?.Accept(New NodesVisitor(pSemanticModel, DefaultVBOptions, ReportException))
                If s_usedStacks.Count > 0 Then
                    s_usedIdentifiers = DirectCast(s_usedStacks.Pop, Dictionary(Of String, SymbolTableEntry))
                End If
            End SyncLock
            Return visualBasicSyntaxNode1
        End Function

        ''' <summary>
        ''' Entry Point for converting source, used in testing and legacy applications
        ''' </summary>
        ''' <param name="SourceTree"></param>
        ''' <param name="SkipAutoGenerated"></param>
        ''' <param name="pSemanticModel"></param>
        ''' <returns></returns>
        <Obsolete("Don't use this routine any more. Use the new one instead to specify what 'Options' to include in output, use 'New DefaultVBOptions' to get the legacy behavior.")>
        Public Shared Function Convert(SourceTree As CS.CSharpSyntaxNode, SkipAutoGenerated As Boolean, pSemanticModel As SemanticModel, ReportException As Action(Of Exception), Progress As IProgress(Of ProgressReport), CancelToken As CancellationToken) As VB.VisualBasicSyntaxNode
            Return Convert(SourceTree, SkipAutoGenerated, New DefaultVBOptions, pSemanticModel, ReportException, Progress, CancelToken)
        End Function

    End Class

End Namespace
