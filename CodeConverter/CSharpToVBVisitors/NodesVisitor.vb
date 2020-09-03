' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Diagnostics.CodeAnalysis
Imports System.Text

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Simplification
Imports Microsoft.CodeAnalysis.VisualBasic

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBCodeConverter.ToVisualBasic

    Partial Public Class CSharpConverter

        Partial Friend Class NodesVisitor
            Inherits CS.CSharpSyntaxVisitor(Of VisualBasicSyntaxNode)

            ' This file contains all the stuff accessed by multiple Visitor functions in Class NodeVisitor and Visitors that
            ' had no better home.

            Private ReadOnly _commonConversions As CommonConversions
            Private ReadOnly _defaultVBOptions As DefaultVBOptions
            Private ReadOnly _isModuleStack As New Stack(Of Boolean)
            Private ReadOnly _mSemanticModel As SemanticModel
            Private ReadOnly _placeholder As Integer = 1
            Private ReadOnly _reportException As Action(Of Exception)
            Private _membersList As SyntaxList(Of VBS.StatementSyntax)
            Public ReadOnly AllImports As New List(Of VBS.ImportsStatementSyntax)()
            Public ReadOnly InlineAssignHelperMarkers As New List(Of CSS.BaseTypeDeclarationSyntax)()
            'Public ReadOnly ByRefHelperMarkers As New List(Of CSS.BaseTypeDeclarationSyntax)()
            Public VBHeaderLeadingTrivia As SyntaxTriviaList

            Public Sub New(lSemanticModel As SemanticModel, DefaultVBOptions As DefaultVBOptions, ReportException As Action(Of Exception))
                _mSemanticModel = lSemanticModel
                _reportException = ReportException
                _defaultVBOptions = DefaultVBOptions
                _commonConversions = New CommonConversions(lSemanticModel)
            End Sub

            Public ReadOnly Property IsModule As Boolean
                Get
                    If _isModuleStack.Count = 0 Then
                        Return False
                    End If
                    Return _isModuleStack.Peek
                End Get
            End Property

            Private Shared Function IsInvokeIdentifier(sns As CSS.SimpleNameSyntax) As Boolean
                Return sns.Identifier.Value.Equals("Invoke")
            End Function

            Private Function GetIdentifierNameFromName(Expression As VBS.ExpressionSyntax) As VBS.IdentifierNameSyntax
                Select Case True
                    Case TypeOf Expression Is VBS.IdentifierNameSyntax
                        Return DirectCast(Expression, VBS.IdentifierNameSyntax)
                    Case TypeOf Expression Is VBS.MemberAccessExpressionSyntax
                        Dim memberAccess As VBS.MemberAccessExpressionSyntax = DirectCast(Expression, VBS.MemberAccessExpressionSyntax)
                        Return Me.GetIdentifierNameFromName(memberAccess.Name)
                    Case Else
                        Throw New NotSupportedException($"Cannot get SimpleNameSyntax from {Expression.Kind()}:" & vbCrLf & "{expressionSyntax}")
                End Select
            End Function

            <ExcludeFromCodeCoverage>
            Public Overrides Function DefaultVisit(node As SyntaxNode) As VisualBasicSyntaxNode
                Throw New NotImplementedException(node.GetType().ToString & " not implemented!")
            End Function

            Public Overrides Function VisitCompilationUnit(node As CSS.CompilationUnitSyntax) As VisualBasicSyntaxNode
                If node.GetLeadingTrivia.FirstOrDefault.IsKind(CS.SyntaxKind.SingleLineCommentTrivia) Then
                    VBHeaderLeadingTrivia = node.GetLeadingTrivia.GetDocumentBanner
                End If
                For Each [using] As CSS.UsingDirectiveSyntax In node.Usings
                    If s_originalRequest.CancelToken.IsCancellationRequested Then
                        Throw New OperationCanceledException
                    End If
                    [using].Accept(Me)
                Next
                Dim externList As New List(Of VisualBasicSyntaxNode)
                ' externlist is potentially a list of empty lines with trivia
                For Each extern As CSS.ExternAliasDirectiveSyntax In node.Externs
                    externList.Add(extern.Accept(Me))
                Next

                Dim Options As New SyntaxList(Of VBS.OptionStatementSyntax)
                With _defaultVBOptions
                    If .OptionCompareInclude Then
                        Options = Options.Add(Factory.OptionStatement(CompareToken, If(.OptionCompare = "Text", TextToken, BinaryToken)).WithTrailingEOL(RemoveLastLineContinuation:=True))
                    End If
                    If .OptionExplicitInclude Then
                        Options = Options.Add(Factory.OptionStatement(ExplicitToken, If(.OptionExplicit = "On", OnToken, OffToken)).WithTrailingEOL(RemoveLastLineContinuation:=True))
                    End If
                    If .OptionInferInclude Then
                        Options = Options.Add(Factory.OptionStatement(InferToken, If(.OptionInfer = "On", OnToken, OffToken)).WithTrailingEOL(RemoveLastLineContinuation:=True))

                    End If
                    If .OptionStrictInclude Then
                        Options = Options.Add(Factory.OptionStatement(StrictToken, If(.OptionStrict = "On", OnToken, OffToken)).WithTrailingEOL(RemoveLastLineContinuation:=True))
                    End If
                End With
                _membersList = New SyntaxList(Of VBS.StatementSyntax)
                For Each m As CSS.MemberDeclarationSyntax In node.Members
                    If s_originalRequest.CancelToken.IsCancellationRequested Then
                        Throw New OperationCanceledException
                    End If
                    Dim Statement As VBS.StatementSyntax = DirectCast(m.Accept(Me), VBS.StatementSyntax)
                    If Statement Is Nothing Then
                        Options = New SyntaxList(Of VBS.OptionStatementSyntax)
                    Else
                        _membersList = _membersList.AddRange(ReplaceOneStatementWithMarkedStatements(m, Statement))
                    End If
                Next

                Dim ListOfAttributes As List(Of VBS.AttributesStatementSyntax) = node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) Factory.AttributesStatement(Factory.SingletonList(DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))).ToList
                Dim firstImportOrEmpty As VBS.StatementSyntax = Factory.EmptyStatement
                If AllImports.Any Then
                    If _membersList.Any AndAlso _membersList(0).HasLeadingTrivia Then
                        If (TypeOf _membersList(0) IsNot VBS.NamespaceBlockSyntax AndAlso
                                TypeOf _membersList(0) IsNot VBS.ModuleBlockSyntax) OrElse
                                _membersList(0).GetLeadingTrivia.ToFullString.Contains("auto-generated", StringComparison.OrdinalIgnoreCase) Then
                            Dim HeadingTriviaList As SyntaxTriviaList = _membersList(0).GetLeadingTrivia
                            If HeadingTriviaList(0).IsKind(SyntaxKind.EndOfLineTrivia) Then
                                HeadingTriviaList.RemoveAt(0)
                                If HeadingTriviaList.Any Then
                                    HeadingTriviaList.Add(VBEOLTrivia)
                                End If
                            End If
                            Dim NewMemberList As New SyntaxList(Of VBS.StatementSyntax)
                            NewMemberList = NewMemberList.Add(_membersList(0).WithLeadingTrivia(VBSpaceTrivia))
                            _membersList = NewMemberList.AddRange(_membersList.RemoveAt(0))
                            Dim newLeadingTrivia As New SyntaxTriviaList
                            ' Remove Leading whitespace
                            For Each t As SyntaxTrivia In HeadingTriviaList
                                If Not t.IsWhitespaceOrEndOfLine Then
                                    newLeadingTrivia = newLeadingTrivia.Add(t)
                                End If
                            Next
                            AllImports(0) = AllImports(0).WithPrependedLeadingTrivia(newLeadingTrivia)
                        End If
                    End If
                    For Each e As IndexClass(Of VBS.ImportsStatementSyntax) In AllImports.WithIndex
                        Dim importsClause As VBS.ImportsClauseSyntax = e.Value.ImportsClauses.FirstOrDefault
                        If TypeOf importsClause Is VBS.SimpleImportsClauseSyntax Then
                            Dim simpleImportsClause As VBS.SimpleImportsClauseSyntax = DirectCast(importsClause, VBS.SimpleImportsClauseSyntax)
                            If TypeOf simpleImportsClause.Name Is VBS.IdentifierNameSyntax Then
                                If DirectCast(simpleImportsClause.Name, VBS.IdentifierNameSyntax).Identifier.ValueText = "System" Then
                                    If Not e.IsLast Then
                                        AllImports(e.Index + 1) = AllImports(e.Index + 1).WithMergedLeadingTrivia(e.Value.GetLeadingTrivia)
                                    Else
                                        firstImportOrEmpty = Factory.EmptyStatement.WithTriviaFrom(AllImports(e.Index))
                                    End If
                                    AllImports.RemoveAt(e.Index)
                                    Exit For
                                End If
                            End If
                        End If
                    Next
                    If Options.Any Then
                        If VBHeaderLeadingTrivia.Any Then
                            Options = Options.Replace(Options.First, Options.First.WithLeadingTrivia(VBHeaderLeadingTrivia.Add(VBEOLTrivia)))
                            If AllImports.Any AndAlso Not AllImports.First.GetLeadingTrivia.FirstOrDefault.IsKind(SyntaxKind.EndOfLineTrivia) Then
                                Options = Options.Replace(Options.Last, Options.Last.WithAppendedEOL)
                            End If
                        End If
                    End If
                ElseIf Options.Any Then
                    If ListOfAttributes.Any AndAlso ListOfAttributes(0).GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        Options = Options.Replace(Options(0), Options(0).WithLeadingTrivia(ListOfAttributes(0).GetLeadingTrivia))
                        ListOfAttributes(0) = ListOfAttributes(0).WithLeadingTrivia(ListOfAttributes(0).GetLeadingTrivia.WithUniqueTrivia(VBHeaderLeadingTrivia))
                        If Not ListOfAttributes.First.GetLeadingTrivia.FirstOrDefault.IsKind(SyntaxKind.EndOfLineTrivia) Then
                            Options = Options.Replace(Options.Last, Options.Last.WithAppendedEOL)
                        End If
                    ElseIf _membersList.Any AndAlso
                        VBHeaderLeadingTrivia.Any AndAlso
                        _membersList(0).GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then

                        Options = Options.Replace(Options(0), Options(0).WithLeadingTrivia(VBHeaderLeadingTrivia))
                        _membersList = _membersList.Replace(_membersList(0), _membersList(0).WithLeadingTrivia(_membersList(0).GetLeadingTrivia.WithUniqueTrivia(VBHeaderLeadingTrivia)))
                        If Not _membersList(0).GetLeadingTrivia.FirstOrDefault.IsKind(SyntaxKind.EndOfLineTrivia) Then
                            Options = Options.Replace(Options.Last, Options.Last.WithAppendedEOL)
                        End If
                    Else
                        If VBHeaderLeadingTrivia.Any Then
                            Options = Options.Replace(Options(0), Options(0).WithLeadingTrivia(VBHeaderLeadingTrivia.Add(VBEOLTrivia)))
                        End If
                        Options = Options.Replace(Options.Last, Options.Last.WithAppendedEOL)
                    End If
                End If
                If HasMarkerError() Then
                    ' There are statements that were left out of translation
                    Throw New ApplicationException(GetMarkerErrorMessage)
                End If
                Dim compilationUnitSyntax1 As VBS.CompilationUnitSyntax
                If AllImports.Any OrElse _membersList.Any OrElse ListOfAttributes.Any Then
                    compilationUnitSyntax1 = Factory.CompilationUnit(Options,
                                             Factory.List(AllImports),
                                             Factory.List(ListOfAttributes),
                                             _membersList,
                                             EndOfFileToken.WithConvertedTriviaFrom(node.EndOfFileToken))

                Else
                    _membersList = _membersList.Add(Factory.EmptyStatement.WithLeadingTrivia(VBHeaderLeadingTrivia))
                    compilationUnitSyntax1 = Factory.CompilationUnit(options:=Nothing,
                                                                     [imports]:=Nothing,
                                                                     attributes:=Nothing,
                                                                     _membersList,
                                                                     EndOfFileToken.WithConvertedTriviaFrom(node.EndOfFileToken))
                End If
                If externList.Any Then
                    Return compilationUnitSyntax1.
                                    WithTriviaFrom(externList(0))
                End If
                Return compilationUnitSyntax1
            End Function

            Public Overrides Function VisitDeclarationPattern(node As CSS.DeclarationPatternSyntax) As VisualBasicSyntaxNode
                Dim StatementWithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                Dim LeadingTrivia As SyntaxTriviaList = StatementWithIssue.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=True, "VB has no direct equivalent To C# pattern variables 'is' expressions")
                Dim Designation As CSS.SingleVariableDesignationSyntax = DirectCast(node.Designation, CSS.SingleVariableDesignationSyntax)

                Dim value As VBS.ExpressionSyntax = Factory.ParseExpression($"TryCast({node.Designation.Accept(Me).NormalizeWhitespace.ToFullString}, {node.Type.Accept(Me).NormalizeWhitespace.ToFullString})")

                Dim SeparatedSyntaxListOfModifiedIdentifier As SeparatedSyntaxList(Of VBS.ModifiedIdentifierSyntax) =
                        Factory.SingletonSeparatedList(
                            Factory.ModifiedIdentifier(GenerateSafeVBToken(Designation.Identifier, node, _mSemanticModel)))
                Dim VariableType As VBS.TypeSyntax = DirectCast(node.Type.Accept(Me), VBS.TypeSyntax)

                Dim SeparatedListOfvariableDeclarations As SeparatedSyntaxList(Of VBS.VariableDeclaratorSyntax) =
                        Factory.SingletonSeparatedList(
                            Factory.VariableDeclarator(
                                SeparatedSyntaxListOfModifiedIdentifier,
                                Factory.SimpleAsClause(VariableType),
                                Factory.EqualsValue(NothingExpression)
                                    )
                             )

                Dim DeclarationToBeAdded As VBS.LocalDeclarationStatementSyntax =
                    Factory.LocalDeclarationStatement(
                                        DimModifier,
                                        SeparatedListOfvariableDeclarations
                                        ).WithAdditionalAnnotations(Simplifier.Annotation).WithLeadingTrivia(LeadingTrivia)

                StatementWithIssue.AddMarker(DeclarationToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                Return value
            End Function

            Public Overrides Function VisitGlobalStatement(node As CSS.GlobalStatementSyntax) As VisualBasicSyntaxNode
                Dim MethodBodyVisitor As New MethodBodyVisitor(_mSemanticModel, Me)
                _membersList = _membersList.AddRange(node.Statement.Accept(MethodBodyVisitor))
                Return Nothing
            End Function

            Public Overrides Function VisitImplicitElementAccess(node As CSS.ImplicitElementAccessSyntax) As VisualBasicSyntaxNode
                If node.ArgumentList.Arguments.Count > 1 Then
                    Throw New NotSupportedException("ImplicitElementAccess can only have one argument!")
                End If
                Return node.ArgumentList.Arguments(0).Expression.Accept(Me).WithConvertedTriviaFrom(node.ArgumentList.Arguments(0).Expression)
            End Function

            Public Overrides Function VisitMakeRefExpression(node As CSS.MakeRefExpressionSyntax) As VisualBasicSyntaxNode
                GetStatementwithIssues(node).AddMarker(FlagUnsupportedStatements(GetStatementwithIssues(node),
                                                                                 "MakeRef Expressions",
                                                                                 CommentOutOriginalStatements:=False),
                                                       StatementHandlingOption.PrependStatement,
                                                       AllowDuplicates:=True)
                Return Factory.InvocationExpression(Factory.ParseExpression($"__makeref({node.Expression.Accept(Me).ToFullString})").WithConvertedTriviaFrom(node))
            End Function

            Public Overrides Function VisitOmittedArraySizeExpression(node As CSS.OmittedArraySizeExpressionSyntax) As VisualBasicSyntaxNode
                Return Factory.OmittedArgument()
            End Function

            Public Overrides Function VisitRefExpression(node As CSS.RefExpressionSyntax) As VisualBasicSyntaxNode
                Dim StatementwithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                StatementwithIssue.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=True, "ref expression")
                Return node.Expression.Accept(Me)
            End Function

            Public Overrides Function VisitRefType(node As CSS.RefTypeSyntax) As VisualBasicSyntaxNode
                Dim StatementwithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                StatementwithIssue.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=True, "ref type")
                Return node.Type.Accept(Me)
            End Function

            Public Overrides Function VisitRefTypeExpression(node As CSS.RefTypeExpressionSyntax) As VisualBasicSyntaxNode
                Dim StatementwithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                StatementwithIssue.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=True, "ref type expression")
                Return node.Expression.Accept(Me)
            End Function

            Public Overrides Function VisitSingleVariableDesignation(Node As CSS.SingleVariableDesignationSyntax) As VisualBasicSyntaxNode
                Dim identifier As SyntaxToken = GenerateSafeVBToken(Node.Identifier, Node, _mSemanticModel)
                Dim identifierExpression As VBS.IdentifierNameSyntax = Factory.IdentifierName(identifier)

                If Node.Parent.IsKind(CS.SyntaxKind.DeclarationExpression) Then
                    ' var quantity
                    Dim Designation As CSS.DeclarationExpressionSyntax = CType(Node.Parent, CSS.DeclarationExpressionSyntax)
                    ' out var quantity
                    Dim Declaration As CSS.ArgumentSyntax = CType(Designation.Parent, CSS.ArgumentSyntax)
                    ' (item.tostring, out var quantity)
                    Dim TypeName As VBS.TypeSyntax
                    If Designation.Type.IsVar Then
                        TypeName = Factory.PredefinedType(ObjectKeyword)
                        Dim ArgumentList As CSS.ArgumentListSyntax = TryCast(Declaration.Parent, CSS.ArgumentListSyntax)
                        If ArgumentList IsNot Nothing AndAlso
                           ArgumentList.Arguments.Count = 2 AndAlso
                           ArgumentList.Parent.IsKind(CS.SyntaxKind.InvocationExpression) AndAlso
                           ArgumentList.Arguments(index:=1).Equals(Declaration) Then
                            Dim Invocation As CSS.InvocationExpressionSyntax = TryCast(ArgumentList.Parent, CSS.InvocationExpressionSyntax)
                            If Invocation IsNot Nothing Then
                                Dim Expression As CSS.MemberAccessExpressionSyntax = TryCast(Invocation.Expression, CSS.MemberAccessExpressionSyntax)
                                If Expression IsNot Nothing AndAlso Expression.Name.Identifier.ValueText = "TryGetValue" Then
                                    Dim expression1 As CSS.ExpressionSyntax
                                    If TypeOf Invocation.Expression Is CSS.MemberAccessExpressionSyntax Then
                                        expression1 = CType(Invocation.Expression, CSS.MemberAccessExpressionSyntax).Expression
                                        Dim _Typeinfo As TypeInfo = _mSemanticModel.GetTypeInfo(expression1)
                                        If _Typeinfo.Type IsNot Nothing AndAlso Not _Typeinfo.Type.IsErrorType Then
                                            TypeName = _Typeinfo.Type.ConvertToType()
                                            If TypeOf TypeName Is VBS.GenericNameSyntax Then
                                                Dim _arguments As SeparatedSyntaxList(Of VBS.TypeSyntax) = CType(TypeName, VBS.GenericNameSyntax).TypeArgumentList.Arguments
                                                If _arguments.Count = 2 Then
                                                    TypeName = _arguments(index:=1)
                                                End If
                                            End If
                                        End If
                                    End If
                                End If
                            End If
                        End If
                    Else
                        TypeName = CType(Designation.Type.Accept(Me), VBS.TypeSyntax)
                    End If

                    Dim declarationToBeAdded As VBS.LocalDeclarationStatementSyntax =
                                FactoryDimStatement(identifier,
                                                    Factory.SimpleAsClause(TypeName),
                                                    Factory.EqualsValue(NothingExpression))

                    GetStatementwithIssues(Node).AddMarker(declarationToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                ElseIf Node.Parent.IsKind(CS.SyntaxKind.DeclarationPattern) Then
                    Dim DeclarationPattern As CSS.DeclarationPatternSyntax = DirectCast(Node.Parent, CSS.DeclarationPatternSyntax)
                    Dim CasePatternSwitchLabel As CSS.SwitchLabelSyntax = DirectCast(DeclarationPattern.Parent, CSS.SwitchLabelSyntax)
                    Dim SwitchSection As CSS.SwitchSectionSyntax = DirectCast(CasePatternSwitchLabel.Parent, CSS.SwitchSectionSyntax)
                    Dim SwitchStatement As CSS.SwitchStatementSyntax = DirectCast(SwitchSection.Parent, CSS.SwitchStatementSyntax)
                    Dim SwitchExpression As VBS.ExpressionSyntax = DirectCast(SwitchStatement.Expression.Accept(Me), VBS.ExpressionSyntax)

                    Dim TypeName As VBS.TypeSyntax = DirectCast(DeclarationPattern.Type.Accept(Me), VBS.TypeSyntax)
                    Return Factory.TypeOfIsExpression(SwitchExpression, TypeName)
                End If

                Return identifierExpression
            End Function

            Public Overrides Function VisitStackAllocArrayCreationExpression(node As CSS.StackAllocArrayCreationExpressionSyntax) As VisualBasicSyntaxNode
                node.FirstAncestorOrSelf(Of CSS.StatementSyntax).AddMarker(FlagUnsupportedStatements(node.FirstAncestorOrSelf(Of CSS.StatementSyntax), "StackAlloc", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=True)
                Return PredefinedTypeObject
            End Function

            Public Overrides Function VisitSwitchExpression(node As CSS.SwitchExpressionSyntax) As VisualBasicSyntaxNode
                Dim StatementWithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                Dim governingExpression As VBS.ExpressionSyntax = CType(node.GoverningExpression.Accept(Me), VBS.ExpressionSyntax)
                Dim SelectCaseStatement As VBS.SelectStatementSyntax = Factory.SelectStatement(governingExpression.WithLeadingTrivia(VBSpaceTrivia)).WithLeadingTrivia(governingExpression.GetLeadingTrivia)
                Dim ResultNameToken As SyntaxToken = Factory.Identifier(node.GetUniqueVariableNameInScope("tempVar", _mSemanticModel))
                Dim ResultIdentifier As VBS.IdentifierNameSyntax = Factory.IdentifierName(ResultNameToken)
                Dim _Typeinfo As TypeInfo = _mSemanticModel.GetTypeInfo(node.Arms(0).Expression)
                Dim AsClause As VBS.AsClauseSyntax = Nothing
                If _Typeinfo.Type IsNot Nothing AndAlso Not _Typeinfo.Type.IsErrorType Then
                    If TypeOf _Typeinfo.Type Is INamedTypeSymbol AndAlso _Typeinfo.Type.IsTupleType Then
                        AsClause = Factory.SimpleAsClause(_Typeinfo.Type.ConvertCSTupleToVBType.WithLeadingTrivia(VBSpaceTrivia))
                    Else
                        AsClause = Factory.SimpleAsClause(_Typeinfo.Type.ConvertToType().WithLeadingTrivia(VBSpaceTrivia))
                    End If
                End If

                Dim Blocks As New SyntaxList(Of VBS.CaseBlockSyntax)
                For Each e As IndexClass(Of CSS.SwitchExpressionArmSyntax) In node.Arms.WithIndex
                    Dim arm As CSS.SwitchExpressionArmSyntax = e.Value
                    Dim equalsTokenWithTrivia As SyntaxToken = EqualsToken
                    If TypeOf arm.Pattern Is CSS.ConstantPatternSyntax Then
                        Dim ConstatntPattern As CSS.ConstantPatternSyntax = CType(arm.Pattern, CSS.ConstantPatternSyntax)
                        Dim ConstatntPatternExpression As VBS.ExpressionSyntax = CType(ConstatntPattern.Expression.Accept(Me), VBS.ExpressionSyntax)
                        Dim RelationalCaseClause As VBS.RelationalCaseClauseSyntax = Factory.CaseEqualsClause(ConstatntPatternExpression.WithLeadingTrivia(VBSpaceTrivia))
                        Dim CaseStatement As VBS.CaseStatementSyntax = Factory.CaseStatement(RelationalCaseClause).WithPrependedLeadingTrivia(ConstatntPatternExpression.GetLeadingTrivia)
                        Dim VBNode As VisualBasicSyntaxNode = arm.Expression.Accept(Me)
                        Dim Statements As SyntaxList(Of VBS.StatementSyntax)
                        If VBNode.IsKind(SyntaxKind.ThrowStatement) Then
                            Statements = Factory.SingletonList(DirectCast(VBNode, VBS.StatementSyntax))
                        Else
                            Dim Expression As VBS.ExpressionSyntax = CType(VBNode, VBS.ExpressionSyntax)
                            Dim savedLeadingTrivia As SyntaxTriviaList = Expression.GetLeadingTrivia
                            Expression = Expression.WithLeadingTrivia(VBSpaceTrivia)
                            Statements = Factory.SingletonList(Of VBS.StatementSyntax)(Factory.SimpleAssignmentStatement(ResultIdentifier,
                                                                                                                         Expression.WithTrailingEOL(RemoveLastLineContinuation:=True)
                                                                                                                        ).WithLeadingTrivia(savedLeadingTrivia))
                        End If
                        Blocks = Blocks.Add(Factory.CaseBlock(CaseStatement.WithTrailingEOL(RemoveLastLineContinuation:=True), Statements))
                    ElseIf TypeOf arm.Pattern Is CSS.DiscardPatternSyntax Then
                        Dim ExpressionOrThrow As VisualBasicSyntaxNode = arm.Expression.Accept(Me)
                        Dim Statements As SyntaxList(Of VBS.StatementSyntax)
                        If TypeOf ExpressionOrThrow Is VBS.ExpressionSyntax Then
                            Statements = Factory.SingletonList(Of VBS.StatementSyntax)(Factory.SimpleAssignmentStatement(ResultIdentifier, CType(ExpressionOrThrow, VBS.ExpressionSyntax).AdjustExpressionTrivia(AdjustLeading:=True).WithTrailingEOL(RemoveLastLineContinuation:=True)))
                        Else
                            Statements = Factory.SingletonList(Of VBS.StatementSyntax)(CType(ExpressionOrThrow, VBS.ThrowStatementSyntax).WithTrailingEOL(RemoveLastLineContinuation:=True))
                        End If
                        If arm.WhenClause IsNot Nothing Then
                            Dim WhenClause As VBS.ExpressionSyntax = CType(e.Value.WhenClause.Accept(Me), VBS.ExpressionSyntax)
                            Dim CaseClause As SeparatedSyntaxList(Of VBS.CaseClauseSyntax) = Factory.SingletonSeparatedList(Of VBS.CaseClauseSyntax)(Factory.SimpleCaseClause(WhenClause))
                            Blocks = Blocks.Add(Factory.CaseBlock(Factory.CaseStatement(CaseClause).WithTrailingEOL(RemoveLastLineContinuation:=True), Statements))
                        Else
                            Blocks = Blocks.Add(Factory.CaseElseBlock(Factory.CaseElseStatement(Factory.ElseCaseClause).WithConvertedLeadingTriviaFrom(arm).WithTrailingEOL(RemoveLastLineContinuation:=True), Statements))
                        End If
                    ElseIf TypeOf arm.Pattern Is CSS.RecursivePatternSyntax Then
                        StatementWithIssue.AddMarker(FlagUnsupportedStatements(node, "Switch Expression with Recursive Pattern Syntax", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=True)
                        Return ResultIdentifier
                    ElseIf TypeOf arm.Pattern Is CSS.DeclarationPatternSyntax Then
                        Dim Pattern As CSS.DeclarationPatternSyntax = DirectCast(arm.Pattern, CSS.DeclarationPatternSyntax)
                        Dim VariableType As VBS.TypeSyntax = DirectCast(Pattern.Type.Accept(Me), VBS.TypeSyntax).WithLeadingTrivia(VBSpaceTrivia)
                        Dim _tryCast As VBS.TryCastExpressionSyntax = Factory.TryCastExpression(governingExpression, VariableType.WithLeadingTrivia(VBSpaceTrivia))
                        If Pattern.Designation.IsKind(CS.SyntaxKind.SingleVariableDesignation) Then
                            Dim designation As CSS.SingleVariableDesignationSyntax = CType(Pattern.Designation, CSS.SingleVariableDesignationSyntax)
                            Dim identifierToken As SyntaxToken = GenerateSafeVBToken(designation.Identifier, node, _mSemanticModel)
                            Dim statements As New SyntaxList(Of VBS.StatementSyntax)
                            Dim initializer As VBS.EqualsValueSyntax = Factory.EqualsValue(Factory.DirectCastExpression(governingExpression, VariableType))
                            statements = statements.Add(FactoryDimStatement(identifierToken,
                                                                            Factory.SimpleAsClause(VariableType),
                                                                            initializer).WithConvertedLeadingTriviaFrom(arm).WithTrailingEOL(RemoveLastLineContinuation:=True)
                                                        )
                            Dim CaseClause As SeparatedSyntaxList(Of VBS.CaseClauseSyntax) =
                                    Factory.SingletonSeparatedList(Of VBS.CaseClauseSyntax)(Factory.SimpleCaseClause(Factory.EqualsExpression(_tryCast, Factory.TrueLiteralExpression(TrueKeyword))))
                            Dim armExpression As VBS.ExpressionSyntax = CType(arm.Expression.Accept(Me), VBS.ExpressionSyntax)
                            Dim right As VBS.ExpressionSyntax = armExpression.AdjustExpressionTrivia(AdjustLeading:=True)
                            If right.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                equalsTokenWithTrivia = equalsTokenWithTrivia.WithTrailingTrivia(VBSpaceTrivia, LineContinuation, VBEOLTrivia)
                            End If
                            statements = statements.Add(Factory.AssignmentStatement(SyntaxKind.SimpleAssignmentStatement, ResultIdentifier, equalsTokenWithTrivia, right).WithTrailingEOL(RemoveLastLineContinuation:=True))
                            Blocks = Blocks.Add(Factory.CaseBlock(Factory.CaseStatement(CaseClause).WithTrailingEOL(RemoveLastLineContinuation:=True), statements))
                        ElseIf Pattern.Designation.IsKind(CS.SyntaxKind.DiscardDesignation) Then
                            Dim statements As New SyntaxList(Of VBS.StatementSyntax)
                            Dim armExpression As VBS.ExpressionSyntax = CType(arm.Expression.Accept(Me), VBS.ExpressionSyntax)
                            Dim right As VBS.ExpressionSyntax = armExpression.AdjustExpressionTrivia(AdjustLeading:=True)
                            If right.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                equalsTokenWithTrivia = equalsTokenWithTrivia.WithTrailingTrivia(VBSpaceTrivia, LineContinuation, VBEOLTrivia)
                            End If
                            Dim CaseClause As SeparatedSyntaxList(Of VBS.CaseClauseSyntax) =
                                    Factory.SingletonSeparatedList(Of VBS.CaseClauseSyntax)(Factory.SimpleCaseClause(Factory.EqualsExpression(_tryCast, Factory.TrueLiteralExpression(TrueKeyword))))
                            statements = statements.Add(Factory.AssignmentStatement(SyntaxKind.SimpleAssignmentStatement, ResultIdentifier, equalsTokenWithTrivia, right).WithTrailingEOL(RemoveLastLineContinuation:=True))
                            Blocks = Blocks.Add(Factory.CaseBlock(Factory.CaseStatement(CaseClause).WithTrailingEOL(RemoveLastLineContinuation:=True), statements))
                        Else
                            Stop
                        End If
                    ElseIf TypeOf arm.Pattern Is CSS.VarPatternSyntax Then
                        Dim VarPattern As CSS.VarPatternSyntax = DirectCast(arm.Pattern, CSS.VarPatternSyntax)
                        Dim Statements As SyntaxList(Of VBS.StatementSyntax)
                        Dim Identifier As SyntaxToken
                        If TypeOf VarPattern.Designation Is CSS.SingleVariableDesignationSyntax Then
                            Identifier = GenerateSafeVBToken(id:=DirectCast(VarPattern.Designation, CSS.SingleVariableDesignationSyntax).Identifier, node, _mSemanticModel)
                        ElseIf TypeOf VarPattern.Designation Is CSS.ParenthesizedVariableDesignationSyntax Then
                            Dim sBuilder As New StringBuilder
                            CreateDesignationName(ProcessVariableDesignation(CType(VarPattern.Designation, CSS.ParenthesizedVariableDesignationSyntax)), sBuilder)
                            Identifier = GenerateSafeVBToken(id:=CS.SyntaxFactory.Identifier(sBuilder.ToString), node, _mSemanticModel)
                        Else
                            Stop
                            _reportException?.Invoke(UnreachableException)
                        End If
                        Dim Name As VBS.IdentifierNameSyntax = Factory.IdentifierName(Identifier.ToString)

                        Dim VariableName As VBS.ModifiedIdentifierSyntax = Factory.ModifiedIdentifier(Identifier.WithTrailingTrivia(VBSpaceTrivia))
                        Dim ExpressionOrThrow As VisualBasicSyntaxNode = arm.Expression.Accept(Me)

                        If TypeOf ExpressionOrThrow Is VBS.ExpressionSyntax Then
                            Statements = Factory.SingletonList(Of VBS.StatementSyntax)(Factory.SimpleAssignmentStatement(ResultIdentifier, CType(ExpressionOrThrow, VBS.ExpressionSyntax).AdjustExpressionTrivia(AdjustLeading:=True).WithTrailingEOL(RemoveLastLineContinuation:=True)))
                        Else
                            Statements = New SyntaxList(Of VBS.StatementSyntax)
                            Statements = Statements.Add(FactoryDimStatement(Identifier,
                                                                           asClause:=Nothing,
                                                                           initializer:=Nothing
                                                                           ).WithTrailingEOL(RemoveLastLineContinuation:=True))
                            Statements = Statements.Add(CType(ExpressionOrThrow.WithTrailingEOL(RemoveLastLineContinuation:=True), VBS.StatementSyntax))
                        End If

                        If Not e.IsLast Then
                            Dim WhenClause As VBS.ExpressionSyntax = CType(e.Value.WhenClause.Accept(Me), VBS.ExpressionSyntax)
                            Dim CaseClause As SeparatedSyntaxList(Of VBS.CaseClauseSyntax) = Factory.SingletonSeparatedList(Of VBS.CaseClauseSyntax)(Factory.SimpleCaseClause(WhenClause))
                            Blocks = Blocks.Add(Factory.CaseBlock(Factory.CaseStatement(CaseClause).WithTrailingEOL(RemoveLastLineContinuation:=True), Statements))
                        Else
                            Blocks = Blocks.Add(Factory.CaseElseBlock(Factory.CaseElseStatement(Factory.ElseCaseClause).WithConvertedLeadingTriviaFrom(arm).WithTrailingEOL(RemoveLastLineContinuation:=True), Statements))
                        End If
                    Else
                        Stop
                    End If
                Next
                Dim EndSelectStatement As VBS.EndBlockStatementSyntax = Factory.EndBlockStatement(
                                                                    SyntaxKind.EndSelectStatement,
                                                                    EndKeyword,
                                                                    SelectKeyword).
                                                                        WithConvertedTriviaFrom(node.CloseBraceToken)
                Dim stmt As VBS.SelectBlockSyntax = Factory.SelectBlock(SelectCaseStatement,
                                                                        Blocks,
                                                                        EndSelectStatement
                                                                        )
                Dim localDeclarationStatement As VBS.LocalDeclarationStatementSyntax = FactoryDimStatement(ResultNameToken, AsClause, initializer:=Nothing)
                StatementWithIssue.AddMarker(localDeclarationStatement.WithTrailingEOL(RemoveLastLineContinuation:=True), StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                StatementWithIssue.AddMarker(stmt, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                Return ResultIdentifier
            End Function

            Public Overrides Function VisitVariableDeclaration(node As CSS.VariableDeclarationSyntax) As VisualBasicSyntaxNode
                Dim StatementWithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                Dim LeadingTrivia As SyntaxTriviaList = StatementWithIssue.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=True, "VB has no direct equivalent To C# var pattern expressions")
                Return MyBase.VisitVariableDeclaration(node)
            End Function

            Public Overrides Function VisitVariableDeclarator(node As CSS.VariableDeclaratorSyntax) As VisualBasicSyntaxNode
                Dim Identifier As SyntaxToken = GenerateSafeVBToken(node.Identifier, node, _mSemanticModel)
                Dim ArgumentList As New List(Of VBS.ArgumentSyntax)
                If node.ArgumentList Is Nothing Then
                    Return Factory.ModifiedIdentifier(Identifier).WithTrailingTrivia(VBSpaceTrivia)
                Else
                    For Each e As IndexClass(Of CSS.ArgumentSyntax) In node.ArgumentList.Arguments.WithIndex
                        Dim vbExpression As VBS.ExpressionSyntax = CType(e.Value.Expression.Accept(Me), VBS.ExpressionSyntax)
                        If TypeOf vbExpression Is VBS.LiteralExpressionSyntax Then
                            Dim LiteralExpression As VBS.LiteralExpressionSyntax = CType(vbExpression, VBS.LiteralExpressionSyntax)
                            ArgumentList.Add(Factory.SimpleArgument(Factory.LiteralExpression(SyntaxKind.NumericLiteralExpression, Factory.Literal(CInt(LiteralExpression.Token.Value) - 1))).WithConvertedLeadingTriviaFrom(e.Value.Expression))
                        ElseIf TypeOf vbExpression Is VBS.IdentifierNameSyntax Then
                            Dim Id As VBS.IdentifierNameSyntax = CType(vbExpression, VBS.IdentifierNameSyntax)
                            ArgumentList.Add(Factory.SimpleArgument(Factory.BinaryExpression(SyntaxKind.SubtractExpression, Id, MinusToken, ExpressionD1)).WithConvertedLeadingTriviaFrom(e.Value.Expression))
                        ElseIf TypeOf vbExpression Is VBS.BinaryExpressionSyntax Then
                            Dim Value As VBS.BinaryExpressionSyntax = CType(vbExpression, VBS.BinaryExpressionSyntax)
                            If vbExpression.IsKind(SyntaxKind.AddExpression) Then
                                If Value.Right.IsKind(SyntaxKind.NumericLiteralExpression) AndAlso CType(Value.Right, VBS.LiteralExpressionSyntax).Token.ValueText = "1" Then
                                    ArgumentList.Add(Factory.SimpleArgument(Value.Left.WithConvertedLeadingTriviaFrom(e.Value.Expression)))
                                    Continue For
                                End If
                            End If
                            ArgumentList.Add(Factory.SimpleArgument(Factory.BinaryExpression(SyntaxKind.SubtractExpression, Value, MinusToken, ExpressionD1)).WithConvertedLeadingTriviaFrom(e.Value.Expression))
                        Else
                            Stop
                        End If
                    Next
                End If
                Dim Nullable As SyntaxToken = Nothing
                Dim ArrayBounds As VBS.ArgumentListSyntax = Factory.ArgumentList(Factory.SeparatedList(ArgumentList))
                Return Factory.ModifiedIdentifier(Identifier.WithTrailingTrivia(VBSpaceTrivia), Nullable, ArrayBounds, arrayRankSpecifiers:=Nothing)
            End Function

            Public Overrides Function VisitWhenClause(node As CSS.WhenClauseSyntax) As VisualBasicSyntaxNode
                Return node.Condition.Accept(Me)
            End Function

        End Class

    End Class

End Namespace
