' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Diagnostics.CodeAnalysis
Imports System.Text

Imports CSharpToVBCodeConverter.Util

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CSharp.Syntax
Imports Microsoft.CodeAnalysis.Simplification

Imports CS = Microsoft.CodeAnalysis.CSharp

Imports VB = Microsoft.CodeAnalysis.VisualBasic

Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBCodeConverter.DestVisualBasic

    Partial Public Class CSharpConverter

        Partial Friend Class NodesVisitor
            Inherits CS.CSharpSyntaxVisitor(Of VB.VisualBasicSyntaxNode)

            ' This file contains all the stuff accessed by multiple Visitor functions in Class NodeVisitor and Visitors that
            ' had no better home.

            Private ReadOnly _isModuleStack As New Stack(Of Boolean)
            Private ReadOnly _mSemanticModel As SemanticModel
            Private _placeholder As Integer = 1
            Public ReadOnly _allImports As List(Of VBS.ImportsStatementSyntax) = New List(Of VBS.ImportsStatementSyntax)()
            Public ReadOnly _inlineAssignHelperMarkers As List(Of BaseTypeDeclarationSyntax) = New List(Of BaseTypeDeclarationSyntax)()
            Private ReadOnly _reportException As Action(Of Exception)

            Public Sub New(lSemanticModel As SemanticModel, ReportException As Action(Of Exception))
                _mSemanticModel = lSemanticModel
                _reportException = ReportException
            End Sub

            Public ReadOnly Property IsModule As Boolean
                Get
                    If _isModuleStack.Count = 0 Then
                        Return False
                    End If
                    Return _isModuleStack.Peek
                End Get
            End Property

            <ExcludeFromCodeCoverage>
            Public Overrides Function DefaultVisit(node As SyntaxNode) As VB.VisualBasicSyntaxNode
                Throw New NotImplementedException(node.[GetType]().ToString & " not implemented!")
            End Function

            Public Overrides Function VisitCompilationUnit(node As CompilationUnitSyntax) As VB.VisualBasicSyntaxNode
                For Each [using] As UsingDirectiveSyntax In node.Usings
                    If s_originalRequest.CancelToken.IsCancellationRequested Then
                        Throw New OperationCanceledException
                    End If
                    [using].Accept(Me)
                Next
                Dim externList As New List(Of VB.VisualBasicSyntaxNode)
                ' externlist is potentially a list of empty lines with trivia
                For Each extern As ExternAliasDirectiveSyntax In node.Externs
                    externList.Add(extern.Accept(Me))
                Next

                Dim Options As SyntaxList(Of VBS.OptionStatementSyntax) = VBFactory.List(Of VBS.OptionStatementSyntax)
                Options = Options.Add(VBFactory.OptionStatement(ExplicitToken, OffToken))
                Options = Options.Add(VBFactory.OptionStatement(InferToken, OnToken))
                Options = Options.Add(VBFactory.OptionStatement(StrictToken, OffToken).WithTrailingEOL)

                Dim ListOfAttributes As SyntaxList(Of VBS.AttributesStatementSyntax) = VBFactory.List(node.AttributeLists.Select(Function(a As AttributeListSyntax) VBFactory.AttributesStatement(VBFactory.SingletonList(DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))))
                Dim MemberList As New List(Of VBS.StatementSyntax)
                For Each m As MemberDeclarationSyntax In node.Members
                    If s_originalRequest.CancelToken.IsCancellationRequested Then
                        Throw New OperationCanceledException
                    End If
                    MemberList.Add(DirectCast(m.Accept(Me), VBS.StatementSyntax))
                Next
                Dim Members As SyntaxList(Of VBS.StatementSyntax) = VBFactory.List(MemberList)
                Dim compilationUnitSyntax1 As VBS.CompilationUnitSyntax
                Dim EndOfFIleTokenWithTrivia As SyntaxToken = EndOfFileToken.WithConvertedTriviaFrom(node.EndOfFileToken)

                If externList.Count > 0 Then
                    compilationUnitSyntax1 = VBFactory.CompilationUnit(
                        VBFactory.List(Of VBS.OptionStatementSyntax)(),
                        VBFactory.List(_allImports),
                        ListOfAttributes,
                        Members).WithTriviaFrom(externList(0))
                ElseIf _allImports.Count > 0 Then
                    If Members.Count > 0 AndAlso Members(0).HasLeadingTrivia Then
                        If (TypeOf Members(0) IsNot VBS.NamespaceBlockSyntax AndAlso TypeOf Members(0) IsNot VBS.ModuleBlockSyntax) OrElse
                            Members(0).GetLeadingTrivia.ToFullString.Contains("auto-generated", StringComparison.OrdinalIgnoreCase) Then
                            Dim HeadingTriviaList As New List(Of SyntaxTrivia)
                            HeadingTriviaList.AddRange(Members(0).GetLeadingTrivia)
                            If HeadingTriviaList(0).IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                HeadingTriviaList.RemoveAt(0)
                                If HeadingTriviaList.Count > 0 Then
                                    HeadingTriviaList.Add(VBEOLTrivia)
                                End If
                            End If
                            Dim NewMemberList As New SyntaxList(Of VBS.StatementSyntax)
                            NewMemberList = NewMemberList.Add(Members(0).WithLeadingTrivia(SpaceTrivia))
                            Members = NewMemberList.AddRange(Members.RemoveAt(0))
                            Dim NewLeadingTrivia As New List(Of SyntaxTrivia)
                            ' Remove Leading whitespace
                            For Each t As SyntaxTrivia In HeadingTriviaList
                                If Not t.IsWhitespaceOrEndOfLine Then
                                    NewLeadingTrivia.Add(t)
                                End If
                            Next
                            _allImports(0) = _allImports(0).WithPrependedLeadingTrivia(NewLeadingTrivia)
                        End If
                    End If
                    compilationUnitSyntax1 = VBFactory.CompilationUnit(
                                                                Options,
                                                                VBFactory.List(_allImports),
                                                                ListOfAttributes,
                                                                Members,
                                                                EndOfFIleTokenWithTrivia
                                                                )
                Else
                    compilationUnitSyntax1 = VBFactory.CompilationUnit(
                                                                Options,
                                                                VBFactory.List(_allImports),
                                                                ListOfAttributes,
                                                                Members,
                                                                EndOfFIleTokenWithTrivia)
                End If
                If HasMarkerError() Then
                    ' There are statements that were left out of translation
                    Throw New ApplicationException(GetMarkerErrorMessage)
                End If
                Return compilationUnitSyntax1
            End Function

            Public Overrides Function VisitDeclarationPattern(node As DeclarationPatternSyntax) As VB.VisualBasicSyntaxNode
                Dim StatementWithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                Dim LeadingTrivia As SyntaxTriviaList = StatementWithIssue.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=True, "VB has no direct equivalent To C# pattern variables 'is' expressions")
                Dim Designation As SingleVariableDesignationSyntax = DirectCast(node.Designation, SingleVariableDesignationSyntax)

                Dim value As VBS.ExpressionSyntax = VBFactory.ParseExpression($"TryCast({node.Designation.Accept(Me).NormalizeWhitespace.ToFullString}, {node.Type.Accept(Me).NormalizeWhitespace.ToFullString})")

                Dim SeparatedSyntaxListOfModifiedIdentifier As SeparatedSyntaxList(Of VBS.ModifiedIdentifierSyntax) =
                        VBFactory.SingletonSeparatedList(
                            VBFactory.ModifiedIdentifier(GenerateSafeVBToken(Designation.Identifier, IsQualifiedName:=False, IsTypeName:=False)))
                Dim VariableType As VBS.TypeSyntax = DirectCast(node.Type.Accept(Me), VBS.TypeSyntax)

                Dim SeparatedListOfvariableDeclarations As SeparatedSyntaxList(Of VBS.VariableDeclaratorSyntax) =
                        VBFactory.SingletonSeparatedList(
                            VBFactory.VariableDeclarator(
                                SeparatedSyntaxListOfModifiedIdentifier,
                                VBFactory.SimpleAsClause(VariableType),
                                VBFactory.EqualsValue(NothingExpression)
                                    )
                             )

                Dim DeclarationToBeAdded As VBS.LocalDeclarationStatementSyntax =
                    VBFactory.LocalDeclarationStatement(
                                        DimModifier,
                                        SeparatedListOfvariableDeclarations
                                        ).WithAdditionalAnnotations(Simplifier.Annotation).WithLeadingTrivia(LeadingTrivia)

                StatementWithIssue.AddMarker(DeclarationToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                Return value
            End Function

            Public Overrides Function VisitImplicitElementAccess(node As ImplicitElementAccessSyntax) As VB.VisualBasicSyntaxNode
                If node.ArgumentList.Arguments.Count > 1 Then
                    Throw New NotSupportedException("ImplicitElementAccess can only have one argument!")
                End If
                Return node.ArgumentList.Arguments(0).Expression.Accept(Me).WithConvertedTriviaFrom(node.ArgumentList.Arguments(0).Expression)
            End Function

            Public Overrides Function VisitLocalFunctionStatement(node As LocalFunctionStatementSyntax) As VB.VisualBasicSyntaxNode
                Return MyBase.VisitLocalFunctionStatement(node)
            End Function

            Public Overrides Function VisitMakeRefExpression(node As MakeRefExpressionSyntax) As VB.VisualBasicSyntaxNode

                Dim StatementwithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                StatementwithIssue.AddMarker(FlagUnsupportedStatements(StatementwithIssue, "MakeRef Expressions", CommentOutOriginalStatements:=False), StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                Dim Expression As VBS.ExpressionSyntax = VBFactory.ParseExpression($"__makeref({node.Expression.Accept(Me).ToFullString})").WithConvertedTriviaFrom(node)
                Return VBFactory.InvocationExpression(Expression)
            End Function

            Public Overrides Function VisitOmittedArraySizeExpression(node As OmittedArraySizeExpressionSyntax) As VB.VisualBasicSyntaxNode
                Return VBFactory.OmittedArgument()
            End Function

            Public Overrides Function VisitRefExpression(node As RefExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim StatementwithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                StatementwithIssue.AddMarker(FlagUnsupportedStatements(StatementwithIssue, "ref expression", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=False)
                Return NothingExpression
            End Function

            Public Overrides Function VisitRefType(node As RefTypeSyntax) As VB.VisualBasicSyntaxNode
                Dim StatementwithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                StatementwithIssue.AddMarker(FlagUnsupportedStatements(StatementwithIssue, "ref type", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=False)
                Return HandleRefType
            End Function

            Public Overrides Function VisitRefTypeExpression(node As RefTypeExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim StatementwithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                StatementwithIssue.AddMarker(FlagUnsupportedStatements(StatementwithIssue, "ref type expression", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=False)
                Return Nothing
            End Function

            ''' <summary>
            '''
            ''' </summary>
            ''' <param name="Node"></param>
            ''' <returns></returns>
            ''' <remarks>Added by PC</remarks>
            Public Overrides Function VisitSingleVariableDesignation(Node As SingleVariableDesignationSyntax) As VB.VisualBasicSyntaxNode
                Dim Identifier As SyntaxToken = GenerateSafeVBToken(Node.Identifier, IsQualifiedName:=False, IsTypeName:=False)
                Dim IdentifierExpression As VBS.IdentifierNameSyntax = VBFactory.IdentifierName(Identifier)
                Dim ModifiedIdentifier As VBS.ModifiedIdentifierSyntax = VBFactory.ModifiedIdentifier(Identifier).WithTrailingTrivia(SpaceTrivia)
                Dim SeparatedSyntaxListOfModifiedIdentifier As SeparatedSyntaxList(Of VBS.ModifiedIdentifierSyntax) =
                    VBFactory.SingletonSeparatedList(
                        ModifiedIdentifier
                        )

                If Node.Parent.IsKind(CS.SyntaxKind.DeclarationExpression) Then
                    Dim Parent As DeclarationExpressionSyntax = DirectCast(Node.Parent, DeclarationExpressionSyntax)
                    Dim TypeName As VBS.TypeSyntax = CType(Parent.Type.Accept(Me), VBS.TypeSyntax)

                    Dim SeparatedListOfvariableDeclarations As SeparatedSyntaxList(Of VBS.VariableDeclaratorSyntax) =
                        VBFactory.SingletonSeparatedList(
                            VBFactory.VariableDeclarator(
                                SeparatedSyntaxListOfModifiedIdentifier,
                                VBFactory.SimpleAsClause(TypeName),
                                VBFactory.EqualsValue(NothingExpression)
                                    )
                             )

                    Dim DeclarationToBeAdded As VBS.LocalDeclarationStatementSyntax =
                        VBFactory.LocalDeclarationStatement(
                            DimModifier,
                            SeparatedListOfvariableDeclarations).WithAdditionalAnnotations(Simplifier.Annotation)

                    GetStatementwithIssues(Node).AddMarker(DeclarationToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                ElseIf Node.Parent.IsKind(CS.SyntaxKind.DeclarationPattern) Then
                    Dim DeclarationPattern As DeclarationPatternSyntax = DirectCast(Node.Parent, DeclarationPatternSyntax)
                    Dim CasePatternSwitchLabel As SwitchLabelSyntax = DirectCast(DeclarationPattern.Parent, SwitchLabelSyntax)
                    Dim SwitchSection As SwitchSectionSyntax = DirectCast(CasePatternSwitchLabel.Parent, SwitchSectionSyntax)
                    Dim SwitchStatement As SwitchStatementSyntax = DirectCast(SwitchSection.Parent, SwitchStatementSyntax)
                    Dim SwitchExpression As VBS.ExpressionSyntax = DirectCast(SwitchStatement.Expression.Accept(Me), VBS.ExpressionSyntax)

                    Dim TypeName As VBS.TypeSyntax = DirectCast(DeclarationPattern.Type.Accept(Me), VBS.TypeSyntax)
                    'DeclarationPattern.Type.CheckCorrectnessLeadingTrivia(($"' TODO: VB does not support Declaration Pattern. An attempt was made to convert the original Case Clause was ""case {Node.Parent.ToFullString}:"""))
                    Return VBFactory.TypeOfIsExpression(SwitchExpression, TypeName)
                End If

                Return IdentifierExpression
            End Function

            Public Overrides Function VisitStackAllocArrayCreationExpression(node As StackAllocArrayCreationExpressionSyntax) As VB.VisualBasicSyntaxNode
                node.FirstAncestorOrSelf(Of StatementSyntax).AddMarker(FlagUnsupportedStatements(node.FirstAncestorOrSelf(Of StatementSyntax), "StackAlloc", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=True)
                Return PredefinedTypeObject
            End Function

            Public Overrides Function VisitSwitchExpression(node As SwitchExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim SwitchVariableDeclared As Boolean = False
                Dim StatementWithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                Dim governingExpression As VBS.ExpressionSyntax = CType(node.GoverningExpression.Accept(Me), VBS.ExpressionSyntax)
                Dim SelectCaseStatement As VBS.SelectStatementSyntax = VBFactory.SelectStatement(governingExpression)
                Dim ResultNameToken As SyntaxToken = VBFactory.Identifier(MethodBodyVisitor.GetUniqueVariableNameInScope(node, "tempVar", _mSemanticModel))
                Dim ResultIdentifier As VBS.IdentifierNameSyntax = VBFactory.IdentifierName(ResultNameToken)
                Dim _Typeinfo As TypeInfo = ModelExtensions.GetTypeInfo(_mSemanticModel, node.Arms(0).Expression)
                Dim AsClause As VBS.AsClauseSyntax = Nothing
                If _Typeinfo.Type IsNot Nothing AndAlso Not _Typeinfo.Type.IsErrorType Then
                    If TypeOf _Typeinfo.Type Is INamedTypeSymbol AndAlso _Typeinfo.Type.IsTupleType Then
                        AsClause = VBFactory.SimpleAsClause(ConvertCSTupleToVBType(_Typeinfo.Type.ToString).WithLeadingTrivia(SpaceTrivia))
                    Else
                        AsClause = VBFactory.SimpleAsClause(ConvertToType(_Typeinfo.Type.ToString).WithLeadingTrivia(SpaceTrivia))
                    End If
                End If
                Dim ResultVariable As VBS.VariableDeclaratorSyntax = VBFactory.VariableDeclarator(VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(ResultNameToken)), AsClause, initializer:=Nothing)


                Dim Blocks As New SyntaxList(Of VBS.CaseBlockSyntax)
                For i As Integer = 0 To node.Arms.Count - 1
                    Dim equalsTokenWithTrivia As SyntaxToken = EqualsToken
                    Dim arm As SwitchExpressionArmSyntax = node.Arms(i)
                    If TypeOf arm.Pattern Is ConstantPatternSyntax Then
                        Dim ConstatntPattern As ConstantPatternSyntax = CType(arm.Pattern, ConstantPatternSyntax)
                        Dim ConstatntPatternExpression As VBS.ExpressionSyntax = CType(ConstatntPattern.Expression.Accept(Me), VBS.ExpressionSyntax)
                        Dim RelationalCaseClause As VBS.RelationalCaseClauseSyntax = VBFactory.CaseEqualsClause(ConstatntPatternExpression.WithLeadingTrivia(SpaceTrivia))
                        Dim CaseStatement As VBS.CaseStatementSyntax = VBFactory.CaseStatement(RelationalCaseClause).WithPrependedLeadingTrivia(ConstatntPatternExpression.GetLeadingTrivia)
                        Dim VBNode As VB.VisualBasicSyntaxNode = arm.Expression.Accept(Me)
                        Dim Statements As SyntaxList(Of VBS.StatementSyntax)
                        If VBNode.IsKind(VB.SyntaxKind.ThrowStatement) Then
                            Statements = VBFactory.SingletonList(DirectCast(VBNode, VBS.StatementSyntax))
                        Else
                            Dim Expression As VBS.ExpressionSyntax = CType(VBNode, VBS.ExpressionSyntax)
                            Dim LeadingTrivia As New List(Of SyntaxTrivia)
                            LeadingTrivia.AddRange(Expression.GetLeadingTrivia)
                            Expression = Expression.WithLeadingTrivia(SpaceTrivia)
                            Statements = VBFactory.SingletonList(Of VBS.StatementSyntax)(VBFactory.SimpleAssignmentStatement(ResultIdentifier, Expression.WithTrailingEOL).WithLeadingTrivia(LeadingTrivia))
                        End If
                        Blocks = Blocks.Add(VBFactory.CaseBlock(CaseStatement.WithTrailingEOL, Statements))
                    ElseIf TypeOf arm.Pattern Is DiscardPatternSyntax Then
                        Dim ExpressionOrThrow As VB.VisualBasicSyntaxNode = arm.Expression.Accept(Me)
                        Dim Statements As SyntaxList(Of VBS.StatementSyntax)
                        If TypeOf ExpressionOrThrow Is VBS.ExpressionSyntax Then
                            Statements = VBFactory.SingletonList(Of VBS.StatementSyntax)(VBFactory.SimpleAssignmentStatement(ResultIdentifier, CType(ExpressionOrThrow, VBS.ExpressionSyntax).AdjustExpressionLeadingTrivia.WithTrailingEOL))
                        Else
                            Statements = VBFactory.SingletonList(Of VBS.StatementSyntax)(CType(ExpressionOrThrow, VBS.ThrowStatementSyntax).WithTrailingEOL)
                        End If
                        If arm.WhenClause IsNot Nothing Then
                            Dim WhenClause As VBS.ExpressionSyntax = CType(node.Arms(i).WhenClause.Accept(Me), VBS.ExpressionSyntax)
                            Dim CaseClause As SeparatedSyntaxList(Of VBS.CaseClauseSyntax) = VBFactory.SingletonSeparatedList(Of VBS.CaseClauseSyntax)(VBFactory.SimpleCaseClause(WhenClause))
                            Blocks = Blocks.Add(VBFactory.CaseBlock(VBFactory.CaseStatement(CaseClause).WithTrailingEOL, Statements))
                        Else
                            Blocks = Blocks.Add(VBFactory.CaseElseBlock(VBFactory.CaseElseStatement(VBFactory.ElseCaseClause).WithTrailingEOL, Statements))
                        End If
                    ElseIf TypeOf arm.Pattern Is RecursivePatternSyntax Then
                        StatementWithIssue.AddMarker(FlagUnsupportedStatements(node, "Switch Expression with Recursive Pattern Syntax", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=True)
                        Return ResultIdentifier
                    ElseIf TypeOf arm.Pattern Is DeclarationPatternSyntax Then
                        Dim Pattern As DeclarationPatternSyntax = DirectCast(arm.Pattern, DeclarationPatternSyntax)
                        Dim VariableType As VBS.TypeSyntax = DirectCast(Pattern.Type.Accept(Me), VBS.TypeSyntax).WithLeadingTrivia(SpaceTrivia)
                        Dim _tryCast As VBS.TryCastExpressionSyntax = VBFactory.TryCastExpression(governingExpression, VariableType.WithLeadingTrivia(SpaceTrivia))
                        If Pattern.Designation.IsKind(CS.SyntaxKind.SingleVariableDesignation) Then
                            Dim designation As SingleVariableDesignationSyntax = CType(Pattern.Designation, SingleVariableDesignationSyntax)
                            Dim identifierToken As SyntaxToken = GenerateSafeVBToken(designation.Identifier, IsQualifiedName:=False, IsTypeName:=False)
                            If Not SwitchVariableDeclared Then
                                Dim SeparatedSyntaxListOfModifiedIdentifier As SeparatedSyntaxList(Of VBS.ModifiedIdentifierSyntax) =
                                        VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(identifierToken))

                                Dim SeparatedListOfvariableDeclarations As SeparatedSyntaxList(Of VBS.VariableDeclaratorSyntax) =
                                    VBFactory.SingletonSeparatedList(
                                        VBFactory.VariableDeclarator(
                                                        SeparatedSyntaxListOfModifiedIdentifier,
                                                        asClause:=Nothing,
                                                        initializer:=Nothing
                                                                    )
                                         )
                                Dim DeclarationToBeAdded As VBS.LocalDeclarationStatementSyntax =
                                        VBFactory.LocalDeclarationStatement(
                                                DimModifier,
                                                SeparatedListOfvariableDeclarations
                                                ).WithAdditionalAnnotations(Simplifier.Annotation).WithPrependedLeadingTrivia(VariableType.GetLeadingTrivia).WithTrailingEOL

                                StatementWithIssue.AddMarker(DeclarationToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                                SwitchVariableDeclared = True
                            End If
                            Dim CaseClause As SeparatedSyntaxList(Of VBS.CaseClauseSyntax) =
                                    VBFactory.SingletonSeparatedList(Of VBS.CaseClauseSyntax)(VBFactory.SimpleCaseClause(VBFactory.EqualsExpression(_tryCast, VBFactory.TrueLiteralExpression(TrueKeyword))))
                            Dim statements As New SyntaxList(Of VBS.StatementSyntax)
                            Dim right As VBS.ExpressionSyntax = CType(arm.Expression.Accept(Me), VBS.ExpressionSyntax).AdjustExpressionLeadingTrivia
                            If right.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                equalsTokenWithTrivia = equalsTokenWithTrivia.WithTrailingTrivia(SpaceTrivia, LineContinuation, VBEOLTrivia)
                            End If
                            statements = statements.Add(VBFactory.AssignmentStatement(VB.SyntaxKind.SimpleAssignmentStatement, ResultIdentifier, equalsTokenWithTrivia, right))
                            statements = statements.Add(VBFactory.AssignmentStatement(VB.SyntaxKind.SimpleAssignmentStatement, VBFactory.IdentifierName(identifierToken), equalsTokenWithTrivia, ResultIdentifier))
                            Blocks = Blocks.Add(VBFactory.CaseBlock(VBFactory.CaseStatement(CaseClause).WithTrailingEOL, statements))
                        ElseIf Pattern.Designation.IsKind(CS.SyntaxKind.DiscardDesignation) Then
                            Dim statements As New SyntaxList(Of VBS.StatementSyntax)
                            Dim right As VBS.ExpressionSyntax = CType(arm.Expression.Accept(Me), VBS.ExpressionSyntax).AdjustExpressionLeadingTrivia
                            If right.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                equalsTokenWithTrivia = equalsTokenWithTrivia.WithTrailingTrivia(SpaceTrivia, LineContinuation, VBEOLTrivia)
                            End If
                            Dim CaseClause As SeparatedSyntaxList(Of VBS.CaseClauseSyntax) =
                                    VBFactory.SingletonSeparatedList(Of VBS.CaseClauseSyntax)(VBFactory.SimpleCaseClause(VBFactory.EqualsExpression(_tryCast, VBFactory.TrueLiteralExpression(TrueKeyword))))
                            statements = statements.Add(VBFactory.AssignmentStatement(VB.SyntaxKind.SimpleAssignmentStatement, ResultIdentifier, equalsTokenWithTrivia, right))
                            Blocks = Blocks.Add(VBFactory.CaseBlock(VBFactory.CaseStatement(CaseClause).WithTrailingEOL, statements))
                        Else
                            Stop
                        End If
                    ElseIf TypeOf arm.Pattern Is VarPatternSyntax Then
                        Dim VarPattern As VarPatternSyntax = DirectCast(arm.Pattern, VarPatternSyntax)
                        Dim Statements As SyntaxList(Of VBS.StatementSyntax)
                        Dim Identifier As SyntaxToken
                        If TypeOf VarPattern.Designation Is SingleVariableDesignationSyntax Then
                            Identifier = GenerateSafeVBToken(id:=DirectCast(VarPattern.Designation, SingleVariableDesignationSyntax).Identifier, IsQualifiedName:=False, IsTypeName:=False)
                        ElseIf TypeOf VarPattern.Designation Is ParenthesizedVariableDesignationSyntax Then
                            Dim sBuilder As New StringBuilder
                            CreateDesignationName(ProcessVariableDesignation(CType(VarPattern.Designation, ParenthesizedVariableDesignationSyntax)), sBuilder)
                            Identifier = GenerateSafeVBToken(id:=CS.SyntaxFactory.Identifier(sBuilder.ToString), IsQualifiedName:=False, IsTypeName:=False)
                        Else
                            Stop
                            Throw UnreachableException
                        End If
                        Dim Name As VBS.IdentifierNameSyntax = VBFactory.IdentifierName(Identifier.ToString)

                        Dim VariableName As VBS.ModifiedIdentifierSyntax = VBFactory.ModifiedIdentifier(Identifier.WithTrailingTrivia(SpaceTrivia))
                        Dim ExpressionOrThrow As VB.VisualBasicSyntaxNode = arm.Expression.Accept(Me)

                        If TypeOf ExpressionOrThrow Is VBS.ExpressionSyntax Then
                            Dim Declarators As SeparatedSyntaxList(Of VBS.VariableDeclaratorSyntax) = VBFactory.SingletonSeparatedList(
                        node:=VBFactory.VariableDeclarator(VBFactory.SingletonSeparatedList(
                        VariableName),
                        asClause:=Nothing, initializer:=VBFactory.EqualsValue(CType(ExpressionOrThrow, VBS.ExpressionSyntax).AdjustExpressionLeadingTrivia)).WithTrailingEOL
                         )
                            Statements = VBFactory.SingletonList(Of VBS.StatementSyntax)(VBFactory.SimpleAssignmentStatement(ResultIdentifier, CType(ExpressionOrThrow, VBS.ExpressionSyntax).AdjustExpressionLeadingTrivia.WithTrailingEOL))
                        Else
                            Dim Declarators As SeparatedSyntaxList(Of VBS.VariableDeclaratorSyntax) = VBFactory.SingletonSeparatedList(
                                node:=VBFactory.VariableDeclarator(
                                                    VBFactory.SingletonSeparatedList(VariableName),
                                                    asClause:=Nothing,
                                                    initializer:=Nothing).WithTrailingEOL
                                            )
                            Statements = New SyntaxList(Of VBS.StatementSyntax)
                            Statements = Statements.Add(VBFactory.LocalDeclarationStatement(DimModifier, Declarators).WithTrailingEOL)
                            Statements = Statements.Add(CType(ExpressionOrThrow.WithTrailingEOL, VBS.StatementSyntax))
                        End If

                        If i < node.Arms.Count - 1 Then
                            Dim WhenClause As VBS.ExpressionSyntax = CType(node.Arms(i).WhenClause.Accept(Me), VBS.ExpressionSyntax)
                            Dim CaseClause As SeparatedSyntaxList(Of VBS.CaseClauseSyntax) = VBFactory.SingletonSeparatedList(Of VBS.CaseClauseSyntax)(VBFactory.SimpleCaseClause(WhenClause))
                            Blocks = Blocks.Add(VBFactory.CaseBlock(VBFactory.CaseStatement(CaseClause).WithTrailingEOL, Statements))
                        Else
                            Blocks = Blocks.Add(VBFactory.CaseElseBlock(VBFactory.CaseElseStatement(VBFactory.ElseCaseClause).WithTrailingEOL, Statements))
                        End If
                    Else
                        Stop
                    End If
                Next
                Dim EndSelectStatement As VBS.EndBlockStatementSyntax = VBFactory.EndBlockStatement(
                                                                    VB.SyntaxKind.EndSelectStatement,
                                                                    EndKeyword,
                                                                    SelectKeyword).
                                                                        WithConvertedTriviaFrom(node.CloseBraceToken)
                Dim stmt As VBS.SelectBlockSyntax = VBFactory.SelectBlock(SelectCaseStatement,
                                                                        Blocks,
                                                                        EndSelectStatement
                                                                        )
                Dim localDeclarationStatement As VBS.LocalDeclarationStatementSyntax = VBFactory.LocalDeclarationStatement(
                                                                                DimModifier,
                                                                                VBFactory.SingletonSeparatedList(ResultVariable))
                StatementWithIssue.AddMarker(localDeclarationStatement.WithTrailingEOL, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                StatementWithIssue.AddMarker(stmt, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                Return ResultIdentifier
            End Function

            Public Overrides Function VisitVariableDeclaration(node As VariableDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim StatementWithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                Dim LeadingTrivia As SyntaxTriviaList = StatementWithIssue.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=True, "VB has no direct equivalent To C# var pattern expressions")
                Return MyBase.VisitVariableDeclaration(node)
            End Function

            Public Overrides Function VisitVariableDeclarator(node As VariableDeclaratorSyntax) As VB.VisualBasicSyntaxNode
                Dim Identifier As SyntaxToken = GenerateSafeVBToken(node.Identifier, IsQualifiedName:=False, IsTypeName:=False)
                Dim ArgumentList As New List(Of VBS.ArgumentSyntax)
                If node.ArgumentList Is Nothing Then
                    Return VBFactory.ModifiedIdentifier(Identifier).WithTrailingTrivia(SpaceTrivia)
                Else
                    For i As Integer = 0 To node.ArgumentList.Arguments.Count - 1
                        Dim Expression As VBS.ExpressionSyntax = CType(node.ArgumentList.Arguments(i).Expression.Accept(Me), VBS.ExpressionSyntax)
                        If TypeOf Expression Is VBS.LiteralExpressionSyntax Then
                            Dim LiteralExpression As VBS.LiteralExpressionSyntax = CType(Expression, VBS.LiteralExpressionSyntax)
                            ArgumentList.Add(VBFactory.SimpleArgument(VBFactory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, VBFactory.Literal(CInt(LiteralExpression.Token.Value) - 1))).WithConvertedLeadingTriviaFrom(node.ArgumentList.Arguments(i).Expression))
                        ElseIf TypeOf Expression Is VBS.IdentifierNameSyntax Then
                            Dim Id As VBS.IdentifierNameSyntax = CType(Expression, VBS.IdentifierNameSyntax)
                            ArgumentList.Add(VBFactory.SimpleArgument(VBFactory.BinaryExpression(VB.SyntaxKind.SubtractExpression, Id, MinusToken, ExpressionD1)).WithConvertedLeadingTriviaFrom(node.ArgumentList.Arguments(i).Expression))
                        ElseIf TypeOf Expression Is VBS.BinaryExpressionSyntax Then
                            Dim Value As VBS.BinaryExpressionSyntax = CType(Expression, VBS.BinaryExpressionSyntax)
                            If Expression.IsKind(VB.SyntaxKind.AddExpression) Then
                                If Value.Right.IsKind(VB.SyntaxKind.NumericLiteralExpression) AndAlso CType(Value.Right, VBS.LiteralExpressionSyntax).Token.ValueText = "1" Then
                                    ArgumentList.Add(VBFactory.SimpleArgument(Value.Left.WithConvertedLeadingTriviaFrom(node.ArgumentList.Arguments(i).Expression)))
                                    Continue For
                                End If
                            End If
                            ArgumentList.Add(VBFactory.SimpleArgument(VBFactory.BinaryExpression(VB.SyntaxKind.SubtractExpression, Value, MinusToken, ExpressionD1)).WithConvertedLeadingTriviaFrom(node.ArgumentList.Arguments(i).Expression))
                        Else
                            Stop
                        End If
                    Next
                End If
                Dim Nullable As SyntaxToken = Nothing
                Dim ArrayBounds As VBS.ArgumentListSyntax = VBFactory.ArgumentList(VBFactory.SeparatedList(ArgumentList))
                Return VBFactory.ModifiedIdentifier(Identifier.WithTrailingTrivia(SpaceTrivia), Nullable, ArrayBounds, arrayRankSpecifiers:=Nothing)
            End Function

            Public Overrides Function VisitWhenClause(node As WhenClauseSyntax) As VB.VisualBasicSyntaxNode
                Return node.Condition.Accept(Me)
            End Function

        End Class

    End Class

End Namespace
