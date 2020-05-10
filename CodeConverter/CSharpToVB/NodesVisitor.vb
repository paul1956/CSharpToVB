' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Diagnostics.CodeAnalysis
Imports System.Text

Imports CSharpToVBCodeConverter.Util

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Simplification
Imports Microsoft.CodeAnalysis.VisualBasic

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax

Imports VB = Microsoft.CodeAnalysis.VisualBasic

Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBCodeConverter.DestVisualBasic

    Partial Public Class CSharpConverter

        Partial Friend Class NodesVisitor
            Inherits CS.CSharpSyntaxVisitor(Of VisualBasicSyntaxNode)

            ' This file contains all the stuff accessed by multiple Visitor functions in Class NodeVisitor and Visitors that
            ' had no better home.

            Private ReadOnly _defaultVBOptions As DefaultVBOptions
            Private ReadOnly _isModuleStack As New Stack(Of Boolean)
            Private ReadOnly _mSemanticModel As SemanticModel
            Private ReadOnly _reportException As Action(Of Exception)
            Private _membersList As SyntaxList(Of VBS.StatementSyntax)
            Private _placeholder As Integer = 1
            Public ReadOnly _allImports As List(Of VBS.ImportsStatementSyntax) = New List(Of VBS.ImportsStatementSyntax)()
            Public ReadOnly _inlineAssignHelperMarkers As List(Of CSS.BaseTypeDeclarationSyntax) = New List(Of CSS.BaseTypeDeclarationSyntax)()

            Public Sub New(lSemanticModel As SemanticModel, DefaultVBOptions As DefaultVBOptions, ReportException As Action(Of Exception))
                _mSemanticModel = lSemanticModel
                _reportException = ReportException
                _defaultVBOptions = DefaultVBOptions
            End Sub

            Public ReadOnly Property IsModule As Boolean
                Get
                    If _isModuleStack.Count = 0 Then
                        Return False
                    End If
                    Return _isModuleStack.Peek
                End Get
            End Property

            Private Shared Function MapVBOptions(DefaultVBOptions As DefaultVBOptions) As SyntaxList(Of VBS.OptionStatementSyntax)
                Dim Options As New SyntaxList(Of VBS.OptionStatementSyntax)
                With DefaultVBOptions
                    If .OptionCompareInclude Then
                        Options = Options.Add(VBFactory.OptionStatement(CompareToken, If(.OptionCompare = "Text", TextToken, BinaryToken)).WithTrailingEOL)
                    End If
                    If .OptionExplicitInclude Then
                        Options = Options.Add(VBFactory.OptionStatement(ExplicitToken, If(.OptionExplicit = "On", OnToken, OffToken)).WithTrailingEOL)
                    End If
                    If .OptionInferInclude Then
                        Options = Options.Add(VBFactory.OptionStatement(InferToken, If(.OptionInfer = "On", OnToken, OffToken)).WithTrailingEOL)

                    End If
                    If .OptionStrictInclude Then
                        Options = Options.Add(VBFactory.OptionStatement(StrictToken, If(.OptionStrict = "On", OnToken, OffToken)).WithTrailingEOL)
                    End If
                End With
                Return Options
            End Function

            <ExcludeFromCodeCoverage>
            Public Overrides Function DefaultVisit(node As SyntaxNode) As VisualBasicSyntaxNode
                Throw New NotImplementedException(node.[GetType]().ToString & " not implemented!")
            End Function

            Public Overrides Function VisitCompilationUnit(node As CSS.CompilationUnitSyntax) As VisualBasicSyntaxNode
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

                Dim Options As SyntaxList(Of VBS.OptionStatementSyntax) = MapVBOptions(_defaultVBOptions)
                _membersList = New SyntaxList(Of VBS.StatementSyntax)
                Dim ListOfAttributes As SyntaxList(Of VBS.AttributesStatementSyntax) = VBFactory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) VBFactory.AttributesStatement(VBFactory.SingletonList(DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))))
                For Each m As CSS.MemberDeclarationSyntax In node.Members
                    If s_originalRequest.CancelToken.IsCancellationRequested Then
                        Throw New OperationCanceledException
                    End If
                    Dim Statement As VBS.StatementSyntax = DirectCast(m.Accept(Me), VBS.StatementSyntax)
                    If Statement IsNot Nothing Then
                        _membersList = _membersList.AddRange(ReplaceOneStatementWithMarkedStatements(m, Statement))
                    Else
                        Options = New SyntaxList(Of VBS.OptionStatementSyntax)
                    End If
                Next
                Dim compilationUnitSyntax1 As VBS.CompilationUnitSyntax
                Dim EndOfFIleTokenWithTrivia As SyntaxToken = EndOfFileToken.WithConvertedTriviaFrom(node.EndOfFileToken)

                If externList.Any Then
                    compilationUnitSyntax1 = VBFactory.CompilationUnit(
                        VBFactory.List(Of VBS.OptionStatementSyntax)(),
                        VBFactory.List(_allImports),
                        ListOfAttributes,
                        _membersList).WithTriviaFrom(externList(0))
                Else
                    If _allImports.Any Then
                        If _membersList.Any AndAlso _membersList(0).HasLeadingTrivia Then
                            If (TypeOf _membersList(0) IsNot VBS.NamespaceBlockSyntax AndAlso
                                TypeOf _membersList(0) IsNot VBS.ModuleBlockSyntax) OrElse
                                _membersList(0).GetLeadingTrivia.ToFullString.Contains("auto-generated", StringComparison.OrdinalIgnoreCase) Then
                                Dim HeadingTriviaList As New List(Of SyntaxTrivia)
                                HeadingTriviaList.AddRange(_membersList(0).GetLeadingTrivia)
                                If HeadingTriviaList(0).IsKind(SyntaxKind.EndOfLineTrivia) Then
                                    HeadingTriviaList.RemoveAt(0)
                                    If HeadingTriviaList.Any Then
                                        HeadingTriviaList.Add(VBEOLTrivia)
                                    End If
                                End If
                                Dim NewMemberList As New SyntaxList(Of VBS.StatementSyntax)
                                NewMemberList = NewMemberList.Add(_membersList(0).WithLeadingTrivia(SpaceTrivia))
                                _membersList = NewMemberList.AddRange(_membersList.RemoveAt(0))
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
                        If Options.Any AndAlso _allImports(0).GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                            Options = Options.Replace(Options(0), Options(0).WithLeadingTrivia(_allImports(0).GetLeadingTrivia))
                            _allImports(0) = _allImports(0).WithLeadingTrivia(VBEOLTrivia)
                        End If
                    ElseIf Options.Any AndAlso ListOfAttributes.Any AndAlso ListOfAttributes(0).GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        Options = Options.Replace(Options(0), Options(0).WithLeadingTrivia(ListOfAttributes(0).GetLeadingTrivia))
                        ListOfAttributes = ListOfAttributes.Replace(ListOfAttributes(0), ListOfAttributes(0).WithLeadingTrivia(VBEOLTrivia))
                    ElseIf Options.Any AndAlso _membersList.Any AndAlso _membersList(0).GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        Options = Options.Replace(Options(0), Options(0).WithLeadingTrivia(_membersList(0).GetLeadingTrivia))
                        _membersList = _membersList.Replace(_membersList(0), _membersList(0).WithLeadingTrivia(VBEOLTrivia))
                    End If
                    compilationUnitSyntax1 = VBFactory.CompilationUnit(
                                                        Options,
                                                        VBFactory.List(_allImports),
                                                        ListOfAttributes,
                                                        _membersList,
                                                        EndOfFIleTokenWithTrivia)
                End If
                If HasMarkerError() Then
                    ' There are statements that were left out of translation
                    Throw New ApplicationException(GetMarkerErrorMessage)
                End If
                Return compilationUnitSyntax1
            End Function

            Public Overrides Function VisitDeclarationPattern(node As CSS.DeclarationPatternSyntax) As VisualBasicSyntaxNode
                Dim StatementWithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                Dim LeadingTrivia As SyntaxTriviaList = StatementWithIssue.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=True, "VB has no direct equivalent To C# pattern variables 'is' expressions")
                Dim Designation As CSS.SingleVariableDesignationSyntax = DirectCast(node.Designation, CSS.SingleVariableDesignationSyntax)

                Dim value As VBS.ExpressionSyntax = VBFactory.ParseExpression($"TryCast({node.Designation.Accept(Me).NormalizeWhitespace.ToFullString}, {node.Type.Accept(Me).NormalizeWhitespace.ToFullString})")

                Dim SeparatedSyntaxListOfModifiedIdentifier As SeparatedSyntaxList(Of VBS.ModifiedIdentifierSyntax) =
                        VBFactory.SingletonSeparatedList(
                            VBFactory.ModifiedIdentifier(GenerateSafeVBToken(Designation.Identifier)))
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

            Public Overrides Function VisitLocalFunctionStatement(node As CSS.LocalFunctionStatementSyntax) As VisualBasicSyntaxNode
                Return MyBase.VisitLocalFunctionStatement(node)
            End Function

            Public Overrides Function VisitMakeRefExpression(node As CSS.MakeRefExpressionSyntax) As VisualBasicSyntaxNode
                GetStatementwithIssues(node).AddMarker(FlagUnsupportedStatements(GetStatementwithIssues(node),
                                                                                 "MakeRef Expressions",
                                                                                 CommentOutOriginalStatements:=False),
                                                       StatementHandlingOption.PrependStatement,
                                                       AllowDuplicates:=True)
                Return VBFactory.InvocationExpression(VBFactory.ParseExpression($"__makeref({node.Expression.Accept(Me).ToFullString})").WithConvertedTriviaFrom(node))
            End Function

            Public Overrides Function VisitOmittedArraySizeExpression(node As CSS.OmittedArraySizeExpressionSyntax) As VisualBasicSyntaxNode
                Return VBFactory.OmittedArgument()
            End Function

            Public Overrides Function VisitRefExpression(node As CSS.RefExpressionSyntax) As VisualBasicSyntaxNode
                Dim StatementwithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                StatementwithIssue.AddMarker(FlagUnsupportedStatements(StatementwithIssue, "ref expression", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=False)
                Return NothingExpression
            End Function

            Public Overrides Function VisitRefType(node As CSS.RefTypeSyntax) As VisualBasicSyntaxNode
                Dim StatementwithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                StatementwithIssue.AddMarker(FlagUnsupportedStatements(StatementwithIssue, "ref type", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=False)
                Return HandleRefType
            End Function

            Public Overrides Function VisitRefTypeExpression(node As CSS.RefTypeExpressionSyntax) As VisualBasicSyntaxNode
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
            Public Overrides Function VisitSingleVariableDesignation(Node As CSS.SingleVariableDesignationSyntax) As VisualBasicSyntaxNode
                Dim identifier As SyntaxToken = GenerateSafeVBToken(Node.Identifier)
                Dim identifierExpression As VBS.IdentifierNameSyntax = VBFactory.IdentifierName(identifier)
                Dim ModifiedIdentifier As VBS.ModifiedIdentifierSyntax = VBFactory.ModifiedIdentifier(identifier).WithTrailingTrivia(SpaceTrivia)
                Dim SeparatedSyntaxListOfModifiedIdentifier As SeparatedSyntaxList(Of VBS.ModifiedIdentifierSyntax) =
                    VBFactory.SingletonSeparatedList(
                        ModifiedIdentifier
                        )

                If Node.Parent.IsKind(CS.SyntaxKind.DeclarationExpression) Then
                    ' var quantity
                    Dim Designation As CSS.DeclarationExpressionSyntax = CType(Node.Parent, CSS.DeclarationExpressionSyntax)
                    ' out var quantity
                    Dim Declaration As CSS.ArgumentSyntax = CType(Designation.Parent, CSS.ArgumentSyntax)
                    ' (item.tostring, out var quantity)
                    Dim TypeName As VBS.TypeSyntax
                    If Designation.Type.IsVar Then
                        TypeName = VBFactory.PredefinedType(ObjectKeyword)
                        Dim ArgumentList As CSS.ArgumentListSyntax = TryCast(Declaration.Parent, CSS.ArgumentListSyntax)
                        If ArgumentList IsNot Nothing AndAlso
                           ArgumentList.Arguments.Count = 2 AndAlso
                           ArgumentList.Parent.IsKind(CS.SyntaxKind.InvocationExpression) AndAlso
                           ArgumentList.Arguments(1).Equals(Declaration) Then
                            Dim Invocation As CSS.InvocationExpressionSyntax = TryCast(ArgumentList.Parent, CSS.InvocationExpressionSyntax)
                            If Invocation IsNot Nothing Then
                                Dim Expression As CSS.MemberAccessExpressionSyntax = TryCast(Invocation.Expression, CSS.MemberAccessExpressionSyntax)
                                If Expression IsNot Nothing Then
                                    If Expression.Name.Identifier.ValueText = "TryGetValue" Then
                                        Dim expression1 As CSS.ExpressionSyntax
                                        If TypeOf Invocation.Expression Is CSS.MemberAccessExpressionSyntax Then
                                            expression1 = CType(Invocation.Expression, CSS.MemberAccessExpressionSyntax).Expression
                                            Dim _Typeinfo As TypeInfo = ModelExtensions.GetTypeInfo(_mSemanticModel, expression1)
                                            If _Typeinfo.Type IsNot Nothing Then
                                                If Not _Typeinfo.Type.IsErrorType Then
                                                    TypeName = ConvertToType(_Typeinfo.Type)
                                                    If TypeOf TypeName Is VBS.GenericNameSyntax Then
                                                        Dim _arguments As SeparatedSyntaxList(Of VBS.TypeSyntax) = CType(TypeName, VBS.GenericNameSyntax).TypeArgumentList.Arguments
                                                        If _arguments.Count = 2 Then
                                                            TypeName = _arguments(1)
                                                        End If
                                                    End If
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

                    Dim SeparatedListOfvariableDeclarations As SeparatedSyntaxList(Of VBS.VariableDeclaratorSyntax) =
                        VBFactory.SingletonSeparatedList(
                            VBFactory.VariableDeclarator(
                                SeparatedSyntaxListOfModifiedIdentifier,
                                VBFactory.SimpleAsClause(TypeName),
                                VBFactory.EqualsValue(NothingExpression)
                                    )
                             )

                    Dim declarationToBeAdded As VBS.LocalDeclarationStatementSyntax =
                        VBFactory.LocalDeclarationStatement(
                            DimModifier,
                            SeparatedListOfvariableDeclarations).WithAdditionalAnnotations(Simplifier.Annotation)

                    GetStatementwithIssues(Node).AddMarker(declarationToBeAdded, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                ElseIf Node.Parent.IsKind(CS.SyntaxKind.DeclarationPattern) Then
                    Dim DeclarationPattern As CSS.DeclarationPatternSyntax = DirectCast(Node.Parent, CSS.DeclarationPatternSyntax)
                    Dim CasePatternSwitchLabel As CSS.SwitchLabelSyntax = DirectCast(DeclarationPattern.Parent, CSS.SwitchLabelSyntax)
                    Dim SwitchSection As CSS.SwitchSectionSyntax = DirectCast(CasePatternSwitchLabel.Parent, CSS.SwitchSectionSyntax)
                    Dim SwitchStatement As CSS.SwitchStatementSyntax = DirectCast(SwitchSection.Parent, CSS.SwitchStatementSyntax)
                    Dim SwitchExpression As VBS.ExpressionSyntax = DirectCast(SwitchStatement.Expression.Accept(Me), VBS.ExpressionSyntax)

                    Dim TypeName As VBS.TypeSyntax = DirectCast(DeclarationPattern.Type.Accept(Me), VBS.TypeSyntax)
                    Return VBFactory.TypeOfIsExpression(SwitchExpression, TypeName)
                End If

                Return identifierExpression
            End Function

            Public Overrides Function VisitStackAllocArrayCreationExpression(node As CSS.StackAllocArrayCreationExpressionSyntax) As VisualBasicSyntaxNode
                node.FirstAncestorOrSelf(Of CSS.StatementSyntax).AddMarker(FlagUnsupportedStatements(node.FirstAncestorOrSelf(Of CSS.StatementSyntax), "StackAlloc", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=True)
                Return PredefinedTypeObject
            End Function

            Public Overrides Function VisitSwitchExpression(node As CSS.SwitchExpressionSyntax) As VisualBasicSyntaxNode
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
                For Each e As IndexClass(Of CSS.SwitchExpressionArmSyntax) In node.Arms.WithIndex
                    Dim arm As CSS.SwitchExpressionArmSyntax = e.Value
                    Dim equalsTokenWithTrivia As SyntaxToken = EqualsToken
                    If TypeOf arm.Pattern Is CSS.ConstantPatternSyntax Then
                        Dim ConstatntPattern As CSS.ConstantPatternSyntax = CType(arm.Pattern, CSS.ConstantPatternSyntax)
                        Dim ConstatntPatternExpression As VBS.ExpressionSyntax = CType(ConstatntPattern.Expression.Accept(Me), VBS.ExpressionSyntax)
                        Dim RelationalCaseClause As VBS.RelationalCaseClauseSyntax = VBFactory.CaseEqualsClause(ConstatntPatternExpression.WithLeadingTrivia(SpaceTrivia))
                        Dim CaseStatement As VBS.CaseStatementSyntax = VBFactory.CaseStatement(RelationalCaseClause).WithPrependedLeadingTrivia(ConstatntPatternExpression.GetLeadingTrivia)
                        Dim VBNode As VisualBasicSyntaxNode = arm.Expression.Accept(Me)
                        Dim Statements As SyntaxList(Of VBS.StatementSyntax)
                        If VBNode.IsKind(SyntaxKind.ThrowStatement) Then
                            Statements = VBFactory.SingletonList(DirectCast(VBNode, VBS.StatementSyntax))
                        Else
                            Dim Expression As VBS.ExpressionSyntax = CType(VBNode, VBS.ExpressionSyntax)
                            Dim LeadingTrivia As New List(Of SyntaxTrivia)
                            LeadingTrivia.AddRange(Expression.GetLeadingTrivia)
                            Expression = Expression.WithLeadingTrivia(SpaceTrivia)
                            Statements = VBFactory.SingletonList(Of VBS.StatementSyntax)(VBFactory.SimpleAssignmentStatement(ResultIdentifier, Expression.WithTrailingEOL).WithLeadingTrivia(LeadingTrivia))
                        End If
                        Blocks = Blocks.Add(VBFactory.CaseBlock(CaseStatement.WithTrailingEOL, Statements))
                    ElseIf TypeOf arm.Pattern Is CSS.DiscardPatternSyntax Then
                        Dim ExpressionOrThrow As VisualBasicSyntaxNode = arm.Expression.Accept(Me)
                        Dim Statements As SyntaxList(Of VBS.StatementSyntax)
                        If TypeOf ExpressionOrThrow Is VBS.ExpressionSyntax Then
                            Statements = VBFactory.SingletonList(Of VBS.StatementSyntax)(VBFactory.SimpleAssignmentStatement(ResultIdentifier, CType(ExpressionOrThrow, VBS.ExpressionSyntax).AdjustExpressionLeadingTrivia.WithTrailingEOL))
                        Else
                            Statements = VBFactory.SingletonList(Of VBS.StatementSyntax)(CType(ExpressionOrThrow, VBS.ThrowStatementSyntax).WithTrailingEOL)
                        End If
                        If arm.WhenClause IsNot Nothing Then
                            Dim WhenClause As VBS.ExpressionSyntax = CType(e.Value.WhenClause.Accept(Me), VBS.ExpressionSyntax)
                            Dim CaseClause As SeparatedSyntaxList(Of VBS.CaseClauseSyntax) = VBFactory.SingletonSeparatedList(Of VBS.CaseClauseSyntax)(VBFactory.SimpleCaseClause(WhenClause))
                            Blocks = Blocks.Add(VBFactory.CaseBlock(VBFactory.CaseStatement(CaseClause).WithTrailingEOL, Statements))
                        Else
                            Blocks = Blocks.Add(VBFactory.CaseElseBlock(VBFactory.CaseElseStatement(VBFactory.ElseCaseClause).WithTrailingEOL, Statements))
                        End If
                    ElseIf TypeOf arm.Pattern Is CSS.RecursivePatternSyntax Then
                        StatementWithIssue.AddMarker(FlagUnsupportedStatements(node, "Switch Expression with Recursive Pattern Syntax", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=True)
                        Return ResultIdentifier
                    ElseIf TypeOf arm.Pattern Is CSS.DeclarationPatternSyntax Then
                        Dim Pattern As CSS.DeclarationPatternSyntax = DirectCast(arm.Pattern, CSS.DeclarationPatternSyntax)
                        Dim VariableType As VBS.TypeSyntax = DirectCast(Pattern.Type.Accept(Me), VBS.TypeSyntax).WithLeadingTrivia(SpaceTrivia)
                        Dim _tryCast As VBS.TryCastExpressionSyntax = VBFactory.TryCastExpression(governingExpression, VariableType.WithLeadingTrivia(SpaceTrivia))
                        If Pattern.Designation.IsKind(CS.SyntaxKind.SingleVariableDesignation) Then
                            Dim designation As CSS.SingleVariableDesignationSyntax = CType(Pattern.Designation, CSS.SingleVariableDesignationSyntax)
                            Dim identifierToken As SyntaxToken = GenerateSafeVBToken(designation.Identifier)
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
                            statements = statements.Add(VBFactory.AssignmentStatement(SyntaxKind.SimpleAssignmentStatement, ResultIdentifier, equalsTokenWithTrivia, right))
                            statements = statements.Add(VBFactory.AssignmentStatement(SyntaxKind.SimpleAssignmentStatement, VBFactory.IdentifierName(identifierToken), equalsTokenWithTrivia, ResultIdentifier))
                            Blocks = Blocks.Add(VBFactory.CaseBlock(VBFactory.CaseStatement(CaseClause).WithTrailingEOL, statements))
                        ElseIf Pattern.Designation.IsKind(CS.SyntaxKind.DiscardDesignation) Then
                            Dim statements As New SyntaxList(Of VBS.StatementSyntax)
                            Dim right As VBS.ExpressionSyntax = CType(arm.Expression.Accept(Me), VBS.ExpressionSyntax).AdjustExpressionLeadingTrivia
                            If right.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                equalsTokenWithTrivia = equalsTokenWithTrivia.WithTrailingTrivia(SpaceTrivia, LineContinuation, VBEOLTrivia)
                            End If
                            Dim CaseClause As SeparatedSyntaxList(Of VBS.CaseClauseSyntax) =
                                    VBFactory.SingletonSeparatedList(Of VBS.CaseClauseSyntax)(VBFactory.SimpleCaseClause(VBFactory.EqualsExpression(_tryCast, VBFactory.TrueLiteralExpression(TrueKeyword))))
                            statements = statements.Add(VBFactory.AssignmentStatement(SyntaxKind.SimpleAssignmentStatement, ResultIdentifier, equalsTokenWithTrivia, right))
                            Blocks = Blocks.Add(VBFactory.CaseBlock(VBFactory.CaseStatement(CaseClause).WithTrailingEOL, statements))
                        Else
                            Stop
                        End If
                    ElseIf TypeOf arm.Pattern Is CSS.VarPatternSyntax Then
                        Dim VarPattern As CSS.VarPatternSyntax = DirectCast(arm.Pattern, CSS.VarPatternSyntax)
                        Dim Statements As SyntaxList(Of VBS.StatementSyntax)
                        Dim Identifier As SyntaxToken
                        If TypeOf VarPattern.Designation Is CSS.SingleVariableDesignationSyntax Then
                            Identifier = GenerateSafeVBToken(id:=DirectCast(VarPattern.Designation, CSS.SingleVariableDesignationSyntax).Identifier)
                        ElseIf TypeOf VarPattern.Designation Is CSS.ParenthesizedVariableDesignationSyntax Then
                            Dim sBuilder As New StringBuilder
                            CreateDesignationName(ProcessVariableDesignation(CType(VarPattern.Designation, CSS.ParenthesizedVariableDesignationSyntax)), sBuilder)
                            Identifier = GenerateSafeVBToken(id:=CS.SyntaxFactory.Identifier(sBuilder.ToString))
                        Else
                            Stop
                            Throw UnreachableException
                        End If
                        Dim Name As VBS.IdentifierNameSyntax = VBFactory.IdentifierName(Identifier.ToString)

                        Dim VariableName As VBS.ModifiedIdentifierSyntax = VBFactory.ModifiedIdentifier(Identifier.WithTrailingTrivia(SpaceTrivia))
                        Dim ExpressionOrThrow As VisualBasicSyntaxNode = arm.Expression.Accept(Me)

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

                        If Not e.IsLast Then
                            Dim WhenClause As VBS.ExpressionSyntax = CType(e.Value.WhenClause.Accept(Me), VBS.ExpressionSyntax)
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
                                                                    SyntaxKind.EndSelectStatement,
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

            Public Overrides Function VisitVariableDeclaration(node As CSS.VariableDeclarationSyntax) As VisualBasicSyntaxNode
                Dim StatementWithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                Dim LeadingTrivia As SyntaxTriviaList = StatementWithIssue.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=True, "VB has no direct equivalent To C# var pattern expressions")
                Return MyBase.VisitVariableDeclaration(node)
            End Function

            Public Overrides Function VisitVariableDeclarator(node As CSS.VariableDeclaratorSyntax) As VisualBasicSyntaxNode
                Dim Identifier As SyntaxToken = GenerateSafeVBToken(node.Identifier, IsQualifiedName:=False, IsTypeName:=False)
                Dim ArgumentList As New List(Of VBS.ArgumentSyntax)
                If node.ArgumentList Is Nothing Then
                    Return VBFactory.ModifiedIdentifier(Identifier).WithTrailingTrivia(SpaceTrivia)
                Else
                    For Each e As IndexClass(Of CSS.ArgumentSyntax) In node.ArgumentList.Arguments.WithIndex
                        Dim vbExpression As VBS.ExpressionSyntax = CType(e.Value.Expression.Accept(Me), VBS.ExpressionSyntax)
                        If TypeOf vbExpression Is VBS.LiteralExpressionSyntax Then
                            Dim LiteralExpression As VBS.LiteralExpressionSyntax = CType(vbExpression, VBS.LiteralExpressionSyntax)
                            ArgumentList.Add(VBFactory.SimpleArgument(VBFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, VBFactory.Literal(CInt(LiteralExpression.Token.Value) - 1))).WithConvertedLeadingTriviaFrom(e.Value.Expression))
                        ElseIf TypeOf vbExpression Is VBS.IdentifierNameSyntax Then
                            Dim Id As VBS.IdentifierNameSyntax = CType(vbExpression, VBS.IdentifierNameSyntax)
                            ArgumentList.Add(VBFactory.SimpleArgument(VBFactory.BinaryExpression(SyntaxKind.SubtractExpression, Id, MinusToken, ExpressionD1)).WithConvertedLeadingTriviaFrom(e.Value.Expression))
                        ElseIf TypeOf vbExpression Is VBS.BinaryExpressionSyntax Then
                            Dim Value As VBS.BinaryExpressionSyntax = CType(vbExpression, VBS.BinaryExpressionSyntax)
                            If vbExpression.IsKind(SyntaxKind.AddExpression) Then
                                If Value.Right.IsKind(SyntaxKind.NumericLiteralExpression) AndAlso CType(Value.Right, VBS.LiteralExpressionSyntax).Token.ValueText = "1" Then
                                    ArgumentList.Add(VBFactory.SimpleArgument(Value.Left.WithConvertedLeadingTriviaFrom(e.Value.Expression)))
                                    Continue For
                                End If
                            End If
                            ArgumentList.Add(VBFactory.SimpleArgument(VBFactory.BinaryExpression(SyntaxKind.SubtractExpression, Value, MinusToken, ExpressionD1)).WithConvertedLeadingTriviaFrom(e.Value.Expression))
                        Else
                            Stop
                        End If
                    Next
                End If
                Dim Nullable As SyntaxToken = Nothing
                Dim ArrayBounds As VBS.ArgumentListSyntax = VBFactory.ArgumentList(VBFactory.SeparatedList(ArgumentList))
                Return VBFactory.ModifiedIdentifier(Identifier.WithTrailingTrivia(SpaceTrivia), Nullable, ArrayBounds, arrayRankSpecifiers:=Nothing)
            End Function

            Public Overrides Function VisitWhenClause(node As CSS.WhenClauseSyntax) As VisualBasicSyntaxNode
                Return node.Condition.Accept(Me)
            End Function

        End Class

    End Class

End Namespace
