' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

Imports System.Diagnostics.CodeAnalysis

Imports CSharpToVBCodeConverter.Util

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CSharp.Syntax
Imports Microsoft.CodeAnalysis.Simplification
Imports Microsoft.CodeAnalysis.VisualBasic

Imports CS = Microsoft.CodeAnalysis.CSharp

Imports VB = Microsoft.CodeAnalysis.VisualBasic

Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBCodeConverter.Visual_Basic

    Partial Public Class CSharpConverter

        Partial Protected Friend Class NodesVisitor
            Inherits CS.CSharpSyntaxVisitor(Of VisualBasicSyntaxNode)

            ' This file contains all the stuff accessed by multiple Visitor functions in Class NodeVisitor and Visitors that
            ' had no better home.

            ReadOnly allImports As List(Of VBS.ImportsStatementSyntax) = New List(Of VBS.ImportsStatementSyntax)()
            ReadOnly inlineAssignHelperMarkers As List(Of BaseTypeDeclarationSyntax) = New List(Of BaseTypeDeclarationSyntax)()
            Private ReadOnly IsModuleStack As New Stack(Of Boolean)
            Private ReadOnly mSemanticModel As SemanticModel
            Private placeholder As Integer = 1

            Public Sub New(lSemanticModel As SemanticModel)
                mSemanticModel = lSemanticModel
            End Sub

            Public ReadOnly Property IsModule As Boolean
                Get
                    If IsModuleStack.Count = 0 Then
                        Return False
                    End If
                    Return IsModuleStack.Peek
                End Get
            End Property

            <SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            <ExcludeFromCodeCoverage>
            Public Overrides Function DefaultVisit(node As SyntaxNode) As VisualBasicSyntaxNode
                Throw New NotImplementedException(node.[GetType]().ToString & " not implemented!")
            End Function

            <SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitCompilationUnit(node As CompilationUnitSyntax) As VisualBasicSyntaxNode

                For Each [using] As UsingDirectiveSyntax In node.Usings
                    [using].Accept(Me)
                Next
                Dim externList As New List(Of VisualBasicSyntaxNode)
                ' externlist is potentially a list of empty lines with trivia
                For Each extern As ExternAliasDirectiveSyntax In node.Externs
                    externList.Add(extern.Accept(Me))
                Next

                Dim Options As SyntaxList(Of VBS.OptionStatementSyntax) = VBFactory.List(Of VBS.OptionStatementSyntax)
                Options = Options.Add(VBFactory.OptionStatement(ExplicitToken, OffToken))
                Options = Options.Add(VBFactory.OptionStatement(InferToken, OnToken))
                Options = Options.Add(VBFactory.OptionStatement(StrictToken, OffToken).WithTrailingEOL)

                Dim ListOfAttributes As SyntaxList(Of VBS.AttributesStatementSyntax) = VBFactory.List(node.AttributeLists.Select(Function(a As AttributeListSyntax) VBFactory.AttributesStatement(VBFactory.SingletonList(DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))))
                Dim Members As SyntaxList(Of VBS.StatementSyntax) = VBFactory.List(node.Members.Select(Function(m As MemberDeclarationSyntax) DirectCast(m.Accept(Me), VBS.StatementSyntax)))
                Dim compilationUnitSyntax1 As VBS.CompilationUnitSyntax
                Dim EndOfFIleTokenWithTrivia As SyntaxToken = EndOfFileToken.WithConvertedTriviaFrom(node.EndOfFileToken)

                If externList.Count > 0 Then
                    compilationUnitSyntax1 = VBFactory.CompilationUnit(
                        VBFactory.List(Of VBS.OptionStatementSyntax)(),
                        VBFactory.List(allImports),
                        ListOfAttributes,
                        Members).WithTriviaFrom(externList(0))
                ElseIf allImports.Count > 0 Then
                    If Members.Count > 0 AndAlso Members(0).HasLeadingTrivia Then
                        If (TypeOf Members(0) IsNot VBS.NamespaceBlockSyntax AndAlso TypeOf Members(0) IsNot VBS.ModuleBlockSyntax) OrElse
                            Members(0).GetLeadingTrivia.ToFullString.Contains("auto-generated", StringComparison.InvariantCultureIgnoreCase) Then
                            Dim HeadingTriviaList As New List(Of SyntaxTrivia)
                            HeadingTriviaList.AddRange(Members(0).GetLeadingTrivia)
                            If HeadingTriviaList(0).IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                HeadingTriviaList.RemoveAt(0)
                                If HeadingTriviaList.Count > 0 Then
                                    HeadingTriviaList.Add(VB_EOLTrivia)
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
                            allImports(0) = allImports(0).WithPrependedLeadingTrivia(NewLeadingTrivia)
                        End If
                    End If
                    compilationUnitSyntax1 = VBFactory.CompilationUnit(
                                                                Options,
                                                                VBFactory.List(allImports),
                                                                ListOfAttributes,
                                                                Members,
                                                                EndOfFIleTokenWithTrivia
                                                                )
                Else
                    compilationUnitSyntax1 = VBFactory.CompilationUnit(
                                                                Options,
                                                                VBFactory.List(allImports),
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

            Public Overrides Function VisitDeclarationPattern(node As DeclarationPatternSyntax) As VisualBasicSyntaxNode
                Dim StatementWithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                Dim LeadingTrivia As SyntaxTriviaList = CheckCorrectnessLeadingTrivia(StatementWithIssue, "VB has no direct equivalent To C# pattern variables 'is' expressions")
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

            <SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitImplicitElementAccess(node As ImplicitElementAccessSyntax) As VisualBasicSyntaxNode
                If node.ArgumentList.Arguments.Count > 1 Then
                    Throw New NotSupportedException("ImplicitElementAccess can only have one argument!")
                End If
                Return node.ArgumentList.Arguments(0).Expression.Accept(Me).WithConvertedTriviaFrom(node.ArgumentList.Arguments(0).Expression)
            End Function

            Public Overrides Function VisitLocalFunctionStatement(node As LocalFunctionStatementSyntax) As VisualBasicSyntaxNode
                Return MyBase.VisitLocalFunctionStatement(node)
            End Function

            Public Overrides Function VisitMakeRefExpression(node As MakeRefExpressionSyntax) As VisualBasicSyntaxNode

                Dim StatementwithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                StatementwithIssue.AddMarker(FlagUnsupportedStatements(StatementwithIssue, "MakeRef Expressions", CommentOutOriginalStatements:=False), StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                Dim Expression As VBS.ExpressionSyntax = VBFactory.ParseExpression($"__makeref({node.Expression.Accept(Me).ToFullString})").WithConvertedTriviaFrom(node)
                Return VBFactory.InvocationExpression(Expression)
            End Function

            Public Overrides Function VisitOmittedArraySizeExpression(node As OmittedArraySizeExpressionSyntax) As VisualBasicSyntaxNode
                Return VBFactory.OmittedArgument()
            End Function

            Public Overrides Function VisitRefExpression(node As RefExpressionSyntax) As VisualBasicSyntaxNode
                Dim StatementwithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                StatementwithIssue.AddMarker(FlagUnsupportedStatements(StatementwithIssue, "ref expression", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=False)
                Return NothingExpression
            End Function

            Public Overrides Function VisitRefType(node As RefTypeSyntax) As VisualBasicSyntaxNode
                Dim StatementwithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                StatementwithIssue.AddMarker(FlagUnsupportedStatements(StatementwithIssue, "ref type", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=False)
                Return Handle_RefType
            End Function

            Public Overrides Function VisitRefTypeExpression(node As RefTypeExpressionSyntax) As VisualBasicSyntaxNode
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
            <SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitSingleVariableDesignation(Node As SingleVariableDesignationSyntax) As VisualBasicSyntaxNode
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

            <SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitStackAllocArrayCreationExpression(node As StackAllocArrayCreationExpressionSyntax) As VisualBasicSyntaxNode
                node.FirstAncestorOrSelf(Of StatementSyntax).AddMarker(FlagUnsupportedStatements(node.FirstAncestorOrSelf(Of StatementSyntax), "StackAlloc", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=True)
                Return PredefinedTypeObject
            End Function

            <SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitSwitchExpression(node As SwitchExpressionSyntax) As VisualBasicSyntaxNode
                Dim StatementWithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)

                Dim NewCaseStatement As VBS.SelectStatementSyntax = VBFactory.SelectStatement(CType(node.GoverningExpression.Accept(Me), VBS.ExpressionSyntax))
                Dim TempVariableName As SyntaxToken = VBFactory.Identifier(MethodBodyVisitor.GetUniqueVariableNameInScope(node, "tempVar", mSemanticModel))
                Dim TempIdentifier As VBS.IdentifierNameSyntax = VBFactory.IdentifierName(TempVariableName)
                Dim _Typeinfo As TypeInfo = ModelExtensions.GetTypeInfo(mSemanticModel, node.GoverningExpression)
                Dim Variable As VBS.VariableDeclaratorSyntax
                Dim AsClause As VBS.AsClauseSyntax = Nothing
                If _Typeinfo.Type IsNot Nothing AndAlso Not _Typeinfo.Type.IsErrorType Then
                    AsClause = VBFactory.SimpleAsClause(ConvertToType(_Typeinfo.Type.ToString))
                End If
                Variable = VBFactory.VariableDeclarator(VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(TempVariableName)), AsClause, initializer:=Nothing)
                Dim Blocks As New SyntaxList(Of VBS.CaseBlockSyntax)
                For Each arm As SwitchExpressionArmSyntax In node.Arms
                    If TypeOf arm.Pattern Is ConstantPatternSyntax Then
                        Dim ConstatntPattern As ConstantPatternSyntax = CType(arm.Pattern, ConstantPatternSyntax)
                        Dim ConstatntPatternExpression As VBS.ExpressionSyntax = CType(ConstatntPattern.Expression.Accept(Me), VBS.ExpressionSyntax)
                        Dim RelationalCaseClause As VBS.RelationalCaseClauseSyntax = VBFactory.CaseEqualsClause(ConstatntPatternExpression.WithLeadingTrivia(SpaceTrivia))
                        Dim CaseStatement As VBS.CaseStatementSyntax = VBFactory.CaseStatement(RelationalCaseClause).WithPrependedLeadingTrivia(ConstatntPatternExpression.GetLeadingTrivia)
                        Dim Expression As VBS.ExpressionSyntax = CType(arm.Expression.Accept(Me), VBS.ExpressionSyntax)
                        Dim LeadingTrivia As New List(Of SyntaxTrivia)
                        LeadingTrivia.AddRange(Expression.GetLeadingTrivia)
                        Expression = Expression.WithLeadingTrivia(SpaceTrivia)
                        Dim Statements As SyntaxList(Of VBS.StatementSyntax) = VBFactory.SingletonList(Of VBS.StatementSyntax)(VBFactory.SimpleAssignmentStatement(TempIdentifier, Expression.WithTrailingEOL).WithLeadingTrivia(LeadingTrivia))
                        Blocks = Blocks.Add(VBFactory.CaseBlock(CaseStatement.WithTrailingEOL, Statements))
                    ElseIf TypeOf arm.Pattern Is DiscardPatternSyntax Then
                        Dim ExpressionOrThrow As VisualBasicSyntaxNode = arm.Expression.Accept(Me)
                        Dim Statements As SyntaxList(Of VBS.StatementSyntax)
                        If TypeOf ExpressionOrThrow Is VBS.ExpressionSyntax Then
                            Statements = VBFactory.SingletonList(Of VBS.StatementSyntax)(VBFactory.SimpleAssignmentStatement(TempIdentifier, CType(ExpressionOrThrow, VBS.ExpressionSyntax).WithTrailingEOL))
                        Else
                            Statements = VBFactory.SingletonList(Of VBS.StatementSyntax)(CType(ExpressionOrThrow, VBS.ThrowStatementSyntax).WithTrailingEOL)
                        End If
                        Blocks = Blocks.Add(VBFactory.CaseElseBlock(VBFactory.CaseElseStatement(VBFactory.ElseCaseClause).WithTrailingEOL, Statements))
                    ElseIf TypeOf arm.Pattern Is RecursivePatternSyntax Then
                        StatementWithIssue.AddMarker(FlagUnsupportedStatements(node, "Switch Expression with Recursive Pattern Syntax", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=True)
                        Return TempIdentifier
                    ElseIf TypeOf arm.Pattern Is DeclarationPatternSyntax Then
                        StatementWithIssue.AddMarker(FlagUnsupportedStatements(node, "Switch Expression with Declaration Pattern Syntax", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=True)
                        Return TempIdentifier
                    ElseIf TypeOf arm.Pattern Is VarPatternSyntax Then
                        Dim VarPattern As VarPatternSyntax = DirectCast(arm.Pattern, VarPatternSyntax)
                        Dim Statements As SyntaxList(Of VBS.StatementSyntax)
                        Dim SingleVariableDesignation As SingleVariableDesignationSyntax = DirectCast(VarPattern.Designation, SingleVariableDesignationSyntax)
                        Dim Identifier As SyntaxToken = GenerateSafeVBToken(id:=SingleVariableDesignation.Identifier, IsQualifiedName:=False, IsTypeName:=False)
                        Dim Name As VBS.IdentifierNameSyntax = VBFactory.IdentifierName(Identifier.ToString)

                        Dim VariableName As VBS.ModifiedIdentifierSyntax = VBFactory.ModifiedIdentifier(Identifier.WithTrailingTrivia(SpaceTrivia))
                        Dim ExpressionOrThrow As VisualBasicSyntaxNode = arm.Expression.Accept(Me)

                        If TypeOf ExpressionOrThrow Is VBS.ExpressionSyntax Then
                            Dim Declarators As SeparatedSyntaxList(Of VBS.VariableDeclaratorSyntax) = VBFactory.SingletonSeparatedList(
                            node:=VBFactory.VariableDeclarator(VBFactory.SingletonSeparatedList(
                            VariableName),
                            asClause:=Nothing, initializer:=VBFactory.EqualsValue(CType(ExpressionOrThrow, VBS.ExpressionSyntax))).WithTrailingEOL
                             )
                            Statements = VBFactory.SingletonList(Of VBS.StatementSyntax)(VBFactory.SimpleAssignmentStatement(TempIdentifier, CType(ExpressionOrThrow, VBS.ExpressionSyntax).WithTrailingEOL))
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
                        Blocks = Blocks.Add(VBFactory.CaseElseBlock(VBFactory.CaseElseStatement(VBFactory.ElseCaseClause).WithTrailingEOL, Statements))
                    Else
                        Stop
                    End If
                Next
                Dim EndSelectStatement As VBS.EndBlockStatementSyntax = VBFactory.EndBlockStatement(
                                                                    VB.SyntaxKind.EndSelectStatement,
                                                                    EndKeyword,
                                                                    SelectKeyword).
                                                                        WithConvertedTriviaFrom(node.CloseBraceToken)
                Dim stmt As VBS.SelectBlockSyntax = VBFactory.SelectBlock(NewCaseStatement,
                                                                        Blocks,
                                                                        EndSelectStatement
                                                                        )
                Dim localDeclarationStatement As VBS.LocalDeclarationStatementSyntax = VBFactory.LocalDeclarationStatement(
                                                                                DimModifier,
                                                                                VBFactory.SingletonSeparatedList(Variable))
                StatementWithIssue.AddMarker(localDeclarationStatement.WithTrailingEOL, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                StatementWithIssue.AddMarker(stmt, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                Return TempIdentifier
            End Function

            <SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitVariableDeclaration(node As VariableDeclarationSyntax) As VisualBasicSyntaxNode
                Dim StatementWithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                Dim LeadingTrivia As SyntaxTriviaList = CheckCorrectnessLeadingTrivia(StatementWithIssue, "VB has no direct equivalent To C# var pattern expressions")
                Return MyBase.VisitVariableDeclaration(node)
            End Function

            <SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitVariableDeclarator(node As VariableDeclaratorSyntax) As VisualBasicSyntaxNode
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
                            ArgumentList.Add(VBFactory.SimpleArgument(VBFactory.BinaryExpression(VB.SyntaxKind.SubtractExpression, Id, MinusToken, Expression_1)).WithConvertedLeadingTriviaFrom(node.ArgumentList.Arguments(i).Expression))
                        ElseIf TypeOf Expression Is VBS.BinaryExpressionSyntax Then
                            Dim Value As VBS.BinaryExpressionSyntax = CType(Expression, VBS.BinaryExpressionSyntax)
                            If Expression.IsKind(VB.SyntaxKind.AddExpression) Then
                                If Value.Right.IsKind(VB.SyntaxKind.NumericLiteralExpression) AndAlso CType(Value.Right, VBS.LiteralExpressionSyntax).Token.ValueText = "1" Then
                                    ArgumentList.Add(VBFactory.SimpleArgument(Value.Left.WithConvertedLeadingTriviaFrom(node.ArgumentList.Arguments(i).Expression)))
                                    Continue For
                                End If
                            End If
                            ArgumentList.Add(VBFactory.SimpleArgument(VBFactory.BinaryExpression(VB.SyntaxKind.SubtractExpression, Value, MinusToken, Expression_1)).WithConvertedLeadingTriviaFrom(node.ArgumentList.Arguments(i).Expression))
                        Else
                            Stop
                        End If
                    Next
                End If
                Dim Nullable As SyntaxToken = Nothing
                Dim ArrayBounds As VBS.ArgumentListSyntax = VBFactory.ArgumentList(VBFactory.SeparatedList(ArgumentList))
                Return VBFactory.ModifiedIdentifier(Identifier.WithTrailingTrivia(SpaceTrivia), Nullable, ArrayBounds, arrayRankSpecifiers:=Nothing)
            End Function

            <SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitWhenClause(node As WhenClauseSyntax) As VisualBasicSyntaxNode
                Return node.Condition.Accept(Me)
            End Function

        End Class

    End Class

End Namespace
