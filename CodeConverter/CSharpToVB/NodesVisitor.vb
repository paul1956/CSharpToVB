Option Explicit On
Option Infer Off
Option Strict On

Imports IVisualBasicCode.CodeConverter.Util

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CSharp.Syntax
Imports Microsoft.CodeAnalysis.Simplification
Imports Microsoft.CodeAnalysis.VisualBasic
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace IVisualBasicCode.CodeConverter.Visual_Basic

    Partial Public Class CSharpConverter

        Partial Protected Friend Class NodesVisitor
            Inherits CS.CSharpSyntaxVisitor(Of VB.VisualBasicSyntaxNode)

            ' This file contains all the stuff accessed by multiple Visitor functions in Class NodeVisitor and Visitors that
            ' had no better home.

            ReadOnly allImports As List(Of VBS.ImportsStatementSyntax) = New List(Of VBS.ImportsStatementSyntax)()
            ReadOnly inlineAssignHelperMarkers As List(Of CSS.BaseTypeDeclarationSyntax) = New List(Of CSS.BaseTypeDeclarationSyntax)()
            Private ReadOnly IsModuleStack As New Stack(Of Boolean)
            Private ReadOnly mSemanticModel As SemanticModel
            Private placeholder As Integer = 1

            Public Sub New(lSemanticModel As SemanticModel)
                Me.mSemanticModel = lSemanticModel
            End Sub

            Public ReadOnly Property IsModule As Boolean
                Get
                    If Me.IsModuleStack.Count = 0 Then
                        Return False
                    End If
                    Return Me.IsModuleStack.Peek
                End Get
            End Property

            Public Overrides Function DefaultVisit(node As SyntaxNode) As VB.VisualBasicSyntaxNode
                Throw New NotImplementedException(node.[GetType]().ToString & " not implemented!")
            End Function

            Public Overrides Function VisitCompilationUnit(node As CSS.CompilationUnitSyntax) As VB.VisualBasicSyntaxNode

                For Each [using] As CSS.UsingDirectiveSyntax In node.Usings
                    [using].Accept(Me)
                Next
                Dim externList As New List(Of VB.VisualBasicSyntaxNode)
                ' externlist is potentially a list of empty lines with trivia
                For Each extern As CSS.ExternAliasDirectiveSyntax In node.Externs
                    externList.Add(extern.Accept(Me))
                Next

                Dim Options As SyntaxList(Of VBS.OptionStatementSyntax) = VBFactory.List(Of VBS.OptionStatementSyntax)
                Options = Options.Add(VBFactory.OptionStatement(ExplicitToken, OffToken))
                Options = Options.Add(VBFactory.OptionStatement(InferToken, OnToken))
                Options = Options.Add(VBFactory.OptionStatement(StrictToken, OffToken))

                Dim ListOfAttributes As SyntaxList(Of VBS.AttributesStatementSyntax) = VBFactory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) VBFactory.AttributesStatement(VBFactory.SingletonList(DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))))
                Dim Members As SyntaxList(Of VBS.StatementSyntax) = VBFactory.List(node.Members.Select(Function(m As CSS.MemberDeclarationSyntax) DirectCast(m.Accept(Me), VBS.StatementSyntax)))
                Dim compilationUnitSyntax1 As VBS.CompilationUnitSyntax
                Dim EndOfFIleTokenWithTrivia As SyntaxToken = EndOfFileToken.WithConvertedTriviaFrom(node.EndOfFileToken)

                If externList.Count > 0 Then
                    compilationUnitSyntax1 = VBFactory.CompilationUnit(
                        VBFactory.List(Of VBS.OptionStatementSyntax)(),
                        VBFactory.List(Me.allImports),
                        ListOfAttributes,
                        Members).WithTriviaFrom(externList(0))
                ElseIf Me.allImports.Count > 0 Then
                    If Members.Count > 0 AndAlso Members(0).HasLeadingTrivia Then
                        If (TypeOf Members(0) IsNot VBS.NamespaceBlockSyntax AndAlso TypeOf Members(0) IsNot VBS.ModuleBlockSyntax) OrElse Members(0).GetLeadingTrivia.ToFullString.Contains("auto-generated") Then
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
                            Me.allImports(0) = Me.allImports(0).WithPrependedLeadingTrivia(NewLeadingTrivia)
                        End If
                    End If
                    compilationUnitSyntax1 = VBFactory.CompilationUnit(
                                                                Options,
                                                                VBFactory.List(Me.allImports),
                                                                ListOfAttributes,
                                                                Members,
                                                                EndOfFIleTokenWithTrivia
                                                                )
                Else
                    compilationUnitSyntax1 = VBFactory.CompilationUnit(
                                                                Options,
                                                                VBFactory.List(Me.allImports),
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

            Public Overrides Function VisitDeclarationPattern(node As CSS.DeclarationPatternSyntax) As VB.VisualBasicSyntaxNode
                Dim StatementWithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                Dim LeadingTrivia As SyntaxTriviaList = CheckCorrectnessLeadingTrivia(StatementWithIssue, "VB has no direct equivalent To C# pattern variables 'is' expressions")
                Dim Designation As CSS.SingleVariableDesignationSyntax = DirectCast(node.Designation, CSS.SingleVariableDesignationSyntax)

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

            Public Overrides Function VisitImplicitElementAccess(node As CSS.ImplicitElementAccessSyntax) As VB.VisualBasicSyntaxNode
                If node.ArgumentList.Arguments.Count > 1 Then
                    Throw New NotSupportedException("ImplicitElementAccess can only have one argument!")
                End If
                Return node.ArgumentList.Arguments(0).Expression.Accept(Me).WithConvertedTriviaFrom(node.ArgumentList.Arguments(0).Expression)
            End Function

            Public Overrides Function VisitLocalFunctionStatement(node As CSS.LocalFunctionStatementSyntax) As VB.VisualBasicSyntaxNode
                Return MyBase.VisitLocalFunctionStatement(node)
            End Function

            Public Overrides Function VisitMakeRefExpression(node As CSS.MakeRefExpressionSyntax) As VB.VisualBasicSyntaxNode

                Dim StatementwithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                StatementwithIssue.AddMarker(FlagUnsupportedStatements(StatementwithIssue, "MakeRef Expressions", CommentOutOriginalStatements:=False), StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                Dim Expression As VBS.ExpressionSyntax = VBFactory.ParseExpression($"__makeref({node.Expression.Accept(Me).ToFullString})").WithConvertedTriviaFrom(node)
                Return VBFactory.InvocationExpression(Expression)
            End Function

            Public Overrides Function VisitOmittedArraySizeExpression(node As CSS.OmittedArraySizeExpressionSyntax) As VB.VisualBasicSyntaxNode
                Return VBFactory.OmittedArgument()
            End Function

            Public Overrides Function VisitRefExpression(node As CSS.RefExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim StatementwithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                StatementwithIssue.AddMarker(FlagUnsupportedStatements(StatementwithIssue, "ref expression", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=False)
                Return NothingExpression
            End Function

            Public Overrides Function VisitRefType(node As CSS.RefTypeSyntax) As VB.VisualBasicSyntaxNode
                Dim StatementwithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                StatementwithIssue.AddMarker(FlagUnsupportedStatements(StatementwithIssue, "ref type", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=False)
                Return Handle_RefType
            End Function

            Public Overrides Function VisitRefTypeExpression(node As CSS.RefTypeExpressionSyntax) As VB.VisualBasicSyntaxNode
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
            Public Overrides Function VisitSingleVariableDesignation(Node As CSS.SingleVariableDesignationSyntax) As VB.VisualBasicSyntaxNode
                Dim Identifier As SyntaxToken = GenerateSafeVBToken(Node.Identifier, IsQualifiedName:=False)
                Dim IdentifierExpression As VBS.IdentifierNameSyntax = VBFactory.IdentifierName(Identifier)
                Dim ModifiedIdentifier As VBS.ModifiedIdentifierSyntax = VBFactory.ModifiedIdentifier(Identifier).WithTrailingTrivia(SpaceTrivia)
                Dim SeparatedSyntaxListOfModifiedIdentifier As SeparatedSyntaxList(Of VBS.ModifiedIdentifierSyntax) =
                    VBFactory.SingletonSeparatedList(
                        ModifiedIdentifier
                        )

                If Node.Parent.IsKind(CS.SyntaxKind.DeclarationExpression) Then
                    Dim Parent As CSS.DeclarationExpressionSyntax = DirectCast(Node.Parent, CSS.DeclarationExpressionSyntax)
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
                    Dim DeclarationPattern As CSS.DeclarationPatternSyntax = DirectCast(Node.Parent, CSS.DeclarationPatternSyntax)
                    Dim CasePatternSwitchLabel As CSS.SwitchLabelSyntax = DirectCast(DeclarationPattern.Parent, CSS.SwitchLabelSyntax)
                    Dim SwitchSection As CSS.SwitchSectionSyntax = DirectCast(CasePatternSwitchLabel.Parent, CSS.SwitchSectionSyntax)
                    Dim SwitchStatement As CSS.SwitchStatementSyntax = DirectCast(SwitchSection.Parent, CSS.SwitchStatementSyntax)
                    Dim SwitchExpression As VBS.ExpressionSyntax = DirectCast(SwitchStatement.Expression.Accept(Me), VBS.ExpressionSyntax)

                    Dim TypeName As VBS.TypeSyntax = DirectCast(DeclarationPattern.Type.Accept(Me), VBS.TypeSyntax)
                    'DeclarationPattern.Type.CheckCorrectnessLeadingTrivia(($"' TODO: VB does not support Declaration Pattern. An attempt was made to convert the original Case Clause was ""case {Node.Parent.ToFullString}:"""))
                    Return VBFactory.TypeOfIsExpression(SwitchExpression, TypeName)
                End If

                Return IdentifierExpression
            End Function

            Public Overrides Function VisitStackAllocArrayCreationExpression(node As CSS.StackAllocArrayCreationExpressionSyntax) As VB.VisualBasicSyntaxNode
                node.FirstAncestorOrSelf(Of CSS.StatementSyntax).AddMarker(FlagUnsupportedStatements(node.FirstAncestorOrSelf(Of CSS.StatementSyntax), "StackAlloc", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=True)
                Return PredefinedTypeObject
            End Function

            Public Overrides Function VisitSwitchExpression(node As SwitchExpressionSyntax) As VisualBasicSyntaxNode
                Dim StatementWithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)

                Dim NewCaseStatement As VBS.SelectStatementSyntax = VBFactory.SelectStatement(CType(node.GoverningExpression.Accept(Me), VBS.ExpressionSyntax))
                Dim TempVariableName As SyntaxToken = VBFactory.Identifier(MethodBodyVisitor.GetUniqueVariableNameInScope(node, "tempVar", Me.mSemanticModel))
                Dim TempIdentifier As VBS.IdentifierNameSyntax = VBFactory.IdentifierName(TempVariableName)
                Dim _Typeinfo As TypeInfo = ModelExtensions.GetTypeInfo(Me.mSemanticModel, node.GoverningExpression)
                Dim Variable As VBS.VariableDeclaratorSyntax
                Dim AsClause As VBS.AsClauseSyntax = Nothing
                If _Typeinfo.Type IsNot Nothing AndAlso Not _Typeinfo.Type.IsErrorType Then
                    AsClause = VBFactory.SimpleAsClause(ConvertToType(_Typeinfo.Type.ToString))
                End If
                Variable = VBFactory.VariableDeclarator(VBFactory.SingletonSeparatedList(VBFactory.ModifiedIdentifier(TempVariableName)), AsClause, initializer:=Nothing)
                Dim Blocks As New SyntaxList(Of VBS.CaseBlockSyntax)
                For Each arm As CSS.SwitchExpressionArmSyntax In node.Arms
                    If TypeOf arm.Pattern Is CSS.ConstantPatternSyntax Then
                        Dim ConstatntPattern As ConstantPatternSyntax = CType(arm.Pattern, ConstantPatternSyntax)
                        Dim RelationalCaseClause As VBS.RelationalCaseClauseSyntax = VBFactory.CaseEqualsClause(CType(ConstatntPattern.Expression.Accept(Me), VBS.ExpressionSyntax))
                        Dim CaseStatement As VBS.CaseStatementSyntax = VBFactory.CaseStatement(RelationalCaseClause)
                        Dim Statements As SyntaxList(Of VBS.StatementSyntax) = VBFactory.SingletonList(Of VBS.StatementSyntax)(VBFactory.SimpleAssignmentStatement(TempIdentifier, CType(arm.Expression.Accept(Me), VBS.ExpressionSyntax)))
                        Blocks = Blocks.Add(VBFactory.CaseBlock(CaseStatement, Statements))
                    ElseIf TypeOf arm.Pattern Is CSS.DiscardPatternSyntax Then
                        Dim ExpressionOrThrow As VisualBasicSyntaxNode = arm.Expression.Accept(Me)
                        Dim Statements As SyntaxList(Of VBS.StatementSyntax)
                        If TypeOf ExpressionOrThrow Is VBS.ExpressionSyntax Then
                            Statements = VBFactory.SingletonList(Of VBS.StatementSyntax)(VBFactory.SimpleAssignmentStatement(TempIdentifier, CType(ExpressionOrThrow, VBS.ExpressionSyntax)))
                        Else
                            Statements = VBFactory.SingletonList(Of VBS.StatementSyntax)(CType(ExpressionOrThrow, VBS.ThrowStatementSyntax))
                        End If
                        Blocks = Blocks.Add(VBFactory.CaseElseBlock(VBFactory.CaseElseStatement(VBFactory.ElseCaseClause), Statements))
                    ElseIf TypeOf arm.Pattern Is CSS.RecursivePatternSyntax Then
                        StatementWithIssue.AddMarker(FlagUnsupportedStatements(node, "Swith Expression with Recursive Pattern Syntax", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=True)
                        Return TempIdentifier
                    ElseIf TypeOf arm.Pattern Is CSS.DeclarationPatternSyntax Then
                        StatementWithIssue.AddMarker(FlagUnsupportedStatements(node, "Swith Expression with Declaration Pattern Syntax", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=True)
                        Return TempIdentifier
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
                                                                                VBFactory.SingletonSeparatedList(Of VBS.VariableDeclaratorSyntax)(Variable))
                StatementWithIssue.AddMarker(localDeclarationStatement, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                StatementWithIssue.AddMarker(stmt, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                Return TempIdentifier
            End Function

            Public Overrides Function VisitVariableDeclaration(node As VariableDeclarationSyntax) As VisualBasicSyntaxNode
                Dim StatementWithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                Dim LeadingTrivia As SyntaxTriviaList = CheckCorrectnessLeadingTrivia(StatementWithIssue, "VB has no direct equivalent To C# var pattern expressions")
                Return MyBase.VisitVariableDeclaration(node)
            End Function

            Public Overrides Function VisitVariableDeclarator(node As CSS.VariableDeclaratorSyntax) As VB.VisualBasicSyntaxNode
                Dim Identifier As SyntaxToken = GenerateSafeVBToken(node.Identifier, IsQualifiedName:=False)
                Dim ArgumentList As New List(Of VBS.ArgumentSyntax)
                If node.ArgumentList IsNot Nothing Then
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
                Else
                    Return VBFactory.ModifiedIdentifier(Identifier).WithTrailingTrivia(SpaceTrivia)
                End If
                Dim Nullable As SyntaxToken = Nothing
                Dim ArrayBounds As VBS.ArgumentListSyntax = VBFactory.ArgumentList(VBFactory.SeparatedList(ArgumentList))
                Return VBFactory.ModifiedIdentifier(Identifier.WithTrailingTrivia(SpaceTrivia), Nullable, ArrayBounds, arrayRankSpecifiers:=Nothing)
            End Function

            Public Overrides Function VisitWhenClause(node As WhenClauseSyntax) As VisualBasicSyntaxNode
                Return node.Condition.Accept(Me)
            End Function
        End Class

    End Class

End Namespace