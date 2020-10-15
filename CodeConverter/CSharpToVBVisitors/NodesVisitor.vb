' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Diagnostics.CodeAnalysis
Imports System.Text

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax

Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBConverter.ToVisualBasic

    Partial Public Class CSharpConverter

        Partial Friend Class NodesVisitor
            Inherits CS.CSharpSyntaxVisitor(Of VB.VisualBasicSyntaxNode)

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

            Friend Sub New(lSemanticModel As SemanticModel, DefaultVBOptions As DefaultVBOptions, ReportException As Action(Of Exception))
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
            Public Overrides Function DefaultVisit(node As SyntaxNode) As VB.VisualBasicSyntaxNode
                Throw New NotImplementedException(node.GetType().ToString & " not implemented!")
            End Function

            Public Overrides Function VisitCompilationUnit(node As CSS.CompilationUnitSyntax) As VB.VisualBasicSyntaxNode
                If node.GetLeadingTrivia.FirstOrDefault.IsKind(CS.SyntaxKind.SingleLineCommentTrivia) Then
                    VBHeaderLeadingTrivia = node.GetLeadingTrivia.GetDocumentBanner
                End If
                For Each [using] As CSS.UsingDirectiveSyntax In node.Usings
                    If s_originalRequest.CancelToken.IsCancellationRequested Then
                        Throw New OperationCanceledException
                    End If
                    [using].Accept(Me)
                Next
                Dim externList As New List(Of VB.VisualBasicSyntaxNode)
                ' externlist is potentially a list of empty lines with trivia
                For Each extern As CSS.ExternAliasDirectiveSyntax In node.Externs
                    externList.Add(extern.Accept(Me))
                Next

                Dim Options As New SyntaxList(Of VBS.OptionStatementSyntax)
                With _defaultVBOptions
                    If .OptionCompareInclude Then
                        Options = Options.Add(Factory.OptionStatement(CompareToken, If(.OptionCompare = "Text", TextToken, BinaryToken)).WithTrailingEOL)
                    End If
                    If .OptionExplicitInclude Then
                        Options = Options.Add(Factory.OptionStatement(ExplicitToken, If(.OptionExplicit = "On", OnToken, OffToken)).WithTrailingEOL)
                    End If
                    If .OptionInferInclude Then
                        Options = Options.Add(Factory.OptionStatement(InferToken, If(.OptionInfer = "On", OnToken, OffToken)).WithTrailingEOL)

                    End If
                    If .OptionStrictInclude Then
                        Options = Options.Add(Factory.OptionStatement(StrictToken, If(.OptionStrict = "On", OnToken, OffToken)).WithTrailingEOL)
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
                If AllImports.Any Then
                    If _membersList.Any AndAlso _membersList(0).HasLeadingTrivia Then
                        If (TypeOf _membersList(0) IsNot VBS.NamespaceBlockSyntax AndAlso
                                TypeOf _membersList(0) IsNot VBS.ModuleBlockSyntax) OrElse
                                _membersList(0).GetLeadingTrivia.ToFullString.Contains("auto-generated", StringComparison.OrdinalIgnoreCase) Then
                            Dim HeadingTriviaList As SyntaxTriviaList = _membersList(0).GetLeadingTrivia
                            If HeadingTriviaList(0).IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                HeadingTriviaList = HeadingTriviaList.RemoveAt(0)
                                If HeadingTriviaList.Any Then
                                    HeadingTriviaList = HeadingTriviaList.Add(VBEOLTrivia)
                                End If
                            End If
                            Dim NewMemberList As New SyntaxList(Of VBS.StatementSyntax)
                            NewMemberList = NewMemberList.Add(_membersList(0).WithLeadingTrivia(Factory.Space))
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
                    For i As Integer = 0 To AllImports.Count - 1
                        Dim importsClause As VBS.ImportsClauseSyntax = AllImports(i).ImportsClauses.FirstOrDefault
                        If TypeOf importsClause Is VBS.SimpleImportsClauseSyntax Then
                            Dim simpleImportsClause As VBS.SimpleImportsClauseSyntax = DirectCast(importsClause, VBS.SimpleImportsClauseSyntax)
                            If TypeOf simpleImportsClause.Name Is VBS.IdentifierNameSyntax Then
                                If i = 0 Then
                                    Select Case DirectCast(simpleImportsClause.Name, VBS.IdentifierNameSyntax).Identifier.ValueText
                                        Case "System"
                                            Dim IsLast As Boolean = i = (AllImports.Count - 1)
                                            If Not IsLast Then
                                                AllImports(i + 1) = AllImports(i + 1).WithMergedLeadingTrivia(AllImports(i).GetLeadingTrivia)
                                            End If
                                            AllImports.RemoveAt(i)
                                            Exit For
                                        Case CompilerServices, InteropServices
                                            AllImports(0) = AllImports(0).WithLeadingTrivia(node.GetLeadingTrivia.ConvertTriviaList).WithUniqueLeadingTrivia(VBHeaderLeadingTrivia)
                                            If _membersList.Any Then
                                                _membersList = _membersList.Replace(_membersList(0), _membersList(0).WithUniqueLeadingTrivia(AllImports(0).GetLeadingTrivia))
                                            End If
                                    End Select
                                End If
                            End If
                        End If
                    Next
                    If Options.Any Then
                        If VBHeaderLeadingTrivia.Any Then
                            Options = Options.Replace(Options.First, Options.First.WithLeadingTrivia(VBHeaderLeadingTrivia.Add(VBEOLTrivia)))
                            If AllImports.Any AndAlso Not AllImports.First.GetLeadingTrivia.FirstOrDefault.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                Options = Options.Replace(Options.Last, Options.Last.WithAppendedEOL)
                            End If
                        End If
                    End If
                ElseIf Options.Any Then
                    If ListOfAttributes.Any AndAlso ListOfAttributes(0).GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        Options = Options.Replace(Options(0), Options(0).WithLeadingTrivia(VBHeaderLeadingTrivia.Add(VBEOLTrivia)))
                        ListOfAttributes(0) = ListOfAttributes(0).WithUniqueLeadingTrivia(VBHeaderLeadingTrivia)
                        If Not ListOfAttributes.First.GetLeadingTrivia.FirstOrDefault.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            Options = Options.Replace(Options.Last, Options.Last.WithAppendedEOL)
                        End If
                    ElseIf _membersList.Any AndAlso
                        VBHeaderLeadingTrivia.Any AndAlso
                        _membersList(0).GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then

                        Options = Options.Replace(Options(0), Options(0).WithLeadingTrivia(VBHeaderLeadingTrivia))
                        _membersList = _membersList.Replace(_membersList(0), _membersList(0).WithUniqueLeadingTrivia(VBHeaderLeadingTrivia))
                        If Not _membersList(0).GetLeadingTrivia.FirstOrDefault.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
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

            Public Overrides Function VisitGlobalStatement(node As CSS.GlobalStatementSyntax) As VB.VisualBasicSyntaxNode
                Dim MethodBodyVisitor As New MethodBodyVisitor(_mSemanticModel, Me)
                _membersList = _membersList.AddRange(node.Statement.Accept(MethodBodyVisitor))
                Return Nothing
            End Function

            Public Overrides Function VisitImplicitElementAccess(node As CSS.ImplicitElementAccessSyntax) As VB.VisualBasicSyntaxNode
                If node.ArgumentList.Arguments.Count > 1 Then
                    Throw New NotSupportedException("ImplicitElementAccess can only have one argument!")
                End If
                Return node.ArgumentList.Arguments(0).Expression.Accept(Me).WithConvertedTriviaFrom(node.ArgumentList.Arguments(0).Expression)
            End Function

            Public Overrides Function VisitImplicitObjectCreationExpression(node As CSS.ImplicitObjectCreationExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim commentTrivia As SyntaxTriviaList = New SyntaxTriviaList
                commentTrivia = commentTrivia.Add(Factory.Space)
                commentTrivia = commentTrivia.Add(LineContinuation)
                commentTrivia = commentTrivia.Add(Factory.CommentTrivia(" ' TODO Visual Basic does not support 'Implicit Object Creation Expression'"))
                Return Factory.ObjectCreationExpression(PredefinedTypeObject.WithTrailingTrivia(commentTrivia))
            End Function

            Public Overrides Function VisitImplicitStackAllocArrayCreationExpression(node As CSS.ImplicitStackAllocArrayCreationExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim possibleInitializer As VB.VisualBasicSyntaxNode = node.Initializer?.Accept(Me)
                Dim initializer As VBS.ObjectCollectionInitializerSyntax = Nothing
                Dim argumentList As VBS.ArgumentListSyntax = Nothing
                Select Case possibleInitializer.Kind
                    Case VB.SyntaxKind.CollectionInitializer
                        initializer = Factory.ObjectCollectionInitializer(initializer:=DirectCast(possibleInitializer, VBS.CollectionInitializerSyntax))
                    Case VB.SyntaxKind.ObjectCollectionInitializer
                        initializer = DirectCast(possibleInitializer, VBS.ObjectCollectionInitializerSyntax)
                    Case VB.SyntaxKind.ObjectMemberInitializer
                        ' Remove trailing trivia before with
                        If argumentList IsNot Nothing Then
                            argumentList = argumentList.WithCloseParenToken(CloseParenToken)
                        End If
                        Dim memberinitializer As VBS.ObjectMemberInitializerSyntax = DirectCast(possibleInitializer, VBS.ObjectMemberInitializerSyntax)
                        Return Factory.ObjectCreationExpression(NewKeyword, Factory.List(Of VBS.AttributeListSyntax)(), PredefinedTypeObject, argumentList, memberinitializer)

                    Case Else
                        _reportException?.Invoke(UnexpectedValue(NameOf(possibleInitializer)))
                End Select
                Return Factory.ObjectCreationExpression(NewKeyword, attributeLists:=Nothing, PredefinedTypeObject, argumentList:=Nothing, initializer)
            End Function

            Public Overrides Function VisitMakeRefExpression(node As CSS.MakeRefExpressionSyntax) As VB.VisualBasicSyntaxNode
                GetStatementwithIssues(node).AddMarker(FlagUnsupportedStatements(GetStatementwithIssues(node),
                                                                                 "MakeRef Expressions",
                                                                                 CommentOutOriginalStatements:=False),
                                                       StatementHandlingOption.PrependStatement,
                                                       AllowDuplicates:=True)
                Return Factory.InvocationExpression(Factory.ParseExpression($"__makeref({node.Expression.Accept(Me).ToFullString})").WithConvertedTriviaFrom(node))
            End Function

            Public Overrides Function VisitOmittedArraySizeExpression(node As CSS.OmittedArraySizeExpressionSyntax) As VB.VisualBasicSyntaxNode
                Return Factory.OmittedArgument()
            End Function

            Public Overrides Function VisitRecordDeclaration(node As CSS.RecordDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim Members As New List(Of VBS.StatementSyntax)
                For Each m As CSS.MemberDeclarationSyntax In node.Members
                    Dim Item As VBS.StatementSyntax = DirectCast(m.Accept(Me), VBS.StatementSyntax)
                    If Item Is Nothing Then
                        Members.Add(Factory.EmptyStatement.WithConvertedTriviaFrom(m))
                    Else
                        Members.AddRange(ReplaceOneStatementWithMarkedStatements(m, Item))
                    End If
                Next
                If node.ParameterList IsNot Nothing Then
                    For Each m As CSS.ParameterSyntax In node.ParameterList.Parameters
                        Dim name As String = m.Identifier.ValueText
                        Dim type As VBS.TypeSyntax = CType(m.Type.Accept(Me), VBS.TypeSyntax)
                        Dim asClause As VBS.AsClauseSyntax = Factory.SimpleAsClause(type)
                        Members.Add(FactoryDimStatement(name, asClause, initializer:=Nothing))
                    Next
                End If
                Dim ParameterList As VBS.ParameterListSyntax = DirectCast(node.ParameterList?.Accept(Me), VBS.ParameterListSyntax)
                If Members.Any Then
                    Members(0) = Members(0).WithPrependedLeadingTrivia(CollectConvertedTokenTrivia(node.OpenBraceToken, GetLeading:=True, GetTrailing:=False))
                End If
                Dim ListOfAttributes As SyntaxList(Of VBS.AttributeListSyntax) = Factory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))
                Dim TypeParameterList As VBS.TypeParameterListSyntax = DirectCast(node.TypeParameterList?.Accept(Me), VBS.TypeParameterListSyntax)
                Dim Modifiers As SyntaxTokenList = Factory.TokenList(ConvertModifiers(node.Modifiers, Me.IsModule, TokenContext.Struct))
                Dim StructureStatement As VBS.StructureStatementSyntax
                StructureStatement = DirectCast(Factory.StructureStatement(ListOfAttributes,
                                                                            Factory.TokenList(Modifiers),
                                                                            StructureKeyword.WithConvertedTriviaFrom(node.Keyword),
                                                                            GenerateSafeVBToken(node.Identifier, node, _mSemanticModel),
                                                                            TypeParameterList
                                                                            ).RestructureAttributesAndModifiers(ListOfAttributes.Any, Modifiers.Any), VBS.StructureStatementSyntax).WithTrailingEOL

                Dim StructureBlock As VBS.StructureBlockSyntax = Factory.StructureBlock(
                                                    StructureStatement,
                                                    [inherits]:=Nothing,
                                                    [implements]:=Nothing,
                                                    members:=Factory.List(Members),
                                                    endStructureStatement:=Factory.EndStructureStatement(EndKeyword.WithTrailingTrivia(Factory.Space), StructureKeyword).WithConvertedTriviaFrom(node.CloseBraceToken)
                                                    )
                Dim ErrorModifiers As New List(Of String)
                For Each t As SyntaxToken In node.Modifiers
                    Select Case t.Text
                        Case "unsafe"
                            ErrorModifiers.Add("unsafe")
                        Case "ref"
                            ErrorModifiers.Add("ref")
                    End Select
                Next

                ' These errors are handled elsewhere just ignore
                ReplaceOneStatementWithMarkedStatements(node, StructureBlock)
                If ErrorModifiers.Any Then
                    StructureBlock = StructureBlock.WithPrependedLeadingTrivia(Factory.CommentTrivia($"' TODO TASK: VB has no direct equivalent to C# {String.Join(" or ", ErrorModifiers)} Structure"))
                End If
                StructureBlock = StructureBlock.WithPrependedLeadingTrivia(Factory.CommentTrivia($"' TODO TASK: VB has no direct equivalent to C# Records"))
                Return StructureBlock
            End Function

            Public Overrides Function VisitRefExpression(node As CSS.RefExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim StatementwithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                StatementwithIssue.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=True, "ref expression")
                Return node.Expression.Accept(Me)
            End Function

            Public Overrides Function VisitRefType(node As CSS.RefTypeSyntax) As VB.VisualBasicSyntaxNode
                Dim StatementwithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                StatementwithIssue.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=True, "ref type")
                Return node.Type.Accept(Me)
            End Function

            Public Overrides Function VisitRefTypeExpression(node As CSS.RefTypeExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim StatementwithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                StatementwithIssue.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=True, "ref type expression")
                Return node.Expression.Accept(Me)
            End Function

            Public Overrides Function VisitStackAllocArrayCreationExpression(node As CSS.StackAllocArrayCreationExpressionSyntax) As VB.VisualBasicSyntaxNode
                node.FirstAncestorOrSelf(Of CSS.StatementSyntax).AddMarker(FlagUnsupportedStatements(node.FirstAncestorOrSelf(Of CSS.StatementSyntax), "StackAlloc", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=True)
                Return PredefinedTypeObject
            End Function

            Public Overrides Function VisitSwitchExpression(node As CSS.SwitchExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim StatementWithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                Dim governingExpression As VBS.ExpressionSyntax = CType(node.GoverningExpression.Accept(Me), VBS.ExpressionSyntax)
                Dim SelectCaseStatement As VBS.SelectStatementSyntax = Factory.SelectStatement(governingExpression.WithLeadingTrivia(Factory.Space)).WithLeadingTrivia(governingExpression.GetLeadingTrivia)
                Dim ResultNameToken As SyntaxToken = Factory.Identifier(node.GetUniqueVariableNameInScope("tempVar", _mSemanticModel))
                Dim ResultIdentifier As VBS.IdentifierNameSyntax = Factory.IdentifierName(ResultNameToken)
                Dim _Typeinfo As TypeInfo = _mSemanticModel.GetTypeInfo(node.Arms(0).Expression)
                Dim AsClause As VBS.AsClauseSyntax = Nothing
                If _Typeinfo.Type IsNot Nothing AndAlso Not _Typeinfo.Type.IsErrorType Then
                    If TypeOf _Typeinfo.Type Is INamedTypeSymbol AndAlso _Typeinfo.Type.IsTupleType Then
                        AsClause = Factory.SimpleAsClause(_Typeinfo.Type.ToString.ConvertCSStringToName.WithLeadingTrivia(Factory.Space))
                    Else
                        AsClause = Factory.SimpleAsClause(_Typeinfo.Type.ConvertToType.WithLeadingTrivia(Factory.Space))
                    End If
                End If

                Dim Blocks As New SyntaxList(Of VBS.CaseBlockSyntax)
                Dim CaseClause As SeparatedSyntaxList(Of VBS.CaseClauseSyntax)
                Dim Statements As SyntaxList(Of VBS.StatementSyntax)
                Dim WhenClause As VBS.ExpressionSyntax
                Dim CaseStatement As VBS.CaseStatementSyntax = Nothing
                For Each e As IndexClass(Of CSS.SwitchExpressionArmSyntax) In node.Arms.WithIndex
                    Dim arm As CSS.SwitchExpressionArmSyntax = e.Value
                    Dim equalsTokenWithTrivia As SyntaxToken = EqualsToken
                    Dim VBNode As VB.VisualBasicSyntaxNode = arm.Expression.Accept(Me)
                    If VBNode.IsKind(VB.SyntaxKind.ThrowStatement) Then
                        Statements = Factory.SingletonList(DirectCast(VBNode, VBS.StatementSyntax))
                    Else
                        Dim Expression As VBS.ExpressionSyntax = CType(VBNode, VBS.ExpressionSyntax)
                        Dim savedLeadingTrivia As SyntaxTriviaList = Expression.GetLeadingTrivia
                        Expression = Expression.WithLeadingTrivia(Factory.Space)
                        Statements = Factory.SingletonList(Of VBS.StatementSyntax)(Factory.SimpleAssignmentStatement(ResultIdentifier,
                                                                                                                     Expression.WithTrailingEOL
                                                                                                                    ).WithLeadingTrivia(savedLeadingTrivia))
                    End If
                    If TypeOf arm.Pattern Is CSS.ConstantPatternSyntax Then
                        Dim ConstantPatternExpression As VBS.ExpressionSyntax = CType(arm.Pattern.Accept(Me), VBS.ExpressionSyntax)
                        Dim RelationalCaseClause As VBS.RelationalCaseClauseSyntax = Factory.CaseEqualsClause(CType(arm.Pattern.Accept(Me), VBS.ExpressionSyntax).WithLeadingTrivia(Factory.Space))
                        CaseStatement = Factory.CaseStatement(RelationalCaseClause).WithPrependedLeadingTrivia(ConstantPatternExpression.GetLeadingTrivia)
                    ElseIf TypeOf arm.Pattern Is CSS.DiscardPatternSyntax Then
                        If arm.WhenClause IsNot Nothing Then
                            WhenClause = CType(e.Value.WhenClause.Accept(Me), VBS.ExpressionSyntax)
                            CaseClause = Factory.SingletonSeparatedList(Of VBS.CaseClauseSyntax)(Factory.SimpleCaseClause(WhenClause))
                        Else
                            WhenClause = Nothing
                        End If
                        CaseStatement = Factory.CaseStatement(CaseClause)
                    ElseIf TypeOf arm.Pattern Is CSS.RecursivePatternSyntax Then
                        StatementWithIssue.AddMarker(FlagUnsupportedStatements(node, "Switch Expression with Recursive Pattern Syntax", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=True)
                        Return ResultIdentifier
                    ElseIf TypeOf arm.Pattern Is CSS.DeclarationPatternSyntax Then
                        Dim Pattern As CSS.DeclarationPatternSyntax = DirectCast(arm.Pattern, CSS.DeclarationPatternSyntax)
                        Dim VariableType As VBS.TypeSyntax = DirectCast(Pattern.Type.Accept(Me), VBS.TypeSyntax).WithLeadingTrivia(Factory.Space)
                        Dim _tryCast As VBS.TryCastExpressionSyntax = Factory.TryCastExpression(governingExpression, VariableType.WithLeadingTrivia(Factory.Space))
                        If Pattern.Designation.IsKind(CS.SyntaxKind.SingleVariableDesignation) Then
                            Dim designation As CSS.SingleVariableDesignationSyntax = CType(Pattern.Designation, CSS.SingleVariableDesignationSyntax)
                            Dim identifierToken As SyntaxToken = GenerateSafeVBToken(designation.Identifier, node, _mSemanticModel)
                            Dim initializer As VBS.EqualsValueSyntax = Factory.EqualsValue(Factory.DirectCastExpression(governingExpression, VariableType))
                            Statements = Statements.Insert(0, FactoryDimStatement(identifierToken,
                                                                                        Factory.SimpleAsClause(VariableType),
                                                                                        initializer).WithConvertedLeadingTriviaFrom(arm).WithTrailingEOL
                                                                                       )
                            Dim armExpression As VBS.ExpressionSyntax = CType(arm.Expression.Accept(Me), VBS.ExpressionSyntax)
                            Dim right As VBS.ExpressionSyntax = armExpression.AdjustExpressionTrivia(AdjustLeading:=True)
                            If right.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                equalsTokenWithTrivia = equalsTokenWithTrivia.WithTrailingTrivia(Factory.Space, LineContinuation, VBEOLTrivia)
                            End If
                            Statements = Statements.Add(Factory.AssignmentStatement(VB.SyntaxKind.SimpleAssignmentStatement, ResultIdentifier, equalsTokenWithTrivia, right).WithTrailingEOL)
                            WhenClause = CType(e.Value.WhenClause?.Accept(Me), VBS.ExpressionSyntax)
                            CaseClause = Factory.SingletonSeparatedList(Of VBS.CaseClauseSyntax)(Factory.SimpleCaseClause(Factory.EqualsExpression(_tryCast, Factory.TrueLiteralExpression(TrueKeyword))))
                        ElseIf Pattern.Designation.IsKind(CS.SyntaxKind.DiscardDesignation) Then
                            Dim armExpression As VBS.ExpressionSyntax = CType(arm.Expression.Accept(Me), VBS.ExpressionSyntax)
                            Dim right As VBS.ExpressionSyntax = armExpression.AdjustExpressionTrivia(AdjustLeading:=True)
                            If right.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                equalsTokenWithTrivia = equalsTokenWithTrivia.WithTrailingTrivia(Factory.Space, LineContinuation, VBEOLTrivia)
                            End If
                            Statements = Statements.Insert(0, Factory.AssignmentStatement(VB.SyntaxKind.SimpleAssignmentStatement,
                                                                                                ResultIdentifier,
                                                                                                equalsTokenWithTrivia,
                                                                                                right
                                                                                               ).WithTrailingEOL)
                            CaseClause = Factory.SingletonSeparatedList(Of VBS.CaseClauseSyntax)(Factory.SimpleCaseClause(Factory.EqualsExpression(_tryCast, Factory.TrueLiteralExpression(TrueKeyword))))
                        Else
                            Throw UnexpectedValue(Pattern.Designation)
                        End If
                        CaseStatement = Factory.CaseStatement(CaseClause)
                    ElseIf TypeOf arm.Pattern Is CSS.VarPatternSyntax Then
                        Dim VarPattern As CSS.VarPatternSyntax = DirectCast(arm.Pattern, CSS.VarPatternSyntax)
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

                        Dim VariableName As VBS.ModifiedIdentifierSyntax = Factory.ModifiedIdentifier(Identifier.WithTrailingTrivia(Factory.Space))
                        Dim ExpressionOrThrow As VB.VisualBasicSyntaxNode = arm.Expression.Accept(Me)

                        Statements = Statements.Insert(0, FactoryDimStatement(Identifier,
                                                                                    asClause:=Nothing,
                                                                                    initializer:=Factory.EqualsValue(governingExpression)
                                                                                   ).WithTrailingEOL)
                        If Not e.IsLast Then
                            WhenClause = CType(e.Value.WhenClause?.Accept(Me), VBS.ExpressionSyntax)
                            CaseClause = Factory.SingletonSeparatedList(Of VBS.CaseClauseSyntax)(Factory.SimpleCaseClause(WhenClause))
                            CaseStatement = Factory.CaseStatement(CaseClause)
                        End If
                    ElseIf TypeOf arm.Pattern Is CSS.BinaryPatternSyntax Then
                        CaseClause = Factory.SingletonSeparatedList(Of VBS.CaseClauseSyntax)(Factory.SimpleCaseClause(CType(arm.Pattern.Accept(Me), VBS.ExpressionSyntax)))
                        CaseStatement = Factory.CaseStatement(CaseClause)
                    Else
                        Stop
                        Throw UnexpectedValue(NameOf(CSS.SwitchExpressionSyntax))
                    End If
                    If Not e.IsLast Then
                        If CaseStatement Is Nothing Then
                            Stop
                            Throw New Exception($"TypeOf arm.Pattern is {arm.Pattern.Kind}")
                        End If
                        Blocks = Blocks.Add(Factory.CaseBlock(CaseStatement.WithTrailingEOL, Statements))
                    Else
                        Blocks = Blocks.Add(Factory.CaseElseBlock(Factory.CaseElseStatement(Factory.ElseCaseClause).WithConvertedLeadingTriviaFrom(arm).WithTrailingEOL, Statements))
                    End If
                Next
                Dim EndSelectStatement As VBS.EndBlockStatementSyntax = Factory.EndBlockStatement(
                                                                    VB.SyntaxKind.EndSelectStatement,
                                                                    EndKeyword,
                                                                    SelectKeyword).
                                                                        WithConvertedTriviaFrom(node.CloseBraceToken)
                Dim stmt As VBS.SelectBlockSyntax = Factory.SelectBlock(SelectCaseStatement,
                                                                        Blocks,
                                                                        EndSelectStatement
                                                                        )
                StatementWithIssue.AddMarker(FactoryDimStatement(ResultNameToken,
                                                                 AsClause,
                                                                 initializer:=Nothing).WithTrailingEOL,
                                             StatementHandlingOption.PrependStatement,
                                             AllowDuplicates:=True)
                StatementWithIssue.AddMarker(stmt, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                Return ResultIdentifier
            End Function

            Public Overrides Function VisitVariableDeclaration(node As CSS.VariableDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim StatementWithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                Dim LeadingTrivia As SyntaxTriviaList = StatementWithIssue.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=True, "VB has no direct equivalent To C# var pattern expressions")
                Return MyBase.VisitVariableDeclaration(node)
            End Function

            Public Overrides Function VisitVariableDeclarator(node As CSS.VariableDeclaratorSyntax) As VB.VisualBasicSyntaxNode
                Dim Identifier As SyntaxToken = GenerateSafeVBToken(node.Identifier, node, _mSemanticModel)
                Dim ArgumentList As New List(Of VBS.ArgumentSyntax)
                If node.ArgumentList Is Nothing Then
                    Return Factory.ModifiedIdentifier(Identifier).WithTrailingTrivia(Factory.Space)
                Else
                    For Each e As IndexClass(Of CSS.ArgumentSyntax) In node.ArgumentList.Arguments.WithIndex
                        Dim vbExpression As VBS.ExpressionSyntax = CType(e.Value.Expression.Accept(Me), VBS.ExpressionSyntax)
                        If TypeOf vbExpression Is VBS.LiteralExpressionSyntax Then
                            Dim LiteralExpression As VBS.LiteralExpressionSyntax = CType(vbExpression, VBS.LiteralExpressionSyntax)
                            ArgumentList.Add(Factory.SimpleArgument(Factory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, Factory.Literal(CInt(LiteralExpression.Token.Value) - 1))).WithConvertedLeadingTriviaFrom(e.Value.Expression))
                        ElseIf TypeOf vbExpression Is VBS.IdentifierNameSyntax Then
                            Dim Id As VBS.IdentifierNameSyntax = CType(vbExpression, VBS.IdentifierNameSyntax)
                            ArgumentList.Add(Factory.SimpleArgument(Factory.BinaryExpression(VB.SyntaxKind.SubtractExpression, Id, MinusToken, ExpressionD1)).WithConvertedLeadingTriviaFrom(e.Value.Expression))
                        ElseIf TypeOf vbExpression Is VBS.BinaryExpressionSyntax Then
                            Dim Value As VBS.BinaryExpressionSyntax = CType(vbExpression, VBS.BinaryExpressionSyntax)
                            If vbExpression.IsKind(VB.SyntaxKind.AddExpression) Then
                                If Value.Right.IsKind(VB.SyntaxKind.NumericLiteralExpression) AndAlso CType(Value.Right, VBS.LiteralExpressionSyntax).Token.ValueText = "1" Then
                                    ArgumentList.Add(Factory.SimpleArgument(Value.Left.WithConvertedLeadingTriviaFrom(e.Value.Expression)))
                                    Continue For
                                End If
                            End If
                            ArgumentList.Add(Factory.SimpleArgument(Factory.BinaryExpression(VB.SyntaxKind.SubtractExpression, Value, MinusToken, ExpressionD1)).WithConvertedLeadingTriviaFrom(e.Value.Expression))
                        Else
                            Stop
                        End If
                    Next
                End If
                Dim Nullable As SyntaxToken = Nothing
                Dim ArrayBounds As VBS.ArgumentListSyntax = Factory.ArgumentList(Factory.SeparatedList(ArgumentList))
                Return Factory.ModifiedIdentifier(Identifier.WithTrailingTrivia(Factory.Space), Nullable, ArrayBounds, arrayRankSpecifiers:=Nothing)
            End Function

            Public Overrides Function VisitWhenClause(node As CSS.WhenClauseSyntax) As VB.VisualBasicSyntaxNode
                Return node.Condition.Accept(Me)
            End Function

        End Class

    End Class

End Namespace
