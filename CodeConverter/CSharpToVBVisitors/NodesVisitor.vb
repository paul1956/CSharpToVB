' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Diagnostics.CodeAnalysis
Imports System.Text
Imports Microsoft.CodeAnalysis
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic
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
            Private ReadOnly _originalRequest As ConvertRequest
            Private ReadOnly _placeholder As Integer = 1
            Private ReadOnly _reportException As Action(Of Exception)
            Private ReadOnly _semanticModel As SemanticModel
            Private _membersList As SyntaxList(Of VBS.StatementSyntax)
            Friend _usedIdentifiers As Dictionary(Of String, SymbolTableEntry)
            Public ReadOnly AllImports As New List(Of VBS.ImportsStatementSyntax)()

            Public ReadOnly DiscardHelperMarkers As New List(Of CSS.BaseTypeDeclarationSyntax)()
            Public ReadOnly InlineAssignHelperMarkers As New List(Of CSS.BaseTypeDeclarationSyntax)()
            'Public ReadOnly ByRefHelperMarkers As New List(Of CSS.BaseTypeDeclarationSyntax)()

            Public VBHeaderLeadingTrivia As SyntaxTriviaList

            Friend Sub New(originalRequest As ConvertRequest, lSemanticModel As SemanticModel, DefaultVBOptions As DefaultVBOptions, ReportException As Action(Of Exception))
                _semanticModel = lSemanticModel
                _reportException = ReportException
                _defaultVBOptions = DefaultVBOptions
                _commonConversions = New CommonConversions(lSemanticModel)
                _usedIdentifiers = New Dictionary(Of String, SymbolTableEntry)(StringComparer.Ordinal)
                _originalRequest = originalRequest
                Me.NeedEndUsings = 0
            End Sub

            Friend Property NeedEndUsings As Integer

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
                    If _originalRequest.CancelToken.IsCancellationRequested Then
                        Throw New OperationCanceledException
                    End If
                    [using].Accept(Me)
                Next
                Dim externList As New List(Of VB.VisualBasicSyntaxNode)
                ' externlist is potentially a list of empty lines with trivia
                For Each extern As CSS.ExternAliasDirectiveSyntax In node.Externs
                    externList.Add(extern.Accept(Me))
                Next

                Dim options As New SyntaxList(Of VBS.OptionStatementSyntax)
                With _defaultVBOptions
                    If .OptionCompareInclude Then
                        options = options.Add(Factory.OptionStatement(CompareToken, If(.OptionCompare = "Text", TextToken, BinaryToken)).WithTrailingEOL)
                    End If
                    If .OptionExplicitInclude Then
                        options = options.Add(Factory.OptionStatement(ExplicitToken, If(.OptionExplicit = "On", OnToken, OffToken)).WithTrailingEOL)
                    End If
                    If .OptionInferInclude Then
                        options = options.Add(Factory.OptionStatement(InferToken, If(.OptionInfer = "On", OnToken, OffToken)).WithTrailingEOL)

                    End If
                    If .OptionStrictInclude Then
                        options = options.Add(Factory.OptionStatement(StrictToken, If(.OptionStrict = "On", OnToken, OffToken)).WithTrailingEOL)
                    End If
                End With
                _membersList = New SyntaxList(Of VBS.StatementSyntax)
                For Each m As CSS.MemberDeclarationSyntax In node.Members
                    If _originalRequest.CancelToken.IsCancellationRequested Then
                        Throw New OperationCanceledException
                    End If
                    Dim statement As VBS.StatementSyntax = DirectCast(m.Accept(Me), VBS.StatementSyntax)
                    If statement Is Nothing Then
                        options = New SyntaxList(Of VBS.OptionStatementSyntax)
                    Else
                        _membersList = _membersList.AddRange(ReplaceOneStatementWithMarkedStatements(m, statement))
                    End If
                Next

                Dim listOfAttributes As List(Of VBS.AttributesStatementSyntax) = node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) Factory.AttributesStatement(Factory.SingletonList(DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))).ToList
                If AllImports.Any Then
                    If _membersList.Any AndAlso _membersList(0).HasLeadingTrivia Then
                        If (TypeOf _membersList(0) IsNot VBS.NamespaceBlockSyntax AndAlso
                                TypeOf _membersList(0) IsNot VBS.ModuleBlockSyntax) OrElse
                                _membersList(0).GetLeadingTrivia.ToFullString.Contains("auto-generated", StringComparison.OrdinalIgnoreCase) Then
                            Dim headingTriviaList As SyntaxTriviaList = _membersList(0).GetLeadingTrivia
                            If headingTriviaList(0).IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                headingTriviaList = headingTriviaList.RemoveAt(0)
                                If headingTriviaList.Any Then
                                    headingTriviaList = headingTriviaList.Add(VBEOLTrivia)
                                End If
                            End If
                            Dim newMemberList As New SyntaxList(Of VBS.StatementSyntax)
                            newMemberList = newMemberList.Add(_membersList(0).WithLeadingTrivia(Factory.Space))
                            _membersList = newMemberList.AddRange(_membersList.RemoveAt(0))
                            Dim newLeadingTrivia As New SyntaxTriviaList
                            ' Remove Leading whitespace
                            For Each t As SyntaxTrivia In headingTriviaList
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
                                            Dim isLast As Boolean = i = (AllImports.Count - 1)
                                            If Not isLast Then
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
                    If options.Any Then
                        If VBHeaderLeadingTrivia.Any Then
                            options = options.Replace(options.First, options.First.WithLeadingTrivia(VBHeaderLeadingTrivia.Add(VBEOLTrivia)))
                            If AllImports.Any AndAlso Not AllImports.First.GetLeadingTrivia.FirstOrDefault.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                options = options.Replace(options.Last, options.Last.WithAppendedEOL)
                            End If
                        End If
                    End If
                ElseIf options.Any Then
                    If listOfAttributes.Any AndAlso listOfAttributes(0).GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        options = options.Replace(options(0), options(0).WithLeadingTrivia(VBHeaderLeadingTrivia.Add(VBEOLTrivia)))
                        listOfAttributes(0) = listOfAttributes(0).WithUniqueLeadingTrivia(VBHeaderLeadingTrivia)
                        If Not listOfAttributes.First.GetLeadingTrivia.FirstOrDefault.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            options = options.Replace(options.Last, options.Last.WithAppendedEOL)
                        End If
                    ElseIf _membersList.Any AndAlso
                        VBHeaderLeadingTrivia.Any AndAlso
                        _membersList(0).GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then

                        options = options.Replace(options(0), options(0).WithLeadingTrivia(VBHeaderLeadingTrivia))
                        _membersList = _membersList.Replace(_membersList(0), _membersList(0).WithUniqueLeadingTrivia(VBHeaderLeadingTrivia))
                        If Not _membersList(0).GetLeadingTrivia.FirstOrDefault.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            options = options.Replace(options.Last, options.Last.WithAppendedEOL)
                        End If
                    Else
                        If VBHeaderLeadingTrivia.Any Then
                            options = options.Replace(options(0), options(0).WithLeadingTrivia(VBHeaderLeadingTrivia.Add(VBEOLTrivia)))
                        End If
                        options = options.Replace(options.Last, options.Last.WithAppendedEOL)
                    End If
                End If
                If HasMarkerError() Then
                    ' There are statements that were left out of translation
                    Throw New ApplicationException(GetMarkerErrorMessage)
                End If
                Dim compilationUnitSyntax1 As VBS.CompilationUnitSyntax
                If AllImports.Any OrElse _membersList.Any OrElse listOfAttributes.Any Then
                    compilationUnitSyntax1 = Factory.CompilationUnit(options,
                                             Factory.List(AllImports),
                                             Factory.List(listOfAttributes),
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
                Dim methodBodyVisitor As New MethodBodyVisitor(_semanticModel, Me)
                _membersList = _membersList.AddRange(node.Statement.Accept(methodBodyVisitor))
                Return Nothing
            End Function

            Public Overrides Function VisitImplicitElementAccess(node As CSS.ImplicitElementAccessSyntax) As VB.VisualBasicSyntaxNode
                If node.ArgumentList.Arguments.Count > 1 Then
                    Throw New NotSupportedException("ImplicitElementAccess can only have one argument!")
                End If
                Return node.ArgumentList.Arguments(0).Expression.Accept(Me).WithConvertedTriviaFrom(node.ArgumentList.Arguments(0).Expression)
            End Function

            Public Overrides Function VisitImplicitObjectCreationExpression(node As CSS.ImplicitObjectCreationExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim vbSyntaxNode As VB.VisualBasicSyntaxNode = node.Initializer?.Accept(Me)
                Dim attributeLists As SyntaxList(Of VBS.AttributeListSyntax) = Nothing
                Dim argumentList As VBS.ArgumentListSyntax = CType(node.ArgumentList.Accept(Me), VBS.ArgumentListSyntax)
                If TypeOf vbSyntaxNode Is VBS.ObjectCreationExpressionSyntax Then
                    Dim initializer As VBS.ObjectCreationInitializerSyntax = CType(vbSyntaxNode, VBS.ObjectCreationInitializerSyntax)
                    Return Factory.ObjectCreationExpression(attributeLists, PredefinedTypeObject, argumentList, initializer)
                ElseIf TypeOf vbSyntaxNode Is VBS.CollectionInitializerSyntax Then
                    Return vbSyntaxNode
                ElseIf TypeOf vbSyntaxNode Is VBS.ObjectMemberInitializerSyntax Then
                    '	Private Shared ReadOnly s_xmlSettings As New XmlReaderSettings() With {.DtdProcessing = DtdProcessing.Prohibit}
                    Dim initializer As VBS.ObjectMemberInitializerSyntax = CType(vbSyntaxNode, VBS.ObjectMemberInitializerSyntax)
                    Return Factory.ObjectCreationExpression(newKeyword:=NewKeyword, attributeLists, PredefinedTypeSByte, argumentList, initializer)

                    Stop
                ElseIf vbSyntaxNode Is Nothing Then
                    Return Factory.ObjectCreationExpression(attributeLists, PredefinedTypeObject, argumentList, Nothing)
                End If
                Throw UnreachableException
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
                Dim recordName As SyntaxToken = GenerateSafeVBToken(node.Identifier, node, _usedIdentifiers, _semanticModel)
                Dim recordTypeName As VBS.TypeSyntax = Factory.ParseTypeName(recordName.ToString)
                Dim members As New List(Of VBS.StatementSyntax)
                For Each m As CSS.MemberDeclarationSyntax In node.Members
                    Dim item As VBS.StatementSyntax = DirectCast(m.Accept(Me), VBS.StatementSyntax)
                    If item Is Nothing Then
                        members.Add(Factory.EmptyStatement.WithConvertedTriviaFrom(m))
                    Else
                        members.AddRange(ReplaceOneStatementWithMarkedStatements(m, item))
                    End If
                Next
                Dim parameterList As VBS.ParameterListSyntax = DirectCast(node.ParameterList?.Accept(Me), VBS.ParameterListSyntax)
                Dim constructorStatements As New SyntaxList(Of VBS.StatementSyntax)
                Dim elementList As New List(Of VBS.SimpleNameSyntax)
                For Each m As VBS.ParameterSyntax In parameterList.Parameters
                    Dim right As VBS.SimpleNameSyntax = Factory.IdentifierName(m.Identifier.Identifier)
                    elementList.Add(right)
                    Dim left As VBS.ExpressionSyntax = Factory.SimpleMemberAccessExpression(Factory.MeExpression, right)
                    members.Add(FactoryDimStatement(m.Identifier.Identifier, m.AsClause, initializer:=Nothing))
                    Dim assignmentStmt As VBS.AssignmentStatementSyntax = Factory.AssignmentStatement(VB.SyntaxKind.SimpleAssignmentStatement, left, EqualsToken, right)
                    constructorStatements = constructorStatements.Add(assignmentStmt)
                Next
                Dim subNewStatement As VBS.SubNewStatementSyntax =
                    Factory.SubNewStatement(attributeLists:=Nothing,
                                            modifiers:=Nothing,
                                            parameterList
                                           ).WithPrependedLeadingTrivia(CollectConvertedTokenTrivia(node.OpenBraceToken, GetLeading:=True, GetTrailing:=False)).WithTrailingEOL
                Dim newBlock As VBS.ConstructorBlockSyntax = Factory.ConstructorBlock(subNewStatement, constructorStatements)
                members.Add(newBlock)

                'Public Overrides Function Equals(anotherObject) As Boolean
                '    Dim anotherRecord = TryCast(anotherObject, Info)
                '    If anotherRecord Is Nothing Then Return False
                '    Return Equals(anotherRecord)
                'End Function
                Dim equalsIdentifierToken As SyntaxToken = Factory.Identifier("Equals")
                Dim asObject As VBS.SimpleAsClauseSyntax = Factory.SimpleAsClause(PredefinedTypeObject)
                Dim anotherObjectToken As SyntaxToken = Factory.Identifier("anotherObject")
                Dim anotherRecordAsObjectParam As VBS.ParameterSyntax = Factory.Parameter(attributeLists:=Nothing,
                                                                                          modifiers:=Nothing,
                                                                                          Factory.ModifiedIdentifier(anotherObjectToken),
                                                                                          asObject,
                                                                                          [default]:=Nothing
                                                                                         )
                Dim asBoolean As VBS.SimpleAsClauseSyntax = Factory.SimpleAsClause(PredefinedTypeBoolean)
                Dim statements As New SyntaxList(Of VBS.StatementSyntax)
                Dim equalsObjectFuncStmt As VBS.MethodStatementSyntax = Factory.FunctionStatement(attributeLists:=Nothing,
                                                                                                  PublicModifier.Add(OverridesKeyword),
                                                                                                  equalsIdentifierToken,
                                                                                                  typeParameterList:=Nothing,
                                                                                                  Factory.ParameterList(Factory.SingletonSeparatedList(anotherRecordAsObjectParam)),
                                                                                                  asBoolean,
                                                                                                  handlesClause:=Nothing,
                                                                                                  implementsClause:=Nothing
                                                                                                 )
                Dim equalsValue As VBS.EqualsValueSyntax = Factory.EqualsValue(Factory.TryCastExpression(Factory.IdentifierName(anotherObjectToken),
                                                                                                         recordTypeName
                                                                                                        )
                                                                              )
                Dim anotherRecordToken As SyntaxToken = Factory.Identifier("anotherRecord")
                statements = statements.Add(VisualBasicSyntaxFactory.FactoryDimStatement(anotherRecordToken,
                                                                                         asObject,
                                                                                         equalsValue
                                                                                        )
                                           )
                Dim returnFalse As VBS.StatementSyntax = Factory.ReturnStatement(Factory.FalseLiteralExpression(FalseKeyword))
                statements = statements.Add(Factory.SingleLineIfStatement(Factory.IsExpression(Factory.IdentifierName(anotherRecordToken), NothingExpression), Factory.SingletonList(returnFalse), elseClause:=Nothing))
                Dim argument As VBS.ArgumentSyntax = Factory.SimpleArgument(Factory.IdentifierName(anotherRecordToken))
                Dim arguments As SeparatedSyntaxList(Of VBS.ArgumentSyntax) = Factory.SingletonSeparatedList(Of VBS.ArgumentSyntax)(argument)
                Dim agrumentList As VBS.ArgumentListSyntax = Factory.ArgumentList(arguments)
                statements = statements.Add(Factory.ReturnStatement(Factory.InvocationExpression(Factory.IdentifierName(equalsIdentifierToken), agrumentList)))
                members.Add(Factory.FunctionBlock(equalsObjectFuncStmt, statements))

                'Public Overloads Function Equals(anotherRecord As {recordName}) As Boolean
                '    If Not X.Equals(anotherRecord.X) Then Return False
                '    If Not Y.Equals(anotherRecord.Y) Then Return False
                '    If Not Z.Equals(anotherRecord.Z) Then Return False
                '    Return True
                'End Function
                Dim asRecordName As VBS.SimpleAsClauseSyntax = Factory.SimpleAsClause(recordTypeName)
                Dim anotherRecordAsMeParam As VBS.ParameterSyntax = Factory.Parameter(attributeLists:=Nothing,
                                                                                      modifiers:=Nothing,
                                                                                      Factory.ModifiedIdentifier(anotherRecordToken),
                                                                                      asRecordName,
                                                                                      [default]:=Nothing
                                                                                     )
                Dim equalsAnotherRecordFuncStmt As VBS.MethodStatementSyntax = Factory.FunctionStatement(attributeLists:=Nothing,
                                                                                                         PublicModifier.Add(OverloadsKeyword),
                                                                                                         equalsIdentifierToken,
                                                                                                         typeParameterList:=Nothing,
                                                                                                         Factory.ParameterList(Factory.SingletonSeparatedList(anotherRecordAsMeParam)),
                                                                                                         asBoolean,
                                                                                                         handlesClause:=Nothing,
                                                                                                         implementsClause:=Nothing
                                                                                                        )

                statements = New SyntaxList(Of VBS.StatementSyntax)
                For Each e As VBS.SimpleNameSyntax In elementList
                    '   If Not X.Equals(anotherRecord.X) Then Return False
                    argument = Factory.SimpleArgument(Factory.MemberAccessExpression(VB.SyntaxKind.SimpleMemberAccessExpression, Factory.IdentifierName(anotherRecordToken), DotToken, e))
                    arguments = Factory.SingletonSeparatedList(Of VBS.ArgumentSyntax)(argument)
                    agrumentList = Factory.ArgumentList(arguments)
                    Dim operand As VBS.InvocationExpressionSyntax = Factory.InvocationExpression(Factory.SimpleMemberAccessExpression(e, Factory.IdentifierName(equalsIdentifierToken)), agrumentList)
                    Dim condition As VBS.ExpressionSyntax = Factory.NotExpression(NotKeyword, operand)
                    statements = statements.Add(Factory.SingleLineIfStatement(condition, Factory.SingletonList(returnFalse), elseClause:=Nothing))
                Next
                statements = statements.Add(Factory.ReturnStatement(Factory.TrueLiteralExpression(TrueKeyword)))

                members.Add(Factory.FunctionBlock(equalsAnotherRecordFuncStmt, statements))

                Dim listOfAttributes As SyntaxList(Of VBS.AttributeListSyntax) = Factory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))
                Dim typeParameterList As VBS.TypeParameterListSyntax = DirectCast(node.TypeParameterList?.Accept(Me), VBS.TypeParameterListSyntax)
                Dim modifiers As SyntaxTokenList = Factory.TokenList(ConvertModifiers(node.Modifiers, Me.IsModule, TokenContext.Struct))
                Dim classStmt As VBS.ClassStatementSyntax = DirectCast(Factory.ClassStatement(listOfAttributes,
                                                                                              Factory.TokenList(modifiers),
                                                                                              ClassKeyWord.WithConvertedTriviaFrom(node.Keyword),
                                                                                              recordName,
                                                                                              typeParameterList
                                                                                             ).RestructureAttributesAndModifiers(listOfAttributes.Any, modifiers.Any), VBS.ClassStatementSyntax).WithTrailingEOL

                Dim classBlock As VBS.ClassBlockSyntax = Factory.ClassBlock(classStmt,
                                                                            [inherits]:=Nothing,
                                                                            [implements]:=Nothing,
                                                                            members:=Factory.List(members),
                                                                            Factory.EndClassStatement(EndKeyword.WithTrailingTrivia(Factory.Space), ClassKeyWord).WithConvertedTriviaFrom(node.CloseBraceToken)
                                                                           )
                Dim errorModifiers As New List(Of String)
                For Each t As SyntaxToken In node.Modifiers
                    Select Case t.Text
                        Case "unsafe"
                            errorModifiers.Add("unsafe")
                        Case "ref"
                            errorModifiers.Add("ref")
                    End Select
                Next

                ' These errors are handled elsewhere just ignore
                ReplaceOneStatementWithMarkedStatements(node, classBlock)
                If errorModifiers.Any Then
                    classBlock = classBlock.WithPrependedLeadingTrivia(Factory.CommentTrivia($"' TODO TASK: VB has no direct equivalent to C# {String.Join(" or ", errorModifiers)} Structure"))
                End If
                classBlock = classBlock.WithPrependedLeadingTrivia(Factory.CommentTrivia($"' TODO TASK: VB has no direct equivalent to C# Records"))
                Return classBlock
            End Function

            Public Overrides Function VisitRefExpression(node As CSS.RefExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim statementwithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                statementwithIssue.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=True, "ref expression")
                Return node.Expression.Accept(Me)
            End Function

            Public Overrides Function VisitRefType(node As CSS.RefTypeSyntax) As VB.VisualBasicSyntaxNode
                Dim statementwithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                statementwithIssue.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=True, "ref type")
                Return node.Type.Accept(Me)
            End Function

            Public Overrides Function VisitRefTypeExpression(node As CSS.RefTypeExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim statementwithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                statementwithIssue.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=True, "ref type expression")
                Return node.Expression.Accept(Me)
            End Function

            Public Overrides Function VisitStackAllocArrayCreationExpression(node As CSS.StackAllocArrayCreationExpressionSyntax) As VB.VisualBasicSyntaxNode
                node.FirstAncestorOrSelf(Of CSS.StatementSyntax).AddMarker(FlagUnsupportedStatements(node.FirstAncestorOrSelf(Of CSS.StatementSyntax), "StackAlloc", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=True)
                Return PredefinedTypeObject
            End Function

            Public Overrides Function VisitSwitchExpression(node As CSS.SwitchExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim statementWithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                Dim governingExpression As VBS.ExpressionSyntax = CType(node.GoverningExpression.Accept(Me), VBS.ExpressionSyntax)
                Dim selectCaseStatement As VBS.SelectStatementSyntax = Factory.SelectStatement(governingExpression.WithLeadingTrivia(Factory.Space)).WithLeadingTrivia(governingExpression.GetLeadingTrivia)
                Dim resultNameToken As SyntaxToken = Factory.Identifier(node.GetUniqueVariableNameInScope("tempVar", _usedIdentifiers, _semanticModel))
                Dim resultIdentifier As VBS.IdentifierNameSyntax = Factory.IdentifierName(resultNameToken)
                Dim typeinf As TypeInfo = _semanticModel.GetTypeInfo(node.Arms(0).Expression)
                Dim asClause As VBS.AsClauseSyntax = Nothing
                If typeinf.Type IsNot Nothing AndAlso Not typeinf.Type.IsErrorType Then
                    If TypeOf typeinf.Type Is INamedTypeSymbol AndAlso typeinf.Type.IsTupleType Then
                        asClause = Factory.SimpleAsClause(typeinf.Type.ToString.ConvertCSStringToName.WithLeadingTrivia(Factory.Space))
                    Else
                        asClause = Factory.SimpleAsClause(typeinf.Type.ConvertToType.WithLeadingTrivia(Factory.Space))
                    End If
                End If

                Dim blocks As New SyntaxList(Of VBS.CaseBlockSyntax)
                Dim caseClause As SeparatedSyntaxList(Of VBS.CaseClauseSyntax)
                Dim statements As SyntaxList(Of VBS.StatementSyntax)
                Dim whenClause As VBS.ExpressionSyntax
                Dim caseStatement As VBS.CaseStatementSyntax = Nothing
                For Each e As IndexClass(Of CSS.SwitchExpressionArmSyntax) In node.Arms.WithIndex
                    Dim arm As CSS.SwitchExpressionArmSyntax = e.Value
                    Dim equalsTokenWithTrivia As SyntaxToken = EqualsToken
                    Dim vbNode As VB.VisualBasicSyntaxNode = arm.Expression.Accept(Me)
                    If vbNode.IsKind(VB.SyntaxKind.ThrowStatement) Then
                        statements = Factory.SingletonList(DirectCast(vbNode, VBS.StatementSyntax))
                    Else
                        Dim expr As VBS.ExpressionSyntax = CType(vbNode, VBS.ExpressionSyntax)
                        Dim savedLeadingTrivia As SyntaxTriviaList = expr.GetLeadingTrivia
                        expr = expr.WithLeadingTrivia(Factory.Space)
                        statements = Factory.SingletonList(Of VBS.StatementSyntax)(Factory.SimpleAssignmentStatement(resultIdentifier,
                                                                                                                     expr.WithTrailingEOL
                                                                                                                    ).WithLeadingTrivia(savedLeadingTrivia))
                    End If
                    If TypeOf arm.Pattern Is CSS.ConstantPatternSyntax Then
                        Dim constantPatternExpression As VBS.ExpressionSyntax = CType(arm.Pattern.Accept(Me), VBS.ExpressionSyntax)
                        Dim relationalCaseClause As VBS.RelationalCaseClauseSyntax = Factory.CaseEqualsClause(CType(arm.Pattern.Accept(Me), VBS.ExpressionSyntax).WithLeadingTrivia(Factory.Space))
                        caseStatement = Factory.CaseStatement(relationalCaseClause).WithPrependedLeadingTrivia(constantPatternExpression.GetLeadingTrivia)
                    ElseIf TypeOf arm.Pattern Is CSS.DiscardPatternSyntax Then
                        If arm.WhenClause IsNot Nothing Then
                            whenClause = CType(e.Value.WhenClause.Accept(Me), VBS.ExpressionSyntax)
                            caseClause = Factory.SingletonSeparatedList(Of VBS.CaseClauseSyntax)(Factory.SimpleCaseClause(whenClause))
                        Else
                            whenClause = Nothing
                        End If
                        caseStatement = Factory.CaseStatement(caseClause)
                    ElseIf TypeOf arm.Pattern Is CSS.RecursivePatternSyntax Then
                        statementWithIssue.AddMarker(FlagUnsupportedStatements(node, "Switch Expression with Recursive Pattern Syntax", CommentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, AllowDuplicates:=True)
                        Return resultIdentifier
                    ElseIf TypeOf arm.Pattern Is CSS.DeclarationPatternSyntax Then
                        Dim pattern As CSS.DeclarationPatternSyntax = DirectCast(arm.Pattern, CSS.DeclarationPatternSyntax)
                        Dim variableType As VBS.TypeSyntax = DirectCast(pattern.Type.Accept(Me), VBS.TypeSyntax).WithLeadingTrivia(Factory.Space)
                        Dim tryCastExpr As VBS.TryCastExpressionSyntax = Factory.TryCastExpression(governingExpression, variableType.WithLeadingTrivia(Factory.Space))
                        If pattern.Designation.IsKind(CS.SyntaxKind.SingleVariableDesignation) Then
                            Dim designation As CSS.SingleVariableDesignationSyntax = CType(pattern.Designation, CSS.SingleVariableDesignationSyntax)
                            Dim identifierToken As SyntaxToken = GenerateSafeVBToken(designation.Identifier, node, _usedIdentifiers, _semanticModel)
                            Dim initializer As VBS.EqualsValueSyntax = Factory.EqualsValue(Factory.DirectCastExpression(governingExpression, variableType))
                            statements = statements.Insert(0, FactoryDimStatement(identifierToken,
                                                                                        Factory.SimpleAsClause(variableType),
                                                                                        initializer).WithConvertedLeadingTriviaFrom(arm).WithTrailingEOL
                                                                                       )
                            Dim armExpression As VBS.ExpressionSyntax = CType(arm.Expression.Accept(Me), VBS.ExpressionSyntax)
                            Dim right As VBS.ExpressionSyntax = armExpression.AdjustExpressionTrivia(AdjustLeading:=True)
                            If right.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                equalsTokenWithTrivia = equalsTokenWithTrivia.WithTrailingTrivia(Factory.Space, LineContinuation, VBEOLTrivia)
                            End If
                            statements = statements.Add(Factory.AssignmentStatement(VB.SyntaxKind.SimpleAssignmentStatement, resultIdentifier, equalsTokenWithTrivia, right).WithTrailingEOL)
                            whenClause = CType(e.Value.WhenClause?.Accept(Me), VBS.ExpressionSyntax)
                            caseClause = Factory.SingletonSeparatedList(Of VBS.CaseClauseSyntax)(Factory.SimpleCaseClause(Factory.EqualsExpression(tryCastExpr, Factory.TrueLiteralExpression(TrueKeyword))))
                        ElseIf pattern.Designation.IsKind(CS.SyntaxKind.DiscardDesignation) Then
                            Dim armExpression As VBS.ExpressionSyntax = CType(arm.Expression.Accept(Me), VBS.ExpressionSyntax)
                            Dim right As VBS.ExpressionSyntax = armExpression.AdjustExpressionTrivia(AdjustLeading:=True)
                            If right.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                equalsTokenWithTrivia = equalsTokenWithTrivia.WithTrailingTrivia(Factory.Space, LineContinuation, VBEOLTrivia)
                            End If
                            statements = statements.Insert(0, Factory.AssignmentStatement(VB.SyntaxKind.SimpleAssignmentStatement,
                                                                                                resultIdentifier,
                                                                                                equalsTokenWithTrivia,
                                                                                                right
                                                                                               ).WithTrailingEOL)
                            caseClause = Factory.SingletonSeparatedList(Of VBS.CaseClauseSyntax)(Factory.SimpleCaseClause(Factory.EqualsExpression(tryCastExpr, Factory.TrueLiteralExpression(TrueKeyword))))
                        Else
                            Throw UnexpectedValue(pattern.Designation)
                        End If
                        caseStatement = Factory.CaseStatement(caseClause)
                    ElseIf TypeOf arm.Pattern Is CSS.VarPatternSyntax Then
                        Dim varPattern As CSS.VarPatternSyntax = DirectCast(arm.Pattern, CSS.VarPatternSyntax)
                        Dim identifier As SyntaxToken
                        If TypeOf varPattern.Designation Is CSS.SingleVariableDesignationSyntax Then
                            identifier = GenerateSafeVBToken(id:=DirectCast(varPattern.Designation, CSS.SingleVariableDesignationSyntax).Identifier, Node:=node, usedIdentifiers:=_usedIdentifiers, Model:=_semanticModel)
                        ElseIf TypeOf varPattern.Designation Is CSS.ParenthesizedVariableDesignationSyntax Then
                            Dim sBuilder As New StringBuilder
                            CreateDesignationName(ProcessVariableDesignation(CType(varPattern.Designation, CSS.ParenthesizedVariableDesignationSyntax)), sBuilder)
                            identifier = GenerateSafeVBToken(id:=CS.SyntaxFactory.Identifier(sBuilder.ToString), Node:=node, usedIdentifiers:=_usedIdentifiers, Model:=_semanticModel)
                        Else
                            Stop
                            _reportException?.Invoke(UnreachableException)
                        End If
                        Dim name As VBS.IdentifierNameSyntax = Factory.IdentifierName(identifier.ToString)

                        Dim variableName As VBS.ModifiedIdentifierSyntax = Factory.ModifiedIdentifier(identifier.WithTrailingTrivia(Factory.Space))
                        Dim expressionOrThrow As VB.VisualBasicSyntaxNode = arm.Expression.Accept(Me)

                        statements = statements.Insert(0, FactoryDimStatement(identifier,
                                                                                    asClause:=Nothing,
                                                                                    initializer:=Factory.EqualsValue(governingExpression)
                                                                                   ).WithTrailingEOL)
                        If Not e.IsLast Then
                            whenClause = CType(e.Value.WhenClause?.Accept(Me), VBS.ExpressionSyntax)
                            caseClause = Factory.SingletonSeparatedList(Of VBS.CaseClauseSyntax)(Factory.SimpleCaseClause(whenClause))
                            caseStatement = Factory.CaseStatement(caseClause)
                        End If
                    ElseIf TypeOf arm.Pattern Is CSS.BinaryPatternSyntax Then
                        caseClause = Factory.SingletonSeparatedList(Of VBS.CaseClauseSyntax)(Factory.SimpleCaseClause(CType(arm.Pattern.Accept(Me), VBS.ExpressionSyntax)))
                        caseStatement = Factory.CaseStatement(caseClause)
                    Else
                        Stop
                        Throw UnexpectedValue(NameOf(CSS.SwitchExpressionSyntax))
                    End If
                    If Not e.IsLast Then
                        If caseStatement Is Nothing Then
                            Stop
                            Throw New Exception($"TypeOf arm.Pattern is {arm.Pattern.Kind}")
                        End If
                        blocks = blocks.Add(Factory.CaseBlock(caseStatement.WithTrailingEOL, statements))
                    Else
                        blocks = blocks.Add(Factory.CaseElseBlock(Factory.CaseElseStatement(Factory.ElseCaseClause).WithConvertedLeadingTriviaFrom(arm).WithTrailingEOL, statements))
                    End If
                Next
                Dim endSelectStmt As VBS.EndBlockStatementSyntax = Factory.EndBlockStatement(
                                                                    VB.SyntaxKind.EndSelectStatement,
                                                                    EndKeyword,
                                                                    SelectKeyword).
                                                                        WithConvertedTriviaFrom(node.CloseBraceToken)
                Dim stmt As VBS.SelectBlockSyntax = Factory.SelectBlock(selectCaseStatement,
                                                                        blocks,
                                                                        endSelectStmt
                                                                        )
                statementWithIssue.AddMarker(FactoryDimStatement(resultNameToken,
                                                                 asClause,
                                                                 initializer:=Nothing).WithTrailingEOL,
                                             StatementHandlingOption.PrependStatement,
                                             AllowDuplicates:=True)
                statementWithIssue.AddMarker(stmt, StatementHandlingOption.PrependStatement, AllowDuplicates:=True)
                Return resultIdentifier
            End Function

            Public Overrides Function VisitVariableDeclaration(node As CSS.VariableDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim statementWithIssue As CS.CSharpSyntaxNode = GetStatementwithIssues(node)
                Dim leadingTrivia As SyntaxTriviaList = statementWithIssue.CheckCorrectnessLeadingTrivia(AttemptToPortMade:=True, "VB has no direct equivalent To C# var pattern expressions")
                Return MyBase.VisitVariableDeclaration(node)
            End Function

            Public Overrides Function VisitVariableDeclarator(node As CSS.VariableDeclaratorSyntax) As VB.VisualBasicSyntaxNode
                Dim identifier As SyntaxToken = GenerateSafeVBToken(node.Identifier, node, _usedIdentifiers, _semanticModel)
                Dim argumentList As New List(Of VBS.ArgumentSyntax)
                If node.ArgumentList Is Nothing Then
                    Return Factory.ModifiedIdentifier(identifier).WithTrailingTrivia(Factory.Space)
                Else
                    For Each e As IndexClass(Of CSS.ArgumentSyntax) In node.ArgumentList.Arguments.WithIndex
                        Dim vbExpression As VBS.ExpressionSyntax = CType(e.Value.Expression.Accept(Me), VBS.ExpressionSyntax)
                        If TypeOf vbExpression Is VBS.LiteralExpressionSyntax Then
                            Dim literalExpression As VBS.LiteralExpressionSyntax = CType(vbExpression, VBS.LiteralExpressionSyntax)
                            argumentList.Add(Factory.SimpleArgument(Factory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, Factory.Literal(CInt(literalExpression.Token.Value) - 1))).WithConvertedLeadingTriviaFrom(e.Value.Expression))
                        ElseIf TypeOf vbExpression Is VBS.IdentifierNameSyntax Then
                            Dim identName As VBS.IdentifierNameSyntax = CType(vbExpression, VBS.IdentifierNameSyntax)
                            argumentList.Add(Factory.SimpleArgument(Factory.BinaryExpression(VB.SyntaxKind.SubtractExpression, identName, MinusToken, rightExpr)).WithConvertedLeadingTriviaFrom(e.Value.Expression))
                        ElseIf TypeOf vbExpression Is VBS.BinaryExpressionSyntax Then
                            Dim value As VBS.BinaryExpressionSyntax = CType(vbExpression, VBS.BinaryExpressionSyntax)
                            If vbExpression.IsKind(VB.SyntaxKind.AddExpression) Then
                                If value.Right.IsKind(VB.SyntaxKind.NumericLiteralExpression) AndAlso CType(value.Right, VBS.LiteralExpressionSyntax).Token.ValueText = "1" Then
                                    argumentList.Add(Factory.SimpleArgument(value.Left.WithConvertedLeadingTriviaFrom(e.Value.Expression)))
                                    Continue For
                                End If
                            End If
                            argumentList.Add(Factory.SimpleArgument(Factory.BinaryExpression(VB.SyntaxKind.SubtractExpression, value, MinusToken, rightExpr)).WithConvertedLeadingTriviaFrom(e.Value.Expression))
                        Else
                            Stop
                        End If
                    Next
                End If
                Return Factory.ModifiedIdentifier(identifier.WithTrailingTrivia(Factory.Space),
                                                  nullable:=Nothing,
                                                  Factory.ArgumentList(Factory.SeparatedList(argumentList)),
                                                  arrayRankSpecifiers:=Nothing)
            End Function

            Public Overrides Function VisitWhenClause(node As CSS.WhenClauseSyntax) As VB.VisualBasicSyntaxNode
                Return node.Condition.Accept(Me)
            End Function

            Public Overrides Function VisitWithExpression(node As CSS.WithExpressionSyntax) As VB.VisualBasicSyntaxNode


                '' Write the value 4.
                'Console.WriteLine(CType(Function(x)
                '                            Return x + 2
                '                        End Function, Func(Of Integer, Integer))(2))

                Dim attributeLists As New SyntaxList(Of VBS.AttributeListSyntax)
                Dim modifiers As New SyntaxTokenList
                Dim statements As New SyntaxList(Of VBS.StatementSyntax)

                Dim identifier As VBS.ModifiedIdentifierSyntax = Factory.ModifiedIdentifier("_p1")
                Dim objectExpression As CS.CSharpSyntaxNode = node.Expression
                Dim recordType As VBS.TypeSyntax
                If TypeOf objectExpression Is CSS.ObjectCreationExpressionSyntax Then
                    recordType = CType(CType(objectExpression, CSS.ObjectCreationExpressionSyntax).Type.Accept(Me), VBS.TypeSyntax)
                ElseIf TypeOf objectExpression Is CSS.IdentifierNameSyntax Then
                    recordType = CType(CType(objectExpression, CSS.IdentifierNameSyntax).Accept(Me), VBS.TypeSyntax)
                Else
                    Stop
                    Throw UnreachableException
                End If
                Dim asClause As VBS.SimpleAsClauseSyntax = Factory.SimpleAsClause(recordType)
                Dim parameter As VBS.ParameterSyntax = Factory.Parameter(attributeLists, modifiers, identifier, asClause, Nothing)
                Dim parameters As SeparatedSyntaxList(Of VBS.ParameterSyntax)
                parameters = parameters.Add(parameter)
                Dim parameterList As VBS.ParameterListSyntax = Factory.ParameterList(parameters)
                Dim subOrFunctionHeader As VBS.LambdaHeaderSyntax = Factory.FunctionLambdaHeader(attributeLists, modifiers, parameterList, asClause)

                Dim p1 As VBS.SimpleNameSyntax = Factory.IdentifierName("_p1")
                Dim p2 As SyntaxToken = Factory.Identifier("_p2")
                Dim value As VBS.ExpressionSyntax = Factory.MemberAccessExpression(VB.SyntaxKind.SimpleMemberAccessExpression, p1, DotToken, Factory.IdentifierName("Clone"))
                Dim initializer As VBS.EqualsValueSyntax = Factory.EqualsValue(value)
                statements = statements.Add(FactoryDimStatement(p2, asClause, initializer))
                Dim withBlock As New SyntaxList(Of VBS.StatementSyntax)
                For Each expression As CSS.AssignmentExpressionSyntax In node.Initializer.Expressions
                    Dim vbExpression As VB.VisualBasicSyntaxNode = expression.Left.Accept(Me)
                    Dim left As VBS.MemberAccessExpressionSyntax
                    If TypeOf vbExpression Is VBS.MemberAccessExpressionSyntax Then
                        left = CType(expression.Left.Accept(Me), VBS.MemberAccessExpressionSyntax)
                    ElseIf TypeOf vbExpression Is VBS.NameSyntax Then
                        Dim name As VBS.SimpleNameSyntax = CType(expression.Left.Accept(Me), VBS.SimpleNameSyntax)
                        left = Factory.MemberAccessExpression(VB.SyntaxKind.SimpleMemberAccessExpression, DotToken, name)
                    Else
                        Throw UnreachableException
                    End If
                    Dim right As VBS.ExpressionSyntax = CType(expression.Right.Accept(Me), VBS.ExpressionSyntax)
                    withBlock = withBlock.Add(Factory.SimpleAssignmentStatement(left, right))
                Next

                Dim withStatement As VBS.WithStatementSyntax = Factory.WithStatement(Factory.IdentifierName(p2))
                statements = statements.Add(Factory.WithBlock(withStatement, withBlock))
                Dim endSubOrFunctionStatement As VBS.EndBlockStatementSyntax = Factory.EndFunctionStatement(EndKeyword.WithTrailingTrivia(Factory.Space), FunctionKeyword).WithConvertedTriviaFrom(node)

                Dim lambda As VBS.MultiLineLambdaExpressionSyntax = Factory.MultiLineFunctionLambdaExpression(subOrFunctionHeader, statements, endSubOrFunctionStatement)
                Dim typeArguments As New SeparatedSyntaxList(Of VBS.TypeSyntax)
                typeArguments = typeArguments.Add(p1)
                typeArguments = typeArguments.Add(p1)
                Dim typeArgumentList As VBS.TypeArgumentListSyntax = Factory.TypeArgumentList(typeArguments)
                Dim genericName As VBS.GenericNameSyntax = Factory.GenericName("Func", typeArgumentList)
                Dim lambdaArgument As SeparatedSyntaxList(Of VBS.ArgumentSyntax)
                lambdaArgument = lambdaArgument.Add(Factory.SimpleArgument(CType(node.Expression.Accept(Me), VBS.ExpressionSyntax)))
                Dim lambdaArgumentList As VBS.ArgumentListSyntax = Factory.ArgumentList(lambdaArgument)
                Return Factory.InvocationExpression(Factory.CTypeExpression(lambda, genericName), lambdaArgumentList)
            End Function
        End Class

    End Class

End Namespace
