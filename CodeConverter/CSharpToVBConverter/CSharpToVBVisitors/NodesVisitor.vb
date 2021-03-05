' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Diagnostics.CodeAnalysis
Imports System.Text
Imports Extensions
Imports Microsoft.CodeAnalysis
Imports SupportClasses
Imports Utilities
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBConverter.CSharpToVBVisitors

    Partial Public Class CSharpConverter

        Partial Friend Class NodesVisitor
            Inherits CS.CSharpSyntaxVisitor(Of VB.VisualBasicSyntaxNode)

            ' This file contains all the stuff accessed by multiple Visitor functions in Class NodeVisitor and Visitors that
            ' had no better home.

            Private ReadOnly _commonConversions As CommonConversions
            ' ReSharper disable InconsistentNaming
            Private ReadOnly _defaultVBOptions As DefaultVbOptions
            Private ReadOnly _isModuleStack As New Stack(Of Boolean)
            Private ReadOnly _originalRequest As ConvertRequest
            Private ReadOnly _placeholder As Integer = 1
            Private ReadOnly _reportException As Action(Of Exception)
            Private ReadOnly _semanticModel As SemanticModel
            Private _membersList As SyntaxList(Of VBS.StatementSyntax)
            Friend _usedIdentifiers As Dictionary(Of String, SymbolTableEntry)
            ' ReSharper restore InconsistentNaming
            Public ReadOnly s_allImports As New List(Of VBS.ImportsStatementSyntax)()

            Public ReadOnly s_discardHelperMarkers As New List(Of CSS.BaseTypeDeclarationSyntax)()
            Public ReadOnly s_inlineAssignHelperMarkers As New List(Of CSS.BaseTypeDeclarationSyntax)()
            'Public ReadOnly ByRefHelperMarkers As New List(Of CSS.BaseTypeDeclarationSyntax)()

            Public s_vbHeaderLeadingTrivia As SyntaxTriviaList

            Friend Sub New(originalRequest As ConvertRequest, lSemanticModel As SemanticModel, defaultVbOptions As DefaultVbOptions, reportException As Action(Of Exception))
                _semanticModel = lSemanticModel
                _reportException = reportException
                _defaultVBOptions = defaultVbOptions
                _commonConversions = New CommonConversions(lSemanticModel)
                _usedIdentifiers = New Dictionary(Of String, SymbolTableEntry)(StringComparer.Ordinal)
                _originalRequest = originalRequest
                Me.NeededEndUsingCount = 0
            End Sub

            Friend Property NeededEndUsingCount As Integer

            Public ReadOnly Property IsModule As Boolean
                Get
                    If _isModuleStack.Count = 0 Then
                        Return False
                    End If
                    Return _isModuleStack.Peek
                End Get
            End Property

            Private Shared Function ContainsProperty(propertyList As List(Of (Type As VBS.TypeSyntax, ArgumentNames As List(Of String))), name As String) As Boolean
                For Each e As (Type As VBS.TypeSyntax, ArgumentNames As List(Of String)) In propertyList
                    For Each argumentName As String In e.ArgumentNames
                        If argumentName = name Then
                            Return True
                        End If
                    Next
                Next
                Return False
            End Function

            Private Shared Function GetArgsFromInheritedClass(propertyList As List(Of (Type As VBS.TypeSyntax, ArgumentNames As List(Of String)))) As SeparatedSyntaxList(Of VBS.ArgumentSyntax)
                Dim argumentList As New SeparatedSyntaxList(Of VBS.ArgumentSyntax)
                For Each e As (Type As VBS.TypeSyntax, ArgumentNames As List(Of String)) In propertyList
                    For Each argumentName As String In e.ArgumentNames
                        Dim argument As VBS.IdentifierNameSyntax = Factory.IdentifierName(argumentName)
                        argumentList = argumentList.Add(Factory.SimpleArgument(Factory.NameColonEquals(argument), argument))
                    Next
                Next
                Return argumentList
            End Function

            Private Shared Function IsInvokeIdentifier(sns As CSS.SimpleNameSyntax) As Boolean
                Return sns.Identifier.Value.Equals("Invoke")
            End Function

            Private Sub ConvertRecordBaseList(baseType As CSS.BaseTypeDeclarationSyntax, [inherits] As List(Of VBS.InheritsStatementSyntax), ByRef movedFinalTrivia As SyntaxTriviaList, ByRef implementedMembers As List(Of (Type As VBS.TypeSyntax, ArgumentNames As List(Of String))))
                Dim recordType As CSS.TypeSyntax = baseType.BaseList?.Types.FirstOrDefault()?.Type
                If recordType Is Nothing Then
                    Exit Sub
                End If
                Dim typeNode As VB.VisualBasicSyntaxNode = recordType.Accept(Me)
                If typeNode.HasLeadingTrivia Then
                    Dim typeNodeLeadingTrivia As SyntaxTriviaList = typeNode.GetLeadingTrivia
                    If typeNodeLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        [inherits].Add(Factory.InheritsStatement(DirectCast(typeNode.WithLeadingTrivia(SpaceTrivia), VBS.TypeSyntax)).WithLeadingTrivia(typeNodeLeadingTrivia))
                    Else
                        [inherits].Add(Factory.InheritsStatement(DirectCast(typeNode, VBS.TypeSyntax)))
                    End If
                Else
                    [inherits].Add(Factory.InheritsStatement(DirectCast(typeNode, VBS.TypeSyntax)))
                End If
                Dim separatorList As List(Of SyntaxToken) = baseType.BaseList.Types.GetSeparators.ToList
                Dim argumentNames As New List(Of String)
                For Each e As IndexClass(Of CSS.BaseTypeSyntax) In baseType.BaseList.Types.WithIndex
                    If e.index < separatorList.Count Then
                        FilterLeadingTrivia(separatorList(e.index).LeadingTrivia.ConvertTriviaList(), movedFinalTrivia)
                    End If
                    For Each name As VBS.ArgumentSyntax In CType(CType(e.Value, CSS.PrimaryConstructorBaseTypeSyntax).ArgumentList.Accept(Me), VBS.ArgumentListSyntax).Arguments
                        argumentNames.Add(name.ToString)
                    Next
                    implementedMembers.Add((DirectCast(e.Value.Type.Accept(Me), VBS.TypeSyntax), argumentNames))
                Next
                If [inherits].Any Then
                    [inherits]([inherits].Count - 1) = [inherits].Last.WithTrailingEol
                End If
            End Sub

            Private Function GetIdentifierNameFromName(expression As VBS.ExpressionSyntax) As VBS.IdentifierNameSyntax
                Select Case True
                    Case TypeOf expression Is VBS.IdentifierNameSyntax
                        Return DirectCast(expression, VBS.IdentifierNameSyntax)
                    Case TypeOf expression Is VBS.MemberAccessExpressionSyntax
                        Dim memberAccess As VBS.MemberAccessExpressionSyntax = DirectCast(expression, VBS.MemberAccessExpressionSyntax)
                        Return Me.GetIdentifierNameFromName(memberAccess.Name)
                    Case Else
                        Throw New NotSupportedException($"Cannot get SimpleNameSyntax from {expression.Kind()}:" & vbCrLf & "{expressionSyntax}")
                End Select
            End Function

            <ExcludeFromCodeCoverage>
            Public Overrides Function DefaultVisit(node As SyntaxNode) As VB.VisualBasicSyntaxNode
                Throw New NotImplementedException(node.GetType().ToString & " not implemented!")
            End Function

            Public Overrides Function VisitCompilationUnit(node As CSS.CompilationUnitSyntax) As VB.VisualBasicSyntaxNode
                If node.GetLeadingTrivia.FirstOrDefault.IsKind(CS.SyntaxKind.SingleLineCommentTrivia) Then
                    s_vbHeaderLeadingTrivia = node.GetLeadingTrivia.GetDocumentBanner
                End If
                For Each [using] As CSS.UsingDirectiveSyntax In node.Usings
                    If _originalRequest.CancelToken.IsCancellationRequested Then
                        Throw New OperationCanceledException
                    End If
                    [using].Accept(Me)
                Next
                Dim externList As New List(Of VB.VisualBasicSyntaxNode)
                ' externList is potentially a list of empty lines with trivia
                For Each extern As CSS.ExternAliasDirectiveSyntax In node.Externs
                    externList.Add(extern.Accept(Me))
                Next

                Dim options As New SyntaxList(Of VBS.OptionStatementSyntax)
                With _defaultVBOptions
                    If .OptionCompareInclude Then
                        options = options.Add(Factory.OptionStatement(CompareToken, If(.OptionCompare = "Text", TextToken, BinaryToken)).WithTrailingEol)
                    End If
                    If .OptionExplicitInclude Then
                        options = options.Add(Factory.OptionStatement(ExplicitToken, If(.OptionExplicit = "On", OnToken, OffToken)).WithTrailingEol)
                    End If
                    If .OptionInferInclude Then
                        options = options.Add(Factory.OptionStatement(InferToken, If(.OptionInfer = "On", OnToken, OffToken)).WithTrailingEol)

                    End If
                    If .OptionStrictInclude Then
                        options = options.Add(Factory.OptionStatement(StrictToken, If(.OptionStrict = "On", OnToken, OffToken)).WithTrailingEol)
                    End If
                End With
                If options.Any Then
                    options = options.Replace(options(0), options(0).WithLeadingTrivia(Factory.CommentTrivia("' To configure or remove Option's included in result, go to Options/Advanced Options..."), VbEolTrivia))
                End If
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
                If s_allImports.Any Then
                    If _membersList.Any AndAlso _membersList(0).HasLeadingTrivia Then
                        If (TypeOf _membersList(0) IsNot VBS.NamespaceBlockSyntax AndAlso
                                TypeOf _membersList(0) IsNot VBS.ModuleBlockSyntax) OrElse
                                _membersList(0).GetLeadingTrivia.ToFullString.Contains("auto-generated", StringComparison.OrdinalIgnoreCase) Then
                            Dim headingTriviaList As SyntaxTriviaList = _membersList(0).GetLeadingTrivia
                            If headingTriviaList(0).IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                headingTriviaList = headingTriviaList.RemoveAt(0)
                                If headingTriviaList.Any Then
                                    headingTriviaList = headingTriviaList.Add(VbEolTrivia)
                                End If
                            End If
                            Dim newMemberList As New SyntaxList(Of VBS.StatementSyntax)
                            newMemberList = newMemberList.Add(_membersList(0).WithLeadingTrivia(SpaceTrivia))
                            _membersList = newMemberList.AddRange(_membersList.RemoveAt(0))
                            Dim newLeadingTrivia As New SyntaxTriviaList
                            ' Remove Leading whitespace
                            For Each t As SyntaxTrivia In headingTriviaList
                                If Not t.IsWhitespaceOrEndOfLine Then
                                    newLeadingTrivia = newLeadingTrivia.Add(t)
                                End If
                            Next
                            s_allImports(0) = s_allImports(0).WithPrependedLeadingTrivia(newLeadingTrivia)
                        End If
                    End If
                    For i As Integer = 0 To s_allImports.Count - 1
                        Dim importsClause As VBS.ImportsClauseSyntax = s_allImports(i).ImportsClauses.FirstOrDefault
                        Dim simpleImportsClause As VBS.SimpleImportsClauseSyntax = TryCast(importsClause, VBS.SimpleImportsClauseSyntax)
                        If simpleImportsClause IsNot Nothing Then
                            Dim identifierName As VBS.IdentifierNameSyntax = TryCast(simpleImportsClause.Name, VBS.IdentifierNameSyntax)
                            If identifierName IsNot Nothing Then
                                If i = 0 Then
                                    Select Case identifierName.Identifier.ValueText
                                        Case "System"
                                            Dim isLast As Boolean = i = (s_allImports.Count - 1)
                                            If Not isLast Then
                                                s_allImports(i + 1) = s_allImports(i + 1).WithMergedLeadingTrivia(s_allImports(i).GetLeadingTrivia)
                                            End If
                                            s_allImports.RemoveAt(i)
                                            Exit For
                                        Case CompilerServices, InteropServices
                                            s_allImports(0) = s_allImports(0).WithLeadingTrivia(node.GetLeadingTrivia.ConvertTriviaList).WithUniqueLeadingTrivia(s_vbHeaderLeadingTrivia)
                                            If _membersList.Any Then
                                                _membersList = _membersList.Replace(_membersList(0), _membersList(0).WithUniqueLeadingTrivia(s_allImports(0).GetLeadingTrivia))
                                            End If
                                    End Select
                                End If
                            End If
                        End If
                    Next
                    If options.Any Then
                        If s_vbHeaderLeadingTrivia.Any Then
                            options = options.Replace(options.First, options.First.WithLeadingTrivia(s_vbHeaderLeadingTrivia.Add(VbEolTrivia)))

                            If s_allImports.Any AndAlso Not s_allImports(0).GetLeadingTrivia.FirstOrDefault.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                options = options.Replace(options.Last, options.Last.WithAppendedEol)
                            End If
                        End If
                    End If
                ElseIf options.Any Then
                    If listOfAttributes.Any AndAlso listOfAttributes(0).GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        options = options.Replace(options(0), options(0).WithLeadingTrivia(s_vbHeaderLeadingTrivia.Add(VbEolTrivia)))
                        listOfAttributes(0) = listOfAttributes(0).WithUniqueLeadingTrivia(s_vbHeaderLeadingTrivia)

                        If Not listOfAttributes(0).GetLeadingTrivia.FirstOrDefault.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            options = options.Replace(options.Last, options.Last.WithAppendedEol)
                        End If
                    ElseIf _membersList.Any AndAlso
                        s_vbHeaderLeadingTrivia.Any AndAlso
                        _membersList(0).GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then

                        options = options.Replace(options(0), options(0).WithLeadingTrivia(s_vbHeaderLeadingTrivia))
                        _membersList = _membersList.Replace(_membersList(0), _membersList(0).WithUniqueLeadingTrivia(s_vbHeaderLeadingTrivia))

                        If Not _membersList(0).GetLeadingTrivia.FirstOrDefault.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                            options = options.Replace(options.Last, options.Last.WithAppendedEol)
                        End If
                    Else
                        If s_vbHeaderLeadingTrivia.Any Then
                            options = options.Replace(options(0), options(0).WithLeadingTrivia(s_vbHeaderLeadingTrivia.Add(VbEolTrivia)))
                        End If
                        options = options.Replace(options.Last, options.Last.WithAppendedEol)
                    End If
                End If
                If HasMarkerError() Then
                    ' There are statements that were left out of translation
                    Throw New ApplicationException(GetMarkerErrorMessage)
                End If
                Dim compilationUnitSyntax1 As VBS.CompilationUnitSyntax
                If s_allImports.Any OrElse _membersList.Any OrElse listOfAttributes.Any Then
                    compilationUnitSyntax1 = Factory.CompilationUnit(options,
                                             Factory.List(s_allImports),
                                             Factory.List(listOfAttributes),
                                             _membersList,
                                             EndOfFileToken.WithConvertedTriviaFrom(node.EndOfFileToken))
                Else
                    _membersList = _membersList.Add(Factory.EmptyStatement.WithLeadingTrivia(s_vbHeaderLeadingTrivia))
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
                        Dim memberInitializer As VBS.ObjectMemberInitializerSyntax = DirectCast(possibleInitializer, VBS.ObjectMemberInitializerSyntax)
                        Return Factory.ObjectCreationExpression(NewKeyword, Factory.List(Of VBS.AttributeListSyntax)(), PredefinedTypeObject, argumentList, memberInitializer)

                    Case Else
                        _reportException?.Invoke(UnexpectedValue(NameOf(possibleInitializer)))
                End Select
                Return Factory.ObjectCreationExpression(NewKeyword, attributeLists:=Nothing, PredefinedTypeObject, argumentList:=Nothing, initializer)
            End Function

            Public Overrides Function VisitMakeRefExpression(node As CSS.MakeRefExpressionSyntax) As VB.VisualBasicSyntaxNode
                GetStatementWithIssues(node).AddMarker(FlagUnsupportedStatements(GetStatementWithIssues(node),
                                                                                 "MakeRef Expressions",
                                                                                 commentOutOriginalStatements:=False),
                                                       StatementHandlingOption.PrependStatement,
                                                       allowDuplicates:=True)
                Return Factory.InvocationExpression(Factory.ParseExpression($"__makeref({node.Expression.Accept(Me).ToFullString})").WithConvertedTriviaFrom(node))
            End Function

            Public Overrides Function VisitOmittedArraySizeExpression(node As CSS.OmittedArraySizeExpressionSyntax) As VB.VisualBasicSyntaxNode
                Return Factory.OmittedArgument()
            End Function

            Public Overrides Function VisitRecordDeclaration(node As CSS.RecordDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim recordName As SyntaxToken = GenerateSafeVbToken(node.Identifier, node, _semanticModel, _usedIdentifiers)
                Dim recordTypeName As VBS.TypeSyntax = Factory.ParseTypeName(recordName.ToString)
                Dim asRecordName As VBS.SimpleAsClauseSyntax = Factory.SimpleAsClause(recordTypeName)

                Dim members As New List(Of VBS.StatementSyntax)
                Dim [inherits] As New List(Of VBS.InheritsStatementSyntax)()

                Dim finalTrailingTrivia As SyntaxTriviaList
                Dim implementedMembers As New List(Of (Type As VBS.TypeSyntax, ArgumentNames As List(Of String)))
                Me.ConvertRecordBaseList(node, [inherits], finalTrailingTrivia, implementedMembers)
                For Each m As CSS.MemberDeclarationSyntax In node.Members
                    Dim item As VBS.StatementSyntax = DirectCast(m.Accept(Me), VBS.StatementSyntax)
                    If item Is Nothing Then
                        members.Add(Factory.EmptyStatement.WithConvertedTriviaFrom(m))
                    Else
                        members.AddRange(ReplaceOneStatementWithMarkedStatements(m, item))
                    End If
                Next

                Dim constructorStatements As New SyntaxList(Of VBS.StatementSyntax)
                If implementedMembers.Any Then
                    Dim myBaseNewExpression As VBS.InvocationExpressionSyntax = Factory.InvocationExpression(Factory.SimpleMemberAccessExpression(MyBaseExpression,
                                                                                                                                                  Factory.IdentifierName("New")),
                                                                                                             Factory.ArgumentList(GetArgsFromInheritedClass(implementedMembers)))
                    constructorStatements = constructorStatements.Add(Factory.ExpressionStatement(myBaseNewExpression))
                End If
                Dim fullElementList As New List(Of VBS.SimpleNameSyntax)
                Dim parameterList As VBS.ParameterListSyntax = DirectCast(node.ParameterList?.Accept(Me), VBS.ParameterListSyntax)
                If parameterList IsNot Nothing Then
                    For Each e As IndexClass(Of VBS.ParameterSyntax) In parameterList.Parameters.WithIndex
                        Dim assignmentStmt As VBS.AssignmentStatementSyntax
                        Dim left As VBS.ExpressionSyntax
                        Dim right As VBS.SimpleNameSyntax = Factory.IdentifierName(e.Value.Identifier.Identifier)
                        If Not ContainsProperty(implementedMembers, right.Identifier.ToString) Then
                            left = Factory.SimpleMemberAccessExpression(MeExpression, right)

                            Dim propertyStatement As VBS.StatementSyntax = Factory.PropertyStatement(Nothing, PublicModifier, e.Value.Identifier.Identifier, Nothing, e.Value.AsClause, initializer:=Nothing, implementsClause:=Nothing).RemoveExtraLeadingEol
                            members.Add(propertyStatement)
                            assignmentStmt = Factory.AssignmentStatement(VB.SyntaxKind.SimpleAssignmentStatement, left, EqualsToken, right)
                            constructorStatements = constructorStatements.Add(assignmentStmt)
                        End If
                        fullElementList.Add(right)
                    Next
                End If
                members.Add(Factory.ConstructorBlock(Factory.SubNewStatement(attributeLists:=Nothing,
                                                 modifiers:=Nothing,
                                                 parameterList
                                                ).WithPrependedLeadingTrivia(node.OpenBraceToken.CollectConvertedTokenTrivia(getLeading:=True, getTrailing:=False)).WithTrailingEol(), constructorStatements))

                ' Finalize Block
                Dim myBaseInvocationExpr As VBS.InvocationExpressionSyntax
                myBaseInvocationExpr = Factory.InvocationExpression(Factory.MemberAccessExpression(VB.SyntaxKind.SimpleMemberAccessExpression,
                                                                                                   MyBaseExpression,
                                                                                                   DotToken,
                                                                                                   Factory.IdentifierName(FinalizeToken)),
                                                                    Factory.ArgumentList())
                members.Add(Factory.SubBlock(Factory.SubStatement(Nothing,
                                                                  ProtectedModifier.Add(OverridesKeyword),
                                                                  FinalizeToken,
                                                                  Nothing,
                                                                  Factory.ParameterList(),
                                                                  Nothing,
                                                                  Nothing,
                                                                  Nothing),
                                             Factory.SingletonList(Of VBS.StatementSyntax)(Factory.ExpressionStatement(myBaseInvocationExpr))))
                If Not implementedMembers.Any Then
                    ' Function Clone
                    Dim cloneFuncStmt As VBS.MethodStatementSyntax = Factory.FunctionStatement(attributeLists:=Nothing,
                                                                                           FriendModifier,
                                                                                           CloneIdentifier.Identifier,
                                                                                           typeParameterList:=Nothing,
                                                                                           Factory.ParameterList(),
                                                                                           Factory.SimpleAsClause(recordTypeName),
                                                                                           handlesClause:=Nothing,
                                                                                           implementsClause:=Nothing
                                                                                          )

                    ' DirectCast(Me.MemberwiseClone(), Point)
                    Dim cloneStatement As SyntaxList(Of VBS.StatementSyntax) = Factory.SingletonList(Of VBS.StatementSyntax)(
                        Factory.ReturnStatement(Factory.DirectCastExpression(Factory.SimpleMemberAccessExpression(MeExpression, MemberwiseCloneIdentifier), recordTypeName))
                        )
                    members.Add(Factory.FunctionBlock(cloneFuncStmt, cloneStatement))
                End If

                'Public Overrides Function Equals(anotherObject) As Boolean
                '    Dim anotherRecord = TryCast(anotherObject, Info)
                '    If anotherRecord Is Nothing Then Return False
                '    Return Me.Equals(anotherRecord)
                'End Function
                Dim anotherRecordAsObjectParam As VBS.ParameterSyntax = Factory.Parameter(attributeLists:=Nothing,
                                                                                          modifiers:=Nothing,
                                                                                          Factory.ModifiedIdentifier(Factory.Identifier("anotherObject")),
                                                                                          AsObject,
                                                                                          [default]:=Nothing
                                                                                         )
                Dim statements As New SyntaxList(Of VBS.StatementSyntax)

                ' Public Overrides Function Equals(anotherObject) As Boolean
                Dim equalsObjectFuncStmt As VBS.MethodStatementSyntax = Factory.FunctionStatement(attributeLists:=Nothing,
                                                                                                  PublicModifier.Add(OverridesKeyword),
                                                                                                  EqualsIdentifierToken,
                                                                                                  typeParameterList:=Nothing,
                                                                                                  Factory.ParameterList(Factory.SingletonSeparatedList(anotherRecordAsObjectParam)),
                                                                                                  Factory.SimpleAsClause(PredefinedTypeBoolean),
                                                                                                  handlesClause:=Nothing,
                                                                                                  implementsClause:=Nothing
                                                                                                 )
                Dim equalsValue As VBS.EqualsValueSyntax = Factory.EqualsValue(Factory.TryCastExpression(Factory.IdentifierName(Factory.Identifier("anotherObject")),
                                                                                                         recordTypeName
                                                                                                        )
                                                                              )
                Dim anotherRecordToken As SyntaxToken = Factory.Identifier("anotherRecord")
                statements = statements.Add(FactoryDimStatement(anotherRecordToken,
                                                                AsObject,
                                                                equalsValue
                                                                )
                                           )
                Dim returnFalse As VBS.StatementSyntax = Factory.ReturnStatement(Factory.FalseLiteralExpression(FalseKeyword))
                statements = statements.Add(Factory.SingleLineIfStatement(Factory.IsExpression(Factory.IdentifierName(anotherRecordToken), NothingExpression), Factory.SingletonList(returnFalse), elseClause:=Nothing))
                Dim argument As VBS.ArgumentSyntax = Factory.SimpleArgument(Factory.IdentifierName(anotherRecordToken))
                Dim arguments As SeparatedSyntaxList(Of VBS.ArgumentSyntax) = Factory.SingletonSeparatedList(argument)
                Dim argumentList As VBS.ArgumentListSyntax = Factory.ArgumentList(arguments)
                Dim simpleMemberAccess As VBS.MemberAccessExpressionSyntax = Factory.SimpleMemberAccessExpression(MeExpression, Factory.IdentifierName(EqualsIdentifierToken))
                Dim invocationExpression As VBS.InvocationExpressionSyntax = Factory.InvocationExpression(simpleMemberAccess, argumentList)
                statements = statements.Add(Factory.ReturnStatement(invocationExpression))
                members.Add(Factory.FunctionBlock(equalsObjectFuncStmt, statements))

                'Public Overloads Function Equals(anotherRecord As {recordName}) As Boolean
                '    If Not MyBase.X.Equals(anotherRecord.X) Then Return False
                '    If Not MyBase.Y.Equals(anotherRecord.Y) Then Return False
                '    If Not Me.Z.Equals(anotherRecord.Z) Then Return False
                '    Return True
                'End Function
                Dim anotherRecordAsMeParam As VBS.ParameterSyntax = Factory.Parameter(attributeLists:=Nothing,
                                                                                      modifiers:=Nothing,
                                                                                      Factory.ModifiedIdentifier(anotherRecordToken),
                                                                                      asRecordName,
                                                                                      [default]:=Nothing
                                                                                     )
                statements = New SyntaxList(Of VBS.StatementSyntax)
                Dim equalsAnotherRecordFuncStmt As VBS.MethodStatementSyntax = Factory.FunctionStatement(attributeLists:=Nothing,
                                                                                                         PublicModifier.Add(OverloadsKeyword),
                                                                                                         EqualsIdentifierToken,
                                                                                                         typeParameterList:=Nothing,
                                                                                                         Factory.ParameterList(Factory.SingletonSeparatedList(anotherRecordAsMeParam)),
                                                                                                         Factory.SimpleAsClause(PredefinedTypeBoolean),
                                                                                                         handlesClause:=Nothing,
                                                                                                         implementsClause:=Nothing
                                                                                                        )

                For Each e As VBS.SimpleNameSyntax In fullElementList
                    '   If Not Me.X.Equals(anotherRecord.X) Then Return False

                    Dim operand As VBS.InvocationExpressionSyntax = Factory.InvocationExpression(Factory.SimpleMemberAccessExpression(Factory.SimpleMemberAccessExpression(MeExpression, e),
                                                                                                                                      Factory.IdentifierName(EqualsIdentifierToken)),
                                                                                                 Factory.ArgumentList(Factory.SingletonSeparatedList(Of VBS.ArgumentSyntax)(Factory.SimpleArgument(Factory.MemberAccessExpression(VB.SyntaxKind.SimpleMemberAccessExpression,
                                                                                                                                                                                                                                  Factory.IdentifierName(anotherRecordToken),
                                                                                                                                                                                                                                  DotToken,
                                                                                                                                                                                                                                  e)))))
                    Dim condition As VBS.ExpressionSyntax = Factory.NotExpression(NotKeyword, operand)
                    statements = statements.Add(Factory.SingleLineIfStatement(condition, Factory.SingletonList(returnFalse), elseClause:=Nothing))
                Next
                statements = statements.Add(Factory.ReturnStatement(Factory.TrueLiteralExpression(TrueKeyword)))

                members.Add(Factory.FunctionBlock(equalsAnotherRecordFuncStmt, statements))

                '     Public Overrides Function GetHashCode() As Integer
                '         Return MyBase.GetHashCode()
                '     End Function
                Dim getHashCodeIdent As VBS.SimpleNameSyntax = Factory.IdentifierName("GetHashCode")
                Dim getHashCodeFuncStmt As VBS.MethodStatementSyntax = Factory.FunctionStatement(attributeLists:=Nothing,
                                                                                           PublicModifier.Add(OverridesKeyword),
                                                                                           getHashCodeIdent.Identifier,
                                                                                           typeParameterList:=Nothing,
                                                                                           Factory.ParameterList(),
                                                                                           Factory.SimpleAsClause(PredefinedTypeInteger),
                                                                                           handlesClause:=Nothing,
                                                                                           implementsClause:=Nothing
                                                                                          )
                myBaseInvocationExpr = Factory.InvocationExpression(Factory.MemberAccessExpression(VB.SyntaxKind.SimpleMemberAccessExpression,
                                                                                                   MyBaseExpression,
                                                                                                   DotToken,
                                                                                                   Factory.IdentifierName(getHashCodeIdent.Identifier)),
                                                                    Factory.ArgumentList())

                Dim getHashCodeStatement As SyntaxList(Of VBS.StatementSyntax) = Factory.SingletonList(Of VBS.StatementSyntax)(Factory.ReturnStatement(myBaseInvocationExpr))
                members.Add(Factory.FunctionBlock(getHashCodeFuncStmt, getHashCodeStatement))

                ' Public Overrides Function ToString() As String
                ' NamedPoint { X = 5, Y = 3, Name = A }
                Dim toStringIdent As VBS.SimpleNameSyntax = Factory.IdentifierName("ToString")
                Dim toStringFuncStmt As VBS.MethodStatementSyntax = Factory.FunctionStatement(attributeLists:=Nothing,
                                                                                           PublicModifier.Add(OverridesKeyword),
                                                                                           toStringIdent.Identifier,
                                                                                           typeParameterList:=Nothing,
                                                                                           Factory.ParameterList(),
                                                                                           Factory.SimpleAsClause(PredefinedTypeString),
                                                                                           handlesClause:=Nothing,
                                                                                           implementsClause:=Nothing
                                                                                          )

                Dim contents As New SyntaxList(Of VBS.InterpolatedStringContentSyntax)
                Dim nameOfRecordText As String = $"{recordTypeName} = {{{{ "
                contents = contents.Add(Factory.InterpolatedStringText(Factory.InterpolatedStringTextToken(nameOfRecordText, nameOfRecordText)))
                For Each e As IndexClass(Of VBS.SimpleNameSyntax) In fullElementList.WithIndex
                    contents = contents.Add(Factory.InterpolatedStringText(Factory.InterpolatedStringTextToken($"{e.Value} = ", $"{e.Value} = ")))
                    contents = contents.Add(Factory.Interpolation(Factory.SimpleMemberAccessExpression(MeExpression, e.Value)))
                    If e.IsLast Then
                        contents = contents.Add(Factory.InterpolatedStringText(Factory.InterpolatedStringTextToken(" }}", " }}")))
                    Else
                        contents = contents.Add(Factory.InterpolatedStringText(Factory.InterpolatedStringTextToken(", ", ", ")))
                    End If
                Next

                Dim interpolatedString As VBS.ExpressionSyntax = Factory.InterpolatedStringExpression(contents)
                Dim toStringStatement As SyntaxList(Of VBS.StatementSyntax) = Factory.SingletonList(Of VBS.StatementSyntax)(Factory.ReturnStatement(interpolatedString))
                members.Add(Factory.FunctionBlock(toStringFuncStmt, toStringStatement))

                Dim listOfAttributes As SyntaxList(Of VBS.AttributeListSyntax) = Factory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))
                Dim typeParameterList As VBS.TypeParameterListSyntax = DirectCast(node.TypeParameterList?.Accept(Me), VBS.TypeParameterListSyntax)
                Dim modifiers As SyntaxTokenList = Factory.TokenList(ConvertModifiers(node.Modifiers, Me.IsModule, TokenContext.Struct))
                Dim classStmt As VBS.ClassStatementSyntax = DirectCast(Factory.ClassStatement(listOfAttributes,
                                                                                              Factory.TokenList(modifiers),
                                                                                              ClassKeyWord.WithConvertedTriviaFrom(node.Keyword),
                                                                                              recordName,
                                                                                              typeParameterList
                                                                                             ).RestructureAttributesAndModifiers(listOfAttributes.Any, modifiers.Any), VBS.ClassStatementSyntax).WithTrailingEol

                Dim classBlock As VBS.ClassBlockSyntax = Factory.ClassBlock(classStmt,
                                                                           Factory.List([inherits]),
                                                                            [implements]:=Nothing,
                                                                            members:=Factory.List(members),
                                                                            Factory.EndClassStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), ClassKeyWord).WithConvertedTriviaFrom(node.CloseBraceToken)
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
                Dim statementWithIssue As CS.CSharpSyntaxNode = GetStatementWithIssues(node)
                statementWithIssue.CheckCorrectnessLeadingTrivia(attemptToPortMade:=True, "ref expression")
                Return node.Expression.Accept(Me)
            End Function

            Public Overrides Function VisitRefType(node As CSS.RefTypeSyntax) As VB.VisualBasicSyntaxNode
                Dim statementWithIssue As CS.CSharpSyntaxNode = GetStatementWithIssues(node)
                statementWithIssue.CheckCorrectnessLeadingTrivia(attemptToPortMade:=True, "ref type")
                Return node.Type.Accept(Me)
            End Function

            Public Overrides Function VisitRefTypeExpression(node As CSS.RefTypeExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim statementWithIssue As CS.CSharpSyntaxNode = GetStatementWithIssues(node)
                statementWithIssue.CheckCorrectnessLeadingTrivia(attemptToPortMade:=True, "ref type expression")
                Return node.Expression.Accept(Me)
            End Function

            Public Overrides Function VisitStackAllocArrayCreationExpression(node As CSS.StackAllocArrayCreationExpressionSyntax) As VB.VisualBasicSyntaxNode
                node.FirstAncestorOrSelf(Of CSS.StatementSyntax).AddMarker(FlagUnsupportedStatements(node.FirstAncestorOrSelf(Of CSS.StatementSyntax), "StackAlloc", commentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, allowDuplicates:=True)
                Return PredefinedTypeObject
            End Function

            Public Overrides Function VisitSwitchExpression(node As CSS.SwitchExpressionSyntax) As VB.VisualBasicSyntaxNode
                Dim statementWithIssue As CS.CSharpSyntaxNode = GetStatementWithIssues(node)
                Dim governingExpression As VBS.ExpressionSyntax = CType(node.GoverningExpression.Accept(Me), VBS.ExpressionSyntax)
                Dim selectCaseStatement As VBS.SelectStatementSyntax = Factory.SelectStatement(governingExpression.WithLeadingTrivia(SpaceTrivia)).WithLeadingTrivia(governingExpression.GetLeadingTrivia)
                Dim resultNameToken As SyntaxToken = Factory.Identifier(node.GetUniqueVariableNameInScope("tempVar", _usedIdentifiers, _semanticModel))
                Dim resultIdentifier As VBS.IdentifierNameSyntax = Factory.IdentifierName(resultNameToken)
                Dim typeInf As TypeInfo = _semanticModel.GetTypeInfo(node.Arms(0).Expression)
                Dim asClause As VBS.AsClauseSyntax = Nothing
                If typeInf.Type IsNot Nothing AndAlso Not typeInf.Type.IsErrorType Then
                    If TypeOf typeInf.Type Is INamedTypeSymbol AndAlso typeInf.Type.IsTupleType Then
                        asClause = Factory.SimpleAsClause(typeInf.Type.ToString.ConvertCsStringToName.WithLeadingTrivia(SpaceTrivia))
                    Else
                        asClause = Factory.SimpleAsClause(typeInf.Type.ConvertToType.WithLeadingTrivia(SpaceTrivia))
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
                        expr = expr.WithLeadingTrivia(SpaceTrivia)
                        statements = Factory.SingletonList(Of VBS.StatementSyntax)(Factory.SimpleAssignmentStatement(resultIdentifier,
                                                                                                                     expr.WithTrailingEol
                                                                                                                    ).WithLeadingTrivia(savedLeadingTrivia))
                    End If
                    If TypeOf arm.Pattern Is CSS.ConstantPatternSyntax Then
                        Dim constantPatternExpression As VBS.ExpressionSyntax = CType(arm.Pattern.Accept(Me), VBS.ExpressionSyntax)
                        Dim relationalCaseClause As VBS.RelationalCaseClauseSyntax = Factory.CaseEqualsClause(CType(arm.Pattern.Accept(Me), VBS.ExpressionSyntax).WithLeadingTrivia(SpaceTrivia))
                        caseStatement = Factory.CaseStatement(relationalCaseClause).WithPrependedLeadingTrivia(constantPatternExpression.GetLeadingTrivia)
                    ElseIf TypeOf arm.Pattern Is CSS.DiscardPatternSyntax Then
                        If arm.WhenClause IsNot Nothing Then
                            whenClause = CType(e.Value.WhenClause.Accept(Me), VBS.ExpressionSyntax)
                            caseClause = Factory.SingletonSeparatedList(Of VBS.CaseClauseSyntax)(Factory.SimpleCaseClause(whenClause))
                        End If
                        caseStatement = Factory.CaseStatement(caseClause)
                    ElseIf TypeOf arm.Pattern Is CSS.RecursivePatternSyntax Then
                        statementWithIssue.AddMarker(FlagUnsupportedStatements(node, "Switch Expression with Recursive Pattern Syntax", commentOutOriginalStatements:=True), StatementHandlingOption.ReplaceStatement, allowDuplicates:=True)
                        Return resultIdentifier
                    ElseIf TypeOf arm.Pattern Is CSS.DeclarationPatternSyntax Then
                        Dim pattern As CSS.DeclarationPatternSyntax = DirectCast(arm.Pattern, CSS.DeclarationPatternSyntax)
                        Dim variableType As VBS.TypeSyntax = DirectCast(pattern.Type.Accept(Me), VBS.TypeSyntax).WithLeadingTrivia(SpaceTrivia)
                        Dim tryCastExpr As VBS.TryCastExpressionSyntax = Factory.TryCastExpression(governingExpression, variableType.WithLeadingTrivia(SpaceTrivia))
                        If pattern.Designation.IsKind(CS.SyntaxKind.SingleVariableDesignation) Then
                            Dim designation As CSS.SingleVariableDesignationSyntax = CType(pattern.Designation, CSS.SingleVariableDesignationSyntax)
                            Dim identifierToken As SyntaxToken = GenerateSafeVbToken(designation.Identifier, node, _semanticModel, _usedIdentifiers)
                            Dim initializer As VBS.EqualsValueSyntax = Factory.EqualsValue(Factory.DirectCastExpression(governingExpression, variableType))
                            statements = statements.Insert(0, FactoryDimStatement(identifierToken,
                                                                                        Factory.SimpleAsClause(variableType),
                                                                                        initializer).WithConvertedLeadingTriviaFrom(arm).WithTrailingEol
                                                                                       )
                            Dim armExpression As VBS.ExpressionSyntax = CType(arm.Expression.Accept(Me), VBS.ExpressionSyntax)
                            Dim right As VBS.ExpressionSyntax = armExpression.AdjustExpressionTrivia(adjustLeading:=True, directiveNotAllowed:=False)
                            If right.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                equalsTokenWithTrivia = equalsTokenWithTrivia.WithTrailingTrivia(SpaceLineContinueEOL)
                            End If
                            statements = statements.Replace(statements.Last, Factory.AssignmentStatement(VB.SyntaxKind.SimpleAssignmentStatement, resultIdentifier, equalsTokenWithTrivia, right).WithTrailingEol)
                            If e.Value.WhenClause IsNot Nothing Then
                                whenClause = CType(e.Value.WhenClause.Accept(Me), VBS.ExpressionSyntax)
                                caseClause = Factory.SingletonSeparatedList(Of VBS.CaseClauseSyntax)(Factory.SimpleCaseClause(whenClause))
                            Else
                                caseClause = New SeparatedSyntaxList(Of VBS.CaseClauseSyntax)
                            End If
                            caseClause = caseClause.Insert(0, Factory.SimpleCaseClause(Factory.EqualsExpression(tryCastExpr, Factory.TrueLiteralExpression(TrueKeyword))))
                        ElseIf pattern.Designation.IsKind(CS.SyntaxKind.DiscardDesignation) Then
                            Dim armExpression As VBS.ExpressionSyntax = CType(arm.Expression.Accept(Me), VBS.ExpressionSyntax)
                            Dim right As VBS.ExpressionSyntax = armExpression.AdjustExpressionTrivia(adjustLeading:=True, directiveNotAllowed:=False)
                            If right.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                equalsTokenWithTrivia = equalsTokenWithTrivia.WithTrailingTrivia(SpaceLineContinueEOL)
                            End If
                            statements = statements.Insert(0, Factory.AssignmentStatement(VB.SyntaxKind.SimpleAssignmentStatement,
                                                                                                resultIdentifier,
                                                                                                equalsTokenWithTrivia,
                                                                                                right
                                                                                               ).WithTrailingEol)
                            caseClause = Factory.SingletonSeparatedList(Of VBS.CaseClauseSyntax)(Factory.SimpleCaseClause(Factory.EqualsExpression(tryCastExpr, Factory.TrueLiteralExpression(TrueKeyword))))
                        Else
                            Throw UnexpectedValue(pattern.Designation, NameOf(pattern.Designation))
                        End If
                        caseStatement = Factory.CaseStatement(caseClause)
                    ElseIf TypeOf arm.Pattern Is CSS.VarPatternSyntax Then
                        Dim varPattern As CSS.VarPatternSyntax = DirectCast(arm.Pattern, CSS.VarPatternSyntax)
                        Dim identifier As SyntaxToken
                        If TypeOf varPattern.Designation Is CSS.SingleVariableDesignationSyntax Then
                            identifier = GenerateSafeVbToken(id:=DirectCast(varPattern.Designation, CSS.SingleVariableDesignationSyntax).Identifier, node:=node, model:=_semanticModel, usedIdentifiers:=_usedIdentifiers)
                        ElseIf TypeOf varPattern.Designation Is CSS.ParenthesizedVariableDesignationSyntax Then
                            Dim sBuilder As New StringBuilder
                            CreateDesignationName(ProcessVariableDesignation(CType(varPattern.Designation, CSS.ParenthesizedVariableDesignationSyntax)), sBuilder)
                            identifier = GenerateSafeVbToken(id:=CS.SyntaxFactory.Identifier(sBuilder.ToString), node:=node, model:=_semanticModel, usedIdentifiers:=_usedIdentifiers)
                        Else
                            _reportException?.Invoke(UnreachableException)
                        End If

                        statements = statements.Insert(0, FactoryDimStatement(identifier,
                                                                                    asClause:=Nothing,
                                                                                    initializer:=Factory.EqualsValue(governingExpression)
                                                                                   ).WithTrailingEol)
                        If Not e.IsLast Then
                            whenClause = CType(e.Value.WhenClause?.Accept(Me), VBS.ExpressionSyntax)
                            caseClause = Factory.SingletonSeparatedList(Of VBS.CaseClauseSyntax)(Factory.SimpleCaseClause(whenClause))
                            caseStatement = Factory.CaseStatement(caseClause)
                        End If
                    ElseIf TypeOf arm.Pattern Is CSS.BinaryPatternSyntax Then
                        caseClause = Factory.SingletonSeparatedList(Of VBS.CaseClauseSyntax)(Factory.SimpleCaseClause(CType(arm.Pattern.Accept(Me), VBS.ExpressionSyntax)))
                        caseStatement = Factory.CaseStatement(caseClause)
                    Else
                        Throw UnexpectedValue(NameOf(CSS.SwitchExpressionSyntax))
                    End If
                    If Not e.IsLast Then
                        If caseStatement Is Nothing Then
                            Throw New Exception($"TypeOf arm.Pattern is {arm.Pattern.Kind}")
                        End If
                        blocks = blocks.Add(Factory.CaseBlock(caseStatement.WithTrailingEol, statements))
                    Else
                        blocks = blocks.Add(Factory.CaseElseBlock(Factory.CaseElseStatement(Factory.ElseCaseClause).WithConvertedLeadingTriviaFrom(arm).WithTrailingEol, statements))
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
                                                                 initializer:=Nothing).WithTrailingEol,
                                             StatementHandlingOption.PrependStatement,
                                             allowDuplicates:=True)
                statementWithIssue.AddMarker(stmt, StatementHandlingOption.PrependStatement, allowDuplicates:=True)
                Return resultIdentifier
            End Function

            Public Overrides Function VisitVariableDeclaration(node As CSS.VariableDeclarationSyntax) As VB.VisualBasicSyntaxNode
                ' TODO Implement
                'Dim statementWithIssue As CS.CSharpSyntaxNode = GetStatementWithIssues(node)
                'Dim leadingTrivia As SyntaxTriviaList = statementWithIssue.CheckCorrectnessLeadingTrivia(attemptToPortMade:=True, "VB has no direct equivalent To C# var pattern expressions")
                Return MyBase.VisitVariableDeclaration(node)
            End Function

            Public Overrides Function VisitVariableDeclarator(node As CSS.VariableDeclaratorSyntax) As VB.VisualBasicSyntaxNode
                Dim identifier As SyntaxToken = GenerateSafeVbToken(node.Identifier, node, _semanticModel, _usedIdentifiers)
                Dim argumentList As New List(Of VBS.ArgumentSyntax)
                If node.ArgumentList Is Nothing Then
                    Return Factory.ModifiedIdentifier(identifier).WithTrailingTrivia(SpaceTrivia)
                Else
                    For Each e As IndexClass(Of CSS.ArgumentSyntax) In node.ArgumentList.Arguments.WithIndex
                        Dim vbExpression As VBS.ExpressionSyntax = CType(e.Value.Expression.Accept(Me), VBS.ExpressionSyntax)
                        If TypeOf vbExpression Is VBS.LiteralExpressionSyntax Then
                            Dim literalExpression As VBS.LiteralExpressionSyntax = CType(vbExpression, VBS.LiteralExpressionSyntax)
                            argumentList.Add(Factory.SimpleArgument(Factory.LiteralExpression(VB.SyntaxKind.NumericLiteralExpression, Factory.Literal(CInt(literalExpression.Token.Value) - 1))).WithConvertedLeadingTriviaFrom(e.Value.Expression))
                        ElseIf TypeOf vbExpression Is VBS.IdentifierNameSyntax Then
                            Dim identName As VBS.IdentifierNameSyntax = CType(vbExpression, VBS.IdentifierNameSyntax)
                            argumentList.Add(Factory.SimpleArgument(Factory.BinaryExpression(VB.SyntaxKind.SubtractExpression, identName, MinusToken, OneExpression)).WithConvertedLeadingTriviaFrom(e.Value.Expression))
                        ElseIf TypeOf vbExpression Is VBS.BinaryExpressionSyntax Then
                            Dim value As VBS.BinaryExpressionSyntax = CType(vbExpression, VBS.BinaryExpressionSyntax)
                            If vbExpression.IsKind(VB.SyntaxKind.AddExpression) Then
                                If value.Right.IsKind(VB.SyntaxKind.NumericLiteralExpression) AndAlso CType(value.Right, VBS.LiteralExpressionSyntax).Token.ValueText = "1" Then
                                    argumentList.Add(Factory.SimpleArgument(value.Left.WithConvertedLeadingTriviaFrom(e.Value.Expression)))
                                    Continue For
                                End If
                            End If
                            argumentList.Add(Factory.SimpleArgument(Factory.BinaryExpression(VB.SyntaxKind.SubtractExpression, value, MinusToken, OneExpression)).WithConvertedLeadingTriviaFrom(e.Value.Expression))
                        Else
                            Stop
                        End If
                    Next
                End If
                Return Factory.ModifiedIdentifier(identifier.WithTrailingTrivia(SpaceTrivia),
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
                Dim variableITypeSymbol As (_Error As Boolean, _ITypeSymbol As ITypeSymbol) = node.Expression.DetermineType(_semanticModel)
                Dim recordType As VBS.TypeSyntax
                Dim asClause As VBS.SimpleAsClauseSyntax = Nothing
                If Not variableITypeSymbol._Error Then
                    recordType = variableITypeSymbol._ITypeSymbol.ConvertToType
                    If TypeOf recordType Is VBS.QualifiedNameSyntax Then
                        asClause = Factory.SimpleAsClause(CType(recordType, VBS.QualifiedNameSyntax).Right)
                    Else
                        asClause = Factory.SimpleAsClause(recordType)
                    End If
                End If
                Dim identifier As VBS.ModifiedIdentifierSyntax = Factory.ModifiedIdentifier("_p1")
                Dim parameter As VBS.ParameterSyntax = Factory.Parameter(attributeLists, modifiers, identifier, asClause, Nothing)
                Dim parameters As SeparatedSyntaxList(Of VBS.ParameterSyntax)
                parameters = parameters.Add(parameter)
                Dim parameterList As VBS.ParameterListSyntax = Factory.ParameterList(parameters)
                Dim subOrFunctionHeader As VBS.LambdaHeaderSyntax = Factory.FunctionLambdaHeader(attributeLists, modifiers, parameterList, asClause)

                Dim p1 As VBS.SimpleNameSyntax = Factory.IdentifierName("_p1")
                Dim temp As SyntaxToken = Factory.Identifier("temp")
                Dim value As VBS.ExpressionSyntax = Factory.MemberAccessExpression(VB.SyntaxKind.SimpleMemberAccessExpression, p1, DotToken, CloneIdentifier)
                Dim initializer As VBS.EqualsValueSyntax = Factory.EqualsValue(value)
                statements = statements.Add(FactoryDimStatement(temp, asClause, initializer))
                Dim withBlock As New SyntaxList(Of VBS.StatementSyntax)
                For Each expression As CSS.ExpressionSyntax In node.Initializer.Expressions
                    If TypeOf expression Is CSS.AssignmentExpressionSyntax Then
                        Dim assignmentExpression As CSS.AssignmentExpressionSyntax = CType(expression, CSS.AssignmentExpressionSyntax)
                        Dim vbExpression As VB.VisualBasicSyntaxNode = assignmentExpression.Left.Accept(Me)
                        Dim left As VBS.MemberAccessExpressionSyntax
                        If TypeOf vbExpression Is VBS.MemberAccessExpressionSyntax Then
                            left = CType(vbExpression, VBS.MemberAccessExpressionSyntax)
                        ElseIf TypeOf vbExpression Is VBS.NameSyntax Then
                            Dim name As VBS.SimpleNameSyntax = CType(vbExpression, VBS.SimpleNameSyntax)
                            left = Factory.MemberAccessExpression(VB.SyntaxKind.SimpleMemberAccessExpression, DotToken, name)
                        Else
                            Throw UnreachableException
                        End If
                        Dim right As VBS.ExpressionSyntax = CType(assignmentExpression.Right.Accept(Me), VBS.ExpressionSyntax)
                        withBlock = withBlock.Add(Factory.SimpleAssignmentStatement(left, right))
                    Else
                        withBlock = withBlock.Add(Factory.ExpressionStatement(CType(expression.Accept(Me), VBS.ExpressionSyntax)))
                    End If
                Next

                Dim withStatement As VBS.WithStatementSyntax = Factory.WithStatement(Factory.IdentifierName(temp))
                statements = statements.Add(Factory.WithBlock(withStatement, withBlock))
                statements = statements.Add(Factory.ReturnStatement(Factory.IdentifierName(temp)))

                Dim endSubOrFunctionStatement As VBS.EndBlockStatementSyntax = Factory.EndFunctionStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), FunctionKeyword).WithConvertedTriviaFrom(node)

                Dim lambda As VBS.MultiLineLambdaExpressionSyntax = Factory.MultiLineFunctionLambdaExpression(subOrFunctionHeader, statements, endSubOrFunctionStatement)
                Dim lambdaArgument As SeparatedSyntaxList(Of VBS.ArgumentSyntax)
                lambdaArgument = lambdaArgument.Add(Factory.SimpleArgument(CType(node.Expression.Accept(Me), VBS.ExpressionSyntax)))
                Return Factory.InvocationExpression(lambda, Factory.ArgumentList(lambdaArgument))
            End Function

        End Class

    End Class

End Namespace
