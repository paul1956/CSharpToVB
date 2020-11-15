' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Collections.Immutable
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

            Private Const InlineAssignHelperCode As String = "<Obsolete(""Please refactor code that uses this function, it is a simple work-around to simulate inline assignment in VB!"")>
Private Shared Function __InlineAssignHelper(Of T)(ByRef target As T, value As T) As T
target = value
Return value
End Function
"

            '    Private Const ByRefHelperCode As String = "Private Function __VbByRefHelper(Of t)(ByRef byRefValue As t, byRefSetter As Func(Of t, t)) As t
            '        Dim orgValue = byRefValue
            '        byRefValue = byRefSetter(byRefValue)
            '        Return orgValue
            '    End Function

            '    Private Function __VbByRefHelper(Of t, rt)(ByRef byRefValue As t, byRefSetter As Func(Of t, (ByRefValue As t, ReturnValue As rt))) As rt
            '        Dim retValue = byRefSetter(byRefValue)
            '        byRefValue = retValue.ByRefValue
            '        Return retValue.ReturnValue
            '    End Function
            '"

            Private Sub ConvertBaseList(_Type As CSS.BaseTypeDeclarationSyntax, [inherits] As List(Of VBS.InheritsStatementSyntax), [implements] As List(Of VBS.ImplementsStatementSyntax), ByRef Optional ImplementedMembers As ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))) = Nothing)
                Dim typeSyntaxArray As VBS.TypeSyntax()
                Dim startImplementsIndex As Integer = 0
                Select Case _Type.Kind()
                    Case CS.SyntaxKind.ClassDeclaration
                        Dim classOrInterface As CSS.TypeSyntax = _Type.BaseList?.Types.FirstOrDefault()?.Type
                        If classOrInterface Is Nothing Then
                            Exit Sub
                        End If
                        Dim classOrInterfaceSymbol As ISymbol = ModelExtensions.GetSymbolInfo(_mSemanticModel, classOrInterface).Symbol
                        If classOrInterfaceSymbol?.IsInterfaceType() Then
                            typeSyntaxArray = _Type.BaseList?.Types.Select(Function(t As CSS.BaseTypeSyntax) DirectCast(t.Type.Accept(Me), VBS.TypeSyntax)).ToArray()
                            startImplementsIndex = 0
                        Else
                            Dim typeNode As VB.VisualBasicSyntaxNode = classOrInterface.Accept(Me)
                            If typeNode.HasLeadingTrivia Then
                                Dim typeNodeLeadingTrivia As SyntaxTriviaList = typeNode.GetLeadingTrivia
                                If typeNodeLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                    [inherits].Add(Factory.InheritsStatement(DirectCast(typeNode.WithLeadingTrivia(Factory.Space), VBS.TypeSyntax)).WithLeadingTrivia(typeNodeLeadingTrivia))
                                Else
                                    [inherits].Add(Factory.InheritsStatement(DirectCast(typeNode, VBS.TypeSyntax)))
                                End If
                            Else
                                [inherits].Add(Factory.InheritsStatement(DirectCast(typeNode, VBS.TypeSyntax)))
                            End If
                            startImplementsIndex = 1
                            typeSyntaxArray = _Type.BaseList?.Types.Skip(startImplementsIndex).Select(Function(t As CSS.BaseTypeSyntax) DirectCast(t.Type.Accept(Me).WithTrailingTrivia(Factory.Space), VBS.TypeSyntax)).ToArray()
                        End If
                        If typeSyntaxArray.Any Then
                            [implements].Add(Factory.ImplementsStatement(typeSyntaxArray))
                            Dim implementsClauses As IEnumerable(Of CSS.TypeSyntax) = _Type.BaseList?.Types.Skip(startImplementsIndex).Select(Function(t As CSS.BaseTypeSyntax) t.Type)
                            For Each implementsClause As CSS.TypeSyntax In implementsClauses
                                Dim classOrStructDecl As SyntaxNode = Nothing
                                Dim classOrStructType As INamedTypeSymbol = Nothing
                                Dim interfaceTypes As IEnumerable(Of INamedTypeSymbol) = Nothing

                                If implementsClause.TryInitializeState(_mSemanticModel, classOrStructDecl, classOrStructType, interfaceTypes, _originalRequest.CancelToken) Then
                                    Dim items As ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))) = classOrStructType.GetAllImplementedMembers(interfaceTypes, _originalRequest.CancelToken)
                                    ImplementedMembers = ImplementedMembers.AddRange(items)
                                End If
                            Next
                        End If

                    Case CS.SyntaxKind.StructDeclaration
                        typeSyntaxArray = _Type.BaseList?.Types.Select(Function(t As CSS.BaseTypeSyntax) DirectCast(t.Type.Accept(Me), VBS.TypeSyntax)).ToArray()
                        If typeSyntaxArray?.Any Then
                            [implements].Add(Factory.ImplementsStatement(typeSyntaxArray))
                        End If
                    Case CS.SyntaxKind.InterfaceDeclaration
                        Dim baseList As New List(Of VBS.TypeSyntax)
                        Dim newLeadingTrivia As New SyntaxTriviaList
                        If _Type.BaseList IsNot Nothing Then
                            newLeadingTrivia = _Type.BaseList.ColonToken.LeadingTrivia.ConvertTriviaList()
                            Dim csSeparators As List(Of SyntaxToken) = _Type.BaseList.Types.GetSeparators.ToList
                            For Each e As IndexClass(Of CSS.BaseTypeSyntax) In _Type.BaseList.Types.WithIndex
                                Dim item As VBS.TypeSyntax = DirectCast(e.Value.Type.Accept(Me), VBS.TypeSyntax)
                                If item.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                    newLeadingTrivia = newLeadingTrivia.AddRange(item.GetLeadingTrivia)
                                    item = item.WithLeadingTrivia(Factory.Space)
                                End If
                                If Not e.IsLast Then
                                    If csSeparators(e.index).LeadingTrivia.ContainsDirectiveTrivia Then
                                        newLeadingTrivia = newLeadingTrivia.Add(Factory.CommentTrivia($"' TODO: Visual Basic does not support directives in inherits lists. Directive moved!"))
                                        newLeadingTrivia = newLeadingTrivia.Add(VBEOLTrivia)
                                        newLeadingTrivia = newLeadingTrivia.AddRange(csSeparators(e.index).LeadingTrivia.ConvertTriviaList())
                                    ElseIf csSeparators(e.index).LeadingTrivia.ContainsCommentTrivia Then
                                        newLeadingTrivia = newLeadingTrivia.AddRange(csSeparators(e.index).LeadingTrivia.ConvertTriviaList())
                                    End If
                                    If item.GetTrailingTrivia.ContainsCommentTrivia Then
                                        baseList.Add(item)
                                    Else
                                        baseList.Add(item.WithTrailingTrivia(Factory.Space))
                                    End If
                                Else
                                    baseList.Add(item)
                                End If
                            Next
                        End If
                        If baseList.Any Then
                            [inherits].Add(Factory.InheritsStatement(baseList.ToArray).WithLeadingTrivia(newLeadingTrivia))
                        End If
                End Select
                If [implements].Any Then
                    [implements]([implements].Count - 1) = [implements].Last.WithTrailingEOL
                End If
                If [inherits].Any Then
                    [inherits]([inherits].Count - 1) = [inherits].Last.WithTrailingEOL
                End If
            End Sub

            Private Iterator Function PatchInlineHelpers(node As CSS.BaseTypeDeclarationSyntax, IsModule As Boolean) As IEnumerable(Of VBS.StatementSyntax)
                If InlineAssignHelperMarkers.Contains(node) Then
                    InlineAssignHelperMarkers.Remove(node)
                    Yield TryCast(Factory.ParseSyntaxTree(InlineAssignHelperCode.Replace("Shared ", If(IsModule, "", "Shared "), StringComparison.Ordinal)).GetRoot().ChildNodes().FirstOrDefault(), VBS.StatementSyntax)
                End If
            End Function

            Public Overrides Function VisitClassDeclaration(node As CSS.ClassDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim saveUsedIdentifiers As Dictionary(Of String, SymbolTableEntry) = _usedIdentifiers
                SyncLock _originalRequest.UsedStacks
                    If _usedIdentifiers.Any Then
                        _originalRequest.UsedStacks.Push(_usedIdentifiers)
                        _usedIdentifiers.Clear()
                    End If
                    _isModuleStack.Push(node.Modifiers.Contains(CS.SyntaxKind.StaticKeyword) And node.TypeParameterList Is Nothing)
                    If _originalRequest.ImplementedMembers.Any Then
                        _originalRequest.ImplementedMembersStack.Push(_originalRequest.ImplementedMembers)
                        _originalRequest.ImplementedMembers = (New List(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol)))).ToImmutableArray
                    End If
                End SyncLock
                Dim members As New List(Of VBS.StatementSyntax)
                Dim membersLastIndex As Integer = node.Members.Count - 1
                Dim [inherits] As New List(Of VBS.InheritsStatementSyntax)()
                Dim [implements] As New List(Of VBS.ImplementsStatementSyntax)()

                Me.ConvertBaseList(node, [inherits], [implements], _originalRequest.ImplementedMembers)
                Dim staticMethodCount As Integer = 0
                Dim methodCount As Integer = 0
                Dim classType As ITypeSymbol = CType(_mSemanticModel.GetDeclaredSymbol(node), ITypeSymbol)
                For Each e As IndexClass(Of CSS.MemberDeclarationSyntax) In node.Members.WithIndex
                    If _originalRequest.CancelToken.IsCancellationRequested Then
                        Throw New OperationCanceledException
                    End If
                    Dim m As CSS.MemberDeclarationSyntax = e.Value
                    Dim statement As VBS.StatementSyntax = DirectCast(m.Accept(Me), VBS.StatementSyntax).RemoveExtraLeadingEOL.WithTrailingEOL
                    ' Cases below are handled by RestructureAttributesAndModifiers
                    If statement.IsKind(VB.SyntaxKind.FieldDeclaration) Then
                        members.Add(statement)
                        members.AddRange(AddFinalTriviaToField(DirectCast(m, CSS.FieldDeclarationSyntax)))
                    ElseIf statement.IsKind(VB.SyntaxKind.ClassBlock) OrElse
                        statement.IsKind(VB.SyntaxKind.EmptyStatement) OrElse
                        statement.IsKind(VB.SyntaxKind.EnumBlock) OrElse
                        statement.IsKind(VB.SyntaxKind.FunctionBlock) OrElse
                        statement.IsKind(VB.SyntaxKind.InterfaceBlock) OrElse
                        statement.IsKind(VB.SyntaxKind.ModuleBlock) OrElse
                        statement.IsKind(VB.SyntaxKind.StructureBlock) OrElse
                        statement.IsKind(VB.SyntaxKind.SubStatement) Then
                        members.Add(statement.WithTrailingEOL)
                    ElseIf statement.IsKind(VB.SyntaxKind.FunctionStatement) OrElse
                        statement.IsKind(VB.SyntaxKind.SubBlock) Then
                        Dim modifiers As List(Of SyntaxToken)
                        If TypeOf statement Is VBS.MethodBlockSyntax Then
                            Dim block As VBS.MethodBlockSyntax = CType(statement, VBS.MethodBlockSyntax)
                            modifiers = block.BlockStatement.Modifiers.ToList
                        ElseIf TypeOf statement Is VBS.MethodStatementSyntax Then
                            Dim block As VBS.MethodStatementSyntax = CType(statement, VBS.MethodStatementSyntax)
                            modifiers = block.Modifiers.ToList
                        Else
                            modifiers = New List(Of SyntaxToken)
                            Stop
                        End If
                        methodCount += 1
                        If modifiers.Contains(VB.SyntaxKind.SharedKeyword) Then
                            staticMethodCount += 1
                        End If
                        members.Add(statement.WithTrailingEOL)
                    ElseIf statement.IsKind(VB.SyntaxKind.PropertyBlock) Then
                        If TypeOf m Is CSS.PropertyDeclarationSyntax Then
                            If CType(m, CSS.PropertyDeclarationSyntax).ExpressionBody Is Nothing Then
                                members.AddRange(ReplaceOneStatementWithMarkedStatements(m, statement.WithTrailingEOL))
                            Else
                                members.AddRange(ReplaceOneStatementWithMarkedStatements(CType(m, CSS.PropertyDeclarationSyntax).ExpressionBody, statement.WithTrailingEOL))
                            End If
                        Else
                            members.AddRange(ReplaceOneStatementWithMarkedStatements(m, statement.WithTrailingEOL))
                        End If

                        ' Cases below are handled in-line
                    ElseIf statement.IsKind(VB.SyntaxKind.ConstructorBlock) OrElse
                        statement.IsKind(VB.SyntaxKind.DelegateFunctionStatement) OrElse
                        statement.IsKind(VB.SyntaxKind.DelegateSubStatement) OrElse
                        statement.IsKind(VB.SyntaxKind.EventBlock) OrElse
                        statement.IsKind(VB.SyntaxKind.EventStatement) OrElse
                        statement.IsKind(VB.SyntaxKind.OperatorBlock) OrElse
                        statement.IsKind(VB.SyntaxKind.PropertyStatement) Then
                        members.Add(statement.WithConvertedTriviaFrom(m).WithTrailingEOL)
                    Else
                        members.Add(statement.WithConvertedTriviaFrom(m).WithTrailingEOL)
                    End If
                    If e.IsFirst Then
                        If members.Any Then
                            members(0) = members(0).WithPrependedLeadingTrivia(CollectConvertedTokenTrivia(node.OpenBraceToken, GetLeading:=True, GetTrailing:=False))
                        Else
                            Stop
                        End If
                    End If
                Next

                Dim id As SyntaxToken = GenerateSafeVBToken(node.Identifier, node, _usedIdentifiers, _mSemanticModel).WithConvertedTrailingTriviaFrom(node.Identifier)

                members.AddRange(Me.PatchInlineHelpers(node, Me.IsModule))

                Dim listOfAttributes As SyntaxList(Of VBS.AttributeListSyntax) = Factory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))
                Dim typeParameterList As VBS.TypeParameterListSyntax = DirectCast(node.TypeParameterList?.Accept(Me), VBS.TypeParameterListSyntax)
                If typeParameterList IsNot Nothing Then
                    If id.TrailingTrivia.ContainsCommentTrivia Then
                        Stop
                    End If
                    id = id.WithTrailingTrivia(Factory.Space)
                End If
                Dim notInsideClassOrStruct As Boolean = _isModuleStack.Count < 2 AndAlso node.IsNotInStructure

                If Me.IsModule AndAlso notInsideClassOrStruct Then
                    Dim moduleModifiers As SyntaxTokenList = Factory.TokenList(ConvertModifiers(node.Modifiers, Me.IsModule, TokenContext.InterfaceOrModule))
                    Dim moduleKeywordWithTrivia As SyntaxToken = ModuleKeyword.WithConvertedLeadingTriviaFrom(node.Keyword).WithTrailingTrivia(Factory.Space)
                    Dim prependedTrivia As SyntaxTriviaList = DedupLeadingTrivia(node, ModuleKeyword, listOfAttributes.ToList, moduleModifiers)
                    Dim moduleStmt As VBS.ModuleStatementSyntax = DirectCast(Factory.ModuleStatement(listOfAttributes,
                                                                                                     moduleModifiers,
                                                                                                     moduleKeywordWithTrivia,
                                                                                                     id,
                                                                                                     typeParameterList
                                                                                                    ).WithPrependedLeadingTrivia(prependedTrivia) _
                                                                                                     .RestructureAttributesAndModifiers(listOfAttributes.Any, moduleModifiers.Any),
                                                                                VBS.ModuleStatementSyntax).WithTrailingEOL

                    moduleStmt = DirectCast(PrependStatementWithMarkedStatementTrivia(node, moduleStmt), VBS.ModuleStatementSyntax)
                    Dim endModule As VBS.EndBlockStatementSyntax = FactoryEndBlockStatement(VB.SyntaxKind.EndModuleStatement, ModuleKeyword, CollectConvertedTokenTrivia(node.CloseBraceToken, GetLeading:=True, GetTrailing:=True))
                    Dim moduleBlock As VBS.ModuleBlockSyntax = Factory.ModuleBlock(
                                                                                    moduleStmt,
                                                                                    Factory.List([inherits]),
                                                                                    Factory.List([implements]),
                                                                                    Factory.List(members),
                                                                                    endModule
                                                                                    ).WithAppendedTrailingTrivia(node.GetTrailingTrivia.ConvertTriviaList())
                    SyncLock _originalRequest.UsedStacks
                        _isModuleStack.Pop()
                        If _originalRequest.UsedStacks.Count > 0 Then
                            _usedIdentifiers = DirectCast(_originalRequest.UsedStacks.Pop, Dictionary(Of String, SymbolTableEntry))
                        End If
                        If _originalRequest.ImplementedMembersStack.Count > 0 Then
                            _originalRequest.ImplementedMembers = DirectCast(_originalRequest.ImplementedMembersStack.Pop, ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))))
                        End If
                    End SyncLock
                    Return moduleBlock
                Else
                    Dim classModifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, IsModule:=False, If(Me.IsModule, TokenContext.InterfaceOrModule, TokenContext.Global)).ToList
                    Dim classKeywordWithTrivia As SyntaxToken = ClassKeyWord.WithConvertedTriviaFrom(node.Keyword)
                    If methodCount > 0 AndAlso
                        staticMethodCount = methodCount AndAlso
                        Not classModifiers.Contains(VB.SyntaxKind.StaticKeyword, VB.SyntaxKind.NotInheritableKeyword) Then
                        If classModifiers.Count = 0 Then
                            classModifiers.Add(NotInheritableKeyword.WithLeadingTrivia(classKeywordWithTrivia.LeadingTrivia))
                            classKeywordWithTrivia = classKeywordWithTrivia.WithLeadingTrivia(Factory.Space)
                        Else
                            Dim sharedIndex As Integer = classModifiers.IndexOf(VB.SyntaxKind.SharedKeyword)

                            If sharedIndex = -1 Then
                                classModifiers.Add(NotInheritableKeyword)
                            Else
                                classModifiers(sharedIndex) = NotInheritableKeyword.WithTriviaFrom(classModifiers(sharedIndex))
                            End If
                        End If
                    End If
                    Dim prependedTrivia As SyntaxTriviaList = DedupLeadingTrivia(node, classKeywordWithTrivia, listOfAttributes.ToList, classModifiers)
                    Dim classStmt As VBS.ClassStatementSyntax = DirectCast(Factory.ClassStatement(
                                                                            listOfAttributes,
                                                                            Factory.TokenList(classModifiers),
                                                                            classKeywordWithTrivia,
                                                                            id,
                                                                            typeParameterList
                                                                            ).WithPrependedLeadingTrivia(prependedTrivia).
                                                                            RestructureAttributesAndModifiers(listOfAttributes.Any, classModifiers.Any), VBS.ClassStatementSyntax)
                    classStmt = DirectCast(PrependStatementWithMarkedStatementTrivia(node, classStmt), VBS.ClassStatementSyntax)
                    If [inherits].Count = 0 AndAlso [implements].Count = 0 Then
                        classStmt = classStmt.WithTrailingEOL
                    Else
                        If classStmt.GetTrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                            Dim initialTriviaList As SyntaxTriviaList = classStmt.GetTrailingTrivia
                            Dim newTrailingTrivia As SyntaxTriviaList
                            For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                                Dim trivia As SyntaxTrivia = e.Value
                                Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(initialTriviaList, e.index, LookaheadCount:=1)
                                Dim foundSpace As Boolean = False
                                Select Case trivia.RawKind
                                    Case VB.SyntaxKind.WhitespaceTrivia
                                        If nextTrivia.IsKind(VB.SyntaxKind.None) OrElse nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                            Continue For
                                        End If
                                        newTrailingTrivia = newTrailingTrivia.Add(Factory.Space)
                                        foundSpace = True
                                    Case VB.SyntaxKind.EndOfLineTrivia
                                        foundSpace = False
                                    Case VB.SyntaxKind.EndIfDirectiveTrivia
                                        newTrailingTrivia = newTrailingTrivia.Add(trivia)
                                        If nextTrivia.IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                                            newTrailingTrivia = newTrailingTrivia.Add(VBEOLTrivia)
                                            newTrailingTrivia = newTrailingTrivia.Add(nextTrivia)
                                            e.MoveNext()
                                        End If
                                    Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                                        If Not foundSpace Then
                                            newTrailingTrivia = newTrailingTrivia.Add(Factory.Space)
                                        End If
                                        newTrailingTrivia = newTrailingTrivia.AddRange({LineContinuation,
                                                                                        Factory.Space,
                                                                                        trivia})
                                    Case Else
                                End Select
                            Next
                            classStmt = classStmt.WithTrailingTrivia(newTrailingTrivia)
                        Else
                            classStmt = classStmt.WithTrailingTrivia(Factory.Space)
                        End If
                    End If
                    Dim endClass As VBS.EndBlockStatementSyntax = FactoryEndBlockStatement(VB.SyntaxKind.EndClassStatement, ClassKeyWord, CollectConvertedTokenTrivia(node.CloseBraceToken, GetLeading:=True, GetTrailing:=True))
                    Dim classBlock As VBS.ClassBlockSyntax = Factory.ClassBlock(
                                                                                classStmt,
                                                                                Factory.List([inherits]),
                                                                                Factory.List([implements]),
                                                                                Factory.List(members),
                                                                                endClass
                                                                                ).WithAppendedTrailingTrivia(node.GetTrailingTrivia.ConvertTriviaList)
                    _usedIdentifiers = saveUsedIdentifiers
                    SyncLock _originalRequest.UsedStacks
                        _isModuleStack.Pop()
                        If _originalRequest.UsedStacks.Count > 0 Then
                            _usedIdentifiers = DirectCast(_originalRequest.UsedStacks.Pop, Dictionary(Of String, SymbolTableEntry))
                        End If
                        If _originalRequest.ImplementedMembersStack.Count > 0 Then
                            _originalRequest.ImplementedMembers = DirectCast(_originalRequest.ImplementedMembersStack.Pop, ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))))
                        End If
                    End SyncLock
                    Return classBlock
                End If
            End Function

            Public Overrides Function VisitDelegateDeclaration(node As CSS.DelegateDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim identifier As SyntaxToken = GenerateSafeVBToken(node.Identifier, node, _usedIdentifiers, _mSemanticModel)
                Dim methodInfo As INamedTypeSymbol = TryCast(ModelExtensions.GetDeclaredSymbol(_mSemanticModel, node), INamedTypeSymbol)
                Dim attrLists As SyntaxList(Of VBS.AttributeListSyntax) = Factory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))
                Dim modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, Me.IsModule, TokenContext.Global).ToList
                Dim typeParamList As VBS.TypeParameterListSyntax = DirectCast(node.TypeParameterList?.Accept(Me), VBS.TypeParameterListSyntax)?.WithoutTrailingTrivia
                Dim paramList As VBS.ParameterListSyntax = DirectCast(node.ParameterList?.Accept(Me), VBS.ParameterListSyntax)?.WithoutTrailingTrivia
                If methodInfo.DelegateInvokeMethod.GetReturnType()?.SpecialType = SpecialType.System_Void Then
                    Return Factory.DelegateSubStatement(attrLists, Factory.TokenList(modifiers), identifier, typeParamList, paramList, asClause:=Nothing).WithConvertedTriviaFrom(node)
                Else
                    Dim vbNode As VB.VisualBasicSyntaxNode = node.ReturnType.Accept(Me)
                    Dim returnType As VBS.TypeSyntax = DirectCast(vbNode, VBS.TypeSyntax)
                    Dim asClause As VBS.SimpleAsClauseSyntax = Factory.SimpleAsClause(returnType.WithLeadingTrivia(Factory.Space))
                    Return Factory.DelegateFunctionStatement(attrLists, Factory.TokenList(modifiers), identifier, typeParamList, paramList, asClause).WithConvertedTriviaFrom(node)
                End If
            End Function

            Public Overrides Function VisitEnumDeclaration(node As CSS.EnumDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim members As New List(Of VBS.StatementSyntax)
                Dim csMembers As SeparatedSyntaxList(Of CSS.EnumMemberDeclarationSyntax) = node.Members
                If csMembers.Any Then
                    Dim csSeparators As New List(Of SyntaxToken)
                    csSeparators.AddRange(node.Members.GetSeparators)

                    Dim csIdentifierTrailingTrivia As SyntaxTriviaList
                    Dim csSeparatorTrailingTrivia As SyntaxTriviaList
                    Dim csMovedTrailingSpace As SyntaxTriviaList
                    csMovedTrailingSpace = csMovedTrailingSpace.Add(CSEmptySpaceTrivia)
                    Dim vbEnumSatement As VBS.StatementSyntax
                    Dim leadingTrivia As SyntaxTriviaList = CollectConvertedTokenTrivia(node.OpenBraceToken, GetLeading:=True, GetTrailing:=False)
                    For Each e As IndexClass(Of CSS.EnumMemberDeclarationSyntax) In csMembers.WithIndex
                        If e.IsLast Then
                            Exit For
                        End If
                        csIdentifierTrailingTrivia = e.Value.Identifier.TrailingTrivia
                        If csIdentifierTrailingTrivia.Count = 1 AndAlso csIdentifierTrailingTrivia(0).IsWhitespace Then
                            csMovedTrailingSpace = csMovedTrailingSpace.AddRange({csIdentifierTrailingTrivia(0), CS.SyntaxFactory.Space})
                        ElseIf csIdentifierTrailingTrivia.Count = 0 Then
                            csMovedTrailingSpace = csMovedTrailingSpace.Add(CS.SyntaxFactory.Space)
                        End If
                        vbEnumSatement = DirectCast(e.Value.Accept(Me), VBS.StatementSyntax)
                        csSeparatorTrailingTrivia = csSeparators(e.index).TrailingTrivia
                        If csSeparatorTrailingTrivia.Any Then
                            If csSeparatorTrailingTrivia(0).IsKind(CS.SyntaxKind.WhitespaceTrivia) Then
                                csMovedTrailingSpace = csMovedTrailingSpace.Add(csSeparatorTrailingTrivia(0))
                                csSeparatorTrailingTrivia = csSeparatorTrailingTrivia.RemoveAt(0)
                                csSeparatorTrailingTrivia = csSeparatorTrailingTrivia.InsertRange(0, csMovedTrailingSpace)
                            ElseIf csSeparatorTrailingTrivia(0).IsWhitespace Then
                            End If
                        End If
                        If e.IsFirst Then
                            members.Add(vbEnumSatement.WithPrependedLeadingTrivia(leadingTrivia).WithTrailingTrivia(csSeparatorTrailingTrivia.ConvertTriviaList()))
                        Else
                            members.Add(vbEnumSatement.WithTrailingTrivia(csSeparatorTrailingTrivia.ConvertTriviaList()))
                        End If
                        csMovedTrailingSpace = New SyntaxTriviaList
                        csMovedTrailingSpace = csMovedTrailingSpace.Add(CS.SyntaxFactory.Space)
                    Next
                    If csSeparators.Count = 0 OrElse csSeparators.Count <> csMembers.Count Then
                        csSeparatorTrailingTrivia = New SyntaxTriviaList
                    Else
                        csSeparatorTrailingTrivia = csSeparators.Last.TrailingTrivia
                    End If
                    If csIdentifierTrailingTrivia.Any AndAlso csIdentifierTrailingTrivia(0).IsWhitespace Then
                        csMovedTrailingSpace = csMovedTrailingSpace.Add(csIdentifierTrailingTrivia(0))
                    ElseIf csIdentifierTrailingTrivia.Count = 0 Then
                    Else
                        Stop
                    End If
                    vbEnumSatement = DirectCast(csMembers.Last.Accept(Me), VBS.StatementSyntax)
                    If csMovedTrailingSpace.Any Then
                        Dim memberLastTrailingTrivia As SyntaxTriviaList = csMembers.Last.GetTrailingTrivia
                        If csSeparatorTrailingTrivia.Any AndAlso csSeparatorTrailingTrivia(0).IsKind(CS.SyntaxKind.WhitespaceTrivia) Then
                            csMovedTrailingSpace = csMovedTrailingSpace.Add(csSeparatorTrailingTrivia(0))
                            csSeparatorTrailingTrivia = csSeparatorTrailingTrivia.RemoveAt(0)
                            csSeparatorTrailingTrivia = csSeparatorTrailingTrivia.InsertRange(0, csMovedTrailingSpace)
                        ElseIf csMembers.Last.HasTrailingTrivia Then
                            If memberLastTrailingTrivia(0).IsKind(CS.SyntaxKind.WhitespaceTrivia) Then
                                csMovedTrailingSpace = csMovedTrailingSpace.Add(memberLastTrailingTrivia(0))
                                memberLastTrailingTrivia = memberLastTrailingTrivia.RemoveAt(0)
                                memberLastTrailingTrivia = memberLastTrailingTrivia.InsertRange(0, csMovedTrailingSpace)
                                csMembers = csMembers.Replace(csMembers.Last, csMembers.Last.WithTrailingTrivia(memberLastTrailingTrivia))
                            End If
                        End If
                    End If

                    If csMembers.Count = 1 Then
                        members.Add(DirectCast(csMembers.Last.Accept(Me), VBS.StatementSyntax).WithPrependedLeadingTrivia(leadingTrivia).WithAppendedTrailingTrivia(csSeparatorTrailingTrivia.ConvertTriviaList()))
                    Else
                        members.Add(DirectCast(csMembers.Last.Accept(Me), VBS.StatementSyntax).WithAppendedTrailingTrivia(csSeparatorTrailingTrivia.ConvertTriviaList()))
                    End If
                End If

                Dim baseType As VBS.TypeSyntax = DirectCast(node.BaseList?.Types.Single().Accept(Me), VBS.TypeSyntax)
                Dim listOfAttributes As SyntaxList(Of VBS.AttributeListSyntax) = Factory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))
                Dim modifiers As SyntaxTokenList = Factory.TokenList(ConvertModifiers(node.Modifiers, Me.IsModule, TokenContext.Global))
                Dim underlyingType As VBS.SimpleAsClauseSyntax = If(baseType Is Nothing, Nothing, Factory.SimpleAsClause(baseType))
                Dim enumStatement As VBS.EnumStatementSyntax = DirectCast(Factory.EnumStatement(listOfAttributes,
                                                                                                modifiers,
                                                                                                EnumKeyword.WithConvertedTriviaFrom(node.EnumKeyword),
                                                                                                identifier:=GenerateSafeVBToken(id:=node.Identifier, Node:=node, usedIdentifiers:=_usedIdentifiers, Model:=_mSemanticModel),
                                                                                                underlyingType).
                                                                                       RestructureAttributesAndModifiers(listOfAttributes.Any, modifiers.Any), VBS.EnumStatementSyntax)

                Dim endBlockStmt As VBS.EndBlockStatementSyntax = FactoryEndBlockStatement(VB.SyntaxKind.EndEnumStatement, EnumKeyword, CollectConvertedTokenTrivia(node.CloseBraceToken, GetLeading:=True, GetTrailing:=True))
                Dim enumBlock As VBS.EnumBlockSyntax = Factory.EnumBlock(enumStatement.WithTrailingEOL,
                                                                           Factory.List(members),
                                                                           endBlockStmt
                                                                           )
                Return PrependStatementWithMarkedStatementTrivia(node, enumBlock)
            End Function

            Public Overrides Function VisitEnumMemberDeclaration(node As CSS.EnumMemberDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim initializer As VBS.ExpressionSyntax = DirectCast(node.EqualsValue?.Value.Accept(Me), VBS.ExpressionSyntax)
                Return Factory.EnumMemberDeclaration(Factory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax))), GenerateSafeVBToken(node.Identifier, node, _usedIdentifiers, _mSemanticModel), initializer:=If(initializer Is Nothing, Nothing, Factory.EqualsValue(initializer))).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitExplicitInterfaceSpecifier(node As CSS.ExplicitInterfaceSpecifierSyntax) As VB.VisualBasicSyntaxNode
                Return node.Name.Accept(Me)
            End Function

            Public Overrides Function VisitExternAliasDirective(node As CSS.ExternAliasDirectiveSyntax) As VB.VisualBasicSyntaxNode
                Return FlagUnsupportedStatements(node, "Extern Alias", CommentOutOriginalStatements:=True)
            End Function

            Public Overrides Function VisitInterfaceDeclaration(node As CSS.InterfaceDeclarationSyntax) As VB.VisualBasicSyntaxNode
                SyncLock _originalRequest.UsedStacks
                    If _originalRequest.ImplementedMembers.Any Then
                        _originalRequest.ImplementedMembersStack.Push(_originalRequest.ImplementedMembers)
                        _originalRequest.ImplementedMembers = (New List(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol)))).ToImmutableArray
                    End If
                End SyncLock
                Dim listOfAttributes As SyntaxList(Of VBS.AttributeListSyntax) = Factory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))
                Dim modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, Me.IsModule, TokenContext.InterfaceOrModule).ToList
                If node.Modifiers.Contains(CS.SyntaxKind.UnsafeKeyword) Then
                    Return FlagUnsupportedStatements(node, "unsafe interfaces", CommentOutOriginalStatements:=True)
                End If
                Dim members As New List(Of VBS.StatementSyntax)
                For Each e As IndexClass(Of CSS.MemberDeclarationSyntax) In node.Members.WithIndex
                    If e.IsFirst AndAlso node.OpenBraceToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        members.Add(DirectCast(e.Value.Accept(Me), VBS.StatementSyntax).WithPrependedLeadingTrivia(node.OpenBraceToken.LeadingTrivia.ConvertTriviaList()))
                    Else
                        members.Add(DirectCast(e.Value.Accept(Me), VBS.StatementSyntax))
                    End If
                Next
                Dim identifier As SyntaxToken = GenerateSafeVBToken(node.Identifier, node, _usedIdentifiers, _mSemanticModel)
                Dim typeParameterList As VBS.TypeParameterListSyntax = DirectCast(node.TypeParameterList?.Accept(Me), VBS.TypeParameterListSyntax)
                Dim statementLeadingTrivia As SyntaxTriviaList
                If node.Modifiers.Any AndAlso modifiers.Count = 0 Then
                    statementLeadingTrivia = statementLeadingTrivia.AddRange(node.Modifiers(0).LeadingTrivia.ConvertTriviaList())
                    Return FlagUnsupportedStatements(node, "Directive within statement", CommentOutOriginalStatements:=True)
                Else
                    statementLeadingTrivia = statementLeadingTrivia.Add(Factory.Space)
                End If
                Dim interfaceStatement As VBS.InterfaceStatementSyntax = DirectCast(
                                    Factory.InterfaceStatement(listOfAttributes,
                                                                    Factory.TokenList(modifiers),
                                                                    InterfaceKeyword.WithLeadingTrivia(statementLeadingTrivia),
                                                                    identifier,
                                                                    typeParameterList).RestructureAttributesAndModifiers(listOfAttributes.Any, modifiers.Any),
                                                                            VBS.InterfaceStatementSyntax
                                                                            ).WithTrailingEOL
                Dim [inherits] As List(Of VBS.InheritsStatementSyntax) = New List(Of VBS.InheritsStatementSyntax)()
                Dim [implements] As List(Of VBS.ImplementsStatementSyntax) = New List(Of VBS.ImplementsStatementSyntax)()
                Me.ConvertBaseList(node, [inherits], [implements])
                Dim inheritsStatementList As SyntaxList(Of VBS.InheritsStatementSyntax) = Factory.List([inherits])
                Dim implementsStatementList As SyntaxList(Of VBS.ImplementsStatementSyntax) = Factory.List([implements])
                Dim statementList As SyntaxList(Of VBS.StatementSyntax) = Factory.List(members)
                Dim endInterfaceStatement As VBS.EndBlockStatementSyntax = FactoryEndBlockStatement(VB.SyntaxKind.EndInterfaceStatement, InterfaceKeyword, CollectConvertedTokenTrivia(node.CloseBraceToken, GetLeading:=True, GetTrailing:=True))

                Return Factory.InterfaceBlock(interfaceStatement,
                                              inheritsStatementList,
                                              implementsStatementList,
                                              statementList,
                                              endInterfaceStatement
                                             ).WithConvertedLeadingTriviaFrom(node)
            End Function

            Public Overrides Function VisitNamespaceDeclaration(node As CSS.NamespaceDeclarationSyntax) As VB.VisualBasicSyntaxNode
                SyncLock _originalRequest.UsedStacks
                    If _originalRequest.UsedStacks.Count > 0 Then
                        _usedIdentifiers = DirectCast(_originalRequest.UsedStacks.Pop, Dictionary(Of String, SymbolTableEntry))
                    End If
                End SyncLock

                For Each [using] As CSS.UsingDirectiveSyntax In node.Usings
                    [using].Accept(Me)
                Next

                For Each extern As CSS.ExternAliasDirectiveSyntax In node.Externs
                    extern.Accept(Me)
                Next

                Dim newOpenBrackTrivia As SyntaxTriviaList = CollectConvertedTokenTrivia(node.OpenBraceToken, GetLeading:=True, GetTrailing:=True)
                Dim members As New List(Of VBS.StatementSyntax)

                For Each e As IndexClass(Of CSS.MemberDeclarationSyntax) In node.Members.WithIndex
                    If _originalRequest.CancelToken.IsCancellationRequested Then
                        Throw New OperationCanceledException
                    End If
                    Dim item As VBS.StatementSyntax = DirectCast(e.Value.Accept(Me), VBS.StatementSyntax)
                    If e.IsFirst AndAlso newOpenBrackTrivia.Any AndAlso newOpenBrackTrivia.ContainsCommentOrDirectiveTrivia Then
                        item = item.WithPrependedLeadingTrivia(newOpenBrackTrivia)
                    End If
                    members.Add(item)
                    If e.IsLast Then
                        members(e.index) = members.Last.
                            WithAppendedTrailingTrivia(node.CloseBraceToken.LeadingTrivia.ConvertTriviaList()).
                            WithAppendedTrailingTrivia(node.CloseBraceToken.TrailingTrivia.ConvertTriviaList())
                    End If
                Next

                Return Factory.NamespaceBlock(Factory.NamespaceStatement(NamespaceKeyword,
                                                                         DirectCast(node.Name.Accept(Me), VBS.NameSyntax)
                                                                        ).WithTrailingEOL(),
                                              Factory.List(members),
                                              Factory.EndNamespaceStatement(EndKeyword.WithTrailingTrivia(Factory.Space), NamespaceKeyword)
                                             ) _
                                            .WithConvertedLeadingTriviaFrom(node.NamespaceKeyword) _
                                            .WithUniqueLeadingTrivia(VBHeaderLeadingTrivia) _
                                            .WithTrailingTrivia(node.GetTrailingTrivia.ConvertTriviaList)
            End Function

            Public Overrides Function VisitStructDeclaration(node As CSS.StructDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim [inherits] As List(Of VBS.InheritsStatementSyntax) = New List(Of VBS.InheritsStatementSyntax)
                Dim [implements] As List(Of VBS.ImplementsStatementSyntax) = New List(Of VBS.ImplementsStatementSyntax)
                If _originalRequest.ImplementedMembers.Any Then
                    _originalRequest.ImplementedMembersStack.Push(_originalRequest.ImplementedMembers)
                    _originalRequest.ImplementedMembers = (New List(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol)))).ToImmutableArray
                End If
                Me.ConvertBaseList(node, [inherits], [implements], _originalRequest.ImplementedMembers)
                Dim members As New List(Of VBS.StatementSyntax)
                For Each m As CSS.MemberDeclarationSyntax In node.Members
                    Dim item As VBS.StatementSyntax = DirectCast(m.Accept(Me), VBS.StatementSyntax)
                    If item Is Nothing Then
                        members.Add(Factory.EmptyStatement.WithConvertedTriviaFrom(m))
                    Else
                        members.AddRange(ReplaceOneStatementWithMarkedStatements(m, item))
                    End If
                Next
                SyncLock _originalRequest.UsedStacks
                    If _originalRequest.ImplementedMembersStack.Count > 0 Then
                        _originalRequest.ImplementedMembers = DirectCast(_originalRequest.ImplementedMembersStack.Pop, ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))))
                    End If
                End SyncLock
                If members.Any Then
                    members(0) = members(0).WithPrependedLeadingTrivia(CollectConvertedTokenTrivia(node.OpenBraceToken, GetLeading:=True, GetTrailing:=False))
                End If
                Dim listOfAttributes As SyntaxList(Of VBS.AttributeListSyntax) = Factory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))
                Dim typeParameterList As VBS.TypeParameterListSyntax = DirectCast(node.TypeParameterList?.Accept(Me), VBS.TypeParameterListSyntax)
                Dim modifiers As SyntaxTokenList = Factory.TokenList(ConvertModifiers(node.Modifiers, Me.IsModule, TokenContext.Struct))
                Dim structureStmt As VBS.StructureStatementSyntax
                structureStmt = DirectCast(Factory.StructureStatement(listOfAttributes,
                                                                            Factory.TokenList(modifiers),
                                                                            StructureKeyword.WithConvertedTriviaFrom(node.Keyword),
                                                                            GenerateSafeVBToken(node.Identifier, node, _usedIdentifiers, _mSemanticModel),
                                                                            typeParameterList
                                                                            ).RestructureAttributesAndModifiers(listOfAttributes.Any, modifiers.Any), VBS.StructureStatementSyntax).WithTrailingEOL

                Dim structureBlock As VBS.StructureBlockSyntax = Factory.StructureBlock(
                                                    structureStmt,
                                                    Factory.List([inherits]),
                                                    Factory.List([implements]),
                                                    Factory.List(members),
                                                    Factory.EndStructureStatement(EndKeyword.WithTrailingTrivia(Factory.Space), StructureKeyword).WithConvertedTriviaFrom(node.CloseBraceToken)
                                                    )
                Dim errorModifiers As New List(Of String)
                For Each t As SyntaxToken In node.Modifiers
                    Select Case t.Text
                        Case "unsafe"
                            errorModifiers.Add("unsafe")
                        Case "ref"
                            errorModifiers.Add("unsafe")
                    End Select
                Next

                ' These errors are handled elsewhere just ignore
                ReplaceOneStatementWithMarkedStatements(node, structureBlock)
                If errorModifiers.Any Then
                    structureBlock = structureBlock.WithPrependedLeadingTrivia(Factory.CommentTrivia($"' TODO TASK: VB has no direct equivalent to C# {String.Join(" or ", errorModifiers)} Structure"))
                End If
                Return structureBlock
            End Function

            Public Overrides Function VisitUsingDirective(node As CSS.UsingDirectiveSyntax) As VB.VisualBasicSyntaxNode
                SyncLock _originalRequest.UsedStacks
                    If _usedIdentifiers.Any Then
                        _originalRequest.UsedStacks.Push(_usedIdentifiers)
                        _usedIdentifiers.Clear()
                    End If
                End SyncLock
                Dim importsName As VBS.NameSyntax
                Dim [alias] As VBS.ImportAliasClauseSyntax = Nothing
                Dim identifier As SyntaxToken
                If node.Alias IsNot Nothing Then
                    Dim aliasName As CSS.IdentifierNameSyntax = node.Alias.Name
                    identifier = GenerateSafeVBToken(aliasName.Identifier, node, _usedIdentifiers, _mSemanticModel)
                    [alias] = Factory.ImportAliasClause(identifier)
                End If
                importsName = DirectCast(node.Name.Accept(Me), VBS.NameSyntax)
                Dim importsNameString As String = importsName.ToString
                If importsName.IsKind(VB.SyntaxKind.IdentifierName) AndAlso
                        VB.SyntaxFacts.IsKeywordKind(VB.SyntaxFacts.GetKeywordKind(importsNameString)) Then
                    importsName = Factory.IdentifierName($"[{importsNameString}]")
                End If

                Dim clause As VBS.ImportsClauseSyntax = Factory.SimpleImportsClause([alias], importsName)

                Dim import As VBS.ImportsStatementSyntax
                If AllImports.Any Then
                    import = Factory.ImportsStatement(Factory.SingletonSeparatedList(clause)).WithConvertedTriviaFrom(node)
                Else
                    import = Factory.ImportsStatement(Factory.SingletonSeparatedList(clause)) _
                                                    .WithConvertedLeadingTriviaFrom(node) _
                                                    .WithUniqueLeadingTrivia(VBHeaderLeadingTrivia) _
                                                    .WithTrailingTrivia(node.GetTrailingTrivia.ConvertTriviaList)
                End If
                Dim matchNotFound As Boolean = True
                If AllImports.Any Then
                    For Each importStmt As VBS.ImportsStatementSyntax In AllImports
                        Dim importsClause As VBS.SimpleImportsClauseSyntax = DirectCast(importStmt.ImportsClauses(0), VBS.SimpleImportsClauseSyntax)
                        If importsClause.Alias IsNot Nothing AndAlso importsClause.Alias.ToString = [alias]?.ToString Then
                            matchNotFound = False
                            Exit For
                        Else
                            If importsClause.Name.ToString.RemoveBrackets = importsNameString.RemoveBrackets Then
                                matchNotFound = False
                                Exit For
                            End If
                            If TypeOf node.Parent Is CSS.NamespaceDeclarationSyntax Then
                                If importsClause.Name.ToString.TrimStart($"{CType(node.Parent, CSS.NamespaceDeclarationSyntax).Name}.") = importsNameString.RemoveBrackets Then
                                    matchNotFound = False
                                End If
                            End If
                        End If
                    Next
                End If
                If matchNotFound Then
                    AllImports.Add(import)
                End If

                SyncLock _originalRequest.UsedStacks
                    If _originalRequest.UsedStacks.Count > 0 Then
                        _usedIdentifiers = DirectCast(_originalRequest.UsedStacks.Pop, Dictionary(Of String, SymbolTableEntry))
                    End If
                End SyncLock
                Return Nothing
            End Function

        End Class

    End Class

End Namespace
