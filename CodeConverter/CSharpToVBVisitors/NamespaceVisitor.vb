' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Collections.Immutable
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic

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
            Private Const DiscardHelperCode As String = "
Private Shared  WriteOnly Property __ As Object
    Set
    End Set
End Property
"
            Private Shared ReadOnly s_leadingDirectiveMovedComment As SyntaxTrivia = Factory.CommentTrivia("' This directive was moved from leading statement trivia")
            Private Shared ReadOnly s_trailingDirectiveMovedComment As SyntaxTrivia = Factory.CommentTrivia("' This directive was moved from trailing statement trivia")


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

            Private Sub ConvertBaseList(_Type As CSS.BaseTypeDeclarationSyntax, [inherits] As List(Of InheritsStatementSyntax), [implements] As List(Of ImplementsStatementSyntax), ByRef MovedFinalTrivia As SyntaxTriviaList, ByRef Optional ImplementedMembers As ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))) = Nothing)
                Dim typeSyntaxArray As TypeSyntax()
                Dim startImplementsIndex As Integer = 0
                Select Case _Type.Kind()
                    Case CS.SyntaxKind.ClassDeclaration
                        Dim classOrInterface As CSS.TypeSyntax = _Type.BaseList?.Types.FirstOrDefault()?.Type
                        If classOrInterface Is Nothing Then
                            Exit Sub
                        End If
                        Dim classOrInterfaceSymbol As ISymbol = ModelExtensions.GetSymbolInfo(_semanticModel, classOrInterface).Symbol
                        If classOrInterfaceSymbol?.IsInterfaceType() Then
                            Dim typeSyntaxList As New List(Of TypeSyntax)

                            FilterLeadingTrivia(ConvertTriviaList(_Type.BaseList.ColonToken.LeadingTrivia), MovedFinalTrivia)
                            For Each e As IndexClass(Of CSS.BaseTypeSyntax) In _Type.BaseList?.Types.WithIndex
                                Dim item1 As TypeSyntax = DirectCast(e.Value.Type.Accept(Me), TypeSyntax)
                                item1 = item1.WithLeadingTrivia(FilterLeadingTrivia(item1.GetLeadingTrivia, MovedFinalTrivia))
                                item1 = item1.WithTrailingTrivia(FilterTrailingTrivia(item1.GetLeadingTrivia, MovedFinalTrivia))
                                If e.IsLast Then
                                    item1 = item1.WithTrailingEOL
                                End If
                                typeSyntaxList.Add(item1)
                            Next
                            typeSyntaxArray = typeSyntaxList.ToArray
                            startImplementsIndex = 0
                        Else
                            Dim typeNode As VB.VisualBasicSyntaxNode = classOrInterface.Accept(Me)
                            If typeNode.HasLeadingTrivia Then
                                Dim typeNodeLeadingTrivia As SyntaxTriviaList = typeNode.GetLeadingTrivia
                                If typeNodeLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                    [inherits].Add(Factory.InheritsStatement(DirectCast(typeNode.WithLeadingTrivia(Factory.Space), TypeSyntax)).WithLeadingTrivia(typeNodeLeadingTrivia))
                                Else
                                    [inherits].Add(Factory.InheritsStatement(DirectCast(typeNode, TypeSyntax)))
                                End If
                            Else
                                [inherits].Add(Factory.InheritsStatement(DirectCast(typeNode, TypeSyntax)))
                            End If
                            startImplementsIndex = 1
                            Dim typeSyntaxList As New List(Of TypeSyntax)
                            Dim separatorList As List(Of SyntaxToken) = _Type.BaseList.Types.GetSeparators.ToList
                            For Each e As IndexClass(Of CSS.BaseTypeSyntax) In _Type.BaseList.Types.WithIndex
                                If e.index < separatorList.Count Then
                                    FilterLeadingTrivia(ConvertTriviaList(separatorList(e.index).LeadingTrivia), MovedFinalTrivia)
                                End If
                                If e.IsFirst Then
                                    Continue For
                                End If
                                typeSyntaxList.Add(DirectCast(e.Value.Type.Accept(Me).WithTrailingTrivia(Factory.Space), TypeSyntax))
                            Next
                            typeSyntaxArray = typeSyntaxList.ToArray()
                        End If
                        If typeSyntaxArray.Any Then
                            [implements].Add(Factory.ImplementsStatement(typeSyntaxArray))
                            Dim implementsClauses As IEnumerable(Of CSS.TypeSyntax) = _Type.BaseList?.Types.Skip(startImplementsIndex).Select(Function(t As CSS.BaseTypeSyntax) t.Type)
                            For Each implementsClause As CSS.TypeSyntax In implementsClauses
                                Dim classOrStructDecl As SyntaxNode = Nothing
                                Dim classOrStructType As INamedTypeSymbol = Nothing
                                Dim interfaceTypes As IEnumerable(Of INamedTypeSymbol) = Nothing

                                If implementsClause.TryInitializeState(_semanticModel, classOrStructDecl, classOrStructType, interfaceTypes, _originalRequest.CancelToken) Then
                                    Dim items As ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))) = classOrStructType.GetAllImplementedMembers(interfaceTypes, _originalRequest.CancelToken)
                                    ImplementedMembers = ImplementedMembers.AddRange(items)
                                End If
                            Next
                        End If

                    Case CS.SyntaxKind.StructDeclaration
                        typeSyntaxArray = _Type.BaseList?.Types.Select(Function(t As CSS.BaseTypeSyntax) DirectCast(t.Type.Accept(Me), TypeSyntax)).ToArray()
                        If typeSyntaxArray?.Any Then
                            [implements].Add(Factory.ImplementsStatement(typeSyntaxArray))
                        End If
                    Case CS.SyntaxKind.InterfaceDeclaration
                        Dim baseList As New List(Of TypeSyntax)
                        Dim newLeadingTrivia As New SyntaxTriviaList
                        If _Type.BaseList IsNot Nothing Then
                            newLeadingTrivia = _Type.BaseList.ColonToken.LeadingTrivia.ConvertTriviaList()
                            Dim csSeparators As List(Of SyntaxToken) = _Type.BaseList.Types.GetSeparators.ToList
                            For Each e As IndexClass(Of CSS.BaseTypeSyntax) In _Type.BaseList.Types.WithIndex
                                Dim item As TypeSyntax = DirectCast(e.Value.Type.Accept(Me), TypeSyntax)
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

            Private Iterator Function PatchInlineHelpers(node As CSS.BaseTypeDeclarationSyntax, IsModule As Boolean) As IEnumerable(Of StatementSyntax)
                If InlineAssignHelperMarkers.Contains(node) Then
                    InlineAssignHelperMarkers.Remove(node)
                    Yield TryCast(Factory.ParseSyntaxTree(InlineAssignHelperCode.Replace("Shared ", If(IsModule, "", "Shared "), StringComparison.Ordinal)).GetRoot().ChildNodes().FirstOrDefault(), StatementSyntax)
                End If
                If DiscardHelperMarkers.Contains(node) Then
                    DiscardHelperMarkers.Remove(node)
                    Yield TryCast(Factory.ParseSyntaxTree(DiscardHelperCode.Replace("Shared ", If(IsModule, "", "Shared "), StringComparison.Ordinal)).GetRoot().ChildNodes().FirstOrDefault(), StatementSyntax)
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
                Dim members As New List(Of StatementSyntax)
                Dim membersLastIndex As Integer = node.Members.Count - 1
                Dim [inherits] As New List(Of InheritsStatementSyntax)()
                Dim [implements] As New List(Of ImplementsStatementSyntax)()

                Dim finalTrailingTrivia As SyntaxTriviaList

                Me.ConvertBaseList(node, [inherits], [implements], finalTrailingTrivia, _originalRequest.ImplementedMembers)
                Dim staticMethodCount As Integer = 0
                Dim methodCount As Integer = 0
                Dim classType As ITypeSymbol = CType(_semanticModel.GetDeclaredSymbol(node), ITypeSymbol)
                For Each e As IndexClass(Of CSS.MemberDeclarationSyntax) In node.Members.WithIndex
                    If _originalRequest.CancelToken.IsCancellationRequested Then
                        Throw New OperationCanceledException
                    End If
                    Dim m As CSS.MemberDeclarationSyntax = e.Value
                    Dim statement As StatementSyntax = DirectCast(m.Accept(Me), StatementSyntax).RemoveExtraLeadingEOL.WithTrailingEOL
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
                        If TypeOf statement Is MethodBlockSyntax Then
                            Dim block As MethodBlockSyntax = CType(statement, MethodBlockSyntax)
                            modifiers = block.BlockStatement.Modifiers.ToList
                        ElseIf TypeOf statement Is MethodStatementSyntax Then
                            Dim block As MethodStatementSyntax = CType(statement, MethodStatementSyntax)
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

                Dim id As SyntaxToken = GenerateSafeVBToken(node.Identifier, node, _usedIdentifiers, _semanticModel).WithConvertedTriviaFrom(node.Identifier)
                id = AdjustTokenLeadingTrivia(id)

                members.AddRange(Me.PatchInlineHelpers(node, Me.IsModule))

                Dim listOfAttributes As SyntaxList(Of AttributeListSyntax) = Factory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), AttributeListSyntax)))
                Dim typeParameterList As TypeParameterListSyntax = DirectCast(node.TypeParameterList?.Accept(Me), TypeParameterListSyntax)
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
                    Dim moduleStmt As ModuleStatementSyntax = DirectCast(Factory.ModuleStatement(listOfAttributes,
                                                                                                     moduleModifiers,
                                                                                                     moduleKeywordWithTrivia,
                                                                                                     id,
                                                                                                     typeParameterList
                                                                                                    ).WithPrependedLeadingTrivia(prependedTrivia) _
                                                                                                     .RestructureAttributesAndModifiers(listOfAttributes.Any, moduleModifiers.Any),
                                                                                ModuleStatementSyntax).WithTrailingEOL

                    moduleStmt = DirectCast(PrependStatementWithMarkedStatementTrivia(node, moduleStmt), ModuleStatementSyntax)
                    Dim endModule As EndBlockStatementSyntax = FactoryEndBlockStatement(VB.SyntaxKind.EndModuleStatement, ModuleKeyword, CollectConvertedTokenTrivia(node.CloseBraceToken, GetLeading:=True, GetTrailing:=True))
                    Dim moduleBlock As ModuleBlockSyntax = Factory.ModuleBlock(
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
                    If classKeywordWithTrivia.TrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                        Stop
                    Else
                        classKeywordWithTrivia = classKeywordWithTrivia.WithTrailingTrivia(Factory.Space())
                    End If
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
                    Dim classStmt As ClassStatementSyntax = DirectCast(Factory.ClassStatement(
                                                                            listOfAttributes,
                                                                            Factory.TokenList(classModifiers),
                                                                            classKeywordWithTrivia,
                                                                            id,
                                                                            typeParameterList
                                                                            ).WithPrependedLeadingTrivia(prependedTrivia).
                                                                            RestructureAttributesAndModifiers(listOfAttributes.Any, classModifiers.Any), ClassStatementSyntax)
                    classStmt = DirectCast(PrependStatementWithMarkedStatementTrivia(node, classStmt), ClassStatementSyntax)
                    If [inherits].Count = 0 AndAlso [implements].Count = 0 Then
                        classStmt = classStmt.WithTrailingEOL
                    Else
                        Dim movedTrailingTrivia As SyntaxTriviaList
                        If classStmt.GetTrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                            Dim initialTriviaList As SyntaxTriviaList = classStmt.GetTrailingTrivia
                            classStmt = classStmt.WithTrailingTrivia(FilterTrailingTrivia(initialTriviaList, movedTrailingTrivia)).WithTrailingEOL
                        End If
                        If movedTrailingTrivia.Any Or finalTrailingTrivia.Any Then
                            If [inherits].Any Then
                                For Each e As IndexClass(Of InheritsStatementSyntax) In [inherits].WithIndex
                                    [inherits](e.index) = [inherits](e.index).WithLeadingTrivia(FilterLeadingTrivia(e.Value.GetLeadingTrivia, movedTrailingTrivia))
                                    [inherits](e.index) = [inherits](e.index).WithTrailingTrivia(FilterTrailingTrivia(e.Value.GetLeadingTrivia, movedTrailingTrivia)).WithTrailingEOL
                                Next
                            End If
                            If [implements].Any Then
                                For Each e As IndexClass(Of ImplementsStatementSyntax) In [implements].WithIndex
                                    [implements](e.index) = [implements](e.index).WithLeadingTrivia(FilterLeadingTrivia(e.Value.GetLeadingTrivia, movedTrailingTrivia))
                                    [implements](e.index) = [implements](e.index).WithTrailingTrivia(FilterTrailingTrivia(e.Value.GetLeadingTrivia, movedTrailingTrivia)).WithTrailingEOL
                                Next
                                movedTrailingTrivia = movedTrailingTrivia.AddRange(finalTrailingTrivia)
                                [implements]([implements].Count - 1) = [implements].Last.WithAppendedTrailingTrivia(movedTrailingTrivia).WithTrailingEOL
                            Else
                                movedTrailingTrivia = movedTrailingTrivia.AddRange(finalTrailingTrivia)
                                [inherits]([inherits].Count - 1) = [inherits].Last.WithAppendedTrailingTrivia(movedTrailingTrivia).WithTrailingEOL
                            End If
                        End If
                    End If
                    Dim endClass As EndBlockStatementSyntax = FactoryEndBlockStatement(VB.SyntaxKind.EndClassStatement, ClassKeyWord, CollectConvertedTokenTrivia(node.CloseBraceToken, GetLeading:=True, GetTrailing:=True))
                    Dim classBlock As ClassBlockSyntax = Factory.ClassBlock(
                                                                                classStmt.WithTrailingEOL,
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

            Private Shared Function FilterLeadingTrivia(initialTriviaList As SyntaxTriviaList, ByRef newLeadingingTrivia As SyntaxTriviaList) As SyntaxTriviaList
                Dim replacementTrailingTrivia As SyntaxTriviaList
                For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                    Dim triviaComment As SyntaxTrivia = Factory.CommentTrivia("This directive was moved from leading statement trivia")
                    Dim trivia As SyntaxTrivia = e.Value
                    Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(initialTriviaList, e.index, LookaheadCount:=1)
                    Dim foundSpace As Boolean = False
                    Dim useReplacementTrailingTrivia As Boolean = True
                    Select Case trivia.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            If nextTrivia.IsKind(VB.SyntaxKind.None) OrElse nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                Continue For
                            End If
                            If useReplacementTrailingTrivia Then
                                replacementTrailingTrivia = replacementTrailingTrivia.Add(Factory.Space)
                            Else
                                newLeadingingTrivia = newLeadingingTrivia.Add(Factory.Space)
                            End If
                            foundSpace = True
                        Case VB.SyntaxKind.EndOfLineTrivia
                            foundSpace = False
                        Case VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.ElseIfDirectiveTrivia, VB.SyntaxKind.ElseDirectiveTrivia, VB.SyntaxKind.EndIfDirectiveTrivia, VB.SyntaxKind.DisabledTextTrivia
                            If e.IsFirst Then
                                newLeadingingTrivia = newLeadingingTrivia.Add(VBEOLTrivia)
                            End If
                            newLeadingingTrivia = newLeadingingTrivia.Add(trivia)
                            If nextTrivia.IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                                newLeadingingTrivia = newLeadingingTrivia.Add(VBEOLTrivia)
                                newLeadingingTrivia = newLeadingingTrivia.Add(nextTrivia)
                                e.MoveNext()
                            End If
                            useReplacementTrailingTrivia = True
                        Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                            If useReplacementTrailingTrivia Then
                                If Not foundSpace Then
                                    replacementTrailingTrivia = replacementTrailingTrivia.Add(Factory.Space)
                                End If
                                replacementTrailingTrivia = replacementTrailingTrivia.AddRange({LineContinuation, Factory.Space, trivia})
                            Else
                                If Not foundSpace Then
                                    newLeadingingTrivia = newLeadingingTrivia.Add(Factory.Space)
                                End If
                                newLeadingingTrivia = newLeadingingTrivia.AddRange({LineContinuation, Factory.Space, trivia})
                            End If
                        Case Else
                            Stop
                    End Select
                Next

                Return replacementTrailingTrivia
            End Function

            Private Shared Function FilterTrailingTrivia(initialTriviaList As SyntaxTriviaList, ByRef newTrailingTrivia As SyntaxTriviaList) As SyntaxTriviaList
                Dim replacementTrailingTrivia As SyntaxTriviaList
                For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                    Dim trivia As SyntaxTrivia = e.Value
                    Dim nextTrivia As SyntaxTrivia = GetForwardTriviaOrDefault(initialTriviaList, e.index, LookaheadCount:=1)
                    Dim foundSpace As Boolean = False
                    Dim useReplacementTrailingTrivia As Boolean = True
                    Select Case trivia.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            If nextTrivia.IsKind(VB.SyntaxKind.None) OrElse nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                Continue For
                            End If
                            If useReplacementTrailingTrivia Then
                                replacementTrailingTrivia = replacementTrailingTrivia.Add(Factory.Space)
                            Else
                                newTrailingTrivia = newTrailingTrivia.Add(Factory.Space)
                            End If
                            foundSpace = True
                        Case VB.SyntaxKind.EndOfLineTrivia
                            foundSpace = False
                        Case VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.ElseIfDirectiveTrivia, VB.SyntaxKind.ElseDirectiveTrivia, VB.SyntaxKind.EndIfDirectiveTrivia, VB.SyntaxKind.DisabledTextTrivia
                            If e.IsFirst Then
                                newTrailingTrivia = newTrailingTrivia.Add(VBEOLTrivia)
                            End If
                            newTrailingTrivia = newTrailingTrivia.Add(trivia)

                            If nextTrivia.IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                                newTrailingTrivia = newTrailingTrivia.Add(VBEOLTrivia)
                                newTrailingTrivia = newTrailingTrivia.Add(nextTrivia)
                                e.MoveNext()
                            End If
                            useReplacementTrailingTrivia = True
                        Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                            If useReplacementTrailingTrivia Then
                                If Not foundSpace Then
                                    replacementTrailingTrivia = replacementTrailingTrivia.Add(Factory.Space)
                                End If
                                replacementTrailingTrivia = replacementTrailingTrivia.AddRange({LineContinuation, Factory.Space, trivia})
                            Else
                                If Not foundSpace Then
                                    newTrailingTrivia = newTrailingTrivia.Add(Factory.Space)
                                End If
                                newTrailingTrivia = newTrailingTrivia.AddRange({LineContinuation, Factory.Space, trivia})
                            End If
                        Case Else
                            Stop
                    End Select
                Next

                Return replacementTrailingTrivia
            End Function

            Public Overrides Function VisitDelegateDeclaration(node As CSS.DelegateDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim identifier As SyntaxToken = GenerateSafeVBToken(node.Identifier, node, _usedIdentifiers, _semanticModel)
                Dim methodInfo As INamedTypeSymbol = TryCast(ModelExtensions.GetDeclaredSymbol(_semanticModel, node), INamedTypeSymbol)
                Dim attrLists As SyntaxList(Of AttributeListSyntax) = Factory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), AttributeListSyntax)))
                Dim modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, Me.IsModule, TokenContext.Global).ToList
                Dim typeParamList As TypeParameterListSyntax = DirectCast(node.TypeParameterList?.Accept(Me), TypeParameterListSyntax)?.WithoutTrailingTrivia
                Dim paramList As ParameterListSyntax = DirectCast(node.ParameterList?.Accept(Me), ParameterListSyntax)?.WithoutTrailingTrivia
                If methodInfo.DelegateInvokeMethod.GetReturnType()?.SpecialType = SpecialType.System_Void Then
                    Return Factory.DelegateSubStatement(attrLists, Factory.TokenList(modifiers), identifier, typeParamList, paramList, asClause:=Nothing).WithConvertedTriviaFrom(node)
                Else
                    Dim vbNode As VB.VisualBasicSyntaxNode = node.ReturnType.Accept(Me)
                    Dim returnType As TypeSyntax = DirectCast(vbNode, TypeSyntax)
                    Dim asClause As SimpleAsClauseSyntax = Factory.SimpleAsClause(returnType.WithLeadingTrivia(Factory.Space))
                    Return Factory.DelegateFunctionStatement(attrLists, Factory.TokenList(modifiers), identifier, typeParamList, paramList, asClause).WithConvertedTriviaFrom(node)
                End If
            End Function

            Public Overrides Function VisitEnumDeclaration(node As CSS.EnumDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim members As New List(Of StatementSyntax)
                Dim csMembers As SeparatedSyntaxList(Of CSS.EnumMemberDeclarationSyntax) = node.Members
                If csMembers.Any Then
                    Dim csSeparators As New List(Of SyntaxToken)
                    csSeparators.AddRange(node.Members.GetSeparators)

                    Dim csIdentifierTrailingTrivia As SyntaxTriviaList
                    Dim csSeparatorTrailingTrivia As SyntaxTriviaList
                    Dim csMovedTrailingSpace As SyntaxTriviaList
                    csMovedTrailingSpace = csMovedTrailingSpace.Add(CSEmptySpaceTrivia)
                    Dim vbEnumSatement As StatementSyntax
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
                        vbEnumSatement = DirectCast(e.Value.Accept(Me), StatementSyntax)
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
                    vbEnumSatement = DirectCast(csMembers.Last.Accept(Me), StatementSyntax)
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
                        members.Add(DirectCast(csMembers.Last.Accept(Me), StatementSyntax).WithPrependedLeadingTrivia(leadingTrivia).WithAppendedTrailingTrivia(csSeparatorTrailingTrivia.ConvertTriviaList()))
                    Else
                        members.Add(DirectCast(csMembers.Last.Accept(Me), StatementSyntax).WithAppendedTrailingTrivia(csSeparatorTrailingTrivia.ConvertTriviaList()))
                    End If
                End If

                Dim baseType As TypeSyntax = DirectCast(node.BaseList?.Types.Single().Accept(Me), TypeSyntax)
                Dim listOfAttributes As SyntaxList(Of AttributeListSyntax) = Factory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), AttributeListSyntax)))
                Dim modifiers As SyntaxTokenList = Factory.TokenList(ConvertModifiers(node.Modifiers, Me.IsModule, TokenContext.Global))
                Dim underlyingType As SimpleAsClauseSyntax = If(baseType Is Nothing, Nothing, Factory.SimpleAsClause(baseType))
                Dim enumStatement As EnumStatementSyntax = DirectCast(Factory.EnumStatement(listOfAttributes,
                                                                                                modifiers,
                                                                                                EnumKeyword.WithConvertedTriviaFrom(node.EnumKeyword),
                                                                                                identifier:=GenerateSafeVBToken(id:=node.Identifier, Node:=node, usedIdentifiers:=_usedIdentifiers, Model:=_semanticModel),
                                                                                                underlyingType).
                                                                                       RestructureAttributesAndModifiers(listOfAttributes.Any, modifiers.Any), EnumStatementSyntax)

                Dim endBlockStmt As EndBlockStatementSyntax = FactoryEndBlockStatement(VB.SyntaxKind.EndEnumStatement, EnumKeyword, CollectConvertedTokenTrivia(node.CloseBraceToken, GetLeading:=True, GetTrailing:=True))
                Dim enumBlock As EnumBlockSyntax = Factory.EnumBlock(enumStatement.WithTrailingEOL,
                                                                           Factory.List(members),
                                                                           endBlockStmt
                                                                           )
                Return PrependStatementWithMarkedStatementTrivia(node, enumBlock)
            End Function

            Public Overrides Function VisitEnumMemberDeclaration(node As CSS.EnumMemberDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim initializer As ExpressionSyntax = DirectCast(node.EqualsValue?.Value.Accept(Me), ExpressionSyntax)
                Return Factory.EnumMemberDeclaration(Factory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), AttributeListSyntax))), GenerateSafeVBToken(node.Identifier, node, _usedIdentifiers, _semanticModel), initializer:=If(initializer Is Nothing, Nothing, Factory.EqualsValue(initializer))).WithConvertedTriviaFrom(node)
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
                Dim listOfAttributes As SyntaxList(Of AttributeListSyntax) = Factory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), AttributeListSyntax)))
                Dim modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, Me.IsModule, TokenContext.InterfaceOrModule).ToList
                If node.Modifiers.Contains(CS.SyntaxKind.UnsafeKeyword) Then
                    Return FlagUnsupportedStatements(node, "unsafe interfaces", CommentOutOriginalStatements:=True)
                End If
                Dim members As New List(Of StatementSyntax)
                For Each e As IndexClass(Of CSS.MemberDeclarationSyntax) In node.Members.WithIndex
                    If e.IsFirst AndAlso node.OpenBraceToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        members.Add(DirectCast(e.Value.Accept(Me), StatementSyntax).WithPrependedLeadingTrivia(node.OpenBraceToken.LeadingTrivia.ConvertTriviaList()))
                    Else
                        members.Add(DirectCast(e.Value.Accept(Me), StatementSyntax))
                    End If
                Next
                Dim identifier As SyntaxToken = GenerateSafeVBToken(node.Identifier, node, _usedIdentifiers, _semanticModel)
                Dim typeParameterList As TypeParameterListSyntax = DirectCast(node.TypeParameterList?.Accept(Me), TypeParameterListSyntax)
                Dim statementLeadingTrivia As SyntaxTriviaList
                If node.Modifiers.Any AndAlso modifiers.Count = 0 Then
                    statementLeadingTrivia = statementLeadingTrivia.AddRange(node.Modifiers(0).LeadingTrivia.ConvertTriviaList())
                    Return FlagUnsupportedStatements(node, "Directive within statement", CommentOutOriginalStatements:=True)
                Else
                    statementLeadingTrivia = statementLeadingTrivia.Add(Factory.Space)
                End If
                Dim interfaceStatement As InterfaceStatementSyntax = DirectCast(
                                    Factory.InterfaceStatement(listOfAttributes,
                                                                    Factory.TokenList(modifiers),
                                                                    InterfaceKeyword.WithLeadingTrivia(statementLeadingTrivia),
                                                                    identifier,
                                                                    typeParameterList).RestructureAttributesAndModifiers(listOfAttributes.Any, modifiers.Any),
                                                                            InterfaceStatementSyntax
                                                                            ).WithTrailingEOL
                Dim [inherits] As List(Of InheritsStatementSyntax) = New List(Of InheritsStatementSyntax)()
                Dim [implements] As List(Of ImplementsStatementSyntax) = New List(Of ImplementsStatementSyntax)()
                Dim movedTrailingTrivia As SyntaxTriviaList
                Me.ConvertBaseList(node, [inherits], [implements], movedTrailingTrivia)
                If movedTrailingTrivia.Any Then
                    Stop
                End If
                Dim inheritsStatementList As SyntaxList(Of InheritsStatementSyntax) = Factory.List([inherits])
                Dim implementsStatementList As SyntaxList(Of ImplementsStatementSyntax) = Factory.List([implements])
                Dim statementList As SyntaxList(Of StatementSyntax) = Factory.List(members)
                Dim endInterfaceStatement As EndBlockStatementSyntax = FactoryEndBlockStatement(VB.SyntaxKind.EndInterfaceStatement, InterfaceKeyword, CollectConvertedTokenTrivia(node.CloseBraceToken, GetLeading:=True, GetTrailing:=True))

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
                Dim members As New List(Of StatementSyntax)

                For Each e As IndexClass(Of CSS.MemberDeclarationSyntax) In node.Members.WithIndex
                    If _originalRequest.CancelToken.IsCancellationRequested Then
                        Throw New OperationCanceledException
                    End If
                    Dim item As StatementSyntax = DirectCast(e.Value.Accept(Me), StatementSyntax)
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
                                                                         DirectCast(node.Name.Accept(Me), NameSyntax)
                                                                        ).WithTrailingEOL(),
                                              Factory.List(members),
                                              Factory.EndNamespaceStatement(EndKeyword.WithTrailingTrivia(Factory.Space), NamespaceKeyword)
                                             ) _
                                            .WithConvertedLeadingTriviaFrom(node.NamespaceKeyword) _
                                            .WithUniqueLeadingTrivia(VBHeaderLeadingTrivia) _
                                            .WithTrailingTrivia(node.GetTrailingTrivia.ConvertTriviaList)
            End Function

            Public Overrides Function VisitStructDeclaration(node As CSS.StructDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim [inherits] As List(Of InheritsStatementSyntax) = New List(Of InheritsStatementSyntax)
                Dim [implements] As List(Of ImplementsStatementSyntax) = New List(Of ImplementsStatementSyntax)
                If _originalRequest.ImplementedMembers.Any Then
                    _originalRequest.ImplementedMembersStack.Push(_originalRequest.ImplementedMembers)
                    _originalRequest.ImplementedMembers = (New List(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol)))).ToImmutableArray
                End If
                Dim movedTrailingTrivia As SyntaxTriviaList
                Me.ConvertBaseList(node, [inherits], [implements], movedTrailingTrivia, _originalRequest.ImplementedMembers)
                Dim members As New List(Of StatementSyntax)
                For Each m As CSS.MemberDeclarationSyntax In node.Members
                    Dim item As StatementSyntax = DirectCast(m.Accept(Me), StatementSyntax)
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
                Dim listOfAttributes As SyntaxList(Of AttributeListSyntax) = Factory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), AttributeListSyntax)))
                Dim typeParameterList As TypeParameterListSyntax = DirectCast(node.TypeParameterList?.Accept(Me), TypeParameterListSyntax)
                Dim modifiers As SyntaxTokenList = Factory.TokenList(ConvertModifiers(node.Modifiers, Me.IsModule, TokenContext.Struct))
                Dim structureStmt As StructureStatementSyntax
                structureStmt = DirectCast(Factory.StructureStatement(listOfAttributes,
                                                                            Factory.TokenList(modifiers),
                                                                            StructureKeyword.WithConvertedTriviaFrom(node.Keyword),
                                                                            GenerateSafeVBToken(node.Identifier, node, _usedIdentifiers, _semanticModel),
                                                                            typeParameterList
                                                                            ).RestructureAttributesAndModifiers(listOfAttributes.Any, modifiers.Any), StructureStatementSyntax).WithTrailingEOL

                Dim structureBlock As StructureBlockSyntax = Factory.StructureBlock(
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
                Dim importsName As NameSyntax
                Dim [alias] As ImportAliasClauseSyntax = Nothing
                Dim identifier As SyntaxToken
                If node.Alias IsNot Nothing Then
                    Dim aliasName As CSS.IdentifierNameSyntax = node.Alias.Name
                    identifier = GenerateSafeVBToken(aliasName.Identifier, node, _usedIdentifiers, _semanticModel)
                    [alias] = Factory.ImportAliasClause(identifier)
                End If
                importsName = DirectCast(node.Name.Accept(Me), NameSyntax)
                Dim importsNameString As String = importsName.ToString
                If importsName.IsKind(VB.SyntaxKind.IdentifierName) AndAlso
                        VB.SyntaxFacts.IsKeywordKind(VB.SyntaxFacts.GetKeywordKind(importsNameString)) Then
                    importsName = Factory.IdentifierName($"[{importsNameString}]")
                End If

                Dim clause As ImportsClauseSyntax = Factory.SimpleImportsClause([alias], importsName)

                Dim import As ImportsStatementSyntax
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
                    For Each importStmt As ImportsStatementSyntax In AllImports
                        Dim importsClause As SimpleImportsClauseSyntax = DirectCast(importStmt.ImportsClauses(0), SimpleImportsClauseSyntax)
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
