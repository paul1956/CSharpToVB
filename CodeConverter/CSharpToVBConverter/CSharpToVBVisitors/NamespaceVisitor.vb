' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Collections.Immutable
Imports Extensions
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax
Imports SupportClasses
Imports Utilities
Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic

Namespace CSharpToVBConverter.CSharpToVBVisitors

    Partial Public Class CSharpConverter

        Partial Friend Class NodesVisitor
            Inherits CS.CSharpSyntaxVisitor(Of VB.VisualBasicSyntaxNode)

            Private Const DiscardHelperCode As String = "
Private Shared WriteOnly Property __ As Object
    Set
    End Set
End Property
"

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

            Private Shared Function FilterLeadingTrivia(initialTriviaList As SyntaxTriviaList, ByRef movedLeadingTrivia As SyntaxTriviaList) As SyntaxTriviaList
                If Not initialTriviaList.ContainsDirectiveTrivia Then
                    Return initialTriviaList
                End If
                Dim replacementLeadingTrivia As SyntaxTriviaList
                Dim foundSpace As Boolean = False
                Dim useReplacementLeadingTrivia As Boolean = True
                For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                    Dim trivia As SyntaxTrivia = e.Value
                    Dim nextTrivia As SyntaxTrivia = initialTriviaList.GetForwardTriviaOrDefault(e.Index, lookaheadCount:=1)
                    Select Case trivia.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            If nextTrivia.IsKind(VB.SyntaxKind.None) OrElse nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                Continue For
                            End If
                            If useReplacementLeadingTrivia Then
                                replacementLeadingTrivia = replacementLeadingTrivia.Add(trivia)
                            Else
                                movedLeadingTrivia = movedLeadingTrivia.Add(trivia)
                            End If
                            foundSpace = True
                        Case VB.SyntaxKind.EndOfLineTrivia
                            foundSpace = False
                        Case VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.ElseIfDirectiveTrivia, VB.SyntaxKind.ElseDirectiveTrivia, VB.SyntaxKind.EndIfDirectiveTrivia, VB.SyntaxKind.DisabledTextTrivia
                            If e.IsFirst Then
                                movedLeadingTrivia = movedLeadingTrivia.Add(VbEolTrivia)
                            End If
                            movedLeadingTrivia = movedLeadingTrivia.Add(trivia)
                            If nextTrivia.IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                                movedLeadingTrivia = movedLeadingTrivia.Add(VbEolTrivia)
                                movedLeadingTrivia = movedLeadingTrivia.Add(nextTrivia)
                                e.MoveNext()
                            End If
                            useReplacementLeadingTrivia = False
                        Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                            If useReplacementLeadingTrivia Then
                                If Not foundSpace Then
                                    replacementLeadingTrivia = replacementLeadingTrivia.Add(SpaceTrivia)
                                End If
                                replacementLeadingTrivia = replacementLeadingTrivia.AddRange(LineContinueSpace)
                                replacementLeadingTrivia = replacementLeadingTrivia.Add(trivia)
                            Else
                                If Not foundSpace Then
                                    movedLeadingTrivia = movedLeadingTrivia.Add(SpaceTrivia)
                                End If
                                movedLeadingTrivia = movedLeadingTrivia.AddRange(LineContinueSpace)
                                movedLeadingTrivia = movedLeadingTrivia.Add(trivia)
                            End If
                        Case VB.SyntaxKind.LineContinuationTrivia
                            If Not foundSpace Then
                                replacementLeadingTrivia = replacementLeadingTrivia.Add(SpaceTrivia)
                            End If
                            replacementLeadingTrivia = replacementLeadingTrivia.Add(trivia)
                            foundSpace = False
                        Case Else
                            Stop
                    End Select
                Next

                Return replacementLeadingTrivia
            End Function

            Private Shared Function FilterTrailingTrivia(initialTriviaList As SyntaxTriviaList, ByRef newTrailingTrivia As SyntaxTriviaList) As SyntaxTriviaList
                If Not initialTriviaList.ContainsDirectiveTrivia Then
                    Return initialTriviaList
                End If
                Dim replacementTrailingTrivia As SyntaxTriviaList
                Dim useReplacementTrailingTrivia As Boolean = False
                Dim foundSpace As Boolean = False
                Dim foundLineContinuation As Boolean = False
                For Each e As IndexClass(Of SyntaxTrivia) In initialTriviaList.WithIndex
                    Dim trivia As SyntaxTrivia = e.Value
                    Dim nextTrivia As SyntaxTrivia = initialTriviaList.GetForwardTriviaOrDefault(e.Index, lookaheadCount:=1)
                    Select Case trivia.RawKind
                        Case VB.SyntaxKind.WhitespaceTrivia
                            If nextTrivia.IsKind(VB.SyntaxKind.None) OrElse nextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                Continue For
                            End If
                            If useReplacementTrailingTrivia Then
                                replacementTrailingTrivia = replacementTrailingTrivia.Add(SpaceTrivia)
                            Else
                                newTrailingTrivia = newTrailingTrivia.Add(SpaceTrivia)
                            End If
                            foundSpace = True
                        Case VB.SyntaxKind.EndOfLineTrivia
                            If useReplacementTrailingTrivia Then
                                replacementTrailingTrivia = replacementTrailingTrivia.Add(trivia)
                            Else
                                newTrailingTrivia = newTrailingTrivia.Add(trivia)
                            End If
                            foundSpace = False
                        Case VB.SyntaxKind.IfDirectiveTrivia, VB.SyntaxKind.ElseIfDirectiveTrivia, VB.SyntaxKind.ElseDirectiveTrivia, VB.SyntaxKind.EndIfDirectiveTrivia, VB.SyntaxKind.DisabledTextTrivia
                            If e.IsFirst Then
                                newTrailingTrivia = newTrailingTrivia.Add(VbEolTrivia)
                            End If
                            newTrailingTrivia = newTrailingTrivia.Add(trivia)

                            If nextTrivia.IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                                newTrailingTrivia = newTrailingTrivia.Add(VbEolTrivia)
                                newTrailingTrivia = newTrailingTrivia.Add(nextTrivia)
                                e.MoveNext()
                            End If
                            useReplacementTrailingTrivia = True
                        Case VB.SyntaxKind.CommentTrivia, VB.SyntaxKind.DocumentationCommentTrivia
                            If useReplacementTrailingTrivia Then
                                If Not foundSpace Then
                                    replacementTrailingTrivia = replacementTrailingTrivia.Add(SpaceTrivia)
                                End If
                                replacementTrailingTrivia = replacementTrailingTrivia.AddRange(LineContinueSpace)
                                replacementTrailingTrivia = replacementTrailingTrivia.Add(trivia)
                            Else
                                If Not foundSpace Then
                                    newTrailingTrivia = newTrailingTrivia.Add(SpaceTrivia)
                                End If
                                If Not foundLineContinuation Then
                                    newTrailingTrivia = newTrailingTrivia.AddRange(LineContinueSpace)
                                End If
                                newTrailingTrivia = newTrailingTrivia.Add(trivia)
                                foundLineContinuation = False
                            End If
                        Case VB.SyntaxKind.LineContinuationTrivia
                            replacementTrailingTrivia = replacementTrailingTrivia.Add(trivia)
                            foundLineContinuation = True
                            foundSpace = False
                        Case Else
                            Throw UnreachableException
                    End Select
                Next

                Return replacementTrailingTrivia
            End Function

            Private Shared Function TrimStart(name As NameSyntax, trimString As String) As String
                Dim input As String = name.ToString
                If Not input.StartsWith(trimString, StringComparison.OrdinalIgnoreCase) Then
                    Return input
                End If
                Return input.Substring(trimString.Length)
            End Function

            Private Sub ConvertBaseList(baseType As CSS.BaseTypeDeclarationSyntax, [inherits] As List(Of InheritsStatementSyntax), [implements] As List(Of ImplementsStatementSyntax), ByRef movedFinalTrivia As SyntaxTriviaList, ByRef Optional implementedMembers As ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))) = Nothing)
                Dim typeSyntaxArray As TypeSyntax()
                Dim startImplementsIndex As Integer
                Select Case baseType.Kind()
                    Case CS.SyntaxKind.ClassDeclaration
                        Dim classOrInterface As CSS.TypeSyntax = baseType.BaseList?.Types.FirstOrDefault()?.Type
                        If classOrInterface Is Nothing Then
                            Exit Sub
                        End If
                        Dim classOrInterfaceSymbol As ISymbol = _semanticModel.GetSymbolInfo(classOrInterface).Symbol
                        Dim typesHaveComments As Boolean = False
                        If classOrInterfaceSymbol?.IsInterfaceType() Then
                            Dim typeSyntaxList As New List(Of TypeSyntax)

                            FilterLeadingTrivia(baseType.BaseList.ColonToken.LeadingTrivia.ConvertTriviaList(), movedFinalTrivia)
                            For Each e As IndexClass(Of CSS.BaseTypeSyntax) In baseType.BaseList?.Types.WithIndex
                                Dim item1 As TypeSyntax = DirectCast(e.Value.Type.Accept(Me), TypeSyntax)
                                item1 = item1.WithLeadingTrivia(FilterLeadingTrivia(item1.GetLeadingTrivia, movedFinalTrivia))
                                item1 = item1.WithTrailingTrivia(FilterTrailingTrivia(item1.GetTrailingTrivia, movedFinalTrivia))
                                If item1.ContainsCommentOrDirectiveTrivia Then
                                    typesHaveComments = True
                                End If
                                If e.IsLast Then
                                    item1 = item1.WithTrailingEol
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
                                    [inherits].Add(Factory.InheritsStatement(DirectCast(typeNode.WithLeadingTrivia(SpaceTrivia), TypeSyntax)).WithLeadingTrivia(typeNodeLeadingTrivia))
                                Else
                                    [inherits].Add(Factory.InheritsStatement(DirectCast(typeNode, TypeSyntax)))
                                End If
                            Else
                                [inherits].Add(Factory.InheritsStatement(DirectCast(typeNode, TypeSyntax)))
                            End If
                            startImplementsIndex = 1
                            Dim typeSyntaxList As New List(Of TypeSyntax)
                            Dim separatorList As List(Of SyntaxToken) = baseType.BaseList.Types.GetSeparators.ToList
                            For Each e As IndexClass(Of CSS.BaseTypeSyntax) In baseType.BaseList.Types.WithIndex
                                If e.Index < separatorList.Count Then
                                    FilterLeadingTrivia(separatorList(e.Index).LeadingTrivia.ConvertTriviaList(), movedFinalTrivia)
                                End If
                                If e.IsFirst Then
                                    Continue For
                                End If
                                typeSyntaxList.Add(DirectCast(e.Value.Type.Accept(Me).WithTrailingTrivia(SpaceTrivia), TypeSyntax))
                            Next
                            typeSyntaxArray = typeSyntaxList.ToArray()
                        End If
                        If typeSyntaxArray.Any Then
                            Dim implKeyword As SyntaxToken = ImplementsKeyword
                            If typesHaveComments Then
                                implKeyword = implKeyword.WithTrailingTrivia(SpaceLineContinueEOL)
                            End If
                            [implements].Add(Factory.ImplementsStatement(implKeyword, Factory.SeparatedList(typeSyntaxArray)))
                            Dim implementsClauses As IEnumerable(Of CSS.TypeSyntax) = baseType.BaseList?.Types.Skip(startImplementsIndex).Select(Function(t As CSS.BaseTypeSyntax) t.Type)
                            For Each implementsClause As CSS.TypeSyntax In implementsClauses
                                Dim classOrStructDecl As SyntaxNode = Nothing
                                Dim classOrStructType As INamedTypeSymbol = Nothing
                                Dim interfaceTypes As IEnumerable(Of INamedTypeSymbol) = Nothing

                                If implementsClause.TryInitializeState(_semanticModel, classOrStructDecl, classOrStructType, interfaceTypes, _originalRequest.CancelToken) Then
                                    Dim items As ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))) = classOrStructType.GetAllImplementedMembers(interfaceTypes, _originalRequest.CancelToken)
                                    implementedMembers = implementedMembers.AddRange(items)
                                End If
                            Next
                        End If

                    Case CS.SyntaxKind.StructDeclaration
                        typeSyntaxArray = baseType.BaseList?.Types.Select(Function(t As CSS.BaseTypeSyntax) DirectCast(t.Type.Accept(Me), TypeSyntax)).ToArray()
                        If typeSyntaxArray?.Any Then
                            [implements].Add(Factory.ImplementsStatement(typeSyntaxArray))
                        End If
                    Case CS.SyntaxKind.InterfaceDeclaration
                        Dim baseList As New List(Of TypeSyntax)
                        Dim newLeadingTrivia As New SyntaxTriviaList
                        If baseType.BaseList IsNot Nothing Then
                            newLeadingTrivia = baseType.BaseList.ColonToken.LeadingTrivia.ConvertTriviaList()
                            Dim csSeparators As List(Of SyntaxToken) = baseType.BaseList.Types.GetSeparators.ToList
                            For Each e As IndexClass(Of CSS.BaseTypeSyntax) In baseType.BaseList.Types.WithIndex
                                Dim item As TypeSyntax = DirectCast(e.Value.Type.Accept(Me), TypeSyntax)
                                If item.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                    newLeadingTrivia = newLeadingTrivia.AddRange(item.GetLeadingTrivia)
                                    item = item.WithLeadingTrivia(SpaceTrivia)
                                End If
                                If Not e.IsLast Then
                                    If csSeparators(e.Index).LeadingTrivia.ContainsDirectiveTrivia Then
                                        newLeadingTrivia = newLeadingTrivia.Add(Factory.CommentTrivia($"' TODO: Visual Basic does not support directives in inherits lists. Directive moved!"))
                                        newLeadingTrivia = newLeadingTrivia.Add(VbEolTrivia)
                                        newLeadingTrivia = newLeadingTrivia.AddRange(csSeparators(e.Index).LeadingTrivia.ConvertTriviaList())
                                    ElseIf csSeparators(e.Index).LeadingTrivia.ContainsCommentTrivia Then
                                        newLeadingTrivia = newLeadingTrivia.AddRange(csSeparators(e.Index).LeadingTrivia.ConvertTriviaList())
                                    End If
                                    If item.GetTrailingTrivia.ContainsCommentTrivia Then
                                        baseList.Add(item)
                                    Else
                                        baseList.Add(item.WithTrailingTrivia(SpaceTrivia))
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
                    [implements]([implements].Count - 1) = [implements].Last.WithTrailingEol
                End If
                If [inherits].Any Then
                    [inherits]([inherits].Count - 1) = [inherits].Last.WithTrailingEol
                End If
            End Sub

            Private Iterator Function PatchInlineHelpers(node As CSS.BaseTypeDeclarationSyntax, localIsModule As Boolean) As IEnumerable(Of StatementSyntax)
                If _inlineAssignHelperMarkers.Contains(node) Then
                    _inlineAssignHelperMarkers.Remove(node)
                    Yield TryCast(Factory.ParseSyntaxTree(InlineAssignHelperCode.Replace("Shared ", If(localIsModule, "", "Shared "), StringComparison.Ordinal)).GetRoot().ChildNodes().FirstOrDefault(), StatementSyntax)
                End If
                If _discardHelperMarkers.Contains(node) Then
                    _discardHelperMarkers.Remove(node)
                    Yield TryCast(Factory.ParseSyntaxTree(DiscardHelperCode.Replace("Shared ", If(localIsModule, "", "Shared "), StringComparison.Ordinal)).GetRoot().ChildNodes().FirstOrDefault(), StatementSyntax)
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
                Dim [inherits] As New List(Of InheritsStatementSyntax)()
                Dim [implements] As New List(Of ImplementsStatementSyntax)()

                Dim finalTrailingTrivia As SyntaxTriviaList

                Dim id As SyntaxToken = GenerateSafeVbToken(node.Identifier, node, _semanticModel, _usedIdentifiers).WithConvertedTriviaFrom(node.Identifier)
                id = id.AdjustTokenLeadingTrivia()
                Me.ConvertBaseList(node, [inherits], [implements], finalTrailingTrivia, _originalRequest.ImplementedMembers)
                Dim staticMethodCount As Integer = 0
                Dim methodCount As Integer = 0
                For Each e As IndexClass(Of CSS.MemberDeclarationSyntax) In node.Members.WithIndex
                    If _originalRequest.CancelToken.IsCancellationRequested Then
                        Throw New OperationCanceledException
                    End If
                    Dim m As CSS.MemberDeclarationSyntax = e.Value
                    Dim statement As StatementSyntax = DirectCast(m.Accept(Me), StatementSyntax).RemoveExtraLeadingEol.WithTrailingEol
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
                        members.Add(statement.WithTrailingEol)
                    ElseIf statement.IsKind(VB.SyntaxKind.FunctionStatement) OrElse
                        statement.IsKind(VB.SyntaxKind.SubBlock) Then
                        Dim modifiers As List(Of SyntaxToken)
                        If s_statementDictionary.ContainsKey(node) Then
                            Dim index As Integer = s_statementDictionary(node)

                            Dim insertionPoint As Integer = 0
                            For Each stmtTuple As StatementHandling In s_statementHandlingList
                                If stmtTuple.Index = index Then
                                    members.Insert(insertionPoint, stmtTuple.Statement)
                                    insertionPoint += 1
                                End If
                            Next
                        End If
                        s_statementDictionary.Remove(node)
                        If s_statementDictionary.Count = 0 Then
                            s_statementHandlingList.Clear()
                        End If
                        If TypeOf statement Is MethodBlockSyntax Then
                            Dim block As MethodBlockSyntax = CType(statement, MethodBlockSyntax)
                            modifiers = block.BlockStatement.Modifiers.ToList
                        ElseIf TypeOf statement Is MethodStatementSyntax Then
                            Dim block As MethodStatementSyntax = CType(statement, MethodStatementSyntax)
                            modifiers = block.Modifiers.ToList
                        Else
                            modifiers = New List(Of SyntaxToken)
                        End If
                        methodCount += 1
                        If modifiers.Contains(VB.SyntaxKind.SharedKeyword) Then
                            staticMethodCount += 1
                        End If
                        members.Add(statement.WithTrailingEol)
                    ElseIf statement.IsKind(VB.SyntaxKind.PropertyBlock) Then
                        If TypeOf m Is CSS.PropertyDeclarationSyntax Then
                            If CType(m, CSS.PropertyDeclarationSyntax).ExpressionBody Is Nothing Then
                                members.AddRange(ReplaceOneStatementWithMarkedStatements(m, statement.WithTrailingEol))
                            Else
                                members.AddRange(ReplaceOneStatementWithMarkedStatements(CType(m, CSS.PropertyDeclarationSyntax).ExpressionBody, statement.WithTrailingEol))
                            End If
                        Else
                            members.AddRange(ReplaceOneStatementWithMarkedStatements(m, statement.WithTrailingEol))
                        End If

                        ' Cases below are handled in-line
                    ElseIf statement.IsKind(VB.SyntaxKind.ConstructorBlock) OrElse
                        statement.IsKind(VB.SyntaxKind.DelegateFunctionStatement) OrElse
                        statement.IsKind(VB.SyntaxKind.DelegateSubStatement) OrElse
                        statement.IsKind(VB.SyntaxKind.EventBlock) OrElse
                        statement.IsKind(VB.SyntaxKind.EventStatement) OrElse
                        statement.IsKind(VB.SyntaxKind.OperatorBlock) OrElse
                        statement.IsKind(VB.SyntaxKind.PropertyStatement) Then
                        members.Add(statement.WithConvertedTriviaFrom(m).WithTrailingEol)
                    Else
                        members.Add(statement.WithConvertedTriviaFrom(m).WithTrailingEol)
                    End If
                    If e.IsFirst Then
                        If members.Any Then
                            members(0) = members(0).WithPrependedLeadingTrivia(node.OpenBraceToken.CollectConvertedTokenTrivia(getLeading:=True, getTrailing:=False))
                        Else
                            Stop
                        End If
                    End If
                Next

                members.AddRange(Me.PatchInlineHelpers(node, Me.IsModule))

                Dim listOfAttributes As SyntaxList(Of AttributeListSyntax) = Factory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), AttributeListSyntax)))
                Dim typeParameterList As TypeParameterListSyntax = DirectCast(node.TypeParameterList?.Accept(Me), TypeParameterListSyntax)
                If typeParameterList IsNot Nothing Then
                    If id.TrailingTrivia.ContainsCommentTrivia Then
                        Stop
                    End If
                    id = id.WithTrailingTrivia(SpaceTrivia)
                End If
                Dim notInsideClassOrStruct As Boolean = _isModuleStack.Count < 2 AndAlso node.IsNotInStructure

                If Me.IsModule AndAlso notInsideClassOrStruct Then
                    Dim moduleModifiers As SyntaxTokenList = Factory.TokenList(ConvertModifiers(node.Modifiers, Me.IsModule, TokenContext.InterfaceOrModule))
                    Dim moduleKeywordWithTrivia As SyntaxToken = ModuleKeyword.WithConvertedLeadingTriviaFrom(node.Keyword).WithTrailingTrivia(SpaceTrivia)
                    Dim prependedTrivia As SyntaxTriviaList = node.DedupLeadingTrivia(ModuleKeyword, listOfAttributes.ToList, moduleModifiers)
                    Dim moduleStmt As ModuleStatementSyntax = DirectCast(Factory.ModuleStatement(listOfAttributes,
                                                                                                     moduleModifiers,
                                                                                                     moduleKeywordWithTrivia,
                                                                                                     id,
                                                                                                     typeParameterList
                                                                                                    ).WithPrependedLeadingTrivia(prependedTrivia) _
                                                                                                     .RestructureAttributesAndModifiers(listOfAttributes.Any, moduleModifiers.Any),
                                                                                ModuleStatementSyntax).WithTrailingEol

                    moduleStmt = DirectCast(PrependStatementWithMarkedStatementTrivia(node, moduleStmt), ModuleStatementSyntax)
                    Dim endModule As EndBlockStatementSyntax = FactoryEndBlockStatement(VB.SyntaxKind.EndModuleStatement, ModuleKeyword, node.CloseBraceToken.CollectConvertedTokenTrivia(getLeading:=True, getTrailing:=True))
                    Dim moduleBlock As ModuleBlockSyntax = Factory.ModuleBlock(moduleStmt,
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
                    Dim classModifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, isModule:=False, If(Me.IsModule, TokenContext.InterfaceOrModule, TokenContext.Global)).ToList
                    Dim classKeywordWithTrivia As SyntaxToken = ClassKeyWord.WithConvertedTriviaFrom(node.Keyword)
                    If classKeywordWithTrivia.TrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                        Stop
                    Else
                        classKeywordWithTrivia = classKeywordWithTrivia.WithTrailingTrivia(SpaceTrivia)
                    End If
                    If methodCount > 0 AndAlso
                        staticMethodCount = methodCount AndAlso
                        Not classModifiers.Contains(VB.SyntaxKind.StaticKeyword, VB.SyntaxKind.NotInheritableKeyword) Then
                        If classModifiers.Count = 0 Then
                            classModifiers.Add(NotInheritableKeyword.WithLeadingTrivia(classKeywordWithTrivia.LeadingTrivia))
                            classKeywordWithTrivia = classKeywordWithTrivia.WithLeadingTrivia(SpaceTrivia)
                        Else
                            Dim sharedIndex As Integer = classModifiers.IndexOf(VB.SyntaxKind.SharedKeyword)

                            If sharedIndex = -1 Then
                                classModifiers.Add(NotInheritableKeyword)
                            Else
                                classModifiers(sharedIndex) = NotInheritableKeyword.WithTriviaFrom(classModifiers(sharedIndex))
                            End If
                        End If
                    End If
                    Dim prependedTrivia As SyntaxTriviaList = node.DedupLeadingTrivia(classKeywordWithTrivia, listOfAttributes.ToList, classModifiers)
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
                        classStmt = classStmt.WithTrailingEol
                    Else
                        Dim movedTrailingTrivia As SyntaxTriviaList
                        If classStmt.GetTrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                            Dim initialTriviaList As SyntaxTriviaList = classStmt.GetTrailingTrivia
                            classStmt = classStmt.WithTrailingTrivia(FilterTrailingTrivia(initialTriviaList, movedTrailingTrivia)).WithTrailingEol
                        End If
                        If movedTrailingTrivia.Any Or finalTrailingTrivia.Any Then
                            If [inherits].Any Then
                                For Each e As IndexClass(Of InheritsStatementSyntax) In [inherits].WithIndex
                                    [inherits](e.Index) = [inherits](e.Index).WithLeadingTrivia(FilterLeadingTrivia(e.Value.GetLeadingTrivia, movedTrailingTrivia))
                                    [inherits](e.Index) = [inherits](e.Index).WithTrailingTrivia(FilterTrailingTrivia(e.Value.GetLeadingTrivia, movedTrailingTrivia)).WithTrailingEol
                                Next
                            End If
                            If [implements].Any Then
                                For Each e As IndexClass(Of ImplementsStatementSyntax) In [implements].WithIndex
                                    [implements](e.Index) = [implements](e.Index).WithLeadingTrivia(FilterLeadingTrivia(e.Value.GetLeadingTrivia, movedTrailingTrivia))
                                    [implements](e.Index) = [implements](e.Index).WithTrailingTrivia(FilterTrailingTrivia(e.Value.GetLeadingTrivia, movedTrailingTrivia)).WithTrailingEol
                                Next
                                movedTrailingTrivia = movedTrailingTrivia.AddRange(finalTrailingTrivia)
                                [implements]([implements].Count - 1) = [implements].Last.WithAppendedTrailingTrivia(movedTrailingTrivia).WithTrailingEol
                            Else
                                movedTrailingTrivia = movedTrailingTrivia.AddRange(finalTrailingTrivia)
                                [inherits]([inherits].Count - 1) = [inherits].Last.WithAppendedTrailingTrivia(movedTrailingTrivia).WithTrailingEol
                            End If
                        End If
                    End If
                    Dim endClass As EndBlockStatementSyntax = FactoryEndBlockStatement(VB.SyntaxKind.EndClassStatement,
                                                                                       ClassKeyWord,
                                                                                       node.CloseBraceToken.TrailingTrivia.ConvertTriviaList).WithLeadingTrivia(node.CloseBraceToken.LeadingTrivia.ConvertTriviaList)
                    Dim classBlock As ClassBlockSyntax = Factory.ClassBlock(
                                                                                classStmt.WithTrailingEol,
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
                Dim identifier As SyntaxToken = GenerateSafeVbToken(node.Identifier, node, _semanticModel, _usedIdentifiers)
                Dim methodInfo As INamedTypeSymbol = TryCast(_semanticModel.GetDeclaredSymbol(node), INamedTypeSymbol)
                Dim listOfAttrLists As New List(Of AttributeListSyntax)
                For Each e As IndexClass(Of CSS.AttributeListSyntax) In node.AttributeLists.WithIndex
                    If e.IsFirst Then
                        listOfAttrLists.Add(DirectCast(e.Value.Accept(Me), AttributeListSyntax))
                    Else
                        If e.Value.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                            listOfAttrLists(e.Index - 1) = listOfAttrLists(e.Index - 1).WithTrailingTrivia(listOfAttrLists(e.Index - 1).GetLeadingTrivia.WithLastLineContinuation)
                            listOfAttrLists.Add(DirectCast(e.Value.Accept(Me).WithPrependedLeadingTrivia(SpaceLineContinue.ToArray), AttributeListSyntax))
                        Else
                            listOfAttrLists.Add(DirectCast(e.Value.Accept(Me), AttributeListSyntax))
                        End If
                    End If
                Next
                Dim attrLists As SyntaxList(Of AttributeListSyntax) = Factory.List(listOfAttrLists)
                Dim modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, Me.IsModule, TokenContext.Global).ToList
                Dim typeParamList As TypeParameterListSyntax = DirectCast(node.TypeParameterList?.Accept(Me), TypeParameterListSyntax)?.WithoutTrailingTrivia
                Dim paramList As ParameterListSyntax = DirectCast(node.ParameterList?.Accept(Me), ParameterListSyntax)?.WithoutTrailingTrivia
                If methodInfo.DelegateInvokeMethod.GetReturnType()?.SpecialType = SpecialType.System_Void Then
                    Return Factory.DelegateSubStatement(attrLists, Factory.TokenList(modifiers), identifier, typeParamList, paramList, asClause:=Nothing).WithConvertedTriviaFrom(node)
                Else
                    Dim vbNode As VB.VisualBasicSyntaxNode = node.ReturnType.Accept(Me)
                    Dim returnType As TypeSyntax = DirectCast(vbNode, TypeSyntax)
                    Dim asClause As SimpleAsClauseSyntax = Factory.SimpleAsClause(returnType.WithLeadingTrivia(SpaceTrivia))
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
                    csMovedTrailingSpace = csMovedTrailingSpace.Add(s_csEmptySpaceTrivia)
                    Dim vbEnumStatement As StatementSyntax
                    Dim leadingTrivia As SyntaxTriviaList = node.OpenBraceToken.CollectConvertedTokenTrivia(getLeading:=True, getTrailing:=False)
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
                        vbEnumStatement = DirectCast(e.Value.Accept(Me), StatementSyntax)
                        csSeparatorTrailingTrivia = csSeparators(e.Index).TrailingTrivia
                        If csSeparatorTrailingTrivia.Any Then
                            If csSeparatorTrailingTrivia(0).IsKind(CS.SyntaxKind.WhitespaceTrivia) Then
                                csMovedTrailingSpace = csMovedTrailingSpace.Add(csSeparatorTrailingTrivia(0))
                                csSeparatorTrailingTrivia = csSeparatorTrailingTrivia.RemoveAt(0)
                                csSeparatorTrailingTrivia = csSeparatorTrailingTrivia.InsertRange(0, csMovedTrailingSpace)
                            ElseIf csSeparatorTrailingTrivia(0).IsWhitespace Then
                            End If
                        End If
                        If e.IsFirst Then
                            members.Add(vbEnumStatement.WithPrependedLeadingTrivia(leadingTrivia).WithTrailingTrivia(csSeparatorTrailingTrivia.ConvertTriviaList()))
                        Else
                            members.Add(vbEnumStatement.WithTrailingTrivia(csSeparatorTrailingTrivia.ConvertTriviaList()))
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
                                                                                                identifier:=GenerateSafeVbToken(id:=node.Identifier, node:=node, model:=_semanticModel, usedIdentifiers:=_usedIdentifiers),
                                                                                                underlyingType).
                                                                                       RestructureAttributesAndModifiers(listOfAttributes.Any, modifiers.Any), EnumStatementSyntax)

                Dim endBlockStmt As EndBlockStatementSyntax = FactoryEndBlockStatement(VB.SyntaxKind.EndEnumStatement, EnumKeyword, node.CloseBraceToken.CollectConvertedTokenTrivia(getLeading:=True, getTrailing:=True))
                Dim enumBlock As EnumBlockSyntax = Factory.EnumBlock(enumStatement.WithTrailingEol,
                                                                           Factory.List(members),
                                                                           endBlockStmt
                                                                           )
                Return PrependStatementWithMarkedStatementTrivia(node, enumBlock)
            End Function

            Public Overrides Function VisitEnumMemberDeclaration(node As CSS.EnumMemberDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim initializer As ExpressionSyntax = DirectCast(node.EqualsValue?.Value.Accept(Me), ExpressionSyntax)
                Dim attributeLists As New SyntaxList(Of AttributeListSyntax)
                For Each e As IndexClass(Of CSS.AttributeListSyntax) In node.AttributeLists.WithIndex
                    Dim attributeList As AttributeListSyntax = DirectCast(e.Value.Accept(Me), AttributeListSyntax)
                    If e.IsFirst AndAlso Not attributeList.ContainsCommentOrDirectiveTrivia Then
                        attributeList = attributeList.WithPrependedLeadingTrivia(VbEolTrivia)
                    End If
                    If Not e.IsLast Then
                        attributeList = attributeList.WithTrailingTrivia(attributeList.GetTrailingTrivia.WithLastLineContinuation)
                    End If
                    attributeLists = attributeLists.Add(attributeList)
                Next
                Return Factory.EnumMemberDeclaration(attributeLists, GenerateSafeVbToken(node.Identifier, node, _semanticModel, _usedIdentifiers), initializer:=If(initializer Is Nothing, Nothing, Factory.EqualsValue(initializer))).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitExplicitInterfaceSpecifier(node As CSS.ExplicitInterfaceSpecifierSyntax) As VB.VisualBasicSyntaxNode
                Return node.Name.Accept(Me)
            End Function

            Public Overrides Function VisitExternAliasDirective(node As CSS.ExternAliasDirectiveSyntax) As VB.VisualBasicSyntaxNode
                Return FlagUnsupportedStatements(node, "Extern Alias", commentOutOriginalStatements:=True)
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
                    Return FlagUnsupportedStatements(node, "unsafe interfaces", commentOutOriginalStatements:=True)
                End If
                Dim members As New List(Of StatementSyntax)
                For Each e As IndexClass(Of CSS.MemberDeclarationSyntax) In node.Members.WithIndex
                    If e.IsFirst AndAlso node.OpenBraceToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        members.Add(DirectCast(e.Value.Accept(Me), StatementSyntax).WithPrependedLeadingTrivia(node.OpenBraceToken.LeadingTrivia.ConvertTriviaList()))
                    Else
                        members.Add(DirectCast(e.Value.Accept(Me), StatementSyntax))
                    End If
                Next
                Dim identifier As SyntaxToken = GenerateSafeVbToken(node.Identifier, node, _semanticModel, _usedIdentifiers)
                Dim typeParameterList As TypeParameterListSyntax = DirectCast(node.TypeParameterList?.Accept(Me), TypeParameterListSyntax)
                Dim statementLeadingTrivia As SyntaxTriviaList
                If node.Modifiers.Any AndAlso modifiers.Count = 0 Then
                    Return FlagUnsupportedStatements(node, "Directive within statement", commentOutOriginalStatements:=True)
                Else
                    statementLeadingTrivia = statementLeadingTrivia.Add(SpaceTrivia)
                End If
                Dim interfaceStatement As InterfaceStatementSyntax = DirectCast(
                                    Factory.InterfaceStatement(listOfAttributes,
                                                                    Factory.TokenList(modifiers),
                                                                    InterfaceKeyword.WithLeadingTrivia(statementLeadingTrivia),
                                                                    identifier,
                                                                    typeParameterList).RestructureAttributesAndModifiers(listOfAttributes.Any, modifiers.Any),
                                                                            InterfaceStatementSyntax
                                                                            ).WithTrailingEol
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
                Dim endInterfaceStatement As EndBlockStatementSyntax = FactoryEndBlockStatement(VB.SyntaxKind.EndInterfaceStatement, InterfaceKeyword, node.CloseBraceToken.CollectConvertedTokenTrivia(getLeading:=True, getTrailing:=True))

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

                Dim newOpenBraceTrivia As SyntaxTriviaList = node.OpenBraceToken.CollectConvertedTokenTrivia(getLeading:=True, getTrailing:=True)
                Dim members As New List(Of StatementSyntax)

                For Each e As IndexClass(Of CSS.MemberDeclarationSyntax) In node.Members.WithIndex
                    If _originalRequest.CancelToken.IsCancellationRequested Then
                        Throw New OperationCanceledException
                    End If
                    Dim item As StatementSyntax = DirectCast(e.Value.Accept(Me), StatementSyntax)
                    If e.IsFirst AndAlso newOpenBraceTrivia.Any AndAlso newOpenBraceTrivia.ContainsCommentOrDirectiveTrivia Then
                        item = item.WithPrependedLeadingTrivia(newOpenBraceTrivia)
                    End If
                    members.Add(item)
                    If e.IsLast Then
                        members(e.Index) = members.Last.
                            WithAppendedTrailingTrivia(node.CloseBraceToken.LeadingTrivia.ConvertTriviaList()).
                            WithAppendedTrailingTrivia(node.CloseBraceToken.TrailingTrivia.ConvertTriviaList())
                    End If
                Next

                Return Factory.NamespaceBlock(Factory.NamespaceStatement(NamespaceKeyword,
                                                                         DirectCast(node.Name.Accept(Me), NameSyntax)
                                                                        ).WithTrailingEol(),
                                              Factory.List(members),
                                              Factory.EndNamespaceStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), NamespaceKeyword)
                                             ) _
                                            .WithConvertedLeadingTriviaFrom(node.NamespaceKeyword) _
                                            .WithUniqueLeadingTrivia(_vbHeaderLeadingTrivia) _
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
                    members(0) = members(0).WithPrependedLeadingTrivia(node.OpenBraceToken.CollectConvertedTokenTrivia(getLeading:=True, getTrailing:=False))
                End If
                Dim listOfAttributes As SyntaxList(Of AttributeListSyntax) = Factory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), AttributeListSyntax)))
                Dim typeParameterList As TypeParameterListSyntax = DirectCast(node.TypeParameterList?.Accept(Me), TypeParameterListSyntax)
                Dim modifiers As SyntaxTokenList = Factory.TokenList(ConvertModifiers(node.Modifiers, Me.IsModule, TokenContext.Struct))
                Dim structureStmt As StructureStatementSyntax
                structureStmt = DirectCast(Factory.StructureStatement(listOfAttributes,
                                                                            Factory.TokenList(modifiers),
                                                                            StructureKeyword.WithConvertedTriviaFrom(node.Keyword),
                                                                            GenerateSafeVbToken(node.Identifier, node, _semanticModel, _usedIdentifiers),
                                                                            typeParameterList
                                                                            ).RestructureAttributesAndModifiers(listOfAttributes.Any, modifiers.Any), StructureStatementSyntax).WithTrailingEol

                Dim structureBlock As StructureBlockSyntax = Factory.StructureBlock(
                                                    structureStmt,
                                                    Factory.List([inherits]),
                                                    Factory.List([implements]),
                                                    Factory.List(members),
                                                    Factory.EndStructureStatement(EndKeyword.WithTrailingTrivia(SpaceTrivia), StructureKeyword).WithConvertedTriviaFrom(node.CloseBraceToken)
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
                    identifier = GenerateSafeVbToken(aliasName.Identifier, node, _semanticModel, _usedIdentifiers)
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
                If _allImports.Any Then
                    import = Factory.ImportsStatement(Factory.SingletonSeparatedList(clause)).WithConvertedTriviaFrom(node)
                Else
                    import = Factory.ImportsStatement(Factory.SingletonSeparatedList(clause)) _
                                                    .WithConvertedLeadingTriviaFrom(node) _
                                                    .WithUniqueLeadingTrivia(_vbHeaderLeadingTrivia) _
                                                    .WithTrailingTrivia(node.GetTrailingTrivia.ConvertTriviaList)
                End If
                Dim matchNotFound As Boolean = True
                If _allImports.Any Then
                    For Each importStmt As ImportsStatementSyntax In _allImports
                        Dim importsClause As SimpleImportsClauseSyntax = DirectCast(importStmt.ImportsClauses(0), SimpleImportsClauseSyntax)
                        If importsClause.Alias IsNot Nothing AndAlso importsClause.Alias.ToString = [alias]?.ToString Then
                            matchNotFound = False
                            Exit For
                        Else
                            If importsClause.Name.ToString.RemoveBrackets = importsNameString.RemoveBrackets Then
                                matchNotFound = False
                                Exit For
                            End If
                            Dim namespaceDeclaration As CSS.NamespaceDeclarationSyntax = TryCast(node.Parent, CSS.NamespaceDeclarationSyntax)
                            If namespaceDeclaration IsNot Nothing Then
                                If TrimStart(importsClause.Name, $"{namespaceDeclaration.Name}.") = importsNameString.RemoveBrackets Then
                                    matchNotFound = False
                                End If
                            End If
                        End If
                    Next
                End If
                If matchNotFound Then
                    _allImports.Add(import)
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
