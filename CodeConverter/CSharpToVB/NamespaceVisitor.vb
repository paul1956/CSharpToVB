' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Imports System.Collections.Immutable
Imports System.Runtime.InteropServices
Imports System.Threading

Imports CSharpToVBCodeConverter.Util

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBCodeConverter.DestVisualBasic

    Partial Public Class CSharpConverter

        Partial Friend Class NodesVisitor
            Inherits CS.CSharpSyntaxVisitor(Of VB.VisualBasicSyntaxNode)

            Private Const InlineAssignHelperCode As String = "<Obsolete(""Please refactor code that uses this function, it is a simple work-around to simulate inline assignment in VB!"")>
Private Shared Function __InlineAssignHelper(Of T)(ByRef target As T, value As T) As T
target = value
Return value
End Function
"

            ''' <summary>
            ''' Returns new leading trivia for first Item in a List
            ''' This Trivia consists of all the OpenBrace Leading Trivia and the trailing Trivia only of does not just contain a CRLF
            ''' </summary>
            ''' <param name="OpenBraceToken"></param>
            ''' <returns>List(Of SyntaxTrivia) from OpenBrace</returns>
            Private Shared Function ConvertOpenBraceTrivia(OpenBraceToken As SyntaxToken) As List(Of SyntaxTrivia)
                Dim LeadingTrivia As List(Of SyntaxTrivia) = ConvertTrivia(OpenBraceToken.LeadingTrivia).ToList
                Dim OpenBraceTrailingTrivia As SyntaxTriviaList = OpenBraceToken.TrailingTrivia
                If OpenBraceTrailingTrivia.Count <> 1 OrElse Not OpenBraceTrailingTrivia(0).IsEndOfLine Then
                    LeadingTrivia.AddRange(ConvertTrivia(OpenBraceTrailingTrivia))
                End If
                Return LeadingTrivia
            End Function

            Private Shared Function TryInitializeState(model As SemanticModel, node As SyntaxNode, CancelToken As CancellationToken,
                                                       <Out()> ByRef classOrStructDecl As SyntaxNode, <Out()> ByRef classOrStructType As INamedTypeSymbol, <Out()> ByRef interfaceTypes As IEnumerable(Of INamedTypeSymbol)) As Boolean
                Dim NodeTypeIsTypeSyntax As Boolean = TypeOf node Is CSS.TypeSyntax
                Dim interfaceNode As CSS.TypeSyntax = If(NodeTypeIsTypeSyntax, CType(node, CSS.TypeSyntax), Nothing)
                If NodeTypeIsTypeSyntax AndAlso TypeOf interfaceNode.Parent Is CSS.BaseTypeSyntax AndAlso interfaceNode.Parent.IsParentKind(CS.SyntaxKind.BaseList) AndAlso CType(interfaceNode.Parent, CSS.BaseTypeSyntax).Type Is interfaceNode Then
                    If interfaceNode.Parent.Parent.IsParentKind(CS.SyntaxKind.ClassDeclaration) OrElse interfaceNode.Parent.Parent.IsParentKind(CS.SyntaxKind.StructDeclaration) Then
                        Dim interfaceSymbolInfo As SymbolInfo = model.GetSymbolInfo(interfaceNode, CancelToken)
                        If interfaceSymbolInfo.CandidateReason <> CandidateReason.WrongArity Then
                            Dim interfaceType As INamedTypeSymbol = TryCast(interfaceSymbolInfo.GetAnySymbol(), INamedTypeSymbol)
                            If interfaceType IsNot Nothing AndAlso interfaceType.TypeKind = Microsoft.CodeAnalysis.TypeKind.Interface Then
                                classOrStructDecl = TryCast(interfaceNode.Parent.Parent.Parent, CSS.TypeDeclarationSyntax)
                                classOrStructType = TryCast(model.GetDeclaredSymbol(classOrStructDecl, CancelToken), INamedTypeSymbol)
                                interfaceTypes = SpecializedCollection.SingletonEnumerable(interfaceType)
                                Return interfaceTypes IsNot Nothing AndAlso classOrStructType IsNot Nothing
                            End If
                        End If
                    End If
                End If

                classOrStructDecl = Nothing
                classOrStructType = Nothing
                interfaceTypes = Nothing
                Return False
            End Function

            Private Sub ConvertBaseList(_Type As CSS.BaseTypeDeclarationSyntax, [inherits] As List(Of VBS.InheritsStatementSyntax), [implements] As List(Of VBS.ImplementsStatementSyntax), ByRef Optional ImplementedMembers As ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))) = Nothing)
                Dim TypeSyntaxArray As VBS.TypeSyntax()
                Dim StartImplementsIndex As Integer = 0
                Select Case _Type.Kind()
                    Case CS.SyntaxKind.ClassDeclaration
                        Dim classOrInterface As CSS.TypeSyntax = _Type.BaseList?.Types.FirstOrDefault()?.Type
                        If classOrInterface Is Nothing Then
                            Return
                        End If
                        Dim classOrInterfaceSymbol As ISymbol = ModelExtensions.GetSymbolInfo(_mSemanticModel, classOrInterface).Symbol
                        If classOrInterfaceSymbol?.IsInterfaceType() Then
                            TypeSyntaxArray = _Type.BaseList?.Types.Select(Function(t As CSS.BaseTypeSyntax) DirectCast(t.Type.Accept(Me), VBS.TypeSyntax)).ToArray()
                            StartImplementsIndex = 0
                        Else
                            Dim TypeNode As VB.VisualBasicSyntaxNode = classOrInterface.Accept(Me)
                            If TypeNode.HasLeadingTrivia Then
                                Dim TypeNodeLeadingTrivia As SyntaxTriviaList = TypeNode.GetLeadingTrivia
                                If TypeNodeLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                    [inherits].Add(VBFactory.InheritsStatement(DirectCast(TypeNode.WithLeadingTrivia(SpaceTrivia), VBS.TypeSyntax)).WithLeadingTrivia(TypeNodeLeadingTrivia))
                                Else
                                    [inherits].Add(VBFactory.InheritsStatement(DirectCast(TypeNode, VBS.TypeSyntax)))
                                End If
                            Else
                                [inherits].Add(VBFactory.InheritsStatement(DirectCast(TypeNode, VBS.TypeSyntax)))
                            End If
                            StartImplementsIndex = 1
                            TypeSyntaxArray = _Type.BaseList?.Types.Skip(StartImplementsIndex).Select(Function(t As CSS.BaseTypeSyntax) DirectCast(t.Type.Accept(Me).WithTrailingTrivia(SpaceTrivia), VBS.TypeSyntax)).ToArray()
                        End If
                        If TypeSyntaxArray.Length > 0 Then
                            [implements].Add(VBFactory.ImplementsStatement(TypeSyntaxArray))
                            Dim ImplementsClauses As IEnumerable(Of CSS.TypeSyntax) = _Type.BaseList?.Types.Skip(StartImplementsIndex).Select(Function(t As CSS.BaseTypeSyntax) t.Type)
                            For Each ImplementsClause As CSS.TypeSyntax In ImplementsClauses
                                Dim classOrStructDecl As SyntaxNode = Nothing
                                Dim classOrStructType As INamedTypeSymbol = Nothing
                                Dim interfaceTypes As IEnumerable(Of INamedTypeSymbol) = Nothing

                                If TryInitializeState(_mSemanticModel, ImplementsClause, s_originalRequest.CancelToken, classOrStructDecl, classOrStructType, interfaceTypes) Then
                                    Dim items As ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))) = classOrStructType.GetAllImplementedMembers(interfaceTypes, s_originalRequest.CancelToken)
                                    ImplementedMembers = ImplementedMembers.AddRange(items)
                                End If
                            Next
                        End If

                    Case CS.SyntaxKind.StructDeclaration
                        TypeSyntaxArray = _Type.BaseList?.Types.Select(Function(t As CSS.BaseTypeSyntax) DirectCast(t.Type.Accept(Me), VBS.TypeSyntax)).ToArray()
                        If TypeSyntaxArray?.Length > 0 Then
                            [implements].Add(VBFactory.ImplementsStatement(TypeSyntaxArray))
                        End If
                    Case CS.SyntaxKind.InterfaceDeclaration
                        Dim baseList As New List(Of VBS.TypeSyntax)
                        Dim newLeadingTrivia As New List(Of SyntaxTrivia)
                        If _Type.BaseList IsNot Nothing Then
                            newLeadingTrivia.AddRange(ConvertTrivia(_Type.BaseList.ColonToken.LeadingTrivia))
                            Dim csSeparators As List(Of SyntaxToken) = _Type.BaseList.Types.GetSeparators.ToList
                            For Each e As IndexClass(Of CSS.BaseTypeSyntax) In _Type.BaseList.Types.WithIndex
                                Dim item As VBS.TypeSyntax = DirectCast(e.Value.Type.Accept(Me), VBS.TypeSyntax)
                                If item.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                    newLeadingTrivia.AddRange(item.GetLeadingTrivia)
                                    item = item.WithLeadingTrivia(SpaceTrivia)
                                End If
                                If Not e.IsLast Then
                                    If csSeparators(e.Index).LeadingTrivia.ContainsDirectiveTrivia Then
                                        newLeadingTrivia.Add(VBFactory.CommentTrivia($"' TODO: Visual Basic does not support directives in inherits lists. Directive moved!"))
                                        newLeadingTrivia.Add(VBEOLTrivia)
                                        newLeadingTrivia.AddRange(ConvertTrivia(csSeparators(e.Index).LeadingTrivia))
                                    ElseIf csSeparators(e.Index).LeadingTrivia.ContainscommentTrivia Then
                                        newLeadingTrivia.AddRange(ConvertTrivia(csSeparators(e.Index).LeadingTrivia))
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
                            [inherits].Add(VBFactory.InheritsStatement(baseList.ToArray).WithLeadingTrivia(newLeadingTrivia))
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
                If _inlineAssignHelperMarkers.Contains(node) Then
                    _inlineAssignHelperMarkers.Remove(node)
                    Yield TryCast(VBFactory.ParseSyntaxTree(InlineAssignHelperCode.Replace("Shared ", If(IsModule, "", "Shared "), StringComparison.Ordinal)).GetRoot().ChildNodes().FirstOrDefault(), VBS.StatementSyntax)
                End If
            End Function

            Friend Shared Function IsNotInStructure(node As CS.CSharpSyntaxNode) As Boolean
                Dim StatementWithIssues As CS.CSharpSyntaxNode = node
                While StatementWithIssues IsNot Nothing
                    'If TypeOf StatementWithIssues Is CSS.ClassDeclarationSyntax Then
                    '    Exit While
                    'End If

                    If TypeOf StatementWithIssues Is CSS.StructDeclarationSyntax Then
                        Exit While
                    End If

                    StatementWithIssues = CType(StatementWithIssues.Parent, CS.CSharpSyntaxNode)
                End While
                If StatementWithIssues Is Nothing Then
                    Return True
                End If

                Return False
            End Function

            Public Overrides Function VisitClassDeclaration(node As CSS.ClassDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim saveUsedIdentifiers As Dictionary(Of String, SymbolTableEntry) = s_usedIdentifiers
                SyncLock s_usedStacks
                    s_usedStacks.Push(s_usedIdentifiers)
                    s_usedIdentifiers.Clear()
                    _isModuleStack.Push(node.Modifiers.Contains(CS.SyntaxKind.StaticKeyword) And node.TypeParameterList Is Nothing)
                    If s_implementedMembers.Any Then
                        s_implementedMembersStack.Push(s_implementedMembers)
                        s_implementedMembers = (New List(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol)))).ToImmutableArray
                    End If
                End SyncLock
                Dim members As New List(Of VBS.StatementSyntax)
                Dim MembersLastIndex As Integer = node.Members.Count - 1
                Dim [inherits] As New List(Of VBS.InheritsStatementSyntax)()
                Dim [implements] As New List(Of VBS.ImplementsStatementSyntax)()

                ConvertBaseList(node, [inherits], [implements], s_implementedMembers)

                Dim classType As ITypeSymbol = CType(_mSemanticModel.GetDeclaredSymbol(node), ITypeSymbol)
                For Each e As IndexClass(Of CSS.MemberDeclarationSyntax) In node.Members.WithIndex
                    If s_originalRequest.CancelToken.IsCancellationRequested Then
                        Throw New OperationCanceledException
                    End If
                    Dim m As CSS.MemberDeclarationSyntax = e.Value
                    Dim Statement As VBS.StatementSyntax = DirectCast(m.Accept(Me), VBS.StatementSyntax)
                    ' Cases below are handled by RestructureAttributesAndModifiers
                    If Statement.IsKind(VB.SyntaxKind.FieldDeclaration) Then
                        members.Add(Statement)
                        members.AddRange(AddFinalTriviaToField(DirectCast(m, CSS.FieldDeclarationSyntax)))
                    ElseIf Statement.IsKind(VB.SyntaxKind.ClassBlock) OrElse
                        Statement.IsKind(VB.SyntaxKind.EmptyStatement) OrElse
                        Statement.IsKind(VB.SyntaxKind.EnumBlock) OrElse
                        Statement.IsKind(VB.SyntaxKind.FunctionBlock) OrElse
                        Statement.IsKind(VB.SyntaxKind.FunctionStatement) OrElse
                        Statement.IsKind(VB.SyntaxKind.InterfaceBlock) OrElse
                        Statement.IsKind(VB.SyntaxKind.ModuleBlock) OrElse
                        Statement.IsKind(VB.SyntaxKind.StructureBlock) OrElse
                        Statement.IsKind(VB.SyntaxKind.SubBlock) OrElse
                        Statement.IsKind(VB.SyntaxKind.SubStatement) Then
                        members.Add(Statement.WithTrailingEOL)
                    ElseIf Statement.IsKind(VB.SyntaxKind.PropertyBlock) Then
                        If TypeOf m Is CSS.PropertyDeclarationSyntax Then
                            If CType(m, CSS.PropertyDeclarationSyntax).ExpressionBody Is Nothing Then
                                members.AddRange(ReplaceOneStatementWithMarkedStatements(m, Statement.WithTrailingEOL))
                            Else
                                members.AddRange(ReplaceOneStatementWithMarkedStatements(CType(m, CSS.PropertyDeclarationSyntax).ExpressionBody, Statement.WithTrailingEOL))
                            End If
                        Else
                            members.AddRange(ReplaceOneStatementWithMarkedStatements(m, Statement.WithTrailingEOL))
                        End If

                        ' Cases below are handled in-line
                    ElseIf Statement.IsKind(VB.SyntaxKind.ConstructorBlock) OrElse
                        Statement.IsKind(VB.SyntaxKind.DelegateFunctionStatement) OrElse
                        Statement.IsKind(VB.SyntaxKind.DelegateSubStatement) OrElse
                        Statement.IsKind(VB.SyntaxKind.EventBlock) OrElse
                        Statement.IsKind(VB.SyntaxKind.EventStatement) OrElse
                        Statement.IsKind(VB.SyntaxKind.OperatorBlock) OrElse
                        Statement.IsKind(VB.SyntaxKind.PropertyStatement) Then
                        members.Add(Statement.WithConvertedTriviaFrom(m).WithTrailingEOL)
                    Else
                        members.Add(Statement.WithConvertedTriviaFrom(m).WithTrailingEOL)
                    End If
                    If e.IsFirst Then
                        If node.OpenBraceToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                            If members.Any Then
                                members(0) = members(0).WithPrependedLeadingTrivia(ConvertOpenBraceTrivia(node.OpenBraceToken))
                            Else
                                Stop
                            End If
                        End If
                    End If
                Next

                Dim id As SyntaxToken = GenerateSafeVBToken(node.Identifier).WithConvertedTrailingTriviaFrom(node.Identifier)

                members.AddRange(PatchInlineHelpers(node, IsModule))

                Dim ListOfAttributes As SyntaxList(Of VBS.AttributeListSyntax) = VBFactory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))
                Dim typeParameterList As VBS.TypeParameterListSyntax = DirectCast(node.TypeParameterList?.Accept(Me), VBS.TypeParameterListSyntax)
                If typeParameterList IsNot Nothing Then
                    If id.TrailingTrivia.ContainsCommentTrivia Then
                        Stop
                    End If
                    id = id.WithTrailingTrivia(SpaceTrivia)
                End If
                Dim NotInsideClassOrStruct As Boolean = _isModuleStack.Count < 2 AndAlso IsNotInStructure(node)

                If IsModule AndAlso NotInsideClassOrStruct Then
                    Dim ModuleModifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, IsModule, TokenContext.InterfaceOrModule)
                    Dim ModuleKeywordWithTrivia As SyntaxToken = ModuleKeyword.WithConvertedLeadingTriviaFrom(node.Keyword).WithTrailingTrivia(SpaceTrivia)
                    Dim PrependedTrivia As List(Of SyntaxTrivia) = DedupLeadingTrivia(node, ModuleKeyword, ListOfAttributes.ToList, ModuleModifiers)
                    Dim ModuleStatement As VBS.ModuleStatementSyntax = DirectCast(VBFactory.ModuleStatement(
                                                                            ListOfAttributes,
                                                                            VBFactory.TokenList(ModuleModifiers),
                                                                            ModuleKeywordWithTrivia,
                                                                            id,
                                                                            typeParameterList
                                                                            ).WithPrependedLeadingTrivia(PrependedTrivia).
                                                                            RestructureAttributesAndModifiers(ListOfAttributes.Any, ModuleModifiers.Any), VBS.ModuleStatementSyntax).WithTrailingEOL

                    ModuleStatement = DirectCast(PrependStatementWithMarkedStatementTrivia(node, ModuleStatement), VBS.ModuleStatementSyntax)
                    Dim EndModule As VBS.EndBlockStatementSyntax = VBFactory.EndModuleStatement().WithConvertedTriviaFrom(node.CloseBraceToken)
                    Dim ModuleBlock As VBS.ModuleBlockSyntax = VBFactory.ModuleBlock(
                                                                                    ModuleStatement,
                                                                                    VBFactory.List([inherits]),
                                                                                    VBFactory.List([implements]),
                                                                                    VBFactory.List(members),
                                                                                    EndModule
                                                                                    ).WithAppendedTrailingTrivia(ConvertTrivia(node.GetTrailingTrivia))
                    SyncLock s_usedStacks
                        _isModuleStack.Pop()
                        If s_usedStacks.Count > 0 Then
                            s_usedIdentifiers = DirectCast(s_usedStacks.Pop, Dictionary(Of String, SymbolTableEntry))
                        End If
                        If s_implementedMembersStack.Count > 0 Then
                            s_implementedMembers = DirectCast(s_implementedMembersStack.Pop, ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))))
                        End If
                    End SyncLock
                    Return ModuleBlock
                Else
                    Dim ClassModifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, IsModule:=False, If(IsModule, TokenContext.InterfaceOrModule, TokenContext.Class))
                    Dim ClassKeywordWithTrivia As SyntaxToken = ClassKeyWord.WithConvertedTriviaFrom(node.Keyword)
                    Dim PrependedTrivia As List(Of SyntaxTrivia) = DedupLeadingTrivia(node, ClassKeywordWithTrivia, ListOfAttributes.ToList, ClassModifiers)
                    Dim ClassStatement As VBS.ClassStatementSyntax = DirectCast(VBFactory.ClassStatement(
                                                                            ListOfAttributes,
                                                                            VBFactory.TokenList(ClassModifiers),
                                                                            ClassKeywordWithTrivia,
                                                                            id,
                                                                            typeParameterList
                                                                            ).WithPrependedLeadingTrivia(PrependedTrivia).
                                                                            RestructureAttributesAndModifiers(ListOfAttributes.Any, ClassModifiers.Any), VBS.ClassStatementSyntax)
                    ClassStatement = DirectCast(PrependStatementWithMarkedStatementTrivia(node, ClassStatement), VBS.ClassStatementSyntax)
                    If [inherits].Count = 0 AndAlso [implements].Count = 0 Then
                        ClassStatement = ClassStatement.WithTrailingEOL
                    Else
                        If ClassStatement.GetTrailingTrivia.ContainsCommentOrDirectiveTrivia Then
                            Dim OldTrailingTrivia As SyntaxTriviaList = ClassStatement.GetTrailingTrivia
                            Dim NewTrailingTrivia As New List(Of SyntaxTrivia)
                            For Each e As IndexClass(Of SyntaxTrivia) In OldTrailingTrivia.WithIndex
                                Dim Trivia As SyntaxTrivia = e.Value
                                Dim NextTrivia As SyntaxTrivia = If(Not e.IsLast, OldTrailingTrivia(e.Index + 1), Nothing)
                                Dim FoundSpace As Boolean = False
                                Select Case Trivia.RawKind
                                    Case VB.SyntaxKind.WhitespaceTrivia
                                        If NextTrivia.IsKind(VB.SyntaxKind.None) OrElse NextTrivia.IsKind(VB.SyntaxKind.EndOfLineTrivia) Then
                                            Continue For
                                        End If
                                        NewTrailingTrivia.Add(SpaceTrivia)
                                        FoundSpace = True
                                    Case VB.SyntaxKind.EndOfLineTrivia
                                        FoundSpace = False
                                    Case VB.SyntaxKind.EndIfDirectiveTrivia
                                        NewTrailingTrivia.Add(Trivia)
                                        If NextTrivia.IsKind(VB.SyntaxKind.WhitespaceTrivia) Then
                                            NewTrailingTrivia.Add(VBEOLTrivia)
                                            NewTrailingTrivia.Add(NextTrivia)
                                            e.MoveNext()
                                        End If
                                    Case VB.SyntaxKind.CommentTrivia
                                        If Not FoundSpace Then
                                            NewTrailingTrivia.Add(SpaceTrivia)
                                        End If
                                        NewTrailingTrivia.Add(LineContinuation)
                                        NewTrailingTrivia.Add(SpaceTrivia)
                                        NewTrailingTrivia.Add(Trivia)
                                    Case Else
                                End Select
                            Next
                            ClassStatement = ClassStatement.WithTrailingTrivia(NewTrailingTrivia)
                        Else
                            ClassStatement = ClassStatement.WithTrailingTrivia(SpaceTrivia)
                        End If
                    End If
                    Dim EndClass As VBS.EndBlockStatementSyntax = VBFactory.EndClassStatement().WithConvertedTriviaFrom(node.CloseBraceToken)
                    Dim ClassBlock As VBS.ClassBlockSyntax = VBFactory.ClassBlock(
                                                                                ClassStatement,
                                                                                VBFactory.List([inherits]),
                                                                                VBFactory.List([implements]),
                                                                                VBFactory.List(members),
                                                                                EndClass
                                                                                ).WithConvertedTrailingTriviaFrom(node)
                    s_usedIdentifiers = saveUsedIdentifiers
                    SyncLock s_usedStacks
                        _isModuleStack.Pop()
                        If s_usedStacks.Count > 0 Then
                            s_usedIdentifiers = DirectCast(s_usedStacks.Pop, Dictionary(Of String, SymbolTableEntry))
                        End If
                        If s_implementedMembersStack.Count > 0 Then
                            s_implementedMembers = DirectCast(s_implementedMembersStack.Pop, ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))))
                        End If
                    End SyncLock
                    Return ClassBlock
                End If
            End Function

            Public Overrides Function VisitDelegateDeclaration(node As CSS.DelegateDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim Identifier As SyntaxToken = GenerateSafeVBToken(node.Identifier)
                Dim methodInfo As INamedTypeSymbol = TryCast(ModelExtensions.GetDeclaredSymbol(_mSemanticModel, node), INamedTypeSymbol)
                Dim AttributeLists As SyntaxList(Of VBS.AttributeListSyntax) = VBFactory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))
                Dim Modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, IsModule)
                Dim TypeParameterList As VBS.TypeParameterListSyntax = DirectCast(node.TypeParameterList?.Accept(Me), VBS.TypeParameterListSyntax)?.WithoutTrailingTrivia
                Dim ParameterList As VBS.ParameterListSyntax = DirectCast(node.ParameterList?.Accept(Me), VBS.ParameterListSyntax)?.WithoutTrailingTrivia
                If methodInfo.DelegateInvokeMethod.GetReturnType()?.SpecialType = SpecialType.System_Void Then
                    Return VBFactory.DelegateSubStatement(AttributeLists, VBFactory.TokenList(Modifiers), Identifier, TypeParameterList, ParameterList, asClause:=Nothing).WithConvertedTriviaFrom(node)
                Else
                    Dim VBNode As VB.VisualBasicSyntaxNode = node.ReturnType.Accept(Me)
                    Dim ReturnType As VBS.TypeSyntax = DirectCast(VBNode, VBS.TypeSyntax)
                    Dim AsClause As VBS.SimpleAsClauseSyntax = VBFactory.SimpleAsClause(ReturnType.WithLeadingTrivia(SpaceTrivia))
                    Return VBFactory.DelegateFunctionStatement(AttributeLists, VBFactory.TokenList(Modifiers), Identifier, TypeParameterList, ParameterList, AsClause).WithConvertedTriviaFrom(node)
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
                    Dim movedTrailingSpace As String = ""
                    Dim vbEnumSatement As VBS.StatementSyntax
                    Dim leadingTrivia As List(Of SyntaxTrivia) = ConvertOpenBraceTrivia(node.OpenBraceToken)
                    For Each e As IndexClass(Of CSS.EnumMemberDeclarationSyntax) In csMembers.WithIndex
                        If e.IsLast Then
                            Exit For
                        End If
                        csIdentifierTrailingTrivia = e.Value.Identifier.TrailingTrivia
                        If csIdentifierTrailingTrivia.Count = 1 AndAlso csIdentifierTrailingTrivia(0).IsWhitespace Then
                            movedTrailingSpace &= csIdentifierTrailingTrivia(0).ToString & " "
                        ElseIf csIdentifierTrailingTrivia.Count = 0 Then
                            movedTrailingSpace &= " "
                        Else
                            Stop
                        End If
                        vbEnumSatement = DirectCast(e.Value.Accept(Me), VBS.StatementSyntax)
                        csSeparatorTrailingTrivia = csSeparators(e.Index).TrailingTrivia
                        If csSeparatorTrailingTrivia.Any Then
                            If csSeparatorTrailingTrivia(0).IsKind(CS.SyntaxKind.WhitespaceTrivia) Then
                                movedTrailingSpace &= csSeparatorTrailingTrivia(0).ToString
                                csSeparatorTrailingTrivia = csSeparatorTrailingTrivia.RemoveAt(0)
                                csSeparatorTrailingTrivia = csSeparatorTrailingTrivia.Insert(0, CS.SyntaxFactory.Whitespace(movedTrailingSpace))
                            ElseIf csSeparatorTrailingTrivia(0).IsWhitespace Then
                                ' ignore
                            Else
                                Stop
                            End If
                        Else
                            Stop
                        End If
                        If e.IsFirst Then
                            members.Add(vbEnumSatement.WithPrependedLeadingTrivia(leadingTrivia).WithTrailingTrivia(ConvertTrivia(csSeparatorTrailingTrivia)))
                        Else
                            members.Add(vbEnumSatement.WithTrailingTrivia(ConvertTrivia(csSeparatorTrailingTrivia)))
                        End If
                        movedTrailingSpace = ""
                    Next
                    If csSeparators.Count = 0 OrElse csSeparators.Count <> csMembers.Count Then
                        csSeparatorTrailingTrivia = New SyntaxTriviaList
                    Else
                        csSeparatorTrailingTrivia = csSeparators.Last.TrailingTrivia
                    End If
                    If csIdentifierTrailingTrivia.Any AndAlso csIdentifierTrailingTrivia(0).IsWhitespace Then
                        movedTrailingSpace &= csIdentifierTrailingTrivia(0).ToString
                    ElseIf csIdentifierTrailingTrivia.Count = 0 Then
                    Else
                        Stop
                    End If
                    vbEnumSatement = DirectCast(csMembers.Last.Accept(Me), VBS.StatementSyntax)
                    If movedTrailingSpace.Any Then
                        Dim MemberLastTrailingTrivia As SyntaxTriviaList = csMembers.Last.GetTrailingTrivia
                        If csSeparatorTrailingTrivia.Any AndAlso csSeparatorTrailingTrivia(0).IsKind(CS.SyntaxKind.WhitespaceTrivia) Then
                            movedTrailingSpace &= csSeparatorTrailingTrivia(0).ToString
                            csSeparatorTrailingTrivia = csSeparatorTrailingTrivia.RemoveAt(0)
                            csSeparatorTrailingTrivia = csSeparatorTrailingTrivia.Insert(0, CS.SyntaxFactory.Whitespace(movedTrailingSpace))
                        ElseIf csMembers.Last.HasTrailingTrivia Then
                            If MemberLastTrailingTrivia(0).IsKind(CS.SyntaxKind.WhitespaceTrivia) Then
                                movedTrailingSpace &= MemberLastTrailingTrivia(0).ToString
                                MemberLastTrailingTrivia = MemberLastTrailingTrivia.RemoveAt(0)
                                MemberLastTrailingTrivia = MemberLastTrailingTrivia.Insert(0, CS.SyntaxFactory.Whitespace(movedTrailingSpace))
                                csMembers = csMembers.Replace(csMembers.Last, csMembers.Last.WithTrailingTrivia(MemberLastTrailingTrivia))
                            ElseIf csMembers.Last.GetTrailingTrivia(0).IsWhitespace Then
                                ' ignore
                            Else
                                Stop
                            End If
                        End If
                    End If

                    If csMembers.Count = 1 Then
                        members.Add(DirectCast(csMembers.Last.Accept(Me), VBS.StatementSyntax).WithPrependedLeadingTrivia(leadingTrivia).WithAppendedTrailingTrivia(ConvertTrivia(csSeparatorTrailingTrivia)))
                    Else
                        members.Add(DirectCast(csMembers.Last.Accept(Me), VBS.StatementSyntax).WithAppendedTrailingTrivia(ConvertTrivia(csSeparatorTrailingTrivia)))
                    End If
                End If

                Dim BaseType As VBS.TypeSyntax = DirectCast(node.BaseList?.Types.Single().Accept(Me), VBS.TypeSyntax)
                Dim ListOfAttributes As SyntaxList(Of VBS.AttributeListSyntax) = VBFactory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))
                Dim Modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, IsModule)
                Dim UnderlyingType As VBS.SimpleAsClauseSyntax = If(BaseType Is Nothing, Nothing, VBFactory.SimpleAsClause(BaseType))
                Dim EnumStatement As VBS.EnumStatementSyntax = DirectCast(VBFactory.EnumStatement(ListOfAttributes,
                                                                                               VBFactory.TokenList(Modifiers),
                                                                                               EnumKeyword.WithConvertedTriviaFrom(node.EnumKeyword),
                                                                                               identifier:=GenerateSafeVBToken(id:=node.Identifier),
                                                                                               UnderlyingType).
                                                                                       RestructureAttributesAndModifiers(ListOfAttributes.Any, Modifiers.Any), VBS.EnumStatementSyntax)

                Dim EndBlockStatement As VBS.EndBlockStatementSyntax = VBFactory.EndEnumStatement().WithConvertedTriviaFrom(node.CloseBraceToken)
                Dim EnumBlock As VBS.EnumBlockSyntax = VBFactory.EnumBlock(EnumStatement.WithTrailingEOL,
                                                                           VBFactory.List(members),
                                                                           EndBlockStatement
                                                                           )
                Return PrependStatementWithMarkedStatementTrivia(node, EnumBlock)
            End Function

            Public Overrides Function VisitEnumMemberDeclaration(node As CSS.EnumMemberDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim initializer As VBS.ExpressionSyntax = DirectCast(node.EqualsValue?.Value.Accept(Me), VBS.ExpressionSyntax)
                Return VBFactory.EnumMemberDeclaration(VBFactory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax))), GenerateSafeVBToken(node.Identifier), initializer:=If(initializer Is Nothing, Nothing, VBFactory.EqualsValue(initializer))).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitExplicitInterfaceSpecifier(node As CSS.ExplicitInterfaceSpecifierSyntax) As VB.VisualBasicSyntaxNode
                Return node.Name.Accept(Me)
            End Function

            Public Overrides Function VisitExternAliasDirective(node As CSS.ExternAliasDirectiveSyntax) As VB.VisualBasicSyntaxNode
                Return FlagUnsupportedStatements(node, "Extern Alias", CommentOutOriginalStatements:=True)
            End Function

            Public Overrides Function VisitInterfaceDeclaration(node As CSS.InterfaceDeclarationSyntax) As VB.VisualBasicSyntaxNode
                SyncLock s_usedStacks
                    If s_implementedMembers.Any Then
                        s_implementedMembersStack.Push(s_implementedMembers)
                        s_implementedMembers = (New List(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol)))).ToImmutableArray
                    End If
                End SyncLock
                Dim ListOfAttributes As SyntaxList(Of VBS.AttributeListSyntax) = VBFactory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))
                Dim Modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, IsModule, TokenContext.InterfaceOrModule)
                If node.Modifiers.Contains(CS.SyntaxKind.UnsafeKeyword) Then
                    Return FlagUnsupportedStatements(node, "unsafe interfaces", CommentOutOriginalStatements:=True)
                End If
                Dim members As New List(Of VBS.StatementSyntax)
                For Each e As IndexClass(Of CSS.MemberDeclarationSyntax) In node.Members.WithIndex
                    If e.IsFirst AndAlso node.OpenBraceToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        members.Add(DirectCast(e.Value.Accept(Me), VBS.StatementSyntax).WithPrependedLeadingTrivia(ConvertTrivia(node.OpenBraceToken.LeadingTrivia)))
                    Else
                        members.Add(DirectCast(e.Value.Accept(Me), VBS.StatementSyntax))
                    End If
                Next
                Dim Identifier As SyntaxToken = GenerateSafeVBToken(node.Identifier)
                Dim TypeParameterList As VBS.TypeParameterListSyntax = DirectCast(node.TypeParameterList?.Accept(Me), VBS.TypeParameterListSyntax)
                Dim StatementLeadingTrivia As New List(Of SyntaxTrivia)
                If node.Modifiers.Any AndAlso Modifiers.Count = 0 Then
                    StatementLeadingTrivia.AddRange(ConvertTrivia(node.Modifiers(0).LeadingTrivia()))
                    Return FlagUnsupportedStatements(node, "Directive within Statement", CommentOutOriginalStatements:=True)
                Else
                    StatementLeadingTrivia.Add(SpaceTrivia)
                End If
                Dim InterfaceStatement As VBS.InterfaceStatementSyntax = DirectCast(
                                    VBFactory.InterfaceStatement(ListOfAttributes,
                                                                    VBFactory.TokenList(Modifiers),
                                                                    InterfaceKeyword.WithLeadingTrivia(StatementLeadingTrivia),
                                                                    Identifier,
                                                                    TypeParameterList).RestructureAttributesAndModifiers(ListOfAttributes.Any, Modifiers.Any),
                                                                            VBS.InterfaceStatementSyntax
                                                                            ).WithTrailingEOL
                Dim [inherits] As List(Of VBS.InheritsStatementSyntax) = New List(Of VBS.InheritsStatementSyntax)()
                Dim [implements] As List(Of VBS.ImplementsStatementSyntax) = New List(Of VBS.ImplementsStatementSyntax)()
                ConvertBaseList(node, [inherits], [implements])
                Dim InheritsStatementList As SyntaxList(Of VBS.InheritsStatementSyntax) = VBFactory.List([inherits])
                Dim ImplementsStatementList As SyntaxList(Of VBS.ImplementsStatementSyntax) = VBFactory.List([implements])
                Dim StatementList As SyntaxList(Of VBS.StatementSyntax) = VBFactory.List(members)
                Dim EndInterfaceStatement As VBS.EndBlockStatementSyntax = VBFactory.EndInterfaceStatement.WithConvertedLeadingTriviaFrom(node.CloseBraceToken)
                Return VBFactory.InterfaceBlock(InterfaceStatement,
                                                    InheritsStatementList,
                                                    ImplementsStatementList,
                                                    StatementList,
                                                    EndInterfaceStatement
                                                    ).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitNamespaceDeclaration(node As CSS.NamespaceDeclarationSyntax) As VB.VisualBasicSyntaxNode
                SyncLock s_usedStacks
                    If s_usedStacks.Count > 0 Then
                        s_usedIdentifiers = DirectCast(s_usedStacks.Pop, Dictionary(Of String, SymbolTableEntry))
                    End If
                End SyncLock

                For Each [using] As CSS.UsingDirectiveSyntax In node.Usings
                    [using].Accept(Me)
                Next

                For Each extern As CSS.ExternAliasDirectiveSyntax In node.Externs
                    extern.Accept(Me)
                Next

                Dim LeadingTrivia As List(Of SyntaxTrivia) = ConvertOpenBraceTrivia(node.OpenBraceToken)
                Dim members As New List(Of VBS.StatementSyntax)

                For Each e As IndexClass(Of CSS.MemberDeclarationSyntax) In node.Members.WithIndex
                    If s_originalRequest.CancelToken.IsCancellationRequested Then
                        Throw New OperationCanceledException
                    End If
                    members.Add(DirectCast(e.Value.Accept(Me), VBS.StatementSyntax))
                    If e.IsFirst AndAlso LeadingTrivia.Any Then
                        members(0) = members(0).WithPrependedLeadingTrivia(LeadingTrivia)
                    End If
                    If e.IsLast Then
                        members(e.Index) = members.Last.
                            WithAppendedTrailingTrivia(ConvertTrivia(node.CloseBraceToken.LeadingTrivia)).
                            WithAppendedTrailingTrivia(ConvertTrivia(node.CloseBraceToken.TrailingTrivia))
                    End If
                Next

                Dim NamespaceStatement As VBS.NamespaceStatementSyntax = VBFactory.NamespaceStatement(NamespaceKeyword, DirectCast(node.Name.Accept(Me), VBS.NameSyntax))
                Dim members1 As SyntaxList(Of VBS.StatementSyntax) = VBFactory.List(members)
                Dim EndNamespaceStatement As VBS.EndBlockStatementSyntax = VBFactory.EndNamespaceStatement
                Dim namespaceBlock As VBS.NamespaceBlockSyntax = VBFactory.NamespaceBlock(NamespaceStatement, members1, EndNamespaceStatement).WithConvertedTriviaFrom(node)
                Return namespaceBlock
            End Function

            Public Overrides Function VisitStructDeclaration(node As CSS.StructDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim [inherits] As List(Of VBS.InheritsStatementSyntax) = New List(Of VBS.InheritsStatementSyntax)
                Dim [implements] As List(Of VBS.ImplementsStatementSyntax) = New List(Of VBS.ImplementsStatementSyntax)
                If s_implementedMembers.Any Then
                    s_implementedMembersStack.Push(s_implementedMembers)
                    s_implementedMembers = (New List(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol)))).ToImmutableArray
                End If
                ConvertBaseList(node, [inherits], [implements], s_implementedMembers)
                Dim Members As New List(Of VBS.StatementSyntax)
                For Each m As CSS.MemberDeclarationSyntax In node.Members
                    Dim Item As VBS.StatementSyntax = DirectCast(m.Accept(Me), VBS.StatementSyntax)
                    If Item Is Nothing Then
                        Members.Add(VBFactory.EmptyStatement.WithConvertedTriviaFrom(m))
                    Else
                        Members.AddRange(ReplaceOneStatementWithMarkedStatements(m, Item))
                    End If
                Next
                SyncLock s_usedStacks
                    If s_implementedMembersStack.Count > 0 Then
                        s_implementedMembers = DirectCast(s_implementedMembersStack.Pop, ImmutableArray(Of (type As INamedTypeSymbol, members As ImmutableArray(Of ISymbol))))
                    End If
                End SyncLock
                If Members.Any Then
                    Members(0) = Members(0).WithPrependedLeadingTrivia(ConvertOpenBraceTrivia(node.OpenBraceToken))
                End If
                Dim ListOfAttributes As SyntaxList(Of VBS.AttributeListSyntax) = VBFactory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))
                Dim TypeParameterList As VBS.TypeParameterListSyntax = DirectCast(node.TypeParameterList?.Accept(Me), VBS.TypeParameterListSyntax)
                Dim Modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, IsModule, TokenContext.Struct)
                Dim StructureStatement As VBS.StructureStatementSyntax
                StructureStatement = DirectCast(VBFactory.StructureStatement(ListOfAttributes,
                                                                            VBFactory.TokenList(Modifiers),
                                                                            StructureKeyword.WithConvertedTriviaFrom(node.Keyword),
                                                                            GenerateSafeVBToken(node.Identifier),
                                                                            TypeParameterList
                                                                            ).RestructureAttributesAndModifiers(ListOfAttributes.Any, Modifiers.Any), VBS.StructureStatementSyntax).WithTrailingEOL

                Dim StructureBlock As VBS.StructureBlockSyntax = VBFactory.StructureBlock(
                                                    StructureStatement,
                                                    VBFactory.List([inherits]),
                                                    VBFactory.List([implements]),
                                                    VBFactory.List(Members),
                                                    VBFactory.EndStructureStatement.WithConvertedTriviaFrom(node.CloseBraceToken)
                                                    )
                Dim ErrorModifiers As New List(Of String)
                For Each t As SyntaxToken In node.Modifiers
                    Select Case t.Text
                        Case "unsafe"
                            ErrorModifiers.Add("unsafe")
                        Case "ref"
                            ErrorModifiers.Add("unsafe")
                    End Select
                Next

                ' These errors are handled elsewhere just ignore
                ReplaceOneStatementWithMarkedStatements(node, StructureBlock)
                If ErrorModifiers.Any Then
                    StructureBlock = StructureBlock.WithPrependedLeadingTrivia(VBFactory.CommentTrivia($"' TODO TASK: VB has no direct equivalent to C# {String.Join(" or ", ErrorModifiers)} Structure"))
                End If
                Return StructureBlock
            End Function

            Public Overrides Function VisitUsingDirective(node As CSS.UsingDirectiveSyntax) As VB.VisualBasicSyntaxNode
                SyncLock s_usedStacks
                    s_usedStacks.Push(s_usedIdentifiers)
                End SyncLock
                s_usedIdentifiers.Clear()
                Dim ImportsName As VBS.NameSyntax
                Dim [Alias] As VBS.ImportAliasClauseSyntax = Nothing
                Dim Identifier As SyntaxToken
                If node.[Alias] IsNot Nothing Then
                    Dim name As CSS.IdentifierNameSyntax = node.[Alias].Name
                    Identifier = GenerateSafeVBToken(name.Identifier)
                    [Alias] = VBFactory.ImportAliasClause(Identifier)
                End If
                ImportsName = DirectCast(node.Name.Accept(Me), VBS.NameSyntax)

                Dim clause As VBS.ImportsClauseSyntax = VBFactory.SimpleImportsClause([Alias], ImportsName)
                Dim import As VBS.ImportsStatementSyntax = VBFactory.ImportsStatement(VBFactory.SingletonSeparatedList(clause)).WithConvertedTriviaFrom(node)
                Dim MatchNotFound As Boolean = True
                If _allImports.Any Then
                    For Each ImportStatement As VBS.ImportsStatementSyntax In _allImports
                        Dim ImportsClause As VBS.SimpleImportsClauseSyntax = DirectCast(ImportStatement.ImportsClauses(0), VBS.SimpleImportsClauseSyntax)
                        If ImportsClause.Alias IsNot Nothing AndAlso ImportsClause.Alias.ToString = [Alias]?.ToString Then
                            MatchNotFound = False
                            Exit For
                        Else
                            If ImportsClause.Name.ToString = ImportsName.ToString Then
                                MatchNotFound = False
                                Exit For
                            End If
                        End If
                    Next
                End If
                If MatchNotFound Then
                    _allImports.Add(import)
                End If

                SyncLock s_usedStacks
                    If s_usedStacks.Count > 0 Then
                        s_usedIdentifiers = DirectCast(s_usedStacks.Pop, Dictionary(Of String, SymbolTableEntry))
                    End If
                End SyncLock
                Return Nothing
            End Function

        End Class

    End Class

End Namespace
