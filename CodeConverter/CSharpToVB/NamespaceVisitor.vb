Option Explicit On
Option Infer Off
Option Strict On

Imports IVisualBasicCode.CodeConverter.Util

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace IVisualBasicCode.CodeConverter.Visual_Basic

    Partial Public Class CSharpConverter

        Partial Protected Friend Class NodesVisitor
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

            Private Sub ConvertBaseList(type As CSS.BaseTypeDeclarationSyntax, [inherits] As List(Of VBS.InheritsStatementSyntax), [implements] As List(Of VBS.ImplementsStatementSyntax))
                Dim arr As VBS.TypeSyntax()
                Select Case type.Kind()
                    Case CS.SyntaxKind.ClassDeclaration
                        Dim classOrInterface As CSS.TypeSyntax = type.BaseList?.Types.FirstOrDefault()?.Type
                        If classOrInterface Is Nothing Then
                            Return
                        End If
                        Dim classOrInterfaceSymbol As ISymbol = ModelExtensions.GetSymbolInfo(Me.mSemanticModel, classOrInterface).Symbol
                        If classOrInterfaceSymbol?.IsInterfaceType() Then
                            arr = type.BaseList?.Types.Select(Function(t As CSS.BaseTypeSyntax) DirectCast(t.Type.Accept(Me), VBS.TypeSyntax)).ToArray()
                            If arr.Length > 0 Then [implements].Add(VB.SyntaxFactory.ImplementsStatement(arr))
                        Else
                            Dim TypeNode As VB.VisualBasicSyntaxNode = classOrInterface.Accept(Me)
                            If TypeNode.HasLeadingTrivia Then
                                Dim TypeNodeLeadingTrivia As SyntaxTriviaList = TypeNode.GetLeadingTrivia
                                If TypeNodeLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                                    [inherits].Add(VB.SyntaxFactory.InheritsStatement(DirectCast(TypeNode.WithLeadingTrivia(SpaceTrivia), VBS.TypeSyntax)).WithLeadingTrivia(TypeNodeLeadingTrivia))
                                Else
                                    [inherits].Add(VB.SyntaxFactory.InheritsStatement(DirectCast(TypeNode, VBS.TypeSyntax)))
                                End If
                            Else
                                [inherits].Add(VB.SyntaxFactory.InheritsStatement(DirectCast(TypeNode, VBS.TypeSyntax)))
                            End If
                            arr = type.BaseList?.Types.Skip(1).Select(Function(t As CSS.BaseTypeSyntax) DirectCast(t.Type.Accept(Me).WithTrailingTrivia(SpaceTrivia), VBS.TypeSyntax)).ToArray()
                            If arr.Length > 0 Then [implements].Add(VB.SyntaxFactory.ImplementsStatement(arr))
                        End If

                    Case CS.SyntaxKind.StructDeclaration
                        arr = type.BaseList?.Types.Select(Function(t As CSS.BaseTypeSyntax) DirectCast(t.Type.Accept(Me), VBS.TypeSyntax)).ToArray()
                        If arr?.Length > 0 Then [implements].Add(VB.SyntaxFactory.ImplementsStatement(arr))
                    Case CS.SyntaxKind.InterfaceDeclaration
                        arr = type.BaseList?.Types.Select(Function(t As CSS.BaseTypeSyntax) DirectCast(t.Type.Accept(Me), VBS.TypeSyntax)).ToArray()
                        If arr?.Length > 0 Then [inherits].Add(VB.SyntaxFactory.InheritsStatement(arr))
                End Select
                If [implements].Count > 0 Then
                    [implements]([implements].Count - 1) = [implements].Last.WithTrailingEOL
                End If
                If [inherits].Count > 0 Then
                    [inherits]([inherits].Count - 1) = [inherits].Last.WithTrailingEOL
                End If
            End Sub

            Private Iterator Function PatchInlineHelpers(node As CSS.BaseTypeDeclarationSyntax, IsModule As Boolean) As IEnumerable(Of VBS.StatementSyntax)
                If Me.inlineAssignHelperMarkers.Contains(node) Then
                    Me.inlineAssignHelperMarkers.Remove(node)
                    Yield TryCast(VB.SyntaxFactory.ParseSyntaxTree(InlineAssignHelperCode.Replace("Shared ", If(IsModule, "", "Shared "))).GetRoot().ChildNodes().FirstOrDefault(), VBS.StatementSyntax)
                End If
            End Function

            Public Overrides Function VisitClassDeclaration(node As CSS.ClassDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim saveUsedIdentifiers As Dictionary(Of String, SymbolTableEntry) = UsedIdentifiers
                UsedIdentifierStack.Push(UsedIdentifiers)
                Me.IsModuleStack.Push(node.Modifiers.Any(CS.SyntaxKind.StaticKeyword) And node.TypeParameterList Is Nothing)

                Dim members As New List(Of VBS.StatementSyntax)
                Dim MembersLastIndex As Integer = node.Members.Count - 1

                For i As Integer = 0 To MembersLastIndex
                    Dim m As CSS.MemberDeclarationSyntax = node.Members(i)
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
                        Statement.IsKind(VB.SyntaxKind.PropertyBlock) OrElse
                        Statement.IsKind(VB.SyntaxKind.StructureBlock) OrElse
                        Statement.IsKind(VB.SyntaxKind.SubBlock) OrElse
                        Statement.IsKind(VB.SyntaxKind.SubStatement) Then
                        members.Add(Statement.WithTrailingEOL)
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
                    If i = 0 Then
                        members(0) = members(0).WithPrependedLeadingTrivia(ConvertOpenBraceTrivia(node.OpenBraceToken))
                    End If
                Next

                Dim id As SyntaxToken = GenerateSafeVBToken(node.Identifier, False)

                Dim [inherits] As New List(Of VBS.InheritsStatementSyntax)()
                Dim [implements] As New List(Of VBS.ImplementsStatementSyntax)()

                Me.ConvertBaseList(node, [inherits], [implements])
                members.AddRange(Me.PatchInlineHelpers(node, Me.IsModule))

                Dim ListOfAttributes As SyntaxList(Of VBS.AttributeListSyntax) = VB.SyntaxFactory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))
                Dim typeParameterList As VBS.TypeParameterListSyntax = DirectCast(node.TypeParameterList?.Accept(Me), VBS.TypeParameterListSyntax)

                If Me.IsModule Then
                    Dim ModuleModifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, Me.IsModule, TokenContext.InterfaceOrModule)
                    Dim ModuleKeywordWithTrivia As SyntaxToken = ModuleKeyword.WithConvertedLeadingTriviaFrom(node.Keyword).WithTrailingTrivia(SpaceTrivia)
                    Dim PrependedTrivia As List(Of SyntaxTrivia) = Me.DedupLeadingTrivia(node, ModuleKeyword, ListOfAttributes.ToList, ModuleModifiers)
                    Dim ModuleStatement As VBS.ModuleStatementSyntax = DirectCast(VB.SyntaxFactory.ModuleStatement(
                                                                            ListOfAttributes,
                                                                            VB.SyntaxFactory.TokenList(ModuleModifiers),
                                                                            ModuleKeywordWithTrivia,
                                                                            id,
                                                                            typeParameterList
                                                                            ).WithPrependedLeadingTrivia(PrependedTrivia).
                                                                            RestructureAttributesAndModifiers(ListOfAttributes.Count > 0, ModuleModifiers.Count > 0), VBS.ModuleStatementSyntax).WithTrailingEOL

                    ModuleStatement = DirectCast(PrependStatementWithMarkedStatementTrivia(node, ModuleStatement), VBS.ModuleStatementSyntax)
                    Dim EndModule As VBS.EndBlockStatementSyntax = VB.SyntaxFactory.EndModuleStatement().WithConvertedTriviaFrom(node.CloseBraceToken)
                    Dim ModuleBlock As VBS.ModuleBlockSyntax = VB.SyntaxFactory.ModuleBlock(
                                                                                    ModuleStatement,
                                                                                    VB.SyntaxFactory.List([inherits]),
                                                                                    VB.SyntaxFactory.List([implements]),
                                                                                    VB.SyntaxFactory.List(members),
                                                                                    EndModule
                                                                                    ).WithAppendedTrailingTrivia(ConvertTrivia(node.GetTrailingTrivia))
                    Me.IsModuleStack.Pop()
                    SyncLock UsedIdentifierStack
                        If UsedIdentifierStack.Count > 0 Then
                            UsedIdentifiers = DirectCast(UsedIdentifierStack.Pop, Dictionary(Of String, SymbolTableEntry))
                        End If
                    End SyncLock
                    Return ModuleBlock
                Else
                    Dim ClassModifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, Me.IsModule, TokenContext.Class)
                    Dim ClassKeywordWithTrivia As SyntaxToken = ClassKeyWord.WithConvertedTriviaFrom(node.Keyword)
                    Dim PrependedTrivia As List(Of SyntaxTrivia) = Me.DedupLeadingTrivia(node, ClassKeywordWithTrivia, ListOfAttributes.ToList, ClassModifiers)
                    Dim ClassStatement As VBS.ClassStatementSyntax = DirectCast(VB.SyntaxFactory.ClassStatement(
                                                                            ListOfAttributes,
                                                                            VB.SyntaxFactory.TokenList(ClassModifiers),
                                                                            ClassKeywordWithTrivia,
                                                                            id,
                                                                            typeParameterList
                                                                            ).WithPrependedLeadingTrivia(PrependedTrivia).
                                                                            RestructureAttributesAndModifiers(ListOfAttributes.Count > 0, ClassModifiers.Count > 0), VBS.ClassStatementSyntax)
                    ClassStatement = DirectCast(PrependStatementWithMarkedStatementTrivia(node, ClassStatement), VBS.ClassStatementSyntax).WithTrailingEOL
                    Dim EndClass As VBS.EndBlockStatementSyntax = VB.SyntaxFactory.EndClassStatement().WithConvertedTriviaFrom(node.CloseBraceToken)
                    Dim ClassBlock As VBS.ClassBlockSyntax = VB.SyntaxFactory.ClassBlock(
                                                                                ClassStatement,
                                                                                VB.SyntaxFactory.List([inherits]),
                                                                                VB.SyntaxFactory.List([implements]),
                                                                                VB.SyntaxFactory.List(members),
                                                                                EndClass
                                                                                ).WithConvertedTrailingTriviaFrom(node)
                    UsedIdentifiers = saveUsedIdentifiers
                    Me.IsModuleStack.Pop()
                    SyncLock UsedIdentifierStack
                        If UsedIdentifierStack.Count > 0 Then
                            UsedIdentifiers = DirectCast(UsedIdentifierStack.Pop, Dictionary(Of String, SymbolTableEntry))
                        End If
                    End SyncLock
                    Return ClassBlock
                End If
            End Function

            Public Overrides Function VisitDelegateDeclaration(node As CSS.DelegateDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim Identifier As SyntaxToken = GenerateSafeVBToken(node.Identifier, False)
                Dim methodInfo As INamedTypeSymbol = TryCast(ModelExtensions.GetDeclaredSymbol(Me.mSemanticModel, node), INamedTypeSymbol)
                Dim AttributeLists As SyntaxList(Of VBS.AttributeListSyntax) = VB.SyntaxFactory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))
                Dim Modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, Me.IsModule)
                Dim TypeParameterList As VBS.TypeParameterListSyntax = DirectCast(node.TypeParameterList?.Accept(Me), VBS.TypeParameterListSyntax)?.WithoutTrailingTrivia
                Dim ParameterList As VBS.ParameterListSyntax = DirectCast(node.ParameterList?.Accept(Me), VBS.ParameterListSyntax)?.WithoutTrailingTrivia
                If methodInfo.DelegateInvokeMethod.GetReturnType()?.SpecialType = SpecialType.System_Void Then
                    Return VB.SyntaxFactory.DelegateSubStatement(AttributeLists, VB.SyntaxFactory.TokenList(Modifiers), Identifier, TypeParameterList, ParameterList, asClause:=Nothing).WithConvertedTriviaFrom(node)
                Else
                    Dim VBNode As VB.VisualBasicSyntaxNode = node.ReturnType.Accept(Me)
                    Dim ReturnType As VBS.TypeSyntax = DirectCast(VBNode, VBS.TypeSyntax)
                    Dim AsClause As VBS.SimpleAsClauseSyntax = VB.SyntaxFactory.SimpleAsClause(ReturnType.WithLeadingTrivia(SpaceTrivia))
                    Return VB.SyntaxFactory.DelegateFunctionStatement(AttributeLists, VB.SyntaxFactory.TokenList(Modifiers), Identifier, TypeParameterList, ParameterList, AsClause).WithConvertedTriviaFrom(node)
                End If
            End Function

            Public Overrides Function VisitEnumDeclaration(node As CSS.EnumDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim members As New List(Of VBS.StatementSyntax)
                Dim CS_Members As SeparatedSyntaxList(Of CSS.EnumMemberDeclarationSyntax) = node.Members
                If CS_Members.Count > 0 Then
                    Dim CS_Separators As New List(Of SyntaxToken)
                    CS_Separators.AddRange(node.Members.GetSeparators)

                    Dim CS_IdentifierTrailingTrivia As SyntaxTriviaList
                    Dim CS_SeparatorTrailingTrivia As SyntaxTriviaList
                    Dim MovedTrailingSpace As String = ""
                    Dim TranslatedEnumSatement As VBS.StatementSyntax
                    Dim LeadingTrivia As List(Of SyntaxTrivia) = ConvertOpenBraceTrivia(node.OpenBraceToken)
                    For i As Integer = 0 To CS_Members.Count - 2
                        CS_IdentifierTrailingTrivia = CS_Members(i).Identifier.TrailingTrivia
                        If CS_IdentifierTrailingTrivia.Count = 1 AndAlso CS_IdentifierTrailingTrivia(0).IsWhitespace Then
                            MovedTrailingSpace &= CS_IdentifierTrailingTrivia(0).ToString & " "
                        ElseIf CS_IdentifierTrailingTrivia.Count = 0 Then
                            MovedTrailingSpace &= " "
                        Else
                            Stop
                        End If
                        TranslatedEnumSatement = DirectCast(CS_Members(i).Accept(Me), VBS.StatementSyntax)
                        CS_SeparatorTrailingTrivia = CS_Separators(i).TrailingTrivia
                        If CS_SeparatorTrailingTrivia.Any Then
                            If CS_SeparatorTrailingTrivia(0).IsKind(CS.SyntaxKind.WhitespaceTrivia) Then
                                MovedTrailingSpace &= CS_SeparatorTrailingTrivia(0).ToString
                                CS_SeparatorTrailingTrivia = CS_SeparatorTrailingTrivia.RemoveAt(0)
                                CS_SeparatorTrailingTrivia = CS_SeparatorTrailingTrivia.Insert(0, CS.SyntaxFactory.Whitespace(MovedTrailingSpace))
                            ElseIf CS_SeparatorTrailingTrivia(0).IsWhitespace Then
                                ' ignore
                            Else
                                Stop
                            End If
                        Else
                            Stop
                        End If
                        If i = 0 Then
                            members.Add(TranslatedEnumSatement.WithPrependedLeadingTrivia(LeadingTrivia).WithTrailingTrivia(ConvertTrivia(CS_SeparatorTrailingTrivia)))
                        Else
                            members.Add(TranslatedEnumSatement.WithTrailingTrivia(ConvertTrivia(CS_SeparatorTrailingTrivia)))
                        End If
                        MovedTrailingSpace = ""
                    Next
                    If CS_Separators.Count = 0 OrElse CS_Separators.Count <> CS_Members.Count Then
                        CS_SeparatorTrailingTrivia = New SyntaxTriviaList
                    Else
                        CS_SeparatorTrailingTrivia = CS_Separators.Last.TrailingTrivia
                    End If
                    If CS_IdentifierTrailingTrivia.Any AndAlso CS_IdentifierTrailingTrivia(0).IsWhitespace Then
                        MovedTrailingSpace &= CS_IdentifierTrailingTrivia(0).ToString
                    ElseIf CS_IdentifierTrailingTrivia.Count = 0 Then
                    Else
                        Stop
                    End If
                    TranslatedEnumSatement = DirectCast(CS_Members.Last.Accept(Me), VBS.StatementSyntax)
                    If MovedTrailingSpace.Any Then
                        Dim MemberLastTrailingTrivia As SyntaxTriviaList = CS_Members.Last.GetTrailingTrivia
                        If CS_SeparatorTrailingTrivia.Any AndAlso CS_SeparatorTrailingTrivia(0).IsKind(CS.SyntaxKind.WhitespaceTrivia) Then
                            MovedTrailingSpace &= CS_SeparatorTrailingTrivia(0).ToString
                            CS_SeparatorTrailingTrivia = CS_SeparatorTrailingTrivia.RemoveAt(0)
                            CS_SeparatorTrailingTrivia = CS_SeparatorTrailingTrivia.Insert(0, CS.SyntaxFactory.Whitespace(MovedTrailingSpace))
                        ElseIf CS_Members.Last.HasTrailingTrivia Then
                            If MemberLastTrailingTrivia(0).IsKind(CS.SyntaxKind.WhitespaceTrivia) Then
                                MovedTrailingSpace &= MemberLastTrailingTrivia(0).ToString
                                MemberLastTrailingTrivia = MemberLastTrailingTrivia.RemoveAt(0)
                                MemberLastTrailingTrivia = MemberLastTrailingTrivia.Insert(0, CS.SyntaxFactory.Whitespace(MovedTrailingSpace))
                                CS_Members = CS_Members.Replace(CS_Members.Last, CS_Members.Last.WithTrailingTrivia(MemberLastTrailingTrivia))
                            ElseIf CS_Members.Last.GetTrailingTrivia(0).IsWhitespace Then
                                ' ignore
                            Else
                                Stop
                            End If
                        End If
                    End If

                    If CS_Members.Count = 1 Then
                        members.Add(DirectCast(CS_Members.Last.Accept(Me), VBS.StatementSyntax).WithPrependedLeadingTrivia(LeadingTrivia).WithAppendedTrailingTrivia(ConvertTrivia(CS_SeparatorTrailingTrivia)))
                    Else
                        members.Add(DirectCast(CS_Members.Last.Accept(Me), VBS.StatementSyntax).WithAppendedTrailingTrivia(ConvertTrivia(CS_SeparatorTrailingTrivia)))
                    End If
                End If

                Dim BaseType As VBS.TypeSyntax = DirectCast(node.BaseList?.Types.Single().Accept(Me), VBS.TypeSyntax)
                Dim ListOfAttributes As SyntaxList(Of VBS.AttributeListSyntax) = VB.SyntaxFactory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))
                Dim Modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, Me.IsModule)
                Dim UnderlyingType As VBS.SimpleAsClauseSyntax = If(BaseType Is Nothing, Nothing, VB.SyntaxFactory.SimpleAsClause(BaseType))
                Dim EnumStatement As VBS.EnumStatementSyntax = DirectCast(VB.SyntaxFactory.EnumStatement(ListOfAttributes,
                                                                                               VB.SyntaxFactory.TokenList(Modifiers),
                                                                                               EnumKeyword.WithConvertedTriviaFrom(node.EnumKeyword),
                                                                                               identifier:=GenerateSafeVBToken(id:=node.Identifier, IsQualifiedName:=False),
                                                                                               UnderlyingType).
                                                                                       RestructureAttributesAndModifiers(ListOfAttributes.Count > 0, Modifiers.Count > 0), VBS.EnumStatementSyntax)

                Dim EndBlockStatement As VBS.EndBlockStatementSyntax = VB.SyntaxFactory.EndEnumStatement().WithConvertedTriviaFrom(node.CloseBraceToken)
                Dim EnumBlock As VBS.EnumBlockSyntax = VB.SyntaxFactory.EnumBlock(EnumStatement.WithTrailingEOL,
                                                                           VB.SyntaxFactory.List(members),
                                                                           EndBlockStatement
                                                                           )
                Return PrependStatementWithMarkedStatementTrivia(node, EnumBlock)
            End Function

            Public Overrides Function VisitEnumMemberDeclaration(node As CSS.EnumMemberDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim initializer As VBS.ExpressionSyntax = DirectCast(node.EqualsValue?.Value.Accept(Me), VBS.ExpressionSyntax)
                Return VB.SyntaxFactory.EnumMemberDeclaration(VB.SyntaxFactory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax))), GenerateSafeVBToken(node.Identifier, False), initializer:=If(initializer Is Nothing, Nothing, VB.SyntaxFactory.EqualsValue(initializer))).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitExplicitInterfaceSpecifier(node As CSS.ExplicitInterfaceSpecifierSyntax) As VB.VisualBasicSyntaxNode
                Return node.Name.Accept(Me)
            End Function

            Public Overrides Function VisitExternAliasDirective(node As CSS.ExternAliasDirectiveSyntax) As VB.VisualBasicSyntaxNode
                Return FlagUnsupportedStatements(node, "Extern Alias", CommentOutOriginalStatements:=True)
            End Function

            Public Overrides Function VisitInterfaceDeclaration(node As CSS.InterfaceDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim [inherits] As List(Of VBS.InheritsStatementSyntax) = New List(Of VBS.InheritsStatementSyntax)()
                Dim [implements] As List(Of VBS.ImplementsStatementSyntax) = New List(Of VBS.ImplementsStatementSyntax)()
                Me.ConvertBaseList(node, [inherits], [implements])
                Dim ListOfAttributes As SyntaxList(Of VBS.AttributeListSyntax) = VB.SyntaxFactory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))
                Dim Modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, Me.IsModule, TokenContext.InterfaceOrModule)
                If node.Modifiers.ToString.Contains("unsafe") Then
                    Return FlagUnsupportedStatements(node, "unsafe interfaces", CommentOutOriginalStatements:=True)
                End If
                Dim members As New List(Of VBS.StatementSyntax)
                For i As Integer = 0 To node.Members.Count - 1
                    Dim m As CSS.MemberDeclarationSyntax = node.Members(i)
                    If i = 0 AndAlso node.OpenBraceToken.LeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        members.Add(DirectCast(m.Accept(Me), VBS.StatementSyntax).WithPrependedLeadingTrivia(ConvertTrivia(node.OpenBraceToken.LeadingTrivia)))
                    Else
                        members.Add(DirectCast(m.Accept(Me), VBS.StatementSyntax))
                    End If
                Next
                Dim Identifier As SyntaxToken = GenerateSafeVBToken(node.Identifier, False)
                Dim TypeParameterList As VBS.TypeParameterListSyntax = DirectCast(node.TypeParameterList?.Accept(Me), VBS.TypeParameterListSyntax)
                Dim StatementLeadingTrivia As New List(Of SyntaxTrivia)
                If node.Modifiers.Count > 0 AndAlso Modifiers.Count = 0 Then
                    StatementLeadingTrivia.AddRange(ConvertTrivia(node.Modifiers(0).LeadingTrivia()))
                    Return FlagUnsupportedStatements(node, "Directive within Statement", CommentOutOriginalStatements:=True)
                Else
                    StatementLeadingTrivia.Add(SpaceTrivia)
                End If
                Dim InterfaceStatement As VBS.InterfaceStatementSyntax = DirectCast(
                                    VB.SyntaxFactory.InterfaceStatement(ListOfAttributes,
                                                                    VB.SyntaxFactory.TokenList(Modifiers),
                                                                    InterfaceKeyword.WithLeadingTrivia(StatementLeadingTrivia),
                                                                    Identifier,
                                                                    TypeParameterList).RestructureAttributesAndModifiers(ListOfAttributes.Count > 0, Modifiers.Count > 0),
                                                                            VBS.InterfaceStatementSyntax
                                                                            ).WithTrailingEOL
                Dim InheritsStatementList As SyntaxList(Of VBS.InheritsStatementSyntax) = VB.SyntaxFactory.List([inherits])
                Dim ImplementsStatementList As SyntaxList(Of VBS.ImplementsStatementSyntax) = VB.SyntaxFactory.List([implements])
                Dim StatementList As SyntaxList(Of VBS.StatementSyntax) = VB.SyntaxFactory.List(members)
                Dim EndInterfaceStatement As VBS.EndBlockStatementSyntax = VB.SyntaxFactory.EndInterfaceStatement.WithConvertedLeadingTriviaFrom(node.CloseBraceToken)
                Return VB.SyntaxFactory.InterfaceBlock(InterfaceStatement,
                                                    InheritsStatementList,
                                                    ImplementsStatementList,
                                                    StatementList,
                                                    EndInterfaceStatement
                                                    ).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitNamespaceDeclaration(node As CSS.NamespaceDeclarationSyntax) As VB.VisualBasicSyntaxNode
                SyncLock UsedIdentifierStack
                    If UsedIdentifierStack.Count > 0 Then
                        UsedIdentifiers = DirectCast(UsedIdentifierStack.Pop, Dictionary(Of String, SymbolTableEntry))
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
                Dim LastMemberIndex As Integer = node.Members.Count - 1

                For i As Integer = 0 To LastMemberIndex
                    members.Add(DirectCast(node.Members(i).Accept(Me), VBS.StatementSyntax))
                    If i = 0 AndAlso LeadingTrivia.Count > 0 Then
                        members(0) = members(0).WithPrependedLeadingTrivia(LeadingTrivia)
                    End If
                    If i = LastMemberIndex Then
                        members(LastMemberIndex) = members.Last.
                                                        WithAppendedTrailingTrivia(ConvertTrivia(node.CloseBraceToken.LeadingTrivia)).
                                                        WithAppendedTrailingTrivia(ConvertTrivia(node.CloseBraceToken.TrailingTrivia))
                    End If
                Next

                Dim NamespaceStatement As VBS.NamespaceStatementSyntax = VB.SyntaxFactory.NamespaceStatement(NamespaceKeyword, DirectCast(node.Name.Accept(Me), VBS.NameSyntax))
                Dim members1 As SyntaxList(Of VBS.StatementSyntax) = VB.SyntaxFactory.List(members)
                Dim EndNamespaceStatement As VBS.EndBlockStatementSyntax = VB.SyntaxFactory.EndNamespaceStatement
                Dim namespaceBlock As VBS.NamespaceBlockSyntax = VB.SyntaxFactory.NamespaceBlock(NamespaceStatement, members1, EndNamespaceStatement).WithConvertedTriviaFrom(node)
                Return namespaceBlock
            End Function

            Public Overrides Function VisitStructDeclaration(node As CSS.StructDeclarationSyntax) As VB.VisualBasicSyntaxNode
                Dim Members As New List(Of VBS.StatementSyntax)
                For Each m As CSS.MemberDeclarationSyntax In node.Members
                    Dim Item As VBS.StatementSyntax = DirectCast(m.Accept(Me), VBS.StatementSyntax)
                    If Item Is Nothing Then
                        Members.Add(VB.SyntaxFactory.EmptyStatement.WithConvertedTriviaFrom(m))
                    Else
                        Members.Add(Item)
                    End If
                Next
                Dim [inherits] As List(Of VBS.InheritsStatementSyntax) = New List(Of VBS.InheritsStatementSyntax)()
                Dim [implements] As List(Of VBS.ImplementsStatementSyntax) = New List(Of VBS.ImplementsStatementSyntax)()
                Me.ConvertBaseList(node, [inherits], [implements])
                Members.AddRange(Me.PatchInlineHelpers(node, Me.IsModule))
                If Members.Any Then
                    Members(0) = Members(0).WithPrependedLeadingTrivia(ConvertOpenBraceTrivia(node.OpenBraceToken))
                End If
                Dim ListOfAttributes As SyntaxList(Of VBS.AttributeListSyntax) = VB.SyntaxFactory.List(node.AttributeLists.Select(Function(a As CSS.AttributeListSyntax) DirectCast(a.Accept(Me), VBS.AttributeListSyntax)))
                Dim TypeParameterList As VBS.TypeParameterListSyntax = DirectCast(node.TypeParameterList?.Accept(Me), VBS.TypeParameterListSyntax)
                Dim Modifiers As List(Of SyntaxToken) = ConvertModifiers(node.Modifiers, Me.IsModule, TokenContext.Struct)
                Dim StructureStatement As VBS.StructureStatementSyntax
                StructureStatement = DirectCast(VB.SyntaxFactory.StructureStatement(ListOfAttributes,
                                                                            VB.SyntaxFactory.TokenList(Modifiers),
                                                                            StructureKeyword.WithConvertedTriviaFrom(node.Keyword),
                                                                            GenerateSafeVBToken(node.Identifier, False),
                                                                            TypeParameterList
                                                                            ).RestructureAttributesAndModifiers(ListOfAttributes.Count > 0, Modifiers.Count > 0), VBS.StructureStatementSyntax).WithTrailingEOL

                Dim StructureBlock As VBS.StructureBlockSyntax = VB.SyntaxFactory.StructureBlock(
                                                    StructureStatement,
                                                    VB.SyntaxFactory.List([inherits]),
                                                    VB.SyntaxFactory.List([implements]),
                                                    VB.SyntaxFactory.List(Members),
                                                    VB.SyntaxFactory.EndStructureStatement.WithConvertedTriviaFrom(node.CloseBraceToken)
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
                ReplaceStatementsWithMarkedStatements(node, VB.SyntaxFactory.SingletonList(Of VBS.StatementSyntax)(StructureBlock))
                If ErrorModifiers.Count > 0 Then
                    StructureBlock = StructureBlock.WithPrependedLeadingTrivia(VB.SyntaxFactory.CommentTrivia($"' TODO TASK: VB has no direct equivalent to C# {String.Join(" or ", ErrorModifiers)} Structure"))
                End If
                Return StructureBlock
            End Function

            Public Overrides Function VisitUsingDirective(node As CSS.UsingDirectiveSyntax) As VB.VisualBasicSyntaxNode
                UsedIdentifierStack.Push(UsedIdentifiers)
                UsedIdentifiers.Clear()
                Dim ImportsName As VBS.NameSyntax
                Dim [Alias] As VBS.ImportAliasClauseSyntax = Nothing
                Dim Identifier As SyntaxToken
                If node.[Alias] IsNot Nothing Then
                    Dim name As CSS.IdentifierNameSyntax = node.[Alias].Name
                    Identifier = GenerateSafeVBToken(name.Identifier, False)
                    [Alias] = VB.SyntaxFactory.ImportAliasClause(Identifier)
                End If
                ImportsName = DirectCast(node.Name.Accept(Me), VBS.NameSyntax)

                Dim clause As VBS.ImportsClauseSyntax = VB.SyntaxFactory.SimpleImportsClause([Alias], ImportsName)
                Dim import As VBS.ImportsStatementSyntax = VB.SyntaxFactory.ImportsStatement(VB.SyntaxFactory.SingletonSeparatedList(clause)).WithConvertedTriviaFrom(node)
                Dim MatchNotFound As Boolean = True
                If Me.allImports.Count > 0 Then
                    For Each ImportStatement As VBS.ImportsStatementSyntax In Me.allImports
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
                    Me.allImports.Add(import)
                End If

                SyncLock UsedIdentifierStack
                    If UsedIdentifierStack.Count > 0 Then
                        UsedIdentifiers = DirectCast(UsedIdentifierStack.Pop, Dictionary(Of String, SymbolTableEntry))
                    End If
                End SyncLock
                Return Nothing
            End Function

        End Class

    End Class

End Namespace