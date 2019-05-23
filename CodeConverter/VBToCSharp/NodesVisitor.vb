Option Explicit On
Option Infer Off
Option Strict On

Imports System
Imports System.Collections.Generic
Imports System.Linq
Imports IVisualBasicCode.CodeConverter.Util
Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CSharp
Imports Microsoft.CodeAnalysis.CSharp.Syntax
Imports ArgumentListSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.ArgumentListSyntax
Imports ArgumentSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.ArgumentSyntax
Imports ArrayRankSpecifierSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.ArrayRankSpecifierSyntax
Imports ArrayTypeSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.ArrayTypeSyntax
Imports AttributeListSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.AttributeListSyntax
Imports CatchFilterClauseSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.CatchFilterClauseSyntax
Imports EnumMemberDeclarationSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.EnumMemberDeclarationSyntax
Imports ExpressionSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.ExpressionSyntax
Imports IdentifierNameSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.IdentifierNameSyntax
Imports InterpolatedStringContentSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.InterpolatedStringContentSyntax
Imports NameSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.NameSyntax
Imports ParameterListSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.ParameterListSyntax
Imports ParameterSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.ParameterSyntax
Imports SimpleNameSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.SimpleNameSyntax
Imports SyntaxFactory = Microsoft.CodeAnalysis.CSharp.SyntaxFactory
Imports SyntaxKind = Microsoft.CodeAnalysis.CSharp.SyntaxKind
Imports TypeArgumentListSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.TypeArgumentListSyntax
Imports TypeParameterConstraintClauseSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.TypeParameterConstraintClauseSyntax
Imports TypeParameterListSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.TypeParameterListSyntax
Imports TypeParameterSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.TypeParameterSyntax
Imports TypeSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.TypeSyntax
Imports VBSyntax = Microsoft.CodeAnalysis.VisualBasic.Syntax
Imports VBasic = Microsoft.CodeAnalysis.VisualBasic

Namespace IVisualBasicCode.CodeConverter.CSharp
    Partial Public Class VisualBasicConverter
        Class NodesVisitor
            Inherits VBasic.VisualBasicSyntaxVisitor(Of CSharpSyntaxNode)

            Private mSemanticModel As SemanticModel
            Private mTargetDocument As Document
            Private ReadOnly createConvertMethodsLookupByReturnType_Renamed As Dictionary(Of ITypeSymbol, String)
            Private ReadOnly additionalDeclarations As New Dictionary(Of MemberDeclarationSyntax, MemberDeclarationSyntax())()
            Private ReadOnly withBlockTempVariableNames As New Stack(Of String)()

            Public Sub New(ByVal lSemanticModel As SemanticModel, ByVal targetDocument As Document)
                Me.mSemanticModel = lSemanticModel
                Me.mTargetDocument = targetDocument
                Me.createConvertMethodsLookupByReturnType_Renamed = CreateConvertMethodsLookupByReturnType(lSemanticModel)
            End Sub

            Private Shared Function CreateConvertMethodsLookupByReturnType(ByVal semanticModel As SemanticModel) As Dictionary(Of ITypeSymbol, String)
                Dim systemDotConvert As String = GetType(Convert).FullName
                Dim namedTypeSymbol As INamedTypeSymbol = semanticModel.Compilation.GetTypeByMetadataName(systemDotConvert)
                Dim convertMethods As IEnumerable(Of ISymbol) = namedTypeSymbol.GetMembers().
                    Where(Function(m As ISymbol) m.Name.StartsWith("To", StringComparison.Ordinal) AndAlso m.GetParameters().Length = 1)
                Dim methodsByType As Dictionary(Of ITypeSymbol, String) = convertMethods.Where(Function(m As ISymbol) m.Name <> NameOf(System.Convert.ToBase64String)).
                    GroupBy(Function(m) New With
                                {
                                Key .ReturnType = m.GetReturnType(),
                                Key .Name = $"{systemDotConvert}.{m.Name}"
                                }
                            ).ToDictionary(Function(m) m.Key.ReturnType, Function(m) m.Key.Name)

                Return methodsByType
            End Function
            Public Overrides Function DefaultVisit(ByVal node As SyntaxNode) As CSharpSyntaxNode
                Throw New NotImplementedException($"{node.GetType().FullName} not implemented!")
            End Function

            Public Overrides Function VisitGetTypeExpression(ByVal node As VBSyntax.GetTypeExpressionSyntax) As CSharpSyntaxNode
                Return SyntaxFactory.TypeOfExpression(CType(node.Type.Accept(Me), TypeSyntax))
            End Function

            Public Overrides Function VisitGlobalName(ByVal node As VBSyntax.GlobalNameSyntax) As CSharpSyntaxNode
                Return SyntaxFactory.IdentifierName(SyntaxFactory.Token(SyntaxKind.GlobalKeyword))
            End Function

#Region "Attributes"

            Private Function ConvertAttribute(ByVal attributeList As VBSyntax.AttributeListSyntax) As IEnumerable(Of AttributeListSyntax)
                Return attributeList.Attributes.Select(Function(a As VBSyntax.AttributeSyntax) CType(a.Accept(Me), AttributeListSyntax))
            End Function

            Public Overrides Function VisitAttribute(ByVal node As VBSyntax.AttributeSyntax) As CSharpSyntaxNode
                Return SyntaxFactory.AttributeList(If(node.Target Is Nothing, Nothing, SyntaxFactory.AttributeTargetSpecifier(ConvertToken(node.Target.AttributeModifier))), SyntaxFactory.SingletonSeparatedList(SyntaxFactory.Attribute(CType(node.Name.Accept(Me), NameSyntax), CType(node.ArgumentList?.Accept(Me), AttributeArgumentListSyntax))))
            End Function

#End Region

            Public Overrides Function VisitCompilationUnit(ByVal node As VBSyntax.CompilationUnitSyntax) As CSharpSyntaxNode
                Dim attributes As SyntaxList(Of AttributeListSyntax) = SyntaxFactory.List(node.Attributes.SelectMany(Function(a As VBSyntax.AttributesStatementSyntax) a.AttributeLists).SelectMany(AddressOf ConvertAttribute))
                Dim members As SyntaxList(Of MemberDeclarationSyntax) = SyntaxFactory.List(node.Members.Select(Function(m As VBSyntax.StatementSyntax) CType(m.Accept(Me), MemberDeclarationSyntax)))

                Dim options As VBasic.VisualBasicCompilationOptions = CType(mSemanticModel.Compilation.Options, VBasic.VisualBasicCompilationOptions)

                Return SyntaxFactory.CompilationUnit(SyntaxFactory.List(Of ExternAliasDirectiveSyntax)(), SyntaxFactory.List(options.GlobalImports.Select(Function(gi As VBasic.GlobalImport) gi.Clause).Concat(node.Imports.SelectMany(Function(imp As VBSyntax.ImportsStatementSyntax) imp.ImportsClauses)).Select(Function(c As VBSyntax.ImportsClauseSyntax) CType(c.Accept(Me), UsingDirectiveSyntax))), attributes, members)
            End Function

            Public Overrides Function VisitSimpleImportsClause(ByVal node As VBSyntax.SimpleImportsClauseSyntax) As CSharpSyntaxNode
                If node.Alias IsNot Nothing Then
                    Return SyntaxFactory.UsingDirective(SyntaxFactory.NameEquals(SyntaxFactory.IdentifierName(ConvertIdentifier(node.Alias.Identifier, mSemanticModel))), CType(node.Name.Accept(Me), NameSyntax))
                End If
                Return SyntaxFactory.UsingDirective(CType(node.Name.Accept(Me), NameSyntax))
            End Function

            Public Overrides Function VisitNamespaceBlock(ByVal node As VBSyntax.NamespaceBlockSyntax) As CSharpSyntaxNode
                Dim members As IEnumerable(Of MemberDeclarationSyntax) = node.Members.Select(Function(m As VBSyntax.StatementSyntax) CType(m.Accept(Me), MemberDeclarationSyntax))

                Return SyntaxFactory.NamespaceDeclaration(CType(node.NamespaceStatement.Name.Accept(Me), NameSyntax), SyntaxFactory.List(Of ExternAliasDirectiveSyntax)(), SyntaxFactory.List(Of UsingDirectiveSyntax)(), SyntaxFactory.List(members))
            End Function

#Region "Namespace Members"

            Private Iterator Function ConvertMembers(ByVal members As SyntaxList(Of VBSyntax.StatementSyntax)) As IEnumerable(Of MemberDeclarationSyntax)
                For Each member As MemberDeclarationSyntax In members.Select(Function(m As VBSyntax.StatementSyntax) CType(m.Accept(Me), MemberDeclarationSyntax))
                    Dim declarations() As MemberDeclarationSyntax = Nothing
                    If TypeOf member Is BaseFieldDeclarationSyntax AndAlso additionalDeclarations.TryGetValue(member, declarations) Then
                        For Each d As MemberDeclarationSyntax In declarations
                            Yield d
                        Next d
                        additionalDeclarations.Remove(member)
                    Else
                        Yield member
                    End If
                Next member
            End Function

            Public Overrides Function VisitClassBlock(ByVal node As VBSyntax.ClassBlockSyntax) As CSharpSyntaxNode
                Dim stmt As VBSyntax.ClassStatementSyntax = node.ClassStatement
                Dim attributes As SyntaxList(Of AttributeListSyntax) = SyntaxFactory.List(stmt.AttributeLists.SelectMany(AddressOf ConvertAttribute))
                Dim members As SyntaxList(Of MemberDeclarationSyntax) = SyntaxFactory.List(ConvertMembers(node.Members))

                Dim parameters As TypeParameterListSyntax = Nothing
                Dim constraints As SyntaxList(Of TypeParameterConstraintClauseSyntax) = Nothing
                SplitTypeParameters(stmt.TypeParameterList, parameters, constraints)

                Return SyntaxFactory.ClassDeclaration(attributes, ConvertModifiers(stmt.Modifiers), ConvertIdentifier(stmt.Identifier, mSemanticModel), parameters, ConvertInheritsAndImplements(node.Inherits, node.Implements), constraints, members)
            End Function

            Private Function ConvertInheritsAndImplements(ByVal [inherits] As SyntaxList(Of VBSyntax.InheritsStatementSyntax), ByVal [implements] As SyntaxList(Of VBSyntax.ImplementsStatementSyntax)) As BaseListSyntax
                If [inherits].Count + [implements].Count = 0 Then
                    Return Nothing
                End If
                Dim baseTypes As List(Of BaseTypeSyntax) = New List(Of BaseTypeSyntax)()
                For Each t As VBSyntax.TypeSyntax In [inherits].SelectMany(Function(c As VBSyntax.InheritsStatementSyntax) c.Types).Concat([implements].SelectMany(Function(c As VBSyntax.ImplementsStatementSyntax) c.Types))
                    baseTypes.Add(SyntaxFactory.SimpleBaseType(CType(t.Accept(Me), TypeSyntax)))
                Next t
                Return SyntaxFactory.BaseList(SyntaxFactory.SeparatedList(baseTypes))
            End Function

            Public Overrides Function VisitModuleBlock(ByVal node As VBSyntax.ModuleBlockSyntax) As CSharpSyntaxNode
                Dim stmt As VBSyntax.ModuleStatementSyntax = node.ModuleStatement
                Dim attributes As SyntaxList(Of AttributeListSyntax) = SyntaxFactory.List(stmt.AttributeLists.SelectMany(AddressOf ConvertAttribute))
                Dim members As SyntaxList(Of MemberDeclarationSyntax) = SyntaxFactory.List(ConvertMembers(node.Members))

                Dim parameters As TypeParameterListSyntax = Nothing
                Dim constraints As SyntaxList(Of TypeParameterConstraintClauseSyntax) = Nothing
                SplitTypeParameters(stmt.TypeParameterList, parameters, constraints)

                Return SyntaxFactory.ClassDeclaration(attributes, ConvertModifiers(stmt.Modifiers, TokenContext.InterfaceOrModule).Add(SyntaxFactory.Token(SyntaxKind.StaticKeyword)), ConvertIdentifier(stmt.Identifier, mSemanticModel), parameters, ConvertInheritsAndImplements(node.Inherits, node.Implements), constraints, members)
            End Function
            Public Overrides Function VisitStructureBlock(ByVal node As VBSyntax.StructureBlockSyntax) As CSharpSyntaxNode
                Dim stmt As VBSyntax.StructureStatementSyntax = node.StructureStatement
                Dim attributes As SyntaxList(Of AttributeListSyntax) = SyntaxFactory.List(stmt.AttributeLists.SelectMany(AddressOf ConvertAttribute))
                Dim members As SyntaxList(Of MemberDeclarationSyntax) = SyntaxFactory.List(ConvertMembers(node.Members))

                Dim parameters As TypeParameterListSyntax = Nothing
                Dim constraints As SyntaxList(Of TypeParameterConstraintClauseSyntax) = Nothing
                SplitTypeParameters(stmt.TypeParameterList, parameters, constraints)

                Return SyntaxFactory.StructDeclaration(attributes, ConvertModifiers(stmt.Modifiers, TokenContext.Global), ConvertIdentifier(stmt.Identifier, mSemanticModel), parameters, ConvertInheritsAndImplements(node.Inherits, node.Implements), constraints, members)
            End Function

            Public Overrides Function VisitInterfaceBlock(ByVal node As VBSyntax.InterfaceBlockSyntax) As CSharpSyntaxNode
                Dim stmt As VBSyntax.InterfaceStatementSyntax = node.InterfaceStatement
                Dim attributes As SyntaxList(Of AttributeListSyntax) = SyntaxFactory.List(stmt.AttributeLists.SelectMany(AddressOf ConvertAttribute))
                Dim members As SyntaxList(Of MemberDeclarationSyntax) = SyntaxFactory.List(ConvertMembers(node.Members))

                Dim parameters As TypeParameterListSyntax = Nothing
                Dim constraints As SyntaxList(Of TypeParameterConstraintClauseSyntax) = Nothing
                SplitTypeParameters(stmt.TypeParameterList, parameters, constraints)

                Return SyntaxFactory.InterfaceDeclaration(attributes, ConvertModifiers(stmt.Modifiers, TokenContext.InterfaceOrModule), ConvertIdentifier(stmt.Identifier, mSemanticModel), parameters, ConvertInheritsAndImplements(node.Inherits, node.Implements), constraints, members)
            End Function

            Public Overrides Function VisitEnumBlock(ByVal node As VBSyntax.EnumBlockSyntax) As CSharpSyntaxNode
                Dim stmt As VBSyntax.EnumStatementSyntax = node.EnumStatement
                ' we can cast to SimpleAsClause because other types make no sense as enum-type.
                Dim asClause As VBSyntax.SimpleAsClauseSyntax = CType(stmt.UnderlyingType, VBSyntax.SimpleAsClauseSyntax)
                Dim attributes As IEnumerable(Of AttributeListSyntax) = stmt.AttributeLists.SelectMany(AddressOf ConvertAttribute)
                Dim baseList As BaseListSyntax = Nothing
                If asClause IsNot Nothing Then
                    baseList = SyntaxFactory.BaseList(SyntaxFactory.SingletonSeparatedList(Of BaseTypeSyntax)(SyntaxFactory.SimpleBaseType(CType(asClause.Type.Accept(Me), TypeSyntax))))
                    If asClause.AttributeLists.Count > 0 Then
                        attributes = attributes.Concat(SyntaxFactory.AttributeList(SyntaxFactory.AttributeTargetSpecifier(SyntaxFactory.Token(SyntaxKind.ReturnKeyword)), SyntaxFactory.SeparatedList(asClause.AttributeLists.SelectMany(Function(l As VBSyntax.AttributeListSyntax) ConvertAttribute(l).SelectMany(Function(a As AttributeListSyntax) a.Attributes)))))
                    End If
                End If
                Dim members As SeparatedSyntaxList(Of EnumMemberDeclarationSyntax) = SyntaxFactory.SeparatedList(node.Members.Select(Function(m As VBSyntax.StatementSyntax) CType(m.Accept(Me), EnumMemberDeclarationSyntax)))
                Return SyntaxFactory.EnumDeclaration(SyntaxFactory.List(attributes), ConvertModifiers(stmt.Modifiers, TokenContext.Global), ConvertIdentifier(stmt.Identifier, mSemanticModel), baseList, members)
            End Function

            Public Overrides Function VisitEnumMemberDeclaration(ByVal node As VBSyntax.EnumMemberDeclarationSyntax) As CSharpSyntaxNode
                Dim attributes As SyntaxList(Of AttributeListSyntax) = SyntaxFactory.List(node.AttributeLists.SelectMany(AddressOf ConvertAttribute))
                Return SyntaxFactory.EnumMemberDeclaration(attributes, ConvertIdentifier(node.Identifier, mSemanticModel), CType(node.Initializer?.Accept(Me), EqualsValueClauseSyntax))
            End Function

            Public Overrides Function VisitDelegateStatement(ByVal node As VBSyntax.DelegateStatementSyntax) As CSharpSyntaxNode
                Dim attributes As IEnumerable(Of AttributeListSyntax) = node.AttributeLists.SelectMany(AddressOf ConvertAttribute)

                Dim typeParameters As TypeParameterListSyntax = Nothing
                Dim constraints As SyntaxList(Of TypeParameterConstraintClauseSyntax) = Nothing
                SplitTypeParameters(node.TypeParameterList, typeParameters, constraints)

                Dim returnType As TypeSyntax
                Dim asClause As VBSyntax.SimpleAsClauseSyntax = node.AsClause
                If asClause Is Nothing Then
                    returnType = SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.VoidKeyword))
                Else
                    returnType = CType(asClause.Type.Accept(Me), TypeSyntax)
                    If asClause.AttributeLists.Count > 0 Then
                        attributes = attributes.Concat(SyntaxFactory.AttributeList(SyntaxFactory.AttributeTargetSpecifier(SyntaxFactory.Token(SyntaxKind.ReturnKeyword)), SyntaxFactory.SeparatedList(asClause.AttributeLists.SelectMany(Function(l As VBSyntax.AttributeListSyntax) ConvertAttribute(l).SelectMany(Function(a As AttributeListSyntax) a.Attributes)))))
                    End If
                End If

                Return SyntaxFactory.DelegateDeclaration(SyntaxFactory.List(attributes), ConvertModifiers(node.Modifiers, TokenContext.Global), returnType, ConvertIdentifier(node.Identifier, mSemanticModel), typeParameters, CType(node.ParameterList?.Accept(Me), ParameterListSyntax), constraints)
            End Function

#End Region

#Region "Type Members"

            Public Overrides Function VisitFieldDeclaration(ByVal node As VBSyntax.FieldDeclarationSyntax) As CSharpSyntaxNode
                Dim attributes As IEnumerable(Of AttributeListSyntax) = node.AttributeLists.SelectMany(AddressOf ConvertAttribute)
                Dim unConvertableModifiers As List(Of String) = node.Modifiers.Where(Function(m As SyntaxToken) SyntaxTokenExtensions.IsKind(m, VBasic.SyntaxKind.WithEventsKeyword)).Select(Function(m As SyntaxToken) m.Text).ToList()
                Dim convertableModifiers As IEnumerable(Of SyntaxToken) = node.Modifiers.Where(Function(m As SyntaxToken) Not SyntaxTokenExtensions.IsKind(m, VBasic.SyntaxKind.WithEventsKeyword))
                Dim convertedModifiers As SyntaxTokenList = ConvertModifiers(convertableModifiers, TokenContext.VariableOrConst)
                Dim key As FieldDeclarationSyntax = SyntaxFactory.FieldDeclaration(SyntaxFactory.VariableDeclaration(SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.VoidKeyword))))
                Dim declarations As List(Of BaseFieldDeclarationSyntax) = New List(Of BaseFieldDeclarationSyntax)(node.Declarators.Count)

                For Each declarator As VBSyntax.VariableDeclaratorSyntax In node.Declarators
                    For Each decl As VariableDeclarationSyntax In SplitVariableDeclarations(declarator, Me, mSemanticModel).Values
                        Dim baseFieldDeclarationSyntax As FieldDeclarationSyntax = SyntaxFactory.FieldDeclaration(SyntaxFactory.List(attributes), convertedModifiers, decl)
                        declarations.Add(If(unConvertableModifiers.Any(), baseFieldDeclarationSyntax.WithAppendedTrailingTrivia(SyntaxFactory.Comment($"/* TODO ERROR didn't convert: {String.Join(",", unConvertableModifiers)} */")), baseFieldDeclarationSyntax))
                    Next decl
                Next declarator

                additionalDeclarations.Add(key, declarations.ToArray())
                Return key
            End Function
            Private Function [Throw](underscore As VBSyntax.AsClauseSyntax) As TypeSyntax
                Throw New NotImplementedException($"{underscore.GetType().FullName} not implemented!")
            End Function
            Public Overrides Function VisitPropertyStatement(ByVal node As VBSyntax.PropertyStatementSyntax) As CSharpSyntaxNode
                Dim hasBody As Boolean = TypeOf node.Parent Is VBSyntax.PropertyBlockSyntax
                Dim attributes As IEnumerable(Of AttributeListSyntax) = node.AttributeLists.SelectMany(AddressOf ConvertAttribute)
                Dim isReadonly As Boolean = node.Modifiers.Any(Function(m As SyntaxToken) SyntaxTokenExtensions.IsKind(m, VBasic.SyntaxKind.ReadOnlyKeyword))
                Dim convertibleModifiers As IEnumerable(Of SyntaxToken) = node.Modifiers.Where(Function(m As SyntaxToken) Not SyntaxTokenExtensions.IsKind(m, VBasic.SyntaxKind.ReadOnlyKeyword))
                Dim modifiers As SyntaxTokenList = ConvertModifiers(convertibleModifiers, GetMethodOrPropertyContext(node))
                Dim isIndexer As Boolean = node.Modifiers.Any(Function(m As SyntaxToken) SyntaxTokenExtensions.IsKind(m, VBasic.SyntaxKind.DefaultKeyword)) AndAlso node.Identifier.ValueText.Equals("Items", StringComparison.OrdinalIgnoreCase)

                Dim initializer As EqualsValueClauseSyntax = CType(node.Initializer?.Accept(Me), EqualsValueClauseSyntax)
                Dim tmp As VBSyntax.TypeSyntax = CType(node.AsClause?.TypeSwitch(
                    Function(c As VBSyntax.SimpleAsClauseSyntax) c.Type,
                    Function(c As VBSyntax.AsNewClauseSyntax)
                        initializer = SyntaxFactory.EqualsValueClause(CType(c.NewExpression.Accept(Me), ExpressionSyntax))
                        Return VBasic.SyntaxExtensions.Type(c.NewExpression)
                    End Function,
                    Function(underscore As VBSyntax.AsClauseSyntax) As VBSyntax.TypeSyntax
                        Throw New NotImplementedException($"{underscore.GetType().FullName} not implemented!")
                    End Function), VBSyntax.TypeSyntax)

                Dim rawType As TypeSyntax = CType(If(tmp?.Accept(Me), SyntaxFactory.ParseTypeName("var")), TypeSyntax)


                Dim accessors As AccessorListSyntax = Nothing
                If Not hasBody Then
                    Dim accessorList As List(Of AccessorDeclarationSyntax) = New List(Of AccessorDeclarationSyntax) From {SyntaxFactory.AccessorDeclaration(SyntaxKind.GetAccessorDeclaration).WithSemicolonToken(SyntaxFactory.Token(SyntaxKind.SemicolonToken))}
                    If Not isReadonly Then
                        accessorList.Add(SyntaxFactory.AccessorDeclaration(SyntaxKind.SetAccessorDeclaration).WithSemicolonToken(SyntaxFactory.Token(SyntaxKind.SemicolonToken)))
                    End If
                    accessors = SyntaxFactory.AccessorList(SyntaxFactory.List(accessorList))
                Else
                    accessors = SyntaxFactory.AccessorList(SyntaxFactory.List(CType(node.Parent, VBSyntax.PropertyBlockSyntax).Accessors.Select(Function(a As VBSyntax.AccessorBlockSyntax) CType(a.Accept(Me), AccessorDeclarationSyntax))))
                End If

                If isIndexer Then
                    Return SyntaxFactory.IndexerDeclaration(SyntaxFactory.List(attributes), modifiers, rawType, Nothing, SyntaxFactory.BracketedParameterList(SyntaxFactory.SeparatedList(node.ParameterList.Parameters.Select(Function(p As VBSyntax.ParameterSyntax) CType(p.Accept(Me), ParameterSyntax)))), accessors)
                Else
                    Return SyntaxFactory.PropertyDeclaration(SyntaxFactory.List(attributes), modifiers, rawType, Nothing, ConvertIdentifier(node.Identifier, mSemanticModel), accessors, Nothing, initializer, SyntaxFactory.Token(If(initializer Is Nothing, SyntaxKind.None, SyntaxKind.SemicolonToken)))
                End If
            End Function
            Public Overrides Function VisitPropertyBlock(ByVal node As VBSyntax.PropertyBlockSyntax) As CSharpSyntaxNode
                Return node.PropertyStatement.Accept(Me)
            End Function

            Public Overrides Function VisitAccessorBlock(ByVal node As VBSyntax.AccessorBlockSyntax) As CSharpSyntaxNode
                Dim blockKind As SyntaxKind
                Dim isIterator As Boolean = node.GetModifiers().Any(Function(m As SyntaxToken) SyntaxTokenExtensions.IsKind(m, VBasic.SyntaxKind.IteratorKeyword))
                Dim body As BlockSyntax = SyntaxFactory.Block(node.Statements.SelectMany(Function(s As VBSyntax.StatementSyntax) s.Accept(New MethodBodyVisitor(mSemanticModel, Me, withBlockTempVariableNames) With {.IsIterator = isIterator})))
                Dim attributes As SyntaxList(Of AttributeListSyntax) = SyntaxFactory.List(node.AccessorStatement.AttributeLists.Select(Function(a As VBSyntax.AttributeListSyntax) CType(a.Accept(Me), AttributeListSyntax)))
                Dim modifiers As SyntaxTokenList = ConvertModifiers(node.AccessorStatement.Modifiers, TokenContext.Local)

                Select Case node.Kind()
                    Case VBasic.SyntaxKind.GetAccessorBlock
                        blockKind = SyntaxKind.GetAccessorDeclaration
                    Case VBasic.SyntaxKind.SetAccessorBlock
                        blockKind = SyntaxKind.SetAccessorDeclaration
                    Case VBasic.SyntaxKind.AddHandlerAccessorBlock
                        blockKind = SyntaxKind.AddAccessorDeclaration
                    Case VBasic.SyntaxKind.RemoveHandlerAccessorBlock
                        blockKind = SyntaxKind.RemoveAccessorDeclaration
                    Case Else
                        Throw New NotSupportedException()
                End Select
                Return SyntaxFactory.AccessorDeclaration(blockKind, attributes, modifiers, body)
            End Function

            Public Overrides Function VisitMethodBlock(ByVal node As VBSyntax.MethodBlockSyntax) As CSharpSyntaxNode
                Dim block As BaseMethodDeclarationSyntax = CType(node.SubOrFunctionStatement.Accept(Me), BaseMethodDeclarationSyntax)
                Dim isIterator As Boolean = node.SubOrFunctionStatement.Modifiers.Any(Function(m As SyntaxToken) SyntaxTokenExtensions.IsKind(m, VBasic.SyntaxKind.IteratorKeyword))

                Return block.WithBody(SyntaxFactory.Block(node.Statements.SelectMany(Function(s As VBSyntax.StatementSyntax) s.Accept(New MethodBodyVisitor(mSemanticModel, Me, withBlockTempVariableNames) With {.isIterator = isIterator}))))
            End Function

            Public Overrides Function VisitMethodStatement(ByVal node As VBSyntax.MethodStatementSyntax) As CSharpSyntaxNode
                Dim attributes As IEnumerable(Of AttributeListSyntax) = node.AttributeLists.SelectMany(AddressOf ConvertAttribute)
                Dim hasBody As Boolean = TypeOf node.Parent Is VBSyntax.MethodBlockBaseSyntax

                If NameOf(Finalize).Equals(node.Identifier.ValueText, StringComparison.OrdinalIgnoreCase) AndAlso node.Modifiers.Any(Function(m As SyntaxToken) VBasic.VisualBasicExtensions.Kind(m) = VBasic.SyntaxKind.OverridesKeyword) Then
                    Dim decl As DestructorDeclarationSyntax = SyntaxFactory.DestructorDeclaration(ConvertIdentifier(node.GetAncestor(Of VBSyntax.TypeBlockSyntax)().BlockStatement.Identifier, mSemanticModel)).WithAttributeLists(SyntaxFactory.List(attributes))
                    If hasBody Then
                        Return decl
                    End If
                    Return decl.WithSemicolonToken(SyntaxFactory.Token(SyntaxKind.SemicolonToken))
                Else
                    Dim tokenContext As TokenContext = GetMethodOrPropertyContext(node)
                    Dim modifiers As SyntaxTokenList = ConvertModifiers(node.Modifiers, tokenContext)

                    Dim typeParameters As TypeParameterListSyntax = Nothing
                    Dim constraints As SyntaxList(Of TypeParameterConstraintClauseSyntax) = Nothing
                    SplitTypeParameters(node.TypeParameterList, typeParameters, constraints)

                    Dim decl As MethodDeclarationSyntax = SyntaxFactory.MethodDeclaration(
                        SyntaxFactory.List(attributes),
                        modifiers,
                        CType(If(node.AsClause?.Type.Accept(Me), SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.VoidKeyword))), TypeSyntax),
                        Nothing,
                        ConvertIdentifier(node.Identifier, Me.mSemanticModel),
                        typeParameters,
                        CType(node.ParameterList.Accept(Me), ParameterListSyntax),
                        constraints,
                        Nothing,
                        CType(Nothing, ArrowExpressionClauseSyntax)
                        )
                    If hasBody Then
                        Return decl
                    End If
                    Return decl.WithSemicolonToken(SyntaxFactory.Token(SyntaxKind.SemicolonToken))
                End If
            End Function

            Private Function GetMethodOrPropertyContext(ByVal node As VBSyntax.StatementSyntax) As TokenContext
                Dim parentType As INamedTypeSymbol = mSemanticModel.GetDeclaredSymbol(node).ContainingType
                Select Case parentType.TypeKind
                    Case TypeKind.Module
                        Return TokenContext.MemberInModule
                    Case TypeKind.Class
                        Return TokenContext.MemberInClass
                    Case TypeKind.Interface
                        Return TokenContext.MemberInInterface
                    Case TypeKind.Struct
                        Return TokenContext.MemberInStruct
                    Case Else
                        Throw New ArgumentOutOfRangeException(NameOf(node))
                End Select
            End Function

            Public Overrides Function VisitEventBlock(ByVal node As VBSyntax.EventBlockSyntax) As CSharpSyntaxNode
                Dim block As VBSyntax.EventStatementSyntax = node.EventStatement
                Dim attributes As IEnumerable(Of AttributeListSyntax) = block.AttributeLists.SelectMany(AddressOf ConvertAttribute)
                Dim modifiers As SyntaxTokenList = ConvertModifiers(block.Modifiers, TokenContext.Member)

                Dim rawType As TypeSyntax = If(CType(block.AsClause?.Type.Accept(Me), TypeSyntax), SyntaxFactory.ParseTypeName("var"))

                Return SyntaxFactory.EventDeclaration(SyntaxFactory.List(attributes), modifiers, rawType, Nothing, ConvertIdentifier(block.Identifier, mSemanticModel), SyntaxFactory.AccessorList(SyntaxFactory.List(node.Accessors.Select(Function(a As VBSyntax.AccessorBlockSyntax) CType(a.Accept(Me), AccessorDeclarationSyntax)))))
            End Function

            Public Overrides Function VisitEventStatement(ByVal node As VBSyntax.EventStatementSyntax) As CSharpSyntaxNode
                Dim attributes As IEnumerable(Of AttributeListSyntax) = node.AttributeLists.SelectMany(AddressOf ConvertAttribute)
                Dim modifiers As SyntaxTokenList = ConvertModifiers(node.Modifiers, TokenContext.Member)
                Dim id As SyntaxToken = ConvertIdentifier(node.Identifier, mSemanticModel)

                If node.AsClause Is Nothing Then
                    Dim key As EventFieldDeclarationSyntax = SyntaxFactory.EventFieldDeclaration(SyntaxFactory.VariableDeclaration(SyntaxFactory.ParseTypeName("__event" & id.ValueText)))
                    Dim delegateName As SyntaxToken = SyntaxFactory.Identifier(id.ValueText & NameOf(EventHandler))

                    Dim delegateDecl As DelegateDeclarationSyntax = SyntaxFactory.DelegateDeclaration(SyntaxFactory.List(Of AttributeListSyntax)(), modifiers, SyntaxFactory.ParseTypeName("void"), delegateName, Nothing, CType(node.ParameterList.Accept(Me), ParameterListSyntax), SyntaxFactory.List(Of TypeParameterConstraintClauseSyntax)())

                    Dim eventDecl As EventFieldDeclarationSyntax = SyntaxFactory.EventFieldDeclaration(SyntaxFactory.List(attributes), modifiers, SyntaxFactory.VariableDeclaration(SyntaxFactory.IdentifierName(delegateName), SyntaxFactory.SingletonSeparatedList(SyntaxFactory.VariableDeclarator(id))))

                    additionalDeclarations.Add(key, New MemberDeclarationSyntax() {eventDecl, delegateDecl})
                    Return key
                Else
                    Return SyntaxFactory.EventFieldDeclaration(SyntaxFactory.List(attributes), modifiers, SyntaxFactory.VariableDeclaration(CType(node.AsClause.Type.Accept(Me), TypeSyntax), SyntaxFactory.SingletonSeparatedList(SyntaxFactory.VariableDeclarator(id))))
                End If
                Throw New NotSupportedException()
            End Function

            Public Overrides Function VisitOperatorBlock(ByVal node As VBSyntax.OperatorBlockSyntax) As CSharpSyntaxNode
                Dim block As VBSyntax.OperatorStatementSyntax = node.OperatorStatement
                Dim attributes As IEnumerable(Of AttributeListSyntax) = block.AttributeLists.SelectMany(AddressOf ConvertAttribute)
                Dim modifiers As SyntaxTokenList = ConvertModifiers(block.Modifiers, TokenContext.Member)
                Return SyntaxFactory.OperatorDeclaration(SyntaxFactory.List(attributes), modifiers, If(CType(block.AsClause?.Type.Accept(Me), TypeSyntax), SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.VoidKeyword))), ConvertToken(block.OperatorToken), CType(block.ParameterList.Accept(Me), ParameterListSyntax), SyntaxFactory.Block(node.Statements.SelectMany(Function(s As VBSyntax.StatementSyntax) s.Accept(New MethodBodyVisitor(mSemanticModel, Me, withBlockTempVariableNames)))), Nothing)
            End Function
            Public Overrides Function VisitConstructorBlock(ByVal node As VBSyntax.ConstructorBlockSyntax) As CSharpSyntaxNode
                Dim block As VBSyntax.MethodBaseSyntax = node.BlockStatement
                Dim attributes As IEnumerable(Of AttributeListSyntax) = block.AttributeLists.SelectMany(AddressOf ConvertAttribute)
                Dim modifiers As SyntaxTokenList = ConvertModifiers(block.Modifiers, TokenContext.Member)


                Dim ctor As VBSyntax.InvocationExpressionSyntax = TryCast(TryCast(node.Statements.FirstOrDefault(), VBSyntax.ExpressionStatementSyntax)?.Expression, VBSyntax.InvocationExpressionSyntax)
                Dim ctorExpression As VBSyntax.MemberAccessExpressionSyntax = TryCast(ctor?.Expression, VBSyntax.MemberAccessExpressionSyntax)
                Dim ctorArgs As ArgumentListSyntax = CType(ctor?.ArgumentList.Accept(Me), ArgumentListSyntax)

                Dim statements As IEnumerable(Of VBSyntax.StatementSyntax)
                Dim ctorCall As ConstructorInitializerSyntax
                If ctorExpression Is Nothing OrElse Not ctorExpression.Name.Identifier.IsKindOrHasMatchingText(VBasic.SyntaxKind.NewKeyword) Then
                    statements = node.Statements
                    ctorCall = Nothing
                ElseIf TypeOf ctorExpression.Expression Is VBSyntax.MyBaseExpressionSyntax Then
                    statements = node.Statements.Skip(1)
                    ctorCall = SyntaxFactory.ConstructorInitializer(SyntaxKind.BaseConstructorInitializer, If(ctorArgs, SyntaxFactory.ArgumentList()))
                ElseIf TypeOf ctorExpression.Expression Is VBSyntax.MeExpressionSyntax OrElse TypeOf ctorExpression.Expression Is VBSyntax.MyClassExpressionSyntax Then
                    statements = node.Statements.Skip(1)
                    ctorCall = SyntaxFactory.ConstructorInitializer(SyntaxKind.ThisConstructorInitializer, If(ctorArgs, SyntaxFactory.ArgumentList()))
                Else
                    statements = node.Statements
                    ctorCall = Nothing
                End If

                Return SyntaxFactory.ConstructorDeclaration(SyntaxFactory.List(attributes), modifiers, ConvertIdentifier(node.GetAncestor(Of VBSyntax.TypeBlockSyntax)().BlockStatement.Identifier, mSemanticModel), CType(block.ParameterList.Accept(Me), ParameterListSyntax), ctorCall, SyntaxFactory.Block(statements.SelectMany(Function(s As VBSyntax.StatementSyntax) s.Accept(New MethodBodyVisitor(mSemanticModel, Me, withBlockTempVariableNames)))))
            End Function

            Public Overrides Function VisitTypeParameterList(ByVal node As VBSyntax.TypeParameterListSyntax) As CSharpSyntaxNode
                Return SyntaxFactory.TypeParameterList(SyntaxFactory.SeparatedList(node.Parameters.Select(Function(p As VBSyntax.TypeParameterSyntax) CType(p.Accept(Me), TypeParameterSyntax))))
            End Function

            Public Overrides Function VisitParameterList(ByVal node As VBSyntax.ParameterListSyntax) As CSharpSyntaxNode
                If TypeOf node.Parent Is VBSyntax.PropertyStatementSyntax Then
                    Return SyntaxFactory.BracketedParameterList(SyntaxFactory.SeparatedList(node.Parameters.Select(Function(p As VBSyntax.ParameterSyntax) CType(p.Accept(Me), ParameterSyntax))))
                End If
                Return SyntaxFactory.ParameterList(SyntaxFactory.SeparatedList(node.Parameters.Select(Function(p As VBSyntax.ParameterSyntax) CType(p.Accept(Me), ParameterSyntax))))
            End Function

            Public Overrides Function VisitParameter(ByVal node As VBSyntax.ParameterSyntax) As CSharpSyntaxNode
                Dim id As SyntaxToken = ConvertIdentifier(node.Identifier.Identifier, mSemanticModel)
                Dim returnType As TypeSyntax = CType(node.AsClause?.Type.Accept(Me), TypeSyntax)
                If returnType IsNot Nothing AndAlso Not SyntaxTokenExtensions.IsKind(node.Identifier.Nullable, SyntaxKind.None) Then
                    Dim arrayType As ArrayTypeSyntax = TryCast(returnType, ArrayTypeSyntax)
                    If arrayType Is Nothing Then
                        returnType = SyntaxFactory.NullableType(returnType)
                    Else
                        returnType = arrayType.WithElementType(SyntaxFactory.NullableType(arrayType.ElementType))
                    End If
                End If
                Dim [default] As EqualsValueClauseSyntax = Nothing
                If node.Default IsNot Nothing Then
                    [default] = SyntaxFactory.EqualsValueClause(CType(node.Default?.Value.Accept(Me), ExpressionSyntax))
                End If
                Dim attributes As List(Of AttributeListSyntax) = node.AttributeLists.SelectMany(AddressOf ConvertAttribute).ToList()
                Dim outAttributeIndex As Integer = attributes.FindIndex(Function(a As AttributeListSyntax) a.Attributes.Single().Name.ToString() = "Out")
                Dim modifiers As SyntaxTokenList = ConvertModifiers(node.Modifiers, TokenContext.Local)
                If outAttributeIndex > -1 Then
                    attributes.RemoveAt(outAttributeIndex)
                    modifiers = modifiers.Replace(SyntaxFactory.Token(SyntaxKind.RefKeyword), SyntaxFactory.Token(SyntaxKind.OutKeyword))
                End If
                Return SyntaxFactory.Parameter(SyntaxFactory.List(attributes), modifiers, returnType, id, [default])
            End Function

#End Region

#Region "Expressions"

            Public Overrides Function VisitAwaitExpression(ByVal node As VBSyntax.AwaitExpressionSyntax) As CSharpSyntaxNode
                Return SyntaxFactory.AwaitExpression(CType(node.Expression.Accept(Me), ExpressionSyntax))
            End Function

            Public Overrides Function VisitCatchBlock(ByVal node As VBSyntax.CatchBlockSyntax) As CSharpSyntaxNode
                Dim stmt As VBSyntax.CatchStatementSyntax = node.CatchStatement
                Dim catcher As CatchDeclarationSyntax
                If stmt.IdentifierName Is Nothing Then
                    catcher = Nothing
                Else
                    Dim typeInfo As ITypeSymbol = ModelExtensions.GetTypeInfo(mSemanticModel, stmt.IdentifierName).Type
                    catcher = SyntaxFactory.CatchDeclaration(SyntaxFactory.ParseTypeName(typeInfo.ToMinimalDisplayString(mSemanticModel, node.SpanStart)), ConvertIdentifier(stmt.IdentifierName.Identifier, mSemanticModel))
                End If

                Dim filter As CatchFilterClauseSyntax = CType(stmt.WhenClause?.Accept(Me), CatchFilterClauseSyntax)

                Return SyntaxFactory.CatchClause(catcher, filter, SyntaxFactory.Block(node.Statements.SelectMany(Function(s As VBSyntax.StatementSyntax) s.Accept(New MethodBodyVisitor(mSemanticModel, Me, withBlockTempVariableNames)))))
            End Function

            Public Overrides Function VisitCatchFilterClause(ByVal node As VBSyntax.CatchFilterClauseSyntax) As CSharpSyntaxNode
                Return SyntaxFactory.CatchFilterClause(CType(node.Filter.Accept(Me), ExpressionSyntax))
            End Function

            Public Overrides Function VisitFinallyBlock(ByVal node As VBSyntax.FinallyBlockSyntax) As CSharpSyntaxNode
                Return SyntaxFactory.FinallyClause(SyntaxFactory.Block(node.Statements.SelectMany(Function(s As VBSyntax.StatementSyntax) s.Accept(New MethodBodyVisitor(mSemanticModel, Me, withBlockTempVariableNames)))))
            End Function

            Public Overrides Function VisitCTypeExpression(ByVal node As VBSyntax.CTypeExpressionSyntax) As CSharpSyntaxNode
                Dim expressionSyntax As ExpressionSyntax = CType(node.Expression.Accept(Me), ExpressionSyntax)
                Dim convertMethodForKeywordOrNull As ExpressionSyntax = GetConvertMethodForKeywordOrNull(node.Type)

                Return If(convertMethodForKeywordOrNull IsNot Nothing,
                    SyntaxFactory.InvocationExpression(convertMethodForKeywordOrNull,
                                                        SyntaxFactory.ArgumentList(
                                                            SyntaxFactory.SingletonSeparatedList(
                                                                SyntaxFactory.Argument(expressionSyntax)))), ' Hopefully will be a compile error if it's wrong
                                                        CType(SyntaxFactory.CastExpression(CType(node.Type.Accept(Me), TypeSyntax), expressionSyntax), ExpressionSyntax))
            End Function

            Public Overrides Function VisitPredefinedCastExpression(ByVal node As VBSyntax.PredefinedCastExpressionSyntax) As CSharpSyntaxNode
                Dim expressionSyntax As ExpressionSyntax = CType(node.Expression.Accept(Me), ExpressionSyntax)
                If SyntaxTokenExtensions.IsKind(node.Keyword, VBasic.SyntaxKind.CDateKeyword) Then
                    Return SyntaxFactory.CastExpression(SyntaxFactory.ParseTypeName("DateTime"), expressionSyntax)
                End If

                Dim convertMethodForKeywordOrNull As ExpressionSyntax = GetConvertMethodForKeywordOrNull(node)

                Return If(convertMethodForKeywordOrNull IsNot Nothing,
                            CType(
                                SyntaxFactory.InvocationExpression(convertMethodForKeywordOrNull,
                                    SyntaxFactory.ArgumentList(
                                    SyntaxFactory.SingletonSeparatedList(
                                        SyntaxFactory.Argument(expressionSyntax)))
                                        ), ExpressionSyntax),' Hopefully will be a compile error if it's wrong
                            SyntaxFactory.CastExpression(SyntaxFactory.PredefinedType(ConvertToken(node.Keyword)), CType(node.Expression.Accept(Me), ExpressionSyntax))) ' Hopefully will be a compile error if it's wrong
            End Function

            Private Function GetConvertMethodForKeywordOrNull(ByVal type As SyntaxNode) As ExpressionSyntax
                Dim convertedType As ITypeSymbol = mSemanticModel.GetTypeInfo(type).ConvertedType
                Dim convertMethodName As String = Nothing
                Return If(createConvertMethodsLookupByReturnType_Renamed.TryGetValue(convertedType, convertMethodName), SyntaxFactory.ParseExpression(convertMethodName), Nothing)
            End Function

            Public Overrides Function VisitTryCastExpression(ByVal node As VBSyntax.TryCastExpressionSyntax) As CSharpSyntaxNode
                Return SyntaxFactory.BinaryExpression(SyntaxKind.AsExpression, CType(node.Expression.Accept(Me), ExpressionSyntax), CType(node.Type.Accept(Me), TypeSyntax))
            End Function

            Public Overrides Function VisitLiteralExpression(ByVal node As VBSyntax.LiteralExpressionSyntax) As CSharpSyntaxNode
                If node.Token.Value Is Nothing Then
                    Dim type As ITypeSymbol = ModelExtensions.GetTypeInfo(mSemanticModel, node).ConvertedType
                    If type Is Nothing Then
                        Return Literal(Nothing).WithTrailingTrivia(SyntaxFactory.Comment("/* TODO Change to default(_) if this is not a reference type */"))
                    End If
                    Return If(Not type.IsReferenceType, SyntaxFactory.DefaultExpression(SyntaxFactory.ParseTypeName(type.ToMinimalDisplayString(mSemanticModel, node.SpanStart))), Literal(Nothing))
                End If
                Return Literal(node.Token.Value)
            End Function

            Public Overrides Function VisitInterpolatedStringExpression(ByVal node As VBSyntax.InterpolatedStringExpressionSyntax) As CSharpSyntaxNode
                Return SyntaxFactory.InterpolatedStringExpression(SyntaxFactory.Token(SyntaxKind.InterpolatedStringStartToken), SyntaxFactory.List(node.Contents.Select(Function(c As VBSyntax.InterpolatedStringContentSyntax) CType(c.Accept(Me), InterpolatedStringContentSyntax))))
            End Function
            Public Overrides Function VisitInterpolatedStringText(ByVal node As VBSyntax.InterpolatedStringTextSyntax) As CSharpSyntaxNode
                Return SyntaxFactory.InterpolatedStringText(SyntaxFactory.Token(Nothing, SyntaxKind.InterpolatedStringTextToken, node.TextToken.Text, node.TextToken.ValueText, Nothing))
            End Function

            Public Overrides Function VisitInterpolation(ByVal node As VBSyntax.InterpolationSyntax) As CSharpSyntaxNode
                Return SyntaxFactory.Interpolation(CType(node.Expression.Accept(Me), ExpressionSyntax))
            End Function

            Public Overrides Function VisitInterpolationFormatClause(ByVal node As VBSyntax.InterpolationFormatClauseSyntax) As CSharpSyntaxNode
                Return MyBase.VisitInterpolationFormatClause(node)
            End Function

            Public Overrides Function VisitMeExpression(ByVal node As VBSyntax.MeExpressionSyntax) As CSharpSyntaxNode
                Return SyntaxFactory.ThisExpression()
            End Function

            Public Overrides Function VisitMyBaseExpression(ByVal node As VBSyntax.MyBaseExpressionSyntax) As CSharpSyntaxNode
                Return SyntaxFactory.BaseExpression()
            End Function

            Public Overrides Function VisitParenthesizedExpression(ByVal node As VBSyntax.ParenthesizedExpressionSyntax) As CSharpSyntaxNode
                Return SyntaxFactory.ParenthesizedExpression(CType(node.Expression.Accept(Me), ExpressionSyntax))
            End Function

            Public Overrides Function VisitMemberAccessExpression(ByVal node As VBSyntax.MemberAccessExpressionSyntax) As CSharpSyntaxNode
                Dim simpleNameSyntax As SimpleNameSyntax = CType(node.Name.Accept(Me), SimpleNameSyntax)

                Dim left As ExpressionSyntax = CType(node.Expression?.Accept(Me), ExpressionSyntax)
                If left Is Nothing Then
                    If Not node.Parent.Parent.IsKind(VBasic.SyntaxKind.WithBlock) Then
                        Return SyntaxFactory.MemberBindingExpression(simpleNameSyntax)
                    End If

                    left = SyntaxFactory.IdentifierName(withBlockTempVariableNames.Peek())
                End If

                If node.Expression.IsKind(VBasic.SyntaxKind.GlobalName) Then
                    Return SyntaxFactory.AliasQualifiedName(CType(left, IdentifierNameSyntax), simpleNameSyntax)
                Else
                    Dim memberAccessExpressionSyntax As MemberAccessExpressionSyntax = SyntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, left, simpleNameSyntax)
                    'INSTANT VB TODO TASK: VB has no equivalent to C# pattern variables in 'is' expressions:
                    'ORIGINAL LINE: if (ModelExtensions.GetSymbolInfo(semanticModel, node).Symbol is IMethodSymbol methodSymbol && methodSymbol.ReturnType.Equals(ModelExtensions.GetTypeInfo(semanticModel, node).ConvertedType))
                    If TypeOf ModelExtensions.GetSymbolInfo(mSemanticModel, node).Symbol Is IMethodSymbol Then
                        Dim methodSymbol As IMethodSymbol = CType(ModelExtensions.GetSymbolInfo(mSemanticModel, node).Symbol, IMethodSymbol)
                        If methodSymbol.ReturnType.Equals(ModelExtensions.GetTypeInfo(mSemanticModel, node).ConvertedType) Then
                            Return SyntaxFactory.InvocationExpression(memberAccessExpressionSyntax, SyntaxFactory.ArgumentList())
                        End If
                    End If
                    Return memberAccessExpressionSyntax
                End If
            End Function

            Public Overrides Function VisitConditionalAccessExpression(ByVal node As VBSyntax.ConditionalAccessExpressionSyntax) As CSharpSyntaxNode
                Return SyntaxFactory.ConditionalAccessExpression(CType(node.Expression.Accept(Me), ExpressionSyntax), CType(node.WhenNotNull.Accept(Me), ExpressionSyntax))
            End Function

            Public Overrides Function VisitArgumentList(ByVal node As VBSyntax.ArgumentListSyntax) As CSharpSyntaxNode
                If node.Parent.IsKind(VBasic.SyntaxKind.Attribute) Then
                    Return SyntaxFactory.AttributeArgumentList(SyntaxFactory.SeparatedList(node.Arguments.Select(AddressOf ToAttributeArgument)))
                End If
                Return SyntaxFactory.ArgumentList(SyntaxFactory.SeparatedList(node.Arguments.Select(Function(a As VBSyntax.ArgumentSyntax) CType(a.Accept(Me), ArgumentSyntax))))
            End Function

            Public Overrides Function VisitSimpleArgument(ByVal node As VBSyntax.SimpleArgumentSyntax) As CSharpSyntaxNode
                Dim argID As Integer = CType(node.Parent, VBSyntax.ArgumentListSyntax).Arguments.IndexOf(node)
                Dim invocation As SyntaxNode = node.Parent.Parent
                If TypeOf invocation Is VBSyntax.ArrayCreationExpressionSyntax Then
                    Return node.Expression.Accept(Me)
                End If
                Dim symbol As ISymbol = invocation.TypeSwitch(
                    Function(e As VBSyntax.InvocationExpressionSyntax) ExtractMatch(ModelExtensions.GetSymbolInfo(mSemanticModel, e)),
                    Function(e As VBSyntax.ObjectCreationExpressionSyntax) ExtractMatch(ModelExtensions.GetSymbolInfo(mSemanticModel, e)),
                    Function(e As VBSyntax.RaiseEventStatementSyntax) ExtractMatch(ModelExtensions.GetSymbolInfo(mSemanticModel, e.Name)),
                    Function(underscore As SyntaxNode) As ISymbol
                        Throw New NotSupportedException()
                    End Function)
                Dim token As SyntaxToken = Nothing
                If symbol IsNot Nothing Then
                    Dim parameterKinds As List(Of RefKind) = symbol.GetParameters().Select(Function(param As IParameterSymbol) param.RefKind).ToList()
                    'WARNING: If named parameters can reach here it won't work properly for them
                    Dim refKind As RefKind = If(argID >= parameterKinds.Count AndAlso symbol.IsParams(), RefKind.None, parameterKinds(argID))
                    Select Case refKind
                        Case RefKind.None
                            token = Nothing
                        Case RefKind.Ref
                            token = SyntaxFactory.Token(SyntaxKind.RefKeyword)
                        Case RefKind.Out
                            token = SyntaxFactory.Token(SyntaxKind.OutKeyword)
                        Case Else
                            Throw New ArgumentOutOfRangeException()
                    End Select
                End If
                Return SyntaxFactory.Argument(If(node.IsNamed, SyntaxFactory.NameColon(CType(node.NameColonEquals.Name.Accept(Me), IdentifierNameSyntax)), Nothing), token, CType(node.Expression.Accept(Me), ExpressionSyntax))
            End Function

            Private Function ExtractMatch(ByVal info As SymbolInfo) As ISymbol
                If info.Symbol Is Nothing AndAlso info.CandidateSymbols.Length = 0 Then
                    Return Nothing
                End If
                If info.Symbol IsNot Nothing Then
                    Return info.Symbol
                End If
                If info.CandidateSymbols.Length = 1 Then
                    Return info.CandidateSymbols(0)
                End If
                Return Nothing
            End Function

            Private Function ToAttributeArgument(ByVal arg As VBSyntax.ArgumentSyntax) As AttributeArgumentSyntax
                If Not (TypeOf arg Is VBSyntax.SimpleArgumentSyntax) Then
                    Throw New NotSupportedException()
                End If
                Dim a As VBSyntax.SimpleArgumentSyntax = CType(arg, VBSyntax.SimpleArgumentSyntax)
                Dim attr As AttributeArgumentSyntax = SyntaxFactory.AttributeArgument(CType(a.Expression.Accept(Me), ExpressionSyntax))
                If a.IsNamed Then
                    attr = attr.WithNameEquals(SyntaxFactory.NameEquals(CType(a.NameColonEquals.Name.Accept(Me), IdentifierNameSyntax)))
                End If
                Return attr
            End Function

            Public Overrides Function VisitNameOfExpression(ByVal node As VBSyntax.NameOfExpressionSyntax) As CSharpSyntaxNode
                Return SyntaxFactory.InvocationExpression(SyntaxFactory.IdentifierName("nameof"), SyntaxFactory.ArgumentList(SyntaxFactory.SingletonSeparatedList(SyntaxFactory.Argument(CType(node.Argument.Accept(Me), ExpressionSyntax)))))
            End Function

            Public Overrides Function VisitEqualsValue(ByVal node As VBSyntax.EqualsValueSyntax) As CSharpSyntaxNode
                Return SyntaxFactory.EqualsValueClause(CType(node.Value.Accept(Me), ExpressionSyntax))
            End Function

            Public Overrides Function VisitAnonymousObjectCreationExpression(ByVal node As VBSyntax.AnonymousObjectCreationExpressionSyntax) As CSharpSyntaxNode
                Return MyBase.VisitAnonymousObjectCreationExpression(node)
            End Function

            Public Overrides Function VisitObjectCreationExpression(ByVal node As VBSyntax.ObjectCreationExpressionSyntax) As CSharpSyntaxNode
                Dim argumentListSyntax As VBSyntax.ArgumentListSyntax = If(node.ArgumentList, VBasic.SyntaxFactory.ArgumentList()) 'VB can omit empty arg lists entirely
                Return SyntaxFactory.ObjectCreationExpression(CType(node.Type.Accept(Me), TypeSyntax), CType(argumentListSyntax.Accept(Me), ArgumentListSyntax), CType(node.Initializer?.Accept(Me), InitializerExpressionSyntax))
            End Function

            Public Overrides Function VisitArrayCreationExpression(ByVal node As VBSyntax.ArrayCreationExpressionSyntax) As CSharpSyntaxNode
                Dim arguments As IEnumerable(Of ExpressionSyntax)
                If node.ArrayBounds IsNot Nothing Then
                    arguments = node.ArrayBounds.Arguments.Select(Function(a As VBSyntax.ArgumentSyntax) IncreaseArrayUpperBoundExpression(CType(a, VBSyntax.SimpleArgumentSyntax).Expression))
                Else
                    arguments = Enumerable.Empty(Of ExpressionSyntax)()
                End If
                Dim bounds As SyntaxList(Of ArrayRankSpecifierSyntax) = SyntaxFactory.List(node.RankSpecifiers.Select(Function(r As VBSyntax.ArrayRankSpecifierSyntax) CType(r.Accept(Me), ArrayRankSpecifierSyntax)))
                Return SyntaxFactory.ArrayCreationExpression(SyntaxFactory.ArrayType(CType(node.Type.Accept(Me), TypeSyntax), bounds), CType(node.Initializer?.Accept(Me), InitializerExpressionSyntax))
            End Function

            Public Overrides Function VisitCollectionInitializer(ByVal node As VBSyntax.CollectionInitializerSyntax) As CSharpSyntaxNode
                If node.Initializers.Count = 0 AndAlso TypeOf node.Parent Is VBSyntax.ArrayCreationExpressionSyntax Then
                    Return Nothing
                End If
                Dim initializer As InitializerExpressionSyntax = SyntaxFactory.InitializerExpression(SyntaxKind.CollectionInitializerExpression, SyntaxFactory.SeparatedList(node.Initializers.Select(Function(i As VBSyntax.ExpressionSyntax) CType(i.Accept(Me), ExpressionSyntax))))
                Dim typeInfo As TypeInfo = ModelExtensions.GetTypeInfo(mSemanticModel, node)
                Return If(typeInfo.Type Is Nothing AndAlso (typeInfo.ConvertedType?.SpecialType = SpecialType.System_Collections_IEnumerable OrElse typeInfo.ConvertedType?.IsKind(SymbolKind.ArrayType) = True), CType(SyntaxFactory.ImplicitArrayCreationExpression(initializer), CSharpSyntaxNode), initializer)
            End Function

            Public Overrides Function VisitObjectCollectionInitializer(ByVal node As VBSyntax.ObjectCollectionInitializerSyntax) As CSharpSyntaxNode
                Return node.Initializer.Accept(Me) 'Dictionary initializer comes through here despite the FROM keyword not being in the source code
            End Function
            Private Function IncreaseArrayUpperBoundExpression(ByVal expr As VBSyntax.ExpressionSyntax) As ExpressionSyntax
                Dim constant As [Optional](Of Object) = mSemanticModel.GetConstantValue(expr)
                If constant.HasValue AndAlso TypeOf constant.Value Is Integer Then
                    Return SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(Math.Truncate(CInt(constant.Value)) + 1))
                End If

                Return SyntaxFactory.BinaryExpression(SyntaxKind.SubtractExpression, CType(expr.Accept(Me), ExpressionSyntax), SyntaxFactory.Token(SyntaxKind.PlusToken), SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(1)))
            End Function

            Public Overrides Function VisitBinaryConditionalExpression(ByVal node As VBSyntax.BinaryConditionalExpressionSyntax) As CSharpSyntaxNode
                Return SyntaxFactory.BinaryExpression(SyntaxKind.CoalesceExpression, CType(node.FirstExpression.Accept(Me), ExpressionSyntax), CType(node.SecondExpression.Accept(Me), ExpressionSyntax))
            End Function

            Public Overrides Function VisitTernaryConditionalExpression(ByVal node As VBSyntax.TernaryConditionalExpressionSyntax) As CSharpSyntaxNode
                Return SyntaxFactory.ConditionalExpression(CType(node.Condition.Accept(Me), ExpressionSyntax), CType(node.WhenTrue.Accept(Me), ExpressionSyntax), CType(node.WhenFalse.Accept(Me), ExpressionSyntax))
            End Function

            Public Overrides Function VisitTypeOfExpression(ByVal node As VBSyntax.TypeOfExpressionSyntax) As CSharpSyntaxNode
                Dim expr As BinaryExpressionSyntax = SyntaxFactory.BinaryExpression(SyntaxKind.IsExpression, CType(node.Expression.Accept(Me), ExpressionSyntax), CType(node.Type.Accept(Me), TypeSyntax))
                If node.IsKind(VBasic.SyntaxKind.TypeOfIsNotExpression) Then
                    Return SyntaxFactory.PrefixUnaryExpression(SyntaxKind.LogicalNotExpression, expr)
                Else
                    Return expr
                End If
            End Function

            Public Overrides Function VisitUnaryExpression(ByVal node As VBSyntax.UnaryExpressionSyntax) As CSharpSyntaxNode
                Dim expr As ExpressionSyntax = CType(node.Operand.Accept(Me), ExpressionSyntax)
                If node.IsKind(VBasic.SyntaxKind.AddressOfExpression) Then
                    Return expr
                End If
                Dim kind As SyntaxKind = ConvertToken(VBasic.VisualBasicExtensions.Kind(node), TokenContext.Local)
                Return SyntaxFactory.PrefixUnaryExpression(kind, SyntaxFactory.Token(CSharpUtil.GetExpressionOperatorTokenKind(kind)), expr)
            End Function

            Public Overrides Function VisitBinaryExpression(ByVal node As VBSyntax.BinaryExpressionSyntax) As CSharpSyntaxNode
                If node.IsKind(VBasic.SyntaxKind.IsExpression) Then
                    Dim otherArgument As ExpressionSyntax = Nothing
                    If node.Left.IsKind(VBasic.SyntaxKind.NothingLiteralExpression) Then
                        otherArgument = CType(node.Right.Accept(Me), ExpressionSyntax)
                    End If
                    If node.Right.IsKind(VBasic.SyntaxKind.NothingLiteralExpression) Then
                        otherArgument = CType(node.Left.Accept(Me), ExpressionSyntax)
                    End If
                    If otherArgument IsNot Nothing Then
                        Return SyntaxFactory.BinaryExpression(SyntaxKind.EqualsExpression, otherArgument, SyntaxFactory.LiteralExpression(SyntaxKind.NullLiteralExpression))
                    End If
                End If
                If node.IsKind(VBasic.SyntaxKind.IsNotExpression) Then
                    Dim otherArgument As ExpressionSyntax = Nothing
                    If node.Left.IsKind(VBasic.SyntaxKind.NothingLiteralExpression) Then
                        otherArgument = CType(node.Right.Accept(Me), ExpressionSyntax)
                    End If
                    If node.Right.IsKind(VBasic.SyntaxKind.NothingLiteralExpression) Then
                        otherArgument = CType(node.Left.Accept(Me), ExpressionSyntax)
                    End If
                    If otherArgument IsNot Nothing Then
                        Return SyntaxFactory.BinaryExpression(SyntaxKind.NotEqualsExpression, otherArgument, SyntaxFactory.LiteralExpression(SyntaxKind.NullLiteralExpression))
                    End If
                End If
                Dim kind As SyntaxKind = ConvertToken(VBasic.VisualBasicExtensions.Kind(node), TokenContext.Local)
                Return SyntaxFactory.BinaryExpression(kind, CType(node.Left.Accept(Me), ExpressionSyntax), SyntaxFactory.Token(CSharpUtil.GetExpressionOperatorTokenKind(kind)), CType(node.Right.Accept(Me), ExpressionSyntax))
            End Function

            Public Overrides Function VisitInvocationExpression(ByVal node As VBSyntax.InvocationExpressionSyntax) As CSharpSyntaxNode
                Dim invocationSymbol As ISymbol = ExtractMatch(ModelExtensions.GetSymbolInfo(mSemanticModel, node))
                Dim symbol As ISymbol = ExtractMatch(ModelExtensions.GetSymbolInfo(mSemanticModel, node.Expression))
                If invocationSymbol?.IsIndexer() = True OrElse symbol?.GetReturnType()?.IsArrayType() = True AndAlso Not (TypeOf symbol Is IMethodSymbol) Then 'The null case happens quite a bit - should try to fix
                    Return SyntaxFactory.ElementAccessExpression(CType(node.Expression.Accept(Me), ExpressionSyntax), SyntaxFactory.BracketedArgumentList(SyntaxFactory.SeparatedList(node.ArgumentList.Arguments.Select(Function(a As VBSyntax.ArgumentSyntax) CType(a.Accept(Me), ArgumentSyntax)))))
                End If

                Return SyntaxFactory.InvocationExpression(CType(node.Expression.Accept(Me), ExpressionSyntax), CType(node.ArgumentList.Accept(Me), ArgumentListSyntax))
            End Function

            Public Overrides Function VisitSingleLineLambdaExpression(ByVal node As VBSyntax.SingleLineLambdaExpressionSyntax) As CSharpSyntaxNode
                Dim body As CSharpSyntaxNode
                If TypeOf node.Body Is VBSyntax.ExpressionSyntax Then
                    body = node.Body.Accept(Me)
                Else
                    Dim stmt As SyntaxList(Of StatementSyntax) = node.Body.Accept(New MethodBodyVisitor(mSemanticModel, Me, withBlockTempVariableNames))
                    body = If(stmt.Count = 1, DirectCast(stmt(0), CSharpSyntaxNode), DirectCast(SyntaxFactory.Block(stmt), CSharpSyntaxNode))
                End If
                Dim param As ParameterListSyntax = CType(node.SubOrFunctionHeader.ParameterList.Accept(Me), ParameterListSyntax)
                If param.Parameters.Count = 1 Then
                    Return SyntaxFactory.SimpleLambdaExpression(param.Parameters(0), body)
                End If
                Return SyntaxFactory.ParenthesizedLambdaExpression(param, body)
            End Function

            Public Overrides Function VisitMultiLineLambdaExpression(ByVal node As VBSyntax.MultiLineLambdaExpressionSyntax) As CSharpSyntaxNode
                Dim body As BlockSyntax = SyntaxFactory.Block(node.Statements.SelectMany(Function(s As VBSyntax.StatementSyntax) s.Accept(New MethodBodyVisitor(mSemanticModel, Me, withBlockTempVariableNames))))
                Dim param As ParameterListSyntax = CType(node.SubOrFunctionHeader.ParameterList.Accept(Me), ParameterListSyntax)
                If param.Parameters.Count = 1 Then
                    Return SyntaxFactory.SimpleLambdaExpression(param.Parameters(0), body)
                End If
                Return SyntaxFactory.ParenthesizedLambdaExpression(param, body)
            End Function

#End Region

#Region "Type Name / Modifier"

            Public Overrides Function VisitPredefinedType(ByVal node As VBSyntax.PredefinedTypeSyntax) As CSharpSyntaxNode
                If SyntaxTokenExtensions.IsKind(node.Keyword, VBasic.SyntaxKind.DateKeyword) Then
                    Return SyntaxFactory.IdentifierName("System.DateTime")
                End If
                Return SyntaxFactory.PredefinedType(ConvertToken(node.Keyword))
            End Function

            Public Overrides Function VisitNullableType(ByVal node As VBSyntax.NullableTypeSyntax) As CSharpSyntaxNode
                Return SyntaxFactory.NullableType(CType(node.ElementType.Accept(Me), TypeSyntax))
            End Function

            Public Overrides Function VisitArrayType(ByVal node As VBSyntax.ArrayTypeSyntax) As CSharpSyntaxNode
                Return SyntaxFactory.ArrayType(CType(node.ElementType.Accept(Me), TypeSyntax), SyntaxFactory.List(node.RankSpecifiers.Select(Function(r As VBSyntax.ArrayRankSpecifierSyntax) CType(r.Accept(Me), ArrayRankSpecifierSyntax))))
            End Function

            Public Overrides Function VisitArrayRankSpecifier(ByVal node As VBSyntax.ArrayRankSpecifierSyntax) As CSharpSyntaxNode
                Return SyntaxFactory.ArrayRankSpecifier(SyntaxFactory.SeparatedList(Enumerable.Repeat(Of ExpressionSyntax)(SyntaxFactory.OmittedArraySizeExpression(), node.Rank)))
            End Function

            Private Sub SplitTypeParameters(ByVal typeParameterList As VBSyntax.TypeParameterListSyntax, ByRef parameters As TypeParameterListSyntax, ByRef constraints As SyntaxList(Of TypeParameterConstraintClauseSyntax))
                parameters = Nothing
                constraints = SyntaxFactory.List(Of TypeParameterConstraintClauseSyntax)()
                If typeParameterList Is Nothing Then
                    Return
                End If
                Dim paramList As List(Of TypeParameterSyntax) = New List(Of TypeParameterSyntax)()
                Dim constraintList As List(Of TypeParameterConstraintClauseSyntax) = New List(Of TypeParameterConstraintClauseSyntax)()
                For Each p As VBSyntax.TypeParameterSyntax In typeParameterList.Parameters
                    Dim tp As TypeParameterSyntax = CType(p.Accept(Me), TypeParameterSyntax)
                    paramList.Add(tp)
                    Dim constraint As TypeParameterConstraintClauseSyntax = CType(p.TypeParameterConstraintClause?.Accept(Me), TypeParameterConstraintClauseSyntax)
                    If constraint IsNot Nothing Then
                        constraintList.Add(constraint)
                    End If
                Next p
                parameters = SyntaxFactory.TypeParameterList(SyntaxFactory.SeparatedList(paramList))
                constraints = SyntaxFactory.List(constraintList)
            End Sub

            Public Overrides Function VisitTypeParameter(ByVal node As VBSyntax.TypeParameterSyntax) As CSharpSyntaxNode
                Dim variance As SyntaxToken = Nothing
                If Not SyntaxTokenExtensions.IsKind(node.VarianceKeyword, VBasic.SyntaxKind.None) Then
                    variance = SyntaxFactory.Token(If(SyntaxTokenExtensions.IsKind(node.VarianceKeyword, VBasic.SyntaxKind.InKeyword), SyntaxKind.InKeyword, SyntaxKind.OutKeyword))
                End If
                Return SyntaxFactory.TypeParameter(SyntaxFactory.List(Of AttributeListSyntax)(), variance, ConvertIdentifier(node.Identifier, mSemanticModel))
            End Function
            Public Overrides Function VisitTypeParameterSingleConstraintClause(ByVal node As VBSyntax.TypeParameterSingleConstraintClauseSyntax) As CSharpSyntaxNode
                Dim id As IdentifierNameSyntax = SyntaxFactory.IdentifierName(ConvertIdentifier(CType(node.Parent, VBSyntax.TypeParameterSyntax).Identifier, mSemanticModel))
                Return SyntaxFactory.TypeParameterConstraintClause(id, SyntaxFactory.SingletonSeparatedList(CType(node.Constraint.Accept(Me), TypeParameterConstraintSyntax)))
            End Function

            Public Overrides Function VisitTypeParameterMultipleConstraintClause(ByVal node As VBSyntax.TypeParameterMultipleConstraintClauseSyntax) As CSharpSyntaxNode
                Dim id As IdentifierNameSyntax = SyntaxFactory.IdentifierName(ConvertIdentifier(CType(node.Parent, VBSyntax.TypeParameterSyntax).Identifier, mSemanticModel))
                Return SyntaxFactory.TypeParameterConstraintClause(id, SyntaxFactory.SeparatedList(node.Constraints.Select(Function(c As VBSyntax.ConstraintSyntax) CType(c.Accept(Me), TypeParameterConstraintSyntax))))
            End Function

            Public Overrides Function VisitSpecialConstraint(ByVal node As VBSyntax.SpecialConstraintSyntax) As CSharpSyntaxNode
                If SyntaxTokenExtensions.IsKind(node.ConstraintKeyword, VBasic.SyntaxKind.NewKeyword) Then
                    Return SyntaxFactory.ConstructorConstraint()
                End If
                Return SyntaxFactory.ClassOrStructConstraint(If(node.IsKind(VBasic.SyntaxKind.ClassConstraint), SyntaxKind.ClassConstraint, SyntaxKind.StructConstraint))
            End Function

            Public Overrides Function VisitTypeConstraint(ByVal node As VBSyntax.TypeConstraintSyntax) As CSharpSyntaxNode
                Return SyntaxFactory.TypeConstraint(CType(node.Type.Accept(Me), TypeSyntax))
            End Function

#End Region

#Region "NameSyntax"

            Public Overrides Function VisitIdentifierName(ByVal node As VBSyntax.IdentifierNameSyntax) As CSharpSyntaxNode
                Return SyntaxFactory.IdentifierName(ConvertIdentifier(node.Identifier, mSemanticModel, node.GetAncestor(Of VBSyntax.AttributeSyntax)() IsNot Nothing))
            End Function

            Public Overrides Function VisitQualifiedName(ByVal node As VBSyntax.QualifiedNameSyntax) As CSharpSyntaxNode
                Dim lhsSyntax As NameSyntax = CType(node.Left.Accept(Me), NameSyntax)
                Dim rhsSyntax As SimpleNameSyntax = CType(node.Right.Accept(Me), SimpleNameSyntax)

                Return If(node.Left.IsKind(VBasic.SyntaxKind.GlobalName), CType(SyntaxFactory.AliasQualifiedName(CType(lhsSyntax, IdentifierNameSyntax), rhsSyntax), CSharpSyntaxNode), SyntaxFactory.QualifiedName(lhsSyntax, rhsSyntax))
            End Function

            Public Overrides Function VisitGenericName(ByVal node As VBSyntax.GenericNameSyntax) As CSharpSyntaxNode
                Return SyntaxFactory.GenericName(ConvertIdentifier(node.Identifier, mSemanticModel), CType(node.TypeArgumentList?.Accept(Me), TypeArgumentListSyntax))
            End Function

            Public Overrides Function VisitTypeArgumentList(ByVal node As VBSyntax.TypeArgumentListSyntax) As CSharpSyntaxNode
                Return SyntaxFactory.TypeArgumentList(SyntaxFactory.SeparatedList(node.Arguments.Select(Function(a As VBSyntax.TypeSyntax) CType(a.Accept(Me), TypeSyntax))))
            End Function

#End Region
        End Class
    End Class
End Namespace
