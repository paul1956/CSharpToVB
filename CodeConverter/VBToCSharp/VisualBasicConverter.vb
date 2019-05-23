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
Imports Microsoft.CodeAnalysis.Text
Imports ArrayRankSpecifierSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.ArrayRankSpecifierSyntax
Imports ArrayTypeSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.ArrayTypeSyntax
Imports CSharpExtensions = Microsoft.CodeAnalysis.CSharp.CSharpExtensions
Imports ExpressionSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.ExpressionSyntax
Imports SyntaxFactory = Microsoft.CodeAnalysis.CSharp.SyntaxFactory
Imports SyntaxFacts = Microsoft.CodeAnalysis.CSharp.SyntaxFacts
Imports SyntaxKind = Microsoft.CodeAnalysis.CSharp.SyntaxKind
Imports TypeSyntax = Microsoft.CodeAnalysis.CSharp.Syntax.TypeSyntax
Imports VBasic = Microsoft.CodeAnalysis.VisualBasic
Imports VBSyntax = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace IVisualBasicCode.CodeConverter.CSharp
    Partial Public Class VisualBasicConverter
        Private Enum TokenContext
            [Global]
            InterfaceOrModule
            Local
            Member
            VariableOrConst
            MemberInModule
            MemberInClass
            MemberInStruct
            MemberInInterface
        End Enum

        Public Shared Function Convert(ByVal input As VBasic.VisualBasicSyntaxNode, ByVal semanticModel As SemanticModel, ByVal targetDocument As Document) As CSharpSyntaxNode
            Return input.Accept(New NodesVisitor(semanticModel, targetDocument))
        End Function

        Public Shared Function ConvertText(ByVal text As String, ByVal references() As MetadataReference) As ConversionResult
            If text Is Nothing Then
                Throw New ArgumentNullException(NameOf(text))
            End If
            If references Is Nothing Then
                Throw New ArgumentNullException(NameOf(references))
            End If
            Dim tree As SyntaxTree = VBasic.SyntaxFactory.ParseSyntaxTree(SourceText.From(text))
            Dim compilation As VBasic.VisualBasicCompilation = VBasic.VisualBasicCompilation.Create(NameOf(Conversion), {tree}, references)
            Try
                Dim cSharpSyntaxNode1 As CSharpSyntaxNode = Convert(CType(tree.GetRoot(), VBasic.VisualBasicSyntaxNode), compilation.GetSemanticModel(tree, True), Nothing)
                Return New ConversionResult(cSharpSyntaxNode1, LanguageNames.VisualBasic, LanguageNames.CSharp)
            Catch ex As Exception
                Return New ConversionResult(ex)
            End Try
        End Function

        Shared Function SplitVariableDeclarations(ByVal declarator As VBSyntax.VariableDeclaratorSyntax, ByVal lNodesVisitor As NodesVisitor, ByVal semanticModel As SemanticModel) As Dictionary(Of String, VariableDeclarationSyntax)
            Dim tmp As VBasic.VisualBasicSyntaxNode = declarator.AsClause?.TypeSwitch(
                Function(c As VBSyntax.SimpleAsClauseSyntax) c.Type,
                Function(c As VBSyntax.AsNewClauseSyntax) VBasic.SyntaxExtensions.Type(c.NewExpression),
                Function(underscore As VBSyntax.AsClauseSyntax) As VBSyntax.TypeSyntax
                    Throw New NotImplementedException($"{underscore.GetType().FullName} not implemented!")
                End Function)


            Dim rawType As TypeSyntax = CType(If(tmp?.Accept(lNodesVisitor), SyntaxFactory.ParseTypeName("var")), TypeSyntax)
            Dim initializer As ExpressionSyntax = If(CType(declarator.AsClause?.TypeSwitch(
                Function(underscore As VBSyntax.SimpleAsClauseSyntax) declarator.Initializer?.Value,
                Function(c As VBSyntax.AsNewClauseSyntax) c.NewExpression
                )?.Accept(lNodesVisitor), ExpressionSyntax), CType(declarator.Initializer?.Value.Accept(lNodesVisitor), ExpressionSyntax))

            Dim newDecls As Dictionary(Of String, VariableDeclarationSyntax) = New Dictionary(Of String, VariableDeclarationSyntax)()

            For Each name As VBSyntax.ModifiedIdentifierSyntax In declarator.Names
                Dim type As TypeSyntax = rawType
                If Not SyntaxTokenExtensions.IsKind(name.Nullable, VBasic.SyntaxKind.None) Then
                    If TypeOf type Is ArrayTypeSyntax Then
                        type = CType(type, ArrayTypeSyntax).WithElementType(SyntaxFactory.NullableType(CType(type, ArrayTypeSyntax).ElementType))
                    Else
                        type = SyntaxFactory.NullableType(type)
                    End If
                End If
                If name.ArrayRankSpecifiers.Count > 0 Then
                    type = SyntaxFactory.ArrayType(type, SyntaxFactory.List(name.ArrayRankSpecifiers.Select(Function(a As VBSyntax.ArrayRankSpecifierSyntax) CType(a.Accept(lNodesVisitor), ArrayRankSpecifierSyntax))))
                End If
                Dim decl As VariableDeclarationSyntax = Nothing
                Dim v As VariableDeclaratorSyntax = SyntaxFactory.VariableDeclarator(ConvertIdentifier(name.Identifier, semanticModel), Nothing, If(initializer Is Nothing, Nothing, SyntaxFactory.EqualsValueClause(initializer)))
                Dim k As String = type.ToString()
                If newDecls.TryGetValue(k, decl) Then
                    newDecls(k) = decl.AddVariables(v)
                Else
                    newDecls(k) = SyntaxFactory.VariableDeclaration(type, SyntaxFactory.SingletonSeparatedList(v))
                End If
            Next name

            Return newDecls
        End Function

        Private Shared Function Literal(ByVal o As Object) As ExpressionSyntax
            Return GetLiteralExpression(o)
        End Function

        Friend Shared Function GetLiteralExpression(ByVal value As Object) As ExpressionSyntax
            If TypeOf value Is Boolean Then
                Return SyntaxFactory.LiteralExpression(If(DirectCast(value, Boolean), SyntaxKind.TrueLiteralExpression, SyntaxKind.FalseLiteralExpression))
            End If
            If TypeOf value Is Byte Then
                Return SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(DirectCast(value, Byte)))
            End If
            If TypeOf value Is SByte Then
                Return SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(DirectCast(value, SByte)))
            End If
            If TypeOf value Is Short Then
                Return SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(DirectCast(value, Short)))
            End If
            If TypeOf value Is UShort Then
                Return SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(DirectCast(value, UShort)))
            End If
            If TypeOf value Is Integer Then
                Return SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(DirectCast(value, Integer)))
            End If
            If TypeOf value Is UInteger Then
                Return SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(DirectCast(value, UInteger)))
            End If
            If TypeOf value Is Long Then
                Return SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(DirectCast(value, Long)))
            End If
            If TypeOf value Is ULong Then
                Return SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(DirectCast(value, ULong)))
            End If

            If TypeOf value Is Single Then
                Return SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(DirectCast(value, Single)))
            End If
            If TypeOf value Is Double Then
                Return SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(DirectCast(value, Double)))
            End If
            If TypeOf value Is Decimal Then
                Return SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(DirectCast(value, Decimal)))
            End If

            If TypeOf value Is Char Then
                Return SyntaxFactory.LiteralExpression(SyntaxKind.CharacterLiteralExpression, SyntaxFactory.Literal(DirectCast(value, Char)))
            End If

            If TypeOf value Is String Then
                Return SyntaxFactory.LiteralExpression(SyntaxKind.StringLiteralExpression, SyntaxFactory.Literal(DirectCast(value, String)))
            End If

            If value Is Nothing Then
                Return SyntaxFactory.LiteralExpression(SyntaxKind.NullLiteralExpression)
            End If

            Return Nothing
        End Function


        Private Shared Function ConvertIdentifier(ByVal id As SyntaxToken, ByVal semanticModel As SemanticModel, Optional ByVal isAttribute As Boolean = False) As SyntaxToken
            Dim text As String = id.ValueText
            Dim keywordKind As SyntaxKind = SyntaxFacts.GetKeywordKind(text)
            If keywordKind <> SyntaxKind.None Then
                Return SyntaxFactory.Identifier("@" & text)
            End If
            If id.SyntaxTree Is semanticModel.SyntaxTree Then
                Dim symbol As ISymbol = semanticModel.GetSymbolInfo(id.Parent).Symbol
                If symbol IsNot Nothing AndAlso Not String.IsNullOrWhiteSpace(symbol.Name) Then
                    If symbol.IsConstructor() AndAlso isAttribute Then
                        text = symbol.ContainingType.Name
                        If text.EndsWith(NameOf(Attribute), StringComparison.Ordinal) Then
                            text = text.Remove(text.Length - NameOf(Attribute).Length)
                        End If
                        'INSTANT VB TODO TASK: VB has no equivalent to C# pattern variables in 'is' expressions:
                        'ORIGINAL LINE: else if (text.StartsWith("_", StringComparison.Ordinal) && symbol is IFieldSymbol fieldSymbol && If(fieldSymbol.AssociatedSymbol Is null, null, fieldSymbol.AssociatedSymbol.IsKind(SymbolKind.@Property)) == true)
                    ElseIf text.StartsWith("_", StringComparison.Ordinal) AndAlso TypeOf symbol Is IFieldSymbol Then
                        Dim fieldSymbol As IFieldSymbol = CType(symbol, IFieldSymbol)
                        If fieldSymbol.AssociatedSymbol IsNot Nothing Then
                            text = If(fieldSymbol.AssociatedSymbol?.IsKind(SymbolKind.Property) = True, fieldSymbol.AssociatedSymbol.Name, symbol.Name)
                        End If
                    End If
                End If
            End If
            Return SyntaxFactory.Identifier(text)
        End Function

        Private Shared Function ConvertModifiers(ByVal modifiers As IEnumerable(Of SyntaxToken), Optional ByVal context As TokenContext = TokenContext.Global) As SyntaxTokenList
            Return SyntaxFactory.TokenList(ConvertModifiersCore(modifiers, context))
        End Function

        Private Shared Function ConvertModifiers(ByVal modifiers As SyntaxTokenList, Optional ByVal context As TokenContext = TokenContext.Global) As SyntaxTokenList
            Return SyntaxFactory.TokenList(ConvertModifiersCore(modifiers, context).Where(Function(t As SyntaxToken) CSharpExtensions.Kind(t) <> SyntaxKind.None))
        End Function

        Private Shared Function ConvertModifier(ByVal m As SyntaxToken, Optional ByVal context As TokenContext = TokenContext.Global) As SyntaxToken?
            Dim vbSyntaxKind As VBasic.SyntaxKind = VBasic.VisualBasicExtensions.Kind(m)
            Select Case vbSyntaxKind
                Case VBasic.SyntaxKind.DateKeyword
                    Return SyntaxFactory.Identifier("System.DateTime")
            End Select
            Dim token As SyntaxKind = ConvertToken(vbSyntaxKind, context)
            Return If(token = SyntaxKind.None, Nothing, New SyntaxToken?(SyntaxFactory.Token(token)))
        End Function
        Private Shared Iterator Function ConvertModifiersCore(ByVal modifiers As IEnumerable(Of SyntaxToken), ByVal context As TokenContext) As IEnumerable(Of SyntaxToken)
            Dim contextsWithIdenticalDefaults As TokenContext() = {TokenContext.Global, TokenContext.Local, TokenContext.InterfaceOrModule, TokenContext.MemberInInterface}
            If Not contextsWithIdenticalDefaults.Contains(context) Then
                Dim visibility As Boolean = False
                For Each token As SyntaxToken In modifiers
                    If IsVisibility(token, context) Then
                        visibility = True
                        Exit For
                    End If
                Next token
                If Not visibility Then
                    Yield VisualBasicDefaultVisibility(context)
                End If
            End If
            For Each token As SyntaxToken In modifiers.Where(Function(m As SyntaxToken) Not IgnoreInContext(m, context)).OrderBy(Function(m As SyntaxToken) SyntaxTokenExtensions.IsKind(m, VBasic.SyntaxKind.PartialKeyword))
                Dim m As SyntaxToken? = ConvertModifier(token, context)
                If m.HasValue Then
                    Yield m.Value
                End If
            Next token
            If context = TokenContext.MemberInModule Then
                Yield SyntaxFactory.Token(SyntaxKind.StaticKeyword)
            End If
        End Function

        Private Shared Function IgnoreInContext(ByVal m As SyntaxToken, ByVal context As TokenContext) As Boolean
            Select Case VBasic.VisualBasicExtensions.Kind(m)
                Case VBasic.SyntaxKind.OptionalKeyword, VBasic.SyntaxKind.ByValKeyword, VBasic.SyntaxKind.IteratorKeyword, VBasic.SyntaxKind.DimKeyword
                    Return True
                Case VBasic.SyntaxKind.ReadOnlyKeyword, VBasic.SyntaxKind.WriteOnlyKeyword
                    Return context = TokenContext.Member
                Case Else
                    Return False
            End Select
        End Function

        Private Shared Function IsVisibility(ByVal token As SyntaxToken, ByVal context As TokenContext) As Boolean
            Return token.IsKind(VBasic.SyntaxKind.PublicKeyword, VBasic.SyntaxKind.FriendKeyword, VBasic.SyntaxKind.ProtectedKeyword, VBasic.SyntaxKind.PrivateKeyword) OrElse (context = TokenContext.VariableOrConst AndAlso SyntaxTokenExtensions.IsKind(token, VBasic.SyntaxKind.ConstKeyword))
        End Function
        Private Shared Function VisualBasicDefaultVisibility(ByVal context As TokenContext) As SyntaxToken
            Select Case context
                Case TokenContext.Global, TokenContext.InterfaceOrModule
                    Return SyntaxFactory.Token(SyntaxKind.InternalKeyword)
                Case TokenContext.Member, TokenContext.MemberInModule, TokenContext.MemberInClass, TokenContext.MemberInInterface, TokenContext.MemberInStruct
                    Return SyntaxFactory.Token(SyntaxKind.PublicKeyword)
                Case TokenContext.Local, TokenContext.VariableOrConst
                    Return SyntaxFactory.Token(SyntaxKind.PrivateKeyword)
            End Select
            Throw New ArgumentOutOfRangeException(NameOf(context), context, "Specified argument was out of the range of valid values.")
        End Function

        Private Shared Function ConvertToken(ByVal t As SyntaxToken, Optional ByVal context As TokenContext = TokenContext.Global) As SyntaxToken
            Dim vbSyntaxKind As VBasic.SyntaxKind = VBasic.VisualBasicExtensions.Kind(t)
            Return SyntaxFactory.Token(ConvertToken(vbSyntaxKind, context))
        End Function

        Private Shared Function ConvertToken(ByVal t As VBasic.SyntaxKind, Optional ByVal context As TokenContext = TokenContext.Global) As SyntaxKind
            Select Case t
                Case VBasic.SyntaxKind.None
                    Return SyntaxKind.None
                ' built-in types
                Case VBasic.SyntaxKind.BooleanKeyword
                    Return SyntaxKind.BoolKeyword
                Case VBasic.SyntaxKind.ByteKeyword
                    Return SyntaxKind.ByteKeyword
                Case VBasic.SyntaxKind.SByteKeyword
                    Return SyntaxKind.SByteKeyword
                Case VBasic.SyntaxKind.ShortKeyword
                    Return SyntaxKind.ShortKeyword
                Case VBasic.SyntaxKind.UShortKeyword
                    Return SyntaxKind.UShortKeyword
                Case VBasic.SyntaxKind.IntegerKeyword
                    Return SyntaxKind.IntKeyword
                Case VBasic.SyntaxKind.UIntegerKeyword
                    Return SyntaxKind.UIntKeyword
                Case VBasic.SyntaxKind.LongKeyword
                    Return SyntaxKind.LongKeyword
                Case VBasic.SyntaxKind.ULongKeyword
                    Return SyntaxKind.ULongKeyword
                Case VBasic.SyntaxKind.DoubleKeyword
                    Return SyntaxKind.DoubleKeyword
                Case VBasic.SyntaxKind.SingleKeyword
                    Return SyntaxKind.FloatKeyword
                Case VBasic.SyntaxKind.DecimalKeyword
                    Return SyntaxKind.DecimalKeyword
                Case VBasic.SyntaxKind.StringKeyword
                    Return SyntaxKind.StringKeyword
                Case VBasic.SyntaxKind.CharKeyword
                    Return SyntaxKind.CharKeyword
                Case VBasic.SyntaxKind.ObjectKeyword
                    Return SyntaxKind.ObjectKeyword
                ' literals
                Case VBasic.SyntaxKind.NothingKeyword
                    Return SyntaxKind.NullKeyword
                Case VBasic.SyntaxKind.TrueKeyword
                    Return SyntaxKind.TrueKeyword
                Case VBasic.SyntaxKind.FalseKeyword
                    Return SyntaxKind.FalseKeyword
                Case VBasic.SyntaxKind.MeKeyword
                    Return SyntaxKind.ThisKeyword
                Case VBasic.SyntaxKind.MyBaseKeyword
                    Return SyntaxKind.BaseKeyword
                ' modifiers
                Case VBasic.SyntaxKind.PublicKeyword
                    Return SyntaxKind.PublicKeyword
                Case VBasic.SyntaxKind.FriendKeyword
                    Return SyntaxKind.InternalKeyword
                Case VBasic.SyntaxKind.ProtectedKeyword
                    Return SyntaxKind.ProtectedKeyword
                Case VBasic.SyntaxKind.PrivateKeyword
                    Return SyntaxKind.PrivateKeyword
                Case VBasic.SyntaxKind.ByRefKeyword
                    Return SyntaxKind.RefKeyword
                Case VBasic.SyntaxKind.ParamArrayKeyword
                    Return SyntaxKind.ParamsKeyword
                Case VBasic.SyntaxKind.ReadOnlyKeyword
                    Return SyntaxKind.ReadOnlyKeyword
                Case VBasic.SyntaxKind.OverridesKeyword
                    Return SyntaxKind.OverrideKeyword
                Case VBasic.SyntaxKind.OverloadsKeyword
                    Return SyntaxKind.NewKeyword
                Case VBasic.SyntaxKind.OverridableKeyword
                    Return SyntaxKind.VirtualKeyword
                Case VBasic.SyntaxKind.SharedKeyword
                    Return SyntaxKind.StaticKeyword
                Case VBasic.SyntaxKind.ConstKeyword
                    Return SyntaxKind.ConstKeyword
                Case VBasic.SyntaxKind.PartialKeyword
                    Return SyntaxKind.PartialKeyword
                Case VBasic.SyntaxKind.MustInheritKeyword
                    Return SyntaxKind.AbstractKeyword
                Case VBasic.SyntaxKind.MustOverrideKeyword
                    Return SyntaxKind.AbstractKeyword
                Case VBasic.SyntaxKind.NotOverridableKeyword, VBasic.SyntaxKind.NotInheritableKeyword
                    Return SyntaxKind.SealedKeyword
                ' unary operators
                Case VBasic.SyntaxKind.UnaryMinusExpression
                    Return SyntaxKind.UnaryMinusExpression
                Case VBasic.SyntaxKind.UnaryPlusExpression
                    Return SyntaxKind.UnaryPlusExpression
                Case VBasic.SyntaxKind.NotExpression
                    Return SyntaxKind.LogicalNotExpression
                ' binary operators
                Case VBasic.SyntaxKind.ConcatenateExpression, VBasic.SyntaxKind.AddExpression
                    Return SyntaxKind.AddExpression
                Case VBasic.SyntaxKind.SubtractExpression
                    Return SyntaxKind.SubtractExpression
                Case VBasic.SyntaxKind.MultiplyExpression
                    Return SyntaxKind.MultiplyExpression
                Case VBasic.SyntaxKind.DivideExpression
                    Return SyntaxKind.DivideExpression
                Case VBasic.SyntaxKind.ModuloExpression
                    Return SyntaxKind.ModuloExpression
                Case VBasic.SyntaxKind.AndAlsoExpression
                    Return SyntaxKind.LogicalAndExpression
                Case VBasic.SyntaxKind.OrElseExpression
                    Return SyntaxKind.LogicalOrExpression
                Case VBasic.SyntaxKind.OrExpression
                    Return SyntaxKind.BitwiseOrExpression
                Case VBasic.SyntaxKind.AndExpression
                    Return SyntaxKind.BitwiseAndExpression
                Case VBasic.SyntaxKind.ExclusiveOrExpression
                    Return SyntaxKind.ExclusiveOrExpression
                Case VBasic.SyntaxKind.EqualsExpression
                    Return SyntaxKind.EqualsExpression
                Case VBasic.SyntaxKind.NotEqualsExpression
                    Return SyntaxKind.NotEqualsExpression
                Case VBasic.SyntaxKind.GreaterThanExpression
                    Return SyntaxKind.GreaterThanExpression
                Case VBasic.SyntaxKind.GreaterThanOrEqualExpression
                    Return SyntaxKind.GreaterThanOrEqualExpression
                Case VBasic.SyntaxKind.LessThanExpression
                    Return SyntaxKind.LessThanExpression
                Case VBasic.SyntaxKind.LessThanOrEqualExpression
                    Return SyntaxKind.LessThanOrEqualExpression
                ' assignment
                Case VBasic.SyntaxKind.SimpleAssignmentStatement
                    Return SyntaxKind.SimpleAssignmentExpression
                Case VBasic.SyntaxKind.ConcatenateAssignmentStatement, VBasic.SyntaxKind.AddAssignmentStatement
                    Return SyntaxKind.AddAssignmentExpression
                Case VBasic.SyntaxKind.SubtractAssignmentStatement
                    Return SyntaxKind.SubtractAssignmentExpression
                Case VBasic.SyntaxKind.MultiplyAssignmentStatement
                    Return SyntaxKind.MultiplyAssignmentExpression
                Case VBasic.SyntaxKind.DivideAssignmentStatement
                    Return SyntaxKind.DivideAssignmentExpression
                ' Casts
                Case VBasic.SyntaxKind.CObjKeyword
                    Return SyntaxKind.ObjectKeyword
                Case VBasic.SyntaxKind.CBoolKeyword
                    Return SyntaxKind.BoolKeyword
                Case VBasic.SyntaxKind.CCharKeyword
                    Return SyntaxKind.CharKeyword
                Case VBasic.SyntaxKind.CSByteKeyword
                    Return SyntaxKind.SByteKeyword
                Case VBasic.SyntaxKind.CByteKeyword
                    Return SyntaxKind.ByteKeyword
                Case VBasic.SyntaxKind.CShortKeyword
                    Return SyntaxKind.ShortKeyword
                Case VBasic.SyntaxKind.CUShortKeyword
                    Return SyntaxKind.UShortKeyword
                Case VBasic.SyntaxKind.CIntKeyword
                    Return SyntaxKind.IntKeyword
                Case VBasic.SyntaxKind.CUIntKeyword
                    Return SyntaxKind.UIntKeyword
                Case VBasic.SyntaxKind.CLngKeyword
                    Return SyntaxKind.LongKeyword
                Case VBasic.SyntaxKind.CULngKeyword
                    Return SyntaxKind.ULongKeyword
                Case VBasic.SyntaxKind.CDecKeyword
                    Return SyntaxKind.DecimalKeyword
                Case VBasic.SyntaxKind.CSngKeyword
                    Return SyntaxKind.FloatKeyword
                Case VBasic.SyntaxKind.CDblKeyword
                    Return SyntaxKind.DoubleKeyword
                Case VBasic.SyntaxKind.CStrKeyword
                    Return SyntaxKind.StringKeyword
                '
                Case VBasic.SyntaxKind.AssemblyKeyword
                    Return SyntaxKind.AssemblyKeyword
                Case VBasic.SyntaxKind.AsyncKeyword
                    Return SyntaxKind.AsyncKeyword
            End Select
            Throw New NotSupportedException(t & " not supported!")
        End Function
    End Class
End Namespace
