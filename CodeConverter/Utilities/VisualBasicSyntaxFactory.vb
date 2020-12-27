' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

' DO NOT REORDER DOCUMENT Tokens must be defined BEFORE they are used, the #If is require to prevent code cleanup
#If True Then

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory

Namespace CSharpToVBConverter

    Public Module VisualBasicSyntaxFactory

        Public Const Quote As String = """"
        Public Const DoubleQuote As String = """"""
        Public Const SyntaxKindNone As SyntaxKind = 0
        Public Const SystemString As SpecialType = SpecialType.System_String
        Public Const UnicodeOpenQuote As Char = ChrW(&H201C)
        Public Const UnicodeCloseQuote As Char = ChrW(&H201D)
        Public Const UnicodeDoubleOpenQuote As String = UnicodeOpenQuote & UnicodeOpenQuote
        Public Const UnicodeDoubleCloseQuote As String = UnicodeCloseQuote & UnicodeCloseQuote
        Public Const UnicodeFullWidthQuoationMark As Char = ChrW(&HFF02)

#Region "Token"

        Public ReadOnly AddHandlerKeyword As SyntaxToken = Factory.Token(SyntaxKind.AddHandlerKeyword)
        Public ReadOnly AddressOfKeyword As SyntaxToken = Factory.Token(SyntaxKind.AddressOfKeyword)
        Public ReadOnly AmpersandToken As SyntaxToken = Factory.Token(SyntaxKind.AmpersandToken)
        Public ReadOnly AndAlsoKeyword As SyntaxToken = Factory.Token(SyntaxKind.AndAlsoKeyword)
        Public ReadOnly AndKeyword As SyntaxToken = Factory.Token(SyntaxKind.AndKeyword)
        Public ReadOnly AsKeyword As SyntaxToken = Factory.Token(SyntaxKind.AsKeyword)
        Public ReadOnly AssemblyKeyword As SyntaxToken = Factory.Token(SyntaxKind.AssemblyKeyword)
        Public ReadOnly AsteriskEqualsToken As SyntaxToken = Factory.Token(SyntaxKind.AsteriskEqualsToken)
        Public ReadOnly AsteriskToken As SyntaxToken = Factory.Token(SyntaxKind.AsteriskToken)
        Public ReadOnly AsyncKeyword As SyntaxToken = Factory.Token(SyntaxKind.AsyncKeyword)
        Public ReadOnly AwaitKeyword As SyntaxToken = Factory.Token(SyntaxKind.AwaitKeyword)
        Public ReadOnly BeginCDataToken As SyntaxToken = Factory.Token(SyntaxKind.BeginCDataToken)
        Public ReadOnly BlockKeyword As SyntaxToken = Factory.Token(SyntaxKind.OperatorKeyword)
        Public ReadOnly BooleanKeyword As SyntaxToken = Factory.Token(SyntaxKind.BooleanKeyword)
        Public ReadOnly ByRefKeyword As SyntaxToken = Factory.Token(SyntaxKind.ByRefKeyword)
        Public ReadOnly ByteKeyword As SyntaxToken = Factory.Token(SyntaxKind.ByteKeyword)
        Public ReadOnly ByValKeyword As SyntaxToken = Factory.Token(SyntaxKind.ByValKeyword)
        Public ReadOnly CaseKeyword As SyntaxToken = Factory.Token(SyntaxKind.CaseKeyword)
        Public ReadOnly CBoolKeyword As SyntaxToken = Factory.Token(SyntaxKind.CBoolKeyword)
        Public ReadOnly CByteKeyword As SyntaxToken = Factory.Token(SyntaxKind.CByteKeyword)
        Public ReadOnly CCharKeyword As SyntaxToken = Factory.Token(SyntaxKind.CCharKeyword)
        Public ReadOnly CDateKeyword As SyntaxToken = Factory.Token(SyntaxKind.CDateKeyword)
        Public ReadOnly CDblKeyword As SyntaxToken = Factory.Token(SyntaxKind.CDblKeyword)
        Public ReadOnly CDecKeyword As SyntaxToken = Factory.Token(SyntaxKind.CDecKeyword)
        Public ReadOnly CharKeyword As SyntaxToken = Factory.Token(SyntaxKind.CharKeyword)
        Public ReadOnly CIntKeyword As SyntaxToken = Factory.Token(SyntaxKind.CIntKeyword)
        Public ReadOnly ClassKeyWord As SyntaxToken = Factory.Token(SyntaxKind.ClassKeyword)
        Public ReadOnly CLngKeyword As SyntaxToken = Factory.Token(SyntaxKind.CLngKeyword)
        Public ReadOnly CloseBraceToken As SyntaxToken = Factory.Token(SyntaxKind.CloseBraceToken)
        Public ReadOnly CloseParenToken As SyntaxToken = Factory.Token(SyntaxKind.CloseParenToken)
        Public ReadOnly CObjKeyword As SyntaxToken = Factory.Token(SyntaxKind.CObjKeyword)
        Public ReadOnly CommaToken As SyntaxToken = Factory.Token(SyntaxKind.CommaToken)
        Public ReadOnly ConstKeyword As SyntaxToken = Factory.Token(SyntaxKind.ConstKeyword)
        Public ReadOnly CSByteKeyword As SyntaxToken = Factory.Token(SyntaxKind.CSByteKeyword)
        Public ReadOnly CShortKeyword As SyntaxToken = Factory.Token(SyntaxKind.CShortKeyword)
        Public ReadOnly CSngKeyword As SyntaxToken = Factory.Token(SyntaxKind.CSngKeyword)
        Public ReadOnly CStrKeyword As SyntaxToken = Factory.Token(SyntaxKind.CStrKeyword)
        Public ReadOnly CTypeKeyword As SyntaxToken = Factory.Token(SyntaxKind.CTypeKeyword)
        Public ReadOnly CUIntKeyword As SyntaxToken = Factory.Token(SyntaxKind.CUIntKeyword)
        Public ReadOnly CULngKeyword As SyntaxToken = Factory.Token(SyntaxKind.CULngKeyword)
        Public ReadOnly CUShortKeyword As SyntaxToken = Factory.Token(SyntaxKind.CUShortKeyword)
        Public ReadOnly CustomKeyword As SyntaxToken = Factory.Token(SyntaxKind.CustomKeyword)
        Public ReadOnly DateKeyword As SyntaxToken = Factory.Token(SyntaxKind.DateKeyword)
        Public ReadOnly DecimalKeyword As SyntaxToken = Factory.Token(SyntaxKind.DecimalKeyword)
        Public ReadOnly DefaultKeyword As SyntaxToken = Factory.Token(SyntaxKind.DefaultKeyword)
        Public ReadOnly DimKeyword As SyntaxToken = Factory.Token(SyntaxKind.DimKeyword)
        Public ReadOnly DoKeyword As SyntaxToken = Factory.Token(SyntaxKind.DoKeyword)
        Public ReadOnly DotToken As SyntaxToken = Factory.Token(SyntaxKind.DotToken)
        Public ReadOnly DoubleKeyword As SyntaxToken = Factory.Token(SyntaxKind.DoubleKeyword)
        Public ReadOnly DoubleQuoteToken As SyntaxToken = Factory.Token(SyntaxKind.DoubleQuoteToken)
        Public ReadOnly ElseIfKeyword As SyntaxToken = Factory.Token(SyntaxKind.ElseIfKeyword)
        Public ReadOnly ElseKeyword As SyntaxToken = Factory.Token(SyntaxKind.ElseKeyword)
        Public ReadOnly EmptyToken As SyntaxToken = Factory.Token(SyntaxKind.EmptyToken)
        Public ReadOnly EndCDataToken As SyntaxToken = Factory.Token(SyntaxKind.EndCDataToken)
        Public ReadOnly EndKeyword As SyntaxToken = Factory.Token(SyntaxKind.EndKeyword)
        Public ReadOnly EndOfFileToken As SyntaxToken = Factory.Token(SyntaxKind.EndOfFileToken)
        Public ReadOnly EnumKeyword As SyntaxToken = Factory.Token(SyntaxKind.EnumKeyword)
        Public ReadOnly EqualsToken As SyntaxToken = Factory.Token(SyntaxKind.EqualsToken)
        Public ReadOnly ExternalChecksumKeyword As SyntaxToken = Factory.Token(SyntaxKind.ExternalChecksumKeyword)
        Public ReadOnly ExternalSourceKeyword As SyntaxToken = Factory.Token(SyntaxKind.ExternalSourceKeyword)
        Public ReadOnly FalseKeyword As SyntaxToken = Factory.Token(SyntaxKind.FalseKeyword)
        Public ReadOnly ForKeyword As SyntaxToken = Factory.Token(SyntaxKind.ForKeyword)
        Public ReadOnly FriendKeyword As SyntaxToken = Factory.Token(SyntaxKind.FriendKeyword)
        Public ReadOnly FromKeyword As SyntaxToken = Factory.Token(SyntaxKind.FromKeyword)
        Public ReadOnly FunctionKeyword As SyntaxToken = Factory.Token(SyntaxKind.FunctionKeyword)
        Public ReadOnly GetKeyword As SyntaxToken = Factory.Token(SyntaxKind.GetKeyword)
        Public ReadOnly GreaterThanEqualsToken As SyntaxToken = Factory.Token(SyntaxKind.GreaterThanEqualsToken)
        Public ReadOnly GreaterThanGreaterThanEqualsToken As SyntaxToken = Factory.Token(SyntaxKind.GreaterThanGreaterThanEqualsToken)
        Public ReadOnly GreaterThanGreaterThanToken As SyntaxToken = Factory.Token(SyntaxKind.GreaterThanGreaterThanToken)
        Public ReadOnly GreaterThanToken As SyntaxToken = Factory.Token(SyntaxKind.GreaterThanToken)
        Public ReadOnly HashToken As SyntaxToken = Factory.Token(SyntaxKind.HashToken)
        Public ReadOnly IfKeyword As SyntaxToken = Factory.Token(SyntaxKind.IfKeyword)
        Public ReadOnly InKeyword As SyntaxToken = Factory.Token(SyntaxKind.InKeyword)
        Public ReadOnly IntegerKeyword As SyntaxToken = Factory.Token(SyntaxKind.IntegerKeyword)
        Public ReadOnly InterfaceKeyword As SyntaxToken = Factory.Token(SyntaxKind.InterfaceKeyword)
        Public ReadOnly IsFalse As SyntaxToken = Factory.Token(SyntaxKind.IsFalseKeyword)
        Public ReadOnly IsKeyword As SyntaxToken = Factory.Token(SyntaxKind.IsKeyword)
        Public ReadOnly IsNotKeyword As SyntaxToken = Factory.Token(SyntaxKind.IsNotKeyword)
        Public ReadOnly IsTrueKeyword As SyntaxToken = Factory.Token(SyntaxKind.IsTrueKeyword)
        Public ReadOnly IteratorKeyword As SyntaxToken = Factory.Token(SyntaxKind.IteratorKeyword)
        Public ReadOnly KeyKeyword As SyntaxToken = Factory.Token(SyntaxKind.KeyKeyword)
        Public ReadOnly LessThanEqualsToken As SyntaxToken = Factory.Token(SyntaxKind.LessThanEqualsToken)
        Public ReadOnly LessThanGreaterThanToken As SyntaxToken = Factory.Token(SyntaxKind.LessThanGreaterThanToken)
        Public ReadOnly LessThanLessThanEqualsToken As SyntaxToken = Factory.Token(SyntaxKind.LessThanLessThanEqualsToken)
        Public ReadOnly LessThanLessThanToken As SyntaxToken = Factory.Token(SyntaxKind.LessThanLessThanToken)
        Public ReadOnly LessThanToken As SyntaxToken = Factory.Token(SyntaxKind.LessThanToken)
        Public ReadOnly LongKeyword As SyntaxToken = Factory.Token(SyntaxKind.LongKeyword)
        Public ReadOnly MeKeyword As SyntaxToken = Factory.Token(SyntaxKind.MeKeyword)
        Public ReadOnly MinusEqualsToken As SyntaxToken = Factory.Token(SyntaxKind.MinusEqualsToken)
        Public ReadOnly MinusToken As SyntaxToken = Factory.Token(SyntaxKind.MinusToken)
        Public ReadOnly ModKeyword As SyntaxToken = Factory.Token(SyntaxKind.ModKeyword)
        Public ReadOnly ModuleKeyword As SyntaxToken = Factory.Token(SyntaxKind.ModuleKeyword)
        Public ReadOnly MustInheritKeyword As SyntaxToken = Factory.Token(SyntaxKind.MustInheritKeyword)
        Public ReadOnly MustOverrideKeyword As SyntaxToken = Factory.Token(SyntaxKind.MustOverrideKeyword)
        Public ReadOnly MyBaseKeyword As SyntaxToken = Factory.Token(SyntaxKind.MyBaseKeyword)
        Public ReadOnly NamespaceKeyword As SyntaxToken = Factory.Token(SyntaxKind.NamespaceKeyword)
        Public ReadOnly NarrowingKeyword As SyntaxToken = Factory.Token(SyntaxKind.NarrowingKeyword)
        Public ReadOnly NewKeyword As SyntaxToken = Factory.Token(SyntaxKind.NewKeyword)
        Public ReadOnly NothingKeyword As SyntaxToken = Factory.Token(SyntaxKind.NothingKeyword)
        Public ReadOnly NotInheritableKeyword As SyntaxToken = Factory.Token(SyntaxKind.NotInheritableKeyword)
        Public ReadOnly NotKeyword As SyntaxToken = Factory.Token(SyntaxKind.NotKeyword)
        Public ReadOnly NotOverridableKeyword As SyntaxToken = Factory.Token(SyntaxKind.NotOverridableKeyword)
        Public ReadOnly NullableToken As SyntaxToken = Factory.Token(SyntaxKind.QuestionToken)
        Public ReadOnly ObjectKeyword As SyntaxToken = Factory.Token(SyntaxKind.ObjectKeyword)
        Public ReadOnly OfKeyword As SyntaxToken = Factory.Token(SyntaxKind.OfKeyword)
        Public ReadOnly OpenBraceToken As SyntaxToken = Factory.Token(SyntaxKind.OpenBraceToken)
        Public ReadOnly openParenToken As SyntaxToken = Factory.Token(SyntaxKind.OpenParenToken)
        Public ReadOnly OptionalKeyword As SyntaxToken = Factory.Token(SyntaxKind.OptionalKeyword)
        Public ReadOnly OrElseKeyword As SyntaxToken = Factory.Token(SyntaxKind.OrElseKeyword)
        Public ReadOnly OrKeyword As SyntaxToken = Factory.Token(SyntaxKind.OrKeyword)
        Public ReadOnly OutKeyword As SyntaxToken = Factory.Token(SyntaxKind.OutKeyword)
        Public ReadOnly OverloadsKeyword As SyntaxToken = Factory.Token(SyntaxKind.OverloadsKeyword)
        Public ReadOnly OverridableKeyword As SyntaxToken = Factory.Token(SyntaxKind.OverridableKeyword)
        Public ReadOnly OverridesKeyword As SyntaxToken = Factory.Token(SyntaxKind.OverridesKeyword)
        Public ReadOnly ParamArrayKeyword As SyntaxToken = Factory.Token(SyntaxKind.ParamArrayKeyword)
        Public ReadOnly PartialKeyword As SyntaxToken = Factory.Token(SyntaxKind.PartialKeyword)
        Public ReadOnly PlusEqualsToken As SyntaxToken = Factory.Token(SyntaxKind.PlusEqualsToken)
        Public ReadOnly PlusToken As SyntaxToken = Factory.Token(SyntaxKind.PlusToken)
        Public ReadOnly PrivateKeyword As SyntaxToken = Factory.Token(SyntaxKind.PrivateKeyword)
        Public ReadOnly PropertyKeyword As SyntaxToken = Factory.Token(SyntaxKind.PropertyKeyword)
        Public ReadOnly ProtectedKeyword As SyntaxToken = Factory.Token(SyntaxKind.ProtectedKeyword)
        Public ReadOnly PublicKeyword As SyntaxToken = Factory.Token(SyntaxKind.PublicKeyword)
        Public ReadOnly QuestionToken As SyntaxToken = Factory.Token(SyntaxKind.QuestionToken)
        Public ReadOnly ReadOnlyKeyword As SyntaxToken = Factory.Token(SyntaxKind.ReadOnlyKeyword)
        Public ReadOnly RegionKeyword As SyntaxToken = Factory.Token(SyntaxKind.RegionKeyword)
        Public ReadOnly RemoveHandlerKeyword As SyntaxToken = Factory.Token(SyntaxKind.RemoveHandlerKeyword)
        Public ReadOnly SByteKeyword As SyntaxToken = Factory.Token(SyntaxKind.SByteKeyword)
        Public ReadOnly SelectKeyword As SyntaxToken = Factory.Token(SyntaxKind.SelectKeyword)
        Public ReadOnly SetKeyword As SyntaxToken = Factory.Token(SyntaxKind.SetKeyword)
        Public ReadOnly ShadowsKeyword As SyntaxToken = Factory.Token(SyntaxKind.ShadowsKeyword)
        Public ReadOnly SharedKeyword As SyntaxToken = Factory.Token(SyntaxKind.SharedKeyword)
        Public ReadOnly ShortKeyword As SyntaxToken = Factory.Token(SyntaxKind.ShortKeyword)
        Public ReadOnly SingleKeyword As SyntaxToken = Factory.Token(SyntaxKind.SingleKeyword)
        Public ReadOnly SlashEqualsToken As SyntaxToken = Factory.Token(SyntaxKind.SlashEqualsToken)
        Public ReadOnly SlashToken As SyntaxToken = Factory.Token(SyntaxKind.SlashToken)
        Public ReadOnly StringKeyword As SyntaxToken = Factory.Token(SyntaxKind.StringKeyword)
        Public ReadOnly StructureKeyword As SyntaxToken = Factory.Token(SyntaxKind.StructureKeyword)
        Public ReadOnly SubKeyword As SyntaxToken = Factory.Token(SyntaxKind.SubKeyword)
        Public ReadOnly SyncLockKeyword As SyntaxToken = Factory.Token(SyntaxKind.SyncLockKeyword)
        Public ReadOnly ThenKeyword As SyntaxToken = Factory.Token(SyntaxKind.ThenKeyword)
        Public ReadOnly ToKeyword As SyntaxToken = Factory.Token(SyntaxKind.ToKeyword)
        Public ReadOnly TrueKeyword As SyntaxToken = Factory.Token(SyntaxKind.TrueKeyword)
        Public ReadOnly TryCastKeyword As SyntaxToken = Factory.Token(SyntaxKind.TryCastKeyword)
        Public ReadOnly TryKeyword As SyntaxToken = Factory.Token(SyntaxKind.TryKeyword)
        Public ReadOnly TypeKeyword As SyntaxToken = Factory.Token(SyntaxKind.TypeKeyword)
        Public ReadOnly UIntegerKeyword As SyntaxToken = Factory.Token(SyntaxKind.UIntegerKeyword)
        Public ReadOnly ULongKeyword As SyntaxToken = Factory.Token(SyntaxKind.ULongKeyword)
        Public ReadOnly UShortKeyword As SyntaxToken = Factory.Token(SyntaxKind.UShortKeyword)
        Public ReadOnly UsingKeyword As SyntaxToken = Factory.Token(SyntaxKind.UsingKeyword)
        Public ReadOnly WhileKeyword As SyntaxToken = Factory.Token(SyntaxKind.WhileKeyword)
        Public ReadOnly WideningKeyword As SyntaxToken = Factory.Token(SyntaxKind.WideningKeyword)
        Public ReadOnly WithKeyword As SyntaxToken = Factory.Token(SyntaxKind.WithKeyword)
        Public ReadOnly WriteOnlyKeyword As SyntaxToken = Factory.Token(SyntaxKind.WriteOnlyKeyword)
        Public ReadOnly XorKeyword As SyntaxToken = Factory.Token(SyntaxKind.XorKeyword)

#Region "Options"

        Public ReadOnly BinaryToken As SyntaxToken = Factory.Token(SyntaxKind.BinaryKeyword)
        Public ReadOnly CompareToken As SyntaxToken = Factory.Token(SyntaxKind.CompareKeyword)
        Public ReadOnly ExplicitToken As SyntaxToken = Factory.Token(SyntaxKind.ExplicitKeyword)
        Public ReadOnly InferToken As SyntaxToken = Factory.Token(SyntaxKind.InferKeyword)
        Public ReadOnly OffToken As SyntaxToken = Factory.Token(SyntaxKind.OffKeyword)
        Public ReadOnly OnToken As SyntaxToken = Factory.Token(SyntaxKind.OnKeyword)
        Public ReadOnly StrictToken As SyntaxToken = Factory.Token(SyntaxKind.StrictKeyword)
        Public ReadOnly TextToken As SyntaxToken = Factory.Token(SyntaxKind.TextKeyword)

#End Region

#End Region

#Region "Predefined Types"

        Public ReadOnly HandleRefType As TypeSyntax = Factory.ParseTypeName("HandleRefType")
        Public ReadOnly IntPtrType As TypeSyntax = Factory.ParseTypeName("IntPtr")
        Public ReadOnly PredefinedTypeBoolean As TypeSyntax = Factory.PredefinedType(BooleanKeyword)
        Public ReadOnly PredefinedTypeByte As TypeSyntax = Factory.PredefinedType(ByteKeyword)
        Public ReadOnly PredefinedTypeChar As TypeSyntax = Factory.PredefinedType(CharKeyword)
        Public ReadOnly PredefinedTypeDate As TypeSyntax = Factory.PredefinedType(DateKeyword)
        Public ReadOnly PredefinedTypeDecimal As TypeSyntax = Factory.PredefinedType(DecimalKeyword)
        Public ReadOnly PredefinedTypeDouble As TypeSyntax = Factory.PredefinedType(DoubleKeyword)
        Public ReadOnly PredefinedTypeInteger As TypeSyntax = Factory.PredefinedType(IntegerKeyword)
        Public ReadOnly PredefinedTypeLong As TypeSyntax = Factory.PredefinedType(LongKeyword)
        Public ReadOnly PredefinedTypeObject As TypeSyntax = Factory.PredefinedType(ObjectKeyword)
        Public ReadOnly PredefinedTypeSByte As TypeSyntax = Factory.PredefinedType(SByteKeyword)
        Public ReadOnly PredefinedTypeShort As TypeSyntax = Factory.PredefinedType(ShortKeyword)
        Public ReadOnly PredefinedTypeSingle As TypeSyntax = Factory.PredefinedType(SingleKeyword)
        Public ReadOnly PredefinedTypeString As TypeSyntax = Factory.PredefinedType(StringKeyword)
        Public ReadOnly PredefinedTypeUInteger As TypeSyntax = Factory.PredefinedType(UIntegerKeyword)
        Public ReadOnly PredefinedTypeULong As TypeSyntax = Factory.PredefinedType(ULongKeyword)
        Public ReadOnly PredefinedTypeUShort As TypeSyntax = Factory.PredefinedType(UShortKeyword)
#End Region

#Region "trivia"

        Public ReadOnly LineContinuation As SyntaxTrivia = Factory.LineContinuationTrivia("_")
        Public ReadOnly VBEOLTrivia As SyntaxTrivia = Factory.EndOfLineTrivia(vbCrLf)

#End Region

#Region "Expressions"

        Public ReadOnly IntPrtSizeExpression As ExpressionSyntax = Factory.ParseExpression("IntPrt.Size")
        Public ReadOnly rightExpr As ExpressionSyntax = Factory.LiteralExpression(SyntaxKind.NumericLiteralExpression, Factory.Literal(1))
        Public ReadOnly NothingExpression As LiteralExpressionSyntax = Factory.NothingLiteralExpression(NothingKeyword)
        Public ReadOnly DoubleQuoteExpression As LiteralExpressionSyntax = Factory.LiteralExpression(SyntaxKind.StringLiteralExpression, Factory.Literal(""))

#End Region

        Public ReadOnly EndUsingStatement As EndBlockStatementSyntax = Factory.EndUsingStatement

        Public ReadOnly CompilerServices As String = "System.Runtime.CompilerServices"
        Public ReadOnly DimModifier As SyntaxTokenList = Factory.TokenList(DimKeyword.WithTrailingTrivia(Factory.Space))
        Public ReadOnly InteropServices As String = "System.Runtime.InteropServices"
        Public ReadOnly ExtensionAttribute As AttributeSyntax = Factory.Attribute(Nothing, Factory.ParseTypeName("Extension"), Factory.ArgumentList())
        Public ReadOnly ImportComilierServices As ImportsStatementSyntax = Factory.ImportsStatement(Factory.SingletonSeparatedList(Of ImportsClauseSyntax)(Factory.SimpleImportsClause(Factory.IdentifierName(CompilerServices)))).WithAppendedEOL
        Public ReadOnly ImportInteropServices As ImportsStatementSyntax = Factory.ImportsStatement(Factory.SingletonSeparatedList(Of ImportsClauseSyntax)(Factory.SimpleImportsClause(Factory.IdentifierName(InteropServices)))).WithAppendedEOL
        Public ReadOnly PublicModifier As SyntaxTokenList = Factory.TokenList(PublicKeyword)
        Public ReadOnly RuntimeInteropServicesOut As TypeSyntax = Factory.ParseTypeName("Out")
        Public ReadOnly ValueModifiedIdentifier As ModifiedIdentifierSyntax = Factory.ModifiedIdentifier("Value")

        Friend Function FactoryDimStatement(Declarator As VariableDeclaratorSyntax) As LocalDeclarationStatementSyntax
            Dim declarators As SeparatedSyntaxList(Of VariableDeclaratorSyntax) = Factory.SingletonSeparatedList(Declarator)
            Return FactoryDimStatement(declarators)
        End Function

        Friend Function FactoryDimStatement(Declarators As SeparatedSyntaxList(Of VariableDeclaratorSyntax)) As LocalDeclarationStatementSyntax
            Return Factory.LocalDeclarationStatement(DimModifier, Declarators).WithTrailingEOL
        End Function

        Friend Function FactoryDimStatement(Name As String, asClause As AsClauseSyntax, initializer As EqualsValueSyntax) As LocalDeclarationStatementSyntax
            Return FactoryDimStatement(Factory.identifier(Name), asClause, initializer)
        End Function

        Friend Function FactoryDimStatement(Name As SyntaxToken, asClause As AsClauseSyntax, initializer As EqualsValueSyntax) As LocalDeclarationStatementSyntax
            Dim modifiedIdentifier As ModifiedIdentifierSyntax = Factory.ModifiedIdentifier(Name).WithTrailingTrivia(Factory.Space)
            Dim names As SeparatedSyntaxList(Of ModifiedIdentifierSyntax) = Factory.SingletonSeparatedList(modifiedIdentifier)
            Dim declarator As VariableDeclaratorSyntax = Factory.VariableDeclarator(names, asClause, initializer)
            Return FactoryDimStatement(declarator)
        End Function

        Friend Function FactoryEndBlockStatement(EndBlockKind As SyntaxKind, BlockKeyword As SyntaxToken, finaltrivia As SyntaxTriviaList) As EndBlockStatementSyntax
            Return Factory.EndBlockStatement(EndBlockKind, EndKeyword.WithTrailingTrivia(Factory.Space), BlockKeyword).
                                                              WithAppendedTrailingTrivia(finaltrivia).
                                                              WithTrailingEOL
        End Function

        Friend Function FactoryTypeArgumentList(DictionaryTypeElement As List(Of TypeSyntax)) As TypeArgumentListSyntax
            Return Factory.TypeArgumentList(openParenToken, OfKeyword.WithTrailingTrivia(Factory.Space), Factory.SeparatedList(DictionaryTypeElement), CloseParenToken)
        End Function

#End If

    End Module
End Namespace
