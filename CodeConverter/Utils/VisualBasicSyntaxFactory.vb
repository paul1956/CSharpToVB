' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

' DO NOT REORDER DOCUMENT Tokens must be defined BEFORE they are used, the #If is require to prevent code cleanup
#If True Then

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax

Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory

Public Module VisualBasicSyntaxFactory

    Public Const Quote As String = """"
    Public Const DoubleQuote As String = """"""
    Public Const SystemString As SpecialType = SpecialType.System_String
    Public Const UnicodeOpenQuote As Char = ChrW(&H201C)
    Public Const UnicodeCloseQuote As Char = ChrW(&H201D)
    Public Const UnicodeDoubleOpenQuote As String = UnicodeOpenQuote & UnicodeOpenQuote
    Public Const UnicodeDoubleCloseQuote As String = UnicodeCloseQuote & UnicodeCloseQuote
    Public Const UnicodeFullWidthQuoationMark As Char = ChrW(&HFF02)

#Region "Token"

    Public ReadOnly AddressOfKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.AddressOfKeyword)
    Public ReadOnly AmpersandToken As SyntaxToken = VBFactory.Token(SyntaxKind.AmpersandToken)
    Public ReadOnly AndAlsoKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.AndAlsoKeyword)
    Public ReadOnly AndKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.AndKeyword)
    Public ReadOnly AsKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.AsKeyword)
    Public ReadOnly AssemblyKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.AssemblyKeyword)
    Public ReadOnly AsterickToken As SyntaxToken = VBFactory.Token(SyntaxKind.AsteriskToken)
    Public ReadOnly AsteriskEqualsToken As SyntaxToken = VBFactory.Token(SyntaxKind.AsteriskEqualsToken)
    Public ReadOnly AsteriskToken As SyntaxToken = VBFactory.Token(SyntaxKind.AsteriskToken)
    Public ReadOnly AsyncKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.AsyncKeyword)
    Public ReadOnly AwaitKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.AwaitKeyword)
    Public ReadOnly BeginCDataToken As SyntaxToken = VBFactory.Token(SyntaxKind.BeginCDataToken)
    Public ReadOnly BlockKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.OperatorKeyword)
    Public ReadOnly BooleanKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.BooleanKeyword)
    Public ReadOnly ByRefKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.ByRefKeyword)
    Public ReadOnly ByteKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.ByteKeyword)
    Public ReadOnly ByValKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.ByValKeyword)
    Public ReadOnly CaseKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.CaseKeyword)
    Public ReadOnly CBoolKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.CBoolKeyword)
    Public ReadOnly CByteKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.CByteKeyword)
    Public ReadOnly CCharKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.CCharKeyword)
    Public ReadOnly CDateKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.CDateKeyword)
    Public ReadOnly CDblKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.CDblKeyword)
    Public ReadOnly CDecKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.CDecKeyword)
    Public ReadOnly CharKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.CharKeyword)
    Public ReadOnly CIntKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.CIntKeyword)
    Public ReadOnly ClassKeyWord As SyntaxToken = VBFactory.Token(SyntaxKind.ClassKeyword)
    Public ReadOnly CLngKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.CLngKeyword)
    Public ReadOnly CloseBraceToken As SyntaxToken = VBFactory.Token(SyntaxKind.CloseBraceToken)
    Public ReadOnly CloseParenToken As SyntaxToken = VBFactory.Token(SyntaxKind.CloseParenToken)
    Public ReadOnly CObjKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.CObjKeyword)
    Public ReadOnly CommaToken As SyntaxToken = VBFactory.Token(SyntaxKind.CommaToken)
    Public ReadOnly ConstKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.ConstKeyword)
    Public ReadOnly CSByteKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.CSByteKeyword)
    Public ReadOnly CShortKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.CShortKeyword)
    Public ReadOnly CSngKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.CSngKeyword)
    Public ReadOnly CStrKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.CStrKeyword)
    Public ReadOnly CTypeKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.CTypeKeyword)
    Public ReadOnly CUIntKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.CUIntKeyword)
    Public ReadOnly CULngKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.CULngKeyword)
    Public ReadOnly CUShortKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.CUShortKeyword)
    Public ReadOnly CustomKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.CustomKeyword)
    Public ReadOnly DateKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.DateKeyword)
    Public ReadOnly DecimalKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.DecimalKeyword)
    Public ReadOnly DefaultKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.DefaultKeyword)
    Public ReadOnly DimKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.DimKeyword)
    Public ReadOnly DoKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.DoKeyword)
    Public ReadOnly DotToken As SyntaxToken = VBFactory.Token(SyntaxKind.DotToken)
    Public ReadOnly DoubleKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.DoubleKeyword)
    Public ReadOnly DoubleQuoteToken As SyntaxToken = VBFactory.Token(SyntaxKind.DoubleQuoteToken)
    Public ReadOnly ElseIfKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.ElseIfKeyword)
    Public ReadOnly ElseKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.ElseKeyword)
    Public ReadOnly EmptyToken As SyntaxToken = VBFactory.Token(SyntaxKind.EmptyToken)
    Public ReadOnly EndCDataToken As SyntaxToken = VBFactory.Token(SyntaxKind.EndCDataToken)
    Public ReadOnly EndKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.EndKeyword)
    Public ReadOnly EndOfFileToken As SyntaxToken = VBFactory.Token(SyntaxKind.EndOfFileToken)
    Public ReadOnly EnumKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.EnumKeyword)
    Public ReadOnly EqualsToken As SyntaxToken = VBFactory.Token(SyntaxKind.EqualsToken)
    Public ReadOnly ExternalChecksumKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.ExternalChecksumKeyword)
    Public ReadOnly ExternalSourceKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.ExternalSourceKeyword)
    Public ReadOnly FalseKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.FalseKeyword)
    Public ReadOnly ForKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.ForKeyword)
    Public ReadOnly FriendKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.FriendKeyword)
    Public ReadOnly FromKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.FromKeyword)
    Public ReadOnly FunctionKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.FunctionKeyword)
    Public ReadOnly GetKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.GetKeyword)
    Public ReadOnly GreaterThanEqualsToken As SyntaxToken = VBFactory.Token(SyntaxKind.GreaterThanEqualsToken)
    Public ReadOnly GreaterThanGreaterThanEqualsToken As SyntaxToken = VBFactory.Token(SyntaxKind.GreaterThanGreaterThanEqualsToken)
    Public ReadOnly GreaterThanGreaterThanToken As SyntaxToken = VBFactory.Token(SyntaxKind.GreaterThanGreaterThanToken)
    Public ReadOnly GreaterThanToken As SyntaxToken = VBFactory.Token(SyntaxKind.GreaterThanToken)
    Public ReadOnly HashToken As SyntaxToken = VBFactory.Token(SyntaxKind.HashToken)
    Public ReadOnly IfKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.IfKeyword)
    Public ReadOnly InKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.InKeyword)
    Public ReadOnly IntegerKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.IntegerKeyword)
    Public ReadOnly InterfaceKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.InterfaceKeyword)
    Public ReadOnly IsFalse As SyntaxToken = VBFactory.Token(SyntaxKind.IsFalseKeyword)
    Public ReadOnly IsTrueKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.IsTrueKeyword)
    Public ReadOnly IteratorKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.IteratorKeyword)
    Public ReadOnly KeyKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.KeyKeyword)
    Public ReadOnly LessThanEqualsToken As SyntaxToken = VBFactory.Token(SyntaxKind.LessThanEqualsToken)
    Public ReadOnly LessThanGreaterThanToken As SyntaxToken = VBFactory.Token(SyntaxKind.LessThanGreaterThanToken)
    Public ReadOnly LessThanLessThanEqualsToken As SyntaxToken = VBFactory.Token(SyntaxKind.LessThanLessThanEqualsToken)
    Public ReadOnly LessThanLessThanToken As SyntaxToken = VBFactory.Token(SyntaxKind.LessThanLessThanToken)
    Public ReadOnly LessThanToken As SyntaxToken = VBFactory.Token(SyntaxKind.LessThanToken)
    Public ReadOnly LongKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.LongKeyword)
    Public ReadOnly MeKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.MeKeyword)
    Public ReadOnly MinusEqualsToken As SyntaxToken = VBFactory.Token(SyntaxKind.MinusEqualsToken)
    Public ReadOnly MinusToken As SyntaxToken = VBFactory.Token(SyntaxKind.MinusToken)
    Public ReadOnly ModKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.ModKeyword)
    Public ReadOnly ModuleKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.ModuleKeyword)
    Public ReadOnly MustInheritKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.MustInheritKeyword)
    Public ReadOnly MustOverrideKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.MustOverrideKeyword)
    Public ReadOnly MyBaseKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.MyBaseKeyword)
    Public ReadOnly NamespaceKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.NamespaceKeyword)
    Public ReadOnly NarrowingKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.NarrowingKeyword)
    Public ReadOnly NewKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.NewKeyword)
    Public ReadOnly NothingKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.NothingKeyword)
    Public ReadOnly NotInheritableKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.NotInheritableKeyword)
    Public ReadOnly NotKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.NotKeyword)
    Public ReadOnly NotOverridableKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.NotOverridableKeyword)
    Public ReadOnly ObjectKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.ObjectKeyword)
    Public ReadOnly OfKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.OfKeyword)
    Public ReadOnly OpenBraceToken As SyntaxToken = VBFactory.Token(SyntaxKind.OpenBraceToken)
    Public ReadOnly OpenParenToken As SyntaxToken = VBFactory.Token(SyntaxKind.OpenParenToken)
    Public ReadOnly OptionalKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.OptionalKeyword)
    Public ReadOnly OrElseKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.OrElseKeyword)
    Public ReadOnly OrKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.OrKeyword)
    Public ReadOnly OutKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.OutKeyword)
    Public ReadOnly OverridableKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.OverridableKeyword)
    Public ReadOnly OverridesKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.OverridesKeyword)
    Public ReadOnly ParamArrayKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.ParamArrayKeyword)
    Public ReadOnly PartialKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.PartialKeyword)
    Public ReadOnly PlusEqualsToken As SyntaxToken = VBFactory.Token(SyntaxKind.PlusEqualsToken)
    Public ReadOnly PlusToken As SyntaxToken = VBFactory.Token(SyntaxKind.PlusToken)
    Public ReadOnly PrivateKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.PrivateKeyword)
    Public ReadOnly PropertyKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.PropertyKeyword)
    Public ReadOnly ProtectedKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.ProtectedKeyword)
    Public ReadOnly PublicKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.PublicKeyword)
    Public ReadOnly QuestionToken As SyntaxToken = VBFactory.Token(SyntaxKind.QuestionToken)
    Public ReadOnly ReadOnlyKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.ReadOnlyKeyword)
    Public ReadOnly RegionKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.RegionKeyword)
    Public ReadOnly SByteKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.SByteKeyword)
    Public ReadOnly SelectKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.SelectKeyword)
    Public ReadOnly SetKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.SetKeyword)
    Public ReadOnly ShadowsKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.ShadowsKeyword)
    Public ReadOnly SharedKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.SharedKeyword)
    Public ReadOnly ShortKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.ShortKeyword)
    Public ReadOnly SingleKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.SingleKeyword)
    Public ReadOnly SlashEqualsToken As SyntaxToken = VBFactory.Token(SyntaxKind.SlashEqualsToken)
    Public ReadOnly SlashToken As SyntaxToken = VBFactory.Token(SyntaxKind.SlashToken)
    Public ReadOnly StringKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.StringKeyword)
    Public ReadOnly StructureKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.StructureKeyword)
    Public ReadOnly SubKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.SubKeyword)
    Public ReadOnly ThenKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.ThenKeyword)
    Public ReadOnly ToKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.ToKeyword)
    Public ReadOnly TrueKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.TrueKeyword)
    Public ReadOnly TryCastKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.TryCastKeyword)
    Public ReadOnly TryKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.TryKeyword)
    Public ReadOnly TypeKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.TypeKeyword)
    Public ReadOnly UIntegerKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.UIntegerKeyword)
    Public ReadOnly ULongKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.ULongKeyword)
    Public ReadOnly WhileKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.WhileKeyword)
    Public ReadOnly WideningKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.WideningKeyword)
    Public ReadOnly WithKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.WithKeyword)
    Public ReadOnly WriteOnlyKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.WriteOnlyKeyword)
    Public ReadOnly XorKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.XorKeyword)
    Public ReadOnly UShortKeyword As SyntaxToken = VBFactory.Token(SyntaxKind.UShortKeyword)

#Region "Options"

    Public ReadOnly BinaryToken As SyntaxToken = VBFactory.Token(SyntaxKind.BinaryKeyword)
    Public ReadOnly CompareToken As SyntaxToken = VBFactory.Token(SyntaxKind.CompareKeyword)
    Public ReadOnly ExplicitToken As SyntaxToken = VBFactory.Token(SyntaxKind.ExplicitKeyword)
    Public ReadOnly InferToken As SyntaxToken = VBFactory.Token(SyntaxKind.InferKeyword)
    Public ReadOnly OffToken As SyntaxToken = VBFactory.Token(SyntaxKind.OffKeyword)
    Public ReadOnly OnToken As SyntaxToken = VBFactory.Token(SyntaxKind.OnKeyword)
    Public ReadOnly StrictToken As SyntaxToken = VBFactory.Token(SyntaxKind.StrictKeyword)
    Public ReadOnly TextToken As SyntaxToken = VBFactory.Token(SyntaxKind.TextKeyword)

#End Region

#End Region

#Region "Predefined Types"

    Public ReadOnly IntPtrType As TypeSyntax = VBFactory.ParseTypeName("IntPtr")
    Public ReadOnly PredefinedTypeBoolean As TypeSyntax = VBFactory.PredefinedType(BooleanKeyword)
    Public ReadOnly PredefinedTypeByte As TypeSyntax = VBFactory.PredefinedType(ByteKeyword)
    Public ReadOnly PredefinedTypeChar As TypeSyntax = VBFactory.PredefinedType(CharKeyword)
    Public ReadOnly PredefinedTypeDate As TypeSyntax = VBFactory.PredefinedType(DateKeyword)
    Public ReadOnly PredefinedTypeDecimal As TypeSyntax = VBFactory.PredefinedType(DecimalKeyword)
    Public ReadOnly PredefinedTypeDouble As TypeSyntax = VBFactory.PredefinedType(DoubleKeyword)
    Public ReadOnly PredefinedTypeInteger As TypeSyntax = VBFactory.PredefinedType(IntegerKeyword)
    Public ReadOnly PredefinedTypeLong As TypeSyntax = VBFactory.PredefinedType(LongKeyword)
    Public ReadOnly PredefinedTypeObject As TypeSyntax = VBFactory.PredefinedType(ObjectKeyword)
    Public ReadOnly PredefinedTypeSByte As TypeSyntax = VBFactory.PredefinedType(SByteKeyword)
    Public ReadOnly PredefinedTypeShort As TypeSyntax = VBFactory.PredefinedType(ShortKeyword)
    Public ReadOnly PredefinedTypeSingle As TypeSyntax = VBFactory.PredefinedType(SingleKeyword)
    Public ReadOnly PredefinedTypeString As TypeSyntax = VBFactory.PredefinedType(StringKeyword)
    Public ReadOnly PredefinedTypeUInteger As TypeSyntax = VBFactory.PredefinedType(UIntegerKeyword)
    Public ReadOnly PredefinedTypeULong As TypeSyntax = VBFactory.PredefinedType(ULongKeyword)
    Public ReadOnly PredefinedTypeUShort As TypeSyntax = VBFactory.PredefinedType(UShortKeyword)

#End Region

#Region "Trivia"

    Public ReadOnly ElasticMarker As SyntaxTrivia = VBFactory.ElasticWhitespace(String.Empty)
    Public ReadOnly LineContinuation As SyntaxTrivia = VBFactory.LineContinuationTrivia("_")
    Public ReadOnly SpaceTrivia As SyntaxTrivia = VBFactory.Space
    Public ReadOnly VBEOLTrivia As SyntaxTrivia = VBFactory.EndOfLineTrivia(vbCrLf)

#End Region

#Region "Expressions"

    Public ReadOnly IntPrtSizeExpression As ExpressionSyntax = VBFactory.ParseExpression("IntPrt.Size")
    Public ReadOnly ExpressionD1 As ExpressionSyntax = VBFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, VBFactory.Literal(1))
    Public ReadOnly NothingExpression As LiteralExpressionSyntax = VBFactory.NothingLiteralExpression(NothingKeyword)
    Public ReadOnly VBCrLfExpression As IdentifierNameSyntax = VBFactory.IdentifierName("vbCrLf")

#End Region

    Public ReadOnly DimModifier As SyntaxTokenList = VBFactory.TokenList(DimKeyword)
    Public ReadOnly ValueModifiedIdentifier As ModifiedIdentifierSyntax = VBFactory.ModifiedIdentifier("Value")
    Public ReadOnly PublicModifier As SyntaxTokenList = VBFactory.TokenList(PublicKeyword)
    Public ReadOnly RuntimeInteropServicesOut As TypeSyntax = VBFactory.ParseTypeName("Runtime.InteropServices.Out")

    Public Function FactoryDimStatement(name As String, asClause As AsClauseSyntax, initializer As EqualsValueSyntax) As LocalDeclarationStatementSyntax
        Return FactoryDimStatement(VBFactory.Identifier(name), asClause, initializer)
    End Function

    Public Function FactoryDimStatement(name As SyntaxToken, asClause As AsClauseSyntax, initializer As EqualsValueSyntax) As LocalDeclarationStatementSyntax
        Dim modifiedIdentifier As ModifiedIdentifierSyntax = VBFactory.ModifiedIdentifier(name).WithTrailingTrivia(SpaceTrivia)
        Dim names As SeparatedSyntaxList(Of ModifiedIdentifierSyntax) = VBFactory.SingletonSeparatedList(modifiedIdentifier)
        Dim modifiers As SyntaxTokenList = VBFactory.TokenList(DimKeyword)
        Dim declarator As VariableDeclaratorSyntax = VBFactory.VariableDeclarator(names, asClause, initializer)
        Dim declarators As SeparatedSyntaxList(Of VariableDeclaratorSyntax) = VBFactory.SingletonSeparatedList(declarator)
        Return VBFactory.LocalDeclarationStatement(modifiers, declarators)
    End Function

#End If

End Module
