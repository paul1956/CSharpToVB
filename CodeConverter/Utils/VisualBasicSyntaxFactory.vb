' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

' DO NOT REORDER DOCUMENT Tokens must be defined BEFORE they are used, the #If is require to prevent code cleanup
#If True Then

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.VisualBasic

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

    Public ReadOnly AddressOfKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.AddressOfKeyword)
    Public ReadOnly AmpersandToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.AmpersandToken)
    Public ReadOnly AndAlsoKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.AndAlsoKeyword)
    Public ReadOnly AndKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.AndKeyword)
    Public ReadOnly AsKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.AsKeyword)
    Public ReadOnly AssemblyKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.AssemblyKeyword)
    Public ReadOnly AsterickToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.AsteriskToken)
    Public ReadOnly AsteriskEqualsToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.AsteriskEqualsToken)
    Public ReadOnly AsteriskToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.AsteriskToken)
    Public ReadOnly AsyncKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.AsyncKeyword)
    Public ReadOnly AwaitKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.AwaitKeyword)
    Public ReadOnly BeginCDataToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.BeginCDataToken)
    Public ReadOnly BlockKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.OperatorKeyword)
    Public ReadOnly BooleanKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.BooleanKeyword)
    Public ReadOnly ByRefKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.ByRefKeyword)
    Public ReadOnly ByteKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.ByteKeyword)
    Public ReadOnly ByValKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.ByValKeyword)
    Public ReadOnly CaseKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.CaseKeyword)
    Public ReadOnly CBoolKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.CBoolKeyword)
    Public ReadOnly CByteKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.CByteKeyword)
    Public ReadOnly CCharKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.CCharKeyword)
    Public ReadOnly CDateKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.CDateKeyword)
    Public ReadOnly CDblKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.CDblKeyword)
    Public ReadOnly CDecKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.CDecKeyword)
    Public ReadOnly CharKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.CharKeyword)
    Public ReadOnly CIntKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.CIntKeyword)
    Public ReadOnly ClassKeyWord As SyntaxToken = SyntaxFactory.Token(SyntaxKind.ClassKeyword)
    Public ReadOnly CLngKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.CLngKeyword)
    Public ReadOnly CloseBraceToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.CloseBraceToken)
    Public ReadOnly CloseParenToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.CloseParenToken)
    Public ReadOnly CObjKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.CObjKeyword)
    Public ReadOnly CommaToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.CommaToken)
    Public ReadOnly ConstKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.ConstKeyword)
    Public ReadOnly CSByteKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.CSByteKeyword)
    Public ReadOnly CShortKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.CShortKeyword)
    Public ReadOnly CSngKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.CSngKeyword)
    Public ReadOnly CStrKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.CStrKeyword)
    Public ReadOnly CTypeKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.CTypeKeyword)
    Public ReadOnly CUIntKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.CUIntKeyword)
    Public ReadOnly CULngKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.CULngKeyword)
    Public ReadOnly CUShortKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.CUShortKeyword)
    Public ReadOnly CustomKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.CustomKeyword)
    Public ReadOnly DateKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.DateKeyword)
    Public ReadOnly DecimalKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.DecimalKeyword)
    Public ReadOnly DefaultKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.DefaultKeyword)
    Public ReadOnly DimKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.DimKeyword)
    Public ReadOnly DoKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.DoKeyword)
    Public ReadOnly DotToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.DotToken)
    Public ReadOnly DoubleKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.DoubleKeyword)
    Public ReadOnly DoubleQuoteToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.DoubleQuoteToken)
    Public ReadOnly ElseIfKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.ElseIfKeyword)
    Public ReadOnly ElseKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.ElseKeyword)
    Public ReadOnly EmptyToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.EmptyToken)
    Public ReadOnly EndCDataToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.EndCDataToken)
    Public ReadOnly EndKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.EndKeyword)
    Public ReadOnly EndOfFileToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.EndOfFileToken)
    Public ReadOnly EnumKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.EnumKeyword)
    Public ReadOnly EqualsToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.EqualsToken)
    Public ReadOnly ExternalChecksumKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.ExternalChecksumKeyword)
    Public ReadOnly ExternalSourceKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.ExternalSourceKeyword)
    Public ReadOnly FalseKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.FalseKeyword)
    Public ReadOnly ForKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.ForKeyword)
    Public ReadOnly FriendKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.FriendKeyword)
    Public ReadOnly FromKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.FromKeyword)
    Public ReadOnly FunctionKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.FunctionKeyword)
    Public ReadOnly GetKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.GetKeyword)
    Public ReadOnly GreaterThanEqualsToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.GreaterThanEqualsToken)
    Public ReadOnly GreaterThanGreaterThanEqualsToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.GreaterThanGreaterThanEqualsToken)
    Public ReadOnly GreaterThanGreaterThanToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.GreaterThanGreaterThanToken)
    Public ReadOnly GreaterThanToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.GreaterThanToken)
    Public ReadOnly HashToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.HashToken)
    Public ReadOnly IfKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.IfKeyword)
    Public ReadOnly InKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.InKeyword)
    Public ReadOnly IntegerKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.IntegerKeyword)
    Public ReadOnly InterfaceKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.InterfaceKeyword)
    Public ReadOnly IsFalse As SyntaxToken = SyntaxFactory.Token(SyntaxKind.IsFalseKeyword)
    Public ReadOnly IsTrueKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.IsTrueKeyword)
    Public ReadOnly IteratorKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.IteratorKeyword)
    Public ReadOnly KeyKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.KeyKeyword)
    Public ReadOnly LessThanEqualsToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.LessThanEqualsToken)
    Public ReadOnly LessThanGreaterThanToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.LessThanGreaterThanToken)
    Public ReadOnly LessThanLessThanEqualsToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.LessThanLessThanEqualsToken)
    Public ReadOnly LessThanLessThanToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.LessThanLessThanToken)
    Public ReadOnly LessThanToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.LessThanToken)
    Public ReadOnly LongKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.LongKeyword)
    Public ReadOnly MeKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.MeKeyword)
    Public ReadOnly MinusEqualsToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.MinusEqualsToken)
    Public ReadOnly MinusToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.MinusToken)
    Public ReadOnly ModKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.ModKeyword)
    Public ReadOnly ModuleKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.ModuleKeyword)
    Public ReadOnly MustInheritKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.MustInheritKeyword)
    Public ReadOnly MustOverrideKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.MustOverrideKeyword)
    Public ReadOnly MyBaseKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.MyBaseKeyword)
    Public ReadOnly NamespaceKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.NamespaceKeyword)
    Public ReadOnly NarrowingKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.NarrowingKeyword)
    Public ReadOnly NewKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.NewKeyword)
    Public ReadOnly NothingKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.NothingKeyword)
    Public ReadOnly NotInheritableKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.NotInheritableKeyword)
    Public ReadOnly NotKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.NotKeyword)
    Public ReadOnly NotOverridableKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.NotOverridableKeyword)
    Public ReadOnly ObjectKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.ObjectKeyword)
    Public ReadOnly OfKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.OfKeyword)
    Public ReadOnly OpenBraceToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.OpenBraceToken)
    Public ReadOnly OpenParenToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.OpenParenToken)
    Public ReadOnly OptionalKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.OptionalKeyword)
    Public ReadOnly OrElseKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.OrElseKeyword)
    Public ReadOnly OrKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.OrKeyword)
    Public ReadOnly OutKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.OutKeyword)
    Public ReadOnly OverridableKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.OverridableKeyword)
    Public ReadOnly OverridesKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.OverridesKeyword)
    Public ReadOnly ParamArrayKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.ParamArrayKeyword)
    Public ReadOnly PartialKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.PartialKeyword)
    Public ReadOnly PlusEqualsToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.PlusEqualsToken)
    Public ReadOnly PlusToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.PlusToken)
    Public ReadOnly PrivateKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.PrivateKeyword)
    Public ReadOnly PropertyKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.PropertyKeyword)
    Public ReadOnly ProtectedKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.ProtectedKeyword)
    Public ReadOnly PublicKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.PublicKeyword)
    Public ReadOnly QuestionToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.QuestionToken)
    Public ReadOnly ReadOnlyKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.ReadOnlyKeyword)
    Public ReadOnly RegionKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.RegionKeyword)
    Public ReadOnly SByteKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.SByteKeyword)
    Public ReadOnly SelectKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.SelectKeyword)
    Public ReadOnly SetKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.SetKeyword)
    Public ReadOnly ShadowsKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.ShadowsKeyword)
    Public ReadOnly SharedKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.SharedKeyword)
    Public ReadOnly ShortKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.ShortKeyword)
    Public ReadOnly SingleKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.SingleKeyword)
    Public ReadOnly SlashEqualsToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.SlashEqualsToken)
    Public ReadOnly SlashToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.SlashToken)
    Public ReadOnly StringKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.StringKeyword)
    Public ReadOnly StructureKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.StructureKeyword)
    Public ReadOnly SubKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.SubKeyword)
    Public ReadOnly ThenKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.ThenKeyword)
    Public ReadOnly ToKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.ToKeyword)
    Public ReadOnly TrueKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.TrueKeyword)
    Public ReadOnly TryCastKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.TryCastKeyword)
    Public ReadOnly TryKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.TryKeyword)
    Public ReadOnly TypeKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.TypeKeyword)
    Public ReadOnly UIntegerKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.UIntegerKeyword)
    Public ReadOnly ULongKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.ULongKeyword)
    Public ReadOnly WhileKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.WhileKeyword)
    Public ReadOnly WideningKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.WideningKeyword)
    Public ReadOnly WithKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.WithKeyword)
    Public ReadOnly WriteOnlyKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.WriteOnlyKeyword)
    Public ReadOnly XorKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.XorKeyword)
    Public ReadOnly UShortKeyword As SyntaxToken = SyntaxFactory.Token(SyntaxKind.UShortKeyword)

#Region "Options"

    Public ReadOnly BinaryToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.BinaryKeyword)
    Public ReadOnly CompareToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.CompareKeyword)
    Public ReadOnly ExplicitToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.ExplicitKeyword)
    Public ReadOnly InferToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.InferKeyword)
    Public ReadOnly OffToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.OffKeyword)
    Public ReadOnly OnToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.OnKeyword)
    Public ReadOnly StrictToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.StrictKeyword)
    Public ReadOnly TextToken As SyntaxToken = SyntaxFactory.Token(SyntaxKind.TextKeyword)

#End Region

#End Region

#Region "Predefined Types"

    Public ReadOnly HandleRefType As Syntax.TypeSyntax = SyntaxFactory.ParseTypeName("HandleRefType")
    Public ReadOnly IntPtrType As Syntax.TypeSyntax = SyntaxFactory.ParseTypeName("IntPtr")
    Public ReadOnly PredefinedTypeBoolean As Syntax.TypeSyntax = SyntaxFactory.PredefinedType(BooleanKeyword)
    Public ReadOnly PredefinedTypeByte As Syntax.TypeSyntax = SyntaxFactory.PredefinedType(ByteKeyword)
    Public ReadOnly PredefinedTypeChar As Syntax.TypeSyntax = SyntaxFactory.PredefinedType(CharKeyword)
    Public ReadOnly PredefinedTypeDate As Syntax.TypeSyntax = SyntaxFactory.PredefinedType(DateKeyword)
    Public ReadOnly PredefinedTypeDecimal As Syntax.TypeSyntax = SyntaxFactory.PredefinedType(DecimalKeyword)
    Public ReadOnly PredefinedTypeDouble As Syntax.TypeSyntax = SyntaxFactory.PredefinedType(DoubleKeyword)
    Public ReadOnly PredefinedTypeInteger As Syntax.TypeSyntax = SyntaxFactory.PredefinedType(IntegerKeyword)
    Public ReadOnly PredefinedTypeLong As Syntax.TypeSyntax = SyntaxFactory.PredefinedType(LongKeyword)
    Public ReadOnly PredefinedTypeObject As Syntax.TypeSyntax = SyntaxFactory.PredefinedType(ObjectKeyword)
    Public ReadOnly PredefinedTypeSByte As Syntax.TypeSyntax = SyntaxFactory.PredefinedType(SByteKeyword)
    Public ReadOnly PredefinedTypeShort As Syntax.TypeSyntax = SyntaxFactory.PredefinedType(ShortKeyword)
    Public ReadOnly PredefinedTypeSingle As Syntax.TypeSyntax = SyntaxFactory.PredefinedType(SingleKeyword)
    Public ReadOnly PredefinedTypeString As Syntax.TypeSyntax = SyntaxFactory.PredefinedType(StringKeyword)
    Public ReadOnly PredefinedTypeUInteger As Syntax.TypeSyntax = SyntaxFactory.PredefinedType(UIntegerKeyword)
    Public ReadOnly PredefinedTypeULong As Syntax.TypeSyntax = SyntaxFactory.PredefinedType(ULongKeyword)
    Public ReadOnly PredefinedTypeUShort As Syntax.TypeSyntax = SyntaxFactory.PredefinedType(UShortKeyword)

#End Region

#Region "Trivia"

    Public ReadOnly ElasticMarker As SyntaxTrivia = SyntaxFactory.ElasticWhitespace(String.Empty)
    Public ReadOnly LineContinuation As SyntaxTrivia = SyntaxFactory.LineContinuationTrivia("_")
    Public ReadOnly SpaceTrivia As SyntaxTrivia = SyntaxFactory.Space
    Public ReadOnly VBEOLTrivia As SyntaxTrivia = SyntaxFactory.EndOfLineTrivia(vbCrLf)

#End Region

#Region "Expressions"

    Public ReadOnly IntPrtSizeExpression As Syntax.ExpressionSyntax = SyntaxFactory.ParseExpression("IntPrt.Size")
    Public ReadOnly ExpressionD1 As Syntax.ExpressionSyntax = SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(1))
    Public ReadOnly NothingExpression As Syntax.LiteralExpressionSyntax = SyntaxFactory.NothingLiteralExpression(NothingKeyword)
    Public ReadOnly VBCrLfExpression As Syntax.IdentifierNameSyntax = SyntaxFactory.IdentifierName("vbCrLf")

#End Region

    Public ReadOnly DimModifier As SyntaxTokenList = SyntaxFactory.TokenList(DimKeyword)
    Public ReadOnly ValueModifiedIdentifier As Syntax.ModifiedIdentifierSyntax = SyntaxFactory.ModifiedIdentifier("Value")
    Public ReadOnly PublicModifier As SyntaxTokenList = SyntaxFactory.TokenList(PublicKeyword)
    Public ReadOnly RuntimeInteropServicesOut As Syntax.TypeSyntax = SyntaxFactory.ParseTypeName("Runtime.InteropServices.Out")
#End If
End Module
