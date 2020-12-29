' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Diagnostics.CodeAnalysis

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBConverter.ToVisualBasic

    Friend Class XMLVisitor
        Inherits CS.CSharpSyntaxVisitor(Of VB.VisualBasicSyntaxNode)

        Private Shared Function GetVBOperatorToken(op As String) As SyntaxToken
            Select Case op
                Case "=="
                    Return EqualsToken
                Case "!="
                    Return LessThanGreaterThanToken
                Case ">"
                    Return GreaterThanToken
                Case ">="
                    Return GreaterThanEqualsToken
                Case "<"
                    Return LessThanToken
                Case "<="
                    Return LessThanEqualsToken
                Case "|"
                    Return OrKeyword
                Case "||"
                    Return OrElseKeyword
                Case "&"
                    Return AndKeyword
                Case "&&"
                    Return AndAlsoKeyword
                Case "+"
                    Return PlusToken
                Case "-"
                    Return MinusToken
                Case "*"
                    Return AsteriskToken
                Case "/"
                    Return SlashToken
                Case "%"
                    Return ModKeyword
                Case "="
                    Return EqualsToken
                Case "+="
                    Return PlusEqualsToken
                Case "-="
                    Return MinusEqualsToken
                Case "!"
                    Return NotKeyword
                Case "~"
                    Return NotKeyword
                Case Else

            End Select

            Throw New ArgumentOutOfRangeException(NameOf(op))
        End Function

        Private Function GatherAttributes(listOfAttributes As SyntaxList(Of CSS.XmlAttributeSyntax)) As SyntaxList(Of VBS.XmlNodeSyntax)
            Dim vbAttributes As New SyntaxList(Of VBS.XmlNodeSyntax)
            For Each a As CSS.XmlAttributeSyntax In listOfAttributes
                Dim attribute As VBS.XmlNodeSyntax = DirectCast(a.Accept(Me), VBS.XmlNodeSyntax).WithConvertedTriviaFrom(a)
                vbAttributes = vbAttributes.Add(attribute)
            Next
            Return vbAttributes
        End Function

        <ExcludeFromCodeCoverage>
        Public Overrides Function DefaultVisit(node As SyntaxNode) As VB.VisualBasicSyntaxNode
            Return MyBase.DefaultVisit(node)
        End Function

        Public Overrides Function VisitConversionOperatorMemberCref(node As CSS.ConversionOperatorMemberCrefSyntax) As VB.VisualBasicSyntaxNode
            Return MyBase.VisitConversionOperatorMemberCref(node)
        End Function

        Public Overrides Function VisitCrefBracketedParameterList(node As CSS.CrefBracketedParameterListSyntax) As VB.VisualBasicSyntaxNode
            Return MyBase.VisitCrefBracketedParameterList(node)
        End Function

        Public Overrides Function VisitCrefParameter(node As CSS.CrefParameterSyntax) As VB.VisualBasicSyntaxNode
            Return node.Type.Accept(Me)
        End Function

        Public Overrides Function VisitCrefParameterList(node As CSS.CrefParameterListSyntax) As VB.VisualBasicSyntaxNode
            Return MyBase.VisitCrefParameterList(node)
        End Function

        Public Overrides Function VisitGenericName(node As CSS.GenericNameSyntax) As VB.VisualBasicSyntaxNode
            Dim identifier As SyntaxToken = Factory.Identifier(node.Identifier.ToString)
            Dim typeList As New List(Of VBS.TypeSyntax)
            For Each a As CSS.TypeSyntax In node.TypeArgumentList.Arguments
                typeList.Add(DirectCast(a.Accept(Me), VBS.TypeSyntax))
            Next
            Return Factory.GenericName(identifier, FactoryTypeArgumentList(typeList))
        End Function

        Public Overrides Function VisitIdentifierName(node As CSS.IdentifierNameSyntax) As VB.VisualBasicSyntaxNode
            Dim identifier As VBS.IdentifierNameSyntax = Factory.IdentifierName(node.Identifier.ToString)
            Return identifier
        End Function

        Public Overrides Function VisitNameMemberCref(node As CSS.NameMemberCrefSyntax) As VB.VisualBasicSyntaxNode
            Dim name As VB.VisualBasicSyntaxNode = node.Name.Accept(Me)
            Dim crefParameters As New List(Of VBS.CrefSignaturePartSyntax)
            Dim signature As VBS.CrefSignatureSyntax = Nothing
            If node.Parameters IsNot Nothing Then
                For Each p As CSS.CrefParameterSyntax In node.Parameters.Parameters
                    crefParameters.Add(Factory.CrefSignaturePart(modifier:=Nothing, DirectCast(p.Accept(Me), VBS.TypeSyntax)))
                Next
                signature = Factory.CrefSignature(crefParameters.ToArray)
            End If
            Return Factory.CrefReference(DirectCast(name, VBS.TypeSyntax), signature:=signature, asClause:=Nothing)
        End Function

        Public Overrides Function VisitOperatorMemberCref(node As CSS.OperatorMemberCrefSyntax) As VB.VisualBasicSyntaxNode
            Return Factory.CrefOperatorReference(GetVBOperatorToken(node.OperatorToken.ValueText).WithLeadingTrivia(Factory.Space))
        End Function

        Public Overrides Function VisitPredefinedType(node As CSS.PredefinedTypeSyntax) As VB.VisualBasicSyntaxNode
            Dim token As SyntaxToken = CS.CSharpExtensions.Kind(node.Keyword).GetTypeToken(context:=TokenContext.XMLComment)
            Select Case token.RawKind
                Case VB.SyntaxKind.EmptyToken
                    Return Factory.ParseTypeName(node.ToString)
                Case VB.SyntaxKind.NothingKeyword
                    Return NothingExpression
                Case Else
                    Return Factory.PredefinedType(token)
            End Select
        End Function

        Public Overrides Function VisitQualifiedCref(QualifiedCref As CSS.QualifiedCrefSyntax) As VB.VisualBasicSyntaxNode
            Dim identifierOrTypeName As VB.VisualBasicSyntaxNode = QualifiedCref.Container.Accept(Me)
            Dim value As VBS.CrefReferenceSyntax = DirectCast(QualifiedCref.Member.Accept(Me), VBS.CrefReferenceSyntax)
            Dim identifier As VBS.NameSyntax = If(TypeOf identifierOrTypeName Is VBS.NameSyntax, DirectCast(identifierOrTypeName, VBS.NameSyntax), Factory.IdentifierName(identifierOrTypeName.ToString))
            Dim qualifiedNameSyntax As VBS.QualifiedNameSyntax = Factory.QualifiedName(left:=identifier,
                                                                                       DotToken,
                                                                                       right:=DirectCast(value.Name, VBS.SimpleNameSyntax))
            If value.Signature Is Nothing Then
                Return qualifiedNameSyntax
            End If
            Return Factory.CrefReference(qualifiedNameSyntax, value.Signature, Nothing)
        End Function

        Public Overrides Function VisitQualifiedName(node As CSS.QualifiedNameSyntax) As VB.VisualBasicSyntaxNode
            Return Factory.QualifiedName(DirectCast(node.Left.Accept(Me), VBS.NameSyntax), DirectCast(node.Right.Accept(Me), VBS.SimpleNameSyntax))
        End Function

        Public Overrides Function VisitTypeCref(node As CSS.TypeCrefSyntax) As VB.VisualBasicSyntaxNode
            Return node.Type.Accept(Me)
        End Function

        Public Overrides Function VisitXmlCDataSection(node As CSS.XmlCDataSectionSyntax) As VB.VisualBasicSyntaxNode
            Return Factory.XmlCDataSection(BeginCDataToken, TranslateTokenList(node.TextTokens), EndCDataToken)
        End Function

        Public Overrides Function VisitXmlComment(node As CSS.XmlCommentSyntax) As VB.VisualBasicSyntaxNode
            Return MyBase.VisitXmlComment(node).WithConvertedTriviaFrom(node)
        End Function

        Public Overrides Function VisitXmlCrefAttribute(node As CSS.XmlCrefAttributeSyntax) As VB.VisualBasicSyntaxNode
            Dim cref As VBS.XmlNameSyntax = CType(node.Name.Accept(Me), VBS.XmlNameSyntax).WithConvertedTriviaFrom(node.Name)
            Dim memberName As VB.VisualBasicSyntaxNode = node.Cref.Accept(Me)
            Dim crefType As VBS.CrefReferenceSyntax
            If TypeOf memberName Is VBS.NameSyntax Then
                crefType = Factory.CrefReference(CType(memberName, VBS.NameSyntax))
            Else
                crefType = CType(memberName, VBS.CrefReferenceSyntax)
            End If
            Return Factory.XmlCrefAttribute(cref, DoubleQuoteToken.WithConvertedTriviaFrom(node.StartQuoteToken), crefType, DoubleQuoteToken.WithConvertedTriviaFrom(node.EndQuoteToken))
        End Function

        Public Overrides Function VisitXmlElement(node As CSS.XmlElementSyntax) As VB.VisualBasicSyntaxNode
            Dim content As New SyntaxList(Of VBS.XmlNodeSyntax)
            Dim startTag As VBS.XmlElementStartTagSyntax = DirectCast(node.StartTag.Accept(Me).WithConvertedTriviaFrom(node.StartTag), VBS.XmlElementStartTagSyntax)

            Dim noEndTag As Boolean = String.IsNullOrWhiteSpace(node.EndTag.Name.LocalName.ValueText)
            Dim endTag As VBS.XmlElementEndTagSyntax
            endTag = If(noEndTag,
                        Factory.XmlElementEndTag(DirectCast(startTag.Name, VBS.XmlNameSyntax).WithConvertedTriviaFrom(node.EndTag)),
                        Factory.XmlElementEndTag(DirectCast(node.EndTag.Name.Accept(Me), VBS.XmlNameSyntax))
                       )
            Try
                For Each e As IndexClass(Of CSS.XmlNodeSyntax) In node.Content.WithIndex
                    Dim vbNode As VBS.XmlNodeSyntax = CType(e.Value.Accept(Me).WithConvertedTriviaFrom(e.Value), VBS.XmlNodeSyntax)
                    If noEndTag Then
                        Dim lastToken As SyntaxToken = vbNode.GetLastToken
                        If lastToken.ValueText.IsNewLine Then
                            vbNode = vbNode.ReplaceToken(lastToken, Factory.Token(VB.SyntaxKind.EmptyToken))
                        End If
                    End If
                    If vbNode.IsKind(VB.SyntaxKind.XmlEmptyElement) Then
                        Dim emptyElement As VBS.XmlEmptyElementSyntax = CType(vbNode, VBS.XmlEmptyElementSyntax)
                        content = content.Add(emptyElement)
                    Else
                        content = content.Add(vbNode)
                    End If
                Next

                If node.EndTag?.HasLeadingTrivia AndAlso node.EndTag.GetLeadingTrivia(0).IsKind(CS.SyntaxKind.DocumentationCommentExteriorTrivia) Then
                    Dim newLeadingTriviaList As New SyntaxTriviaList
                    newLeadingTriviaList = newLeadingTriviaList.Add(Factory.DocumentationCommentExteriorTrivia(node.EndTag.GetLeadingTrivia(0).ToString.Replace("///", "'''", StringComparison.Ordinal)))
                    Dim newTokenList As New SyntaxTokenList
                    newTokenList = newTokenList.Add(Factory.XmlTextLiteralToken(newLeadingTriviaList, " ", " ", New SyntaxTriviaList))
                    content = content.Add(Factory.XmlText(newTokenList))
                    endTag = endTag.WithoutLeadingTrivia
                End If
            Catch ex As OperationCanceledException
                Throw
            Catch ex As Exception
                Stop
            End Try
            Return Factory.XmlElement(startTag, content, endTag)
        End Function

        Public Overrides Function VisitXmlElementEndTag(node As CSS.XmlElementEndTagSyntax) As VB.VisualBasicSyntaxNode
            Return Factory.XmlElementEndTag(DirectCast(node.Name.Accept(Me), VBS.XmlNameSyntax))
        End Function

        Public Overrides Function VisitXmlElementStartTag(node As CSS.XmlElementStartTagSyntax) As VB.VisualBasicSyntaxNode
            Dim listOfAttributes As SyntaxList(Of VBS.XmlNodeSyntax) = Me.GatherAttributes(node.Attributes)
            Return Factory.XmlElementStartTag(DirectCast(node.Name.Accept(Me), VBS.XmlNodeSyntax), listOfAttributes)
        End Function

        Public Overrides Function VisitXmlEmptyElement(node As CSS.XmlEmptyElementSyntax) As VB.VisualBasicSyntaxNode
            Try
                Dim name As VBS.XmlNodeSyntax = DirectCast(node.Name.Accept(Me), VBS.XmlNodeSyntax).WithConvertedTriviaFrom(node.Name)
                Dim listOfAttributes As SyntaxList(Of VBS.XmlNodeSyntax) = Me.GatherAttributes(node.Attributes)
                Return Factory.XmlEmptyElement(name, listOfAttributes).WithConvertedTriviaFrom(node)
            Catch ex As OperationCanceledException
                Throw
            Catch ex As Exception
                Return Factory.XmlText(node.GetText.ToString)
            End Try
        End Function

        Public Overrides Function VisitXmlName(node As CSS.XmlNameSyntax) As VB.VisualBasicSyntaxNode
            Dim prefix As VBS.XmlPrefixSyntax
            prefix = If(node.Prefix Is Nothing, Nothing, DirectCast(node.Prefix.Accept(Me), VBS.XmlPrefixSyntax))
            Dim localName As SyntaxToken = Factory.XmlNameToken(node.LocalName.ValueText, Nothing)
            Return Factory.XmlName(prefix, localName)
        End Function

        Public Overrides Function VisitXmlNameAttribute(node As CSS.XmlNameAttributeSyntax) As VB.VisualBasicSyntaxNode
            Dim name As VBS.XmlNodeSyntax = DirectCast(node.Name.Accept(Me), VBS.XmlNodeSyntax).WithConvertedLeadingTriviaFrom(node.Name)
            Dim valueString As String = node.Identifier.ToString
            Return Factory.XmlAttribute(name, Factory.XmlString(DoubleQuoteToken,
                                                                SyntaxTokenList.Create(Factory.XmlTextLiteralToken(valueString, valueString)),
                                                                DoubleQuoteToken)).WithConvertedTriviaFrom(node).WithConvertedTriviaFrom(node)
        End Function

        Public Overrides Function VisitXmlPrefix(node As CSS.XmlPrefixSyntax) As VB.VisualBasicSyntaxNode
            Return MyBase.VisitXmlPrefix(node).WithConvertedTriviaFrom(node)
        End Function

        Public Overrides Function VisitXmlProcessingInstruction(node As CSS.XmlProcessingInstructionSyntax) As VB.VisualBasicSyntaxNode
            Return MyBase.VisitXmlProcessingInstruction(node).WithConvertedTriviaFrom(node)
        End Function

        Public Overrides Function VisitXmlText(node As CSS.XmlTextSyntax) As VB.VisualBasicSyntaxNode
            Return Factory.XmlText(TranslateTokenList(node.TextTokens))
        End Function

        Public Overrides Function VisitXmlTextAttribute(node As CSS.XmlTextAttributeSyntax) As VB.VisualBasicSyntaxNode
            Dim name As VBS.XmlNodeSyntax = DirectCast(node.Name.Accept(Me).WithConvertedTriviaFrom(node.Name), VBS.XmlNodeSyntax)
            Dim xmlTextStr As String = Factory.XmlText(TranslateTokenList(node.TextTokens)).ToString
            Return Factory.XmlAttribute(name, Factory.XmlString(DoubleQuoteToken,
                                                                SyntaxTokenList.Create(
                                                                Factory.XmlTextLiteralToken(xmlTextStr, xmlTextStr)),
                                                                DoubleQuoteToken)
                                                               )
        End Function

    End Class

End Namespace
