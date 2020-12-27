' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBConverter.ToVisualBasic

    Partial Public Class CSharpConverter

        Partial Friend Class NodesVisitor
            Inherits CS.CSharpSyntaxVisitor(Of VB.VisualBasicSyntaxNode)

            Private Shared Function FindClauseForParameter(node As CSS.TypeParameterSyntax) As CSS.TypeParameterConstraintClauseSyntax
                Dim clauses As SyntaxList(Of CSS.TypeParameterConstraintClauseSyntax)
                Dim parentBlock As SyntaxNode = node.Parent.Parent
                If TypeOf parentBlock Is CSS.StructDeclarationSyntax Then
                    Dim s As CSS.StructDeclarationSyntax = DirectCast(parentBlock, CSS.StructDeclarationSyntax)
                    Return CS.SyntaxFactory.TypeParameterConstraintClause(CS.SyntaxFactory.IdentifierName(s.TypeParameterList.Parameters(0).Identifier.Text), Nothing)
                Else
                    clauses = parentBlock.TypeSwitch(
                    Function(m As CSS.MethodDeclarationSyntax) m.ConstraintClauses,
                    Function(c As CSS.ClassDeclarationSyntax) c.ConstraintClauses,
                    Function(d As CSS.DelegateDeclarationSyntax) d.ConstraintClauses,
                    Function(i As CSS.InterfaceDeclarationSyntax) i.ConstraintClauses,
                    Function(__ As SyntaxNode) As SyntaxList(Of CSS.TypeParameterConstraintClauseSyntax)
                        Throw New NotImplementedException($"{__.GetType().FullName} not implemented!")
                    End Function)
                    Return clauses.FirstOrDefault(Function(c As CSS.TypeParameterConstraintClauseSyntax) c.Name.ToString() = node.ToString())
                End If

            End Function

            Public Overrides Function VisitArrayRankSpecifier(node As CSS.ArrayRankSpecifierSyntax) As VB.VisualBasicSyntaxNode
                Return Factory.ArrayRankSpecifier(openParenToken, Factory.TokenList(Enumerable.Repeat(CommaToken, node.Rank - 1)), CloseParenToken)
            End Function

            Public Overrides Function VisitArrayType(node As CSS.ArrayTypeSyntax) As VB.VisualBasicSyntaxNode
                Return Factory.ArrayType(DirectCast(node.ElementType.Accept(Me), VBS.TypeSyntax), Factory.List(node.RankSpecifiers.Select(Function(rs As CSS.ArrayRankSpecifierSyntax) DirectCast(rs.Accept(Me), VBS.ArrayRankSpecifierSyntax)))).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitClassOrStructConstraint(node As CSS.ClassOrStructConstraintSyntax) As VB.VisualBasicSyntaxNode
                If node.IsKind(CS.SyntaxKind.ClassConstraint) Then
                    Return Factory.ClassConstraint(ClassKeyWord).WithConvertedTriviaFrom(node)
                End If
                If node.IsKind(CS.SyntaxKind.StructConstraint) Then
                    Return Factory.StructureConstraint(StructureKeyword).WithConvertedTriviaFrom(node)
                End If
                Throw New NotSupportedException()
            End Function

            Public Overrides Function VisitConstructorConstraint(node As CSS.ConstructorConstraintSyntax) As VB.VisualBasicSyntaxNode
                Return Factory.NewConstraint(NewKeyword).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitDefaultConstraint(node As CSS.DefaultConstraintSyntax) As VB.VisualBasicSyntaxNode
                Dim commentTrivia As SyntaxTriviaList = New SyntaxTriviaList
                commentTrivia = commentTrivia.Add(Factory.Space)
                commentTrivia = commentTrivia.Add(LineContinuation)
                commentTrivia = commentTrivia.Add(Factory.CommentTrivia(" ' TODO Visual Basic does not support 'Default Constraint'"))
                Return Factory.TypeConstraint(Factory.ParseTypeName("[default]").WithTrailingTrivia(commentTrivia))
            End Function

            Public Overrides Function VisitNullableType(node As CSS.NullableTypeSyntax) As VB.VisualBasicSyntaxNode
                Dim elementType As VB.VisualBasicSyntaxNode = node.ElementType.Accept(Me)
                If TypeOf elementType Is VBS.ArrayTypeSyntax Then
                    Dim arrayType As VBS.ArrayTypeSyntax = DirectCast(elementType, VBS.ArrayTypeSyntax)
                    Dim arrayElementType As VBS.TypeSyntax = arrayType.ElementType
                    Dim elementTypeStr As String = arrayElementType.ToString
                    If elementTypeStr.EndsWith("?"c, StringComparison.OrdinalIgnoreCase) Then
                        arrayElementType = Factory.ParseTypeName(elementTypeStr.TrimEnd("?"c))
                    End If
                    Return Factory.ArrayType(Factory.NullableType(arrayElementType),
                                             arrayType.RankSpecifiers).WithConvertedTriviaFrom(node)
                End If
                If elementType.IsKind(VB.SyntaxKind.PredefinedType) AndAlso elementType.ToString = "String" Then
                    Return elementType
                End If

                Return Factory.NullableType(DirectCast(elementType, VBS.TypeSyntax)).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitPointerType(node As CSS.PointerTypeSyntax) As VB.VisualBasicSyntaxNode
                If node.ToString = "void*" Then
                    Return PredefinedTypeObject.WithConvertedTriviaFrom(node.Parent)
                End If

                Return node.Parent.TypeSwitch(
                    Function(m As CSS.CastExpressionSyntax)
                        Return Factory.AddressOfExpression(Factory.ParseExpression(m.Expression.ToString).WithConvertedTriviaFrom(m))
                    End Function,
                    Function(c As CSS.VariableDeclarationSyntax)
                        Dim operand As VBS.TypeSyntax = DirectCast(node.ElementType.Accept(Me), VBS.TypeSyntax)
                        Return Factory.AddressOfExpression(Factory.ParseExpression(operand.ToString).WithConvertedTriviaFrom(node.ElementType))
                    End Function,
                    Function(d As CSS.ParameterSyntax)
                        Dim operand As VBS.TypeSyntax = DirectCast(node.ElementType.Accept(Me), VBS.TypeSyntax)
                        Return operand.WithConvertedTriviaFrom(node.ElementType)
                    End Function,
                    Function(i As CSS.PointerTypeSyntax)
                        Return IntPtrType
                    End Function,
                    Function(t As CSS.TypeOfExpressionSyntax)
                        Return IntPtrType
                    End Function,
                    Function(__ As SyntaxNode) As VB.VisualBasicSyntaxNode
                        Throw New NotImplementedException($"{__.GetType().FullName} not implemented!")
                    End Function
                    )

            End Function

            Public Overrides Function VisitPredefinedType(node As CSS.PredefinedTypeSyntax) As VB.VisualBasicSyntaxNode
                Dim predefinedType As VBS.PredefinedTypeSyntax = Nothing
                Try
                    If node.Keyword.ToString = "void" Then
                        Return Factory.IdentifierName("void")
                    End If
                    predefinedType = Factory.PredefinedType(CS.CSharpExtensions.Kind(node.Keyword).GetTypeToken())
                Catch ex As OperationCanceledException
                    Throw
                Catch ex As Exception
                    Stop
                End Try
                Return predefinedType
            End Function

            Public Overrides Function VisitSimpleBaseType(node As CSS.SimpleBaseTypeSyntax) As VB.VisualBasicSyntaxNode
                Return ConvertToType(node.NormalizeWhitespace.ToString).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitTypeConstraint(node As CSS.TypeConstraintSyntax) As VB.VisualBasicSyntaxNode
                Return Factory.TypeConstraint(DirectCast(node.Type.Accept(Me), VBS.TypeSyntax)).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitTypeParameter(node As CSS.TypeParameterSyntax) As VB.VisualBasicSyntaxNode
                Dim variance As SyntaxToken = Nothing
                If Not node.VarianceKeyword.IsKind(CS.SyntaxKind.None) Then
                    variance = If(node.VarianceKeyword.IsKind(CS.SyntaxKind.InKeyword), InKeyword, OutKeyword)
                End If

                ' copy generic constraints
                Dim clause As CSS.TypeParameterConstraintClauseSyntax = FindClauseForParameter(node)
                Dim typeParameterConstraintClause As VBS.TypeParameterConstraintClauseSyntax = DirectCast(clause?.Accept(Me), VBS.TypeParameterConstraintClauseSyntax)
                If typeParameterConstraintClause IsNot Nothing AndAlso typeParameterConstraintClause.IsKind(VB.SyntaxKind.TypeParameterMultipleConstraintClause) Then
                    Dim typeParameterMultipleConstraintClause As VBS.TypeParameterMultipleConstraintClauseSyntax = DirectCast(typeParameterConstraintClause, VBS.TypeParameterMultipleConstraintClauseSyntax)
                    If typeParameterMultipleConstraintClause.Constraints.Count = 0 Then
                        typeParameterConstraintClause = Nothing
                    End If
                End If
                Return Factory.TypeParameter(variance,
                                             GenerateSafeVBToken(node.Identifier,
                                                                 node,
                                                                 _semanticModel,
                                                                 _usedIdentifiers,
                                                                 IsQualifiedName:=False,
                                                                 IsTypeName:=True),
                                            typeParameterConstraintClause).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitTypeParameterConstraintClause(node As CSS.TypeParameterConstraintClauseSyntax) As VB.VisualBasicSyntaxNode
                Dim braces As (openBrace As SyntaxToken, closeBrace As SyntaxToken) = node.GetBraces
                If node.Constraints.Count = 1 Then
                    Return Factory.TypeParameterSingleConstraintClause(AsKeyword.WithTrailingTrivia(Factory.Space),
                                                                       DirectCast(node.Constraints(0).Accept(Me), VBS.ConstraintSyntax))
                End If
                Return Factory.TypeParameterMultipleConstraintClause(AsKeyword,
                                                                     OpenBraceToken.WithConvertedTriviaFrom(braces.openBrace),
                                                                     Factory.SeparatedList(node.Constraints.Select(Function(c As CSS.TypeParameterConstraintSyntax)
                                                                                                                       Return DirectCast(c.Accept(Me), VBS.ConstraintSyntax)
                                                                                                                   End Function)),
                                                                     CloseBraceToken.WithConvertedTriviaFrom(braces.closeBrace))
            End Function

            Public Overrides Function VisitTypeParameterList(node As CSS.TypeParameterListSyntax) As VB.VisualBasicSyntaxNode
                Dim nodes As New List(Of VBS.TypeParameterSyntax)
                Dim separators As New List(Of SyntaxToken)
                Dim finalTrailingTrivia As New SyntaxTriviaList
                Dim csSeparators As New List(Of SyntaxToken)
                csSeparators.AddRange(node.Parameters.GetSeparators)
                For index As Integer = 0 To node.Parameters.Count - 2
                    Dim param As CSS.TypeParameterSyntax = node.Parameters(index)
                    Dim itemWithTrivia As VBS.TypeParameterSyntax = DirectCast(param.Accept(Me), VBS.TypeParameterSyntax)
                    If itemWithTrivia.GetLeadingTrivia.ContainsCommentOrDirectiveTrivia Then
                        finalTrailingTrivia = finalTrailingTrivia.AddRange(itemWithTrivia.GetLeadingTrivia)
                    End If
                    finalTrailingTrivia = finalTrailingTrivia.AddRange(itemWithTrivia.GetTrailingTrivia)
                    nodes.Add(itemWithTrivia.WithLeadingTrivia(Factory.Space).WithTrailingTrivia(Factory.Space))
                    separators.Add(CommaToken.WithConvertedTriviaFrom(csSeparators(index)))
                Next
                nodes.Add(DirectCast(node.Parameters.Last.Accept(Me).WithConvertedTrailingTriviaFrom(node.Parameters.Last), VBS.TypeParameterSyntax))
                Dim parameters As SeparatedSyntaxList(Of VBS.TypeParameterSyntax) = Factory.SeparatedList(nodes, separators)
                Return Factory.TypeParameterList(openParenToken,
                                                 OfKeyword.WithTrailingTrivia(Factory.Space),
                                                 parameters:=parameters,
                                                 CloseParenToken.WithConvertedTriviaFrom(node.GreaterThanToken).WithAppendedTrailingTrivia(finalTrailingTrivia)
                                                )
            End Function

        End Class

    End Class

End Namespace
