' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

Imports CSharpToVBCodeConverter.Util

Imports Microsoft.CodeAnalysis

Imports CS = Microsoft.CodeAnalysis.CSharp
Imports CSS = Microsoft.CodeAnalysis.CSharp.Syntax
Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBCodeConverter.Visual_Basic

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
                    Function(Underscore As SyntaxNode) As SyntaxList(Of CSS.TypeParameterConstraintClauseSyntax)
                        Throw New NotImplementedException($"{Underscore.[GetType]().FullName} not implemented!")
                    End Function)
                    Return clauses.FirstOrDefault(Function(c As CSS.TypeParameterConstraintClauseSyntax) c.Name.ToString() = node.ToString())
                End If

            End Function

            Friend Shared Function ConvertNamedTypeToTypeString(TypeString As String) As String
                Dim SplitTypeString() As String = TypeString.Trim.Split(" "c, StringComparison.InvariantCulture)
                If SplitTypeString.Length > 2 Then
                    Stop
                End If
                Dim IndexOfLessThan As Integer = TypeString.IndexOf("<", StringComparison.InvariantCulture)
                Dim TypeName As String = SplitTypeString(0)
                Dim Name As String = If(SplitTypeString.Length = 1, "", SplitTypeString(1) & " As ")
                If IndexOfLessThan > 0 Then
                    Return $"{Name}{TypeName.Left(IndexOfLessThan)}{TypeName.Substring(IndexOfLessThan).
                                    Replace("<", "(Of ", StringComparison.InvariantCulture).
                                    Replace(">", ")", StringComparison.InvariantCulture)}"
                End If
                Return Name & ConvertToType(TypeName).ToString
            End Function

            Private Shared Function ConvertTupleToTypeStrings(TypeString As String) As List(Of String)
                Dim RetList As New List(Of String)
                Dim IndexOfLessThan As Integer = TypeString.IndexOf("<", StringComparison.InvariantCulture)
                If IndexOfLessThan > 0 Then
                    Dim CShar_Types() As String = TypeString.Substring(IndexOfLessThan).
                        Replace("<", "", StringComparison.InvariantCulture).
                        Replace(">", "", StringComparison.InvariantCulture).Split(","c)
                    For Each t As String In CShar_Types
                        RetList.Add(ConvertToType(ConvertToType(t).ToString).ToString)
                    Next
                ElseIf TypeString.EndsWith("DictionaryEntry", StringComparison.InvariantCulture) Then
                    RetList.Add(ConvertToType("Key").ToString)
                    RetList.Add(ConvertToType("Value").ToString)
                Else
                    Stop
                End If
                Return RetList
            End Function

            Public Overrides Function VisitArrayRankSpecifier(node As CSS.ArrayRankSpecifierSyntax) As VB.VisualBasicSyntaxNode
                Return VBFactory.ArrayRankSpecifier(OpenParenToken, VBFactory.TokenList(Enumerable.Repeat(CommaToken, node.Rank - 1)), CloseParenToken)
            End Function

            Public Overrides Function VisitArrayType(node As CSS.ArrayTypeSyntax) As VB.VisualBasicSyntaxNode
                Return VBFactory.ArrayType(DirectCast(node.ElementType.Accept(Me), VBS.TypeSyntax), VBFactory.List(node.RankSpecifiers.Select(Function(rs As CSS.ArrayRankSpecifierSyntax) DirectCast(rs.Accept(Me), VBS.ArrayRankSpecifierSyntax)))).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitClassOrStructConstraint(node As CSS.ClassOrStructConstraintSyntax) As VB.VisualBasicSyntaxNode
                If node.IsKind(CS.SyntaxKind.ClassConstraint) Then
                    Return VBFactory.ClassConstraint(ClassKeyWord).WithConvertedTriviaFrom(node)
                End If
                If node.IsKind(CS.SyntaxKind.StructConstraint) Then
                    Return VBFactory.StructureConstraint(StructureKeyword).WithConvertedTriviaFrom(node)
                End If
                Throw New NotSupportedException()
            End Function

            Public Overrides Function VisitConstructorConstraint(node As CSS.ConstructorConstraintSyntax) As VB.VisualBasicSyntaxNode
                Return VBFactory.NewConstraint(NewKeyword).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitNullableType(node As CSS.NullableTypeSyntax) As VB.VisualBasicSyntaxNode
                Dim TypeSyntax As VB.VisualBasicSyntaxNode = node.ElementType.Accept(Me)
                If TypeOf TypeSyntax Is VBS.ArrayTypeSyntax Then
                    Dim ArrayType As VBS.ArrayTypeSyntax = DirectCast(TypeSyntax, VBS.ArrayTypeSyntax)
                    Dim NullableType As VBS.NullableTypeSyntax = VBFactory.NullableType(DirectCast(TypeSyntax, VBS.ArrayTypeSyntax).ElementType)
                    Return VBFactory.ArrayType(NullableType, ArrayType.RankSpecifiers).WithConvertedTriviaFrom(node)
                End If
                Return VBFactory.NullableType(DirectCast(TypeSyntax, VBS.TypeSyntax)).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitPointerType(node As CSS.PointerTypeSyntax) As VB.VisualBasicSyntaxNode
                If node.ToString = "void*" Then
                    Return PredefinedTypeObject.WithConvertedTriviaFrom(node.Parent)
                End If
                Dim NodeParent As SyntaxNode = node.Parent
                If TypeOf NodeParent Is CSS.CastExpressionSyntax Then
                    Dim OperandWithAmpersand As CSS.ExpressionSyntax = DirectCast(node.Parent, CSS.CastExpressionSyntax).Expression
                    Return VBFactory.AddressOfExpression(VBFactory.ParseExpression(OperandWithAmpersand.ToString).WithConvertedTriviaFrom(OperandWithAmpersand))
                End If
                If TypeOf NodeParent Is CSS.VariableDeclarationSyntax Then
                    Dim Operand As VBS.TypeSyntax = DirectCast(node.ElementType.Accept(Me), VBS.TypeSyntax)
                    Return VBFactory.AddressOfExpression(VBFactory.ParseExpression(Operand.ToString).WithConvertedTriviaFrom(node.ElementType))
                End If
                If TypeOf NodeParent Is CSS.ParameterSyntax Then
                    Dim Operand As VBS.TypeSyntax = DirectCast(node.ElementType.Accept(Me), VBS.TypeSyntax)
                    Return Operand.WithConvertedTriviaFrom(node.ElementType)
                End If

                If node.ToString = "int*" Then
                    Return IntPtrType
                End If
                If node.ToString = "char*" Then
                    Return IntPtrType
                End If
                Return IntPtrType
            End Function

            Public Overrides Function VisitPredefinedType(node As CSS.PredefinedTypeSyntax) As VB.VisualBasicSyntaxNode
                Dim PredefinedType As VBS.PredefinedTypeSyntax = Nothing
                Try
                    If node.Keyword.ToString = "void" Then
                        Return VBFactory.IdentifierName("void")
                    End If
                    PredefinedType = VBFactory.PredefinedType(ConvertTypesTokenToKind(CS.CSharpExtensions.Kind(node.Keyword)))
                Catch ex As OperationCanceledException
                    Throw
                Catch ex As Exception
                    Stop
                End Try
                Return PredefinedType
            End Function

            Public Overrides Function VisitSimpleBaseType(node As CSS.SimpleBaseTypeSyntax) As VB.VisualBasicSyntaxNode
                Dim TypeString As String = node.NormalizeWhitespace.ToString

                Return ConvertToType(TypeString).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitTypeConstraint(node As CSS.TypeConstraintSyntax) As VB.VisualBasicSyntaxNode
                Return VBFactory.TypeConstraint(DirectCast(node.Type.Accept(Me), VBS.TypeSyntax)).WithConvertedTriviaFrom(node)
            End Function

            Public Overrides Function VisitTypeParameter(node As CSS.TypeParameterSyntax) As VB.VisualBasicSyntaxNode
                Dim variance As SyntaxToken = Nothing
                If Not node.VarianceKeyword.IsKind(CS.SyntaxKind.None) Then
                    variance = If(node.VarianceKeyword.IsKind(CS.SyntaxKind.InKeyword), InKeyword, OutKeyword)
                End If

                ' copy generic constraints
                Dim clause As CSS.TypeParameterConstraintClauseSyntax = FindClauseForParameter(node)
                Dim TypeParameterConstraintClause As VBS.TypeParameterConstraintClauseSyntax = DirectCast(clause?.Accept(Me), VBS.TypeParameterConstraintClauseSyntax)
                If TypeParameterConstraintClause IsNot Nothing AndAlso TypeParameterConstraintClause.IsKind(VB.SyntaxKind.TypeParameterMultipleConstraintClause) Then
                    Dim TypeParameterMultipleConstraintClause As VBS.TypeParameterMultipleConstraintClauseSyntax = DirectCast(TypeParameterConstraintClause, VBS.TypeParameterMultipleConstraintClauseSyntax)
                    If TypeParameterMultipleConstraintClause.Constraints.Count = 0 Then
                        TypeParameterConstraintClause = Nothing
                    End If
                End If
                Dim TypeParameterSyntax As VBS.TypeParameterSyntax = VBFactory.TypeParameter(variance, GenerateSafeVBToken(node.Identifier, IsQualifiedName:=False, IsTypeName:=True), TypeParameterConstraintClause).WithConvertedTriviaFrom(node)
                Return TypeParameterSyntax
            End Function

            Public Overrides Function VisitTypeParameterConstraintClause(node As CSS.TypeParameterConstraintClauseSyntax) As VB.VisualBasicSyntaxNode
                Dim Braces As (OpenBrace As SyntaxToken, CloseBrace As SyntaxToken) = node.GetBraces
                Dim OpenBraceTokenWithTrivia As SyntaxToken = OpenBraceToken.WithConvertedTriviaFrom(Braces.OpenBrace)
                Dim CloseBraceTokenWithTrivia As SyntaxToken = VisualBasicSyntaxFactory.CloseBraceToken.WithConvertedTriviaFrom(Braces.CloseBrace)
                If node.Constraints.Count = 1 Then
                    Return VBFactory.TypeParameterSingleConstraintClause(AsKeyword, DirectCast(node.Constraints(0).Accept(Me), VBS.ConstraintSyntax))
                End If
                Dim Constraints As SeparatedSyntaxList(Of VBS.ConstraintSyntax) = VBFactory.SeparatedList(node.Constraints.Select(Function(c As CSS.TypeParameterConstraintSyntax) DirectCast(c.Accept(Me), VBS.ConstraintSyntax)))
                Return VBFactory.TypeParameterMultipleConstraintClause(AsKeyword, OpenBraceTokenWithTrivia, Constraints, CloseBraceTokenWithTrivia)
            End Function

            Public Overrides Function VisitTypeParameterList(node As CSS.TypeParameterListSyntax) As VB.VisualBasicSyntaxNode
                Dim Nodes As New List(Of VBS.TypeParameterSyntax)
                Dim Separators As New List(Of SyntaxToken)
                Dim CS_Separators As New List(Of SyntaxToken)
                CS_Separators.AddRange(node.Parameters.GetSeparators)
                Dim FinalTrailingTrivia As New List(Of SyntaxTrivia)
                For i As Integer = 0 To node.Parameters.Count - 2
                    Dim p As CSS.TypeParameterSyntax = node.Parameters(i)
                    Dim ItemWithTrivia As VBS.TypeParameterSyntax = DirectCast(p.Accept(Me), VBS.TypeParameterSyntax)
                    FinalTrailingTrivia.AddRange(ItemWithTrivia.GetLeadingTrivia)
                    FinalTrailingTrivia.AddRange(ItemWithTrivia.GetTrailingTrivia)
                    Nodes.Add(ItemWithTrivia.WithLeadingTrivia(SpaceTrivia).WithTrailingTrivia(SpaceTrivia))
                    Separators.Add(CommaToken.WithConvertedTriviaFrom(CS_Separators(i)))
                Next
                Nodes.Add(DirectCast(node.Parameters.Last.Accept(Me).WithConvertedTrailingTriviaFrom(node.Parameters.Last), VBS.TypeParameterSyntax))
                Dim SeparatedList As SeparatedSyntaxList(Of VBS.TypeParameterSyntax) = VBFactory.SeparatedList(Nodes, Separators)
                Dim TypeParameterListSyntax As VBS.TypeParameterListSyntax = VBFactory.TypeParameterList(OpenParenToken,
                                                                                                         OfKeyword.WithTrailingTrivia(SpaceTrivia),
                                                                                                         parameters:=SeparatedList,
                                                                                                         CloseParenToken.WithConvertedTriviaFrom(node.GreaterThanToken).WithAppendedTrailingTrivia(FinalTrailingTrivia))
                Return TypeParameterListSyntax
            End Function

        End Class

    End Class

End Namespace
