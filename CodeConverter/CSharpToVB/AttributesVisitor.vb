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

        Partial Protected Friend Class NodesVisitor
            Inherits CS.CSharpSyntaxVisitor(Of VB.VisualBasicSyntaxNode)

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitAttribute(node As CSS.AttributeSyntax) As VB.VisualBasicSyntaxNode
                Dim list As CSS.AttributeListSyntax = DirectCast(node.Parent, CSS.AttributeListSyntax)
                Return VBFactory.Attribute(DirectCast(list.Target?.Accept(Me), VBS.AttributeTargetSyntax),
                                               DirectCast(node.Name.Accept(Me).WithConvertedTriviaFrom(node.Name), VBS.TypeSyntax),
                                               DirectCast(node.ArgumentList?.Accept(Me), VBS.ArgumentListSyntax))
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitAttributeArgument(node As CSS.AttributeArgumentSyntax) As VB.VisualBasicSyntaxNode
                Dim name As VBS.NameColonEqualsSyntax = Nothing
                If node.NameColon IsNot Nothing Then
                    name = VBFactory.NameColonEquals(DirectCast(node.NameColon.Name.Accept(Me), VBS.IdentifierNameSyntax))
                    ' HACK for VB Error
                    If name.ToString = "[error]:=" Then
                        name = Nothing
                    End If
                ElseIf node.NameEquals IsNot Nothing Then
                    name = VBFactory.NameColonEquals(DirectCast(node.NameEquals.Name.Accept(Me), VBS.IdentifierNameSyntax))
                End If

                Dim value As VBS.ExpressionSyntax = DirectCast(node.Expression.Accept(Me), VBS.ExpressionSyntax)
                Return VBFactory.SimpleArgument(name, value).WithConvertedTriviaFrom(node)
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitAttributeArgumentList(node As CSS.AttributeArgumentListSyntax) As VB.VisualBasicSyntaxNode
                Dim ArgumentNodes As New List(Of VBS.ArgumentSyntax)
                Dim TrailingTriviaList As New List(Of SyntaxTrivia)
                For i As Integer = 0 To node.Arguments.Count - 1
                    Dim LocalLeadingTrivia As New List(Of SyntaxTrivia)
                    Dim LocalTrailingTrivia As New List(Of SyntaxTrivia)
                    Dim Item As VBS.ArgumentSyntax = DirectCast(node.Arguments(i).Accept(Me), VBS.ArgumentSyntax)
                    If Item.HasLeadingTrivia Then
                        For Each t As SyntaxTrivia In Item.GetLeadingTrivia
                            If t.IsComment Then
                                TrailingTriviaList.Add(t)
                            Else
                                LocalLeadingTrivia.Add(t)
                            End If
                        Next
                        Item = Item.WithLeadingTrivia(LocalLeadingTrivia)
                    End If
                    If Item.HasTrailingTrivia Then
                        For Each t As SyntaxTrivia In Item.GetTrailingTrivia
                            If t.IsComment OrElse t.IsEndOfLine Then
                                TrailingTriviaList.Add(t)
                            Else
                                LocalTrailingTrivia.Add(t)
                            End If
                        Next
                    End If
                    Item = Item.WithTrailingTrivia(LocalTrailingTrivia)
                    ArgumentNodes.Add(Item)
                Next
                Return VBFactory.ArgumentList(OpenParenToken, VBFactory.SeparatedList(ArgumentNodes), CloseParenToken).WithConvertedLeadingTriviaFrom(node).WithTrailingTrivia(TrailingTriviaList).WithAppendedTrailingTrivia(ConvertTrivia(node.GetTrailingTrivia))
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitAttributeList(node As CSS.AttributeListSyntax) As VB.VisualBasicSyntaxNode
                Dim CS_Separators As IEnumerable(Of SyntaxToken) = node.Attributes.GetSeparators
                Dim LessThanTokenWithTrivia As SyntaxToken = LessThanToken.WithConvertedTriviaFrom(node.OpenBracketToken)
                Dim GreaterThenTokenWithTrivia As SyntaxToken = GreaterThanToken.WithConvertedTriviaFrom(node.CloseBracketToken)
                Dim AttributeList As New List(Of VBS.AttributeSyntax)
                Dim Separators As New List(Of SyntaxToken)
                Dim SeparatorCount As Integer = node.Attributes.Count - 1
                For i As Integer = 0 To SeparatorCount
                    Dim e As CSS.AttributeSyntax = node.Attributes(i)
                    AttributeList.Add(DirectCast(e.Accept(Me), VBS.AttributeSyntax).RemoveExtraLeadingEOL)
                    If SeparatorCount > i Then
                        Separators.Add(CommaToken.WithConvertedTrailingTriviaFrom(CS_Separators(i)))
                    End If
                Next
                RestructureNodesAndSeparators(LessThanTokenWithTrivia, AttributeList, Separators, GreaterThenTokenWithTrivia)
                Dim Attributes1 As SeparatedSyntaxList(Of VBS.AttributeSyntax) = VBFactory.SeparatedList(AttributeList, Separators)
                If Attributes1.Last.HasTrailingTrivia Then
                    Attributes1.Replace(Attributes1.Last, Attributes1.Last.WithoutTrailingTrivia)
                End If
                Return VBFactory.AttributeList(LessThanTokenWithTrivia, Attributes1, GreaterThenTokenWithTrivia)
            End Function

            <CodeAnalysis.SuppressMessage("Design", "CA1062:Validate arguments of public methods", Justification:="Node can't Be Nothing")>
            Public Overrides Function VisitAttributeTargetSpecifier(node As CSS.AttributeTargetSpecifierSyntax) As VB.VisualBasicSyntaxNode
                Dim id As SyntaxToken
                Select Case CS.CSharpExtensions.Kind(node.Identifier)
                    Case CS.SyntaxKind.AssemblyKeyword
                        id = AssemblyKeyword
                    Case CS.SyntaxKind.ModuleKeyword
                        id = ModuleKeyword
                    Case CS.SyntaxKind.ParamKeyword, CS.SyntaxKind.TypeKeyword
                        Return Nothing
                    Case CS.SyntaxKind.ReturnKeyword
                        ' Not necessary, return attributes are moved by ConvertAndSplitAttributes.
                        Return Nothing
                    Case Else
                        Return Nothing
                End Select
                Return VBFactory.AttributeTarget(id).WithConvertedTriviaFrom(node)
            End Function

        End Class

    End Class

End Namespace
