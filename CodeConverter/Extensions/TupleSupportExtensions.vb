' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Runtime.CompilerServices
Imports System.Text

Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.CSharp
Imports Factory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Namespace CSharpToVBCodeConverter.ToVisualBasic

    Public Module TupleSupportExtensions
        ''' <summary>
        ''' Extract String
        ''' </summary>
        ''' <param name="CSNamedTypeString">Source String</param>
        ''' <param name="StartIndex">Start of Substring</param>
        ''' <param name="PossibleIndex">End Index or -1 if no end</param>
        ''' <returns>Substring from i to CommaIndex or end if CommanIndex = -1</returns>
        <Extension>
        Private Function ExtractName(CSNamedTypeString As String, StartIndex As Integer, PossibleIndex As Integer) As String
            Dim length As Integer
            If PossibleIndex < 0 Then
                length = CSNamedTypeString.Length - StartIndex - 1
            Else
                length = PossibleIndex - StartIndex
            End If
            If length <= 0 Then
                Return ""
            End If
            Return MakeVBSafeName(CSNamedTypeString.Substring(StartIndex, length).Trim)
        End Function

        <Extension>
        Friend Function ConvertToTupleElement(TupleElement As IFieldSymbol) As VBS.TupleElementSyntax
            If TupleElement.Type Is Nothing Then
                Return Factory.NamedTupleElement(TupleElement.Name.ToString(Globalization.CultureInfo.InvariantCulture))
            End If
            Dim AsClause As VBS.SimpleAsClauseSyntax = Factory.SimpleAsClause(TupleElement.Type.ConvertToType())
            Return Factory.NamedTupleElement(Factory.Identifier(MakeVBSafeName(TupleElement.Name)), AsClause)
        End Function

        <Extension>
        Friend Function ConvertTupleToVBTypeStrings(CSharpNamedTypeString As String, IncludeName As Boolean) As List(Of String)
            Dim currentChar As String
            Dim openLT As Integer
            Dim openParen As Integer
            Dim tmpString As New StringBuilder
            Dim ElementList As New List(Of String)

            For currentIndex As Integer = 0 To CSharpNamedTypeString.Length - 1
                currentChar = CSharpNamedTypeString(currentIndex)
                Select Case currentChar
                    Case "<"
                        openLT = 1
                        tmpString.Append(currentChar)
                        While openLT <> 0
                            currentIndex += 1
                            currentChar = CSharpNamedTypeString(currentIndex)
                            Select Case currentChar
                                Case ">"
                                    openLT -= 1
                                    tmpString.Append(">"c)
                                    If openLT = 0 Then
                                        Exit While
                                    End If
                                Case "<"
                                    openLT += 1
                                    tmpString.Append("<"c)
                                Case "["
                                    tmpString.Append("("c)
                                Case "]"
                                    tmpString.Append(")"c)
                                Case Else
                                    tmpString.Append(currentChar)
                            End Select
                        End While
                        currentIndex += 1
                        If currentIndex >= CSharpNamedTypeString.Length - 1 OrElse Not ".[".Contains(CSharpNamedTypeString(currentIndex), StringComparison.OrdinalIgnoreCase) Then
                            Dim commaIndex As Integer = CSharpNamedTypeString.IndexOf(",", currentIndex, StringComparison.OrdinalIgnoreCase)
                            If IncludeName Then
                                Dim name As String = CSharpNamedTypeString.ExtractName(currentIndex, commaIndex)
                                If name.Length = 0 Then
                                    ElementList.Add($"{ConvertToType(tmpString.ToString)}")
                                Else
                                    ElementList.Add($"{MakeVBSafeName(name)} As {ConvertToType(tmpString.ToString)}")
                                End If
                                If commaIndex < 0 Then
                                    tmpString.Clear()
                                    Exit For
                                End If
                            Else
                                ElementList.Add(ConvertToType(tmpString.ToString).ToString)
                            End If
                            tmpString.Clear()
                            currentIndex = commaIndex + 1
                        End If
                    Case " " ' variable name
                        Dim commaIndex As Integer = CSharpNamedTypeString.IndexOf(",", currentIndex + 1, StringComparison.OrdinalIgnoreCase)
                        Dim TypePart As String = ConvertToType(tmpString.ToString).ToString
                        If IncludeName Then
                            currentIndex += 1
                            Dim name As String = CSharpNamedTypeString.ExtractName(currentIndex, commaIndex)
                            ElementList.Add($"{MakeVBSafeName(name)} As {TypePart}")
                        Else
                            ElementList.Add(TypePart)
                        End If
                        tmpString.Clear()
                        If commaIndex < 0 Then
                            Exit For
                        End If
                        currentIndex = commaIndex + 1
                    Case ","
                        ElementList.Add(ConvertToType(tmpString.ToString).ToString)
                        tmpString.Clear()
                        currentIndex += If(CSharpNamedTypeString(currentIndex + 1) = " ", 1, 0)
                    Case ")"
                        Dim TypePart As String = ConvertToType(tmpString.ToString).ToString
                        Dim commaIndex As Integer = CSharpNamedTypeString.IndexOf(",", currentIndex + 1, StringComparison.OrdinalIgnoreCase)
                        If IncludeName Then
                            currentIndex += 1
                            Dim name As String = CSharpNamedTypeString.ExtractName(currentIndex, commaIndex)
                            If commaIndex < 0 Then
                                ElementList.Add($"{MakeVBSafeName(name)} As {TypePart}")
                                Exit For
                            End If
                        Else
                            ElementList.Add(TypePart)
                        End If
                        Exit For
                    Case "?"
                        ElementList.Add("Object")
                        tmpString.Clear()
                        Dim commaIndex As Integer = CSharpNamedTypeString.IndexOf(",", currentIndex + 1, StringComparison.OrdinalIgnoreCase)
                        If commaIndex < 0 Then
                            Exit For
                        End If
                        currentIndex = commaIndex + 1
                    Case "("
                        If tmpString.Length <> 0 Then
                            Stop
                        End If
                        openParen += 1
                        tmpString.Append("("c)
                        While openParen <> 0
                            currentIndex += 1
                            currentChar = CSharpNamedTypeString(currentIndex)
                            Select Case currentChar
                                Case ")"
                                    openParen -= 1
                                    tmpString.Append(")"c)
                                    If openParen = 0 Then
                                        Exit While
                                    End If
                                Case "("
                                    openParen += 1
                                    tmpString.Append("("c)
                                Case Else
                                    tmpString.Append(currentChar)
                            End Select
                        End While
                        Dim subTupleList As New List(Of String)
                        subTupleList.AddRange(tmpString.ToString.Substring(1, tmpString.Length - 2).ConvertTupleToVBTypeStrings(IncludeName))
                        ElementList.Add($"({subTupleList.ToArray.JoinLines(", ")})")
                        tmpString.Clear()
                        Dim commaIndex As Integer = CSharpNamedTypeString.IndexOf(",", currentIndex + 1, StringComparison.OrdinalIgnoreCase)
                        If commaIndex < 0 Then
                            Exit For
                        End If
                        If commaIndex = -1 Then
                            Exit For
                        End If
                        currentIndex = commaIndex + 1
                    Case Else
                        tmpString.Append(currentChar)
                End Select
            Next
            If tmpString.Length > 0 Then
                Dim TypePart As String = ConvertToType(tmpString.ToString).ToString
                ElementList.Add(TypePart)
            End If
            Return ElementList
        End Function

    End Module

End Namespace
