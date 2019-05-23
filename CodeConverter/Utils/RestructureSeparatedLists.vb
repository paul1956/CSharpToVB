Option Explicit On
Option Infer Off
Option Strict On

Imports IVisualBasicCode.CodeConverter.Util

Imports Microsoft.CodeAnalysis

Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBFactory = Microsoft.CodeAnalysis.VisualBasic.SyntaxFactory

Public Module RestuructureSeparatedLists
    Public IgnoredIfDepth As Integer = 0

    Public Sub RestructureNodesAndSeparators(Of T As VB.VisualBasicSyntaxNode)(ByRef _OpenToken As SyntaxToken, ByRef Items As List(Of T), ByRef Separators As List(Of SyntaxToken), ByRef _CloseToken As SyntaxToken)
        Dim TokenLeadingTrivia As New List(Of SyntaxTrivia)
        _OpenToken = _OpenToken.WithModifiedTokenTrivia(True)
        For i As Integer = 0 To Items.Count - 2
            Items(i) = CType(Items(i).WithModifiedNodeTrivia(SeparatorFollows:=True), T)
            Separators(i) = Separators(i).WithModifiedTokenTrivia(False)
        Next
        If Items.Count > 0 Then
            Items(Items.Count - 1) = CType(Items.Last.WithModifiedNodeTrivia(SeparatorFollows:=False), T)
        End If

        _CloseToken = _CloseToken.WithModifiedTokenTrivia(LeadingToken:=False)
    End Sub

End Module