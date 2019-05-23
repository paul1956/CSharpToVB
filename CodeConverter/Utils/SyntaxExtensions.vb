Option Explicit On
Option Infer Off
Option Strict On

Imports System.Runtime.CompilerServices

Imports Microsoft.CodeAnalysis
Imports CS = Microsoft.CodeAnalysis.CSharp

Namespace IVisualBasicCode.CodeConverter.Util
    Public Module SyntaxExtensions

        <Extension()>
        Public Function IsParentKind(node As SyntaxNode, kind As CS.SyntaxKind) As Boolean
            Return node IsNot Nothing AndAlso node.Parent.IsKind(kind)
        End Function

        <Extension()>
        Public Function IsParentKind(node As SyntaxToken, kind As CS.SyntaxKind) As Boolean
            Return node.Parent IsNot Nothing AndAlso node.Parent.IsKind(kind)
        End Function

    End Module
End Namespace