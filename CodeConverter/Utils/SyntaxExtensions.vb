' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
Option Explicit On
Option Infer Off
Option Strict On

Imports System.Runtime.CompilerServices

Imports Microsoft.CodeAnalysis
Imports CS = Microsoft.CodeAnalysis.CSharp

Namespace CSharpToVBCodeConverter.Util
    Public Module SyntaxExtensions

        <Extension()>
        Public Function IsParentKind(node As SyntaxNode, kind As CS.SyntaxKind) As Boolean
            Return node IsNot Nothing AndAlso node.Parent.IsKind(kind)
        End Function

    End Module
End Namespace
