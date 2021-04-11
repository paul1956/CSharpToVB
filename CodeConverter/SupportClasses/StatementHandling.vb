' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports VB = Microsoft.CodeAnalysis.VisualBasic
Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Public Class StatementHandling

    Public Sub New(index As Integer, statement As VB.VisualBasicSyntaxNode, statementHandling As StatementHandlingOption)
        _Index = index
        _VbNode = statement
        _HandlingOption = statementHandling
    End Sub

    Property Index As Integer

    ReadOnly Property Statement As VBS.StatementSyntax
        Get
            Return TryCast(_VbNode, VBS.StatementSyntax)
        End Get
    End Property

    Property VbNode As VB.VisualBasicSyntaxNode
    Property HandlingOption As StatementHandlingOption
End Class
