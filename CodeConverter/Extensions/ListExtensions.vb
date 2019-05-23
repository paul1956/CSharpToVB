Option Explicit On
Option Infer Off
Option Strict On

Imports System.Runtime.CompilerServices

Imports VBS = Microsoft.CodeAnalysis.VisualBasic.Syntax

Public Module ListExtensions

    <Extension>
    Public Function ContainsName(ImportList As List(Of VBS.ImportsStatementSyntax), ImportName As String) As Boolean
        For Each ImportToCheck As VBS.ImportsStatementSyntax In ImportList
            For Each Clause As VBS.ImportsClauseSyntax In ImportToCheck.ImportsClauses
                If Clause.ToString = ImportName Then
                    Return True
                End If
            Next
        Next
        Return False
    End Function

End Module