Imports System.Runtime.CompilerServices

Imports Microsoft.CodeAnalysis

Public Module SymbolInfoExtensions

    <Extension>
    Public Function GetAnySymbol(ByVal info As SymbolInfo) As ISymbol
        Return If(info.Symbol, info.CandidateSymbols.FirstOrDefault())
    End Function

End Module