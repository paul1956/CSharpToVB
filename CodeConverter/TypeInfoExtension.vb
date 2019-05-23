Imports System.Runtime.CompilerServices
Imports Microsoft.CodeAnalysis

Public Module TypeInfoExtension
    <Extension>
    Public Function IsErrorType(ByVal symbol As ITypeSymbol) As Boolean
        Return CBool(symbol?.TypeKind = TypeKind.Error)
    End Function

End Module
