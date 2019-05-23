Option Explicit On
Option Infer Off
Option Strict On

Imports System.Runtime.CompilerServices

Namespace IVisualBasicCode.CodeConverter.Util

    Partial Public Module EnumerableExtensions

        <Extension>
        Public Function Contains(Of T)(sequence As IEnumerable(Of T), predicate As Func(Of T, Boolean)) As Boolean
            Return sequence.Any(predicate)
        End Function

        <Extension()>
        Public Function FirstOrNullable(Of T As Structure)(source As IEnumerable(Of T), predicate As Func(Of T, Boolean)) As T?
            If source Is Nothing Then
                Throw New ArgumentNullException(NameOf(source))
            End If

            Return source.Cast(Of T?)().FirstOrDefault(Function(v As T?) predicate(v.Value))
        End Function

    End Module
End Namespace