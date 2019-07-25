' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.
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