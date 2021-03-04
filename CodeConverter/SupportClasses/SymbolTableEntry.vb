' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Namespace SupportClasses

    Public Class SymbolTableEntry

        Friend Sub New(name As String, isType As Boolean, isProperty As Boolean)
            Me.Name = name
            Me.IsType = isType
            Me.IsProperty = isProperty
        End Sub

        Public ReadOnly Property IsProperty As Boolean
        Public ReadOnly Property IsType As Boolean
        Public ReadOnly Property Name As String
    End Class

End Namespace
