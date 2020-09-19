' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Namespace CSharpToVBConverter

    Public Class SymbolTableEntry

        Friend Sub New(Name As String, IsType As Boolean, IsProperty As Boolean)
            Me.Name = Name
            Me.IsType = IsType
            Me.isProperty = IsProperty
        End Sub

        Public ReadOnly Property isProperty As Boolean
        Public ReadOnly Property IsType As Boolean
        Public ReadOnly Property Name As String
    End Class

End Namespace
