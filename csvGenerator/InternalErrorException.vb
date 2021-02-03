' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.


Friend NotInheritable Class InternalErrorException
    Inherits Exception

    Public Sub New(message As String)
        MyBase.New(message)
    End Sub

    Public Sub New(message As String, innerException As Exception)
        MyBase.New(message, innerException)
    End Sub

    ' default constructor
    Public Sub New()
        MyBase.New("Internal error in the Microsoft Visual Basic runtime.")
    End Sub

End Class
