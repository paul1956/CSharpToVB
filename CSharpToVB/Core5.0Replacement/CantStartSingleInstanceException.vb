' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.ComponentModel
Imports System.Runtime.Serialization

''' <summary>
''' Exception for when we launch a single-instance application and it can't connect with the
''' original instance.
''' </summary>
''' <remarks></remarks>
<EditorBrowsable(EditorBrowsableState.Never)>
<Serializable()>
Public Class CantStartSingleInstanceException : Inherits Exception

    ' De-serialization constructor must be defined since we are serializable
    <EditorBrowsable(EditorBrowsableState.Advanced)>
    Protected Sub New(ByVal info As SerializationInfo, ByVal context As StreamingContext)
        MyBase.New(info, context)
    End Sub

    ''' <summary>
    '''  Creates a new exception
    ''' </summary>
    Public Sub New()
        MyBase.New("Can't Start Single Instance Application")
    End Sub

    Public Sub New(message As String)
        MyBase.New(message)
    End Sub

    Public Sub New(message As String, innerException As Exception)
        MyBase.New(message, innerException)
    End Sub

End Class
