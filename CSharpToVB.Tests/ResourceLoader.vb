Option Explicit On
Option Infer Off
Option Strict On


' Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

Imports System
Imports System.IO
Imports System.Reflection
Imports System.Text

Namespace TestResources
    Friend Module ResourceLoader
        Private Function GetResourceStream(name As String) As Stream
            Dim assembly As Assembly = GetType(ResourceLoader).GetTypeInfo().Assembly

            Dim stream As Stream = assembly.GetManifestResourceStream(name)
            If stream Is Nothing Then
                Throw New InvalidOperationException($"Resource '{name}' not found in {assembly.FullName}.")
            End If

            Return stream
        End Function

        Private Function GetResourceBlob(name As String) As Byte()
            Using stream As Stream = GetResourceStream(name)
                Dim bytes As Byte() = New Byte(CInt(stream.Length - 1)) {}
                Using memoryStream As New MemoryStream(bytes)
                    stream.CopyTo(memoryStream)
                End Using

                Return bytes
            End Using
        End Function

        Public Function GetOrCreateResource(ByRef resource() As Byte, name As String) As Byte()
            If resource Is Nothing Then
                resource = GetResourceBlob(name)
            End If

            Return resource
        End Function

        Public Function GetOrCreateResource(ByRef resource As String, name As String) As String
            If resource Is Nothing Then
                Using stream As Stream = GetResourceStream(name)
                    Using streamReader As New StreamReader(stream, Encoding.UTF8, detectEncodingFromByteOrderMarks:=True)
                        resource = streamReader.ReadToEnd()
                    End Using
                End Using
            End If

            Return resource
        End Function
    End Module
End Namespace
