' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Option Strict On
Option Explicit On

Imports System.IO
Imports System.IO.Pipes
Imports System.Runtime.Serialization
Imports System.Xml

Namespace Microsoft.VisualBasic.ApplicationServices

    Friend Module SingleInstanceHelpers
        Private Const NamedPipeOptions As PipeOptions = PipeOptions.CurrentUserOnly

        Private Sub BeginWaitForClientConnectionCallback(pipeServer As NamedPipeServerStream, asyncResult As IAsyncResult, callback As Action(Of String()))
            Dim args As String() = Nothing
            Try
                pipeServer.EndWaitForConnection(asyncResult)
                Try
                    Dim serializer As New DataContractSerializer(GetType(String()))
                    Try
                        args = DirectCast(serializer.ReadObject(pipeServer), String())
                    Catch ex As XmlException
                    End Try
                Finally
                    pipeServer.Disconnect()
                End Try
                WaitForClientConnectionAsync(pipeServer, callback)
            Catch ex As IOException
            Catch ex As ObjectDisposedException
            End Try
            If args IsNot Nothing Then
                callback(args)
            End If
        End Sub

        Friend Function CreatePipeServer(pipeName As String) As NamedPipeServerStream
            Try
                Return New NamedPipeServerStream(
                        pipeName:=pipeName,
                        direction:=PipeDirection.In,
                        maxNumberOfServerInstances:=1,
                        transmissionMode:=PipeTransmissionMode.Byte,
                        options:=NamedPipeOptions)
            Catch ex As IOException
                Return Nothing
            End Try
        End Function

        Friend Function SendSecondInstanceArgs(pipeName As String, timeout As Integer, args As String()) As Boolean
            Try
                Using pipeClient As New NamedPipeClientStream(
                            serverName:=".",
                            pipeName:=pipeName,
                            direction:=PipeDirection.Out,
                            options:=NamedPipeOptions)
                    pipeClient.Connect(timeout)
                    Dim serializer As New DataContractSerializer(GetType(String()))
                    serializer.WriteObject(pipeClient, args)
                End Using
                Return True
            Catch ex As TimeoutException
            Catch ex As IOException
            End Try
            Return False
        End Function

        Friend Sub WaitForClientConnectionAsync(pipeServer As NamedPipeServerStream, callback As Action(Of String()))
            pipeServer.BeginWaitForConnection(Sub(asyncResult As IAsyncResult) BeginWaitForClientConnectionCallback(pipeServer, asyncResult, callback), Nothing)
        End Sub

    End Module

End Namespace
