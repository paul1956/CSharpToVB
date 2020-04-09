' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Xml.Serialization
Namespace Microsoft.VisualBasic.ApplicationServices
    Public Class NamedPipeXMLData
        Sub New()

        End Sub

        ''' <summary>
        '''     A list of command line arguments.
        ''' </summary>
        <XmlElement("CommandLineArguments")>
        Friend Property CommandLineArguments As New List(Of String)

    End Class
End Namespace
