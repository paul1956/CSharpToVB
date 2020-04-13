' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.Xml.Serialization
Namespace Microsoft.VisualBasic.ApplicationServices
    Public Class NamedPipeXMLData
        Sub New()

        End Sub

#Disable Warning CA2227 ' Collection properties should be read only
        ''' <summary>
        '''     A list of command line arguments.
        ''' </summary>
        <XmlElement("CommandLineArguments")>
        Public Property CommandLineArguments As New List(Of String)
#Enable Warning CA2227 ' Collection properties should be read only

    End Class
End Namespace
