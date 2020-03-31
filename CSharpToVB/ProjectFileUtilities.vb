' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.IO
Imports System.Xml

Imports Microsoft.CodeAnalysis
Imports VBMsgBox

Public Module ProjectFileUtilities

    Private Function ChangeExtension(AttributeValue As String, OldExtension As String, NewExtension As String) As String
        Return If(AttributeValue.EndsWith($".{OldExtension}", StringComparison.OrdinalIgnoreCase),
               Path.ChangeExtension(AttributeValue, NewExtension),
               AttributeValue)
    End Function

    Private Sub CopyFile(ProjectSavePath As String, currentProject As String, PartialPathWithFileName As String)
        If String.IsNullOrWhiteSpace(ProjectSavePath) OrElse
                String.IsNullOrWhiteSpace(currentProject) OrElse
                String.IsNullOrWhiteSpace(PartialPathWithFileName) Then
            Stop
            Return
        End If
        Dim DestFileNameWithPath As String = Path.Combine(ProjectSavePath, PartialPathWithFileName)
        Directory.CreateDirectory(Path.GetDirectoryName(DestFileNameWithPath))
        File.Copy(Path.Combine(New FileInfo(currentProject).Directory.FullName, PartialPathWithFileName), DestFileNameWithPath, overwrite:=True)
    End Sub

    Public Function ConvertProjectFile(ProjectSavePath As String, sourceFilePath As String, xmlDoc As XmlDocument) As XmlDocument
        If xmlDoc Is Nothing Then
            Throw New ArgumentNullException(NameOf(xmlDoc))
        End If

        Dim IsDesktopProject As Boolean = xmlDoc.FirstChild.Attributes(0).Value = "Microsoft.NET.Sdk.WindowsDesktop"
        If xmlDoc.FirstChild.Attributes(0).Value.StartsWith("Microsoft.NET.Sdk", StringComparison.OrdinalIgnoreCase) OrElse IsDesktopProject Then
            Dim LeadingXMLSpace As XmlNode = xmlDoc.CreateDocumentFragment()
            LeadingXMLSpace.InnerXml = "    "

            If xmlDoc.FirstChild.HasChildNodes Then
                Dim RemoveNode As XmlNode = Nothing
                For i As Integer = 0 To xmlDoc.FirstChild.ChildNodes.Count - 1
                    Dim RootChildNode As XmlNode = xmlDoc.FirstChild.ChildNodes(i)
                    Select Case RootChildNode.Name
                        Case "PropertyGroup"
                            For J As Integer = 0 To RootChildNode.ChildNodes.Count - 1
                                Dim PropertyGroupChildNode As XmlNode = RootChildNode.ChildNodes(J)
                                Select Case PropertyGroupChildNode.Name
                                    Case "OutputType"
                                        ' No change neccessary
                                    Case "TargetFramework"
                                         ' No change neccessary
                                    Case "RootNamespace"
                                        ' No change neccessary
                                    Case "AssemblyName"
                                        ' No change neccessary
                                    Case "GenerateAssemblyInfo"
                                        ' No change neccessary
                                    Case "UserSecretsId"
                                        ' No change neccessary
                                    Case "AllowUnsafeBlocks"
                                        ' No change neccessary"
                                    Case "UseWindowsForms"
                                        ' No change neccessary
                                    Case "UseWpf"
                                        ' No change neccessary
                                    Case "LangVersion"
                                        If PropertyGroupChildNode.InnerText.Equals("latest", StringComparison.OrdinalIgnoreCase) OrElse
                                           PropertyGroupChildNode.InnerText.Equals("default", StringComparison.OrdinalIgnoreCase) Then
                                            PropertyGroupChildNode.InnerText = "latest"
                                        End If
                                    Case "#whitespace"
                                        ' Capture leading space
                                        If LeadingXMLSpace.InnerXml.Length = 0 Then
                                            LeadingXMLSpace.InnerXml = PropertyGroupChildNode.InnerXml
                                        End If
                                    Case "#comment"
                                        CType(xmlDoc.FirstChild, XmlNode).ChildNodes(i).ChildNodes(J).Value = PropertyGroupChildNode.Value.Replace(".cs", ".vb", StringComparison.OrdinalIgnoreCase)
                                    Case Else
                                        Stop
                                End Select
                            Next J
                        Case "ItemGroup"
                            For J As Integer = 0 To RootChildNode.ChildNodes.Count - 1
                                Dim xmlNode As XmlNode = xmlDoc.FirstChild.ChildNodes(i).ChildNodes(J)
                                Select Case RootChildNode.ChildNodes(J).Name
                                        ' No change neccessary
                                    Case "FrameworkReference"
                                    Case "#whitespace"
                                        ' No change neccessary
                                    Case "#comment"
                                        CType(xmlDoc.FirstChild, XmlNode).ChildNodes(i).ChildNodes(J).Value = xmlNode.Value.Replace(".cs", ".vb", StringComparison.OrdinalIgnoreCase)
                                    Case "Compile"
                                        Dim CompileValue As String = ""
                                        For k As Integer = 0 To xmlNode.Attributes.Count - 1
                                            CompileValue = xmlNode.Attributes(k).Value
                                            CType(xmlDoc.FirstChild, XmlNode).ChildNodes(i).ChildNodes(J).Attributes(k).Value = ChangeExtension(CompileValue, "cs", "vb")
                                        Next k
                                        For k As Integer = 0 To xmlNode.ChildNodes.Count - 1
                                            Select Case xmlDoc.FirstChild.ChildNodes(i).ChildNodes(J).ChildNodes(k).Name
                                                Case "DependentUpon"
                                                    Dim DependentUponNodeValue As String = xmlNode.ChildNodes(k).ChildNodes(0).Value
                                                    If DependentUponNodeValue.EndsWith(".cs", StringComparison.OrdinalIgnoreCase) Then
                                                        CType(xmlDoc.FirstChild, XmlNode).ChildNodes(i).ChildNodes(J).ChildNodes(k).ChildNodes(0).Value = ChangeExtension(DependentUponNodeValue, "cs", "vb")
                                                    Else
                                                        CopyFile(ProjectSavePath, sourceFilePath, Path.Combine(Path.GetDirectoryName(CompileValue), DependentUponNodeValue))
                                                    End If
                                                Case "#whitespace"
                                                    ' Ignore
                                                Case Else
                                                    Stop
                                            End Select
                                        Next k
                                    Case "EmbeddedResource"
                                        If xmlNode.Attributes(0).Value.EndsWith(".resx", StringComparison.OrdinalIgnoreCase) Then
                                            CopyFile(ProjectSavePath, sourceFilePath, xmlNode.Attributes(0).Value)
                                        End If
                                        For k As Integer = 0 To xmlNode.ChildNodes.Count - 1
                                            Select Case xmlNode.ChildNodes(k).Name
                                                Case "DependentUpon"
                                                    For l As Integer = 0 To xmlNode.ChildNodes(k).ChildNodes.Count - 1
                                                        CType(xmlDoc.FirstChild, XmlNode).ChildNodes(i).ChildNodes(J).ChildNodes(k).ChildNodes(l).Value = ChangeExtension(xmlNode.ChildNodes(k).ChildNodes(l).Value, "cs", "vb")
                                                    Next l
                                                Case "#whitespace"
                                                    ' Ignore
                                                Case Else
                                                    Stop
                                            End Select
                                        Next k
                                    Case "Content"
                                        If Not String.IsNullOrWhiteSpace(sourceFilePath) Then
                                            Dim SourceFileName As String = Path.Combine(New FileInfo(sourceFilePath).Directory.FullName, xmlNode.Attributes(0).Value)
                                            If File.Exists(SourceFileName) Then
                                                File.Copy(SourceFileName, Path.Combine(ProjectSavePath, xmlNode.Attributes(0).Value), overwrite:=True)
                                            End If
                                        End If
                                    Case "None"
                                        ' No change neccessary
                                    Case "PackageReference"
                                        ' No change neccessary
                                    Case "ProjectReference"
                                        xmlNode.Attributes(0).Value = xmlNode.Attributes(0).Value.Replace(".csproj", ".vbproj", StringComparison.OrdinalIgnoreCase)
                                    Case "Folder"
                                        ' No change neccessary
                                    Case Else
                                        Stop
                                End Select
                            Next J
                        Case "#whitespace"
                        Case "#comment"
                            CType(xmlDoc.FirstChild, XmlNode).ChildNodes(i).Value = RootChildNode.Value.Replace(".cs", ".vb", StringComparison.OrdinalIgnoreCase)
                        Case Else
                            Stop
                    End Select
                Next i
                If RemoveNode IsNot Nothing Then
                    RemoveNode.RemoveAll()
                End If
            End If
            If Not String.IsNullOrWhiteSpace(ProjectSavePath) Then
                xmlDoc.Save(Path.Combine(ProjectSavePath, New FileInfo(sourceFilePath).Name.Replace(".csproj", ".vbproj", StringComparison.OrdinalIgnoreCase)))
            End If
        Else
            MsgBox("Project conversion only support SDK style projects, project file will not be converted!", MsgBoxStyle.Information)
        End If
        Return xmlDoc
    End Function

    Friend Function DestinationFilePath(ProjectDirectory As String, ProjectSavePath As String, SourceDocumentFileNameWithPath As String
                                        ) As String
        If String.IsNullOrWhiteSpace(ProjectSavePath) Then
            Return String.Empty
        End If
        Dim SubPathFromProject As String = Path.GetDirectoryName(SourceDocumentFileNameWithPath).Replace(ProjectDirectory, "", StringComparison.OrdinalIgnoreCase).Trim("\"c)
        Dim PathToSaveDirectory As String = Path.Combine(ProjectSavePath, SubPathFromProject)
        If Not Directory.Exists(PathToSaveDirectory) Then
            Directory.CreateDirectory(PathToSaveDirectory)
        End If
        Return PathToSaveDirectory
    End Function

    Friend Function CreateDirectoryIfNonexistent(SolutionRoot As String) As String
        If Not Directory.Exists(SolutionRoot) Then
            Directory.CreateDirectory(SolutionRoot)
        End If
        Return SolutionRoot
    End Function

End Module
