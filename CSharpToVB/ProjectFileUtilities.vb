' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.IO
Imports System.Text
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
            Return
        End If
        Dim destFileNameWithPath As String = Path.Combine(ProjectSavePath, PartialPathWithFileName)
        Directory.CreateDirectory(Path.GetDirectoryName(destFileNameWithPath))
        File.Copy(Path.Combine(New FileInfo(currentProject).Directory.FullName, PartialPathWithFileName), destFileNameWithPath, overwrite:=True)
    End Sub

    Public Function ConvertProjectFile(ProjectSavePath As String, sourceFilePath As String, xmlDoc As XmlDocument) As XmlDocument

        If xmlDoc Is Nothing Then
            Throw New ArgumentNullException(NameOf(xmlDoc))
        End If
        Dim nodesToBeRemoved As New List(Of (PropertyIndex As Integer, ChildIndex As Integer))

        Dim isDesktopProject As Boolean = xmlDoc.FirstChild.Attributes(0).Value = "Microsoft.NET.Sdk.WindowsDesktop"
        If xmlDoc.FirstChild.Attributes(0).Value.StartsWith("Microsoft.NET.Sdk", StringComparison.OrdinalIgnoreCase) Then
            Dim leadingXMLSpace As XmlNode = xmlDoc.CreateDocumentFragment()
            leadingXMLSpace.InnerXml = "    "

            If xmlDoc.FirstChild.HasChildNodes Then
                For index As Integer = 0 To xmlDoc.FirstChild.ChildNodes.Count - 1
                    Select Case xmlDoc.FirstChild.ChildNodes(index).Name
                        Case "#whitespace"
                            ' No change necessary
                        Case "PropertyGroup"
                            For childIndex As Integer = 0 To xmlDoc.FirstChild.ChildNodes(index).ChildNodes.Count - 1
                                Dim propertyGroupChildNode As XmlNode = xmlDoc.FirstChild.ChildNodes(index).ChildNodes(childIndex)
                                Select Case propertyGroupChildNode.Name
                                    Case "AssemblyName", "CLSCompliant",
                                         "DefineConstants", "Deterministic",
                                         "GenerateAssemblyInfo",
                                         "Nullable", "OutputType",
                                         "ProduceReferenceAssembly",
                                         "RootNamespace", "StartupObject",
                                         "TargetFramework", "UsePublicApiAnalyzers",
                                         "UserSecretsId", "UseWindowsForms",
                                         "UseWpf", "Win32Manifest",
                                         "#whitespace"
                                       ' No change necessary
                                    Case "AllowUnsafeBlocks", "NoWarn"
                                        If childIndex > 0 AndAlso xmlDoc.FirstChild.ChildNodes(index).ChildNodes(childIndex - 1).Name = "#whitespace" Then
                                            nodesToBeRemoved.Add((index, childIndex - 1))
                                        End If
                                        nodesToBeRemoved.Add((index, childIndex))
                                    Case "LangVersion"
                                        If Not (propertyGroupChildNode.InnerText.Equals("latest", StringComparison.OrdinalIgnoreCase) OrElse
                                           propertyGroupChildNode.InnerText.Equals("default", StringComparison.OrdinalIgnoreCase) OrElse
                                           propertyGroupChildNode.InnerText.StartsWith("$", StringComparison.OrdinalIgnoreCase)) Then
                                            propertyGroupChildNode.InnerText = propertyGroupChildNode.InnerText.Replace(propertyGroupChildNode.InnerText, "latest", StringComparison.OrdinalIgnoreCase)
                                        End If
                                    Case "#comment"
                                        xmlDoc.FirstChild.ChildNodes(index).ChildNodes(childIndex).InnerText = propertyGroupChildNode.InnerText.Replace(".cs", ".vb", StringComparison.OrdinalIgnoreCase)
                                    Case Else
                                        Stop
                                End Select
                            Next childIndex
                        Case "ItemGroup"
                            For childIndex As Integer = 0 To xmlDoc.FirstChild.ChildNodes(index).ChildNodes.Count - 1
                                Dim xmlNode As XmlNode = xmlDoc.FirstChild.ChildNodes(index).ChildNodes(childIndex)
                                Select Case xmlNode.Name
                                    Case "FrameworkReference", "#whitespace"
                                       ' No change necessary
                                    Case "#comment"
                                        xmlDoc.FirstChild.ChildNodes(index).ChildNodes(childIndex).InnerText = xmlNode.InnerText.Replace(".cs", ".vb", StringComparison.OrdinalIgnoreCase)
                                    Case "Compile"
                                        Dim compileValue As String = ""
                                        For k As Integer = 0 To xmlNode.Attributes.Count - 1
                                            compileValue = xmlNode.Attributes(k).Value
                                            xmlDoc.FirstChild.ChildNodes(index).ChildNodes(childIndex).Attributes(k).Value = ChangeExtension(compileValue, "cs", "vb")
                                        Next
                                        For k As Integer = 0 To xmlNode.ChildNodes.Count - 1
                                            Select Case xmlNode.ChildNodes(k).Name
                                                Case "DependentUpon"
                                                    Dim dependentUponNodeValue As String = xmlNode.ChildNodes(k).ChildNodes(0).Value
                                                    If dependentUponNodeValue.EndsWith(".cs", StringComparison.OrdinalIgnoreCase) Then
                                                        xmlDoc.FirstChild.ChildNodes(index).ChildNodes(childIndex).ChildNodes(k).ChildNodes(0).Value = ChangeExtension(dependentUponNodeValue, "cs", "vb")
                                                    Else
                                                        CopyFile(ProjectSavePath, sourceFilePath, Path.Combine(Path.GetDirectoryName(compileValue), dependentUponNodeValue))
                                                    End If
                                                Case "#whitespace"
                                                    ' No change necessary
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
                                                        xmlDoc.FirstChild.ChildNodes(index).ChildNodes(childIndex).ChildNodes(k).ChildNodes(l).Value = ChangeExtension(xmlNode.ChildNodes(k).ChildNodes(l).Value, "cs", "vb")
                                                    Next l
                                                Case "GenerateSource", "LogicalName", "Namespace", "#whitespace"
                                                    ' No change necessary
                                                Case Else
                                                    Stop
                                            End Select
                                        Next k
                                    Case "Content"
                                        If Not String.IsNullOrWhiteSpace(sourceFilePath) Then
                                            Dim sourceFileName As String = Path.Combine(New FileInfo(sourceFilePath).Directory.FullName, xmlNode.Attributes(0).Value)
                                            If File.Exists(sourceFileName) Then
                                                File.Copy(sourceFileName, Path.Combine(ProjectSavePath, xmlNode.Attributes(0).Value), overwrite:=True)
                                            End If
                                        End If
                                    Case "Folder", "Import", "None", "PackageReference"
                                        ' No change necessary
                                    Case "ProjectReference"
                                        xmlDoc.FirstChild.ChildNodes(index).ChildNodes(childIndex).Attributes(0).Value = xmlNode.Attributes(0).Value.Replace(".csproj", ".vbproj", StringComparison.OrdinalIgnoreCase)
                                    Case Else
                                        Stop
                                End Select
                            Next childIndex
                        Case "#comment"
                            xmlDoc.FirstChild.ChildNodes(index).Value = xmlDoc.FirstChild.ChildNodes(index).Value.Replace(".cs", ".vb", StringComparison.OrdinalIgnoreCase)
                        Case Else
                            Stop
                    End Select
                Next index

                If nodesToBeRemoved.Count > 0 Then
                    For index As Integer = nodesToBeRemoved.Count - 1 To 0 Step -1
                        xmlDoc.FirstChild.ChildNodes(nodesToBeRemoved(index).PropertyIndex).RemoveChild(xmlDoc.FirstChild.ChildNodes(nodesToBeRemoved(index).PropertyIndex).ChildNodes(nodesToBeRemoved(index).ChildIndex))
                    Next
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
        Dim subpathFromProject As String = Path.GetDirectoryName(SourceDocumentFileNameWithPath).Replace(ProjectDirectory, "", StringComparison.OrdinalIgnoreCase).Trim("\"c)
        Dim pathToSaveDirectory As String = Path.Combine(ProjectSavePath, subpathFromProject)
        If Not Directory.Exists(pathToSaveDirectory) Then
            Directory.CreateDirectory(pathToSaveDirectory)
        End If
        Return pathToSaveDirectory
    End Function

    Friend Function CreateDirectoryIfNonexistent(SolutionRoot As String) As String
        If Not Directory.Exists(SolutionRoot) Then
            Directory.CreateDirectory(SolutionRoot)
        End If
        Return SolutionRoot
    End Function

End Module
