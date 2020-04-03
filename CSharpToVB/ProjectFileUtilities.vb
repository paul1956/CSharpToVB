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
        Dim DestFileNameWithPath As String = Path.Combine(ProjectSavePath, PartialPathWithFileName)
        Directory.CreateDirectory(Path.GetDirectoryName(DestFileNameWithPath))
        File.Copy(Path.Combine(New FileInfo(currentProject).Directory.FullName, PartialPathWithFileName), DestFileNameWithPath, overwrite:=True)
    End Sub

    Public Function ConvertProjectFile(ProjectSavePath As String, sourceFilePath As String, xmlDoc As XmlDocument) As XmlDocument

        If xmlDoc Is Nothing Then
            Throw New ArgumentNullException(NameOf(xmlDoc))
        End If
        Dim NodesToBeRemoved As New List(Of (PropertyIndex As Integer, ChildIndex As Integer))

        Dim IsDesktopProject As Boolean = xmlDoc.FirstChild.Attributes(0).Value = "Microsoft.NET.Sdk.WindowsDesktop"
        If xmlDoc.FirstChild.Attributes(0).Value.StartsWith("Microsoft.NET.Sdk", StringComparison.OrdinalIgnoreCase) Then
            Dim LeadingXMLSpace As XmlNode = xmlDoc.CreateDocumentFragment()
            LeadingXMLSpace.InnerXml = "    "

            If xmlDoc.FirstChild.HasChildNodes Then
                For i As Integer = 0 To xmlDoc.FirstChild.ChildNodes.Count - 1
                    Select Case xmlDoc.FirstChild.ChildNodes(i).Name
                        Case "#whitespace"
                            ' No change necessary
                        Case "PropertyGroup"
                            Dim PropertyGroupLastIndex As Integer = xmlDoc.FirstChild.ChildNodes(i).ChildNodes.Count - 1
                            For J As Integer = 0 To PropertyGroupLastIndex
                                Dim PropertyGroupChildNode As XmlNode = xmlDoc.FirstChild.ChildNodes(i).ChildNodes(J)
                                Select Case PropertyGroupChildNode.Name
                                    Case "AssemblyName", "CLSCompliant",
                                         "DefineConstants", "Deterministic",
                                         "GenerateAssemblyInfo", "Nullable",
                                         "OutputType", "ProduceReferenceAssembly",
                                         "RootNamespace", "StartupObject",
                                         "TargetFramework", "UsePublicApiAnalyzers",
                                         "UserSecretsId",
                                         "UseWindowsForms", "UseWpf",
                                         "Win32Manifest", "#whitespace"
                                       ' No change necessary
                                    Case "AllowUnsafeBlocks", "NoWarn"
                                        If J > 0 AndAlso xmlDoc.FirstChild.ChildNodes(i).ChildNodes(J - 1).Name = "#whitespace" Then
                                            NodesToBeRemoved.Add((i, J - 1))
                                        End If
                                        NodesToBeRemoved.Add((i, J))
                                        'If J + 1 < PropertyGroupLastIndex AndAlso xmlDoc.FirstChild.ChildNodes(i).ChildNodes(J + 1).Name = "#whitespace" Then
                                        '    J += 1
                                        '    NodesToBeRemoved.Add((i, J))
                                        'End If
                                    Case "LangVersion"
                                        If Not (PropertyGroupChildNode.InnerText.Equals("latest", StringComparison.OrdinalIgnoreCase) OrElse
                                           PropertyGroupChildNode.InnerText.Equals("default", StringComparison.OrdinalIgnoreCase) OrElse
                                           PropertyGroupChildNode.InnerText.StartsWith("$", StringComparison.OrdinalIgnoreCase)) Then
                                            PropertyGroupChildNode.InnerText = PropertyGroupChildNode.InnerText.Replace(PropertyGroupChildNode.InnerText, "latest", StringComparison.OrdinalIgnoreCase)
                                        End If
                                    Case "#comment"
                                        xmlDoc.FirstChild.ChildNodes(i).ChildNodes(J).InnerText = PropertyGroupChildNode.InnerText.Replace(".cs", ".vb", StringComparison.OrdinalIgnoreCase)
                                    Case Else
                                        Stop
                                End Select
                            Next J
                        Case "ItemGroup"
                            For J As Integer = 0 To xmlDoc.FirstChild.ChildNodes(i).ChildNodes.Count - 1
                                Dim xmlNode As XmlNode = xmlDoc.FirstChild.ChildNodes(i).ChildNodes(J)
                                Select Case xmlNode.Name
                                    Case "FrameworkReference", "#whitespace"
                                       ' No change necessary
                                    Case "#comment"
                                        xmlDoc.FirstChild.ChildNodes(i).ChildNodes(J).InnerText = xmlNode.InnerText.Replace(".cs", ".vb", StringComparison.OrdinalIgnoreCase)
                                    Case "Compile"
                                        Dim CompileValue As String = ""
                                        For k As Integer = 0 To xmlNode.Attributes.Count - 1
                                            CompileValue = xmlNode.Attributes(k).Value
                                            xmlDoc.FirstChild.ChildNodes(i).ChildNodes(J).Attributes(k).Value = ChangeExtension(CompileValue, "cs", "vb")
                                        Next
                                        For k As Integer = 0 To xmlNode.ChildNodes.Count - 1
                                            Select Case xmlNode.ChildNodes(k).Name
                                                Case "DependentUpon"
                                                    Dim DependentUponNodeValue As String = xmlNode.ChildNodes(k).ChildNodes(0).Value
                                                    If DependentUponNodeValue.EndsWith(".cs", StringComparison.OrdinalIgnoreCase) Then
                                                        xmlDoc.FirstChild.ChildNodes(i).ChildNodes(J).ChildNodes(k).ChildNodes(0).Value = ChangeExtension(DependentUponNodeValue, "cs", "vb")
                                                    Else
                                                        CopyFile(ProjectSavePath, sourceFilePath, Path.Combine(Path.GetDirectoryName(CompileValue), DependentUponNodeValue))
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
                                                        xmlDoc.FirstChild.ChildNodes(i).ChildNodes(J).ChildNodes(k).ChildNodes(l).Value = ChangeExtension(xmlNode.ChildNodes(k).ChildNodes(l).Value, "cs", "vb")
                                                    Next l
                                                Case "GenerateSource", "LogicalName", "Namespace", "#whitespace"
                                                    ' No change necessary
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
                                    Case "Folder", "Import", "None", "PackageReference"
                                        ' No change necessary
                                    Case "ProjectReference"
                                        xmlDoc.FirstChild.ChildNodes(i).ChildNodes(J).Attributes(0).Value = xmlNode.Attributes(0).Value.Replace(".csproj", ".vbproj", StringComparison.OrdinalIgnoreCase)
                                    Case Else
                                        Stop
                                End Select
                            Next J
                        Case "#comment"
                            xmlDoc.FirstChild.ChildNodes(i).Value = xmlDoc.FirstChild.ChildNodes(i).Value.Replace(".cs", ".vb", StringComparison.OrdinalIgnoreCase)
                        Case Else
                            Stop
                    End Select
                Next i

                If NodesToBeRemoved.Count > 0 Then
                    For i As Integer = NodesToBeRemoved.Count - 1 To 0 Step -1
                        xmlDoc.FirstChild.ChildNodes(NodesToBeRemoved(i).PropertyIndex).RemoveChild(xmlDoc.FirstChild.ChildNodes(NodesToBeRemoved(i).PropertyIndex).ChildNodes(NodesToBeRemoved(i).ChildIndex))
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
