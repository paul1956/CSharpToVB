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
        Dim DestFileNameWithPath As String = Path.Combine(ProjectSavePath, PartialPathWithFileName)
        Directory.CreateDirectory(Path.GetDirectoryName(DestFileNameWithPath))
        File.Copy(Path.Combine(New FileInfo(currentProject).Directory.FullName, PartialPathWithFileName), DestFileNameWithPath, overwrite:=True)
    End Sub

    Friend Sub ConvertProjectFile(ProjectSavePath As String, currentProject As Project, xmlDoc As XmlDocument, root As XmlNode)
        Debug.Assert(root IsNot Nothing)

        Dim IsDesktopProject As Boolean = root.Attributes(0).Value = "Microsoft.NET.Sdk.WindowsDesktop"
        If root.Attributes(0).Value = "Microsoft.NET.Sdk" OrElse IsDesktopProject Then
            Dim FoundUseWindowsFormsWpf As Boolean = False
            Dim FrameworkReferenceNodeIndex As Integer = -1
            Dim PropertyGroupIndex As Integer = 0
            Dim LeadingXMLSpace As XmlNode = xmlDoc.CreateDocumentFragment()
            LeadingXMLSpace.InnerXml = "    "
            If root.HasChildNodes Then
                Dim RemoveNode As XmlNode = Nothing
                For i As Integer = 0 To root.ChildNodes.Count - 1
                    Dim RootChildNode As XmlNode = root.ChildNodes(i)
                    Select Case RootChildNode.Name
                        Case "PropertyGroup"
                            PropertyGroupIndex = i
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
                                    Case "UseWindowsForms"
                                        FoundUseWindowsFormsWpf = True
                                    Case "UseWpf"
                                        FoundUseWindowsFormsWpf = True
                                    Case "#whitespace"
                                        ' Capture leading space
                                        If LeadingXMLSpace.InnerXml.Length = 0 Then
                                            LeadingXMLSpace.InnerXml = PropertyGroupChildNode.InnerXml
                                        End If
                                    Case "#comment"
                                        root.ChildNodes(i).ChildNodes(J).Value = PropertyGroupChildNode.Value.Replace(".cs", ".vb", StringComparison.OrdinalIgnoreCase)
                                    Case Else
                                        Stop
                                End Select
                            Next J
                        Case "ItemGroup"
                            For J As Integer = 0 To RootChildNode.ChildNodes.Count - 1
                                Dim xmlNode As XmlNode = root.ChildNodes(i).ChildNodes(J)
                                Select Case RootChildNode.ChildNodes(J).Name
                                    Case "FrameworkReference"
                                        If RootChildNode.ChildNodes.Count = 3 AndAlso
                                            RootChildNode.ChildNodes(0).Name = "#whitespace" AndAlso
                                            RootChildNode.ChildNodes(0).Name = "#whitespace" Then
                                            FrameworkReferenceNodeIndex = i
                                        Else
                                            RemoveNode = xmlNode
                                        End If
                                    Case "#whitespace"
                                                            ' Ignore
                                    Case "#comment"
                                        root.ChildNodes(i).ChildNodes(J).Value = xmlNode.Value.Replace(".cs", ".vb", StringComparison.OrdinalIgnoreCase)
                                    Case "Compile"
                                        Dim CompileValue As String = ""
                                        For k As Integer = 0 To xmlNode.Attributes.Count - 1
                                            CompileValue = xmlNode.Attributes(k).Value
                                            root.ChildNodes(i).ChildNodes(J).Attributes(k).Value = ChangeExtension(CompileValue, "cs", "vb")
                                        Next k
                                        For k As Integer = 0 To xmlNode.ChildNodes.Count - 1
                                            Select Case root.ChildNodes(i).ChildNodes(J).ChildNodes(k).Name
                                                Case "DependentUpon"
                                                    Dim DependentUponNodeValue As String = xmlNode.ChildNodes(k).ChildNodes(0).Value
                                                    If DependentUponNodeValue.EndsWith(".cs", StringComparison.OrdinalIgnoreCase) Then
                                                        root.ChildNodes(i).ChildNodes(J).ChildNodes(k).ChildNodes(0).Value = ChangeExtension(DependentUponNodeValue, "cs", "vb")
                                                    Else
                                                        CopyFile(ProjectSavePath, currentProject.FilePath, Path.Combine(Path.GetDirectoryName(CompileValue), DependentUponNodeValue))
                                                    End If
                                                Case "#whitespace"
                                                    ' Ignore
                                                Case Else
                                                    Stop
                                            End Select
                                        Next k
                                    Case "EmbeddedResource"
                                        If xmlNode.Attributes(0).Value.EndsWith(".resx", StringComparison.OrdinalIgnoreCase) Then
                                            CopyFile(ProjectSavePath, currentProject.FilePath, xmlNode.Attributes(0).Value)
                                        End If
                                        For k As Integer = 0 To xmlNode.ChildNodes.Count - 1
                                            Select Case xmlNode.ChildNodes(k).Name
                                                Case "DependentUpon"
                                                    For l As Integer = 0 To xmlNode.ChildNodes(k).ChildNodes.Count - 1
                                                        root.ChildNodes(i).ChildNodes(J).ChildNodes(k).ChildNodes(l).Value = ChangeExtension(xmlNode.ChildNodes(k).ChildNodes(l).Value, "cs", "vb")
                                                    Next l
                                                Case "#whitespace"
                                                    ' Ignore
                                                Case Else
                                                    Stop
                                            End Select
                                        Next k
                                    Case "PackageReference"
                                        ' Ignore
                                    Case "ProjectReference"
                                        xmlNode.Attributes(0).Value = xmlNode.Attributes(0).Value.Replace(".csproj", ".vbproj", StringComparison.OrdinalIgnoreCase)
                                    Case "Folder"
                                        ' Ignore
                                    Case "Content"
                                        If xmlNode.Attributes(0).Name.ToUpperInvariant = "INCLUDE" Then
                                            Dim SourceFileName As String = Path.Combine(New FileInfo(currentProject.FilePath).Directory.FullName, xmlNode.Attributes(0).Value)
                                            If File.Exists(SourceFileName) Then
                                                File.Copy(SourceFileName, Path.Combine(ProjectSavePath, xmlNode.Attributes(0).Value), overwrite:=True)
                                            Else
                                                If (Path.GetExtension(SourceFileName).ToUpperInvariant = ".TXT") Then
                                                    Dim NewValue As String = ChangeExtension(xmlNode.Attributes(0).Value, "TXT", "md")
                                                    SourceFileName = Path.ChangeExtension(SourceFileName, "md")
                                                    If File.Exists(SourceFileName) Then
                                                        File.Copy(SourceFileName, Path.Combine(ProjectSavePath, NewValue), overwrite:=True)
                                                        root.ChildNodes(i).ChildNodes(J).Attributes(0).Value = NewValue
                                                        xmlNode.Attributes(0).Value = NewValue
                                                    End If
                                                End If
                                            End If
                                        End If
                                    Case Else
                                        Stop
                                End Select
                            Next J
                        Case "#whitespace"
                        Case "#comment"
                            root.ChildNodes(i).Value = RootChildNode.Value.Replace(".cs", ".vb", StringComparison.OrdinalIgnoreCase)
                        Case Else
                            Stop
                    End Select
                Next i
                If RemoveNode IsNot Nothing Then
                    RemoveNode.RemoveAll()
                End If
            End If
            If FrameworkReferenceNodeIndex >= 0 Then
                root.Attributes(0).Value = "Microsoft.NET.Sdk.WindowsDesktop"
                root.ChildNodes(FrameworkReferenceNodeIndex).RemoveAll()
            End If
            If Not FoundUseWindowsFormsWpf Then
                Dim xmlDocFragment As XmlDocumentFragment = xmlDoc.CreateDocumentFragment()
                xmlDocFragment.InnerXml = "<UseWindowsForms>true</UseWindowsForms>"                                                                '<UseWindowsForms>true</UseWindowsForms>
                root.ChildNodes(PropertyGroupIndex).AppendChild(LeadingXMLSpace)
                root.ChildNodes(PropertyGroupIndex).AppendChild(xmlDocFragment)
            End If

            xmlDoc.Save(Path.Combine(ProjectSavePath, New FileInfo(currentProject.FilePath).Name.Replace(".csproj", ".vbproj", StringComparison.OrdinalIgnoreCase)))
        Else
            msgbox("Project conversion only support SDK style projects")
        End If
    End Sub

    Friend Function DestinationFilePath(ProjectDirectory As String, ProjectSavePath As String, DocumentName As Document) As String
        If String.IsNullOrWhiteSpace(ProjectSavePath) Then
            Return String.Empty
        End If
        Dim SubPathFromProject As String = Path.GetDirectoryName(DocumentName.FilePath).Replace(ProjectDirectory, "", StringComparison.OrdinalIgnoreCase).Trim("\"c)
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
