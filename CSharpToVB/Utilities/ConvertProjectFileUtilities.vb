' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.IO
Imports System.Xml

Imports VBMsgBox

Public Module ConvertProjectFileUtilities

    Private ReadOnly s_compileChildNodeIgnoreList As New List(Of String)(
            {"AutoGen", "DesignTime", "DesignTimeSharedInput",
             "#whitespace"})

    Private ReadOnly s_documentTopLevelIgnoreList As New List(Of String)(
            {"_DependentAssemblyVersionsFile",
         "_DependsOn",
         "_DotNetSdkVersionFile",
         "_NuGetRepackAssembly",
         "_OptimizedDependenciesDir",
         "ApplyNgenOptimization",
         "AssemblyName",
         "AssemblyVersion",
         "AutoGenerateAssemblyVersion",
         "AutoGenerateBindingRedirects",
         "AutomaticBindingRedirects",
         "CLSCompliant",
         "CodeAnalysisRuleSet",
         "CopyLocalLockFileAssemblies",
         "CreateVsixContainer",
         "DebugType",
         "DefaultLanguage",
         "DefineConstants",
         "DeployExtension",
         "Deterministic",
         "DevelopmentDependency",
         "DisableImplicitFrameworkReferences",
         "EnableDefaultItems",
         "ExcludeFromSourceBuild",
         "ExpectedDependency",
         "ExtensionInstallationFolder",
         "ExtensionInstallationRoot",
         "GenerateAssemblyInfo",
         "GenerateDependencyFile",
         "GenerateDocumentationFile",
         "GenerateDocumentionFile",
         "GenerateMicrosoftCodeAnalysisCommitHashAttribute",
         "GeneratePerformanceSensitiveAttribute",
         "GeneratePkgDefFile",
         "ImplicitlyExpandNETStandardFacades",
         "Import",
         "IncludeAssemblyInVSIXContainer",
         "IncludeBuildOutput",
         "IncludeCopyLocalReferencesInVSIXContainer",
         "IncludeDebugSymbolsInLocalVSIXDeployment",
         "IncludeDebugSymbolsInVSIXContainer",
         "IsAnalyzer",
         "IsPackable",
         "IsShipping",
         "IsSourcePackage",
         "IsVisualStudioBuildPackage",
         "LargeAddressAware",
         "MinimumVisualStudioVersion",
         "NoStdLib",
         "Nullable",
         "NuspecPackageId",
         "OutputType",
         "PackageDescription",
         "PackageId",
         "Platform",
         "Platforms",
         "PlatformTarget",
         "Prefer32Bit",
         "ProduceReferenceAssembly",
         "ProjectExtensions",
         "ResolveAssemblyWarnOrErrorOnTargetArchitectureMismatch",
         "RootNamespace",
         "RoslynProjectType",
         "RuntimeIdentifiers",
         "ServerGarbageCollection",
         "ServiceHubAssemblyBasePath",
         "SignAssembly",
         "SkipTests",
         "StartupObject",
         "Target",
         "TargetFramework",
         "TargetFrameworks",
         "TargetsForTfmSpecificContentInPackage",
         "TargetVsixContainerName",
         "TieredCompilation",
         "UseAppHost",
         "UseCodebase",
         "UsePublicApiAnalyzers",
         "UserSecretsId",
         "UseVSHostingProcess",
         "UseWindowsForms",
         "UseWpf",
         "Using",
         "UsingTask",
         "VisualStudioInsertionComponent",
         "Win32Manifest",
         "#whitespace"})

    Private ReadOnly s_embeddedResourceIgnoreList As New List(Of String)(
        {"Generator", "GenerateSource", "LastGenOutput", "LogicalName",
         "ManifestResourceName", "MergeWithCTO", "Namespace",
         "SubType", "#whitespace"})

    Private ReadOnly s_itemGroupIgnoreList As New List(Of String)(
        {"DotNetCliToolReference", "ExpectedDependency",
         "Folder", "FrameworkReference",
         "Import", "InternalsVisibleTo", "None",
         "NugetPackageToIncludeInVsix",
         "PackageReference", "Reference", "RestrictedInternalsVisibleTo",
         "Service", "SuggestedBindingRedirects", "UsingTask",
         "VSCTCompile", "VsdConfigXmlFiles",
         "VSIXSourceItem", "#whitespace"})

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
        Try
            Dim destFileNameWithPath As String = Path.Combine(ProjectSavePath, PartialPathWithFileName)
            If destFileNameWithPath.IndexOfAny(Path.GetInvalidPathChars()) = -1 Then
                Exit Sub
            End If
            Directory.CreateDirectory(Path.GetDirectoryName(destFileNameWithPath))
            File.Copy(Path.Combine(New FileInfo(currentProject).Directory.FullName, PartialPathWithFileName), destFileNameWithPath, overwrite:=True)
        Catch ex As Exception
            Stop
        End Try
    End Sub

    Friend Function ConvertProjectFile(SolutionRoot As String, currentProject As String) As XmlDocument
        Dim xmlDoc As New XmlDocument With {
            .PreserveWhitespace = True
        }
        xmlDoc.Load(currentProject)
        Dim root As XmlNode
        If xmlDoc.DocumentElement IsNot Nothing AndAlso xmlDoc.DocumentElement.Name.Equals("Project", StringComparison.OrdinalIgnoreCase) Then
            root = xmlDoc.DocumentElement
        Else
            root = xmlDoc.FirstChild
        End If
        If root.Attributes.Count = 0 OrElse Not root.Attributes(0).Value.StartsWith("Microsoft.NET.Sdk", StringComparison.OrdinalIgnoreCase) Then
            Return New XmlDocument
        End If
        Dim basePath As String = DestinationFilePath(currentProject, SolutionRoot)
        Return ConvertProjectFile(xmlDoc, currentProject, basePath)
    End Function

    Friend Function CreateDirectoryIfNonexistent(SolutionRoot As String) As String
        If Not Directory.Exists(SolutionRoot) Then
            Directory.CreateDirectory(SolutionRoot)
        End If
        Return SolutionRoot
    End Function

    Friend Function DestinationFilePath(SourceDocumentFileNameWithPath As String, SolutionRoot As String) As String
        If String.IsNullOrWhiteSpace(SolutionRoot) Then
            Return String.Empty
        End If
        If SourceDocumentFileNameWithPath.Contains($".{Path.DirectorySeparatorChar}", StringComparison.OrdinalIgnoreCase) Then
            Stop
        End If
        Dim basePath As String = SolutionRoot.Substring(0, SolutionRoot.Length - 3)
        Dim subpathFromProject As String = Path.GetDirectoryName(SourceDocumentFileNameWithPath).Replace(basePath, "", StringComparison.OrdinalIgnoreCase).Trim(Path.DirectorySeparatorChar)
        Dim pathToSaveDirectory As String = Path.Combine(SolutionRoot, subpathFromProject)
        If Not Directory.Exists(pathToSaveDirectory) Then
            Directory.CreateDirectory(pathToSaveDirectory)
        End If
        Return pathToSaveDirectory
    End Function

    Public Function ConvertProjectFile(xmlDoc As XmlDocument, sourceFilePath As String, ProjectSavePath As String) As XmlDocument
        If ProjectSavePath Is Nothing Then
            Throw New ArgumentNullException(NameOf(ProjectSavePath))
        End If

        If xmlDoc Is Nothing Then
            Throw New ArgumentNullException(NameOf(xmlDoc))
        End If

        Dim root As XmlNode
        Dim isDocument As Boolean
        If xmlDoc.DocumentElement IsNot Nothing AndAlso xmlDoc.DocumentElement.Name.Equals("Project", StringComparison.OrdinalIgnoreCase) Then
            root = xmlDoc.DocumentElement
            isDocument = True
        Else
            root = xmlDoc.FirstChild
            isDocument = False
        End If

        If root.Attributes.Count = 0 OrElse Not root.Attributes(0).Value.StartsWith("Microsoft.NET.Sdk", StringComparison.OrdinalIgnoreCase) Then
            MsgBox("Project conversion only support SDK style projects, project file will not be converted!", MsgBoxStyle.Information)
            Return Nothing
        End If

        Dim nodesToBeRemoved As New List(Of (PropertyIndex As Integer, ChildIndex As Integer))
        If isDocument Then
            Dim isDesktopProject As Boolean = xmlDoc.DocumentElement.Attributes(0).Value = "Microsoft.NET.Sdk.WindowsDesktop"
            Dim leadingXMLSpace As XmlNode = xmlDoc.CreateDocumentFragment()
            leadingXMLSpace.InnerXml = "    "

            If xmlDoc.DocumentElement.HasChildNodes Then
                For index As Integer = 0 To xmlDoc.DocumentElement.ChildNodes.Count - 1
                    If s_documentTopLevelIgnoreList.Contains(xmlDoc.DocumentElement.ChildNodes(index).Name, StringComparer.OrdinalIgnoreCase) Then
                        ' No change necessary
                        Continue For
                    End If
                    Select Case xmlDoc.DocumentElement.ChildNodes(index).Name
                        Case "PropertyGroup"
                            For childIndex As Integer = 0 To xmlDoc.DocumentElement.ChildNodes(index).ChildNodes.Count - 1
                                Dim propertyGroupChildNode As XmlNode = xmlDoc.DocumentElement.ChildNodes(index).ChildNodes(childIndex)
                                If s_documentTopLevelIgnoreList.Contains(propertyGroupChildNode.Name, StringComparer.OrdinalIgnoreCase) Then
                                    Continue For
                                    ' No change necessary
                                End If
                                Select Case propertyGroupChildNode.Name
                                    Case "AllowUnsafeBlocks", "NoWarn"
                                        If childIndex > 0 AndAlso xmlDoc.DocumentElement.ChildNodes(index).ChildNodes(childIndex - 1).Name = "#whitespace" Then
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
                                        xmlDoc.DocumentElement.ChildNodes(index).ChildNodes(childIndex).InnerText = propertyGroupChildNode.InnerText.Replace(".cs", ".vb", StringComparison.OrdinalIgnoreCase)
                                    Case Else
                                        ' propertyGroupChildNode.Name
                                        Stop
                                End Select
                            Next childIndex
                        Case "ItemGroup"
                            For childIndex As Integer = 0 To xmlDoc.DocumentElement.ChildNodes(index).ChildNodes.Count - 1
                                Dim xmlNode As XmlNode = xmlDoc.DocumentElement.ChildNodes(index).ChildNodes(childIndex)
                                If s_itemGroupIgnoreList.Contains(xmlNode.Name, StringComparer.OrdinalIgnoreCase) Then
                                    ' No change necessary
                                    Continue For
                                End If
                                Select Case xmlNode.Name
                                    Case "#comment"
                                        xmlDoc.DocumentElement.ChildNodes(index).ChildNodes(childIndex).InnerText = xmlNode.InnerText.Replace(".cs", ".vb", StringComparison.OrdinalIgnoreCase)
                                    Case "Compile"
                                        Dim compileValue As String = ""
                                        For k As Integer = 0 To xmlNode.Attributes.Count - 1
                                            compileValue = xmlNode.Attributes(k).Value
                                            xmlDoc.DocumentElement.ChildNodes(index).ChildNodes(childIndex).Attributes(k).Value = ChangeExtension(compileValue, "cs", "vb")
                                        Next
                                        For k As Integer = 0 To xmlNode.ChildNodes.Count - 1
                                            If s_compileChildNodeIgnoreList.Contains(xmlNode.ChildNodes(k).Name, StringComparer.OrdinalIgnoreCase) Then
                                                ' No change necessary
                                                Continue For
                                            End If
                                            Select Case xmlNode.ChildNodes(k).Name
                                                Case "DependentUpon"
                                                    Dim dependentUponNodeValue As String = xmlNode.ChildNodes(k).ChildNodes(0).Value
                                                    If dependentUponNodeValue.EndsWith(".cs", StringComparison.OrdinalIgnoreCase) Then
                                                        xmlDoc.DocumentElement.ChildNodes(index).ChildNodes(childIndex).ChildNodes(k).ChildNodes(0).Value = ChangeExtension(dependentUponNodeValue, "cs", "vb")
                                                    Else
                                                        CopyFile(ProjectSavePath, sourceFilePath, Path.Combine(Path.GetDirectoryName(compileValue), dependentUponNodeValue))
                                                    End If
                                                Case "Link"
                                                    Dim linkFileValue As String = xmlNode.ChildNodes(k).ChildNodes(0).Value
                                                    If linkFileValue.EndsWith(".cs", StringComparison.OrdinalIgnoreCase) Then
                                                        xmlDoc.DocumentElement.ChildNodes(index).ChildNodes(childIndex).ChildNodes(k).ChildNodes(0).Value = ChangeExtension(linkFileValue, "cs", "vb")
                                                    End If
                                                Case Else
                                                    ' xmlNode.ChildNodes(k).Name
                                                    Stop
                                            End Select
                                        Next k
                                    Case "Resource"
                                        For Each a As XmlAttribute In xmlNode.Attributes
                                            CopyFile(ProjectSavePath, sourceFilePath, a.Value)
                                        Next
                                    Case "PublicAPI"
                                        For Each a As XmlAttribute In xmlNode.Attributes
                                            If a.Name.Equals("Include", StringComparison.OrdinalIgnoreCase) Then
                                                CopyFile(ProjectSavePath, sourceFilePath, a.Value)
                                            End If
                                        Next
                                    Case "EmbeddedResource"
                                        If xmlNode.Attributes(0).Value.EndsWith(".resx", StringComparison.OrdinalIgnoreCase) Then
                                            CopyFile(ProjectSavePath, sourceFilePath, xmlNode.Attributes(0).Value)
                                        End If
                                        For k As Integer = 0 To xmlNode.ChildNodes.Count - 1
                                            If s_embeddedResourceIgnoreList.Contains(xmlNode.ChildNodes(k).Name, StringComparer.OrdinalIgnoreCase) Then
                                                ' No change necessary
                                                Continue For
                                            End If
                                            Select Case xmlNode.ChildNodes(k).Name
                                                Case "DependentUpon"
                                                    For l As Integer = 0 To xmlNode.ChildNodes(k).ChildNodes.Count - 1
                                                        xmlDoc.DocumentElement.ChildNodes(index).ChildNodes(childIndex).ChildNodes(k).ChildNodes(l).Value = ChangeExtension(xmlNode.ChildNodes(k).ChildNodes(l).Value, "cs", "vb")
                                                    Next l
                                                Case Else
                                                    ' xmlNode.ChildNodes(k).Name
                                                    Stop
                                            End Select
                                        Next k
                                    Case "Content"
                                        CopyFile(ProjectSavePath, sourceFilePath, xmlNode.Attributes(0).Value)
                                    Case "ProjectReference"
                                        xmlDoc.DocumentElement.ChildNodes(index).ChildNodes(childIndex).Attributes(0).Value = xmlNode.Attributes(0).Value.Replace(".csproj", ".vbproj", StringComparison.OrdinalIgnoreCase)
                                    Case Else
                                        ' xmlNode.Name
                                        Stop
                                End Select
                            Next childIndex
                        Case "#comment"
                            xmlDoc.DocumentElement.ChildNodes(index).Value = xmlDoc.DocumentElement.ChildNodes(index).Value.Replace(".cs", ".vb", StringComparison.OrdinalIgnoreCase)
                        Case Else
                            ' xmlDoc.DocumentElement.ChildNodes(index).Name
                            Stop
                    End Select
                Next index

                If nodesToBeRemoved.Count > 0 Then
                    For index As Integer = nodesToBeRemoved.Count - 1 To 0 Step -1
                        xmlDoc.DocumentElement.ChildNodes(nodesToBeRemoved(index).PropertyIndex).RemoveChild(xmlDoc.DocumentElement.ChildNodes(nodesToBeRemoved(index).PropertyIndex).ChildNodes(nodesToBeRemoved(index).ChildIndex))
                    Next
                End If
            End If
            If Not String.IsNullOrWhiteSpace(ProjectSavePath) Then
                xmlDoc.Save(Path.Combine(ProjectSavePath, New FileInfo(sourceFilePath).Name.Replace(".csproj", ".vbproj", StringComparison.OrdinalIgnoreCase)))
            End If
            Return xmlDoc
        Else
            Dim isDesktopProject As Boolean = xmlDoc.FirstChild.Attributes(0).Value = "Microsoft.NET.Sdk.WindowsDesktop"
            Dim leadingXMLSpace As XmlNode = xmlDoc.CreateDocumentFragment()
            leadingXMLSpace.InnerXml = "    "

            If xmlDoc.FirstChild.HasChildNodes Then
                For index As Integer = 0 To xmlDoc.FirstChild.ChildNodes.Count - 1
                    If s_documentTopLevelIgnoreList.Contains(xmlDoc.FirstChild.ChildNodes(index).Name, StringComparer.OrdinalIgnoreCase) Then
                        ' No change necessary
                        Continue For
                    End If
                    Select Case xmlDoc.FirstChild.ChildNodes(index).Name
                        Case "PropertyGroup"
                            For childIndex As Integer = 0 To xmlDoc.FirstChild.ChildNodes(index).ChildNodes.Count - 1
                                Dim propertyGroupChildNode As XmlNode = xmlDoc.FirstChild.ChildNodes(index).ChildNodes(childIndex)
                                If s_documentTopLevelIgnoreList.Contains(propertyGroupChildNode.Name, StringComparer.OrdinalIgnoreCase) Then
                                    Continue For
                                    ' No change necessary
                                End If
                                Select Case propertyGroupChildNode.Name
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
                                If s_itemGroupIgnoreList.Contains(xmlDoc.Name, StringComparer.OrdinalIgnoreCase) Then
                                    Continue For
                                    ' No change necessary
                                End If
                                Select Case xmlNode.Name
                                    Case "#comment"
                                        xmlDoc.FirstChild.ChildNodes(index).ChildNodes(childIndex).InnerText = xmlNode.InnerText.Replace(".cs", ".vb", StringComparison.OrdinalIgnoreCase)
                                    Case "Compile"
                                        Dim compileValue As String = ""
                                        For k As Integer = 0 To xmlNode.Attributes.Count - 1
                                            compileValue = xmlNode.Attributes(k).Value
                                            xmlDoc.FirstChild.ChildNodes(index).ChildNodes(childIndex).Attributes(k).Value = ChangeExtension(compileValue, "cs", "vb")
                                        Next
                                        For k As Integer = 0 To xmlNode.ChildNodes.Count - 1
                                            If s_compileChildNodeIgnoreList.Contains(xmlNode.ChildNodes(k).Name, StringComparer.OrdinalIgnoreCase) Then
                                                ' No change necessary
                                                Continue For
                                            End If
                                            Select Case xmlNode.ChildNodes(k).Name
                                                Case "DependentUpon"
                                                    Dim dependentUponNodeValue As String = xmlNode.ChildNodes(k).ChildNodes(0).Value
                                                    If dependentUponNodeValue.EndsWith(".cs", StringComparison.OrdinalIgnoreCase) Then
                                                        xmlDoc.FirstChild.ChildNodes(index).ChildNodes(childIndex).ChildNodes(k).ChildNodes(0).Value = ChangeExtension(dependentUponNodeValue, "cs", "vb")
                                                    Else
                                                        CopyFile(ProjectSavePath, sourceFilePath, Path.Combine(Path.GetDirectoryName(compileValue), dependentUponNodeValue))
                                                    End If
                                                Case "Link"
                                                    Dim linkFileValue As String = xmlNode.ChildNodes(k).ChildNodes(0).Value
                                                    If linkFileValue.EndsWith(".cs", StringComparison.OrdinalIgnoreCase) Then
                                                        xmlDoc.FirstChild.ChildNodes(index).ChildNodes(childIndex).ChildNodes(k).ChildNodes(0).Value = ChangeExtension(linkFileValue, "cs", "vb")
                                                    End If
                                                Case Else
                                                    Stop
                                            End Select
                                        Next k
                                    Case "Resource"
                                        For Each a As XmlAttribute In xmlNode.Attributes
                                            CopyFile(ProjectSavePath, sourceFilePath, a.Value)
                                        Next
                                    Case "PublicAPI"
                                        For Each a As XmlAttribute In xmlNode.Attributes
                                            If a.Name.Equals("Include", StringComparison.OrdinalIgnoreCase) Then
                                                CopyFile(ProjectSavePath, sourceFilePath, a.Value)
                                            End If
                                        Next
                                    Case "EmbeddedResource"
                                        If xmlNode.Attributes(0).Value.EndsWith(".resx", StringComparison.OrdinalIgnoreCase) Then
                                            CopyFile(ProjectSavePath, sourceFilePath, xmlNode.Attributes(0).Value)
                                        End If
                                        For k As Integer = 0 To xmlNode.ChildNodes.Count - 1
                                            If s_embeddedResourceIgnoreList.Contains(xmlNode.ChildNodes(k).Name, StringComparer.OrdinalIgnoreCase) Then
                                                Continue For
                                            End If
                                            Select Case xmlNode.ChildNodes(k).Name
                                                Case "DependentUpon"
                                                    For l As Integer = 0 To xmlNode.ChildNodes(k).ChildNodes.Count - 1
                                                        xmlDoc.FirstChild.ChildNodes(index).ChildNodes(childIndex).ChildNodes(k).ChildNodes(l).Value = ChangeExtension(xmlNode.ChildNodes(k).ChildNodes(l).Value, "cs", "vb")
                                                    Next l
                                                Case Else
                                                    Stop
                                            End Select
                                        Next k
                                    Case "Content"
                                        CopyFile(ProjectSavePath, sourceFilePath, xmlNode.Attributes(0).Value)
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
            Return xmlDoc
        End If
    End Function

End Module
