' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports System.IO
Imports System.Xml

Public Module ConvertProjectFileUtilities

    Private _projectsToBeAdded As String = ""

    Private ReadOnly s_compileChildNodeIgnoreList As New List(Of String)(
                {"AutoGen", "DesignTime", "DesignTimeSharedInput",
             "#whitespace"})

    Private ReadOnly s_cSProjectFile As XElement = <Project Sdk="Microsoft.NET.Sdk">
                                                       <PropertyGroup>
                                                           <TargetFramework>__TargetFramework__</TargetFramework>
                                                       </PropertyGroup>
                                                       <ItemGroup>
                                                           <Protobuf Include="..\Proto\count.proto" GrpcServices="Both" Link="count.proto"/>
                                                       </ItemGroup>
                                                       <ItemGroup>
                                                           <PackageReference Include="Microsoft.Extensions.Hosting" Version="3.1.4"/>
                                                           <PackageReference Include="Google.Protobuf" Version="3.12.2"/>
                                                           <PackageReference Include="Grpc.Net.ClientFactory" Version="2.29.0"/>
                                                           <PackageReference Include="Grpc.Tools" Version="2.29.0" PrivateAssets="All"/>
                                                       </ItemGroup>
                                                   </Project>

    Private ReadOnly s_documentTopLevelIgnoreList As New List(Of String)(
            {"_DependentAssemblyVersionsFile",
             "_DependsOn",
             "_DotNetSdkVersionFile",
             "_NuGetRepackAssembly",
             "_OptimizedDependenciesDir",
             "AppendTargetFrameworkToOutputPath",
             "ApplyNgenOptimization",
             "AssemblyName",
             "AssemblyVersion",
             "Authors",
             "AutoGenerateAssemblyVersion",
             "AutoGenerateBindingRedirects",
             "AutomaticBindingRedirects",
             "CLSCompliant",
             "CodeAnalysisRuleSet",
             "Company",
             "CopyLocalLockFileAssemblies",
             "CreateVsixContainer",
             "DebugType",
             "DefaultLanguage",
             "DefineConstants",
             "DeployExtension",
             "Description",
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
             "PackageReleaseNotes",
             "Platform",
             "Platforms",
             "PlatformTarget",
             "Prefer32Bit",
             "Product",
             "ProduceReferenceAssembly",
             "ProjectExtensions",
             "ResolveAssemblyWarnOrErrorOnTargetArchitectureMismatch",
             "RootNamespace",
             "RoslynProjectType",
             "RuntimeIdentifier",
             "RuntimeIdentifiers",
             "ServerGarbageCollection",
             "ServiceHubAssemblyBasePath",
             "SignAssembly",
             "SkipTests",
             "StartupObject",
             "Target",
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
             "Version",
             "VisualStudioInsertionComponent",
             "Win32Manifest",
             "#whitespace"})

    Private ReadOnly s_embeddedResourceIgnoreList As New List(Of String)(
        {"Generator", "GenerateSource", "LogicalName",
         "ManifestResourceName", "MergeWithCTO", "Namespace",
         "SubType", "#whitespace"})

    Private ReadOnly s_itemGroupIgnoreList As New List(Of String)(
        {"AssemblyAttribute", "DotNetCliToolReference",
        "ExpectedDependency",
         "Folder", "FrameworkReference",
         "Import", "InternalsVisibleTo", "None",
         "NugetPackageToIncludeInVsix", "NukeExternalFiles",
         "NukeSpecificationFiles",
         "PackageReference", "Reference", "RestrictedInternalsVisibleTo",
         "Service", "SuggestedBindingRedirects", "UsingTask",
         "VSCTCompile", "VsdConfigXmlFiles",
         "VSIXSourceItem", "#whitespace"})

    Private Function ChangeExtension(attributeValue As String, oldExtension As String, newExtension As String) As String
        Return If(attributeValue.EndsWith($".{oldExtension}", StringComparison.OrdinalIgnoreCase),
               Path.ChangeExtension(attributeValue, newExtension),
               attributeValue)
    End Function

    Private Sub ConvertProtoNode(projectSavePath As String, sourceFilePath As String, xmlNode As XmlNode, targetFramework As String)
        Dim basePath As String = New Uri(xmlNode.BaseURI).LocalPath
        If basePath <> sourceFilePath Then
            Stop
        End If

        Dim protoSourcePath As String = Path.Combine(Directory.GetParent(sourceFilePath).Parent.FullName, "Proto")

        If Not Directory.Exists(protoSourcePath) Then
            Exit Sub
        End If
        Dim protoDestinationPath As String = Path.Combine(Directory.GetParent(projectSavePath).FullName, "Proto")
        If Not Directory.Exists(protoDestinationPath) Then
            Directory.CreateDirectory(protoDestinationPath)
            Dim csProtoPath As String = Path.Combine(Directory.GetParent(projectSavePath).FullName, "CSProto")
            Directory.CreateDirectory(csProtoPath)
            File.WriteAllText(Path.Combine(csProtoPath, "CSProto.csproj"), s_cSProjectFile.ToString.Replace("__TargetFramework__", targetFramework, StringComparison.Ordinal))
        End If
        For Each fileName As String In Directory.GetFiles(protoSourcePath)
            File.Copy(fileName, Path.Combine(protoDestinationPath, Path.GetFileName(fileName)), overwrite:=True)
        Next
    End Sub

    Private Sub CopyFile(projectSavePath As String, sourceFilePath As String, partialPathWithFileName As String)
        If String.IsNullOrWhiteSpace(projectSavePath) OrElse
                String.IsNullOrWhiteSpace(sourceFilePath) OrElse
                String.IsNullOrWhiteSpace(partialPathWithFileName) Then
            Exit Sub
        End If
        Try
            Dim destFileNameWithPath As String = Path.Combine(projectSavePath, partialPathWithFileName)
            If destFileNameWithPath.IndexOfAny(Path.GetInvalidPathChars()) = -1 Then
                Exit Sub
            End If
            Directory.CreateDirectory(Path.GetDirectoryName(destFileNameWithPath))
            File.Copy(Path.Combine(New FileInfo(sourceFilePath).Directory.FullName, partialPathWithFileName), destFileNameWithPath, overwrite:=True)
        Catch ex As ObjectDisposedException
            End
        Catch ex As Exception
            Throw
        End Try
    End Sub

    Friend Function DestinationFilePath(sourceDocumentFileNameWithPath As String, solutionRoot As String) As String
        If String.IsNullOrWhiteSpace(solutionRoot) Then
            Return String.Empty
        End If
        If sourceDocumentFileNameWithPath.Contains($".{Path.DirectorySeparatorChar}", StringComparison.OrdinalIgnoreCase) Then
            Stop
        End If
        Dim basePath As String = solutionRoot.Substring(0, solutionRoot.Length - 3)
        Dim subPathFromProject As String = Path.GetDirectoryName(sourceDocumentFileNameWithPath).Replace(basePath, "", StringComparison.OrdinalIgnoreCase).Trim(Path.DirectorySeparatorChar)
        Dim pathToSaveDirectory As String = Path.Combine(solutionRoot, subPathFromProject)
        If Not Directory.Exists(pathToSaveDirectory) Then
            Directory.CreateDirectory(pathToSaveDirectory)
        End If
        Return pathToSaveDirectory
    End Function

    Public Function ConvertProjectFile(sourceFilePath As String, projectSavePath As String) As String
        If String.IsNullOrWhiteSpace(sourceFilePath) Then
            Throw New ArgumentException($"'{sourceFilePath}' cannot be null or whitespace", NameOf(sourceFilePath))
        End If

        If String.IsNullOrWhiteSpace(projectSavePath) Then
            Throw New ArgumentException($"'{projectSavePath}' cannot be null or whitespace", NameOf(sourceFilePath))
        End If

        Dim xmlDoc As New XmlDocument With {
            .PreserveWhitespace = True
        }
        xmlDoc.Load(sourceFilePath)
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
            MsgBox($"Project {sourceFilePath} is not an SDK project, the project file will not be converted!", MsgBoxStyle.Information, "Project Conversion Issue")
            Return ""
        End If
        Dim basePath As String = DestinationFilePath(sourceFilePath, projectSavePath)

        Dim nodesToBeRemoved As New List(Of (PropertyIndex As Integer, ChildIndex As Integer))
        If isDocument Then
            Dim leadingXmlSpace As XmlNode = xmlDoc.CreateDocumentFragment()
            leadingXmlSpace.InnerXml = "    "
            Dim targetFramework As String = String.Empty
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
                                    Case "AllowUnsafeBlocks", "NoWarn", "WarningsAsErrors"
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
                                    Case "TargetFramework"
                                        targetFramework = xmlDoc.DocumentElement.ChildNodes(index).ChildNodes(childIndex).InnerText
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
                                                        CopyFile(basePath, sourceFilePath, Path.Combine(Path.GetDirectoryName(compileValue), dependentUponNodeValue))
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
                                            CopyFile(basePath, sourceFilePath, a.Value)
                                        Next
                                    Case "PublicAPI"
                                        For Each a As XmlAttribute In xmlNode.Attributes
                                            If a.Name.Equals("Include", StringComparison.OrdinalIgnoreCase) Then
                                                CopyFile(basePath, sourceFilePath, a.Value)
                                            End If
                                        Next
                                    Case "EmbeddedResource"
                                        If xmlNode.Attributes(0).Value.EndsWith(".resx", StringComparison.OrdinalIgnoreCase) Then
                                            CopyFile(basePath, sourceFilePath, xmlNode.Attributes(0).Value)
                                        End If
                                        For k As Integer = 0 To xmlNode.ChildNodes.Count - 1
                                            If s_embeddedResourceIgnoreList.Contains(xmlNode.ChildNodes(k).Name, StringComparer.OrdinalIgnoreCase) Then
                                                ' No change necessary
                                                Continue For
                                            End If
                                            Select Case xmlNode.ChildNodes(k).Name
                                                Case "DependentUpon", "CopyToOutputDirectory"
                                                    For l As Integer = 0 To xmlNode.ChildNodes(k).ChildNodes.Count - 1
                                                        xmlDoc.DocumentElement.ChildNodes(index).ChildNodes(childIndex).ChildNodes(k).ChildNodes(l).Value = ChangeExtension(xmlNode.ChildNodes(k).ChildNodes(l).Value, "cs", "vb")
                                                    Next l
                                                Case Else
                                                    ' xmlNode.ChildNodes(k).Name
                                                    Stop
                                            End Select
                                        Next k
                                    Case "Content"
                                        CopyFile(basePath, sourceFilePath, xmlNode.Attributes(0).Value)
                                    Case "ProjectReference"
                                        xmlDoc.DocumentElement.ChildNodes(index).ChildNodes(childIndex).Attributes(0).Value = xmlNode.Attributes(0).Value.Replace(".csproj", ".vbproj", StringComparison.OrdinalIgnoreCase)
                                    Case "Protobuf"
                                        ConvertProtoNode(basePath, sourceFilePath, xmlNode, targetFramework)
                                        _projectsToBeAdded = $"Project(""{{9A19103F-16F7-4668-BE54-9A1E7A4F7556}}"") = ""CSProto"", ""CSProto\CSProto.csproj"", ""{{{Guid.NewGuid.ToString.ToUpperInvariant}}}""{vbCrLf}EndProject{vbCrLf}"
                                        Dim elem As XmlElement = xmlDoc.CreateElement("ProjectReference")
                                        elem.SetAttribute("Include", "..\CSProto\CSProto.csproj")
                                        Dim y As XmlNode = xmlDoc.GetElementsByTagName("Protobuf")(0)
                                        y.ParentNode.ReplaceChild(elem, y)
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
        Else
            Dim leadingXmlSpace As XmlNode = xmlDoc.CreateDocumentFragment()
            leadingXmlSpace.InnerXml = "    "

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
                                       propertyGroupChildNode.InnerText.Equals("Default", StringComparison.OrdinalIgnoreCase) OrElse
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
                                                        CopyFile(basePath, sourceFilePath, Path.Combine(Path.GetDirectoryName(compileValue), dependentUponNodeValue))
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
                                            CopyFile(basePath, sourceFilePath, a.Value)
                                        Next
                                    Case "PublicAPI"
                                        For Each a As XmlAttribute In xmlNode.Attributes
                                            If a.Name.Equals("Include", StringComparison.OrdinalIgnoreCase) Then
                                                CopyFile(basePath, sourceFilePath, a.Value)
                                            End If
                                        Next
                                    Case "EmbeddedResource"
                                        If xmlNode.Attributes(0).Value.EndsWith(".resx", StringComparison.OrdinalIgnoreCase) Then
                                            CopyFile(basePath, sourceFilePath, xmlNode.Attributes(0).Value)
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
                                        CopyFile(basePath, sourceFilePath, xmlNode.Attributes(0).Value)
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
        End If
        If Not String.IsNullOrWhiteSpace(basePath) Then
            xmlDoc.Save(Path.Combine(basePath, Path.GetFileName(sourceFilePath).Replace(".csproj", ".vbproj", StringComparison.OrdinalIgnoreCase)))
        End If
        Return _projectsToBeAdded
    End Function

End Module
