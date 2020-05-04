' Licensed to the .NET Foundation under one or more agreements.
' The .NET Foundation licenses this file to you under the MIT license.
' See the LICENSE file in the project root for more information.

Imports Xunit
Imports CSharpToVBApp
Imports System.Xml

Namespace ProjectFile.Tests

    <TestClass()> Public NotInheritable Class SDKFileConvertTest
        <Fact>
        Public Shared Sub ConvertWebProjectFileTest()

            Dim _originalProjectFile As XElement =
        <Project Sdk="Microsoft.NET.Sdk.Web">
            <PropertyGroup>
                <TargetFramework>netcoreapp3.1</TargetFramework>
                <RootNamespace>Microsoft.eShopWeb.Web</RootNamespace>
                <UserSecretsId>aspnet-Web2-12345678-1234-5678-9E49-1CCCD7FE85F7</UserSecretsId>
                <LangVersion>latest</LangVersion>
                <AllowUnsafeBlocks>true</AllowUnsafeBlocks>
            </PropertyGroup>

            <ItemGroup>
                <Content Remove="compilerconfig.json"/>
            </ItemGroup>

            <ItemGroup>
                <PackageReference Include="Ardalis.ListStartupServices" Version="1.1.3"/>

                <PackageReference Include="Microsoft.EntityFrameworkCore.Tools" Version="3.1.0">
                    <PrivateAssets>all</PrivateAssets>
                    <IncludeAssets>runtime; build; native; contentfiles; analyzers; buildtransitive</IncludeAssets>
                </PackageReference>
            </ItemGroup>
            <ItemGroup>
                <Folder Include="wwwroot\fonts\"/>
            </ItemGroup>
            <ItemGroup>
                <ProjectReference Include="..\ApplicationCore\ApplicationCore.csproj"/>
            </ItemGroup>
            <ItemGroup>
                <None Include="wwwroot\images\products\1.png"/>
            </ItemGroup>
            <ItemGroup>
                <Content Update="appsettings.json">
                    <CopyToOutputDirectory>Always</CopyToOutputDirectory>
                </Content>
            </ItemGroup>
        </Project>

            Dim _expectedProjectFile As XElement =
        <Project Sdk="Microsoft.NET.Sdk.Web">
            <PropertyGroup>
                <TargetFramework>netcoreapp3.1</TargetFramework>
                <RootNamespace>Microsoft.eShopWeb.Web</RootNamespace>
                <UserSecretsId>aspnet-Web2-12345678-1234-5678-9E49-1CCCD7FE85F7</UserSecretsId>
                <LangVersion>latest</LangVersion>
            </PropertyGroup>

            <ItemGroup>
                <Content Remove="compilerconfig.json"/>
            </ItemGroup>

            <ItemGroup>
                <PackageReference Include="Ardalis.ListStartupServices" Version="1.1.3"/>

                <PackageReference Include="Microsoft.EntityFrameworkCore.Tools" Version="3.1.0">
                    <PrivateAssets>all</PrivateAssets>
                    <IncludeAssets>runtime; build; native; contentfiles; analyzers; buildtransitive</IncludeAssets>
                </PackageReference>
            </ItemGroup>
            <ItemGroup>
                <Folder Include="wwwroot\fonts\"/>
            </ItemGroup>
            <ItemGroup>
                <ProjectReference Include="..\ApplicationCore\ApplicationCore.csproj"/>
            </ItemGroup>
            <ItemGroup>
                <None Include="wwwroot\images\products\1.png"/>
            </ItemGroup>
            <ItemGroup>
                <Content Update="appsettings.json">
                    <CopyToOutputDirectory>Always</CopyToOutputDirectory>
                </Content>
            </ItemGroup>
        </Project>

            Dim sourceXmlDoc As New XmlDocument With {
                                .PreserveWhitespace = True
                            }
            sourceXmlDoc.LoadXml(_originalProjectFile.ToString)

            Dim sourceProjectFile As XmlDocument = ConvertProjectFile(sourceXmlDoc, "", "")
            Dim expectedXmlDoc As New XmlDocument With {
                                .PreserveWhitespace = True
                            }
            expectedXmlDoc.LoadXml(_expectedProjectFile.ToString)
            Assert.Equal(expectedXmlDoc.InnerText, sourceProjectFile.InnerText)
        End Sub

        <Fact>
        Public Shared Sub ConvertClassProjectFileTest()

            Dim _originalProjectFile As XElement =
<Project Sdk="Microsoft.NET.Sdk">

    <PropertyGroup>
        <AssemblyName>System.Windows.Forms</AssemblyName>
        <AllowUnsafeBlocks>true</AllowUnsafeBlocks>
        <CLSCompliant>true</CLSCompliant>
        <Nullable>enable</Nullable>

        <NoWarn>$(NoWarn);618</NoWarn>
        <DefineConstants>$(DefineConstants);OPTIMIZED_MEASUREMENTDC;</DefineConstants>
        <Win32Manifest>Resources\System\Windows\Forms\XPThemes.manifest</Win32Manifest>
        <Deterministic>true</Deterministic>
        <ProduceReferenceAssembly>true</ProduceReferenceAssembly>
        <UsePublicApiAnalyzers>true</UsePublicApiAnalyzers>
    </PropertyGroup>

    <ItemGroup>
        <ProjectReference Include="..\..\System.Windows.Forms.Primitives\src\System.Windows.Forms.Primitives.csproj"/>
    </ItemGroup>

    <ItemGroup>
        <PackageReference Include="Microsoft.Win32.Registry" Version="$(MicrosoftWin32RegistryPackageVersion)"/>
    </ItemGroup>

    <ItemGroup>
        <Compile Include="..\..\Common\src\RTLAwareMessageBox.cs" Link="Common\RTLAwareMessageBox.cs"/>
    </ItemGroup>

    <ItemGroup>
        <EmbeddedResource Update="Resources\SR.resx">
            <GenerateSource>true</GenerateSource>
            <Namespace>System</Namespace>
        </EmbeddedResource>
    </ItemGroup>

    <ItemGroup>
        <EmbeddedResource Include="Resources\System\Windows\Forms\Animation.ico">
            <LogicalName>System.Windows.Forms.Animation</LogicalName>
        </EmbeddedResource>
    </ItemGroup>
</Project>

            Dim _expectedProjectFile As XElement =
<Project Sdk="Microsoft.NET.Sdk">

    <PropertyGroup>
        <AssemblyName>System.Windows.Forms</AssemblyName>
        <CLSCompliant>true</CLSCompliant>
        <Nullable>enable</Nullable>

        <DefineConstants>$(DefineConstants);OPTIMIZED_MEASUREMENTDC;</DefineConstants>
        <Win32Manifest>Resources\System\Windows\Forms\XPThemes.manifest</Win32Manifest>
        <Deterministic>true</Deterministic>
        <ProduceReferenceAssembly>true</ProduceReferenceAssembly>
        <UsePublicApiAnalyzers>true</UsePublicApiAnalyzers>
    </PropertyGroup>

    <ItemGroup>
        <ProjectReference Include="..\..\System.Windows.Forms.Primitives\src\System.Windows.Forms.Primitives.vbproj"/>
    </ItemGroup>

    <ItemGroup>
        <PackageReference Include="Microsoft.Win32.Registry" Version="$(MicrosoftWin32RegistryPackageVersion)"/>
    </ItemGroup>

    <ItemGroup>
        <Compile Include="..\..\Common\src\RTLAwareMessageBox.vb" Link="Common\RTLAwareMessageBox.vb"/>
    </ItemGroup>

    <ItemGroup>
        <EmbeddedResource Update="Resources\SR.resx">
            <GenerateSource>true</GenerateSource>
            <Namespace>System</Namespace>
        </EmbeddedResource>
    </ItemGroup>

    <ItemGroup>
        <EmbeddedResource Include="Resources\System\Windows\Forms\Animation.ico">
            <LogicalName>System.Windows.Forms.Animation</LogicalName>
        </EmbeddedResource>
    </ItemGroup>
</Project>
            Dim sourceXmlDoc As New XmlDocument With {
                                .PreserveWhitespace = True
                            }
            sourceXmlDoc.LoadXml(_originalProjectFile.ToString)

            Dim sourceProjectFile As XmlDocument = ConvertProjectFile(sourceXmlDoc, "", "")
            Dim expectedXmlDoc As New XmlDocument With {
                                .PreserveWhitespace = True
                            }
            expectedXmlDoc.LoadXml(_expectedProjectFile.ToString)
            Assert.Equal(expectedXmlDoc.InnerText, sourceProjectFile.InnerText)
        End Sub

        <Fact>
        Public Shared Sub ConvertWinFormsProjectFileTest()

            Dim _originalProjectFile As XElement =
<Project Sdk="Microsoft.NET.Sdk.WindowsDesktop">

    <PropertyGroup>
        <OutputType>WinExe</OutputType>
        <TargetFramework Condition="'$(TargetFrameworkOverride)' == ''">netcoreapp5.0</TargetFramework>
        <TargetFramework Condition="'$(TargetFrameworkOverride)' != ''">TargetFrameworkOverride</TargetFramework>
        <RootNamespace>Company.WinFormsApplication1</RootNamespace>
        <StartupObject>Company.WinFormsApplication1.Form1</StartupObject>
        <LangVersion Condition="'$(langVersion)' != ''">$(ProjectLanguageVersion)</LangVersion>
        <UseWindowsForms>true</UseWindowsForms>
    </PropertyGroup>

    <ItemGroup>
        <Import Include="System.Data"/>
    </ItemGroup>

</Project>
            Dim _expectedProjectFile As XElement =
<Project Sdk="Microsoft.NET.Sdk.WindowsDesktop">

    <PropertyGroup>
        <OutputType>WinExe</OutputType>
        <TargetFramework Condition="'$(TargetFrameworkOverride)' == ''">netcoreapp5.0</TargetFramework>
        <TargetFramework Condition="'$(TargetFrameworkOverride)' != ''">TargetFrameworkOverride</TargetFramework>
        <RootNamespace>Company.WinFormsApplication1</RootNamespace>
        <StartupObject>Company.WinFormsApplication1.Form1</StartupObject>
        <LangVersion Condition="'$(langVersion)' != ''">$(ProjectLanguageVersion)</LangVersion>
        <UseWindowsForms>true</UseWindowsForms>
    </PropertyGroup>

    <ItemGroup>
        <Import Include="System.Data"/>
    </ItemGroup>

</Project>
            Dim sourceXmlDoc As New XmlDocument With {
                                .PreserveWhitespace = True
                            }
            sourceXmlDoc.LoadXml(_originalProjectFile.ToString)

            Dim sourceProjectFile As XmlDocument = ConvertProjectFile(sourceXmlDoc, "", "")
            Dim expectedXmlDoc As New XmlDocument With {
                                .PreserveWhitespace = True
                            }
            expectedXmlDoc.LoadXml(_expectedProjectFile.ToString)
            Assert.Equal(expectedXmlDoc.InnerText, sourceProjectFile.InnerText)
        End Sub
    End Class
End Namespace
