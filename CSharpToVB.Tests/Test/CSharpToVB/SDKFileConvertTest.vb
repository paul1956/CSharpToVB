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
                <Folder Include="wwwroot\lib\"/>
            </ItemGroup>
            <ItemGroup>
                <ProjectReference Include="..\ApplicationCore\ApplicationCore.csproj"/>
            </ItemGroup>
            <ItemGroup>
                <None Include="compilerconfig.json"/>
                <None Include="wwwroot\images\products\1.png"/>
            </ItemGroup>
            <ItemGroup>
                <Content Update="appsettings.json">
                    <CopyToOutputDirectory>Always</CopyToOutputDirectory>
                </Content>
                <Content Update="Views\Shared\_Layout.cshtml">
                    <CopyToPublishDirectory>PreserveNewest</CopyToPublishDirectory>
                </Content>
            </ItemGroup>
        </Project>

            Dim sourceXmlDoc As New XmlDocument With {
                                .PreserveWhitespace = True
                            }
            sourceXmlDoc.LoadXml(_originalProjectFile.ToString)

            Dim actualProjectFile As XmlDocument = ConvertProjectFile("", "", sourceXmlDoc)
            Dim ResultXmlDoc As New XmlDocument With {
                                .PreserveWhitespace = True
                            }
            ResultXmlDoc.LoadXml(_originalProjectFile.ToString)
            Assert.Equal(actualProjectFile.Value, ResultXmlDoc.Value)
        End Sub

        <Fact>
        Public Shared Sub ConvertClassProjectFileTest()

            Dim _originalProjectFile As XElement =
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
                <Folder Include="wwwroot\lib\"/>
            </ItemGroup>
            <ItemGroup>
                <ProjectReference Include="..\ApplicationCore\ApplicationCore.csproj"/>
            </ItemGroup>
            <ItemGroup>
                <None Include="compilerconfig.json"/>
                <None Include="wwwroot\images\products\1.png"/>
            </ItemGroup>
            <ItemGroup>
                <Content Update="appsettings.json">
                    <CopyToOutputDirectory>Always</CopyToOutputDirectory>
                </Content>
                <Content Update="Views\Shared\_Layout.cshtml">
                    <CopyToPublishDirectory>PreserveNewest</CopyToPublishDirectory>
                </Content>
            </ItemGroup>
        </Project>

            Dim sourceXmlDoc As New XmlDocument With {
                                .PreserveWhitespace = True
                            }
            sourceXmlDoc.LoadXml(_originalProjectFile.ToString)

            Dim actualProjectFile As XmlDocument = ConvertProjectFile("", "", sourceXmlDoc)
            Dim ResultXmlDoc As New XmlDocument With {
                                .PreserveWhitespace = True
                            }
            ResultXmlDoc.LoadXml(_originalProjectFile.ToString)
            Assert.Equal(actualProjectFile.Value, ResultXmlDoc.Value)
        End Sub
    End Class
End Namespace
