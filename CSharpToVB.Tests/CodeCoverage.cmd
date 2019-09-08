echo %time%
Set MANUAL_TESTS=
Rem dotnet test --collect:"XPlat Code Coverage" --settings coverletArgs.runsettings
Rem JSON
dotnet test ./CSharpToVB.Tests.vbproj /p:CollectCoverage=true /p:CoverletOutputFormat=json /p:CoverletOutput=./TestResults/LastRun/Coverage.json /p:Exclude=\"[coverlet.*]*,[*]Coverlet.Core*,[xunit*]*,[Microsoft.DotNet.XUnitExtensions]*,[ManageProgressBar]*,[VBMsgBox]*,[CSharpToVBApp]*,[HashLibrary]*"
Rem Opencover
dotnet test ./CSharpToVB.Tests.vbproj /p:CollectCoverage=true /p:CoverletOutputFormat=Opencover /p:CoverletOutput=./TestResults/LastRun/Coverage.Opencover /p:Exclude=\"[coverlet.*]*,[*]Coverlet.Core*,[xunit*]*,[Microsoft.DotNet.XUnitExtensions]*,[ManageProgressBar]*,[VBMsgBox]*,[CSharpToVBApp]*,[HashLibrary]*"
Rem Cobertura
dotnet test ./CSharpToVB.Tests.vbproj /p:CollectCoverage=true /p:CoverletOutputFormat=Cobertura /p:CoverletOutput=./TestResults/LastRun/Coverage.Cobertura  /p:Exclude=\"[coverlet.*]*,[*]Coverlet.Core*,[xunit*]*,[Microsoft.DotNet.XUnitExtensions]*,[ManageProgressBar]*,[VBMsgBox]*,[CSharpToVBApp]*,[HashLibrary]*"
