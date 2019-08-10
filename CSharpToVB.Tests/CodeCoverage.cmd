echo %time%
Set MANUAL_TESTS=
rem dotnet test --collect:"XPlat Code Coverage" --settings coverletArgs.runsettings
Rem dotnet vstest --settings coverletArgs.runsettings
rem dotnet test /p:CollectCoverage=true  /p:CoverletOutput=./TestResults/LastRun/Coverage.json /p:Exclude=\"[coverlet.*]*,[*]Coverlet.Core*,[xunit*]*,[Microsoft.DotNet.XUnitExtensions]*,[ManageProgressBar]*,[VBMsgBox]*,[CSharpToVBApp]*,[HashLibrary]*"
dotnet test ./CSharpToVB.Tests.vbproj /p:CollectCoverage=true /p:CoverletOutputFormat=json /p:CoverletOutput=./TestResults/LastRun/Coverage.json /p:Exclude=\"[coverlet.*]*,[*]Coverlet.Core*,[xunit*]*,[Microsoft.DotNet.XUnitExtensions]*,[ManageProgressBar]*,[VBMsgBox]*,[CSharpToVBApp]*,[HashLibrary]*"
