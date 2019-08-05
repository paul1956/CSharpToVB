echo %time%
Set MANUAL_TESTS=
Rem dotnet test --collect:"XPlat Code Coverage" --settings coverletArgs.runsettings
Rem dotnet vstest --settings coverletArgs.runsettings
dotnet test /p:CollectCoverage=true  /p:CoverletOutput=./TestResults/LastRun/Coverage.json /p:Exclude=\"[coverlet.*]*,[*]Coverlet.Core*,HashLibrary,ManageProgressBar,CSharpToVBApp,Microsoft.DotNet.XUnitExtensions,Microsoft.CodeAnalysis.PooledObjects1,VBMsgBox,xunit.*"