echo %time%
Rem Set MANUAL_TESTS=1
dotnet test --collect:"XPlat Code Coverage" --settings coverletArgs.runsettings
Rem dotnet vstest --settings coverletArgs.runsettings
exit /b %ERRORLEVEL%
