dotnet test --collect:"XPlat Code Coverage" --settings coverletArgs.runsettings
Rem dotnet vstest --settings coverletArgs.runsettings
exit /b %ERRORLEVEL%
