@echo off
REM -- Make help for Project FBD
echo Building Win32 Help files for fbd
start /wait hcrtf -x "FBD.hpj"
echo.
if exist ..\TCP_Debug\nul copy "FBD.hlp" ..\TCP_Debug\Debug
if exist ..\TCP_Debug\nul copy "FBD.cnt" ..\TCP_Debug\Debug
if exist ..\TCP_Release\nul copy "FBD.hlp" ..\TCP_Release\Release
if exist ..\TCP_Release\nul copy "FBD.cnt" ..\TCP_Release\Release
echo.


