''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' FileVersion.vbs -- Print executable file's version number 
'
'    Usage:  cscript /Nologo FileVersion.vbs FILENAME
'
' cscript is the command-line version of Windows Scripting Host 
' which will run the VB script file.  The Nologo option suppresses 
' banner information so output contains version number only
'
' The Windows Scripting Host and VBScript interpreter are now 
' standard components on Windows. However some security software
' may prompt before running .vbs files or block scripts entirely 
' because they have been a vector for malware.
Set objFSO = CreateObject("Scripting.FileSystemObject")
Wscript.Echo objFSO.GetFileVersion(Wscript.Arguments(0))
