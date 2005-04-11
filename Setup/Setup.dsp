# Microsoft Developer Studio Project File - Name="Setup" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=Setup - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "Setup.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Setup.mak" CFG="Setup - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Setup - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "Setup - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE "Setup - Win32 USNA Eval Release" (based on "Win32 (x86) Application")
!MESSAGE "Setup - Win32 Student Release" (based on "Win32 (x86) Application")
!MESSAGE "Setup - Win32 Experiment Release" (based on "Win32 (x86) Application")
!MESSAGE "Setup - Win32 Eval Patch" (based on "Win32 (x86) Application")
!MESSAGE "Setup - Win32 OLI Release" (based on "Win32 (x86) Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""$/Setup", AOAAAAAA"
# PROP Scc_LocalPath "."
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "Setup - Win32 Release"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 5
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MD /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_AFXDLL" /Yu"stdafx.h" /FD /c
# ADD CPP /nologo /MT /W3 /GX /O1 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /Yu"stdafx.h" /FD /c
# SUBTRACT CPP /Fr
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 /nologo /subsystem:windows /machine:I386
# ADD LINK32 version.lib /nologo /subsystem:windows /machine:I386

!ELSEIF  "$(CFG)" == "Setup - Win32 Debug"

# PROP BASE Use_MFC 6
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 5
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MDd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_AFXDLL" /Yu"stdafx.h" /FD /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "USNA_EVAL" /D "STUDENT_INSTALL" /Yu"stdafx.h" /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG" /d "_AFXDLL"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# ADD LINK32 version.lib /nologo /subsystem:windows /debug /machine:I386 /nodefaultlib:"libcmt" /pdbtype:sept /nodefaultlib:"LIBCMT.lib/nodefaultlib:"libcd.lib"
# SUBTRACT LINK32 /pdb:none

!ELSEIF  "$(CFG)" == "Setup - Win32 USNA Eval Release"

# PROP BASE Use_MFC 5
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Setup___Win32_USNA_Eval_Release"
# PROP BASE Intermediate_Dir "Setup___Win32_USNA_Eval_Release"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 5
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "USNA_Eval_Release"
# PROP Intermediate_Dir "USNA_Eval_Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O1 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /Yu"stdafx.h" /FD /c
# SUBTRACT BASE CPP /Fr
# ADD CPP /nologo /MT /W3 /GX /O1 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "USNA_EVAL" /D "STUDENT_INSTALL" /Yu"stdafx.h" /FD /c
# SUBTRACT CPP /Fr
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 version.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 version.lib /nologo /subsystem:windows /machine:I386 /out:"USNA_Eval_Release/SetupUSNA.exe"

!ELSEIF  "$(CFG)" == "Setup - Win32 Student Release"

# PROP BASE Use_MFC 5
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Setup___Win32_Student_Release"
# PROP BASE Intermediate_Dir "Setup___Win32_Student_Release"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 5
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Student_Release"
# PROP Intermediate_Dir "Student_Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O1 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /Yu"stdafx.h" /FD /c
# SUBTRACT BASE CPP /Fr
# ADD CPP /nologo /MT /W3 /GX /O1 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "STUDENT_INSTALL" /Yu"stdafx.h" /FD /c
# SUBTRACT CPP /Fr
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 version.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 version.lib /nologo /subsystem:windows /machine:I386 /out:"Student_Release/SetupStudent.exe"

!ELSEIF  "$(CFG)" == "Setup - Win32 Experiment Release"

# PROP BASE Use_MFC 5
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Setup___Win32_Experiment_Release0"
# PROP BASE Intermediate_Dir "Setup___Win32_Experiment_Release0"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 5
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Experiment_Release"
# PROP Intermediate_Dir "Experiment_Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O1 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "STUDENT_INSTALL" /Yu"stdafx.h" /FD /c
# SUBTRACT BASE CPP /Fr
# ADD CPP /nologo /MT /W3 /GX /O1 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "STUDENT_INSTALL" /D "EXPERIMENT" /Yu"stdafx.h" /FD /c
# SUBTRACT CPP /Fr
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 version.lib /nologo /subsystem:windows /machine:I386 /out:"Student_Release/SetupStudent.exe"
# ADD LINK32 version.lib /nologo /subsystem:windows /machine:I386 /out:"Experiment_Release/SetupExp.exe"

!ELSEIF  "$(CFG)" == "Setup - Win32 Eval Patch"

# PROP BASE Use_MFC 5
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Setup___Win32_Eval_Patch"
# PROP BASE Intermediate_Dir "Setup___Win32_Eval_Patch"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 5
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "USNA_Eval_Patch"
# PROP Intermediate_Dir "USNA_Eval_Patch"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O1 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "USNA_EVAL" /D "STUDENT_INSTALL" /Yu"stdafx.h" /FD /c
# SUBTRACT BASE CPP /Fr
# ADD CPP /nologo /MT /W3 /GX /O1 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "USNA_EVAL" /D "STUDENT_INSTALL" /D "PATCH" /Yu"stdafx.h" /FD /c
# SUBTRACT CPP /Fr
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 version.lib /nologo /subsystem:windows /machine:I386 /out:"USNA_Eval_Release/SetupUSNA.exe"
# ADD LINK32 version.lib /nologo /subsystem:windows /machine:I386 /out:"USNA_Eval_Patch/SetupPatch.exe"

!ELSEIF  "$(CFG)" == "Setup - Win32 OLI Release"

# PROP BASE Use_MFC 5
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Setup___Win32_OLI_Release"
# PROP BASE Intermediate_Dir "Setup___Win32_OLI_Release"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 5
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "OLI_Release"
# PROP Intermediate_Dir "OLI_Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O1 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "STUDENT_INSTALL" /Yu"stdafx.h" /FD /c
# SUBTRACT BASE CPP /Fr
# ADD CPP /nologo /MT /W3 /GX /O1 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "STUDENT_INSTALL" /D "OLI" /Yu"stdafx.h" /FD /c
# SUBTRACT CPP /Fr
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 version.lib /nologo /subsystem:windows /machine:I386 /out:"Student_Release/SetupStudent.exe"
# ADD LINK32 version.lib /nologo /subsystem:windows /machine:I386 /out:"OLI_Release/SetupOLI.exe"

!ENDIF 

# Begin Target

# Name "Setup - Win32 Release"
# Name "Setup - Win32 Debug"
# Name "Setup - Win32 USNA Eval Release"
# Name "Setup - Win32 Student Release"
# Name "Setup - Win32 Experiment Release"
# Name "Setup - Win32 Eval Patch"
# Name "Setup - Win32 OLI Release"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\ChkSpaceDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\Cleanup.cpp
# End Source File
# Begin Source File

SOURCE=.\DirectoryDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\ExitDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\files.cpp
# End Source File
# Begin Source File

SOURCE=.\FinishPg.cpp
# End Source File
# Begin Source File

SOURCE=.\InstDirPg.cpp
# End Source File
# Begin Source File

SOURCE=.\LicenseDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\MainWnd.cpp
# End Source File
# Begin Source File

SOURCE=.\MySheet.cpp
# End Source File
# Begin Source File

SOURCE=.\Picture.cpp
# End Source File
# Begin Source File

SOURCE=.\ProgressDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\RegDlg.cpp
# End Source File
# Begin Source File

SOURCE=.\SettingPg.cpp
# End Source File
# Begin Source File

SOURCE=.\Setup.cpp
# End Source File
# Begin Source File

SOURCE=.\Setup.rc
# End Source File
# Begin Source File

SOURCE=.\Shortcut.cpp
# End Source File
# Begin Source File

SOURCE=.\StartPg.cpp
# End Source File
# Begin Source File

SOURCE=.\StdAfx.cpp
# ADD CPP /Yc"stdafx.h"
# End Source File
# Begin Source File

SOURCE=.\VerMsgDlg.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\ChkSpaceDlg.h
# End Source File
# Begin Source File

SOURCE=.\Cleanup.h
# End Source File
# Begin Source File

SOURCE=.\DirectoryDlg.h
# End Source File
# Begin Source File

SOURCE=.\ExitDlg.h
# End Source File
# Begin Source File

SOURCE=.\Files.h
# End Source File
# Begin Source File

SOURCE=.\FinishPg.h
# End Source File
# Begin Source File

SOURCE=.\InstDirPg.h
# End Source File
# Begin Source File

SOURCE=.\LicenseDlg.h
# End Source File
# Begin Source File

SOURCE=.\MainWnd.h
# End Source File
# Begin Source File

SOURCE=.\MySheet.h
# End Source File
# Begin Source File

SOURCE=.\Picture.h
# End Source File
# Begin Source File

SOURCE=.\ProgressDlg.h
# End Source File
# Begin Source File

SOURCE=.\RegDlg.h
# End Source File
# Begin Source File

SOURCE=.\Resource.h
# End Source File
# Begin Source File

SOURCE=.\SettingPg.h
# End Source File
# Begin Source File

SOURCE=.\Setup.h
# End Source File
# Begin Source File

SOURCE=.\Shortcut.h
# End Source File
# Begin Source File

SOURCE=.\StartPg.h
# End Source File
# Begin Source File

SOURCE=.\StdAfx.h
# End Source File
# Begin Source File

SOURCE=.\VerMsgDlg.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;cnt;rtf;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=.\res\AndesSplash4.bmp
# End Source File
# Begin Source File

SOURCE=.\res\bitmap1.bmp
# End Source File
# Begin Source File

SOURCE=.\res\bmp00001.bmp
# End Source File
# Begin Source File

SOURCE=.\res\cursor1.cur
# End Source File
# Begin Source File

SOURCE=.\res\drives.bmp
# End Source File
# Begin Source File

SOURCE=.\res\FlatAndesInstallImg.jpg
# End Source File
# Begin Source File

SOURCE=.\res\Setup.ico
# End Source File
# Begin Source File

SOURCE=.\res\Setup.rc2
# End Source File
# End Group
# Begin Source File

SOURCE=.\ReadMe.txt
# End Source File
# End Target
# End Project
